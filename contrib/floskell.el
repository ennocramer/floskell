;;; floskell.el --- Indent haskell code using the "floskell" program

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/ennocramer/floskell
;; Package-Requires: ((cl-lib "0.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode

(defvar floskell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap indent-region] #'floskell-reformat-region)
    (define-key map [remap fill-paragraph] #'floskell-reformat-decl-or-fill)
    map)
  "Keymap for `floskell-mode'.")

;;;###autoload
(define-minor-mode floskell-mode
  "Indent code with the floskell program.

Provide the following keybindings:

\\{floskell-mode-map}"
  :init-value nil
  :keymap floskell-mode-map
  :lighter " HI"
  :group 'haskell
  :require 'floskell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization properties

(defcustom floskell-style
  "fundamental"
  "The style to use for formatting."
  :group 'haskell
  :type 'string
  :safe #'stringp)

(defcustom floskell-process-path
  "floskell"
  "Location where the floskell executable is located."
  :group 'haskell
  :type 'string
  :safe #'stringp)

(defcustom floskell-line-length
  nil
  "Optionally override the line length of the formatting style."
  :group 'haskell
  :type '(choice (const :tag "From style" nil)
                 (integer :tag "Override" 80))
  :safe (lambda (val) (or (integerp val) (not val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

;;;###autoload
(defun floskell-reformat-decl ()
  "Re-format the current declaration by parsing and pretty
  printing it. Comments are preserved, although placement may be
  funky."
  (interactive)
  (let ((start-end (floskell-decl-points)))
    (when start-end
      (let ((beg (car start-end))
            (end (cdr start-end)))
        (floskell-reformat-region beg end)))))

;;;###autoload
(defun floskell-reformat-buffer ()
  "Reformat the whole buffer."
  (interactive)
  (floskell-reformat-region (point-min)
                           (point-max)))

;;;###autoload
(defun floskell-reformat-decl-or-fill (justify)
  "Re-format current declaration, or fill paragraph.

Fill paragraph if in a comment, otherwise reformat the current
declaration."
  (interactive (progn
                 ;; Copied from `fill-paragraph'
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'full))))
  (if (floskell-in-comment)
      (fill-paragraph justify t)
    (floskell/reformat-decl)))

;;;###autoload
(defun floskell-reformat-region (beg end)
  "Reformat the given region, accounting for indentation."
  (interactive "r")
  (if (= (save-excursion (goto-char beg)
                         (line-beginning-position))
         beg)
      (floskell-reformat-region-as-is beg end)
    (let* ((column (- beg (line-beginning-position)))
           (string (buffer-substring-no-properties beg end))
           (new-string (with-temp-buffer
                         (insert (make-string column ? ) string)
                         (floskell-reformat-region-as-is (point-min)
                                                        (point-max))
                         (delete-region (point-min) (1+ column))
                         (buffer-substring (point-min)
                                           (point-max)))))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert new-string)))))

;;;###autoload
(defun floskell/reformat-decl ()
  "See `floskell-reformat-decl'."
  (floskell-reformat-decl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal library

(defun floskell-reformat-region-as-is (beg end)
  "Reformat the given region as-is.

This is the place where floskell is actually called."
  (let* ((original (current-buffer))
         (orig-str (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (let ((temp (current-buffer)))
        (with-current-buffer original
          (let ((ret (apply #'call-process-region
                            (append (list beg
                                          end
                                          floskell-process-path
                                          nil ; delete
                                          temp ; output
                                          nil
                                          "--style"
                                          floskell-style)
                                    (when floskell-line-length
                                      (list "--line-length"
                                            (number-to-string
                                             floskell-line-length)))
                                    (floskell-extra-arguments)))))
            (cond
             ((= ret 1)
              (let ((error-string
                     (with-current-buffer temp
                       (let ((string (progn (goto-char (point-min))
                                            (buffer-substring (line-beginning-position)
                                                              (line-end-position)))))
                         string))))
                (if (string= error-string "floskell: Parse error: EOF")
                    (message "language pragma")
                  (error error-string))))
             ((= ret 0)
              (let ((new-str (with-current-buffer temp
                               (buffer-string))))
                (if (not (string= new-str orig-str))
                    (let ((line (line-number-at-pos))
                          (col (current-column)))
                      (delete-region beg
                                     end)
                      (let ((new-start (point)))
                        (insert new-str)
                        (let ((new-end (point)))
                          (goto-char (point-min))
                          (forward-line (1- line))
                          (goto-char (+ (line-beginning-position) col))
                          (when (looking-back "^[ ]+")
                            (back-to-indentation))
                          (delete-trailing-whitespace new-start new-end)))
                      (message "Formatted."))
                  (message "Already formatted.")))))))))))

(defun floskell-decl-points (&optional use-line-comments)
  "Get the start and end position of the current
declaration. This assumes that declarations start at column zero
and that the rest is always indented by one space afterwards, so
Template Haskell uses with it all being at column zero are not
expected to work."
  (cond
   ;; If we're in a block comment spanning multiple lines then let's
   ;; see if it starts at the beginning of the line (or if any comment
   ;; is at the beginning of the line, we don't care to treat it as a
   ;; proper declaration.
   ((and (not use-line-comments)
         (floskell-in-comment)
         (save-excursion (goto-char (line-beginning-position))
                         (floskell-in-comment)))
    nil)
   ((save-excursion
      (goto-char (line-beginning-position))
      (or (looking-at "^-}$")
          (looking-at "^{-$")))
    nil)
   ;; Otherwise we just do our line-based hack.
   (t
    (save-excursion
      (let ((start
             (or (cl-letf
                     (((symbol-function 'jump)
                       #'(lambda ()
                           (search-backward-regexp "^[^ \n]" nil t 1)
                           (cond
                            ((save-excursion (goto-char (line-beginning-position))
                                             (looking-at "|]"))
                             (jump))
                            (t (unless (or (looking-at "^-}$")
                                           (looking-at "^{-$"))
                                 (point)))))))
                   (goto-char (line-end-position))
                   (jump))
                 0))
            (end
             (progn
               (goto-char (1+ (point)))
               (or (cl-letf
                       (((symbol-function 'jump)
                         #'(lambda ()
                             (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                               (cond
                                ((save-excursion (goto-char (line-beginning-position))
                                                 (looking-at "|]"))
                                 (jump))
                                (t (forward-char -1)
                                   (search-backward-regexp "[^\n ]" nil t)
                                   (forward-char)
                                   (point)))))))
                     (jump))
                   (point-max)))))
        (cons start end))))))

(defun floskell-in-comment ()
  "Are we currently in a comment?"
  (save-excursion
    (when (and (= (line-end-position)
                  (point))
               (/= (line-beginning-position) (point)))
      (forward-char -1))
    (and (or (eq 'font-lock-comment-delimiter-face
                 (get-text-property (point) 'face))
             (eq 'font-lock-doc-face
                 (get-text-property (point) 'face))
             (eq 'font-lock-comment-face
                 (get-text-property (point) 'face))
             (save-excursion (goto-char (line-beginning-position))
                             (looking-at "^\-\- ")))
         ;; Pragmas {-# SPECIALIZE .. #-} etc are not to be treated as
         ;; comments, even though they are highlighted as such
         (not (save-excursion (goto-char (line-beginning-position))
                              (looking-at "{-# "))))))

(defun floskell-extra-arguments ()
  "Pass in extra arguments, such as extensions and optionally
other things later."
  (if (boundp 'haskell-language-extensions)
      haskell-language-extensions
    '()))

(provide 'floskell)

;;; floskell.el ends here
