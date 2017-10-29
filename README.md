# Floskell [![Build Status](https://travis-ci.org/ennocramer/floskell.png)](https://travis-ci.org/ennocramer/floskell)

Floskell is a flexible Haskell source code pretty printer.

[Documentation](https://github.com/ennocramer/floskell/blob/master/README.md)

[Examples](https://github.com/ennocramer/floskell/blob/master/styles)

Floskell started as a fork of version 4 of
[Chris Done's hindent](https://github.com/commercialhaskell/hindent).

## Installation

    $ git clone https://github.com/ennocramer/floskell
    $ cd floskell
    $ stack install

## Usage

    floskell is used in a pipeline style:

    $ cat path/to/sourcefile.hs | floskell > outfile.hs

    floskell: arguments: --style [chris-done|johan-tibell|gibiansky|cramer]

## Emacs

In
[contrib/floskell.el](https://github.com/ennocramer/floskell/blob/master/contrib/floskell.el)
there is `floskell-mode`, which provides keybindings to reindent parts
of the buffer:

- `M-q` reformats the current declaration.  When inside a comment, it
  fills the current paragraph instead, like the standard `M-q`.
- `C-M-\` reformats the current region.

To enable it, add the following to your init file:

```lisp
(add-to-list 'load-path "/path/to/floskell/contrib")
(require 'floskell)
(add-hook 'haskell-mode-hook #'floskell-mode)
```

By default it uses the style called `flex`, if you want to use
another, run `M-x customize-variable floskell-style`. If you want to
configure per-project, make a file called `.dir-locals.el` in the
project root directory like this:

``` lisp
((nil . ((floskell-style . "johan-tibell"))))
```

## Vim

The `'formatprg'` option lets you use an external program (like
floskell) to format your text. Put the following line into
~/.vim/ftplugin/haskell.vim to set this option for Haskell files:

    setlocal formatprg=floskell\ --style\ chris-done

Then you can format with floskell using `gq`. Read `:help gq` and `help
'formatprg'` for more details.

Note that unlike in emacs you have to take care of selecting a
sensible buffer region as input to floskell yourself. If that is too
much trouble you can try
[vim-textobj-haskell](https://github.com/gilligan/vim-textobj-haskell)
which provides a text object for top level bindings.

## Atom

Basic support is provided through
[contrib/floskell.coffee](https://github.com/ennocramer/floskell/blob/master/contrib/floskell.coffee),
which adds floskell to atom menu with each available style. Mode should
be installed as package into `.atom\packages\${PACKAGE_NAME}`, here is
simple example of atom
[package](https://github.com/Heather/atom-hindent).
