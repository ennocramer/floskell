# Floskell 0.10.5 (2020-09-21)

* Fix indentation of nested list and tuple expressions
* Fix formatting of slightly overlong module headers
* Fix formatting of promoted types

# Floskell 0.10.4 (2020-08-05)

* Fix formatting of type applications
* Fix for comments hopping over 'where' keyword
* Add safety whitespace override for @ in patterns

# Floskell 0.10.3 (2020-05-17)

* Updated to haskell-src-exts-1.23.0, with support for Block Arguments
  and Arrow Brackets
* Compatible with GHC-8.10.1

# Floskell 0.10.2 (2019-11-11)

* Updated to haskell-src-exts-1.22.0, with support for Template
  Haskell typed splices and quotations
* Compatible with GHC-8.8.1

# Floskell 0.10.1 (2019-05-25)

* Fix for broken off-side rule with multi-line lists
* Fix for increasing comment indentation before instance declarations

# Floskell 0.10.0 (2019-05-03)

* Updated to haskell-src-exts-1.21.0, with support for `DerivingVia` and `TypeInType`
* Support for custom fixity declarations (with common libraries built in)
* More control over sorting and formatting of imports
* More control over formatting of type signatures
* New option to align `=` with `|` in data declarations
* New option to allow `do`, `case`, and lambda in `try-oneline` layouts
* New option to avoid vertical space between declarations
* New option to tabstop-align right-hand-side of function match clauses
* Improved handling of CPP directives
* Improved positioning of comments
* Better error messages for JSON syntax errors in configuration file
* Many formatting fixes

Incompatible changes:

* `formatting/layout/typesig` has been removed in favor of
  `formatting/layout/type`

# Floskell 0.9.0 (2019-01-25)

* Initial release
