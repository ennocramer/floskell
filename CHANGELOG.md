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
