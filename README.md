# Floskell [![Build Status](https://travis-ci.org/ennocramer/floskell.png)](https://travis-ci.org/ennocramer/floskell)

Floskell is a flexible Haskell source code pretty printer.

[Documentation](https://github.com/ennocramer/floskell/blob/master/README.md)

[Examples](https://github.com/ennocramer/floskell/blob/master/styles)

Floskell started as a fork of version 4 of [Chris Done's
hindent](https://github.com/commercialhaskell/hindent).  The
formatting styles present in hindent 4 have been preserved in spirit,
but generally will not produce exactly the same output.


## Installation

    $ git clone https://github.com/ennocramer/floskell
    $ cd floskell
    $ stack install


## Usage

Floskell can be used to reformat Haskell source files in place

    $ floskell path/to/sourcefile.hs

or as a pipeline processor

    $ cat path/to/sourcefile.hs | floskell > outfile.hs

One of the predefined formatting styles can be selected with the
`--style` option

    $ floskell --style cramer path/to/sourcefile.hs

Or the style can be read from a configuration file

    $ floskell --config path/to/config.json path/to/sourcefile.hs

If neither style nor configuration file is given on the command line,
Floskell will try to find a configuration file in the current working
directory or any of its parent directories, or fall back to the users
global configuration file.


## Formatting Process

A style in Floskell is a set of formatting possibilities for different
language constructs.  Floskell formats Haskell code according to a
given style by finding the combination of allowed formatting choices
that result in the best overall layout.

### Penalty

The overall layout of the generated output is judged by a penalty
function.  This function takes into account the number of lines
generated, whether lines are longer than a defined limit, and the
indentation of each line.

In general, Floskell will try to generate

* the smallest number of lines,

* the least amount of indentation, and

* the least amount of overflow.

### Layout

A number of language constructs can be formatted in different ways.
Floskell generally defines two layout choices for these constructs,
`flex` and `vertical`, and three modes to apply these choices, `flex`,
`vertical`, and `try-oneline`.

The layout choice `flex` generally tries to fit as much on each line
as possible, but allows linebreaks in a number of places, while the
`vertical` layout choice forces linebreaks in various places.

The `flex` and `vertical` layout modes simply select the respective
layout choice, while `try-oneline` will first try `flex`, but replace
the choice with `vertical` if the `flex` layout would more than one
line or an overfull line.

An example:

```haskell
-- flex layout for con-decls
data Enum = One | Two | Three

-- vertical layout for con-decls
data Enum = One
          | Two
          | Three
```

### Indentation

A number of language constructs can apply indentation to sub-elements.
Floskell provides two different indentation choices, `aligned` and
`indented`, and three modes to apply these choices, `align`,
`indent-by n`, and `align-or-indent-by n`.

`align` will start the sub-element on the same line and raise the
indentation to align following lines, while `indent-by n` will start
the sub-element on the following line with the indentation raised by
`n`.

`align-or-indent-by n` will allow either choice and select the
formatting with the least penalty.

An example:

```haskell
-- align for do
foo = do x <- xs
         y <- ys
         return (x, y)

-- indent-by 4 for do
foo = do
    x <- xs
    y <- ys
    return (x, y)
```

### Tabstop Alignment

Some language constructs allow for tabstop alignment.  Alignment is
optional and subject to configurable limits, regarding the amount of
added whitespace.

An example:

```haskell
-- let without alignment
let foo = bar
    quux = quuz
in foo quux

-- let with alignment
let foo    = bar
    quuuux = quuz
in foo quuuux
```

### Whitespace

Floskell allows the customization of whitespace around infix
operators, as well as inside parentheses and other enclosing
punctuation characters.

The presence of whitespace or linebreaks is as `before`, meaning
before the operator/enclosed item, `after`, meaning after the
operator/enclosed item, or `both`, meaning both before and after the
operator/enclosed item.

Whitespace configuration can depend on the context where an operator
or enclosing punctuation is used.  The context can be one of
`declaration`, `type`, `pattern`, `expression`, or `other`.

An example:

```haskell
-- tuple with space after/before parentheses and after comma
tuple = ( 1, 2 )
-- tuple without any spaces
tuple = (1,2)
```

### Preprocessor Directives (CPP)

Floskell, in general, supports Haskell source with conditional
compilation directives using the `CPP` language extensions.  However,
due to the way this support is implemented, some care must be taken to
not confuse the Haskell source parser.

Floskell treats conditional compilation directives as if they were
simply comments.  As a consequence, the input must still be valid
Haskell when all preprocessor lines are removed.  This is relevant
when using `#if`/`#else`/`#endif` sequences, as Floskell will see both
the if- and else-block in sequence.  For example, the following cannot
be processed with Floskell, as the first declaration of `prettyPrint`
ends with an incomplete `do` block:

```haskell
#if MIN_VERSION_haskell_src_exts(1,21,0)
    prettyPrint (GadtDecl _ name _ _ mfielddecls ty) = do
#else
    prettyPrint (GadtDecl _ name mfielddecls ty) = do
#endif
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            prettyRecordFields len Declaration decls
            operator Type "->"
        pretty ty
```

Instead, some of the contents of the `do` block have to be duplicated,
so that the contents of the `#if` are valid Haskell on their own.

```haskell
#if MIN_VERSION_haskell_src_exts(1,21,0)
    prettyPrint (GadtDecl _ name _ _ mfielddecls ty) = do
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            prettyRecordFields len Declaration decls
            operator Type "->"
        pretty ty
#else
    prettyPrint (GadtDecl _ name mfielddecls ty) = do
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            prettyRecordFields len Declaration decls
            operator Type "->"
        pretty ty
#endif
```


## Customization

Floskell's behaviour and the style of its output can be modified with
a configuration file.

See the documentation on the [Configuration Format](CONFIGURATION.md)
for a detailed description of the contents of the configuration file.

### Initial Configuration

The `--print-config` command line option can be used to create an
initial configuration file.

    $ floskell --style cramer --print-config > ~/.floskell

This command will create a configuration file with all fields and the
entire definition of the selected style in the `formatting` block.

### Configuration File Location

* If a style is given on the command line, but no explicit
  configuration file, the style will be used as-is and not
  configuration file will be loaded.

* If both a style and an explicit configuration file are given on the
  command line, the explicit configuration file will be loaded and the
  style parameter will replace any style setting in the configuration
  file.

* If neither style nor explicit configuration file are given on the
  command line, Floskell will try to find an applicable configuration
  file.  Floskell will look for

  * a file called `floskell.json` in the current working directory and
    all its parent directories,

  * a file called `config.json` in `~/.floskell`, `~/config/floskell`,
    or `%APPDATA%/floskell`, and lastly

  * a file called `.floskell.json` in `~` or `~/.config`.

  Only the first file found will be loaded.


## Editor Integration

### Emacs

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

By default, Floskell uses the style called `base`.  If you want to use
another, run `M-x customize-variable floskell-style` or create a
Floskell configuration file in your home directory. If you want to
configure per-project, add a configuration file in the project root or
make a file called `.dir-locals.el` in the project root directory like
this:

``` lisp
((nil . ((floskell-style . "johan-tibell"))))
```

### Vim

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

### Atom

Basic support is provided through
[contrib/floskell.coffee](https://github.com/ennocramer/floskell/blob/master/contrib/floskell.coffee),
which adds floskell to atom menu with each available style, and
`Default` which will use the appropriate configuration file. Mode
should be installed as package into `.atom\packages\${PACKAGE_NAME}`,
here is simple example of atom
[package](https://github.com/Heather/atom-hindent).
