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

    floskell: arguments: --style [fundamental|chris-done|johan-tibell|gibiansky|cramer]

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

By default it uses the style called `fundamental`, if you want to use
another, `johan-tibell`, run `M-x customize-variable
floskell-style`. If you want to configure per-project, make a file
called `.dir-locals.el` in the project root directory like this:

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

## Contributing your own printer style

This package comes with a basic fundamental pretty printer, which is
probably not desirable to use.

It comes with other styles implemented on top of this fundamental
printer, in the modules in `Floskell.Styles.*`.

Make a module `Floskell.Styles.YourName` in which to place the printer.

To define your own, see
[Floskell.Styles.Fundamental](https://github.com/ennocramer/floskell/blob/master/src/Floskell/Styles/Fundamental.hs)
for a starting point. This module defines a blank style, adds no
additional extensions. Customizations are specified via the
`styleExtenders` property. See
[Floskell.Styles.ChrisDone](https://github.com/ennocramer/floskell/blob/master/src/Floskell/Styles/ChrisDone.hs)
for an example of a non-trivial style.

Useful combinators can be found in
[Floskell.Pretty](https://github.com/ennocramer/floskell/blob/master/src/Floskell/Pretty.hs)
for defining printers. When you want to use a fundamental printer, use
`prettyNoExt` instead of `pretty`. Comments will still be inserted by
`prettyNoExt`.

If you want to contribute it to the package, add it to the list of
styles in
[Floskell](https://github.com/ennocramer/floskell/blob/master/src/Floskell.hs)
and export it, and open a pull request. Use
[the Erlang git commit guide](https://github.com/erlang/otp/wiki/Writing-good-commit-messages)
for your commits.

## Example

Input code:

``` haskell
foo = do print "OK, go"; foo (foo bar) -- Yep.
          (if bar then bob else pif) (case mu {- cool -} zot of
            Just x -> return (); Nothing -> do putStrLn "yay"; return 1) bill -- Etc
  where potato Cakes {} = 2 * x foo * bar / 5
```

### Fundamental

This is an intentionally very dumb style that demands extension.

``` haskell
foo =
  do print
       "OK, go"
     foo
       (foo
          bar)
       (if bar
           then bob
           else pif)
       (case mu {- cool -}
               zot of
          Just x ->
            return
              ()
          Nothing ->
            do putStrLn
                 "yay"
               return
                 1)
       bill -- Etc
  where potato Cakes{} =
          2 * x
                foo * bar / 5
```

### Johan Tibell

Documented in
[the style guide](https://github.com/tibbe/haskell-style-guide).
This printer style uses some simple heuristics in deciding when to go
to a new line or not, and custom handling of do, if, case alts, rhs,
etc.

``` haskell
foo = do
    print "OK, go"
    foo
        (foo bar)
        (if bar
             then bob
             else pif)
        (case mu {- cool -} zot of
             Just x ->
                 return ()
             Nothing -> do
                 putStrLn "yay"
                 return 1)
        bill -- Etc
  where
    potato Cakes{} =
        2 * x foo * bar / 5
```

### Chris Done

My style is documented in
[the style guide](https://github.com/chrisdone/haskell-style-guide).
This printer style uses some simple heuristics in deciding when to go
to a new line or not.

``` haskell
foo =
  do print "OK, go"
     foo (foo bar)
         (if bar
             then bob
             else pif)
         (case mu {- cool -} zot of
            Just x -> return ()
            Nothing ->
              do putStrLn "yay"
                 return 1)
         bill -- Etc
  where potato Cakes{} = 2 * x foo * bar / 5
```

### Andrew Gibiansky

``` haskell
foo = do
  print "OK, go"
  foo (foo bar) -- Yep.
   (if bar
       then bob
       else pif) (case mu {- cool -} zot of
                    Just x -> return ()
                    Nothing -> do
                      putStrLn "yay"
                      return 1) bill -- Etc

  where
    potato Cakes{} = 2 * x foo * bar / 5
```

### Enno Cramer

``` haskell
foo = do
    print "OK, go"
    foo (foo bar)
        (if bar then bob else pif)
        (case mu {- cool -} zot of
             Just x -> return ()
             Nothing -> do
                 putStrLn "yay"
                 return 1)
        bill -- Etc
  where
    potato Cakes{} = 2 * x foo * bar / 5
```
