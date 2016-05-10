# sourcemap.el [![Build Status](https://travis-ci.org/syohex/emacs-sourcemap.svg)](https://travis-ci.org/syohex/emacs-sourcemap) [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

sourcemap parser in Emacs Lisp.
Only supports decoder now.


## Installation

You can install `sourcemap` from [MELPA](https://melpa.org/) and [MELPA stable](https://stable.melpa.org)

You can install `sourcemap` with the following command.

<kbd>M-x package-install [RET] sourcemap [RET]</kbd>


## Interface

#### `(sourcemap-original-position-for sourcemap properties)`

Find line and column of original file from specified properties.

`properties` is plist and shoul have following properties
- `:source` - source file
- `:line` - Line in generated file
- `:column` - Column in generated file

Return value is property list which has `:line` and `:column`.


#### `(sourcemap-generated-position-for sourcemap properties)`

Find line and column of generated file from specified properties.

`properties` is `plist` and should have following properties
- `:line` - Original line number
- `:column` - Original column number


#### `(sourcemap-goto-corresponding-point properties)`

Go to corres

This is useful for compiling command of `coffee-mode`.
You can use this for moving point in compiled Javascript file which
currespoinding to cursor point in CoffeeScript. If you want to use
this feature, add following configuration.

```lisp
(setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap file
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)
```

`properties` is `plist` and should have following properties
- `:source` - source file
- `:sourcemap` - sourcemap file
- `:line` - line in `:source`(CoffeeScript)
- `:column` - column in `:source`(CoffeeScript)


#### `(sourcemap-from-file file)`

Parse `file` as sourcemap and return sourcemap instance

#### `(sourcemap-from-string string)`

Parse `string` as sourcemap and return sourcemap instance

[melpa-link]: https://melpa.org/#/sourcemap
[melpa-stable-link]: https://stable.melpa.org/#/sourcemap
[melpa-badge]: https://melpa.org/packages/sourcemap-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/sourcemap-badge.svg
