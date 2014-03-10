# sourcemap.el

Source map parser in Emacs Lisp.
Only supports decoder now.


## Installation

You can install `sourcemap.el` from [MELPA](http://melpa.milkbox.net/)


## Interface

#### (sourcemap-from-file file)

Parse `file` as sourcemap and return sourmap instance

#### (sourcemap-from-string string)

Parse `string` as sourcemap and return sourmap instance


#### (sourcemap-original-position-for sourcemap properties)

Find line and column of original file from specified properties.

`properties` is plist and shoul have following properties
- `:line` - Line in generated file
- `:column` - Column in generated file

Return value is property list which has `:line` and `:column`.


#### (sourcemap-generated-position-for sourcemap properties)

Find line and column of generated file from specified properties.

`properties` is `plist` and should have following properties
- `:source` - source file
- `:line` - Original line number
- `:column` - Original column number


#### (sourcemap-goto-corresponding-point properties)

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
