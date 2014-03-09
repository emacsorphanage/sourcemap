# sourcemap.el

Source map parser in Emacs Lisp.
Only supports decoder now.


## Installation

You can install `sourcemap.el` from [MELPA](http://melpa.milkbox.net/)


## Interface

#### (sourcemap-from-file file)

Parse file as sourcemap and return sourmap instance


#### (sourcemap-original-position-for sourcemap properties)

Find line and column of original file from specified properties.

`properties` argument has
- `:line` - Line in generated file
- `:column` - Column in generated file

Return value is property list which has `:line` and `:column`.


#### (sourcemap-generated-position-for sourcemap properties)

Find line and column of generated file from specified properties.

`properties` argument has
- `:source` - source file
- `:line` - Original line number
- `:column` - Original column number
