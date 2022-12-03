# sbcli

A better REPL for SBCL. It handles errors gracefully, is not too verbose, has
readline capabilities, including multiline input and reset, and has optional
syntax highlighting capabilities using [pygmentize](https://pygments.org/).

## Installation

For most cases, calling `./install.sh` should suffice. It will install `sbcli`
into `$PREFIX/bin`, with the environment variable `PREFIX` defaulting to
`/usr/local`. If you are using Mac and having issues with `cl-readline` see the
[installation notes for cl-readline](https://github.com/mrkkrp/cl-readline#installation).

## Dependencies

`sbcli` depends on [alexandria(https://common-lisp.net/project/alexandria/], [cl-str](https://github.com/vindarel/cl-str) and
[cl-readline](https://github.com/mrkkrp/cl-readline). If you have [Quicklisp](http://quicklisp.org/)
installed, all the dependencies will be installed on `sbcli`s first launch. `sbcli`
assumes that Quicklisp is installed under `~/quicklisp`.

## Usage

You should be able to launch `sbcli` by just typing `sbcli`. Once you’re in the
REPL, you can edit normally with readline capabilities. Hitting tab will
autocomplete (note that if there is more than one possibility, you’ll have to
hit tab twice).

Typing `:help` will give you an overview over all the available special cases
and give you an overview over what you’ve defined.

Typing `:h symbol` will enter inspection mode for a symbol. Typing `CTRL-D`
exits inspection mode.

Typing `:doc symbol` prints the available documentation for this symbol.

Typing `(symbol ?` also prints the available documentation for this symbol.

Typing `:q`, `CTRL-D`, or `CTRL-C` will exit the REPL.

Typing `:r` resets the environment.

Typing `:d symbol` dumps the disassembly of a symbol.

Typing `:t <expression>` prints the type returned by an expression.

Typing `:s filename` will save all of the expressions that were typed and
evaluated to the file specified, in the format:

```lisp
(+ 1 2) ; => 3
(* 6 10) ; => 60
```

Of course, depending on your result, this can result in very long lines, or
break if your result contains newlines. Use at your own peril for now!

## Customization

If you want you can add customizations to `sbcli`. On startup it will load a
file called `.sbclirc` in your home directory if it exists. You can execute
arbitrary code there, two of the more interesting values to set are `*prompt*`
and `*ret*`. Check out an example resource file
[here](https://github.com/hellerve/sbcli/blob/master/examples/.sbclirc).

### Exposed Variables

For reference, here is a complete list of the variables we expose:

```lisp
; the name of the REPL, printed on top
*repl-name*    ; => "Veit's REPL for SBCL"

; the prompt and continuation prompt variables
*prompt*       ; => "sbcl> "
*prompt2*      ; => "....> "

; the return value prompt
*ret*          ; => "=> "

; where to store the history
*hist-file*    ; => "~/.sbcli_history"

; the history variable
; while nothing prevents you from writing to it, i advise against it
*hist*         ; => ()

; you can optionally set a path to pygmentize to enable syntax-coloring
; in the REPL. N.B: might lead to slower rendering speed
;
; if you're unsure what to put there, i suggest using
; [which](https://github.com/eudoxia0/which)
*pygmentize* ; => nil
; you can also customize the pygmentize invocation
*pygmentize-options* ; => ("-s" "-l" "lisp")

; the last error encountered in the REPL. You can call
; `invoke-debugger` on it!
*error*
```

<hr/>

Have fun!
