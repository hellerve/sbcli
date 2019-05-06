# sbcli

A better REPL for SBCL. It handles errors greacefully, is not too verbose,
and has readline capabilities, including multiline input and reset.

## Installation

For most cases, calling `./install.sh` should suffice. It will
install `sbcli` into `/usr/local/bin`. If you are using Mac and having
issues with `cl-readline` see the [installation notes for
cl-readline](https://github.com/mrkkrp/cl-readline#installation).

## Dependencies

`sbcli` depends on [Quicklisp](http://quicklisp.org/) and
[cl-readline](https://github.com/mrkkrp/cl-readline).
If you have Quicklisp installed, cl-readline will be installed
on `sbcli`s first launch. `sbcli` assumes that Quicklisp is
installed under `~/quicklisp`.

## Usage

You should be able to launch `sbcli` by just typing `sbcli`. Once you’re in the
REPL, you can edit normally with readline capabilities. Hitting tab will
autocomplete (note that if there are more than one possibility, you’ll have to
hit tab twice).

Typing `:help` will give you an overview over all the available special cases
and give you an overview over what you’ve defined.

Typing `:h symbol` will enter inspection mode for a symbol. Typing `CTRL-D`
exits inspection mode.

Typing `:q`, `CTRL-D`, or `CTRL-C` will exit the REPL.

Typing `:r` resets the environment.

Typing `:d symbol` dumps the disassembly of a symbol.

Typing `:s filename` will save all of the expressions that were typed and
evaluated to the file specified, in the format:

```lisp
(+ 1 2) ; => 3
(* 6 10) ; => 60
```

Of course, depending on your result, this can result in very long lines, or
break if your result contains newlines. Use at your own peril for now!

## Customization

If you want you can add customizations to `sbcli`. On startup
it will load a file called `.sbclirc` in your home directory
if it exists. You can execute arbitrary code there, two of
the more interesting values to set are `*prompt*` and `*ret*`.
Check out an example resource file
[here](https://github.com/hellerve/sbcli/blob/master/examples/.sbclirc).

<hr/>

Have fun!
