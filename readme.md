# Emacs configuration

![GNU Emacs](https://www.gnu.org/software/emacs/images/emacs.png)

This is my configuration directory (`~/.config/emacs/`)
for GNU Emacs editor.

## Files

The "`init-*.el`" files in root directory is the configuration scripts,
which will be loaded by the `init.el` file.
The "`init-lang-*.el`" files in root directory
contains configurations for specific languages,
and the files will be lazily loaded by the "`init-langs.el`" files.

The files under `lisp` directory are packages written by myself
to implement specific functions.
The name prefix "`hek-`" means *HardGraphite's Emacs Kit*.

## Conventions

In `init-*.el` files, there are conventions:

- Configurations are separated by heading title lines,
whose format is like: "`;;;;; {HEADING} ;;;;` ..."
- When using an external package, its name, function and repo/doc URL
shall be recorded in comments starting with "`;;; `"
- Self-defined variables and functions for external packages
shall have names starting with "`+`"

## Tools

A few tools are available in Makefile.
Use command "`make help`" to list it usage.
