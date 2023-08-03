# Emacs configurations

![GNU Emacs](https://www.gnu.org/software/emacs/images/emacs.png)

This is my configuration directory (`~/.config/emacs/`)
for GNU Emacs editor.

## Dependencies

- `GNU Emacs` 29
- `GNU Make`
- various fonts, see `init-config.el`

The followings are optional:

- `ripgrep`: text search tool, used by package `xref`
- `clangd`: language server for C/C++, used by package `eglot`
- `cmake-language-server`: language server for CMake, used by package `eglot`
- `pyright`: language server for Python, used by package `eglot`
- `texlab`: language server for LaTeX, used by package `eglot`
- `pandoc`: markup language converter, used by package `markdown-mode`
- `verilator`: Verilog HDL/SystemVerilog simulator, used by package `verilog-mode`

## How to setup

1. Read `init-config.el`. Make sure the items there are reasonable.
2. Create file `custom.el` and add platform-specific configurations.
3. Run `make get-pkgs` to install packages.
4. Run `make compile` to byte-compile ELisp files in this directory.

## Files

The "`init-*.el`" files in root directory is the configuration scripts,
which will be loaded by the `init.el` file.
They will never be byte-compiled.

The files under `lisp` directory are packages written by myself
to implement specific functions.
The name prefix "`hek-`" means *HardGraphite's Emacs Kit*.

The files under `themes` directory are Emacs themes.

## Conventions

In `init-*.el` files, there are conventions:

- Use file variables: `-*- lexical-binding: t; no-byte-compile: t; -*-`.
- Configurations are separated by heading title lines,
whose format is like: "`;;;;; {HEADING} ;;;;` ..."
- When using an external package, its name, function and repo/doc URL
shall be recorded in comments starting with "`;;; `"
- Self-defined variables and functions for external packages
shall have names starting with "`+`"

## Tools

A few tools are available in Makefile.
Use command "`make help`" to list it usage.
