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

1. Read "`init-config.el`". Make sure the items there are reasonable.
2. Create file "`userinfo.el`" and add personal information.
3. Create file "`custom.el`" and add platform-specific configurations.
4. Run "`make autoloads`" to generate autoloads file for "`./lisp`".
5. Run "`make packages`" to install packages.
6. Run "`make compile`" to byte-compile ELisp files in this directory.

- Steps 2, 3, and 6 are optional.
- For step 2, `calendar-latitude` and `calendar-longitude` are expected.
- Do steps 4 and 6 after modifying "`lisp/*.el`"

## Files

| File Name               | Description           | Must Exist | Version Control |
|-------------------------|-----------------------|-----|-----|
| `init-*.el`             | configuration scripts | YES | YES |
| `custom.el`             | the `custom-file`     | NO  | NO  |
| `userinfo.el`           | user's personal info  | NO  | NO  |
| `themes/*.el`           | themes                | YES | YES |
| `lisp/hek-autoloads.el` | autoloads for `list/` | YES | NO  |
| `lisp/hek-*.el`         | self-written packages | YES | YES |

- "`init-*.el`", "`custome.el`", and "`userinfo.el`" will be loaded by "`init.el`".
- Private data shall be put into "`userinfo.el`",
which is excluded from the version control system.
- Prefix "`hek-`" means "*HardGraphite's Emacs Kit*".

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
