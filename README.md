A simple Emacs extension for the [ghcid](https://github.com/ndmitchell/ghcid) Haskell tool.

This package runs ghcid in a `comint-mode` derived buffer (`ghcid-mode`), output is colorized, the buffer is automatically truncated, and the point follows new output on reloads.

The ghcid repository contains its own [ghcid.el](https://github.com/ndmitchell/ghcid/blob/master/plugins/emacs/ghcid.el), however it only works for stack projects, doesn't work on the latest Emacs version, and is not currently being maintained.

# Interactive Commands

#### ghcid-start

Start or restart ghcid for the current project (see [Project Selection](#Project-Selection)).

#### ghcid-pop-to-buffer

Pops to the ghcid buffer for the current project if it exists, and errors out if it doesn’t.

#### ghcid-pop-to-buffer-or-start

Pops to the ghcid buffer for the current project if it exists, otherwise creates the ghcid buffer and pops to it.

# Project Selection

ghcid.el chooses a working directory (and thus a buffer name) based on the `ghcid-project-system` variable, falling back to the current `default-directory` when no project root can be determined. The `ghcid-project-system` variable may be one of four values:

* `auto-detect` (default): if projectile is loaded, use projectile, otherwise use project.el.
* `project`: use project.el by calling
* `projectile`: use Projectile
* `none`: disable project detection and just use the current `default-directory`.

The chosen directory is stored as the ghcid buffer’s `default-directory` when you run M-x ghcid-start.

# Buffer Naming

By default, ghcid buffers are named per project (example: `*ghcid cool-haskell-project*`). The name of the project is the basename of the chosen project directory.

If you are not in a project or project detection fails, it falls back to `ghcid-default-buffer-name` (default: `*ghcid*`).

# Build System Detection

When `ghcid-start` runs, the ghcid `--command` argument is built automatically by inspecting the project directory.

* If a stack.yaml file exists, it assumes a Stack project and uses `stack ghci ...`
* If a .ghci file exists, it assumes a plain GHCi project and uses `ghci ...`
* Otherwise it is assumed to be a cabal project and uses `cabal repl ...`

This is essentially the same heuristic used by ghcid itself if the `--command` argument is omitted.

# Customizing The ghcid Command

The argument list for ghcid is constructed from two layers of options, ghcid options and GHCi options.

#### ghcid options

The user can specify ghcid options via the variables `ghcid-default-opts` (default: `(list "--reverse-errors" "--color=always")`) and `ghcid-extra-opts` (default: `nil`).

In general, `ghcid-default-opts` is intended to be set by the user to stable defaults. Alternately `ghcid-extra-opts` is intended for programmatic or per-invocation adjustments.

The `--reverse-errors` option is enabled by default because without it ghcid’s output tends to interact poorly with a comint buffer.

Never specify a `--command` option in these variables. That is the job of the [GHCi options](#ghci-options).

#### GHCi options

The user can specify options to ghcid's underlying GHCi invocation via the variables `ghcid-ghci-default-opts` (default: `(list "-ferror-spans" "-fdiagnostics-color=always"))` and `ghcid-ghci-extra-opts` (default: `nil`). These variables are used to build the ghcid `--command` argument.

In general, `ghcid-ghci-default-opts` is intended to be set by the user to stable defaults. Alternately `ghcid-ghci-extra-opts` is intended for programmatic or per-invocation adjustments.
