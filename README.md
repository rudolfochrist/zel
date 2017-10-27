[![MELPA](https://melpa.org/packages/zel-badge.svg)](https://melpa.org/#/zel) [![Build Status](https://travis-ci.org/rudolfochrist/zel.svg?branch=master)](https://travis-ci.org/rudolfochrist/zel)
# zel.el --- Access frecent files easily

zel tracks the most used files, based on _frecency_.  Zel is basically
a port of [z](https://github.com/rupa/z) in Emacs Lisp.

The list of _frecent_ files underlies two concepts:

1. The files are not only ranked on how recent they have been visited,
   but also how frequent they have been visited.  A file that has been
   visited multiple times last week gets a higher score as file
   visited once yesterday.  Outliers should not compromise the
   _frecent_ list.

2. Entries in the _frecent_ list undergo aging.  If the age of a entry
   falls under a threshold it gets removed from the 'frecent' list.

## Installation

### MELPA

If you installed from MELPA, you're done.

###  use-package

```lisp
(use-package zel
  :ensure t
  :demand t
  :bind (("C-x C-r" . zel-find-file-frecent))
  :config (zel-install))
```

### Manual

Install these required packages:

- [frecency](https://github.com/alphapapa/frecency.el)

Then put `zel.el` in your load-path, and put this in your init
file:

```lisp
(require 'zel)
```

## Usage

1. Run `(zel-install)`
2. Bind `zel-find-file-frecent` to a key,
   e.g. `(global-set-key (kbd "C-x C-r") #'zel-find-file-frecent)`
3. Visit some files to build up the database
4. Profit.

As default the _frecent_ history is saved to `zel-history-file`.
Run `M-x customize-group RET zel` for more customization options.

Besides `zel-find-file-frecent`, that lets you select a file with
`completing-read` and switches to it, there is also the command
`zel-diplay-rankings` that shows all entries of the _frecent_ list
along with their score.

If you'd like to stop building up the _frecent_ list then run
`zel-uninstall` to deregister `zel` from all hooks.

## Credits

- https://github.com/rupa/z
- https://github.com/alphapapa/frecency.el

## License

See [LICENSE](LICENSE)
