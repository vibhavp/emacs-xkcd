emacs-xkcd
==========

A hackish implementation of an xkcd (http://xkcd.com/) reader for Emacs.

Enable it by adding
```lisp
(add-to-list 'load-path (expand-file-name "/home/vibhavp/.emacs.d/elisp/emacs-xkcd"))
(require 'emacs-xkcd)
```
to your .emacs file.

# Current keybindings:
| Keybinding | Use                            |  Function      |
|:----------:|:------------------------------:|:--------------:|
| `C-c r`    | Load a random xkcd             | (xkcd-rand)    |
| `C-c r`    | Show alt-text in the minibuffer| (xkcd-alt-text)|
| `<right>`  | Load next xkcd                 | (xkcd-next)    |
| `<left>`   | Load previous xkcd             | (xkcd-prev)    |

#Bugs
Some comics using a different image extensions do not seem to load.