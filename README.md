emacs-xkcd
==========

A hackish implementation of an xkcd (http://xkcd.com/) reader for Emacs.

Enable it by adding
```lisp
(add-to-list 'load-path (expand-file-name "/path/to/emacs-xkcd.el"))
(require 'emacs-xkcd)
```
to your .emacs file.
#Screenshot:
![alt text][screen]
[screen]: http://i.imgur.com/q9vzjvr.png "Screenshot of emacs-xkcd"
#Loading up comics:
`xkcd-get` loads up a user-specified comic.

`xkcd-get-latest` loads up the latest xkcd.
# Current keybindings:
| Keybinding | Use                            |  Function      |
|:----------:|:------------------------------:|:--------------:|
| `C-c r`    | Load a random xkcd             | (xkcd-rand)    |
| `C-c r`    | Show alt-text in the minibuffer| (xkcd-alt-text)|
| `<right>`  | Load next xkcd                 | (xkcd-next)    |
| `<left>`   | Load previous xkcd             | (xkcd-prev)    |

#Bugs
Some comics using a different image extensions do not seem to load.

#TODO
Add support for custom faces.
