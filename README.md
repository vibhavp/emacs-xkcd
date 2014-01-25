emacs-xkcd
==========

[![Build Status](https://travis-ci.org/vibhavp/emacs-xkcd.png?branch=master)](https://travis-ci.org/vibhavp/emacs-xkcd)

A hackish implementation of an xkcd (http://xkcd.com/) reader for Emacs.

Enable it by adding
```lisp
(add-to-list 'load-path (expand-file-name "/path/to/emacs-xkcd.el"))
(require 'emacs-xkcd)
```
to your .emacs file.
#Screenshot:
![alt text][screen]
[screen]: http://i.imgur.com/x08oyQm.png "Screenshot of emacs-xkcd"
#Loading up comics:
`xkcd-get` loads up a user-specified comic.

Files are cached by default to ~/.emacs.d/xkcd/. This can be changed by changing `xkcd-cache-dir` in the group "xkcd". (customize-group xkcd)

`xkcd-get-latest` loads up the latest xkcd.
# Current keybindings:
| Keybinding | Use                            |  Function      |
|:----------:|:------------------------------:|:--------------:|
| `C-c r`    | Load a random xkcd             | (xkcd-rand)    |
| `C-c t`    | Show alt-text in the minibuffer| (xkcd-alt-text)|
| `<right>`  | Load next xkcd                 | (xkcd-next)    |
| `<left>`   | Load previous xkcd             | (xkcd-prev)    |

#Bugs
Some comics using a different image extension do not to load.

#TODO
Add support for custom faces.
