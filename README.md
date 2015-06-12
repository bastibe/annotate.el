Annotate.el
===========

A minor mode that can add non-destructive annotations to any file. When `annotate-mode` is active, `C-c C-a` will create, edit, or delete annotations. 

![screenshot](https://raw.githubusercontent.com/bastibe/annotate.el/master/example.png)

With an active region, `C-c C-a` creates a new annotation for that region. With no active region, `C-c C-a` will create an annotation for the word under point. If point is on an annotated region, `C-c C-a` will edit that annotation instead of creating a new one. Clearing the annotation deletes them.

This package is released under the MIT license.
