Annotate.el [![MELPA][mi]][m] [![MELPA-STABLE][msi]][ms]
===========

[mi]: http://melpa.org/packages/annotate-badge.svg
[m]: http://melpa.org/#/annotate
[msi]: http://stable.melpa.org/packages/annotate-badge.svg
[ms]: http://stable.melpa.org/#/annotate

This package provides a minor mode `annotate-mode`, which can add annotations to arbitrary files without changing the files themselves. This is very useful for code reviews. When `annotate-mode` is active, `C-c C-a` will create, edit, or delete annotations. 

![example-screenshot](https://raw.githubusercontent.com/bastibe/annotate.el/master/example.png)

With an active region, `C-c C-a` creates a new annotation for that region. With no active region, `C-c C-a` will create an annotation for the word under point. If point is on an annotated region, `C-c C-a` will edit that annotation instead of creating a new one. Clearing the annotation deletes them.

Use `C-c ]` to jump to the next annotation and `C-c [` to jump to the previous annotation.

All annotations are saved in `annotate-file` (`~/.annotations` by default).

Annotations can be exported `annotate-export-annotations` as commented unified diffs, like this:

![diff-example-screenshot](https://raw.githubusercontent.com/bastibe/annotate.el/master/diff-example.png)

### Incompatibilities:

- you can't annotate org-mode source code blocks.
- `form-feed-mode`. For unknown reasons, `form-feed-mode` erases all annotations (to be more precise: the `display` text properties of the line feed characters, which is what `annotate` uses to display it's annotations).

This package is released under the MIT license.
