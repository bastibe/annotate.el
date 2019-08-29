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

Alternatively, they can be integrated `annotate-integrate-annotations` as comments into the current buffer, like this:

![integrate-example-screenshot](https://raw.githubusercontent.com/bastibe/annotate.el/master/integrate-example.png)

### Incompatibilities:

- annotations in org-mode source blocks will be underlined, but the annotations don't show up. This is likely a fundamental incompatibility with the way source blocks are highlighted and the way annotations are displayed.

Portion of the code © 2019 Universita' degli Studi di Palermo

This package is released under the MIT license.



### Changelog

- **2015-06-12 V0.1 Bastian Bechtold**
  First working release.

- **2015-06-12 V0.1.1 Bastian Bechtold**
  Improve documentation and add license.

- **2015-06-12 V0.1.2 Bastian Bechtold**
  Fix typo and version error.

- **2015-06-15 V0.1.3 Bastian Bechtold**
  Improve README and auto-remove empty annotations created by earlier bug.

- **2015-06-15 V0.1.4 Bastian Bechtold**
  Minor bug fixes.

- **2015-06-15 V0.1.5 Bastian Bechtold**
  Annotations now work on long lines.

- **2015-06-19 V0.2.0 Bastian Bechtold**
  Annotations can be exported as unified diff files.
  Several smaller bug fixes.

- **2015-06-19 V0.2.1 Bastian Bechtold**
  Now with fewer compile warnings (turns out, not really).

- **2015-06-19 V0.2.2 Bastian Bechtold**
  Now with more compile warnings (0.2.1 didn't work).

- **2015-07-02 V0.2.3 Bastian Bechtold**
  Can now disable minibuffer messages.

- **2015-09-17 V0.2.4 Bastian Bechtold**
  Load and Clear don't mark buffer as modified any more.

- **2015-09-21 V0.3.0 Bastian Bechtold**
  Add key bindings for jumping to next/previous annotation.

- **2015-09-22 V0.3.1 Bastian Bechtold**
  Change key bindings for jumping as to conform with Emacs' standards.
  Didn't change the main key binding though, because I don't know a good alternative.

- **2015-09-23 V0.4.0 Bastian Bechtold**
  Completely reworked the display engine for annotations. You can now have several annotations per line, and annotations should not move any more when editing the line they are on. Finally, annotations can now span several lines.

- **2015-10-06 V0.4.3 Bastian Bechtold**
  Bugfixes. No more hidden newlines, no more annotations in undo-list, no more error messages with annotations at bol, mark deactivated after creating annotation, annotations auto-reflow on frame size change.

- **2015-10-06 V0.4.4 Bastian Bechtold**
  Added a new export system. Let's see if it turns out to be more useful than the previous one.

- **2016-08-25 V0.4.5 Bastian Bechtold**
  Bugfix release for unicode annotations and multiline annotations.

- **2016-09-07 V0.4.6 Bastian Bechtold**
  Bugfix release for annotations ending on an empty line.

- **2016-10-06 V0.4.7 Bastian Bechtold**
  Bugfix release for buffers without a file name.

- **2019-08-29 V0.4.8 cage**
  Bugfix release for annotation on narrow frames.
