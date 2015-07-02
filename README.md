Annotate.el
===========

[![Build Status][ts]][tl] [![Coverage Status][cb]][cl] [![MELPA][mi]][m] [![MELPA-STABLE][msi]][ms] [![Tag Version][gtb]][gtl]  [![License][lb]][lf]

This package provides a minor mode `annotate-mode`, which can add annotations to arbitrary files without changing the files themselves. This is very useful for code reviews. When `annotate-mode` is active, `C-c C-a` will create, edit, or delete annotations.

![example-screenshot](https://raw.githubusercontent.com/bastibe/annotate.el/master/img/example.png)

With an active region, `C-c C-a` creates a new annotation for that region. With no active region, `C-c C-a` will create an annotation for the word under point. If point is on an annotated region, `C-c C-a` will edit that annotation instead of creating a new one. Clearing the annotation deletes them.

All annotations are saved in `annotate-file` (`~/.annotations` by default).

Annotations can be exported `annotate-export-annotations` as commented unified diffs, like this:

![diff-example-screenshot](https://raw.githubusercontent.com/bastibe/annotate.el/master/img/diff-example.png)

This package is released under the MIT license.

[mi]: http://melpa.org/packages/annotate-badge.svg
[m]: http://melpa.org/#/annotate
[msi]: http://stable.melpa.org/packages/annotate-badge.svg
[ms]: http://stable.melpa.org/#/annotate
[lb]: http://img.shields.io/:license-mit-blue.svg
[lf]: ./LICENSE
[ts]: https://travis-ci.org/bastibe/annotate.el
[tl]: https://travis-ci.org/bastibe/annotate.el.svg
[cb]: https://coveralls.io/repos/bastibe/annotate.el/badge.svg
[cl]: https://coveralls.io/r/bastibe/annotate.el
[gtb]: https://img.shields.io/github/tag/bastibe/annotate.el.svg
[gtl]: https://github.com/bastibe/annotate.el/tags
