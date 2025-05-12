;; -*- mode: eask; lexical-binding: t -*-

(package "annotate"
         "2.4.1"
         "annotate files without changing them")

(website-url "https://github.com/bastibe/annotate.el")

(package-file "annotate.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)

(depends-on "emacs" "27.1")

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)))
