(require 'f)

(defvar annotate.el-support-path
  (f-dirname load-file-name))

(defvar annotate.el-features-path
  (f-parent annotate.el-support-path))

(defvar annotate.el-root-path
  (f-parent annotate.el-features-path))

(add-to-list 'load-path annotate.el-root-path)

(require 'annotate.el)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
