;;; test-helper --- Test helper for annotate

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar annotate-test-path
  (f-dirname (f-this-file)))

(defvar annotate-root-path
  (f-parent annotate-test-path))

(defvar annotate-sandbox-path
  (f-expand "sandbox" annotate-test-path))

(when (f-exists? annotate-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" annotate-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory annotate-sandbox-path))
     (when (f-exists? annotate-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir annotate-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'undercover)
(undercover "*.el" "annotate/*.el"
            (:exclude "*-test.el")
            (:send-report nil)
            (:report-file "/tmp/undercover-report.json"))
(require 'ert)
(require 'annotate)

(provide 'test-helper)
;;; test-helper.el ends here
