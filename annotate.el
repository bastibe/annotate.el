(defgroup annotate nil
  "Annotate files without changing them."
  :version 0.1
  :group 'text)

(defcustom annotate-file "~/.file-annotations"
  "File where annotations are stored."
  :type 'file
  :group 'annotate)

(defcustom annotate-highlight-face 'highlight
  "Face for annotations."
  :type 'face
  :group 'annotate)

(defcustom annotate-annotation-face 'highlight
  "Face for annotations."
  :type 'face
  :group 'annotate)

(defcustom annotate-annotation-column 90
  "Where annotations appear."
  :type 'number
  :group 'annotate)

(defun annotate-create-annotation (start end &optional arg)
  "Create a new annotation for selected region."
  (interactive "r")
  (let ((overlay-highlight (make-overlay start end))
        (annotation (read-from-minibuffer "Annotation: "))
        (prefix (make-string (- annotate-annotation-column (annotate-line-length)) ? )))
    (overlay-put overlay-highlight 'face annotate-highlight-face)
    (setq annotation (propertize annotation 'face annotate-annotation-face))
    (save-excursion
      (move-end-of-line nil)
      (put-text-property (point) (1+ (point)) 'display (concat prefix annotation "\n")))))

(defun annotate-line-length ()
  "The length of the line from beginning to end."
  (save-excursion
    (move-end-of-line nil)
    (let ((eol (point)))
      (move-beginning-of-line nil)
      (- eol (point)))))
