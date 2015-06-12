(require 'cl)

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

(defun annotate-annotate ()
  "Create, modify, or delete annotation."
  (interactive)
  (let ((overlay (car (overlays-at (point)))))
    (message "%s" (overlayp overlay))
    (cond ((and (overlayp overlay) (overlay-get overlay 'annotation))
            (annotate-change-annotation (point)))
           (t
            (destructuring-bind (start end) (annotate-bounds)
               (annotate-create-annotation start end))))))

(defun annotate-create-annotation (start end)
  "Create a new annotation for selected region."
  (interactive "r")
  (let ((highlight (make-overlay start end))
        (annotation (read-from-minibuffer "Annotation: "))
        (prefix (make-string (- annotate-annotation-column (annotate-line-length)) ? )))
    (when (not (string= "" annotation))
      (overlay-put highlight 'face annotate-highlight-face)
      (overlay-put highlight 'annotation annotation)
      (setq annotation (propertize annotation 'face annotate-annotation-face))
      (save-excursion
        (move-end-of-line nil)
        (put-text-property (point) (1+ (point)) 'display (concat prefix annotation "\n"))))))

(defun annotate-change-annotation (pos)
  "Change annotation at point. If empty, delete annotation."
  (interactive "d")
  (let* ((highlight (car (overlays-at pos)))
         (annotation (read-from-minibuffer "Annotation: " (overlay-get highlight 'annotation)))
         (prefix (make-string (- annotate-annotation-column (annotate-line-length)) ? )))
    (save-excursion
      (move-end-of-line nil)
      (cond
       ((string= "" annotation)
        (delete-overlay highlight)
        (remove-text-properties (point) (1+ (point)) '(display nil)))
       (t
        (overlay-put highlight 'annotation annotation)
        (setq annotation (propertize annotation 'face annotate-annotation-face))
        (put-text-property (point) (1+ (point)) 'display (concat prefix annotation "\n")))))))

(defun annotate-line-length ()
  "The length of the line from beginning to end."
  (save-excursion
    (move-end-of-line nil)
    (let ((eol (point)))
      (move-beginning-of-line nil)
      (- eol (point)))))

(defun annotate-bounds ()
  "The bounds of the region or whatever is at point."
  (list (cond
         ((region-active-p) (region-beginning))
         ((thing-at-point 'symbol) (car (bounds-of-thing-at-point 'symbol)))
         (t (point)))
        (cond
         ((region-active-p) (region-end))
         ((thing-at-point 'symbol) (cdr (bounds-of-thing-at-point 'symbol)))
         (t (1+ (point))))))

(defun annotate-save-annotations ()
  "Save all annotations to disk."
  (interactive)
  (let ((annotations
         (mapcar 'annotate-describe-annotation (overlays-in 0 (buffer-size)))))
    (append-to-file (concat (format "\n:%s\n" (buffer-file-name))
                            (apply 'concat annotations))
                    nil annotate-file)))

(defun annotate-describe-annotation (highlight)
  (save-excursion
    (goto-char (overlay-start highlight))
    (format "%s (line %s, %s-%s): %s\n"
            (prin1-to-string (buffer-substring-no-properties
                              (overlay-start highlight)
                              (overlay-end highlight)))
            (line-number-at-pos)
            (current-column)
            (+ (current-column) (- (overlay-end highlight)
                                   (overlay-start highlight)))
            (prin1-to-string (overlay-get highlight 'annotation)))))
