(require 'cl)

(defgroup annotate nil
  "Annotate files without changing them."
  :version 0.1
  :group 'text)

(defcustom annotate-file "~/.file-annotations"
  "File where annotations are stored."
  :type 'file
  :group 'annotate)

(defface annotate-highlight
  '((t (:underline "coral")))
  "Face for annotation highlights."
  :group 'annotate)

(defcustom annotate-highlight-face 'annotate-highlight
  "Face for annotations."
  :type 'face
  :group 'annotate)

(defface annotate-annotation
  '((t (:background "coral" :foreground "black")))
  "Face for annotations."
  :group 'annotate)

(defcustom annotate-annotation-face 'annotate-annotation
  "Face for annotations."
  :type 'face
  :group 'annotate)

(defcustom annotate-annotation-column 85
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
  (let* ((highlight (car (overlays-at pos)))
         (annotation (read-from-minibuffer "Annotation: " (overlay-get highlight 'annotation)))
         (prefix (make-string (- annotate-annotation-column (annotate-line-length)) ? )))
    (save-excursion
      (goto-char (overlay-end highlight))
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
  (let ((file-annotations
         (mapcar 'annotate-describe-annotation (overlays-in 0 (buffer-size))))
        (all-annotations (annotate-load-annotation-data)))
    (if (assoc-string (buffer-file-name) all-annotations)
        (setcdr (assoc-string (buffer-file-name) all-annotations)
                file-annotations)
      (setq all-annotations
            (push (cons (buffer-file-name) file-annotations)
                  all-annotations)))
    (annotate-dump-annotation-data all-annotations)))

(defun annotate-load-annotations ()
  "Load all annotations from disk."
  (interactive)
  (let ((annotations (cdr (assoc-string (buffer-file-name)
                                        (annotate-load-annotation-data)))))
    (message "%s" annotations)
    (when (not (eq nil annotations))
      (save-excursion
        (dolist (annotation annotations)
          (message "%s" annotation)
          (let* ((start (nth 0 annotation))
                 (end (nth 1 annotation))
                 (text (nth 2 annotation))
                 (highlight (make-overlay start end)))
            (message "%s" annotation)
            (overlay-put highlight 'face annotate-highlight-face)
            (overlay-put highlight 'annotation text)
            (setq text (propertize text 'face annotate-annotation-face))
            (goto-char end)
            (move-end-of-line nil)
            (let ((prefix (make-string (- annotate-annotation-column
                                          (annotate-line-length)) ? )))
              (put-text-property (point)
                                 (1+ (point))
                                 'display
                                 (concat prefix text "\n")))))))))

(defun annotate-describe-annotation (highlight)
  (list
   (overlay-start highlight)
   (overlay-end highlight)
   (overlay-get highlight 'annotation)))

(defun annotate-load-annotation-data ()
  (with-temp-buffer
    (when (file-exists-p annotate-file)
      (insert-file-contents annotate-file))
    (end-of-buffer)
    (cond ((= (point) 1) nil)
          (t (beginning-of-buffer)
             (read (current-buffer))))))

(defun annotate-dump-annotation-data (data)
  (with-temp-file annotate-file
    (prin1 data (current-buffer))))
