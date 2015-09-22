;;; annotate.el --- annotate files without changing them
;; Copyright (C) 2015 Bastian Bechtold

;; Author: Bastian Bechtold
;; Maintainer: Bastian Bechtold
;; URL: https://github.com/bastibe/annotate.el
;; Created: 2015-06-10
;; Version: 0.3.2

;; This file is NOT part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package provides the minor mode annotate-mode, which can add
;; annotations to arbitrary files without changing the files
;; themselves. Annotations are saved in the annotate-file
;; (~/.annotations by default).
;;
;; To add annotations to a file, select a region and hit C-c C-a. The
;; region will be underlined, and the annotation will be displayed in
;; the right margin. Annotations are saved whenever the file is saved.
;;
;; Use C-c ] to jump to the next annotation and C-c [ to jump to
;; the previous annotation. Use M-x annotate-export-annotations to
;; save annotations as a no-difference diff file.

;;; Code:
(require 'cl-lib)

;;;###autoload
(defgroup annotate nil
  "Annotate files without changing them."
  :version "0.3.2"
  :group 'text)

;;;###autoload
(define-minor-mode annotate-mode
  "Toggle Annotate mode."
  :init-value nil
  :lighter " Ann"
  :keymap (make-sparse-keymap)
  :group 'annotate
  :after-hook (if annotate-mode
                  (annotate-initialize)
                (annotate-shutdown)))

(define-key annotate-mode-map (kbd "C-c C-a") 'annotate-annotate)
(define-key annotate-mode-map (kbd "C-c ]") 'annotate-next-annotation)
(define-key annotate-mode-map (kbd "C-c [") 'annotate-previous-annotation)

;;;###autoload
(defcustom annotate-file "~/.annotations"
  "File where annotations are stored."
  :type 'file
  :group 'annotate)

;;;###autoload
(defface annotate-highlight
  '((t (:underline "coral")))
  "Face for annotation highlights."
  :group 'annotate)

;;;###autoload
(defface annotate-annotation
  '((t (:background "coral" :foreground "black")))
  "Face for annotations."
  :group 'annotate)

;;;###autoload
(defcustom annotate-annotation-column 85
  "Where annotations appear."
  :type 'number
  :group 'annotate)

;;;###autoload
(defcustom annotate-diff-export-context 2
  "How many lines of context to include in diff export."
  :type 'number
  :group 'annotate)

;;;###autoload
(defcustom annotate-use-messages t
  "Whether status messages may appear in the minibuffer."
  :type 'boolean
  :group 'annotate)

(defun annotate-initialize ()
  "Load annotations and set up save hook."
  (annotate-load-annotations)
  (add-hook 'after-save-hook 'annotate-save-annotations t t))

(defun annotate-shutdown ()
  "Clear annotations and remove save hook."
  (annotate-clear-annotations)
  (remove-hook 'after-save-hook 'annotate-save-annotations t))

;;;###autoload
(defun annotate-annotate ()
  "Create, modify, or delete annotation."
  (interactive)
  (let ((overlay (car (overlays-at (point)))))
    (cond ((and (overlayp overlay) (overlay-get overlay 'annotation))
           (annotate-change-annotation (point)))
          (t
           (cl-destructuring-bind (start end) (annotate-bounds)
             (annotate-create-annotation start end))))))

;;;###autoload
(defun annotate-next-annotation ()
  "Move point to the next annotation."
  (interactive)
  ;; get all following overlays
  (let ((overlays
         (overlays-in (point) (buffer-size))))
    ;; skip overlays not created by annotate.el
    (setq overlays (cl-remove-if
                    (lambda (ov)
                      (eq nil (overlay-get ov 'annotation)))
                    overlays))
    ;; skip properties under point
    (dolist (current (overlays-at (point)))
      (setq overlays (remove current overlays)))
    ;; sort overlays ascending
    (setq overlays (sort overlays (lambda (x y)
                                    (< (overlay-start x) (overlay-start y)))))
    (if (eq nil overlays)
        (message "No further annotations.")
      ;; jump to first overlay list
      (goto-char (overlay-start (nth 0 overlays))))))

;;;###autoload
(defun annotate-previous-annotation ()
  "Move point to the previous annotation."
  (interactive)
  ;; get all previous overlays
  (let ((overlays
         (overlays-in 0 (point))))
    ;; skip overlays not created by annotate.el
    (setq overlays (cl-remove-if
                    (lambda (ov)
                      (eq nil (overlay-get ov 'annotation)))
                    overlays))
    ;; sort overlays descending
    (setq overlays (sort overlays (lambda (x y)
                                    (> (overlay-start x) (overlay-start y)))))
    (if (eq nil overlays)
        (message "No previous annotations.")
      ;; jump to first overlay in list
      (goto-char (overlay-start (nth 0 overlays))))))

;;;###autoload
(defun annotate-save-annotations ()
  "Save all annotations to disk."
  (interactive)
  (let ((file-annotations (annotate-describe-annotations))
        (all-annotations (annotate-load-annotation-data)))
    (if (assoc-string (buffer-file-name) all-annotations)
        (setcdr (assoc-string (buffer-file-name) all-annotations)
                file-annotations)
      (setq all-annotations
            (push (cons (buffer-file-name) file-annotations)
                  all-annotations)))
    (annotate-dump-annotation-data all-annotations)
    (if annotate-use-messages
        (message "Annotations saved."))))

;;;###autoload
(defun annotate-export-annotations ()
  "Export all annotations as a unified diff file.
An example might look like this:

--- /home/bastibe/Projects/annotate.el/annotate.el	2015-06-19 15:13:36.718796738 +0200
+++ /home/bastibe/Projects/annotate.el/annotate.el	2015-06-19 15:13:36.718796738 +0200
@@ -73,5 +73,5 @@
 ;;;###autoload
 (defface annotate-highlight
-  '((t (:underline \"coral\")))
+  '((t (:underline \"coral\")))
#        ~~~~~~~~~~~~~~~~~~
#        this doesn't work in cli
   \"Face for annotation highlights.\"
   :group 'annotate)

This diff does not contain any changes, but highlights the
annotation, and can be conveniently viewed in diff-mode."
  (interactive)
  (let ((export-buffer (generate-new-buffer (concat
                                             (buffer-file-name)
                                             ".annotations.diff")))
        (annotations (annotate-describe-annotations))
        (filename (buffer-file-name)))
    ;; write the diff file description
    (with-current-buffer export-buffer
      (let ((time-string
             (format-time-string "%F %H:%M:%S.%N %z"
                                 (nth 5 (file-attributes filename 'integer)))))
        (insert "--- " filename "\t" time-string "\n")
        (insert "+++ " filename "\t" time-string "\n")))
    ;; write diff, highlight, and comment for each annotation
    (save-excursion
      ;; sort annotations by location in the file
      (dolist (ann (sort annotations (lambda (a1 a2)
                                       (< (car a1) (car a2)))))
        (let* ((start (nth 0 ann))
               (end (nth 1 ann))
               (text (nth 2 ann))
               ;; beginning of first annotated line
               (bol (progn (goto-char start)
                           (beginning-of-line)
                           (point)))
               ;; end of last annotated line
               (eol (progn (goto-char end)
                           (end-of-line)
                           (point)))
               ;; all lines that contain annotations
               (annotated-lines (buffer-substring bol eol))
               ;; context lines before the annotation
               (previous-lines (annotate-context-before start))
               ;; context lines after the annotation
               (following-lines (annotate-context-after end))
               ;; line header for diff chunk
               (diff-range (annotate-diff-line-range start end)))
          (with-current-buffer export-buffer
            (insert "@@ " diff-range " @@\n")
            (insert (annotate-prefix-lines " " previous-lines))
            (insert (annotate-prefix-lines "-" annotated-lines))
            ;; loop over annotation lines and insert with highlight
            ;; and annotation text
            (let ((annotation-line-list
                   (butlast (split-string
                             (annotate-prefix-lines "+" annotated-lines)
                             "\n"))))
              (cond
               ;; annotation has only one line
               ((= (length annotation-line-list) 1)
                (insert (car annotation-line-list) "\n")
                (unless (string= (car annotation-line-list) "+")
                  (insert "#"
                          (make-string (- start bol) ? )
                          (make-string (- end start) ?~)
                          "\n"))
                (insert "#" (make-string (- start bol) ? ) text "\n"))
               ;; annotation has more than one line
               (t
                (let ((line (car annotation-line-list))) ; first line
                  ;; first diff line
                  (insert line "\n")
                  ;; underline highlight (from start to eol)
                  (unless (string= line "+") ; empty line
                    (insert "#"
                            (make-string (- start bol) ? )
                            (make-string (- (length line) (- start bol)) ?~)
                            "\n")))
                (dolist (line (cdr (butlast annotation-line-list))) ; nth line
                  ;; nth diff line
                  (insert line "\n")
                  ;; nth underline highlight (from bol to eol)
                  (unless (string= line "+")
                    (insert "#" (make-string (length line) ?~) "\n")))
                (let ((line (car (last annotation-line-list))))
                  ;; last diff line
                  (insert line "\n")
                  ;; last underline highlight (from bol to end)
                  (unless (string= line "+")
                    (insert "#"
                            (make-string (- (length line) (- eol end) 1) ?~)
                            "\n")))
                ;; annotation text
                (insert "#" text "\n"))))
            (insert (annotate-prefix-lines " " following-lines))))))
          (switch-to-buffer export-buffer)
          (diff-mode)
          (view-mode)))

(defun annotate-context-before (pos)
  "Context lines before POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let ((bol (point)))
      (beginning-of-line (- (1- annotate-diff-export-context)))
      (buffer-substring-no-properties (point) (1- bol)))))

(defun annotate-context-after (pos)
  "Context lines after POS."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (let ((eol (point)))
      (end-of-line (1+ annotate-diff-export-context))
      (buffer-substring-no-properties (1+ eol) (point)))))

(defun annotate-prefix-lines (prefix text)
  "Prepend PREFIX to each line in TEXT."
  (let ((lines (split-string text "\n")))
    (apply 'concat (mapcar (lambda (l) (concat prefix l "\n")) lines))))

(defun annotate-diff-line-range (start end)
  "Calculate diff-like line range for annotation."
  (let ((start-line (line-number-at-pos start))
        (diff-size (+ (* 2 annotate-diff-export-context)
                      (1+ (- (line-number-at-pos end) (line-number-at-pos start))))))
  (format "-%i,%i +%i,%i" start-line diff-size start-line diff-size)))

;;;###autoload
(defun annotate-load-annotations ()
  "Load all annotations from disk."
  (interactive)
  (let ((annotations (cdr (assoc-string (buffer-file-name)
                                        (annotate-load-annotation-data))))
        (modified-p (buffer-modified-p)))
    ;; remove empty annotations created by earlier bug:
    (setq annotations (cl-remove-if (lambda (ann) (eq (nth 2 ann) nil))
                                 annotations))
    (when (and (eq nil annotations) annotate-use-messages)
      (message "No annotations found."))
    (when (not (eq nil annotations))
      (save-excursion
        (dolist (annotation annotations)
          (let* ((start (nth 0 annotation))
                 (end (nth 1 annotation))
                 (text (nth 2 annotation))
                 (highlight (make-overlay start end)))
            (overlay-put highlight 'face 'annotate-highlight)
            (overlay-put highlight 'annotation text)
            (setq text (propertize text 'face 'annotate-annotation))
            (goto-char end)
            (move-end-of-line nil)
            (let ((prefix (annotate-make-prefix)))
              (put-text-property (point)
                                 (1+ (point))
                                 'display
                                 (concat prefix text "\n"))))))
      (set-buffer-modified-p modified-p)
      (if annotate-use-messages
          (message "Annotations loaded.")))))

;;;###autoload
(defun annotate-clear-annotations ()
  "Clear all current annotations."
  (interactive)
  (let ((overlays
         (overlays-in 0 (buffer-size)))
        (modified-p (buffer-modified-p)))
    ;; only remove annotations, not all overlays
    (setq overlays (cl-remove-if
                    (lambda (ov)
                      (eq nil (overlay-get ov 'annotation)))
                    overlays))
    (save-excursion
      (dolist (ov overlays)
        (goto-char (overlay-end ov))
        (move-end-of-line nil)
        (delete-overlay ov)
        (remove-text-properties (point) (1+ (point)) '(display nil))))
    (set-buffer-modified-p modified-p)))

(defun annotate-create-annotation (start end)
  "Create a new annotation for selected region."
  (let ((annotation (read-from-minibuffer "Annotation: "))
        (prefix (annotate-make-prefix)))
    (when (not (or (eq nil annotation) (string= "" annotation)))
      (let ((highlight (make-overlay start end)))
        (overlay-put highlight 'face 'annotate-highlight)
        (overlay-put highlight 'annotation annotation)
        (setq annotation (propertize annotation 'face 'annotate-annotation))
        (save-excursion
          (goto-char (max start end))
          (move-end-of-line nil)
          (put-text-property (point) (1+ (point))
                             'display (concat prefix annotation "\n")))))))

(defun annotate-change-annotation (pos)
  "Change annotation at point. If empty, delete annotation."
  (let* ((highlight (car (overlays-at pos)))
         (annotation (read-from-minibuffer "Annotation: " (overlay-get highlight 'annotation)))
         (prefix (annotate-make-prefix)))
    (save-excursion
      (goto-char (overlay-end highlight))
      (move-end-of-line nil)
      (cond
       ;; annotation was cancelled:
       ((eq nil annotation))
       ;; annotation was erased:
       ((string= "" annotation)
        (delete-overlay highlight)
        (remove-text-properties (point) (1+ (point)) '(display nil)))
       ;; annotation was changed:
       (t
        (overlay-put highlight 'annotation annotation)
        (setq annotation (propertize annotation 'face 'annotate-annotation))
        (put-text-property (point) (1+ (point)) 'display (concat prefix annotation "\n")))))))

(defun annotate-make-prefix ()
  "An empty string from the end of the line upto the annotation."
  (save-excursion
    (move-end-of-line nil)
    (let ((eol (point))
          (prefix-length nil))
      (move-beginning-of-line nil)
      (setq prefix-length (- annotate-annotation-column (- eol (point))))
      (if (< prefix-length 2)
          (make-string 2 ? )
        (make-string prefix-length ? )))))

(defun annotate-bounds ()
  "The bounds of the region or whatever is at point."
  (list (cond
         ((use-region-p) (region-beginning))
         ((thing-at-point 'symbol) (car (bounds-of-thing-at-point 'symbol)))
         (t (point)))
        (cond
         ((use-region-p) (region-end))
         ((thing-at-point 'symbol) (cdr (bounds-of-thing-at-point 'symbol)))
         (t (1+ (point))))))

(defun annotate-describe-annotations ()
  "Return a list of all annotations in the current buffer."
  (let ((overlays (overlays-in 0 (buffer-size))))
    ;; skip non-annotation overlays
    (setq overlays
          (cl-remove-if
           (lambda (ov)
             (eq nil (overlay-get ov 'annotation)))
           overlays))
    (mapcar (lambda (ov)
              (list (overlay-start ov)
                    (overlay-end ov)
                    (overlay-get ov 'annotation)))
            overlays)))

(defun annotate-load-annotation-data ()
  "Read and return saved annotations."
  (with-temp-buffer
    (when (file-exists-p annotate-file)
      (insert-file-contents annotate-file))
    (goto-char (point-max))
    (cond ((= (point) 1) nil)
          (t (goto-char (point-min))
             (read (current-buffer))))))

(defun annotate-dump-annotation-data (data)
  "Save `data` into annotation file."
  (with-temp-file annotate-file
    (prin1 data (current-buffer))))

(provide 'annotate)
;;; annotate.el ends here
