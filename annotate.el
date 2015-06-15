;;; annotate.el --- annotate files without changing them
;; Copyright (C) 2015 Bastian Bechtold

;; Author: Bastian Bechtold
;; Maintainer: Bastian Bechtold
;; URL: https://github.com/bastibe/annotate.el
;; Created: 2015-06-10
;; Version: 0.1.2

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

;;; Code:
(require 'cl)

;;;###autoload
(defgroup annotate nil
  "Annotate files without changing them."
  :version "0.1.2"
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
            (destructuring-bind (start end) (annotate-bounds)
               (annotate-create-annotation start end))))))

;;;###autoload
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
    (annotate-dump-annotation-data all-annotations)
    (message "Annotations saved.")))

;;;###autoload
(defun annotate-load-annotations ()
  "Load all annotations from disk."
  (interactive)
  (let ((annotations (cdr (assoc-string (buffer-file-name)
                                        (annotate-load-annotation-data)))))
    ;; remove empty annotations created by earlier bug:
    (setq annotations (remove-if (lambda (ann) (eq (nth 2 ann) nil))
                                 annotations))
    (when (eq nil annotations)
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
            (let ((prefix (make-string (- annotate-annotation-column
                                          (annotate-line-length)) ? )))
              (put-text-property (point)
                                 (1+ (point))
                                 'display
                                 (concat prefix text "\n"))))))
      (message "Annotations loaded."))))

;;;###autoload
(defun annotate-clear-annotations ()
  "Clear all current annotations."
  (interactive)
  (let ((highlights
         (overlays-in 0 (buffer-size))))
    (save-excursion
      (dolist (highlight highlights)
        (goto-char (overlay-end highlight))
        (move-end-of-line nil)
        (delete-overlay highlight)
        (remove-text-properties (point) (1+ (point)) '(display nil))))))

(defun annotate-create-annotation (start end)
  "Create a new annotation for selected region."
  (let ((annotation (read-from-minibuffer "Annotation: "))
        (prefix (make-string (- annotate-annotation-column
                                (annotate-line-length)) ? )))
    (when (not (or (eq nil annotation) (string= "" annotation)))
      (let ((highlight (make-overlay start end)))
        (overlay-put highlight 'face 'annotate-highlight)
        (overlay-put highlight 'annotation annotation)
        (setq annotation (propertize annotation 'face 'annotate-annotation))
        (save-excursion
          (move-end-of-line nil)
          (put-text-property (point) (1+ (point))
                             'display (concat prefix annotation "\n")))))))

(defun annotate-change-annotation (pos)
  "Change annotation at point. If empty, delete annotation."
  (let* ((highlight (car (overlays-at pos)))
         (annotation (read-from-minibuffer "Annotation: " (overlay-get highlight 'annotation)))
         (prefix (make-string (- annotate-annotation-column (annotate-line-length)) ? )))
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

(defun annotate-describe-annotation (highlight)
  "Return list that describes the overlay `highlight`."
  (list
   (overlay-start highlight)
   (overlay-end highlight)
   (overlay-get highlight 'annotation)))

(defun annotate-load-annotation-data ()
  "Read and return saved annotations."
  (with-temp-buffer
    (when (file-exists-p annotate-file)
      (insert-file-contents annotate-file))
    (end-of-buffer)
    (cond ((= (point) 1) nil)
          (t (beginning-of-buffer)
             (read (current-buffer))))))

(defun annotate-dump-annotation-data (data)
  "Save `data` into annotation file."
  (with-temp-file annotate-file
    (prin1 data (current-buffer))))

(provide 'annotate)
;;; annotate.el ends here
