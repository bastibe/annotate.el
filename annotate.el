;;; annotate.el --- annotate files without changing them
;; Copyright (C) 2015 Bastian Bechtold

;; Author: Bastian Bechtold
;; Maintainer: Bastian Bechtold
;; URL: https://github.com/bastibe/annotate.el
;; Created: 2015-06-10
;; Version: 0.4.7

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
  :version "0.4.7"
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

(defcustom annotate-file "~/.annotations"
  "File where annotations are stored."
  :type 'file
  :group 'annotate)

(defface annotate-highlight
  '((t (:underline "coral")))
  "Face for annotation highlights."
  :group 'annotate)

(defface annotate-annotation
  '((t (:background "coral" :foreground "black")))
  "Face for annotations."
  :group 'annotate)

(defcustom annotate-annotation-column 85
  "Where annotations appear."
  :type 'number
  :group 'annotate)

(defcustom annotate-diff-export-context 2
  "How many lines of context to include in diff export."
  :type 'number
  :group 'annotate)

(defcustom annotate-use-messages t
  "Whether status messages may appear in the minibuffer."
  :type 'boolean
  :group 'annotate)

(defcustom annotate-integrate-marker " ANNOTATION: "
  "Marker that is written before every integrated annotation."
  :type 'string
  :group 'annotate)

(defun annotate-initialize ()
  "Load annotations and set up save and display hooks."
  (annotate-load-annotations)
  (add-hook 'after-save-hook 'annotate-save-annotations t t)
  (add-hook 'window-configuration-change-hook 'font-lock-fontify-buffer t t)
  (font-lock-add-keywords
   nil '((annotate--font-lock-matcher (2 (annotate--annotation-builder))
                                      (1 (annotate--change-guard))))))

(defun annotate-shutdown ()
  "Clear annotations and remove save and display hooks."
  (annotate-clear-annotations)
  (remove-hook 'after-save-hook 'annotate-save-annotations t)
  (remove-hook 'window-configuration-change-hook 'font-lock-fontify-buffer t)
  (font-lock-remove-keywords
   nil '((annotate--font-lock-matcher (2 (annotate--annotation-builder))
                                      (1 (annotate--change-guard))))))

(defun annotate-annotate ()
  "Create, modify, or delete annotation."
  (interactive)
  (let ((overlay (car (overlays-at (point)))))
    (cond ((and (overlayp overlay) (overlay-get overlay 'annotation))
           (annotate-change-annotation (point)))
          (t
           (cl-destructuring-bind (start end) (annotate-bounds)
             (annotate-create-annotation start end)))))
  (font-lock-fontify-block 1)
  (set-buffer-modified-p t))

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

(defun annotate-save-annotations ()
  "Save all annotations to disk."
  (interactive)
  (let ((file-annotations (annotate-describe-annotations))
        (all-annotations (annotate-load-annotation-data))
        (filename (substring-no-properties (or (buffer-file-name) ""))))
    (if (assoc-string filename all-annotations)
        (setcdr (assoc-string filename all-annotations)
                file-annotations)
      (setq all-annotations
            (push (cons filename file-annotations)
                  all-annotations)))
    ;; remove duplicate entries (a user reported seeing them)
    (dolist (entry all-annotations)
      (delete-dups entry))
    ;; skip files with no annotations
    (annotate-dump-annotation-data (cl-remove-if
                                    (lambda (entry)
                                      (eq nil (cdr entry)))
                                    all-annotations))
    (if annotate-use-messages
        (message "Annotations saved."))))

(defun annotate-integrate-annotations ()
  "Write all annotations into the file as comments below the annotated line.
An example might look like this:"
  (interactive)
  (save-excursion
    (dolist (ov (sort (overlays-in 0 (buffer-size))
                      (lambda (o1 o2)
                        (< (overlay-start o1) (overlay-start o2)))))
      (goto-char (overlay-start ov))
      (cond
       ;; overlay spans more than one line
       ((string-match "\n" (buffer-substring (overlay-start ov)
                                             (overlay-end ov)))
        ;; partially underline first line
        (let ((ov-start (point))
              (bol (progn (beginning-of-line)
                          (point)))
              (eol (progn (end-of-line)
                          (point))))
          (end-of-line)
          (insert "\n" comment-start
                  (make-string (max 0 (- ov-start bol (length comment-start))) ? )
                  (make-string (max 0 (- eol ov-start)) ?~)))
        ;; fully underline second to second-to-last line
        (while (< (progn (forward-line)
                         (end-of-line)
                         (point)) (overlay-end ov))
          (let ((bol (progn (beginning-of-line)
                            (point)))
                (eol (progn (end-of-line)
                            (point))))
            (end-of-line)
            (insert "\n" comment-start
                    (make-string (max 0 (- eol bol (length comment-start))) ?~))))
        ;; partially underline last line
        (let ((bol (progn (beginning-of-line)
                          (point)))
              (ov-end (overlay-end ov)))
          (end-of-line)
          (insert "\n" comment-start
                  (make-string (max 0 (- ov-end bol (length comment-start))) ?~)))
        ;; insert actual annotation text
        (insert "\n" comment-start annotate-integrate-marker (overlay-get ov 'annotation)))
       ;; overlay is within one line
       (t
        (let ((ov-start (overlay-start ov))
              (ov-end (overlay-end ov))
              (bol (progn (beginning-of-line)
                          (point))))
          (end-of-line)
          (insert "\n" comment-start
                  (make-string (max 0 (- ov-start bol (length comment-start))) ? )
                  (if (= bol ov-start)
                      (make-string (max 0 (- ov-end ov-start 1)) ?~)
                    (make-string (max 0 (- ov-end ov-start)) ?~))
                    "\n" comment-start annotate-integrate-marker (overlay-get ov 'annotation)))))
      (remove-text-properties
         (point) (1+ (point)) '(display nil)))))

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
  (let* ((filename (substring-no-properties (or (buffer-file-name) "")))
         (export-buffer (generate-new-buffer (concat
                                              filename
                                             ".annotations.diff")))
        (annotations (annotate-describe-annotations)))
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

(defun annotate--font-lock-matcher (limit)
  "Finds the next annotation. Matches two areas:
- the area between the overlay and the annotation
- the newline that will display the annotation

The first match will get `annotate--change-guard` as its
`insert-in-front-hook`, to make sure that if a newline is inserted
between the overlay and the annotation, the `display` property of
the newline is properly disposed of.

The second match will get `annotate--annotation-builder` as its
`display` property, which makes the newline look like an
annotation plus the newline."
  (goto-char (next-overlay-change (point)))
  (if (>= (point) limit)
      nil ; no match found before limit
    (progn
      ;; go to the end of the longest overlay under point
      (let ((overlays (sort (overlays-at (point))
                            (lambda (x y)
                              (> (overlay-end x) (overlay-end y))))))
        (if overlays
            (goto-char (overlay-end (car overlays)))))
      ;; capture the area from the overlay to EOL for the modification guard
      ;; and the newline itself for the annotation.
      (re-search-forward "\\(.*\\(\n\\)\\)")
      t)))

(defun annotate-lineate (text line-width)
  "Breaks `text` into lines to fit in the annotation space"
  (let ((available-width (- (window-body-width)
                            annotate-annotation-column))
        ;; if the annotation won't fit at the end of the line:
        (lineated (if (< line-width annotate-annotation-column) "" "\n"))
        (current-pos 0))
    (while (< current-pos (length text))
      (let ((current-line
             (substring text current-pos
                        (min (length text)
                             (+ current-pos available-width -1)))))
        ;; discard characters until the string fits within the available width
        ;; this can happen with unicode characters that are wider than one col
        (while (> (string-width current-line) available-width)
          (setq current-line (substring current-line 0 -1)))
        ;; strip partial last word if necessary, for word wrap:
        (when (and (string-match "[^ ]$" current-line)
                   (< (+ current-pos (length current-line)) (length text)))
          (string-match "[ ][^ ]+$" current-line)
          (setq current-line (replace-match " " nil nil current-line)))
        ;; append white space to the end of continued lines
        (let ((postfix (if (< (length current-line) (length text))
                           (make-string (- available-width (string-width current-line) 1) ? )
                         "")))
          (setq lineated (concat lineated current-line postfix "\n")
                current-pos (+ current-pos (length current-line))))))
    ;; strip trailing newline, if any
    (if (string= (substring lineated (1- (length lineated))) "\n")
        (substring lineated 0 (1- (length lineated)))
      lineated)))

(defun annotate--annotation-builder ()
  "Searches the line before point for annotations, and returns a
`facespec` with the annotation in its `display` property."
  (save-excursion
    (goto-char (1- (point))) ; we start at the start of the next line
    ;; find overlays in the preceding line
    (let ((prefix (annotate-make-prefix)) ; white space before first annotation
          (bol (progn (beginning-of-line) (point)))
          (eol (progn (end-of-line) (point)))
          (text "")
          (overlays nil))
      ;; include previous line if point is at bol:
      (when (eq nil (overlays-in bol eol))
        (setq bol (1- bol)))
      (setq overlays (sort (overlays-in bol eol)
                           (lambda (x y)
                             (< (overlay-end x) (overlay-end y)))))
      ;; put each annotation on its own line
      (dolist (ov overlays)
        (if (overlay-get ov 'annotation)
            (dolist (l (save-match-data
                         (split-string
                          (annotate-lineate (overlay-get ov 'annotation)
                                            (- eol bol)) "\n")))
              (setq text
                    (concat text prefix
                            (propertize l 'face 'annotate-annotation)
                            "\n"))
              ;; white space before for all but the first annotation
              (setq prefix (make-string annotate-annotation-column ? )))))
      ;; build facecpec with the annotation text as display property
      (if (string= text "")
          ;; annotation has been removed: remove display prop
          (list 'face 'default 'display nil)
        ;; annotation has been changed/added: change/add display prop
        (list 'face 'default 'display text)))))

(defun annotate--remove-annotation-property (begin end)
  "Cleans up annotation properties associated with a region."
  ;; inhibit infinite loop
  (setq inhibit-modification-hooks t)
  ;; inhibit property removal to the undo list
  (buffer-disable-undo)
  (save-excursion
    (goto-char end)
    ;; go to the EOL where the
    ;; annotated newline used to be
    (end-of-line)
    ;; strip dangling display property
    (remove-text-properties
     (point) (1+ (point)) '(display nil)))
  (buffer-enable-undo)
  (setq inhibit-modification-hooks nil))

(defun annotate--change-guard ()
  "Returns a `facespec` with an `insert-behind-hooks` property
that strips dangling `display` properties of text insertions if
text is inserted. This cleans up after newline insertions between
an overlay and it's annotation."
  (list 'face nil
        'insert-in-front-hooks '(annotate--remove-annotation-property)))

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

(defun annotate-load-annotations ()
  "Load all annotations from disk."
  (interactive)
  (let ((annotations (cdr (assoc-string
                           (substring-no-properties (or (buffer-file-name) ""))
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
          (let ((start (nth 0 annotation))
                (end (nth 1 annotation))
                (text (nth 2 annotation)))
            (annotate-create-annotation start end text)))))
    (set-buffer-modified-p modified-p)
    (font-lock-fontify-buffer)
    (if annotate-use-messages
        (message "Annotations loaded."))))

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
    (dolist (ov overlays)
      (annotate--remove-annotation-property
       (overlay-start ov)
       (overlay-end ov))
      (delete-overlay ov))
    (set-buffer-modified-p modified-p)))

(defun annotate-create-annotation (start end &optional text)
  "Create a new annotation for selected region."
  (let ((annotation (or text (read-from-minibuffer "Annotation: "))))
    (when (not (or (eq nil annotation) (string= "" annotation)))
      (let ((highlight (make-overlay start end)))
        (overlay-put highlight 'face 'annotate-highlight)
        (overlay-put highlight 'annotation annotation))
      (if (use-region-p) (deactivate-mark))))
  (save-excursion
    (goto-char end)
    (font-lock-fontify-block 1)))

(defun annotate-change-annotation (pos)
  "Change annotation at point. If empty, delete annotation."
  (let* ((highlight (car (overlays-at pos)))
         (annotation (read-from-minibuffer
                      "Annotation: "
                      (overlay-get highlight 'annotation))))
    (save-excursion
      (goto-char (overlay-end highlight))
      (move-end-of-line nil)
      (cond
       ;; annotation was cancelled:
       ((eq nil annotation))
       ;; annotation was erased:
       ((string= "" annotation)
        (annotate--remove-annotation-property
         (overlay-start highlight)
         (overlay-end highlight))
        (delete-overlay highlight))
       ;; annotation was changed:
       (t (overlay-put highlight 'annotation annotation))))))

(defun annotate-make-prefix ()
  "An empty string from the end of the line upto the annotation."
  (save-excursion
    (let* ((line-text (buffer-substring
                       (progn (beginning-of-line) (point))
                       (progn (end-of-line) (point))))
           (prefix-length (- annotate-annotation-column (string-width line-text))))
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
