;;; annotate.el --- annotate files without changing them
;; Copyright (C) 2015 Bastian Bechtold and contributors:
;; Naoya Yamashita (2018)
;; Universita' degli Studi di Palermo (2019)

;; Author: Bastian Bechtold
;; Maintainer: Bastian Bechtold
;; URL: https://github.com/bastibe/annotate.el
;; Created: 2015-06-10
;; Version: 0.4.8

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
  :after-hook (annotate-initialize-maybe))

(define-key annotate-mode-map (kbd "C-c C-a") 'annotate-annotate)

(define-key annotate-mode-map (kbd "C-c C-s") 'annotate-show-annotation-summary)

(define-key annotate-mode-map (kbd "C-c ]") 'annotate-next-annotation)

(define-key annotate-mode-map (kbd "C-c [") 'annotate-previous-annotation)

(defcustom annotate-file (locate-user-emacs-file "annotations" ".annotations")
  "File where annotations are stored."
  :type 'file
  :group 'annotate)

(defface annotate-highlight
  '((t (:underline "coral")))
  "Face for annotation highlights."
  :group 'annotate)

(defface annotate-highlight-secondary
  '((t (:underline "turquoise")))
  "Face for secondary annotation highlights."
  :group 'annotate)

(defface annotate-annotation
  '((t (:background "coral" :foreground "black")))
  "Face for annotations."
  :group 'annotate)

(defface annotate-annotation-secondary
  '((t (:background "turquoise" :foreground "black")))
  "Face for secondary annotations."
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

(defcustom annotate-integrate-higlight ?~
  "Character used to underline an annotated text."
  :type 'character
  :group 'annotate)

(defcustom annotate-fallback-comment "#"
  "When variable comment-start is nil use this string instead."
  :type 'string
  :group 'annotate)

(defcustom annotate-blacklist-major-mode '(org-mode)
  "Prevent loading of annotate-mode When the visited file's
major mode is a member of this list (space separated entries)."
  :type  '(repeat symbol)
  :group 'annotate)

(defconst annotate-warn-file-changed-control-string
  (concat "The file '%s' has changed on disk "
          "from the last time the annotations were saved.\n"
          "Chances are that they will not be displayed correctly")
  "The message to warn the user that file has been modified and
  annotations positions could be outdated")

(defconst annotate-summary-list-prefix "    "
  "The string used as prefix for each text annotation item in summary window")

(defconst annotate-summary-list-prefix-file "* File: "
  "The string used as prefix for each annotated file item in summary window")

(defconst annotate-summary-list-prefix-snippet "** Annotated text: "
  "The string used as prefix for each annotation snippet item in summary window")

(defconst annotate-ellipse-text-marker "..."
  "The string used when a string is truncated with an ellipse")

(defun annotate-annotations-exist-p ()
  (cl-find-if 'annotationp
              (overlays-in 0 (buffer-size))))

(defun annotate-initialize-maybe ()
  "Initialize annotate mode only if buffer's major mode is not in the blacklist (see:
'annotate-blacklist-major-mode'"
  (let ((annotate-allowed-p (with-current-buffer (current-buffer)
                              (not (cl-member major-mode annotate-blacklist-major-mode)))))
    (cond
     ((not annotate-allowed-p)
      (annotate-shutdown)
      (setq annotate-mode nil))
     (annotate-mode
      (when (not (annotate-annotations-exist-p))
        (annotate-initialize)))
     (t
      (annotate-shutdown)))))

(cl-defun annotate-buffer-checksum (&optional (object (current-buffer)))
  "Calculate an hash for the argument 'object'."
  (secure-hash 'md5 object))

(cl-defmacro annotate-with-inhibit-modification-hooks (&rest body)
  "Wrap 'body' in a block with modification-hooks inhibited."
  `(unwind-protect
       (progn
         (setf inhibit-modification-hooks t)
         ,@body)
     (setf inhibit-modification-hooks t)))

(defun annotate-end-of-line-pos ()
 "Get the position of the end of line and rewind the point's
postion (so that it is unchanged after this function is called)."
  (save-excursion
    (end-of-line)
    (point)))

(defun annotate-beginning-of-line-pos ()
  "Get the position of the beginning of line and rewind the point's
postion (so that it is unchanged after this function is called)."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun annotate-before-change-fn (a b)
 "This function is added to 'before-change-functions' hook and
it is called any time the buffer content is changed (so, for
example, text is added or deleted). In particular, it will
rearrange the overlays bounds when an annotated text is
modified (for example a newline is inserted)."
  (annotate-with-inhibit-modification-hooks
   (save-excursion
     (let* ((bol (annotate-beginning-of-line-pos))
            (eol (annotate-end-of-line-pos))
            (ov  (cl-remove-if-not 'annotationp
                                   (overlays-in bol eol))))
       (dolist (overlay ov)
         (annotate--remove-annotation-property (overlay-start overlay)
                                               (overlay-end   overlay))
         ;; move the overlay if we are breaking it
         (when  (<= (overlay-start overlay)
                    a
                    (overlay-end overlay))
           (move-overlay overlay (overlay-start overlay) a)))))))

(defun annotate-initialize ()
  "Load annotations and set up save and display hooks."
  (annotate-load-annotations)
  (add-hook 'after-save-hook 'annotate-save-annotations t t)
  (add-hook 'window-configuration-change-hook 'font-lock-fontify-buffer  t t)
  (add-hook 'before-change-functions          'annotate-before-change-fn t t)
  (font-lock-add-keywords
   nil
   '((annotate--font-lock-matcher (2 (annotate--annotation-builder))
                                  (1 (annotate--change-guard))))))

(defun annotate-shutdown ()
  "Clear annotations and remove save and display hooks."
  (annotate-clear-annotations)
  (remove-hook 'after-save-hook                  'annotate-save-annotations t)
  (remove-hook 'window-configuration-change-hook 'font-lock-fontify-buffer  t)
  (remove-hook 'before-change-functions          'annotate-before-change-fn t)
  (font-lock-remove-keywords
   nil
   '((annotate--font-lock-matcher (2 (annotate--annotation-builder))
                                  (1 (annotate--change-guard))))))

(defun annotate-overlay-filled-p (overlay)
  "Does this overlay contains an 'annotation' property?"
  (and overlay
       (overlayp overlay)
       (overlay-get overlay 'annotation)))

(defun annotationp (overlay)
  "Is 'overlay' an annotation?"
  (annotate-overlay-filled-p overlay))

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
        (filename        (substring-no-properties (or (buffer-file-name) ""))))
    (if (assoc-string filename all-annotations)
        (setcdr (assoc-string filename all-annotations)
                (list file-annotations
                        (annotate-buffer-checksum)))
      (setq all-annotations
            (push (list filename
                        file-annotations
                        (annotate-buffer-checksum))
                  all-annotations)))
    ;; remove duplicate entries (a user reported seeing them)
    (dolist (entry all-annotations)
      (delete-dups entry))
    ;; skip files with no annotations
    (annotate-dump-annotation-data (cl-remove-if (lambda (entry)
                                                   (eq nil (cdr entry)))
                                                 all-annotations))
    (if annotate-use-messages
        (message "Annotations saved."))))

(defun annotate-actual-comment-start ()
  "String for comment start related to current buffer's major
mode."
  (or comment-start
      annotate-fallback-comment))

(defun annotate-actual-comment-end ()
  "String for comment ends, if any, related to current buffer's
major mode."
  (or comment-end
      ""))

(defun annotate-comments-length ()
  "Total length of the comment markers (start and end) strings."
  (+ (string-width (annotate-actual-comment-start))
     (string-width (annotate-actual-comment-end))))

(defun annotate-wrap-in-comment (&rest strings)
  "Put comment markers at the start and  (if it makes sense)
end of a string. See: annotate-actual-comment-start and
annotate-actual-comment-end"
  (apply #'concat (append (list (annotate-actual-comment-start))
                          strings
                          (list (annotate-actual-comment-end)))))

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
          (insert "\n"
                  (annotate-wrap-in-comment (make-string (max 0
                                                              (- ov-start
                                                                 bol
                                                                 (annotate-comments-length)))
                                                         ? )
                                            (make-string (max 0 (- eol ov-start))
                                                         annotate-integrate-higlight))))
        ;; fully underline second to second-to-last line
        (while (< (progn (forward-line)
                         (end-of-line)
                         (point))
                  (overlay-end ov))
          (let ((bol (progn (beginning-of-line)
                            (point)))
                (eol (progn (end-of-line)
                            (point))))
            (end-of-line)
            (insert "\n"
                    (annotate-wrap-in-comment (make-string (max 0
                                                                (- eol
                                                                   bol
                                                                   (annotate-comments-length)))
                                                           annotate-integrate-higlight)))))
        ;; partially underline last line
        (let ((bol (progn (beginning-of-line)
                          (point)))
              (ov-end (overlay-end ov)))
          (end-of-line)
          (insert "\n"
                  (annotate-wrap-in-comment (make-string (max 0
                                                              (- ov-end
                                                                 bol
                                                                 (annotate-comments-length)))
                                                         annotate-integrate-higlight))))
        ;; insert actual annotation text
        (insert "\n"
                (annotate-wrap-in-comment annotate-integrate-marker
                                          (overlay-get ov 'annotation))))
       ;; overlay is within one line
       (t
        (let* ((ov-start         (overlay-start ov))
               (ov-end           (overlay-end ov))
               (bol              (progn (beginning-of-line)
                                        (point)))
               (underline-marker (if (= bol ov-start)
                                     (make-string (max 0 (- ov-end ov-start 1))
                                                  annotate-integrate-higlight)
                                   (make-string (max 0 (- ov-end ov-start))
                                                annotate-integrate-higlight))))
          (end-of-line)
          (insert "\n"
                  (annotate-wrap-in-comment (make-string (max 0
                                                              (- ov-start
                                                                 bol
                                                                 (annotate-comments-length)))
                                                         ? )
                                            underline-marker)
                  "\n"
                  (annotate-wrap-in-comment annotate-integrate-marker
                                            (overlay-get ov 'annotation))))))
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
         (export-buffer      (generate-new-buffer (concat
                                                   filename
                                                   ".annotations.diff")))
         (annotations        (annotate-describe-annotations))
         (parent-buffer-mode major-mode))
    ;; write the diff file description
    (with-current-buffer export-buffer
      (funcall parent-buffer-mode)
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
                  (insert (annotate-wrap-in-comment (make-string (- start bol) ? )
                                                    (make-string (- end start)
                                                                 annotate-integrate-higlight))
                          "\n"))
                (insert (annotate-wrap-in-comment (make-string (- start bol) ? )
                                                  text)
                        "\n"))
               ;; annotation has more than one line
               (t
                (let ((line (car annotation-line-list))) ; first line
                  ;; first diff line
                  (insert line "\n")
                  ;; underline highlight (from start to eol)
                  (unless (string= line "+") ; empty line
                    (insert (annotate-wrap-in-comment (make-string (- start bol) ? )
                                                      (make-string (- (length line) (- start bol))
                                                                   annotate-integrate-higlight))
                            "\n")))
                (dolist (line (cdr (butlast annotation-line-list))) ; nth line
                  ;; nth diff line
                  (insert line "\n")
                  ;; nth underline highlight (from bol to eol)
                  (unless (string= line "+")
                    (insert (annotate-wrap-in-comment (make-string (length line)
                                                                   annotate-integrate-higlight))
                            "\n")))
                (let ((line (car (last annotation-line-list))))
                  ;; last diff line
                  (insert line "\n")
                  ;; last underline highlight (from bol to end)
                  (unless (string= line "+")
                    (insert (annotate-wrap-in-comment (make-string (- (length line)
                                                                      (- eol end)
                                                                      1)
                                                                   annotate-integrate-higlight))
                            "\n")))
                ;; annotation text
                (insert (annotate-wrap-in-comment text)
                        "\n"))))
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

(cl-defstruct annotate-group
  words
  start-word)

(defun annotate-group-by-width (text maximum-width)
  "Groups text in a list formed by chunks of maximum size equal
to 'maximum-width'."
  (cl-labels ((next-word (words)
                         (or (cl-first words)
                             ""))
              (join-until-width (words &optional (word nil))
                                (cond
                                 ((null words)
                                  (make-annotate-group :words      nil
                                                       :start-word word))
                                 (t
                                  (let* ((next-word (next-word words))
                                         (new-word  (if word
                                                        (concat word " " next-word)
                                                      next-word)))
                                    (if (<= (string-width new-word)
                                            maximum-width)
                                        (join-until-width (cl-rest words) new-word)
                                      (make-annotate-group :words      words
                                                           :start-word (or word next-word)))))))
              (split-position (text column-max-width)
                              (let ((character-width (length       text))
                                    (column-width    (string-width text)))
                                (if (= character-width column-width)
                                    column-max-width
                                  (let* ((res    0)
                                         (so-far ""))
                                    (cl-loop for i from 0 below column-max-width
                                             until (>= (string-width so-far)
                                                       column-max-width)
                                             do
                                             (setf so-far (concat so-far (string (elt text i))))
                                             (setf res i))
                                    res))))
              (%group (words so-far)
                      (cond
                       ((null words)
                        so-far)
                       ((<= (string-width (cl-first words))
                            maximum-width)
                        (let* ((potential-start (join-until-width words))
                               (word            (annotate-group-start-word potential-start))
                               (nonjoined-words (annotate-group-words potential-start))
                               (next-word       (cl-first nonjoined-words))
                               (rest-words      nonjoined-words)
                               (potential-start word))
                          (%group rest-words
                                  (append (list potential-start)
                                          so-far))))
                       (t
                        (let* ((word           (cl-first words))
                               (rest-words     (cl-rest words))
                               (split-position (split-position word maximum-width))
                               (prefix         (cl-subseq word 0 split-position))
                               (next-word      (if rest-words
                                                   (cl-first rest-words)
                                                 ""))
                               (raw-suffix     (cl-subseq word split-position))
                               (suffix         (if rest-words
                                                   (concat raw-suffix " " next-word)
                                                 raw-suffix)))
                          (%group (append (list suffix)
                                          (cl-rest rest-words))
                                  (append (list prefix)
                                          so-far)))))))
    (if (< maximum-width 1)
        nil
      (let* ((words   (split-string text " " t))
             (grouped (reverse (%group words '()))))
        grouped))))

(cl-defun annotate-safe-subseq (seq from to &optional (value-if-limits-invalid seq))
  "This return 'value-if-limits-invalid' sequence if 'from' or 'to' are invalids"
  (cond
   ((< to from)
    value-if-limits-invalid)
   ((or (< from 0)
        (> from (length seq))
        (> to   (length seq)))
    value-if-limits-invalid)
   (t
    (cl-subseq seq from to))))

(defun annotate-lineate (text line-width)
  "Breaks `text` into lines to fit in the annotation space"
  (cl-labels ((pad (string max-width add-newline-p)
                   (if (null string)
                       ""
                     (let* ((size       (string-width string))
                            (rest-width (max (- max-width
                                                size)
                                             0))
                            (padding    (make-string rest-width
                                                     ? )))
                       (if add-newline-p
                           (concat string padding "\n")
                         (concat string padding)))))
              (%subseq (seq from to)
                       (if (= (length seq) 1)
                           nil
                         (annotate-safe-subseq seq from to nil))))
  (let* ((theoretical-line-width      (- (window-body-width)
                                         annotate-annotation-column))
         (available-width             (if (> theoretical-line-width 0)
                                          theoretical-line-width
                                        line-width))
         (lineated-list               (annotate-group-by-width text available-width))
         (max-width                   (apply #'max
                                             (mapcar #'string-width lineated-list)))
         (all-but-last-lineated-list  (%subseq lineated-list 0 (1- (length lineated-list))))
         (last-line                   (if all-but-last-lineated-list
                                          (car (last lineated-list))
                                        (cl-first lineated-list)))
         (lineated                    (cl-mapcar (lambda (a)
                                                   (pad a max-width t))
                                                 all-but-last-lineated-list)))
    (apply #'concat
           (append lineated
                   (list (pad last-line max-width nil)))))))

(defun annotate--annotation-builder ()
  "Searches the line before point for annotations, and returns a
`facespec` with the annotation in its `display` property."
  (save-excursion
    (goto-char (1- (point)))  ; we start at the start of the next line
    ;; find overlays in the preceding line
    (let ((prefix             (annotate-make-prefix)) ; white spaces before first annotation
          (bol                (progn (beginning-of-line) (point)))
          (eol                (progn (end-of-line) (point)))
          (text               "")
          (overlays           nil)
          (annotation-counter 1))
      ;; include previous line if point is at bol:
      (when (eq nil (overlays-in bol eol))
        (setq bol (1- bol)))
      (setq overlays
            (sort (cl-remove-if-not 'annotationp (overlays-in bol eol))
                  (lambda (x y)
                    (< (overlay-end x) (overlay-end y)))))
      ;; put each annotation on its own line
      (dolist (ov overlays)
        (cl-incf annotation-counter)
        (let ((face           (if (= (cl-rem annotation-counter 2) 0)
                                  'annotate-annotation
                                'annotate-annotation-secondary))
              (face-highlight (if (= (cl-rem annotation-counter 2) 0)
                                  'annotate-highlight
                                'annotate-highlight-secondary)))
          (overlay-put ov 'face face-highlight)
          (dolist (l (save-match-data
                       (split-string (annotate-lineate (overlay-get ov 'annotation)
                                                       (- eol bol))
                                     "\n")))
            (setq text
                  (concat text
                          prefix
                          (propertize l 'face face)
                          "\n"))
            ;; white space before for all but the first annotation line
            (setq prefix (make-string annotate-annotation-column ? )))))
      ;; build facespec with the annotation text as display property
      (if (string= text "")
          ;; annotation has been removed: remove display prop
          (list 'face 'default 'display nil)
        ;; annotation has been changed/added: change/add display prop
        (list 'face 'default 'display text)))))

(defun annotate--remove-annotation-property (begin end)
  "Cleans up annotation properties associated with a region."
  ;; inhibit infinite loop
  (setq inhibit-modification-hooks t)
  ;; copy undo list
  (let ((saved-undo-list (copy-tree buffer-undo-list t)))
    ;; inhibit property removal to the undo list (and empty it too)
    (buffer-disable-undo)
    (save-excursion
      (goto-char end)
      ;; go to the EOL where the
      ;; annotated newline used to be
      (end-of-line)
      ;; strip dangling display property
      (remove-text-properties
       (point) (1+ (point)) '(display nil)))
    ;; restore undo list
    (setf buffer-undo-list saved-undo-list)
    (buffer-enable-undo)
    (setq inhibit-modification-hooks nil)))

(defun annotate--change-guard ()
  "Returns a `facespec` with an `insert-behind-hooks` property
that strips dangling `display` properties of text insertions if
text is inserted. This cleans up after newline insertions between
an overlay and it's annotation."
  (list 'face
        nil
        'insert-in-front-hooks
        '(annotate--remove-annotation-property)))

(defun annotate-context-before (pos)
  "Context lines before POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let ((bol (point)))
      (beginning-of-line (- (1- annotate-diff-export-context)))
      (buffer-substring-no-properties (point) (max 1 (1- bol))))))

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

(defun annotate-checksum-from-dump (record)
  "Get the checksum field from an annotation list loaded from a
file."
  (and (> (length record) 2)
       (nth 2 record)))

(defun annotate-annotations-from-dump (record)
  "Get the annotations field from an annotation list loaded from a
file."
  (nth 1 record))

(defun annotate-filename-from-dump (record)
  "Get the filename field from an annotation list loaded from a
file."
  (cl-first record))

(defun annotate-beginning-of-annotation (annotation)
  "Get the starting point of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
(annotate-annotations-from-dump (annotate-load-annotations))). "
  (cl-first annotation))

(defun annotate-ending-of-annotation (annotation)
  "Get the ending point of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
(annotate-annotations-from-dump (annotate-load-annotations))). "
  (cl-second annotation))

(defun annotate-text-of-annotation (annotation)
  "Get the text of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
(annotate-annotations-from-dump (annotate-load-annotations))). "
  (nth 2 annotation))

(defun annotate-load-annotation-old-format ()
  "Load all annotations from disk in old format."
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

(defun annotate-load-annotations ()
  "Load all annotations from disk."
  (cl-labels ((old-format-p (annotation)
                            (not (stringp (cl-first (last annotation))))))
    (interactive)
    (let* ((filename             (substring-no-properties (or (buffer-file-name) "")))
           (all-annotations-data (annotate-load-annotation-data))
           (annotation-dump      (assoc-string filename all-annotations-data))
           (annotations          (annotate-annotations-from-dump annotation-dump))
           (old-checksum         (annotate-checksum-from-dump annotation-dump))
           (new-checksum         (annotate-buffer-checksum))
           (modified-p           (buffer-modified-p)))
      (if (old-format-p annotation-dump)
          (annotate-load-annotation-old-format)
        (when (and (not (old-format-p annotation-dump))
                   old-checksum
                   new-checksum
                   (not (string= old-checksum new-checksum)))
          (lwarn '(annotate-mode)
                 :warning
                 annotate-warn-file-changed-control-string
                 filename))
        (when (and (eq nil annotations)
                   annotate-use-messages)
          (message "No annotations found."))
        (when (not (eq nil annotations))
          (save-excursion
            (dolist (annotation annotations)
              (let ((start (nth 0 annotation))
                    (end   (nth 1 annotation))
                    (text  (nth 2 annotation)))
                (annotate-create-annotation start end text)))))
        (set-buffer-modified-p modified-p)
        (font-lock-fontify-buffer)
        (when annotate-use-messages
          (message "Annotations loaded."))))))

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
      (when (use-region-p)
        (deactivate-mark))))
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
      (if (< prefix-length 1)
          (concat " \n" (make-string annotate-annotation-column ? ))
        (make-string prefix-length ? )))))

(defun annotate-bounds ()
  "The bounds of the region or whatever is at point."
  (list (cond
         ((use-region-p)
          (region-beginning))
         ((thing-at-point 'symbol)
          (car (bounds-of-thing-at-point 'symbol)))
         (t
          (point)))
        (cond
         ((use-region-p)
          (if (and (char-before (region-end))
                   (char-equal (char-before (region-end))
                               ?\n))
              (1- (region-end))
            (region-end)))
         ((thing-at-point 'symbol)
          (cdr (bounds-of-thing-at-point 'symbol)))
         (t
          (1+ (point))))))

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
    (cond ((= (point) 1)
           nil)
          (t
           (goto-char (point-min))
           (read (current-buffer))))))

(defun annotate-dump-annotation-data (data)
  "Save `data` into annotation file."
  (with-temp-file annotate-file
    (prin1 data (current-buffer))))

(define-button-type 'annotate-summary-button
  'follow-link t
  'help-echo "Click to show")

(defun annotate-summary-button-pressed (button)
  "Callback called when an annotate-summary-button is activated"
  (let ((buffer (find-file-other-window (button-get button 'file))))
    (with-current-buffer buffer
      (goto-char (button-get button 'go-to)))))

(defun annotate-show-annotation-summary ()
  "Show a summary of all the annotations in a temp buffer"
  (interactive)
  (cl-labels ((ellipsize (text prefix-string)
                         (let* ((prefix-length   (string-width prefix-string))
                                (ellipse-length  (string-width annotate-ellipse-text-marker))
                                (substring-limit (max 0
                                                      (- (window-body-width)
                                                         prefix-length
                                                         ellipse-length
                                                         2)))) ; this is for quotation marks
                           (if (> (string-width text)
                                  (+ (window-body-width)
                                     prefix-length
                                     ellipse-length
                                     2)) ; this is for quotation marks
                               (concat (substring text 0 substring-limit)
                                       annotate-ellipse-text-marker)
                             text)))
              (wrap      (text)
                         (concat "\"" text "\""))
              (insert-item-summary (snippet-text button-text)
                                   (insert annotate-summary-list-prefix-snippet)
                                   (insert (wrap (ellipsize snippet-text
                                                            annotate-summary-list-prefix-snippet)))
                                   (insert "\n")
                                   (insert annotate-summary-list-prefix)
                                   (insert-button (propertize (ellipsize button-text
                                                                         annotate-summary-list-prefix)
                                                              'face
                                                              'bold)
                                                  'file   filename
                                                  'go-to  annotation-begin
                                                  'action 'annotate-summary-button-pressed
                                                  'type   'annotate-summary-button)
                                   (insert "\n\n"))
              (build-snippet (filename annotation-begin annotation-end)
                             (with-temp-buffer
                               (insert-file-contents filename
                                                     nil
                                                     (1- annotation-begin)
                                                     (1- annotation-end))
                               (save-match-data
                                 (replace-regexp-in-string "[\r\n]"
                                                           " "
                                                           (buffer-string))))))

    (with-current-buffer-window
     "*annotations*" nil nil
     (display-buffer "*annotations*")
     (select-window (get-buffer-window "*annotations*" t))
     (outline-mode)
     (use-local-map nil)
     (local-set-key "q" (lambda ()
                          (interactive)
                          (kill-buffer "*annotations*")))
     (let ((dump (annotate-load-annotation-data)))
       (dolist (annotation dump)
         (let ((all-annotations (annotate-annotations-from-dump annotation))
               (filename        (annotate-filename-from-dump annotation)))
           (when (not (null all-annotations))
             (insert (format (concat annotate-summary-list-prefix-file "%s\n\n")
                             filename))
             (dolist (annotation-field all-annotations)
               (let* ((button-text      (format "%s"
                                                (annotate-text-of-annotation annotation-field)))
                      (annotation-begin (annotate-beginning-of-annotation annotation-field))
                      (annotation-end   (annotate-ending-of-annotation    annotation-field))
                      (snippet-text     (build-snippet filename
                                                       annotation-begin
                                                       annotation-end)))
                 (insert-item-summary snippet-text button-text))))))))))

(provide 'annotate)
;;; annotate.el ends here
