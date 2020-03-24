;;; annotate.el --- annotate files without changing them
;; Copyright (C) 2015 Bastian Bechtold and contributors:
;; Naoya Yamashita (2018)
;; Universita' degli Studi di Palermo (2019)

;; Author: Bastian Bechtold
;; Maintainer: Bastian Bechtold
;; URL: https://github.com/bastibe/annotate.el
;; Created: 2015-06-10
;; Version: 0.6.1

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

;; Important note: annotation can not overlaps and newline character
;; can not be annotated.

;;; Code:
(require 'cl-lib)

;;;###autoload
(defgroup annotate nil
  "Annotate files without changing them."
  :version "0.6.1"
  :group 'text)

;;;###autoload
(define-minor-mode annotate-mode
  "Toggle Annotate mode.
See https://github.com/bastibe/annotate.el/ for documentation."
  :init-value nil
  :lighter " Ann"
  :keymap (make-sparse-keymap)
  :group 'annotate
  :after-hook (annotate-initialize-maybe))

(define-key annotate-mode-map (kbd "C-c C-a") 'annotate-annotate)

(define-key annotate-mode-map (kbd "C-c C-s") 'annotate-show-annotation-summary)

(define-key annotate-mode-map (kbd "C-c ]") 'annotate-goto-next-annotation)

(define-key annotate-mode-map (kbd "C-c [") 'annotate-goto-previous-annotation)

(defcustom annotate-file (locate-user-emacs-file "annotations" ".annotations")
  "File where annotations are stored."
  :type 'file
  :group 'annotate)

(defface annotate-highlight
  '((t (:underline "coral")))
  "Face for annotation highlights."
  :group 'annotate)

(defface annotate-highlight-secondary
  '((t (:underline "khaki")))
  "Face for secondary annotation highlights."
  :group 'annotate)

(defface annotate-annotation
  '((t (:background "coral" :foreground "black" :inherit default)))
  "Face for annotations."
  :group 'annotate)

(defface annotate-annotation-secondary
  '((t (:background "khaki" :foreground "black" :inherit default)))
  "Face for secondary annotations."
  :group 'annotate)

(defface annotate-prefix
  '((t (:inherit default)))
 "Face for character used to pad annotation (fill space between
text lines and annotation text)."
 :group 'annotate)

(defcustom annotate-annotation-column 85
  "Where annotations appear."
  :type 'number
  :group 'annotate)

(defcustom annotate-diff-export-context 8
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

(defcustom annotate-blacklist-major-mode '()
  "Prevent loading of annotate-mode When the visited file's
major mode is a member of this list (space separated entries)."
  :type  '(repeat symbol)
  :group 'annotate)

(defcustom annotate-summary-ask-query t
 "If non nil a prompt asking for a query to filter the database
before showing it in a summary window is used. If nil the
database is not filtered at all."
  :type 'boolean
  :group 'annotate)

(defcustom annotate-annotation-max-size-not-place-new-line 15
 "The maximum `string-width` allowed for an annotation to be
 placed on the right margin of the window instead of its own line
 after the annotated text."
  :type  'number
  :group 'annotate)

(defcustom annotate-annotation-position-policy :by-length
  "policy for annotation's position:
  - :new-line
    always in a new-line
  - :margin
     always on right margin
  - :by-length
    decide by text's length

    if the length is more than the value of
    `annotate-annotation-max-size-not-place-new-line' place the
    annotation on a new line, place on the right margin
    otherwise.
"
  :type  'symbol
  :group 'annotate)

(defconst annotate-prop-chain-position
  'position)

(defconst annotate-prop-chain-pos-marker-first
  0)

(defconst annotate-prop-chain-pos-marker-last
  -1)

(defconst annotate-warn-file-changed-control-string
  (concat "The file '%s' has changed on disk "
          "from the last time the annotations were saved.\n"
          "Chances are that they will not be displayed correctly.")
  "The message to warn the user that file has been modified and
  annotations positions could be outdated")

(defconst annotate-warn-file-searching-annotation-failed-control-string
  (concat "The file '%s' has changed on disk "
          "from the last time the annotations were saved and "
          "Unfortunately was not possible to show annotation %S "
          "because i failed looking for test %S.")
  "The message to warn the user that file has been modified and
  an annotations could not be restored")

(defconst annotate-error-summary-win-filename-invalid
  "Error: File not found or in an unsupported format"
 "The message to warn the user that file can not be show in
 summary window because does not exist or is in an unsupported
 format.")

(defconst annotate-info-valid-file-extensions
  '(".info" ".info.gz" ".gz")
 "The valid extension for files that contains info document")

(defcustom annotate-search-region-lines-delta 2
 "When the annotated file is out of sync with its annotation
database the software looks for annotated text in the region with
delta equals to the value of this variable. Units are in number
of lines. The center of the region is the position of the
annotation as defined in the database."
  :type 'number
  :group 'annotate)

(defconst annotate-summary-list-prefix "    "
  "The string used as prefix for each text annotation item in summary window")

(defconst annotate-summary-list-prefix-file "* File: "
  "The string used as prefix for each annotated file item in summary window")

(defconst annotate-summary-list-prefix-snippet "** Annotated text: "
  "The string used as prefix for each annotation snippet item in summary window")

(defconst annotate-ellipse-text-marker "..."
  "The string used when a string is truncated with an ellipse")

(defconst annotate-info-root-name "dir"
  "The pseudo-filename of info root")

(defconst annotate-summary-buffer-name "*annotations*"
  "The name of the buffer for summary window")

(defconst annotate-annotation-prompt "Annotation: "
  "The prompt when asking user for annotation modification")

(defconst annotate-summary-delete-button-label "[delete]"
  "The label for the button, in summary window, to delete an annotation")

(defconst annotate-summary-replace-button-label "[replace]"
  "The label for the button, in summary window, to replace an annotation")

(defun annotate-annotations-exist-p ()
  "Does this buffer contains at least one or more annotations?"
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
  (let ((old-mode (gensym)))
    `(let ((,old-mode inhibit-modification-hooks))
       (unwind-protect
           (progn
             (setf inhibit-modification-hooks t)
             ,@body)
         (setf inhibit-modification-hooks ,old-mode)))))

(defun annotate-end-of-line-pos ()
 "Get the position of the end of line and rewind the point's
position (so that it is unchanged after this function is called)."
  (save-excursion
    (end-of-line)
    (point)))

(defun annotate-beginning-of-line-pos ()
  "Get the position of the beginning of line and rewind the point's
position (so that it is unchanged after this function is called)."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun annotate-annotated-text-empty-p (annotation)
  "Does this annotation contains annotated text?"
  (= (overlay-start annotation)
     (overlay-end   annotation)))

(defun annotate-annotation-force-newline-policy (annotation)
  (overlay-put annotation 'force-newline-policy t))

(defun annotate-annotation-newline-policy-forced-p (annotation)
  (overlay-get annotation 'force-newline-policy))

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
         (when (<= (overlay-start overlay)
                   a
                   (overlay-end overlay))
           (move-overlay overlay (overlay-start overlay) a)
           ;; delete overlay if there is no more annotated text
           (when (annotate-annotated-text-empty-p overlay)
             ;; we  are  deleting  the  last element  of  a  chain  (a
             ;; stopper)...
             (when (annotate-chain-last-p overlay)
               ;; move 'stopper' to the previous chain element
               (let ((annot-before (annotate-previous-annotation-ends (overlay-start overlay))))
                 ;; ...if such element exists
                 (when annot-before
                   (annotate-annotation-chain-position annot-before
                                                       annotate-prop-chain-pos-marker-last))))
             (delete-overlay overlay)
             (font-lock-fontify-buffer))))))))

(defun annotate-info-select-fn ()
  "The function to be called when an info buffer is updated"
  (annotate-clear-annotations)
  (annotate-load-annotations)
  (font-lock-fontify-buffer nil))

(defun annotate-initialize ()
  "Load annotations and set up save and display hooks."
  (annotate-load-annotations)
  (add-hook 'after-save-hook                  'annotate-save-annotations t t)
  (add-hook 'window-configuration-change-hook 'font-lock-fontify-buffer  t t)
  (add-hook 'before-change-functions          'annotate-before-change-fn t t)
  (add-hook 'Info-selection-hook              'annotate-info-select-fn   t t)
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
  (remove-hook 'Info-selection-hook              'annotate-info-select-fn   t)
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
  (cl-labels ((create-new-annotation ()
               (cl-destructuring-bind (start end)
                   (annotate-bounds)
                 (let ((annotation-text (read-from-minibuffer annotate-annotation-prompt)))
                   (annotate-create-annotation start end annotation-text nil)))))
    (let ((annotation (annotate-annotation-at (point))))
      (cond
       ((use-region-p)
        (let ((annotations (cl-remove-if-not #'annotationp
                                             (overlays-in (region-beginning)
                                                          (region-end)))))
          (if annotations
              (message "Error: the region overlaps with at least an already existings annotation")
            (create-new-annotation))))
       (annotation
        (annotate-change-annotation (point))
        (font-lock-fontify-buffer nil))
       (t
        (create-new-annotation)))
      (set-buffer-modified-p t))))

(cl-defun annotate-goto-next-annotation (&key (startingp t))
  "Move point to the next annotation."
  (interactive)
  (let ((annotation (annotate-annotation-at (point))))
    (if startingp
        (if annotation
            (let* ((chain-last          (annotate-chain-last annotation))
                   (annotation-last-end (overlay-end chain-last))
                   (look-ahead          (annotate-next-annotation-starts annotation-last-end)))
              (if look-ahead
                  (progn
                    (goto-char annotation-last-end)
                    (annotate-goto-next-annotation :startingp nil))
                (message "This is the last annotation.")))
          (let ((next-annotation (annotate-next-annotation-starts (point))))
            (when next-annotation
              (goto-char (overlay-start next-annotation)))))
      (if annotation
          (let ((chain-first (annotate-chain-first annotation)))
            (goto-char (overlay-start chain-first)))
        (annotate-goto-next-annotation :startingp t)))))

(cl-defun annotate-goto-previous-annotation (&key (startingp t))
  "Move point to the previous annotation."
  (interactive)
  (let ((annotation (annotate-annotation-at (point))))
    (if startingp
        (if annotation
            (let* ((chain-first            (annotate-chain-first annotation))
                   (annotation-first-start (overlay-start chain-first))
                   (look-behind            (annotate-previous-annotation-ends annotation-first-start)))
              (if look-behind
                  (progn
                    (goto-char (1- annotation-first-start))
                    (annotate-goto-previous-annotation :startingp nil))
                (message "This is the first annotation.")))
          (let ((previous-annotation (annotate-previous-annotation-ends (point))))
            (when previous-annotation
              (goto-char (1- (overlay-end previous-annotation))))))
      (if annotation
          (let ((chain-last (annotate-chain-last annotation)))
            (goto-char (overlay-end chain-last)))
        (annotate-goto-previous-annotation :startingp t)))))

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
 "Put comment markers at the start and (if it makes sense)
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
    (dolist (ov (sort (annotate-all-annotations)
                      (lambda (o1 o2)
                        (< (overlay-start o1)
                           (overlay-start o2)))))
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
                  "\n")
          (when (annotate-chain-last-p ov)
            (let ((annotation-integrated-text (annotate-wrap-in-comment annotate-integrate-marker
                                                                        (overlay-get ov 'annotation))))
              (insert annotation-integrated-text)))))))
    (annotate-clear-annotations)))

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
  (let* ((filename      (annotate-actual-file-name))
         (export-buffer (generate-new-buffer (concat filename
                                                     ".annotations.diff")))
         (annotations   (sort (annotate-all-annotations)
                              (lambda (a b)
                                (< (overlay-start a)
                                   (overlay-start b)))))
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
      (dolist (ann annotations)
        (let* ((start (overlay-start ann))
               (end   (overlay-end ann))
               (text  (overlay-get ann 'annotation))
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
               (previous-lines  (annotate-context-before start))
               ;; context lines after the annotation
               (following-lines (annotate-context-after end))
               (chain-last-p    (annotate-chain-last-p ann))
               ;; line header for diff chunk
               (diff-range      (annotate-diff-line-range start end chain-last-p)))
          (with-current-buffer export-buffer
            (insert "@@ " diff-range " @@\n")
            (when previous-lines
              (insert (annotate-prefix-lines " " previous-lines)))
            (insert (annotate-prefix-lines "-" annotated-lines))
            ;; loop over annotation lines and insert with highlight
            ;; and annotation text
            (let ((annotation-line-list (butlast (split-string
                                                  (annotate-prefix-lines "+" annotated-lines)
                                                  "\n")))
                  (integration-padding   (if (and (> (1- start) 0)
                                                  (> (1- start) bol))
                                             (make-string (- (1- start) bol) ? )
                                           "")))
                (insert (car annotation-line-list) "\n")
                (unless (string= (car annotation-line-list) "+")
                  (insert "+"
                          (annotate-wrap-in-comment integration-padding
                                                    (make-string (- end start)
                                                                 annotate-integrate-higlight))
                          "\n"))
                (when (annotate-chain-last-p ann)
                  (insert "+"
                          (annotate-wrap-in-comment integration-padding text)
                          "\n")))
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
      ;; go to the end of the longest annotation under point
      (let ((overlays (sort (cl-remove-if (lambda (a)
                                            (not (and (annotationp a)
                                                      (< (overlay-end a)
                                                         limit))))
                                          (overlays-at (point)))
                            (lambda (x y)
                              (> (overlay-end x)
                                 (overlay-end y))))))
        (when overlays
          (goto-char (overlay-end (car overlays)))))
      ;; capture the area from the overlay to EOL (regexp match #1)
      ;; for the modification guard and the newline itself (regexp
      ;; match #2) for the annotation.
      (re-search-forward "\\(.*\\(\n\\)\\)" limit t))))

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
    (let* ((current-window             (get-buffer-window (current-buffer)))
           (theoretical-line-width     (- (window-body-width current-window)
                                          annotate-annotation-column))
           (available-width            (if (> theoretical-line-width 0)
                                           theoretical-line-width
                                         line-width))
           (lineated-list              (annotate-group-by-width text available-width))
           (max-width                  (apply #'max
                                              (mapcar #'string-width lineated-list)))
           (all-but-last-lineated-list (%subseq lineated-list 0 (1- (length lineated-list))))
           (last-line                   (if all-but-last-lineated-list
                                            (car (last lineated-list))
                                          (cl-first lineated-list)))
           (lineated                   (cl-mapcar (lambda (a)
                                                    (pad a max-width t))
                                                  all-but-last-lineated-list)))
      (apply #'concat
             (append lineated
                     (list (pad last-line max-width nil)))))))

(defun annotate--annotation-builder ()
  "Searches the line before point for annotations, and returns a
`facespec` with the annotation in its `display` property."
  (save-excursion
    (let ((newline-position (point)))
      (goto-char (1- (point))) ; we start at the start of the previous line
      ;; find overlays in the preceding line
      (let ((prefix-first       (annotate-make-prefix)) ; white spaces
                                                        ; before first
                                                        ; line of
                                                        ; annotation
            (prefix-rest        (make-string annotate-annotation-column ? ))
            (bol                (progn (beginning-of-line) (point)))
            (eol                (progn (end-of-line) (point)))
            (annotation-text    "")
            (overlays           nil)
            (annotation-counter 1))
        ;; include previous line if point is at bol:
        (when (null (overlays-in bol eol))
          (setq bol (1- bol)))
        (setq overlays
              (sort (cl-remove-if-not #'annotationp
                                      (overlays-in bol eol))
                    (lambda (x y)
                      (< (overlay-end x) (overlay-end y)))))
        ;; configure each annotation's properties and place it on the
        ;; the window. The actual position of the annotation (newline
        ;; or right marigin) is indicated by the value of the
        ;; variable: `annotate-annotation-position-policy'.
        (dolist (ov overlays)
          (let* ((face                (cond
                                       ((not (annotate-chain-first-p ov))
                                        (let ((first-in-chain (annotate-chain-first ov)))
                                          (overlay-get first-in-chain
                                                       'annotation-face)))
                                       ((= (cl-rem annotation-counter 2) 0)
                                        'annotate-annotation)
                                       (t
                                        'annotate-annotation-secondary)))
                 (face-highlight      (if (= (cl-rem annotation-counter 2) 0)
                                          'annotate-highlight
                                        'annotate-highlight-secondary))
                 (annotation-long-p   (> (string-width (overlay-get ov 'annotation))
                                         annotate-annotation-max-size-not-place-new-line))
                 (position-new-line-p (cl-case annotate-annotation-position-policy
                                        (:new-line
                                         t)
                                        (:by-length
                                         (or (annotate-annotation-newline-policy-forced-p ov)
                                             annotation-long-p))
                                        (otherwise
                                         nil)))
                 (multiline-annotation (if position-new-line-p
                                           (list (overlay-get ov 'annotation))
                                         (save-match-data
                                           (split-string (annotate-lineate (overlay-get ov
                                                                                        'annotation)
                                                                           (- eol bol))
                                                         "\n"))))
                 (annotation-stopper   (if position-new-line-p
                                           (if (= annotation-counter
                                                  (length overlays))
                                               "\n"
                                             "")
                                         "\n")))
            (cl-incf annotation-counter)
            (overlay-put ov 'face face-highlight)
            (if (annotate-chain-first-p ov)
                (overlay-put ov 'annotation-face face)
                (let ((first-in-chain (annotate-chain-first ov)))
                  (overlay-put ov
                               'face
                               (overlay-get first-in-chain 'face))))
            (when (annotate-chain-last-p ov)
              (when position-new-line-p
                (setf prefix-first " \n"))
              (dolist (l multiline-annotation)
                (setq annotation-text
                      (concat annotation-text
                              prefix-first
                              (propertize l 'face face)
                              annotation-stopper))
                ;; white space before for all but the first annotation line
                (if position-new-line-p
                    (setq prefix-first (concat prefix-first prefix-rest))
                  (setq prefix-first prefix-rest))))))
        ;; build facespec with the annotation text as display property
        (if (string= annotation-text "")
          ;; annotation has been removed: remove display prop
          (list 'face 'default 'display nil)
        ;; annotation has been changed/added: change/add display prop
        (list 'face 'default 'display annotation-text))))))

(defun annotate--remove-annotation-property (begin end)
  "Cleans up annotation properties associated with a region."
  (when (> (buffer-size)
           0)
    (annotate-with-inhibit-modification-hooks
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
       (buffer-enable-undo)))))

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
 "Context lines before POS. Return nil if we reach a line before
first line of the buffer"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let ((bol (point)))
      (when (> (1- bol) 0)
        (beginning-of-line (- (1- annotate-diff-export-context)))
        (buffer-substring-no-properties (point) (max 1 (1- bol)))))))

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

(defun annotate-diff-line-range (start end chain-last-p)
  "Calculate diff-like line range for annotation."
  (save-excursion
    (let* ((lines-before      (- (- annotate-diff-export-context)
                                 (forward-line (- annotate-diff-export-context)))) ; this move point, too!
           (start-line        (line-number-at-pos (point)))
           (diff-offset-start (+ 1
                                 (- lines-before)
                                 annotate-diff-export-context))
           (end-increment     (if chain-last-p
                                  2
                                1))
           (diff-offset-end   (+ diff-offset-start
                                 end-increment
                                 (- (line-number-at-pos end)
                                    (line-number-at-pos start)))))
      (format "-%i,%i +%i,%i"
              start-line
              diff-offset-start
              start-line
              diff-offset-end))))

;;; database related procedures

(defun annotate-info-actual-filename ()
 "The info filename that feed this buffer or nil if not this
buffer is not on info-mode"
  (annotate-guess-filename-for-dump Info-current-file nil))

(defun annotate-actual-file-name ()
  "Get the actual file name of the current buffer"
  (substring-no-properties (or (annotate-info-actual-filename)
                               (buffer-file-name)
                               "")))

(cl-defun annotate-guess-filename-for-dump (filename
                                            &optional (return-filename-if-not-found-p t))
  "Prepare an acceptable filename suitable for metadata database."
  (cond
   ((annotate-string-empty-p filename)
    nil)
   ((file-exists-p filename)
    filename)
   (t
    (let ((found (if return-filename-if-not-found-p
                     filename
                   nil)))
      (cl-block surrounding
        (dolist (extension annotate-info-valid-file-extensions)
          (let ((filename-maybe (concat filename extension)))
            (when (file-exists-p filename-maybe)
              (setf found filename-maybe)
              (cl-return-from surrounding found)))))
      found))))

(defun annotate-make-annotation-dump-entry (filename file-annotations checksum)
  (list filename
        file-annotations
        checksum))

(defun annotate-make-record (filename file-annotations checksum)
  "Make an annotation record: see `annotate-load-annotations'"
  (annotate-make-annotation-dump-entry filename file-annotations checksum))

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
(annotate-annotations-from-dump (nth index (annotate-load-annotations)))). "
  (cl-first annotation))

(defun annotate-ending-of-annotation (annotation)
  "Get the ending point of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
(annotate-annotations-from-dump (nth index (annotate-load-annotations))))."
  (cl-second annotation))

(defun annotate-annotation-string (annotation)
  "Get the text of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
(annotate-annotations-from-dump (nth index (annotate-load-annotations))))."
  (nth 2 annotation))

(defun annotate-annotated-text (annotation)
  "Get the annotated text of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
(annotate-annotations-from-dump (nth index (annotate-load-annotations))))."
  (and (> (length annotation) 3)
       (nth 3 annotation)))

(defun annotate-save-annotations ()
  "Save all annotations to disk."
  (interactive)
  (let ((file-annotations (cl-remove-if (lambda (a)
                                          (= (annotate-beginning-of-annotation a)
                                             (annotate-ending-of-annotation    a)))
                                        (annotate-describe-annotations)))
        (all-annotations  (annotate-load-annotation-data))
        (filename         (annotate-guess-filename-for-dump (annotate-actual-file-name))))
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
                                                   (null (cdr entry)))
                                                 all-annotations))
    (if annotate-use-messages
        (message "Annotations saved."))))

(defun annotate-load-annotation-old-format ()
  "Load all annotations from disk in old format."
  (interactive)
  (let ((annotations (cdr (assoc-string (annotate-actual-file-name)
                                        (annotate-load-annotation-data))))
        (modified-p  (buffer-modified-p)))
    ;; remove empty annotations created by earlier bug:
    (setq annotations (cl-remove-if (lambda (ann) (null (nth 2 ann)))
                                    annotations))
    (when (and (null annotations)
               annotate-use-messages)
      (message "No annotations found."))
    (when (not (null annotations))
      (save-excursion
        (dolist (annotation annotations)
          (let ((start              (annotate-beginning-of-annotation annotation))
                (end                (annotate-ending-of-annotation    annotation))
                (annotation-string  (annotate-annotation-string       annotation)))
            (annotate-create-annotation start end annotation-string)))))
    (set-buffer-modified-p modified-p)
    (font-lock-fontify-buffer)
    (if annotate-use-messages
        (message "Annotations loaded."))))

(defun annotate-load-annotations ()
  "Load all annotations from disk.

The format of the database is:

(list record-1 record-2 ... record-n)

Each record is:

(list filename annotations checksum)

where:

filename: a string identifying a file on the file-system, or the
string \"dir\" for top-level info file.

checksum: a string used to fingerprint the annotate file above,
used to check if a file has been modified.

annotations:

(list annotation-1 annotation-2 ... annotation-n) or nil

finally annotation is:

(list start end annotation-string annotated-text)

start:             the buffer position where annotated text start
end:               the buffer position where annotated text ends
annotation-string: the text of annotation
annotated-text:    the substring of buffer starting from 'start' an ending with 'end' (as above)

example:

'(\"/foo/bar\" ((0 9 \"note\" \"annotated\")) hash-as-hex-string)

"
  (cl-labels ((old-format-p (annotation)
                            (not (stringp (cl-first (last annotation))))))
    (interactive)
    (let* ((filename             (annotate-actual-file-name))
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
        (cond
         ((and (null annotations)
               annotate-use-messages)
          (message "No annotations found."))
        (annotations
         (save-excursion
           (dolist (annotation annotations)
             (let ((start             (annotate-beginning-of-annotation annotation))
                   (end               (annotate-ending-of-annotation    annotation))
                   (annotation-string (annotate-annotation-string       annotation))
                   (annotated-text    (annotate-annotated-text          annotation)))
               (annotate-create-annotation start
                                           end
                                           annotation-string
                                           annotated-text))))))
        (set-buffer-modified-p modified-p)
        (font-lock-fontify-buffer)
        (when annotate-use-messages
          (message "Annotations loaded."))))))

(defun annotate-db-clean-records (records-db)
  "Remove records from arg `records-db' that have empty annotation, example:

'((\"/foo/bar.dat\" nil \"abababababababababababababab\")
  (\"/foo/baz.dat\" ((0 9 \"note\" \"annotated\")) \"abababababababababababababab\"))

will become:

'((\"/foo/baz.dat\" ((0 9 \"note\" \"annotated\")) \"abababababababababababababab\"))

i.e. the first record is removed."
  (cl-remove-if (lambda (a) (null (annotate-annotations-from-dump a)))
                records-db))

(defun annotate-db-purge ()
 "Update datbase *on disk* removing all the records with empty
annotation."
  (interactive)
  (let ((db (annotate-db-clean-records (annotate-load-annotation-data))))
    (annotate-dump-annotation-data db)))

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
    (let ((print-length nil))
      (prin1 data (current-buffer)))))

(cl-defmacro with-matching-annotation-fns ((filename
                                            beginning
                                            ending)
                                           &body body)
  "Anaphoric macro to build functions to find annotations"
  `(let ((filename-match-p          (lambda (record)
                                      (string= (annotate-filename-from-dump record)
                                               ,filename)))
         (annotation-limits-match-p (lambda (a)
                                      (and (= (annotate-beginning-of-annotation a)
                                              ,beginning)
                                           (= (annotate-ending-of-annotation    a)
                                              ,ending)))))
     ,@body))

(defun annotate-db-remove-annotation (db-records
                                      record-filename
                                      annotation-beginning
                                      annotation-ending)
  "Remove from database `db-records' the annotation indentified by
 the triplets `record-filename', `annotation-beginning' and
 `annotation-ending'; if such annotation does exists."
  (with-matching-annotation-fns
   (record-filename
    annotation-beginning
    annotation-ending)
   (let ((file-matched-record (cl-find-if filename-match-p db-records)))
     (if file-matched-record
         (let* ((rest-of-db      (cl-remove-if filename-match-p db-records))
                (new-annotations (cl-remove-if annotation-limits-match-p
                                               (annotate-annotations-from-dump file-matched-record)))
                (checksum        (annotate-checksum-from-dump file-matched-record))
                (new-record      (annotate-make-record record-filename
                                                       new-annotations
                                                       checksum)))
           (push new-record
                 rest-of-db))
      db-records))))

(defun annotate-db-replace-annotation (db-records
                                       record-filename
                                       annotation-beginning
                                       annotation-ending
                                       replacing-text)
  "Replace the text of annotation from database `db-records'
 indentified by the triplets `record-filename',
 `annotation-beginning' and `annotation-ending'; if such
 annotation does exists."
  (with-matching-annotation-fns
   (record-filename
    annotation-beginning
    annotation-ending)
   (let ((file-matched-record (cl-find-if filename-match-p db-records)))
     (if file-matched-record
         (let ((old-annotation   (cl-find-if annotation-limits-match-p
                                             (annotate-annotations-from-dump file-matched-record))))
           (if old-annotation
               (let* ((rest-of-db       (cl-remove-if filename-match-p db-records))
                      (rest-annotations (cl-remove-if annotation-limits-match-p
                                                      (annotate-annotations-from-dump file-matched-record)))
                      (checksum         (annotate-checksum-from-dump file-matched-record))
                      (new-annotation   (annotate-make-annotation annotation-beginning
                                                                  annotation-ending
                                                                  replacing-text
                                                                  (annotate-annotated-text old-annotation)))
                      (new-record       (annotate-make-record record-filename
                                                              (append (list new-annotation)
                                                                      rest-annotations)
                                                              checksum)))
                 (push new-record
                       rest-of-db))
             db-records))
       db-records))))

;;;; database related procedures ends here

(defun annotate-clear-annotations ()
  "Clear all current annotations."
  (interactive)
  (let ((overlays   (overlays-in 0 (buffer-size)))
        (modifiedp (buffer-modified-p)))
    ;; only remove annotations, not all overlays
    (setq overlays (cl-remove-if
                    (lambda (ov) (not (annotationp ov)))
                    overlays))
    (dolist (ov overlays)
      (annotate--remove-annotation-property (overlay-start ov)
                                            (overlay-end ov))
      (delete-overlay ov)
      (setf modifiedp t)
    (set-buffer-modified-p modifiedp))))

(defun annotate-string-empty-p (a)
  "Is the arg an empty string or null?"
  (or (null a)
      (string= "" a)))

(cl-defmacro annotate-ensure-annotation ((overlay) &body body)
  "Runs body only if overlay is an annotation (i.e. passes annotationp)"
  `(and (annotationp ,overlay)
        (progn ,@body)))

(defun annotate-annotation-prop-get (annotation property)
  "Get  property  `property'  from  annotation  `annotation'.  If
`annotation' does not pass `annotatonp' returns nil"
  (annotate-ensure-annotation (annotation)
    (overlay-get annotation property)))

(defun annotate-annotation-get-chain-position (annotation)
  "Get property's value that  define position of this annootation
in a chain of annotations"
  (annotate-annotation-prop-get annotation annotate-prop-chain-position))

(defun annotate-annotation-chain-position (annotation pos)
  "Set property's value that  define position of this annootation
in a chain of annotations"
  (overlay-put annotation annotate-prop-chain-position pos))

(defun annotate-chain-last-p (annotation)
  "Non nil if this annotation is the last element of a chain of annotations"
  (let ((value (annotate-annotation-get-chain-position annotation)))
    (and value
         (cl-equalp value annotate-prop-chain-pos-marker-last))))

(defun annotate-chain-first-p (annotation)
  "Non nil if  this annotation is the first element,  or the only
of a chain of annotations"
  (let* ((chain-pos           (annotate-annotation-get-chain-position annotation))
         (annotation-start    (overlay-start annotation))
         (previous-annotation (annotate-previous-annotation-ends annotation-start))
         (previous-chain-pos  (annotate-annotation-get-chain-position previous-annotation)))
    (or (= chain-pos
           annotate-prop-chain-pos-marker-first)
        (and (= chain-pos
                annotate-prop-chain-pos-marker-last)
             (or (null previous-annotation)
                 (= previous-chain-pos
                    annotate-prop-chain-pos-marker-last))))))

(defun annotate-chain-first (annotation)
  "Find first element of the chain where `annotation' belongs"
  (cond
   ((null annotation)
    nil)
   ((annotate-chain-first-p annotation)
    annotation)
   (t
    (let* ((annotation-start    (overlay-start annotation))
           (previous-annotation (annotate-previous-annotation-ends annotation-start)))
      (annotate-chain-first previous-annotation)))))

(defun annotate-chain-last (annotation)
  "Find last element of the chain where `annotation' belongs"
  (cond
   ((null annotation)
    nil)
   ((annotate-chain-last-p annotation)
    annotation)
   (t
    (let* ((annotation-end  (overlay-end annotation))
           (next-annotation (annotate-next-annotation-starts annotation-end)))
      (annotate-chain-last next-annotation)))))

(defun annotate-chain-first-at (pos)
  "Find first element of the chain of annotation that overlap point `pos'"
  (let ((annotation (annotate-annotation-at pos)))
    (annotate-ensure-annotation (annotation)
      (annotate-chain-first annotation))))

(defun annotate-chain-last-at (pos)
  "Find last element of the chain of annotation that overlap point `pos'"
  (let ((annotation (annotate-annotation-at pos)))
    (annotate-ensure-annotation (annotation)
      (annotate-chain-last annotation))))

(defun annotate-find-chain (annotation)
  "Find all annotation that are parts of the chain where `annotation' belongs"
  (annotate-ensure-annotation (annotation)
    (cl-labels ((find-next-annotation (pos)
                 (annotate-annotation-at (next-overlay-change pos))))
      (let* ((chain-first      (annotate-chain-first annotation))
             (results          (list chain-first))
             (chain-last       (annotate-chain-last  annotation))
             (start-pos        (overlay-end chain-first))
             (next-annotation  (find-next-annotation start-pos)))
        (if (eq chain-first
                chain-last)
            results
          (while (not (eq next-annotation
                          chain-last))
            (if next-annotation
                (progn
                  (cl-pushnew next-annotation results)
                  (setf start-pos       (overlay-end next-annotation)))
              (cl-incf start-pos))
            (setf next-annotation (find-next-annotation start-pos)))
          (push chain-last results)
          (reverse results))))))

(defun annotate-annotations-chain-at (pos)
  "Find all annotation that are parts of the chain that overlaps at `point'"
  (annotate-find-chain (annotate-annotation-at pos)))

(defun annotate-create-annotation (start end annotation-text annotated-text)
  "Create a new annotation for selected region.

Here the argument 'annotation-text' is the string that appears
on the margin of the window and 'annotated-text' is the string
that is underlined.

If this function is called from procedure
'annotate-load-annotations' the argument 'annotated-text'
should be not null. In this case we know that an annotation
existed in a text interval defined in the database
metadata (the database located in the file specified by the
variable 'annotate-file') and should just be
restored. Sometimes the annotated text (see above) can not be
found in said interval because the annotated file's content
changed and annotate-mode could not track the
changes (e.g. save the file when annotate-mode was not
active/loaded) in this case the matching
text ('annotated-text') is searched in a region surrounding the
interval and, if found, the buffer is annotated right there.

The searched interval can be customized setting the variable:
'annotate-search-region-lines-delta'.
"
  (cl-labels ((remap-chain-pos (annotations)
               (if (<= (length annotations)
                       1)
                   annotations
                 (let* ((all-but-last (butlast annotations)))
                     (cl-loop for annotation in all-but-last
                              for i from annotate-prop-chain-pos-marker-first
                              do
                              (annotate-annotation-chain-position annotation i)))))
              (create-annotation (start end annotation-text)
               (save-excursion
                 (let ((chain-pos 0)
                       (all-overlays ()))
                   (while (< start end)
                     (goto-char start)
                     (let ((char-maybe-newline (string (char-after))))
                       (if (string= char-maybe-newline "\n")
                           (goto-char (1+ (point)))
                         (progn
                           (re-search-forward "\n" end :goto-end)
                           (when (<= (point) end)
                             (let* ((end-overlay (if (/= (point) end)
                                                     (1- (point))
                                                   (point)))
                                    (highlight (make-overlay start end-overlay)))
                               (overlay-put highlight 'face 'annotate-highlight)
                               (overlay-put highlight 'annotation annotation-text)
                               (annotate-annotation-chain-position highlight
                                                                   annotate-prop-chain-pos-marker-last)
                               (push highlight all-overlays))))))
                     (setf start (point)))
                   (remap-chain-pos (reverse (mapcar #'maybe-force-newline-policy
                                                     all-overlays))))))
              (beginning-of-nth-line (start line-count)
                 (save-excursion
                   (goto-char start)
                   (forward-line line-count)
                   (beginning-of-line)
                   (point)))
              (go-backward           (start)
                 (beginning-of-nth-line
                  start
                  (- annotate-search-region-lines-delta)))
              (go-forward            (start)
                 (beginning-of-nth-line start
                                        annotate-search-region-lines-delta))
              (guess-match-and-add   (start end annotated-text max)
                 (cl-block surrounding
                   (while (< start max)
                     (let ((to-match (ignore-errors
                                       (buffer-substring-no-properties start end))))
                       (if (and to-match
                                (string= to-match annotated-text))
                           (cl-return-from surrounding start))
                       (progn
                         (setf start (1+ start)
                               end   (1+ end)))))
                   nil))
               (maybe-force-newline-policy  (annotation)
                   ;; force  newline policy  if height  of any  the face  of the
                   ;; overlay is different from height of default face
                   (save-excursion
                     (goto-char (overlay-start annotation))
                     (let* ((bol                  (annotate-beginning-of-line-pos))
                            (eol                  (annotate-end-of-line-pos))
                            (changed-face-pos     (min bol (overlay-start annotation)))
                            (limit                (max eol (overlay-end   annotation)))
                            (all-faces            (list (get-text-property changed-face-pos 'face)))
                            (default-face-height  (face-attribute 'default :height))
                            (all-faces-height     ())
                            (force-newline-p      nil))
                       (while (< changed-face-pos limit)
                         (setf changed-face-pos
                               (next-single-property-change changed-face-pos
                                                            'face
                                                            (current-buffer)
                                                            limit))
                         (push (get-text-property changed-face-pos 'face)
                               all-faces))
                       (setf all-faces-height
                             (mapcar (lambda (face)
                                       (face-attribute face :height nil 'default))
                                     (cl-remove-if-not #'facep all-faces)))
                       (setf force-newline-p
                             (cl-find-if (lambda (a) (/= a default-face-height))
                                         all-faces-height))
                       (when force-newline-p
                         (annotate-annotation-force-newline-policy annotation))
                       annotation))))
      (if (not (annotate-string-empty-p annotated-text))
          (let ((text-to-match (ignore-errors
                                 (buffer-substring-no-properties start end))))
            (if (and text-to-match
                     (string= text-to-match annotated-text))
                (create-annotation start end annotation-text)
              (let* ((starting-point-matching (go-backward start))
                     (ending-point-match      (go-forward  start))
                     (length-match            (- end start))
                     (new-match               (guess-match-and-add starting-point-matching
                                                                   (+ starting-point-matching
                                                                      length-match)
                                                                   annotated-text
                                                                   ending-point-match)))
                (and new-match
                     (create-annotation new-match
                                        (+ new-match length-match)
                                        annotation-text)))
              (lwarn '(annotate-mode) ; if matches annotated text failed
                     :warning
                     annotate-warn-file-searching-annotation-failed-control-string
                     (annotate-actual-file-name)
                     annotation-text
                     text-to-match)))
        (create-annotation start end annotation-text)) ; create new annotation
      (when (use-region-p)
        (deactivate-mark))
      (save-excursion
        (goto-char end)
        (font-lock-fontify-block 1))))

(defun annotate-change-annotation (pos)
  "Change annotation at point. If empty, delete annotation."
  (let* ((highlight       (annotate-annotation-at pos))
         (annotation-text (read-from-minibuffer annotate-annotation-prompt
                                                (overlay-get highlight 'annotation))))
    (cl-labels ((delete (annotation)
                 (let ((chain (annotate-find-chain annotation)))
                   (dolist (single-element chain)
                     (goto-char (overlay-end single-element))
                     (move-end-of-line nil)
                      (annotate--remove-annotation-property (overlay-start single-element)
                                                            (overlay-end   single-element))
                      (delete-overlay single-element))))
                (change (annotation)
                  (let ((chain (annotate-find-chain annotation)))
                    (dolist (single-element chain)
                        (overlay-put single-element 'annotation annotation-text)))))
    (save-excursion
      (cond
       ;; annotation was cancelled:
       ((null annotation-text))
       ;; annotation was erased:
       ((string= "" annotation-text)
        (delete highlight))
       ;; annotation was changed:
       (t
        (change highlight)))))))

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

(defun annotate-annotation-at (pos)
  "Returns the annotations (overlay where (annotationp overlay) -> t)
at positions pos or nil if no annotations exists at pos.

NOTE this assumes that annotations never overlaps so the list of
all annotations can contains only one element maximum."
  (let ((all (cl-remove-if-not #'annotationp
                               (overlays-at pos))))
    (cl-first all)))

(defun annotate-previous-annotation-ends (pos)
  "Returns the previous annotation that ends before pos or nil if no annotation
was found.
NOTE this assumes that annotations never overlaps"
  (cl-labels ((previous-annotation-ends (start)
                (let ((annotation (annotate-annotation-at start)))
                  (while (and (/= start
                                  (point-min))
                              (null annotation))
                    (setf start (previous-overlay-change start))
                    (setf annotation (annotate-annotation-at start)))
                  annotation)))
    (let ((annotation (annotate-annotation-at pos)))
      (if annotation
          (previous-annotation-ends (1- (overlay-start annotation)))
        (previous-annotation-ends pos)))))

(defun annotate-next-annotation-starts (pos)
  "Returns the previous annotation that ends before pos or nil if no annotation
was found.
NOTE this assumes that annotations never overlaps"
  (cl-labels ((next-annotation-ends (start)
                (let ((annotation (annotate-annotation-at start)))
                  (while (and (/= start
                                  (point-max))
                              (null annotation))
                    (setf start (next-overlay-change start))
                    (setf annotation (annotate-annotation-at start)))
                  annotation)))
    (let ((annotation (annotate-annotation-at pos)))
      (if annotation
          (next-annotation-ends (overlay-end annotation))
        (next-annotation-ends pos)))))

(defun annotate-symbol-strictly-at-point ()
 "Return non nil if a symbol is at char immediately following
 the point. This is needed as `thing-at-point' family of
 functions returns non nil if the thing (a symbol in this case)
 is around the point, according to the documentation."
  (cl-labels ((after-point ()
               (save-excursion
                 (goto-char (1+ (point)))
                 (bounds-of-thing-at-point 'symbol))))
    (let ((sym-on-point     (bounds-of-thing-at-point 'symbol))
          (sym-after-point  (after-point)))
      (and  sym-on-point
            sym-after-point
            (cl-equalp sym-on-point
                       sym-after-point)))))

(defun annotate-bounds ()
  "The bounds of the region or whatever is at point."
  (cl-labels ((left-ends ()
               (cond
                ((use-region-p)
                 (region-beginning))
                ((annotate-symbol-strictly-at-point)
                 (let* ((annotation-before (annotate-previous-annotation-ends (point)))
                        (boundaries        (bounds-of-thing-at-point 'symbol))
                        (symbol-start      (car boundaries))
                        (annotation-end    (if annotation-before
                                               (overlay-end annotation-before)
                                             -1)))
                   (max symbol-start
                        annotation-end)))
                (t
                 (point))))
              (right-ends ()
               (cond
                ((use-region-p)
                 (if (and (char-before (region-end))
                          (char-equal (char-before (region-end))
                                      ?\n))
                     (1- (region-end))
                   (region-end)))
                ((annotate-symbol-strictly-at-point)
                 (let* ((annotation-after (annotate-next-annotation-starts (point)))
                        (boundaries       (bounds-of-thing-at-point 'symbol))
                        (symbol-end       (cdr boundaries))
                        (annotation-start (if annotation-after
                                              (overlay-start annotation-after)
                                            (1+ symbol-end))))
                       (min symbol-end
                            annotation-start)))
                (t
                 (1+ (point))))))
    (list (left-ends)
          (right-ends))))

(defun annotate-make-annotation (beginning ending annotation annotated-text)
  (list beginning ending annotation annotated-text))

(defun annotate-all-annotations ()
  "Return a list of all annotations in the current buffer."
  (cl-remove-if-not #'annotationp (overlays-in 0 (buffer-size))))

(defun annotate-describe-annotations ()
  "Return a list, suitable for database dump, of all annotations in the current buffer."
  (let ((all-annotations (cl-remove-if-not #'annotationp (overlays-in 0 (buffer-size))))
        (chain-visited   ()))
    (cl-remove-if #'null
                  (mapcar (lambda (annotation)
                            (let* ((chain       (annotate-find-chain annotation))
                                   (chain-first (annotate-chain-first annotation))
                                   (chain-last  (annotate-chain-last annotation))
                                   (from        (overlay-start chain-first))
                                   (to          (overlay-end   chain-last)))
                              (when (not (cl-find-if (lambda (a)
                                                       (eq (cl-first chain)
                                                           (cl-first a)))
                                                     chain-visited))
                                (push chain chain-visited)
                                (list from
                                      to
                                      (overlay-get annotation 'annotation)
                                      (buffer-substring-no-properties from to)))))
                          all-annotations))))

(defun annotate-info-root-dir-p (filename)
  "Is the name of this file equals to the info root node?"
  (string= filename
           annotate-info-root-name))

(defun annotate-guess-file-format (filename)
  "Try to guess the file format.
Non nil if the file format is supported from 'annotate' in a more
sophisticated way than plain text"
  (cl-labels ((file-contents ()
                             (with-temp-buffer
                               (insert-file-contents filename)
                               (buffer-string)))
              (info-format-p () ; lot of guesswork here :(
                             (cond
                              ((annotate-info-root-dir-p filename)
                               :info)
                              (t
                               (let* ((file-contents     (file-contents))
                                      (has-info-p        (string-match "info" filename))
                                      (separator-re      "\^L?\^_\^L?\^J")
                                      (has-separator-p   (string-match separator-re file-contents))
                                      (has-node-p        (string-match "Node:" file-contents)))
                                 (if (or (annotate-info-root-dir-p filename)
                                         (and has-separator-p
                                              has-node-p)
                                         (and has-separator-p
                                              has-info-p))
                                     :info
                                   nil))))))
    (info-format-p)))

;;;; summary window procedures

(define-button-type 'annotate-summary-show-annotation-button
  'follow-link t
  'help-echo "Click to show")

(define-button-type 'annotate-summary-delete-annotation-button
  'follow-link t
  'help-echo "Click to remove annotation")

(define-button-type 'annotate-summary-replace-annotation-button
  'follow-link t
  'help-echo "Click to replace annotation")

(defun annotate-summary-show-annotation-button-pressed (button)
  "Callback called when an annotate-summary-show-annotation-button is activated"
  (let* ((file      (button-get button 'file))
         (file-type (annotate-guess-file-format file)))
    (cond
     ((eq file-type :info)
      (with-current-buffer-window
       "*info*" nil nil
       (info-setup file (current-buffer))
       (switch-to-buffer "*info*"))
      (with-current-buffer "*info*"
        (goto-char (button-get button 'go-to))))
     (t
      (let* ((buffer (find-file-other-window file)))
        (with-current-buffer buffer
          (goto-char (button-get button 'go-to))))))))

(defun annotate-summary-delete-annotation-button-pressed (button)
  (let* ((filename        (button-get button 'file))
         (beginning       (button-get button 'beginning))
         (ending          (button-get button 'ending))
         (begin-of-button (button-get button 'begin-of-button))
         (end-of-button   (button-get button 'end-of-button))
         (db              (annotate-load-annotation-data))
         (filtered        (annotate-db-remove-annotation db filename beginning ending)))
    (annotate-dump-annotation-data filtered) ; save the new database with entry removed
    (cl-labels ((redraw-summary-window () ; update the summary window
                  (with-current-buffer annotate-summary-buffer-name
                    (read-only-mode -1)
                    (save-excursion
                      (button-put button 'invisible t)
                      (let ((annotation-button (previous-button (point))))
                        (button-put annotation-button 'face '(:strike-through t)))
                      (let ((replace-button (next-button (point))))
                        (button-put replace-button 'invisible t)))
                    (read-only-mode 1)))
                ;; if the file where the  deleted annotation belong to is visited,
                ;; update the buffer
                (update-visited-buffer-maybe ()
                  (let ((visited-buffer (find-buffer-visiting filename)))
                    (when visited-buffer ;; a buffer is visiting the file
                      (with-current-buffer visited-buffer
                        (annotate-mode -1)
                        (annotate-mode  1))))))
      (redraw-summary-window)
      (update-visited-buffer-maybe))))

(defun annotate-summary-replace-annotation-button-pressed (button)
  (let* ((filename             (button-get button 'file))
         (annotation-beginning (button-get button 'beginning))
         (annotation-ending    (button-get button 'ending))
         (query                (button-get button 'query))
         (db                   (annotate-load-annotation-data))
         (old-annotation       (button-get button 'text))
         (new-annotation-text  (read-from-minibuffer annotate-annotation-prompt old-annotation)))
    (when (not (annotate-string-empty-p new-annotation-text))
      (let ((replaced-annotation-db (annotate-db-replace-annotation db
                                                                    filename
                                                                    annotation-beginning
                                                                    annotation-ending
                                                                    new-annotation-text)))
        (annotate-dump-annotation-data replaced-annotation-db)
        (annotate-show-annotation-summary query)))))

(defun annotate-show-annotation-summary (&optional arg-query)
 "Show a summary of all the annotations in a temp buffer, the
results can be filtered with a simple query language: see
`annotate-summary-filter-db'."
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
                                  substring-limit)
                               (concat (substring text 0 substring-limit)
                                       annotate-ellipse-text-marker)
                             text)))
              (wrap      (text)
                         (concat "\"" text "\""))
              (insert-item-summary (filename
                                    snippet-text
                                    button-text
                                    annotation-beginning
                                    annotation-ending
                                    filter-query)
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
                                                  'go-to  annotation-beginning
                                                  'action 'annotate-summary-show-annotation-button-pressed
                                                  'type   'annotate-summary-show-annotation-button)
                                   (insert "\n\n")
                                   (insert annotate-summary-list-prefix)
                                   (insert "  ")
                                   (let ((del-button (insert-button
                                                       annotate-summary-delete-button-label
                                                      'file       filename
                                                      'beginning  annotation-beginning
                                                      'ending     annotation-ending
                                                      'action
                                                      'annotate-summary-delete-annotation-button-pressed
                                                      'type
                                                      'annotate-summary-delete-annotation-button)))
                                     (button-put del-button
                                                 'begin-of-button
                                                 (annotate-beginning-of-line-pos))
                                     (button-put del-button
                                                 'end-of-button
                                                 (annotate-end-of-line-pos)))
                                   (insert "\n")
                                   (insert annotate-summary-list-prefix)
                                   (insert "  ")
                                   (insert-button annotate-summary-replace-button-label
                                                  'file       filename
                                                  'beginning  annotation-beginning
                                                  'ending     annotation-ending
                                                  'query      filter-query
                                                  'text       button-text
                                                  'action
                                                  'annotate-summary-replace-annotation-button-pressed
                                                  'type
                                                  'annotate-summary-replace-annotation-button)
                                   (insert "\n\n"))
              (clean-snippet (snippet)
                             (save-match-data
                               (replace-regexp-in-string "[\r\n]"
                                                         " "
                                                         snippet)))
              (build-snippet-info (filename annotation-begin annotation-end)
                                  (with-temp-buffer
                                    (info-setup filename (current-buffer))
                                    (buffer-substring-no-properties annotation-begin
                                                                    annotation-end)))
              (build-snippet (filename annotation-begin annotation-end)
                             (if (file-exists-p filename)
                                 (cond
                                  ((eq (annotate-guess-file-format filename)
                                        :info)
                                   (clean-snippet (build-snippet-info filename
                                                                      annotation-begin
                                                                      annotation-end)))
                                  (t
                                   (with-temp-buffer
                                     (insert-file-contents filename
                                                           nil
                                                           (1- annotation-begin)
                                                           (1- annotation-end))
                                     (clean-snippet (buffer-string)))))
                               (if (annotate-info-root-dir-p filename)
                                   (clean-snippet (build-snippet-info filename
                                                                      annotation-begin
                                                                      annotation-end))
                                 annotate-error-summary-win-filename-invalid)))
              (db-empty-p    (dump)
                             (cl-every (lambda (a)
                                         (cl-every 'null
                                                   (annotate-annotations-from-dump a)))
                                       dump))
              (get-query     ()
                             (cond
                              (arg-query
                               arg-query)
                              (annotate-summary-ask-query
                               (read-from-minibuffer "Query: "))
                              (t
                               ".*"))))
    (let* ((filter-query (get-query))
           (dump         (annotate-summary-filter-db (annotate-load-annotation-data)
                                                     filter-query)))
      (if (db-empty-p dump)
          (when annotate-use-messages
            (message "The annotation database is empty"))
        (with-current-buffer-window
         annotate-summary-buffer-name nil nil
         (display-buffer annotate-summary-buffer-name)
         (select-window (get-buffer-window annotate-summary-buffer-name t))
         (outline-mode)
         (use-local-map nil)
         (local-set-key "q" (lambda ()
                              (interactive)
                              (kill-buffer annotate-summary-buffer-name)))
         (dolist (annotation dump)
           (let* ((all-annotations (annotate-annotations-from-dump annotation))
                  (db-filename     (annotate-filename-from-dump annotation)))
             (when (not (null all-annotations))
               (insert (format (concat annotate-summary-list-prefix-file "%s\n\n")
                               db-filename))
               (dolist (annotation-field all-annotations)
                 (let* ((button-text      (format "%s"
                                                  (annotate-annotation-string annotation-field)))
                        (annotation-begin (annotate-beginning-of-annotation annotation-field))
                        (annotation-end   (annotate-ending-of-annotation    annotation-field))
                        (snippet-text     (build-snippet db-filename
                                                         annotation-begin
                                                         annotation-end)))
                   (insert-item-summary db-filename
                                        snippet-text
                                        button-text
                                        annotation-begin
                                        annotation-end
                                        filter-query))))))
         (read-only-mode 1))))))

;;;; end summary window procedures

;;;; filtering summary window: parser, lexer, etc.

(defvar annotate-summary-query  nil
  "Holds the query to filter annotations when
summary window is shown")

(defvar annotate-summary-query-current-token nil
  "Holds the next token of the query in `annotate-summary-query'")

(defun annotate-summary-query-lexer-symbol (res)
  "The symbol identifying the token (e.g. 'and)"
  (elt res 0))

(defun annotate-summary-query-lexer-string (res)
  "The string associated with this token"
  (elt res 1))

(defun annotate-summary-query-lexer-start (res)
 "The starting point of the substring of
`annotate-summary-query' for this token"
  (elt res 2))

(defun annotate-summary-query-lexer-end (res)
  "The ending point of the substring of
`annotate-summary-query' for this token"

  (elt res 3))

(cl-defun annotate-summary-lexer (&optional (look-ahead-p nil))
  "The lexer for `annotate-summary-query'.

This function, when called, will produce the next token from
`annotate-summary-query'; a token is a substring with a well
defined meaning according to our grammar.

For example this string:

p.* and (a or not b)'

will be broken into these tokens:

'(re        \"p.*\"   0  3)
'(and       \"and\"   4  7)
'(open-par  \"(\"     8  9)
'(re        \"a\"     9 10)
'(or        \"or\"   11 12)
'(not       \"not\"  14 17)
'(re        \"b\"    18 19)
'(close-par \"(\"    19 20)

The format is a proper list where:
- first element
  a symbol representing the type of the token
   - 're                  = regular expression
   - 'and , 'or , 'not    = logical operator
   - 'open-par, close-par = open and closing parenthesis respectively
- second element
  the  value (the actual substring for this token)

- third and fourth element (currently unused)
 the substring limits for this token (as returned by
 `match-beginning' and `match-end'

Note that spaces are ignored and all the tokens except `re' must
not be prefixed with a backslash to match. So, for example not ->
will match the token type 'not but \not will match the token 're;
this way we can 'protect' a regexp that contains reserved
keyword (aka escaping).

The special value :no-more-token is returned after the whole
input is processed.

Calling this function with value of `look-ahead-p' nil will `consume' the token from
`annotate-summary-query' (i.e. that string is modified)

example:
'a and b' -> 'and b', '(re \"a\" 0 1)

when `look-ahead-p' is not nil the token is recognized but not cut away from
`annotate-summary-query'

example:
'a and b' -> 'a and b', '(re \"a\" 0 1)
"
  (cl-labels ((build-token (token-symbol token-string token-beginning token-end)
                              (list token-symbol
                                    token-string
                                    token-beginning
                                    token-end))
              (build-results  (token-symbol register-num)
                              (build-token token-symbol
                                           (match-string register-num annotate-summary-query)
                                           (match-beginning register-num)
                                           (match-end       register-num)))
              (cut-query      (match-results)
                              (setf annotate-summary-query
                                    (cl-subseq annotate-summary-query
                                               (annotate-summary-query-lexer-end match-results)))))
    (let ((re (concat "\\((\\)\\|\\()\\)\\|\\(and\\)\\|\\(not\\)\\|"
                      "\\(or\\)\\|\\(\".*\"\\)\\|\\([^[:space:]]+\\)")))
      (save-match-data
        (let* ((matchedp (string-match re annotate-summary-query))
               (res      (if matchedp
                             (cond
                              ((match-string 1 annotate-summary-query)
                               (build-results 'open-par 1))
                              ((match-string 2 annotate-summary-query)
                               (build-results 'close-par 2))
                              ((match-string 3 annotate-summary-query)
                               (build-results 'and 3))
                              ((match-string 4 annotate-summary-query)
                               (build-results 'not 4))
                              ((match-string 5 annotate-summary-query)
                               (build-results 'or 5))
                              ((match-string 6 annotate-summary-query)
                               (build-results 'escaped-re 6))
                              ((match-string 7 annotate-summary-query)
                               (build-results 're 7))
                              (t
                               :no-more-tokens))
                           :no-more-tokens)))
          (when (and (listp res)
                     (not look-ahead-p))
            (cut-query res))
          res)))))

(defun annotate-summary-query-parse-end-input-p (token)
 "Non nil if there are no more tokens in
`annotate-summary-query'"
  (eq token :no-more-tokens))

(cl-defun annotate-summary-query-parse-note (filter-fn annotation &optional (res nil))
  "Parser rule for note:

This function will parse the following production rules

NOTE       := '(' NOTE ')'
               | NOTE OPERATOR NOTE
               | NOT NOTE
               | RE
               | ESCAPED-RE
               | epsilon
OPERATOR   := AND | OR
FILE-MASK  := RE
RE         := [^[:space:]] ; as regular expression
ESCAPED-RE := DELIMITER
              ANYTHING
              DELIMITER
ANYTHING   := .*           ; as a regualar expression
AND        := 'and'
OR         := 'or'
NOT        := 'not'
DELIMITER  := \" ; ASCII 34 (dec) 22 (hex)

Arguments:

- filter-fn is a function that accept two parameters: the regular
  expression to match (a token of type 're, see the lexer
  `annotate-summary-lexer' and a single annotation record (see
  `annotate-load-annotations').

  This function will reject (its value is nil) records that do
  not match the annotation.

- annotation
  the annotation to test

- res the results of this production rule (internal use only)

"
  (cl-labels ((token-symbol-match-p (looking-symbol token)
                                    (eq looking-symbol
                                        (annotate-summary-query-lexer-symbol token)))
              ;; this function will parse the rule operator
              ;; OPERATOR   := AND | OR
              ;; where
              ;; previous-token is the value of the token just matched in rule
              ;; NOTE OPERATOR NOTE
              ;; filter-fn     see the docstring
              ;; matchp        non nil if (funcall filter-fn previous-token) is not nil
              (operator             (previous-token filter-fn annotation matchp)
                                    (let ((look-ahead        (annotate-summary-lexer t)))
                                      (if (annotate-summary-query-parse-end-input-p look-ahead)
                                          ;; end of input, recurse one more time
                                          (annotate-summary-query-parse-note filter-fn
                                                                             annotation
                                                                             matchp)
                                        (let ((look-ahead-symbol
                                               (annotate-summary-query-lexer-symbol look-ahead))
                                              (look-ahead-string
                                               (annotate-summary-query-lexer-string look-ahead)))
                                          (cond
                                           ((not (cl-find look-ahead-symbol '(and or close-par)))
                                            (error (format (concat "Expecting for operator "
                                                                   "('and' or 'or') or \")\". "
                                                                   "found %S instead")
                                                           look-ahead-string)))
                                           (t
                                            ;; found operator, recurse to search for rhs of rule
                                            ;; NOTE OPERATOR NOTE
                                            (annotate-summary-query-parse-note filter-fn
                                                                               annotation
                                                                               matchp))))))))
    (let* ((look-ahead (annotate-summary-lexer t))) ; the next token that the lexer *will* consume
                                                    ; note the second arg is non nil
      (if (not (annotate-summary-query-parse-end-input-p look-ahead))
          (progn
            (cond
             ((token-symbol-match-p 'close-par look-ahead) ; ignore closing parens
              res)
             ((token-symbol-match-p 'open-par look-ahead) ; next token is an open parens
              ;; trying to match the rule:
              ;; NOTE := '(' NOTE ')'
              (annotate-summary-lexer) ; consume the token ')'
              ;; match the note inside the parens
              (let ((matchp             (annotate-summary-query-parse-note filter-fn
                                                                           annotation)) ; recurse
                    ;; after the note there *must* be a closing parenthesis
                    (maybe-close-parens (annotate-summary-lexer)))
                ;; if not this is an error
                (when (or (annotate-summary-query-parse-end-input-p maybe-close-parens)
                          (not (eq (annotate-summary-query-lexer-symbol maybe-close-parens)
                                   'close-par)))
                  (error "Unmatched parens"))
                ;; continue parsing
                (annotate-summary-query-parse-note filter-fn annotation matchp))) ; recurse
             ((token-symbol-match-p 'not look-ahead)
              (annotate-summary-lexer) ; consume the token 'not'
              ;; the note after the 'not'  operator in rule
              ;; NOTE := NOT NOTE
              ;; the third argument is the value to return if
              ;; there are no more token left in the input string
              (let ((res (annotate-summary-query-parse-note filter-fn annotation :error))) ; recurse
                ;; if there are no more tokens here this is an error
                ;; because, according to the grammar, after a NOT a
                ;; NOTE is non optional
                (if (eq :error res)
                    (error "No more input after 'not'")
                  ;; if the last rule (saved in res) is not nil (and
                  ;; is not :error) return nil, return the last
                  ;; annotation otherwise remember that the user asked
                  ;; for an annotation that *not* matches a regex
                  (if (null res)
                      annotation
                    nil))))
             ;; trying to match the rule:
             ;; NOTE := NOTE AND NOTE
             ((token-symbol-match-p 'and look-ahead)
              (annotate-summary-lexer) ; consume the 'and' token
              (let ((lhs res)          ; the left side of this rule lhs AND rhs
                    (rhs (annotate-summary-query-parse-note filter-fn annotation :error))) ; recurse
                (if (eq :error rhs) ; see the 'not' operator above
                    (error "No more input after 'and'")
                  (and lhs rhs)))) ; both rules must match as this is a logic and
             ;; trying to match the rule:
             ;; NOTE := NOTE OR NOTE
             ((token-symbol-match-p 'or look-ahead)
              (annotate-summary-lexer) ; consume the 'or'
              (let ((lhs res)          ; the left side of this rule (lhs OR rhs)
                    (rhs (annotate-summary-query-parse-note filter-fn annotation :error))) ; recurse
                (if (eq :error rhs)
                    (error "No more input after 'or'")
                  (or lhs rhs)))) ; either lhs or rhs match as this is a logic or
             ((token-symbol-match-p 'escaped-re look-ahead)
              ;; here we match the rule:
              ;; NOTE := ESCAPED-RE
              ;; ESCAPED-RE is a delimited string like "foo bar"
              ;; we first unescape the protected token
              ;; "\"foo bar\"" ->  "foo bar" (yes, just remove the delimiters)
              ;; then we apply the filter function (see the docstring)
              (let* ((escaped   (annotate-summary-query-lexer-string (annotate-summary-lexer)))
                     (unescaped (substring escaped 1 (1- (length escaped)))) ; remove delimiters
                     (matchp    (funcall filter-fn unescaped annotation)))   ; apply the filter funcrion
                ;; and finally continue the parsing saving the results
                ;; of applying the filter-fn function
                (operator escaped filter-fn annotation matchp)))
             (t
              ;; here we match the rule:
              ;; NOTE := RE
              ;; RE   := a regular expression
              ;; first just get the RE token
              (let* ((regex     (annotate-summary-query-lexer-string (annotate-summary-lexer)))
                     ;; then apply the filter function (see the docstring)
                     (matchp    (funcall filter-fn regex annotation)))
                ;; and finally continue the parsing saving the results
                ;; of applying the filter-fn function
                (operator regex filter-fn annotation matchp)))))
        ;; if we are here the lexer can not find any more tokens in the query
        ;; just return the value of res
        res)))) ; end of `(if (not (annotate-summary-query-parse-end-input-p look-ahead))'

(defun annotate-summary-query-parse-expression ()
  "Parse rule for expression:

I feel this  is very likely wrong in many  ways, i hope linguists
are going to forgive me :-)

EXPRESSION := FILE-MASK
               | FILE-MASK AND NOTE
               | FILE-MASK OR NOTE
               | epsilon
NOTE       := '(' NOTE ')'
               | NOTE OPERATOR NOTE
               | NOT NOTE
               | RE
               | ESCAPED-RE
               | epsilon
OPERATOR   := AND | OR
FILE-MASK  := RE
RE         := [^[:space:]] ; as a regular expression
ESCAPED-RE := DELIMITER
              ANYTHING
              DELIMITER
ANYTHING   := .*           ; as a regular expression
AND        := 'and'
OR         := 'or'
NOT        := 'not'
DELIMITER  := \" ; ASCII 34 (dec) 22 (hex)

Note: this function return the annotation part of the record, see
`annotate-load-annotations'.

"
  (lambda (annotation query file-filter-fn note-filter-fn)
    (let ((annotate-summary-query query) ; save the query
          (query-notes-only       nil)) ; the query for just the notes
      (let ((next-token (annotate-summary-lexer))) ; get file-mask
        ;; if there are no more tokes just return all the records
        ;; these match the empty string as in rule
        ;; EXPRESSION := epsilon
        (if (annotate-summary-query-parse-end-input-p next-token)
            (annotate-annotations-from-dump annotation)
          ;; otherwise test the record with the file-mask
          (let* ((filtered-annotation (funcall file-filter-fn
                                               (annotate-summary-query-lexer-string next-token)
                                               annotation))
                 ;; get the operator as in rule
                 (operator-token (annotate-summary-lexer)))
            ;; if there are no operator just return the filtered (by file-mask)
            ;; as in rule
            ;; EXPRESSION := FILE-MASK
            (if (annotate-summary-query-parse-end-input-p operator-token)
                (annotate-annotations-from-dump filtered-annotation)
              ;; otherwise get the operator and continue to parse the rule
              ;; EXPRESSION := FILE-MASK AND NOTE
              ;; or
              ;; EXPRESSION := FILE-MASK OR  NOTE
              (let ((operator (annotate-summary-query-lexer-symbol operator-token)))
                (cond
                 ((eq operator 'or) ; operator is 'or
                  ;; try to parse with the rule
                  ;; EXPRESSION := FILE-MASK OR NOTE
                  ;; return only the list annotation filtered by
                  ;; file-mask the former is non nil
                  (if filtered-annotation
                      (annotate-annotations-from-dump filtered-annotation)
                    ;; the annotation filtered by file-mask is empty, try to
                    ;; match the NOTE rule
                    (let ((look-ahead (annotate-summary-lexer t)))
                      ;; no more input after operator this is wrong
                      ;; according to the rule we are trying to match:
                      ;; EXPRESSION := FILE-MASK OR NOTE
                      (if (annotate-summary-query-parse-end-input-p look-ahead)
                          (error "No more input after 'or'")
                        (progn
                          ;; copy the string for note parsing note
                          ;; that annotate-summary-query only contains
                          ;; the substring to match the NOTE rule
                          (setf query-notes-only (concat annotate-summary-query))
                          ;; parse all the annotations, we get a list
                          ;; where non nil elements are the annotation
                          ;; that passes the note-filter-fn test
                          (mapcar (lambda (a)
                                    (let ((annotate-summary-query (concat query-notes-only)))
                                      (annotate-summary-query-parse-note note-filter-fn
                                                                         a)))
                                  (annotate-annotations-from-dump annotation)))))))
                 ((eq operator 'and)
                  ;; try to parse with the rule
                  ;; EXPRESSION := FILE-MASK OR NOTE
                  (let ((look-ahead (annotate-summary-lexer t)))
                    ;; no more input after operator, this is wrong
                    ;; according to the rule we are trying to match:
                    ;; EXPRESSION := FILE-MASK AND NOTE
                    (if (annotate-summary-query-parse-end-input-p look-ahead)
                        (error "No more input after 'and'")
                      (progn
                        ;; copy the string for note parsing note
                        ;; that annotate-summary-query only contains
                        ;; the substring to match the NOTE rule
                        (setf query-notes-only (concat annotate-summary-query))
                        ;; parse the already filtered by file-mask annotations only
                        ;; we get a list where non nil elements are the annotation
                        ;; that passes the note-filter-fn test
                        (mapcar (lambda (a)
                                  (let ((annotate-summary-query (concat query-notes-only)))
                                    (annotate-summary-query-parse-note note-filter-fn
                                                                       a)))
                                (annotate-annotations-from-dump filtered-annotation))))))
                 (t
                  ;; there is something after the file-mask in the
                  ;; input but it is not an operator
                  (error (format "Unknown operator: %s is not in '(and, or)"
                                 (annotate-summary-query-lexer-string operator-token)))))))))))))

(defun annotate-summary-filter-db (annotations-dump query)
  "Filter an annotation database with a query.

The argument `query' is a string that respect a simple syntax:

- [file-mask] [(and | or) [not] regex-note [(and | or) [not] regexp-note ...]]

where

- file-mask: is a regular expression that should match the filepath
 the annotation refers to;
- and, or, not : you guess? Classics logical operators;
- regex-note: the text of annotation must match this regular expression.

Examples:

- lisp$ and TODO
 matches the text `TODO' in all lisp files

Parenthesis can be used for the expression related to the text of
annotation, like this:

- lisp$ and (TODO or important)
 the same as above but checks also for string `important'

- /home/foo/
 matches all the annotation that refers to file in the directory
 `/home/foo'

- /home/foo/ and not minor
 matches all the annotation that refers to file in the directory
 `/home/foo' and that not contains the text `minor'.

- .* and \"not\"
 the \" can be used to escape strings
"
  (let* ((parser             (annotate-summary-query-parse-expression))
         (filter-file        (lambda (file-mask annotation-dump)
                               (let ((filename
                                      (annotate-filename-from-dump annotation-dump)))
                                 (and (string-match-p file-mask filename)
                                      annotation-dump))))
         (filter-annotations (lambda (re annotation-dump-2)
                               (and (string-match-p re
                                                    (annotate-annotation-string annotation-dump-2))
                                    annotation-dump-2)))
         (filter             (lambda (single-annotation)
                                (let ((filtered-annotations (funcall parser
                                                                     single-annotation
                                                                     query
                                                                     filter-file
                                                                     filter-annotations)))
                                  (setf filtered-annotations
                                        (cl-remove-if 'null filtered-annotations))
                                  (when filtered-annotations
                                    (let ((filename (annotate-filename-from-dump
                                                     single-annotation))
                                          (checksum (annotate-checksum-from-dump
                                                     single-annotation)))
                                      (annotate-make-annotation-dump-entry filename
                                                                           filtered-annotations
                                                                           checksum))))))
         (filtered           (mapcar filter annotations-dump)))
    (cl-remove-if 'null filtered)))

;;;; end of filtering: parser, lexer, etc.

(provide 'annotate)
;;; annotate.el ends here
