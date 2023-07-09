;;; annotate.el --- annotate files without changing them  -*- lexical-binding: t; -*-
;; Copyright (C) 2015 Bastian Bechtold and contributors:
;; Naoya Yamashita (2018)
;; Universita' degli Studi di Palermo (2019)

;; Author: Bastian Bechtold
;; Maintainer: Bastian Bechtold <bastibe.dev@mailbox.org>, cage <cage-dev@twistfold.it>
;; URL: https://github.com/bastibe/annotate.el
;; Created: 2015-06-10
;; Version: 2.0.1

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
;; themselves.  Annotations are saved in the annotate-file
;; (~/.annotations by default).
;;
;; To add annotations to a file, select a region and hit C-c C-a.  The
;; region will be underlined, and the annotation will be displayed in
;; the right margin.  Annotations are saved whenever the file is saved.
;;
;; Use C-c ] to jump to the next annotation and C-c [ to jump to
;; the previous annotation.  Use M-x annotate-export-annotations to
;; save annotations as a no-difference diff file.

;; Important note: annotation can not overlaps and newline character
;; can not be annotated.

;;; Code:

(require 'info)

(require 'cl-lib)

;;;###autoload
(defgroup annotate nil
  "Annotate files without changing them."
  :version "2.0.1"
  :group 'text)

(defvar annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'annotate-annotate)
    (define-key map (kbd "C-c C-d") #'annotate-delete-annotation)
    (define-key map (kbd "C-c C-s") #'annotate-show-annotation-summary)
    (define-key map (kbd "C-c ]")   #'annotate-goto-next-annotation)
    (define-key map (kbd "C-c [")   #'annotate-goto-previous-annotation)
    map))

;;;###autoload
(define-minor-mode annotate-mode
  "Toggle Annotate mode.
See https://github.com/bastibe/annotate.el/ for documentation."
  :lighter " Ann"
  :group 'annotate
  (annotate-initialize-maybe))

(defcustom annotate-file (locate-user-emacs-file "annotations" ".annotations")
  "File where annotations are stored."
  :type 'file)

(defcustom annotate-file-buffer-local nil
 "If non nil (default `nil'), for each annotated file `filename', a database
`filename.notes', containing the annotations, is generated in the
same directory that contains `filename'."
  :type 'string)

(defcustom annotate-buffer-local-database-extension "notes"
 "The extension appended to the annotated filename to get the
name of the local database annotation"
  :type 'string)

(defcustom annotate-highlight-faces '((:underline "coral")
                                      (:underline "khaki"))
  "List of faces for annotated text."
  :type 'list)

(defcustom annotate-annotation-text-faces '((:background "coral" :foreground "black")
                                            (:background "khaki" :foreground "black"))
  "List of faces for annotation's text."
  :type 'list)

(defface annotate-prefix
  '((t (:inherit default)))
 "Face for character used to pad annotation.
This is the fill space between text lines and annotation text.")

(defcustom annotate-annotation-column 85
  "Where annotations appear."
  :type 'number)

(defcustom annotate-diff-export-options ""
 "Options passed to `diff' in `annotate-export-annotations'.
This is used when diffing between a buffer with and without
integrated annotations.
Note that there is an implicit `-u' at the end of default options
that Emacs passes to the diff program."
  :type 'string)

(defcustom annotate-use-messages t
  "Whether status messages may appear in the minibuffer."
  :type 'boolean)

(defcustom annotate-popup-warning-indirect-buffer t
  "Whether an information popup message is shown when killing an
annotated indirect buffer."
  :type 'boolean)

(defcustom annotate-integrate-marker " ANNOTATION: "
  "Marker that is written before every integrated annotation."
  :type 'string)

(defcustom annotate-integrate-highlight ?~
  "Character used to underline an annotated text."
  :type 'character)

(defcustom annotate-fallback-comment "#"
  "When variable `COMMENT-START' is nil use this string instead."
  :type 'string)

(defcustom annotate-blacklist-major-mode '()
  "Major modes in which to prevent auto-activation of command `annotate-mode'.
This is consulted when visiting a file.
It can be useful when some mode does not work well with
annotate (like source blocks in `org-mode') as this ensure that it
will be never loaded, see `annotate-initialize-maybe'."
  :type  '(repeat symbol))

(defcustom annotate-summary-ask-query t
 "If non nil a prompt asking for a query to filter the database
before showing it in a summary window is used. If nil the
database is not filtered at all."
  :type 'boolean)

(defcustom annotate-database-confirm-deletion t
 "If non nil a prompt asking confirmation before deleting a
database file that is going to be empty after saving an annotated
file will be shown."
  :type 'boolean)

(defcustom annotate-annotation-confirm-deletion nil
 "If non nil a prompt asking confirmation before deleting an
annotation file will be shown."
  :type 'boolean)

(defcustom annotate-database-confirm-import t
 "If non nil a prompt asking confirmation before importing a
database file will be shown."
  :type 'boolean)

(defcustom annotate-annotation-max-size-not-place-new-line 15
 "The maximum 'string-width' allowed for an annotation to be
placed on the right margin of the window instead of its own line
 after the annotated text."
  :type  'number)

(defcustom annotate-annotation-position-policy :by-length
  "Policy for annotation's position:
- :new-line
  always in a new-line
- :margin
  always on right margin
- :by-length
  decide by text's length

if the length is more than the value of
`ANNOTATE-ANNOTATION-MAX-SIZE-NOT-PLACE-NEW-LINE' place the
annotation on a new line, place on the right margin
otherwise."
  :type  'symbol)

(defcustom annotate-use-echo-area nil
 "Whether annotation text should appear in the echo area only when mouse
id positioned over the annotated text instead of positioning them in
the the buffer (the default)."
 :type 'boolean)

(defcustom annotate-print-annotation-under-cursor nil
  "Whether annotation text should appear in the minibuffer when
the cursor is positioned over an annotated text (default: nil).

Important note: for this changes to take effect also
annotate-use-echo-area must be non nil"
  :type 'boolean)

(defcustom annotate-print-annotation-under-cursor-prefix "ANNOTATION: "
  "Prefix that is printed before annotation in the minibuffer when
annotate-print-annotation-under-cursor is non nil"
  :type 'string)

(defcustom annotate-print-annotation-under-cursor-delay 0.5
 "The delay (in seconds) after an annotation id printed in the
minibuffer, when the pursor is placed over an annotated text.

This variable works only if `annotate-print-annotation-under-cursor' is non nil"
  :type 'float)

(defcustom annotate-warn-if-hash-mismatch t
 "Whether a warning message should be printed if a mismatch
occurs, for an annotated file, between the hash stored in the
database annotations and the hash calculated from the actual
file.

This usually happens if an annotated file (a file with an entry in the
database) is saved with annotated-mode *not* active or the file
has been modified outside Emacs."
  :type 'boolean)

(defcustom annotate-endline-annotate-whole-line t
 "Whether trying to annotate the end of line character will
annotate the whole line before (or after if the line is composed
by the newline character only) instead."
  :type 'boolean)

(defcustom annotate-search-region-lines-delta 2
 "When the annotated file is out of sync with its annotation
database the software looks for annotated text in the region with
delta equals to the value of this variable. Units are in number
of lines. The center of the region is the position of the
annotation as defined in the database."
  :type 'number)

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
annotations positions could be outdated.")

(defconst annotate-warn-file-searching-annotation-failed-control-string
  (concat "The file '%s' has changed on disk "
          "from the last time the annotations were saved and "
          "Unfortunately was not possible to show annotation %S "
          "because i failed looking for test %S.")
  "The message to warn the user that file has been modified and
an annotations could not be restored.")

(defconst annotate-warn-buffer-has-no-valid-file
  "Annotations can not be saved: unable to find a file for buffer %S"
  "The message to warn the user that a buffer it is not visiting
a valid file to be annotated.")

(defconst annotate-popup-warn-killing-an-indirect-buffer
  (concat "You killed an indirect buffer that contains annotation.\n"
          "Annotate mode can not save annotation in an indirect buffer.\n"
          "The buffer's content has been saved in a regular buffer "
          "(together with its annotations) named:\n\n%S\n\n"
          "If you want you can save that buffer in a file and "
          "the annotations will be saved as well.")
  "The message to warn the user that an annotated indirect buffer
has been killed.")

(defconst annotate-error-summary-win-filename-invalid
  "Error: File not found or in an unsupported format"
 "The message to warn the user that file can not be show in
summary window because does not exist or is in an unsupported
 format.")

(defconst annotate-info-valid-file-extensions
  '(".info" ".info.gz" ".gz")
 "The valid extension for files that contains info document.")

(defconst annotate-summary-list-prefix "    "
  "The string used as prefix for each text annotation item in summary window.")

(defconst annotate-summary-list-prefix-file "* File: "
  "The string used as prefix for each annotated file item in summary window.")

(defconst annotate-summary-list-prefix-snippet "** Annotated text: "
  "The string used as prefix for each annotation snippet item in summary window.")

(defconst annotate-ellipse-text-marker "..."
  "The string used when a string is truncated with an ellipse.")

(defconst annotate-info-root-name "dir"
  "The pseudo-filename of info root.")

(defconst annotate-summary-buffer-name "*annotations*"
  "The name of the buffer for summary window.")

(defconst annotate-dump-from-indirect-bugger-suffix "-was-annotated-indirect-buffer"
  "Append this suffix to a buffer generated from an annotated indirect buffer.")

(defconst annotate-annotation-prompt "Annotation: "
  "The prompt when asking user for annotation modification.")

(defconst annotate-summary-delete-button-label "[delete]"
  "The label for the button, in summary window, to delete an annotation.")

(defconst annotate-summary-replace-button-label "[replace]"
  "The label for the button, in summary window, to replace an annotation.")

(defconst annotate-confirm-deleting-annotation-prompt  "Delete this annotation? "
  "Prompt to be shown when asking for annotation deletion confirm.")

(defconst annotate-message-annotation-loaded "Annotations loaded."
  "The message shown when annotations has been loaded.")

(defconst annotate-message-annotations-not-found "No annotations found."
  "The message shown when no annotations has been loaded from the database.")

;;;; buffer locals variables

(defvar-local annotate-echo-annotation-timer nil
  "The buffer local variable bound to a timer that is in charge to print
the annotation under cursor on the echo area.")

(defvar-local annotate-colors-index-counter 0
  "An always increasing value to address annotation colors
in the customizable colors lists:

- annotate-highlight-faces
- annotate-annotation-text-faces.")

;;;; custom errors

(define-error 'annotate-error "Annotation error")

(define-error 'annotate-empty-annotation-text-error
  "Empty annotation text"
  'annotate-error)

(define-error 'annotate-db-file-not-found
  "Annotations database file not found"
  'annotate-error)

(define-error 'annotate-annotate-region-overlaps
  "Error: the region overlaps with at least an already existing annotation"
  'annotate-error)

(define-error 'annotate-query-parsing-error
  "Parsing failed:"
  'annotate-error)

(cl-defmacro annotate-with-disable-read-only (&body body)
  "Run `BODY' with `READ-ONLY-MODE' temporary disabled."
  (let ((read-mode-p (gensym)))
  `(let ((,read-mode-p (if buffer-read-only
                           1
                         -1)))
     (when (= ,read-mode-p 1)
       (read-only-mode -1))
     ,@body
     (when (= ,read-mode-p 1)
       (read-only-mode 1)))))

(defun annotate-annotations-exist-p ()
  "Does this buffer contains at least one or more annotations?"
  (cl-find-if 'annotationp
              (overlays-in 0 (buffer-size))))

(defun annotate-initialize-maybe ()
  "Initialize annotate mode only if buffer's major mode is not in the blacklist.
See `annotate-blacklist-major-mode'."
  (cl-flet ((shutdown ()
              (setq annotate-mode t)
              (annotate-shutdown)
              (setq annotate-mode nil)))
    (let ((annotate-allowed-p (with-current-buffer (current-buffer)
                                (not (apply #'derived-mode-p annotate-blacklist-major-mode)))))
      (cond
       ((not annotate-allowed-p)
        (shutdown))
       (annotate-mode
        (when (not (annotate-annotations-exist-p))
          (annotate-initialize)))
       (t
        (shutdown))))))

(cl-defun annotate-buffer-checksum (&optional (object (current-buffer)))
  "Calculate an hash for the argument `OBJECT'."
  (secure-hash 'md5 object))

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
  "Does this `ANNOTATION' contains annotated text?"
  (= (overlay-start annotation)
     (overlay-end   annotation)))

(defun annotate-annotation-force-newline-policy (annotation)
  "Force annotate to place `ANNOTATION' on the line after the annotated text.

See: `ANNOTATE-ANNOTATION-POSITION-POLICY'."
  (overlay-put annotation 'force-newline-policy t))

(defun annotate-annotation-newline-policy-forced-p (annotation)
  "Is `ANNOTATION' forced to place annotation on the line after the
annotated text?

See: `ANNOTATE-ANNOTATION-POSITION-POLICY'."
  (overlay-get annotation 'force-newline-policy))

(defun annotate-chain-last-ring (chain)
  "Get the last ring of `CHAIN'."
  (car (last chain)))

(defun annotate--remap-chain-pos (annotations)
  "Remap `ANNOTATIONS' as an annotation 'chain'.

An annotation is a collection of one or more overlays that
contains the property `ANNOTATE-PROP-CHAIN-POSITION'.

The value of `ANNOTATE-PROP-CHAIN-POSITION' in each chain is an
integer starting from:

`ANNOTATE-PROP-CHAIN-POS-MARKER-FIRST' and *always* ending with

`ANNOTATE-PROP-CHAIN-POS-MARKER-LAST'

This means that a value of said property for a chain that
contains only an element is equal to
`ANNOTATE-PROP-CHAIN-POS-MARKER-LAST'.

This function ensure this constrains for the chain `ANNOTATION'
belong."
  (cond
   ((< (length annotations)
       1)
    annotations)
   ((= (length annotations)
       1)
    (annotate-annotation-set-chain-last (cl-first annotations)))
   (t
    (let ((all-but-last (butlast annotations))
          (last-element (car (last annotations))))
      (cl-loop for annotation in all-but-last
               for i from annotate-prop-chain-pos-marker-first
               do
               (annotate-annotation-chain-position annotation i))
      (when last-element
        (annotate-annotation-set-chain-last last-element))))))

(defun annotate-before-change-fn (a _b)
  "This function is added to 'before-change-functions' hook and
it is called any time the buffer content is changed (so, for
example, text is added or deleted). In particular, it will
rearrange the overlays bounds when an annotated text is
modified (for example a newline is inserted)."
  (with-silent-modifications
   (save-excursion
     (let* ((bol (annotate-beginning-of-line-pos))
            (eol (annotate-end-of-line-pos))
            (ov  (cl-remove-if-not #'annotationp
                                   (overlays-in bol eol))))
       (dolist (overlay ov)
         (annotate--remove-annotation-property (overlay-start overlay)
                                               (overlay-end   overlay))
         ;; check if we are breaking the overlay
         (when (<= (overlay-start overlay)
                   a
                   (overlay-end overlay))
           (let ((start-overlay (overlay-start overlay)))
             ;; delete overlay if there is no more annotated text
             (when (<= a start-overlay)
               (let ((chain (cl-remove overlay (annotate-find-chain overlay))))
                 (delete-overlay overlay)
                 (annotate--remap-chain-pos chain))))))))))

(defun annotate-info-select-fn ()
  "The function to be called when an info buffer is updated."
  (annotate-clear-annotations)
  (annotate-load-annotations)
  (font-lock-flush))

(defun on-window-size-change (frame)
  "The function to call when window-size-change-functions is called,
note that the argument `FRAME' is ignored"
  (font-lock-flush))

(defun annotate--filepath->local-database-name (filepath)
 "Generates the file path of the local database form `FILEPATH'."
  (concat (file-name-nondirectory filepath)
          "."
          annotate-buffer-local-database-extension))

(defun annotate--maybe-database-set-buffer-local ()
 "Sets, if user asked to do so, the annotation database to a
local version (i.e. a different database for each annotated file"
  (when annotate-file-buffer-local
    (make-local-variable 'annotate-file)
    (when-let* ((buffer-file-path (buffer-file-name))
                (parent-directory (file-name-directory buffer-file-path))
                (db-name (annotate--filepath->local-database-name buffer-file-path)))
      (setq-local annotate-file db-name))))

(defun annotate-timer-print-annotation-function ()
  "Print annotation under point in the minibuffer.
Used by the timer set in `annotate--maybe-make-timer-print-annotation'.

See also the customizable variables: `annotate-echo-annotation-timer' and
`annotate-print-annotation-under-cursor'."
  (with-current-buffer (current-buffer)
    (when annotate-mode
      (when-let ((annotation (annotate-annotation-at (point))))
        (message "%s%s"
                 annotate-print-annotation-under-cursor-prefix
                 (overlay-get annotation 'annotation))))))

(defun annotate-print-annotation-under-cursor-p ()
  "Non nil if the user configured the package to print
annotation's text in the minibuffer."
  (and annotate-use-echo-area
       annotate-print-annotation-under-cursor))

(defun annotate--maybe-make-timer-print-annotation ()
  "Set the timer to print the annotation's text in the minibuffer.
Used when the mode is activated."
  (when (annotate-print-annotation-under-cursor-p)
    (setf annotate-echo-annotation-timer
          (run-with-idle-timer annotate-print-annotation-under-cursor-delay
                               t
                               #'annotate-timer-print-annotation-function))))

(defun annotate--maybe-cancel-timer-print-annotation ()
  "Cancel the timer to print the annotation's' text in the minibuffer.
Used when the mode is deactivated."
  (when (and (annotate-print-annotation-under-cursor-p)
             annotate-echo-annotation-timer
             (timerp annotate-echo-annotation-timer))
    (cancel-timer annotate-echo-annotation-timer)))

(defun annotate-initialize ()
  "Load annotations and set up save and display hooks."
  (annotate--maybe-database-set-buffer-local)
  (annotate--maybe-make-timer-print-annotation)
  (annotate-load-annotations)
  (add-hook 'kill-buffer-hook                 #'annotate-save-annotations t t)
  (add-hook 'kill-emacs-hook                  #'annotate-save-all-annotated-buffers t nil)
  ;; This hook  is needed to  reorganize the layout of  the annotation
  ;; text when a window vertically resized
  (add-hook 'window-size-change-functions     #'on-window-size-change t t)
  (add-hook 'before-change-functions          #'annotate-before-change-fn t t)
  (add-hook 'Info-selection-hook              #'annotate-info-select-fn   t t)
  (if annotate-use-echo-area
      (font-lock-add-keywords
       nil
       '((annotate--font-lock-matcher (2 (annotate--annotation-builder)))))
    (font-lock-add-keywords
     nil
     '((annotate--font-lock-matcher (2 (annotate--annotation-builder))
                                    (1 (annotate--change-guard)))))))

(defun annotate-shutdown ()
  "Clear annotations and remove save and display hooks."
  (annotate-clear-annotations)
  (annotate--maybe-cancel-timer-print-annotation)
  (remove-hook 'kill-buffer-hook                 #'annotate-save-annotations t)
  (remove-hook 'kill-emacs-hook                  #'annotate-save-all-annotated-buffers nil)
  (remove-hook 'window-size-change-functions     #'on-window-size-change t)
  (remove-hook 'before-change-functions          #'annotate-before-change-fn t)
  (remove-hook 'Info-selection-hook              #'annotate-info-select-fn   t)
  (if annotate-use-echo-area
      (font-lock-remove-keywords
       nil
       '((annotate--font-lock-matcher (2 (annotate--annotation-builder)))))
    (font-lock-remove-keywords
     nil
     '((annotate--font-lock-matcher (2 (annotate--annotation-builder))
                                    (1 (annotate--change-guard)))))))

(defun annotate-overlay-filled-p (overlay)
  "Does this `OVERLAY' contains an 'annotation' property?"
  (and overlay
       (overlayp overlay)
       (overlay-get overlay 'annotation)))

(defun annotationp (overlay)
  "Is `OVERLAY' an annotation?"
  (annotate-overlay-filled-p overlay))

(cl-defmacro annotate-ensure-annotation ((overlay) &body body)
  "Runs `BODY' only if `OVERLAY' is an annotation (i.e. passes annotationp)."
  `(and (annotationp ,overlay)
        (progn ,@body)))

(defun annotate--position-on-annotated-text-p (pos)
  "Does `POS' (as buffer position) corresponds to a character
that belong to some annotated text?"
  (let ((annotation (annotate-annotation-at pos)))
    (if annotation
        t
      ;; there is  a chance  that a  point do not  belong to  the text
      ;; rendered as annotated but belong to a chain anyway example:
      ;;
      ;; legend:
      ;; a = annotated text
      ;; * = non annotated text
      ;; # = annotation
      ;;
      ;; Create a multiline annotation using region.
      ;;
      ;; aaaa
      ;; aaaa
      ;; aaaa
      ;;
      ;;
      ;; aaaa
      ;; aaaa
      ;; aaaa    ####
      ;;
      ;; place the cursor here:
      ;;
      ;; aaaa
      ;; aaaa
      ;; ^ cursor
      ;; aaaa    ####
      ;;
      ;; type some text
      ;;
      ;; aaaa
      ;; *****
      ;; aaaa    ####
      ;;
      ;; the text (the asterisks) is not rendered as annotated but as
      ;; annotations can not have gaps so we enforce this limitation
      ;; and consider it still parts of a chain formed by the
      ;; surrounding annotated text.
      (let* ((previous-annotation (annotate-previous-annotation-ends pos))
             (next-annotation     (annotate-next-annotation-starts   pos))
             (previous-chain      (annotate-chain-first previous-annotation))
             (next-chain          (annotate-chain-first next-annotation)))
        (if (and previous-chain
                 next-chain
                 (eq previous-chain
                     next-chain))
            t
          nil)))))

(defun annotate-delete-chains-in-region (from to)
  "Deletes all the chains enclosed in the range specified by
positions `FROM' and `TO'."
  (let* ((enclosed-chains (annotate-annotations-chain-in-range from to)))
    (dolist (chain enclosed-chains)
      (annotate--delete-annotation-chain (cl-first chain)))))

(defun annotate-count-newline-in-region (from to)
 "Counts the number of newlines character (?\n) in range
specified by `FROM' and `TO'."
  (cl-count-if (lambda (a) (char-equal a ?\n))
               (buffer-substring-no-properties from to)))

(defun annotate-annotate (&optional color-index)
  "Create, modify, or delete annotation.
if `COLOR-INDEX' is not null must be an index that adresses an element both in
- `annotate-highlight-faces'
and
- `annotate-annotation-text-faces'"
  (interactive "P")
  (when color-index
    (setf color-index (min (max (1- color-index) 0)
                           (1- (length annotate-highlight-faces)))))
  (cl-labels ((create-new-annotation ()
               ;; create a new annotation in the region returned by `annotate-bound'
               (cl-destructuring-bind (start end)
                   (annotate-bounds)
                 (let ((annotation-text (read-from-minibuffer annotate-annotation-prompt)))
                   (condition-case nil
                       (annotate-create-annotation start end annotation-text nil color-index)
                     (annotate-empty-annotation-text-error
                      (user-error "Annotation text is empty"))))))
              (cut-right (region-beg region-stop &optional delete-enclosed)
                ;; This function  will trim on  the right one  or more
                ;; existing  chains   of  overlays  that   compose  an
                ;; annotation  (i.e.  the   overlays  applied  on  the
                ;; annotated text). After this  function is called the
                ;; text  staring  from   `region-beg'  and  ending  on
                ;; `region-stop' will be cleared of all annotations if
                ;; `delete-enclosed' is non null.
                (let* ((last-of-chain-to-cut  (annotate-chain-last-at region-beg))
                       (first-of-chain-to-cut (annotate-chain-first-at region-beg))
                       (chain-start           (overlay-start first-of-chain-to-cut))
                       (chain-end             (overlay-end   last-of-chain-to-cut))
                       (newlines-count        (annotate-count-newline-in-region region-beg
                                                                                chain-end))
                       (cut-count             (- chain-end
                                                 region-beg
                                                 newlines-count)))
                  (cl-loop repeat cut-count do
                    (when (annotate-annotation-at chain-start)
                      (annotate--cut-right-annotation first-of-chain-to-cut t)))
                  (when delete-enclosed
                    (annotate-delete-chains-in-region chain-end region-stop))))
              (cut-left (region-stop delete-enclosed)
                ;; This function  will trim  on the  left one  or more
                ;; existing  chains   of  overlays  that   compose  an
                ;; annotation  (i.e.   the  overlays  applied  on  the
                ;; annotated text). After this  function is called the
                ;; text starting from the last  char of the last chain
                ;; element   of   the   annotation   and   ending   on
                ;; `region-stop' will be cleared of all annotations if
                ;; `delete-enclosed' is non null.
                (let* ((last-of-chain-to-cut  (annotate-chain-last-at region-stop))
                       (first-of-chain-to-cut (annotate-chain-first-at region-stop))
                       (chain-start           (overlay-start first-of-chain-to-cut))
                       (chain-end             (overlay-end   last-of-chain-to-cut))
                       (newlines-count        (annotate-count-newline-in-region chain-start
                                                                                region-stop))
                       (cut-count             (- region-stop
                                                 chain-start
                                                 newlines-count)))
                  (cl-loop repeat cut-count do
                    (when (annotate-annotation-at (1- chain-end))
                      (annotate--cut-left-annotation last-of-chain-to-cut)))
                  (when delete-enclosed
                    (annotate-delete-chains-in-region chain-end region-stop))))
              (annotate-overwrite-range (start end)
                ;; annotate  text starting  from  `start'  and ending  on
                ;; `end', overwriting any other annotation existing in
                ;; that range
                 (goto-char end)
                 (push-mark (point) t t)
                 (goto-char start)
                 (annotate-annotate))
              (annotate-line (eol)
                ;; annotate a line that terminate at `eol'
                ;;
                ;; if  the line  contains no  text before  the newline
                ;; annotate the next line with text, if any.
                ;;
                ;; if the line contains a single annotation that spans
                ;; the whole line update the existing annotation
                ;;
                ;; if the  line contains  no annotation, or  more than
                ;; one  annotation,  annotate   the  whole  line  that
                ;; terminate at `eol'
                (let* ((bol                     (annotate-beginning-of-line-pos))
                       (annotations-on-the-line (annotate-annotations-overlay-in-range bol
                                                                                       eol)))
                  (if (= (length annotations-on-the-line)
                         1)
                      (let* ((annotation                    (cl-first annotations-on-the-line))
                             (start-overlay                 (overlay-start annotation))
                             (end-overlay                   (overlay-end   annotation))
                             (annotation-spans-whole-line-p (and (= start-overlay bol)
                                                                 (= end-overlay   eol))))
                        (if annotation-spans-whole-line-p
                            (progn
                              (goto-char end-overlay)
                              (push-mark start-overlay t t)
                              (annotate-change-annotation (overlay-start annotation))
                              (pop-mark))
                          (annotate-overwrite-range bol eol)))
                    (annotate-overwrite-range bol eol)))))
    (let ((annotation (annotate-annotation-at (point))))
      (cond
       ((use-region-p)
        (let* ((region-beg      (region-beginning))
               (region-stop     (region-end))
               (enclosed-chains (annotate-annotations-chain-in-range region-beg region-stop)))
          (cond
           ((and (annotate--position-on-annotated-text-p region-beg)
                 (annotate--position-on-annotated-text-p region-stop))
            ;; aaaaaaaaaaaaaaaaaa
            ;;   ^-----------^
            (let ((starting-chain-at-start (annotate-chain-first-at region-beg))
                  (starting-chain-at-end   (annotate-chain-first-at region-stop)))
              (if (eq starting-chain-at-start
                      starting-chain-at-end)
                  (signal 'annotate-annotate-region-overlaps nil)
                (cut-left region-stop nil)
                (cut-right region-beg region-stop t)
                (create-new-annotation))))
           ((annotate--position-on-annotated-text-p region-beg)
            ;; aaaabbcc**********
            ;;   ^------------^
            (cut-right region-beg region-stop t)
            (create-new-annotation))
           ((annotate--position-on-annotated-text-p region-stop)
            ;; **********cccaaaa
            ;;   ^------------^
            (cut-left region-stop t)
            (create-new-annotation))
           (enclosed-chains
            ;; ****aaaaaaaaaaaaaaa****
            ;;  ^------------------^
            (annotate-delete-chains-in-region region-beg region-stop)
            (create-new-annotation))
           (t
            (create-new-annotation)))))
       (annotation
        (annotate-change-annotation (point))
        (font-lock-flush))
       (t
        (if (annotate--position-on-annotated-text-p (point))
            (signal 'annotate-annotate-region-overlaps nil)
          (let ((char-maybe-newline (char-after)))
            (when char-maybe-newline
              (cond
               ((not (char-equal char-maybe-newline ?\n))
                (create-new-annotation))
               ((null annotate-endline-annotate-whole-line)
                (user-error "The end of line can not be annotated"))
               (t ;; annotate the whole line before or after
                (save-excursion
                  (let* ((bol (annotate-beginning-of-line-pos))
                         (eol (point)))
                    (if (/= eol bol)       ; text before the newline, annotate it
                        (annotate-line eol)
                      (progn                ; no text before  the new
                                            ; line, annotate next line
                                            ; with proper text
                        (forward-line 1)
                        (goto-char (annotate-end-of-line-pos))
                        (annotate-annotate)))))))))))))))

(defun annotate-toggle-annotation-text ()
  "Hide annotation's text at current cursor's point, if such annotation exists."
  (interactive)
  (when-let* ((chain     (annotate-chain-at (point)))
              (last-ring (annotate-chain-last-ring chain)))
    (if (annotate-tail-overlay-hide-text-p last-ring)
        (annotate-chain-show-text chain)
      (annotate-chain-hide-text chain))
    (font-lock-flush)))

(defun annotate-toggle-all-annotations-text ()
"Hide annototation's text in the whole buffer."
  (interactive)
  (let ((chains (annotate-annotations-chain-in-range 0 (buffer-size))))
    (dolist (chain chains)
      (if (annotate-tail-overlay-hide-text-p (annotate-chain-last-ring chain))
          (annotate-chain-show-text chain)
        (annotate-chain-hide-text chain))))
  (font-lock-flush))

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
                (when annotate-use-messages
                  (message "This is the last annotation."))))
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
                (when annotate-use-messages
                  (message "This is the first annotation."))))
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
annotate-actual-comment-end."
  (apply #'concat (append (list (annotate-actual-comment-start))
                          strings
                          (list (annotate-actual-comment-end)))))

(cl-defstruct annotate-overlay-lines
  overlay
  line
  relative-start
  relative-end)

(cl-defun annotate--integrate-annotations (&key (use-annotation-marker t)
                                                (as-new-buffer         t)
                                                (switch-to-new-buffer  t))
  "Export all annotations, This function is not part of the public API."
  (cl-labels ((build-input-text-line ()
               (save-excursion
                 (annotate--split-lines (buffer-substring-no-properties (point-min)
                                                                        (point-max))))))

    (let* ((filename               (annotate-actual-file-name))
           (export-buffer          (generate-new-buffer (concat filename ".annotated.diff")))
           (annotations-overlays   (sort (annotate-all-annotations)
                                         (lambda (a b)
                                           (< (overlay-start a)
                                              (overlay-start b)))))
           (lines-count            (count-lines (point-min) (point-max)))
           (buffer-lines           (build-input-text-line))
           (ov-line-pos            (mapcar (lambda (ov)
                                             (line-number-at-pos (overlay-start ov)))
                                           annotations-overlays))
           (ov-start-pos-in-line   (mapcar (lambda (ov)
                                             (save-excursion
                                               (goto-char (overlay-start ov))
                                               (let ((bol (annotate-beginning-of-line-pos)))
                                                 (- (overlay-start ov) bol))))
                                           annotations-overlays))
           (ov-end-pos-in-line     (mapcar (lambda (ov)
                                             (save-excursion
                                               (goto-char (overlay-start ov))
                                               (let ((bol (annotate-beginning-of-line-pos)))
                                                 (- (overlay-end ov) bol))))
                                           annotations-overlays))
           (overlay-relative-pos   (cl-mapcar (lambda (ov line start end)
                                                (make-annotate-overlay-lines :overlay        ov
                                                                             :line           line
                                                                             :relative-start start
                                                                             :relative-end   end))
                                              annotations-overlays
                                              ov-line-pos
                                              ov-start-pos-in-line
                                              ov-end-pos-in-line))
           (parent-buffer-mode     major-mode)
           (output-buffer          (if as-new-buffer
                                       export-buffer
                                     (current-buffer))))
      (with-current-buffer output-buffer
        (erase-buffer)
        (when as-new-buffer
          (funcall parent-buffer-mode))
        (cl-loop
         for buffer-line in buffer-lines
         for line-number from 1  do
         (let ((overlays-in-line (cl-remove-if-not (lambda (a)
                                                     (= (annotate-overlay-lines-line a)
                                                        line-number))
                                                   overlay-relative-pos)))
           (when (or (/= (1- line-number)
                         lines-count)
                     (not (annotate-string-empty-p buffer-line)))
             (insert buffer-line "\n")
             (cl-loop for ov-line-pos in overlays-in-line do
                      (let* ((overlay         (annotate-overlay-lines-overlay ov-line-pos))
                             (relative-start  (annotate-overlay-lines-relative-start ov-line-pos))
                             (relative-end    (annotate-overlay-lines-relative-end ov-line-pos))
                             (padding         (if (<= (1- relative-start) 0)
                                                  ""
                                                (make-string (1- relative-start) ? )))
                             (annotated-lines (annotate--split-lines (overlay-get overlay
                                                                                  'annotation)))
                             (ov-length       (- relative-end relative-start))
                             (underline       (make-string ov-length
                                                           annotate-integrate-highlight)))
                        (insert (annotate-wrap-in-comment padding underline) "\n")
                        (when (annotate-chain-last-p overlay)
                          (when use-annotation-marker
                            (insert (annotate-wrap-in-comment annotate-integrate-marker) "\n"))
                          (cl-loop for line in annotated-lines do
                                   (insert (annotate-wrap-in-comment line) "\n")))))))))
      (when (and as-new-buffer
                 switch-to-new-buffer)
        (switch-to-buffer output-buffer))
      (when (not as-new-buffer)
        (delete-region (point) (point-max)))
      output-buffer)))

(defun annotate-integrate-annotations ()
  "Write all annotations into the file as comments below the annotated line.
An example might look like this:"
  (interactive)
  (annotate--integrate-annotations :use-annotation-marker t
                                   :as-new-buffer         nil
                                   :switch-to-new-buffer  nil)
  (annotate-clear-annotations))

(defun annotate-export-annotations ()
  "Export all annotations as a unified diff file.
An example might look like this:

--- .../annotate.el/annotate.el	2015-06-19 15:13:36.718796738 +0200
+++ .../annotate.el/annotate.el	2015-06-19 15:13:36.718796738 +0200
@@ -73,5 +73,5 @@
 ;;;###autoload
 (defface annotate-highlight
-  '((t (:underline \"coral\"))))
+  '((t (:underline \"coral\"))))
#        ~~~~~~~~~~~~~~~~~~
#        this doesn't work in cli
   \"Face for annotation highlights.\")

This diff does not contain any changes, but highlights the
annotation, and can be conveniently viewed in diff-mode."
  (interactive)
  (let ((buffer (annotate--integrate-annotations :switch-to-new-buffer nil)))
    (diff-buffers (current-buffer) buffer annotate-diff-export-options)))

(defun annotate--font-lock-matcher (limit)
  "Finds the next annotation. Matches two areas:
- the area between the overlay and the annotation
- the newline that will display the annotation

The first match will get `ANNOTATE--CHANGE-GUARD' as its
`INSERT-IN-FRONT-HOOK', to make sure that if a newline is inserted
between the overlay and the annotation, the 'display' property of
the newline is properly disposed of.

The second match will get `ANNOTATE--ANNOTATION-BUILDER' as its
'display' property, which makes the newline look like an
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
  "Groups `TEXT' in a list formed by chunks of maximum size equal
to `MAXIMUM-WIDTH'."
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
                                          so-far))))))
              (%split-words (text)
                (save-match-data (split-string text "[[:space:]]" t))))
    (if (< maximum-width 1)
        nil
      (let* ((words   (%split-words text))
             (grouped (reverse (%group words '()))))
        grouped))))

(cl-defun annotate-safe-subseq (seq from to &optional (value-if-limits-invalid seq))
  "Return a substring of `SEQ' or `VALUE-IF-LIMITS-INVALID'
sequence if `FROM' or `TO' are invalids."
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
  "Breaks `TEXT' into lines to fit in the annotation space with width `LINE-WIDTH'."
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

(cl-defun annotate--split-lines (text &optional (separator "\n"))
  "Return `TEXT' splitted by `SEPARATOR' (default: \"\n\")."
  (save-match-data
    (split-string text separator)))

(defun annotate--join-with-string (strings junction)
"Join list of string in `STRINGS' using string `JUNCTION'."
  (cl-reduce (lambda (a b) (concat a junction b))
             strings))

(defun annotate-wrap-annotation-in-box (annotation-overlay
                                        begin-of-line
                                        end-of-line
                                        annotation-on-is-own-line-p)
  "Pads or breaks annotation text (as property of
`ANNOTATION-OVERLAY' so that all lines have the same width.

If annotation is a placed on the margin of a window (that is
`ANNOTATION-ON-IS-OWN-LINE-P' is
nil) the text is broken (regardless of words) to fit on the side
of the window using `BEGIN-OF-LINE' `END-OF-LINE'.

If annotation is a note that is placed in its own line the text is padded
with spaces so that a 'box' surround the text without seams, e.g:

aaa      aaa
aa   ->  aa*
a        a**"
  (let ((annotation-text (overlay-get annotation-overlay 'annotation)))
    (cl-labels ((boxify-multiline (raw-annotation-text &optional add-space-at-end)
                  (let* ((lines         (annotate--split-lines raw-annotation-text))
                         (lines-widths  (mapcar #'string-width lines))
                         (max-width     (cl-reduce (lambda (a b) (if (> a b)
                                                                     a
                                                                   b))
                                                   lines-widths
                                                   :initial-value -1))
                         (padding-sizes (mapcar (lambda (a) (- max-width
                                                               (string-width a)))
                                                lines))
                         (paddings      (mapcar (lambda (a) (make-string a ? ))
                                                padding-sizes))
                         (box-lines     (cl-mapcar (lambda (a b) (concat a b))
                                                   lines paddings))
                         (almost-boxed  (annotate--join-with-string box-lines "\n")))
                    (if add-space-at-end
                        (concat almost-boxed " ")
                      almost-boxed))))
      (if annotation-on-is-own-line-p
          (list (boxify-multiline annotation-text t))
        (let* ((lineated         (annotate-lineate annotation-text
                                                   (- end-of-line begin-of-line)))
               (boxed            (boxify-multiline lineated nil)))
          (annotate--split-lines boxed))))))

(defun annotate--current-highlight-face ()
  "Reurns the current annotation color theme."
  (elt annotate-highlight-faces
       (mod annotate-colors-index-counter
            (length annotate-highlight-faces))))

(defun annotate--current-annotation-text-face ()
  "Reurns the current annotation's text color theme."
  (elt annotate-annotation-text-faces
       (mod annotate-colors-index-counter
            (length annotate-annotation-text-faces))))

(defun annotate--annotation-builder ()
  "Searches the line before point for annotations, and returns a
'facespec' with the annotation in its 'display' property."
  (save-excursion
    ;; (let ((newline-position (point)))
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
            (overlays-counter   1)
            (hidden-text        nil))
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
        ;; or right margin) is indicated by the value of the
        ;; variable: `annotate-annotation-position-policy'.
        (dolist (ov overlays)
          (let* ((last-ring-p          (annotate-chain-last-p ov))
                 (annotation-face      (overlay-get ov 'face)) ; added by annotate-create-annotation
                 (annotation-text-face (overlay-get ov 'annotation-face)) ; added by annotate-create-annotation
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
                 (multiline-annotation (annotate-wrap-annotation-in-box ov
                                                                        bol
                                                                        eol
                                                                        position-new-line-p))
                 (annotation-stopper   (if position-new-line-p
                                           (if (= overlays-counter
                                                  (length overlays))
                                               "\n"
                                             "")
                                         "\n"))
                 (tail-hidden-text-p   (and last-ring-p
                                            (annotate-tail-overlay-hide-text-p ov))))
            (setf hidden-text tail-hidden-text-p)
            (cl-incf overlays-counter)
            (overlay-put ov 'face annotation-face)
            (overlay-put ov 'annotation-face annotation-text-face)
            (when (and (not annotate-use-echo-area)
                       (not hidden-text)
                       (annotate-chain-last-p ov))
                (when position-new-line-p
                  (setf prefix-first " \n"))
                (dolist (l multiline-annotation)
                  (setq annotation-text
                        (concat annotation-text
                                prefix-first
                                (propertize l 'face annotation-text-face)
                                annotation-stopper))
                  ;; white space before for all but the first annotation line
                  (if position-new-line-p
                      (setq prefix-first (concat prefix-first prefix-rest))
                    (setq prefix-first prefix-rest))))))
        (when (not annotate-use-echo-area)
          ;; build facespec with the annotation text as display property
          (if (string= annotation-text "")
              ;; annotation has been removed: remove display prop
              (list 'face 'default 'display nil)
            ;; annotation has been changed/added: change/add display prop
            (list 'face 'default 'display annotation-text))))))

(defun annotate--remove-annotation-property (_begin end)
  "Cleans up annotation properties associated within a region
surrounded by `BEGIN' and `END'."
  (when (and annotate-mode
             (> (buffer-size) 0))
    (with-silent-modifications
      (annotate-with-disable-read-only
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
           (when (< (point)
                    (point-max))
             (remove-text-properties (point) (1+ (point)) '(display nil))))
         ;; restore undo list
         (setf buffer-undo-list saved-undo-list)
         (buffer-enable-undo))))))

(defun annotate-annotations-overlay-in-range (from-position to-position)
  "Return the annotations overlays that are enclosed in the range
defined by `FROM-POSITION' and `TO-POSITION'."
  (let ((annotations ()))
    (cl-loop for  i
             from (max 0 (1- from-position))
             to   to-position
             do
      (let ((annotation (annotate-next-annotation-starts i)))
        (annotate-ensure-annotation (annotation)
          (let ((chain-end   (overlay-end   (annotate-chain-last  annotation)))
                (chain-start (overlay-start (annotate-chain-first annotation))))
            (when (and (>= chain-start from-position)
                       (<= chain-end   to-position))
              (cl-pushnew annotation annotations))))))
    (reverse annotations)))

(defun annotate-annotations-chain-in-range (from-position to-position)
  "Return the annotations (chains) that are enclosed in the range
defined by `FROM-POSITION' and `TO-POSITION'."
  (let ((annotations (annotate-annotations-overlay-in-range from-position to-position))
        (chains      ()))
    (cl-loop for annotation in annotations do
      (let ((chain (annotate-find-chain annotation)))
        (cl-pushnew chain chains :test (lambda (a b) (eq (cl-first a) (cl-first b))))))
    (reverse chains)))

(defun annotate--change-guard ()
  "Return a `facespec` with an `insert-behind-hooks` property
that strips dangling `display` properties of text insertions if
text is inserted. This cleans up after newline insertions between
an overlay and it's annotation."
  (list 'face
        nil
        'insert-in-front-hooks
        '(annotate--remove-annotation-property)))

(defun annotate-prefix-lines (prefix text &optional omit-trailing-null)
 "Prepend `PREFIX' to each line in `TEXT'.
If `OMIT-TRAILING-NULL' is non null, empty line at the end of
text will be discarded."
  (let ((lines (annotate--split-lines text "\n")))
    (when omit-trailing-null
      (let ((last-not-empty (cl-position-if-not #'annotate-string-empty-p
                                                lines
                                                :from-end t)))
        (setf lines (cl-subseq lines 0 (1+ last-not-empty)))))
    (apply #'concat (mapcar (lambda (l) (concat prefix l "\n")) lines))))

;;; database related procedures

(defun annotate-info-actual-filename ()
 "The info filename that feed this buffer or nil if not this
buffer is not on info-mode"
  (annotate-guess-filename-for-dump Info-current-file nil))

(cl-defun annotate-indirect-buffer-p (&optional (buffer (current-buffer)))
  "Returns non nil if `BUFFER' (default the current buffer) is an indirect buffer."
  (buffer-base-buffer buffer))

(defun annotate-indirect-buffer-current-p ()
"Returns non nil if the current buffer is an indirect buffer."
  (annotate-indirect-buffer-p))

(defun annotate-actual-file-name ()
  "Get the actual file name of the current buffer."
  (cond
   ((annotate-indirect-buffer-current-p)
    nil)
   (t
    (substring-no-properties (or (annotate-info-actual-filename)
                                 (buffer-file-name)
                                 (buffer-file-name (buffer-base-buffer))
                                 "")))))

(cl-defun annotate-guess-filename-for-dump (filename
                                            &optional (return-filename-if-not-found-p t))
  "Guess an acceptable file name suitable for metadata database from `FILENAME'."
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
  "Make an annotation record: see `ANNOTATE-LOAD-ANNOTATIONS'."
  (list filename
        file-annotations
        checksum))

(defun annotate-make-record (filename file-annotations checksum)
  "Make an annotation record: see `annotate-load-annotations'."
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
\(annotate-annotations-from-dump (nth index (annotate-load-annotations))))."
  (cl-first annotation))

(defun annotate-ending-of-annotation (annotation)
  "Get the ending point of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
\(annotate-annotations-from-dump (nth index (annotate-load-annotations))))."
  (cl-second annotation))

(defun annotate--interval-left-limit (a)
  "Given an annotation record `A' returns the left limit of the annotated text."
  (cl-first a))

(defun annotate--interval-right-limit (a)
  "Given an annotation record `A' returns the right limit of the annotated text."
  (cl-second a))

(defun annotate--make-interval (left-limit right-limit)
  "Make an interval from `LEFT-LIMIT' and `RIGHT-LIMIT'."
  (list left-limit right-limit))

(defun annotate-annotation-interval (annotation)
  "Return the limits where ANNOTATION is applied.
The limit is a list of two numbers (LEFT RIGHT) representing of the portion
of the buffer where this annotation is applied.
Note that this function returns the character interval
yyyyyyyy ggg
  ^^^^^^^  ← Annotation interval in the database (extends for one more than the last character)
  |----|   ← The interval that this function returns.

In other terms the interval in the database is a closed interval while the interval that
this function return is closed on the left and open on the right side."
  (annotate--make-interval (annotate-beginning-of-annotation annotation)
                           (1- (annotate-ending-of-annotation annotation))))

(defun annotate-annotation-string (annotation)
  "Get the text of an annotation. The arg 'annotation' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
\(annotate-annotations-from-dump (nth index (annotate-load-annotations))))."
  (nth 2 annotation))

(defun annotate-annotated-text (annotation)
  "Get the annotated text of an annotation. The arg `ANNOTATION' must be a single
annotation field got from a file dump of all annotated buffers,
essentially what you get from:
\(annotate-annotations-from-dump (nth index (annotate-load-annotations))))."
  (and (> (length annotation) 3)
       (nth 3 annotation)))

(defun annotate-save-all-annotated-buffers ()
  "Save the annotations for all buffer where `annotate-mode' is active."
  (let ((all-annotated-buffers (annotate-buffers-annotate-mode)))
    (cl-loop for annotated-buffer in all-annotated-buffers do
             (with-current-buffer annotated-buffer
               (annotate-save-annotations)))))

(cl-defun annotate--dump-indirect-buffer (annotations &optional (indirect-buffer (current-buffer)))
"Clone an annotated indirect buffer into a new buffer.
`ANNOTATIONS' containd the annotations and `INDIRECT-BUFFER'
\(default the current buffer) is the buffer to be cloned."
  (when annotations
    (let* ((new-buffer-name  (generate-new-buffer-name (concat (buffer-name indirect-buffer)
                                                               annotate-dump-from-indirect-bugger-suffix)))
           (new-buffer           (get-buffer-create new-buffer-name))
           (indirect-content (with-current-buffer (current-buffer)
                               (buffer-string))))
      (with-current-buffer new-buffer
        (annotate-mode -1)
        (insert indirect-content)
        ;; when launching the command `(annotate-mode 1)' annotate
        ;; mode refuses to add the hooks if annotations are already
        ;; present in the buffer.
        ;; So the right way here is: first activate the mode and then
        ;; add the annotations
        (annotate-mode 1)
        (cl-loop for annotation in annotations do
                 (let ((annotation-start (annotate-beginning-of-annotation annotation))
                       (annotation-end   (annotate-ending-of-annotation annotation))
                       (annotation-text  (annotate-annotation-string annotation)))
                   (annotate-create-annotation annotation-start
                                               annotation-end
                                               annotation-text
                                               nil)))
        (pop-to-buffer new-buffer)
        (let* ((info-message (message annotate-popup-warn-killing-an-indirect-buffer
                                      (buffer-name new-buffer)))
               (user-choice  (when annotate-popup-warning-indirect-buffer
                               (x-popup-dialog t (list info-message
                                                       (cons "OK" :ok)
                                                       (cons "Never show again" :bury))))))
          (when (eq user-choice :bury)
            (customize-save-variable 'annotate-popup-warning-indirect-buffer nil)))))))

(defun annotate-save-annotations ()
  "Save all annotations to disk."
  (interactive)
  (let ((file-annotations (cl-remove-if (lambda (a)
                                          (= (annotate-beginning-of-annotation a)
                                             (annotate-ending-of-annotation    a)))
                                        (annotate-describe-annotations)))
        (all-annotations  (annotate-load-annotation-data t))
        (filename         (annotate-guess-filename-for-dump (annotate-actual-file-name))))
    (cond
       (filename
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
                                                       (null (annotate-annotations-from-dump entry)))
                                                     all-annotations))
        (when annotate-use-messages
          (message "Annotations saved.")))
       ((annotate-indirect-buffer-current-p)
        (annotate--dump-indirect-buffer file-annotations))
       (file-annotations
        (lwarn '(annotate-mode)
               :warning
               annotate-warn-buffer-has-no-valid-file
               (current-buffer))))))

(defun annotate-load-annotation-old-format ()
  "Load all annotations from disk in old format."
  (interactive)
  (let ((annotations (cdr (assoc-string (annotate-actual-file-name)
                                        (annotate-load-annotation-data t)))))
    ;; remove empty annotations created by earlier bug:
    (setq annotations (cl-remove-if (lambda (ann) (null (nth 2 ann)))
                                    annotations))
    (when (and (null annotations)
               annotate-use-messages)
      (message annotate-message-annotations-not-found))
    (when (not (null annotations))
      (save-excursion
        (dolist (annotation annotations)
          (let ((start              (annotate-beginning-of-annotation annotation))
                (end                (annotate-ending-of-annotation    annotation))
                (annotation-string  (annotate-annotation-string       annotation)))
            (annotate-create-annotation start end annotation-string nil)))))
    (font-lock-flush)
    (when annotate-use-messages
      (message annotate-message-annotation-loaded))))

(defun annotate-load-annotations ()
  "Load all annotations from disk and redraw the buffer to render the annotations.

The format of the database is:

\(RECORD-1 RECORD-2 ... RECORD-N)

Each record is:

\(FILENAME ANNOTATIONS CHECKSUM)

where:

FILENAME: a string identifying a file on the file-system, or the
string \"dir\" for top-level info file.

CHECKSUM: a string used to fingerprint the annotate file above,
used to check if a file has been modified.

annotations:

\(ANNOTATION-1 ANNOTATION-2 ... ANNOTATION-N) or nil

finally annotation is:

\(START END ANNOTATION-STRING ANNOTATED-TEXT)

START:             the buffer position where annotated text start
END:               the buffer position where annotated text ends
ANNOTATION-STRING: the text of annotation
ANNOTATED-TEXT:    the substring of buffer from START to END (as above)

example:

'(\"/foo/bar\" ((0 9 \"note\" \"annotated\")) hash-as-hex-string)."
  (interactive)
  (cl-labels ((old-format-p (annotation)
                            (not (stringp (cl-first (last annotation))))))
    (let* ((filename             (annotate-actual-file-name))
           (all-annotations-data (annotate-load-annotation-data t))
           (annotation-dump      (assoc-string filename all-annotations-data))
           (annotations          (annotate-annotations-from-dump annotation-dump))
           (old-checksum         (annotate-checksum-from-dump annotation-dump))
           (new-checksum         (annotate-buffer-checksum)))
      (if (old-format-p annotation-dump)
          (annotate-load-annotation-old-format)
        (when (and annotate-warn-if-hash-mismatch
                   (not (old-format-p annotation-dump))
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
          (message annotate-message-annotations-not-found))
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
        (font-lock-flush)
        (when annotate-use-messages
          (message annotate-message-annotation-loaded))))))

(defun annotate-db-clean-records (records-db)
  "Remove records from arg `RECORDS-DB' that have empty annotation, example:

'((\"/foo/bar.dat\" nil \"abababababababababababababab\")
  (\"/foo/baz.dat\" ((0 9 \"note\" \"annotated\")) \"abababababababababababababab\"))

will become:

'((\"/foo/baz.dat\" ((0 9 \"note\" \"annotated\")) \"abababababababababababababab\"))

i.e. the first record is removed."
  (cl-remove-if (lambda (a) (null (annotate-annotations-from-dump a)))
                records-db))

(defun annotate-db-purge ()
 "Update database *on disk* removing all the records with empty
annotation."
  (interactive)
  (let ((db (annotate-db-clean-records (annotate-load-annotation-data t))))
    (annotate-dump-annotation-data db)))

(defun annotate--expand-record-path (record)
"Expand file component of `RECORD'."
  (let* ((short-filename  (annotate-filename-from-dump    record))
         (annotations     (annotate-annotations-from-dump record))
         (file-checksum   (annotate-checksum-from-dump    record))
         (expandp         (not (or (file-remote-p short-filename)
                                   (annotate-info-root-dir-p short-filename))))
         (actual-filename (if expandp
                              (expand-file-name short-filename)
                            short-filename)))
    (annotate-make-record actual-filename
                          annotations
                          file-checksum)))

(defun annotate--deserialize-database-file (file)
  "Return a sexp from the annotation database contained in `FILE'."
  (with-temp-buffer
    (let* ((annotations-file file)
           (attributes       (file-attributes annotations-file)))
      (cond
       ((not (file-exists-p annotations-file))
        (signal 'annotate-db-file-not-found (list annotations-file)))
       ((= (file-attribute-size attributes)
           0)
        nil)
       (t
        (insert-file-contents annotations-file)
        (mapcar #'annotate--expand-record-path (read (current-buffer))))))))

(defun annotate-load-annotation-data (&optional ignore-errors)
  "Read and returns saved annotations."
    (if ignore-errors
        (ignore-errors (annotate--deserialize-database-file annotate-file))
      (annotate--deserialize-database-file annotate-file)))

(defun annotate-dump-annotation-data (data &optional save-empty-db)
  "Save `DATA' into annotation file."
  (cond
   ((or save-empty-db
        data)
    (with-temp-file annotate-file
      (let* ((print-length nil)
             (%abbreviate-filename (lambda (record)
                                     (let ((full-filename (annotate-filename-from-dump    record))
                                           (annotations   (annotate-annotations-from-dump record))
                                           (file-checksum (annotate-checksum-from-dump    record)))
                                       (annotate-make-record (abbreviate-file-name full-filename)
                                                             annotations
                                                             file-checksum))))
             (actual-data (mapcar %abbreviate-filename data)))
        (prin1 actual-data (current-buffer)))))
   ((file-exists-p annotate-file)
    (let* ((confirm-message    "Delete annotations database file %S? ")
           (delete-confirmed-p (or (not annotate-database-confirm-deletion)
                                   (y-or-n-p (format confirm-message annotate-file)))))
      (if delete-confirmed-p
          (condition-case err
              (delete-file annotate-file t)
            (error (message "error removing annotation database: %S"
                            (error-message-string err))))
        (annotate-dump-annotation-data data t))))))

(cl-defmacro with-matching-annotation-fns ((filename
                                            beginning
                                            ending)
                                           &body body)
  "Anaphoric macro to build functions to find annotations."
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
  "Remove from database `DB-RECORDS' the annotation identified by
the triplets `RECORD-FILENAME', `ANNOTATION-BEGINNING' and
 `ANNOTATION-ENDING'; if such annotation does exists."
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
  "Replace the text of annotation from database `DB-RECORDS'
identified by the triplets `RECORD-FILENAME',
 `ANNOTATION-BEGINNING' and `ANNOTATION-ENDING'; if such
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

(defun annotate-db-annotations-starts-before-p (a b)
  "Non nil if  annotation `A' starts before `B'.

In this context annotation means annotation loaded from local
database not the annotation shown in the buffer (therefore these
arguments are 'record' as called in the other database-related
functions)."
  (< (annotate-beginning-of-annotation a)
     (annotate-beginning-of-annotation b)))

;;;; database related procedures ends here

(defun annotate-clear-annotations ()
  "Clear all current annotations."
  (interactive)
  (let ((overlays   (overlays-in 0 (buffer-size))))
    ;; only remove annotations, not all overlays
    (setq overlays (cl-remove-if
                    (lambda (ov) (not (annotationp ov)))
                    overlays))
    (dolist (ov overlays)
      (annotate--remove-annotation-property (overlay-start ov)
                                            (overlay-end ov))
      (delete-overlay ov))))

(defun annotate-string-empty-p (a)
  "Is the arg `A' an empty string or null?"
  (or (null a)
      (string= "" a)))

(defun annotate-annotation-prop-get (annotation property)
  "Get property `PROPERTY' from annotation `ANNOTATION'. If
`ANNOTATION' does not pass `annotationp' returns nil."
  (annotate-ensure-annotation (annotation)
    (overlay-get annotation property)))

(defun annotate-annotation-get-chain-position (annotation)
  "Get property's value that define position of this annotation
in a chain of annotations"
  (annotate-annotation-prop-get annotation annotate-prop-chain-position))

(defun annotate-annotation-chain-position (annotation pos)
  "Set property's value that define position of this annotation
in a chain of annotations."
  (overlay-put annotation annotate-prop-chain-position pos))

(defun annotate-chain-last-p (annotation)
  "Non nil if `ANNOTATION' is the last element of a chain of annotations."
  (let ((value (annotate-annotation-get-chain-position annotation)))
    (and value
         (cl-equalp value annotate-prop-chain-pos-marker-last))))

(defun annotate-chain-first-p (annotation)
  "Non nil if `ANNOTATION' is the first element, or the only
of a chain of annotations."
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
  "Find first element of the chain where `ANNOTATION' belongs."
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
  "Find last element of the chain where `ANNOTATION' belongs."
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
  "Find first element of the chain of annotation that overlap point `POS'."
  (let ((annotation (annotate-annotation-at pos)))
    (annotate-ensure-annotation (annotation)
      (annotate-chain-first annotation))))

(defun annotate-chain-last-at (pos)
  "Find last element of the chain of annotation that overlap point `POS'."
  (let ((annotation (annotate-annotation-at pos)))
    (annotate-ensure-annotation (annotation)
      (annotate-chain-last annotation))))

(defun annotate-chain-at (pos)
  "Find the chain of overlays where point `POS' belongs."
  (let ((annotation (annotate-annotation-at pos)))
    (annotate-ensure-annotation (annotation)
      (annotate-find-chain annotation))))

(defun annotate-annotation-set-chain-first (annotation)
  "Set property's value that define position of this annotation
in a chain of annotations as first."
  (annotate-annotation-chain-position annotation annotate-prop-chain-pos-marker-first))

(defun annotate-annotation-set-chain-last (annotation)
  "Set property's value that define position of this annotation
in a chain of annotations as last."
  (annotate-annotation-chain-position annotation annotate-prop-chain-pos-marker-last))

(defun annotate-find-chain (annotation)
  "Find all ANNOTATION that are parts of the chain where `ANNOTATION' belongs."
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
  "Find all annotation that are parts of the chain that overlaps at `POS'."
  (annotate-find-chain (annotate-annotation-at pos)))

(defun annotate-chain-hide-text (chain)
  "Sets an overlay properties of the last ring of `CHAIN' so that
the annotation's text will not be rendered."
  (let ((last-ring (annotate-chain-last-ring chain)))
    (overlay-put last-ring 'hide-text t)))

(defun annotate-chain-show-text (chain)
  "Sets an overlay properties of the last ring of `CHAIN' so that
the annotation's text will be rendered."
  (let ((last-ring (annotate-chain-last-ring chain)))
    (overlay-put last-ring 'hide-text nil)))

(defun annotate-chain-hide-text-p (chain)
"Non nil if the annotation's text contained in the last ring of `CHAIN' must not be rendered."
  (let ((last-ring (annotate-chain-last (cl-first chain))))
    (annotate-tail-overlay-hide-text-p last-ring)))

(defun annotate-tail-overlay-hide-text-p (overlay)
  "Get the property for hiding the annotation text from `OVERLAY'."
  (overlay-get overlay 'hide-text))

(defun annotate-create-annotation (start end annotation-text annotated-text
                                         &optional color-index)
  "Create a new annotation for selected region (from `START' to  `END'.

Here the argument 'ANNOTATION-TEXT' is the string that appears
on the margin of the window and 'annotated-text' is the string
that is underlined.

If this function is called from procedure
'annotate-load-annotations' the argument `ANNOTATED-TEXT'
should be not null. In this case we know that an annotation
existed in a text interval defined in the database
metadata (the database located in the file specified by the
variable 'annotate-file') and should just be
restored. Sometimes the annotated text (see above) can not be
found in said interval because the annotated file's content
changed and `annotate-mode' could not track the
changes (e.g. save the file when annotate-mode was not
active/loaded) in this case the matching
text ('annotated-text') is searched in a region surrounding the
interval and, if found, the buffer is annotated right there.

The searched interval can be customized setting the variable:
'annotate-search-region-lines-delta'."
  (cl-labels ((create-annotation (start end annotation-text)
               (cl-incf annotate-colors-index-counter)
               (save-excursion
                 (let ((all-overlays ()))
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
                                    (highlight (make-overlay start end-overlay))
                                    (highlight-face (if color-index
                                                        (elt annotate-highlight-faces
                                                             color-index)
                                                      (annotate--current-highlight-face)))
                                    (annotation-face (if color-index
                                                         (elt annotate-annotation-text-faces
                                                              color-index)
                                                       (annotate--current-annotation-text-face))))
                               (overlay-put highlight 'face highlight-face)
                               (overlay-put highlight 'annotation annotation-text)
                               (overlay-put highlight 'annotation-face annotation-face)
                               (annotate-overlay-maybe-set-help-echo highlight
                                                                     annotation-text)
                               (annotate-annotation-chain-position highlight
                                                                   annotate-prop-chain-pos-marker-last)
                               (push highlight all-overlays))))))
                     (setf start (point)))
                   (annotate--remap-chain-pos (reverse (mapcar #'maybe-force-newline-policy
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
                                       (cond
                                        ((facep face)
                                         (face-attribute face :height nil 'default))
                                        ((and (consp face)
                                              (keywordp (cl-first face))) ; a plist
                                         (cl-getf face :height
                                                  (face-attribute 'default :height)))
                                        ((consp face) ; a list of named face, first wins
                                         (face-attribute (cl-first face) :height nil 'default))
                                        (t
                                         (face-attribute 'default :height))))
                                     (remq nil all-faces)))
                       (setf force-newline-p
                             (cl-find-if (lambda (a) (/= a default-face-height))
                                         all-faces-height))
                       (when force-newline-p
                         (annotate-annotation-force-newline-policy annotation))
                       annotation))))
    (if (annotate-string-empty-p annotation-text)
        (signal 'annotate-empty-annotation-text-error t)
      (progn
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
          (font-lock-fontify-block 1))))))

(defun annotate-overlay-put-echo-help (overlay text)
  "Set the property `HELP-ECHO' to `TEXT' in overlay `OVERLAY'."
  (overlay-put overlay 'help-echo text))

(defun annotate-overlay-get-echo-help (overlay)
  "Set the property `HELP-ECHO' from overlay `OVERLAY'."
  (overlay-get overlay 'help-echo))

(defun annotate-overlay-maybe-set-help-echo (overlay annotation-text)
  "Set the property `HELP-ECHO' to `TEXT' in overlay `OVERLAY' if
the annotations should be shown in a popup fashion.

See the variable: `ANNOTATE-USE-ECHO-AREA'."
  (when annotate-use-echo-area
    (annotate-overlay-put-echo-help overlay annotation-text)))

(defun annotate--delete-annotation-chain (annotation)
  "Delete `ANNOTATION' from a buffer and the chain it belongs to.

This function is not part of the public API."
  (annotate-ensure-annotation (annotation)
   (save-excursion
     (with-current-buffer (current-buffer)
       (let* ((chain (annotate-find-chain annotation)))
              ;; (filename      (annotate-actual-file-name))
         (dolist (single-element chain)
           (goto-char (overlay-end single-element))
           (move-end-of-line nil)
           (annotate--remove-annotation-property (overlay-start single-element)
                                                 (overlay-end   single-element))
           (delete-overlay single-element)))))))

(defun annotate--delete-annotation-chain-ring (annotation-ring)
  "Delete overlay of `ANNOTATION-RING' from a buffer.

A ring is a single element of an annotation chain.

This function is not part of the public API."
  (annotate-ensure-annotation (annotation-ring)
    (save-excursion
      (goto-char (overlay-end annotation-ring))
      (move-end-of-line nil)
      (annotate--remove-annotation-property (overlay-start annotation-ring)
                                            (overlay-end   annotation-ring))
      (delete-overlay annotation-ring))))

(defun annotate-delete-chain-element (annotation)
  "Delete a ring (a ring is a single element of an ANNOTATION chain.)
from a chain where `ANNOTATION' belong."
  (annotate-ensure-annotation (annotation)
    (let* ((chain                   (annotate-find-chain    annotation))
           (first-of-chain-p        (annotate-chain-first-p annotation))
           (last-of-chain-p         (annotate-chain-last-p  annotation))
           (only-element-in-chain-p (= (length chain) 1)))
      (annotate--delete-annotation-chain-ring annotation)
      (when (not only-element-in-chain-p)
        (cond
         (first-of-chain-p
          (let ((second-annotation (cl-second chain)))
            (when (not (annotate-chain-last-p second-annotation))
              (annotate-annotation-set-chain-first second-annotation))))
         (last-of-chain-p
          (let ((annotation-before (elt chain (- (length chain) 2))))
            (annotate-annotation-set-chain-last annotation-before))))))))

(defun annotate--cut-left-annotation (annotation)
  "Trims `ANNOTATION' exactly one character from the start."
  (annotate-ensure-annotation (annotation)
    (let* ((chain                       (annotate-find-chain annotation))
           (first-annotation            (annotate-chain-first annotation))
           (chain-start-pos             (overlay-start first-annotation))
           (first-annotation-ending-pos (overlay-end   first-annotation))
           (new-starting-pos            (1+ chain-start-pos)))
      (cond
       ((>= new-starting-pos
            first-annotation-ending-pos) ; delete chain element or entire annotation
        (if (= (length chain)
               1)                        ; the chain is formed by just one element, delete entirely
            (annotate--delete-annotation-chain first-annotation)
          (annotate-delete-chain-element first-annotation))) ; delete just the first element of the chain
       (t
        (move-overlay first-annotation new-starting-pos first-annotation-ending-pos))))))

(defun annotate--cut-right-annotation (annotation &optional refontify-buffer)
  "Trims `ANNOTATION' exactly one character from the end."
  (annotate-ensure-annotation (annotation)
    (let* ((chain                        (annotate-find-chain annotation))
           (last-annotation              (annotate-chain-last annotation))
           (last-annotation-ending-pos   (overlay-end last-annotation))
           (last-annotation-starting-pos (overlay-start last-annotation))
           (new-ending-pos               (1- last-annotation-ending-pos)))
      (cond
       ((<= new-ending-pos
            last-annotation-starting-pos) ; delete chain element or entire annotation
        (if (= (length chain) 1)          ; the chain is formed by just one element, delete entirely
            (annotate--delete-annotation-chain last-annotation)
          (progn ; delete just the last element of the chain
            (annotate-delete-chain-element last-annotation)
            (when refontify-buffer
              (font-lock-flush)))))
       (t
        (move-overlay last-annotation last-annotation-starting-pos new-ending-pos))))))

(defun annotate--delete-annotation-chain-prevent-modification (annotation)
"Delete an annotation chain backing up and restoring modification
status of the buffer before deletion occured.

This function is not part of the public API."
  (annotate-ensure-annotation (annotation)
    (with-silent-modifications
     (annotate--delete-annotation-chain annotation))))

(defun annotate--confirm-annotation-delete ()
  "Prompt user for delete confirmation.
This function is not part of the public API."
  (or (not annotate-annotation-confirm-deletion)
      (y-or-n-p annotate-confirm-deleting-annotation-prompt)))

(cl-defun annotate-delete-annotation (&optional (point (point)))
  "Command to delete an annotation, `POINT' is the buffer
position where to look for annotation (default the cursor
point)."
  (interactive)
  (when-let ((annotation (annotate-annotation-at point)))
    (let* ((delete-confirmed-p (annotate--confirm-annotation-delete)))
      (when delete-confirmed-p
        (annotate--delete-annotation-chain annotation)
        (font-lock-flush)))))

(defun annotate-change-annotation (pos)
  "Change annotation at `POS'.  If empty, delete annotation."
  (let* ((highlight       (annotate-annotation-at pos))
         (annotation-text (read-from-minibuffer annotate-annotation-prompt
                                                (overlay-get highlight 'annotation))))
    (cl-labels ((change (annotation)
                  (let ((chain (annotate-find-chain annotation)))
                    (dolist (single-element chain)
                      (annotate-overlay-maybe-set-help-echo single-element annotation-text)
                      (overlay-put single-element 'annotation annotation-text)))))
      (save-excursion
        (cond
         ;; annotation was cancelled:
         ((null annotation-text))
         ;; annotation was erased:
         ((string= "" annotation-text)
          (let* ((delete-confirmed-p (annotate--confirm-annotation-delete)))
            (when delete-confirmed-p
              (annotate--delete-annotation-chain-prevent-modification highlight))))
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
  "Return the annotations (overlay where (annotationp overlay) -> t)
at positions `POS' or nil if no annotations exists at pos.

NOTE this assumes that annotations never overlaps so the list of
all annotations can contains only one element maximum."
  (let ((all (cl-remove-if-not #'annotationp
                               (overlays-at pos))))
    (cl-first all)))

(defun annotate-previous-annotation-ends (pos)
  "Return the previous annotation that ends before `POS' or nil if no annotation
was found.
NOTE this assumes that annotations never overlaps."
  (cl-labels ((previous-annotation-ends (start)
                (let ((annotation (annotate-annotation-at start)))
                  (while (and (> start
                                  (point-min))
                              (null annotation))
                    (setf start (previous-overlay-change start))
                    (setf annotation (annotate-annotation-at start)))
                  annotation)))
    (let ((annotation (annotate-annotation-at pos)))
      (if annotation
          (previous-annotation-ends (1- (overlay-start annotation)))
        (previous-annotation-ends pos)))))

(defun annotate-previous-annotation (annotation)
  "Return the annotation before `ANNOTATIONS' or nil if no such
annotation exists."
 (annotate-previous-annotation-ends (overlay-start (annotate-chain-first annotation))))

(defun annotate-next-annotation-starts (pos)
  "Return the previous annotation that ends before `POS' or nil if no annotation
was found.
NOTE this assumes that annotations never overlaps."
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

(defun annotate-next-annotation (annotation)
  "Return the annotation after `ANNOTATIONS' or nil if no such
annotation exists."
 (annotate-next-annotation-starts (overlay-end (annotate-chain-last annotation))))

(defun annotate-symbol-strictly-at-point ()
  "Return non nil if a symbol is at char immediately following
the point. This is needed as `THING-AT-POINT' family of
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
 "Make an annotation record that represent an annotation
starting at `BEGINNING', terminate at `ENDING' with annotation
content `ANNOTATION' and annotated text `ANNOTATED-TEXT'."
  (list beginning ending annotation annotated-text))

(defun annotate-all-annotations ()
  "Return a list of all annotations in the current buffer."
  (cl-remove-if-not #'annotationp (overlays-in 0 (buffer-size))))

(defun annotate-describe-annotations ()
  "Return a list of all annotations in the current buffer.
The format is suitable for database dump."
  (let ((all-annotations (cl-remove-if-not #'annotationp (overlays-in 0 (buffer-size))))
        (chain-visited   ()))
    (remq nil
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
  "Is the name of this file (`FILENAME') equals to the info root node?"
  (string= filename
           annotate-info-root-name))

(defun annotate-guess-file-format (filename)
  "Try to guess the file format from `FILENAME'.
Non nil if the file format is supported from 'annotate' in a more
sophisticated way than plain text."
  (cl-labels ((file-contents ()
                             (with-temp-buffer
                               (insert-file-contents filename)
                               (buffer-string)))
              (info-format-p () ; lot of guesswork here :(
                             (cond
                              ((annotate-info-root-dir-p filename)
                               :info)
                              (t
                               (let* ((file-contents   (file-contents))
                                      (has-info-p      (string-match "info" filename))
                                      (separator-re    "\^L?\^_\^L?\^J")
                                      (has-separator-p (string-match separator-re file-contents))
                                      (has-node-p      (string-match "Node:" file-contents)))
                                 (if (and has-separator-p
                                          (or has-node-p
                                              has-info-p))
                                     :info
                                   nil)))))
              (gpg-format-p ()
                            (with-temp-buffer
                              (let* ((magic-0    #x8c)
                                     (magic-1    #x0d)
                                     (magic-4    #x03)
                                     (magic-5    #x02)
                                     (attributes (file-attributes filename))
                                     (file-size  (file-attribute-size attributes)))
                                (when (> file-size 6)
                                  (let* ((bytes      (insert-file-contents-literally filename
                                                                                     nil
                                                                                     0
                                                                                     7)))
                                    (setf bytes
                                          (cl-loop for i from 1 to 6 collect
                                                   (elt (buffer-substring-no-properties i (1+ i))
                                                        0)))
                                    (when (and (= (logand (elt bytes 0) #x0000ff) magic-0)
                                               (= (logand (elt bytes 1) #x0000ff) magic-1)
                                               (= (logand (elt bytes 4) #x0000ff) magic-4)
                                               (= (logand (elt bytes 5) #x0000ff) magic-5))
                                      :encrypted-symmetric)))))))
    (or (gpg-format-p)
        (info-format-p)))) ;; keep this one for last as it is the slowest

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
  "Callback called when an annotate-summary-show-annotation-button is activated."
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

(defun annotate-update-visited-buffer-maybe (filename)
  "Reload annotation mode in the buffer visiting `FILENAME', if such buffer exists."
  (let ((visited-buffer (find-buffer-visiting filename)))
    (when visited-buffer ;; a buffer is visiting the file
      (with-current-buffer visited-buffer
        (annotate-mode -1)
        (annotate-mode  1)))))

(defun annotate-summary-delete-annotation-button-pressed (button)
 "Callback for summary window fired when a 'delete' button is
pressed."
  (let* ((filename        (button-get button 'file))
         (beginning       (button-get button 'beginning))
         (ending          (button-get button 'ending))
         (db              (annotate-load-annotation-data t))
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
                    (read-only-mode 1))))
      (redraw-summary-window)
      (annotate-update-visited-buffer-maybe filename))))

(defun annotate-summary-replace-annotation-button-pressed (button)
  "Callback for summary window fired when a 'replace' button is
pressed."
  (let* ((filename             (button-get button 'file))
         (annotation-beginning (button-get button 'beginning))
         (annotation-ending    (button-get button 'ending))
         (query                (button-get button 'query))
         (db                   (annotate-load-annotation-data t))
         (old-annotation       (button-get button 'text))
         (new-annotation-text  (read-from-minibuffer annotate-annotation-prompt old-annotation)))
    (when (not (annotate-string-empty-p new-annotation-text))
      (let ((replaced-annotation-db (annotate-db-replace-annotation db
                                                                    filename
                                                                    annotation-beginning
                                                                    annotation-ending
                                                                    new-annotation-text)))
        (annotate-dump-annotation-data replaced-annotation-db)
        (annotate-update-visited-buffer-maybe filename)
        (annotate-show-annotation-summary query nil nil)))))

(cl-defun annotate-wrap-text (text &optional (wrapper "\""))
  "Wrap string `TEXT' with string `WRAPPER'."
  (concat wrapper text wrapper))

(cl-defun annotate-unwrap-text (text &optional (wrapper "\"") (left-side t))
  "Remove `WRAPPER' at both ends from `TEXT'."
  (let ((results        text)
        (wrapper-length (length wrapper)))
    (when (>= (length text)
              wrapper-length)
      (if left-side
          (let ((maybe-wrapper (substring results 0 wrapper-length)))
            (when (string= maybe-wrapper wrapper)
              (setf results (substring results wrapper-length))
              (setf results (annotate-unwrap-text results wrapper nil))))
        (let ((maybe-wrapper (substring results
                                        (- (length results)
                                           wrapper-length))))
          (when (string= maybe-wrapper wrapper)
            (setf results (substring results 0 (- (length results)
                                                  wrapper-length)))))))
    results))

(cl-defun annotate-show-annotation-summary (&optional arg-query cut-above-point (save-annotations t))
 "Show a summary of all the annotations in a temp buffer, the
results can be filtered with a simple query language: see
`ANNOTATE-SUMMARY-FILTER-DB'."
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
                         (annotate-wrap-text text "\""))
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
              (build-snippet-from-buffer (filename annotation-begin annotation-end)
                (let ((visited-buffer (find-buffer-visiting filename)))
                  (when visited-buffer ;; a buffer is visiting the file
                    (with-current-buffer visited-buffer
                      (let ((raw-snippet (buffer-substring-no-properties annotation-begin
                                                                         annotation-end)))
                        (clean-snippet raw-snippet))))))
              (build-snippet-from-file (filename annotation-begin annotation-end)
                (with-temp-buffer
                  (insert-file-contents filename
                                        nil
                                        (1- annotation-begin)
                                        (1- annotation-end))
                  (clean-snippet (buffer-string))))
              (build-snippet (filename annotation-begin annotation-end)
                             (if (file-exists-p filename)
                                 (cond
                                  ((eq (annotate-guess-file-format filename)
                                        :info)
                                   (clean-snippet (build-snippet-info filename
                                                                      annotation-begin
                                                                      annotation-end)))
                                  (t
                                   (or (build-snippet-from-buffer filename
                                                                  annotation-begin
                                                                  annotation-end)
                                       (build-snippet-from-file   filename
                                                                  annotation-begin
                                                                  annotation-end))))
                               (if (annotate-info-root-dir-p filename)
                                   (clean-snippet (build-snippet-info filename
                                                                      annotation-begin
                                                                      annotation-end))
                                 annotate-error-summary-win-filename-invalid)))
              (db-empty-p    (dump)
                             (cl-every (lambda (a)
                                         (cl-every #'null
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
    (when save-annotations
      (annotate-save-all-annotated-buffers))
    (let* ((filter-query (get-query))
           (dump         (annotate-summary-filter-db (annotate-load-annotation-data t)
                                                     filter-query
                                                     cut-above-point)))
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
summary window is shown.")

(defvar annotate-summary-query-current-token nil
  "Holds the next token of the query in `ANNOTATE-SUMMARY-QUERY'.")

(defun annotate-summary-query-lexer-symbol (res)
  "The symbol identifying the token (e.g. 'and)."
  (elt res 0))

(defun annotate-summary-query-lexer-string (res)
  "The string associated with this token."
  (elt res 1))

(defun annotate-summary-query-lexer-start (res)
 "The starting point of the substring of
`annotate-summary-query' for this token."
  (elt res 2))

(defun annotate-summary-query-lexer-end (res)
  "The ending point of the substring of
`annotate-summary-query' for this token."

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
  the value (the actual substring for this token)

- third and fourth element (currently unused)
 the substring limits for this token (as returned by
 `match-beginning' and `match-end'.

Note that spaces are ignored and all the tokens except `re' must
not be prefixed with a backslash to match.  So, for example not ->
will match the token type 'not but \not will match the token 're;
this way we can 'protect' a regexp that contains reserved
keyword (aka escaping).

The special value :no-more-token is returned after the whole
input is processed.

Calling this function with value of LOOK-AHEAD-P nil will `CONSUME' the
token from `annotate-summary-query' (i.e. that string is modified).

example:
'a and b' -> 'and b', '(re \"a\" 0 1)

when LOOK-AHEAD-P is not nil the token is recognized but not cut away
from `annotate-summary-query'.

example:
'a and b' -> 'a and b', '(re \"a\" 0 1)"
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
  "Non nil if there are no more tokens in `ANNOTATE-SUMMARY-QUERY'."
  (eq token :no-more-tokens))

(defun annotate-summary-token-symbol-match (looking-symbol token)
  "Return non nil if `LOOKING-SYMBOL' is 'eq' to the symbol
component of `TOKEN'."
  (eq looking-symbol
      (annotate-summary-query-lexer-symbol token)))

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
ANYTHING   := .*           ; as a regular expression
AND        := 'and'
OR         := 'or'
NOT        := 'not'
DELIMITER  := \" ; ASCII 34 (dec) 22 (hex)

Arguments:

- filter-fn is a function that accept two parameters: the regular
  expression to match (a token of type 're, see the lexer
  `ANNOTATE-SUMMARY-LEXER' and a single annotation record (see
  `ANNOTATE-LOAD-ANNOTATIONS').

  This function will reject (its value is nil) records that do
  not match the annotation.

- annotation
  the annotation to test

- res the results of this production rule (internal use only)."
  (cl-labels ((token-symbol-match-p (looking-symbol token)
                (annotate-summary-token-symbol-match looking-symbol token))
              ;; this function will parse the rule operator
              ;; OPERATOR   := AND | OR
              ;; NOTE OPERATOR NOTE
              ;; filter-fn     see the docstring
              ;; matchp        non nil if (funcall filter-fn previous-token) is not nil
              (operator (filter-fn annotation matchp)
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
                       (signal 'annotate-query-parsing-error
                               (list (format (concat "Expecting for operator "
                                                     "('and' or 'or') or \")\". "
                                                     "found %S instead")
                                             look-ahead-string))))
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
                  (signal 'annotate-query-parsing-error '("Unmatched parens")))
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
                    (signal 'annotate-query-parsing-error '("No more input after 'not'"))
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
                    (signal 'annotate-query-parsing-error '("No more input after 'and'"))
                  (and lhs rhs)))) ; both rules must match as this is a logic and
             ;; trying to match the rule:
             ;; NOTE := NOTE OR NOTE
             ((token-symbol-match-p 'or look-ahead)
              (annotate-summary-lexer) ; consume the 'or'
              (let ((lhs res)          ; the left side of this rule (lhs OR rhs)
                    (rhs (annotate-summary-query-parse-note filter-fn annotation :error))) ; recurse
                (if (eq :error rhs)
                    (signal 'annotate-query-parsing-error '("No more input after 'or'"))
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
                     (matchp    (funcall filter-fn unescaped annotation)))   ; apply the filter function
                ;; and finally continue the parsing saving the results
                ;; of applying the filter-fn function
                (operator filter-fn annotation matchp)))
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
                (operator filter-fn annotation matchp)))))
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

Note: this function returns the annotation part of the record, see
`ANNOTATE-LOAD-ANNOTATIONS'."
  (lambda (annotation query file-filter-fn note-filter-fn)
    (let ((annotate-summary-query query) ; save the query
          (query-notes-only       nil)) ; the query for just the notes
      (let ((next-token (annotate-summary-lexer))) ; get potential file-mask
        ;; if there are no more tokens just return all the records
        ;; these match the empty string as in rule
        ;; EXPRESSION := epsilon
        (if (annotate-summary-query-parse-end-input-p next-token)
            (annotate-annotations-from-dump annotation)
          ;; otherwise test the record with the file-mask
          (let* ((quoted-file-mask-p  (annotate-summary-token-symbol-match 'escaped-re
                                                                           next-token))
                 (file-mask-raw       (annotate-summary-query-lexer-string next-token))
                 (file-mask           (if quoted-file-mask-p
                                          (annotate-unwrap-text file-mask-raw "\"")
                                         file-mask-raw))
                 (filtered-annotation (funcall file-filter-fn file-mask annotation))
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
                          (signal 'annotate-query-parsing-error '("No more input after 'or'"))
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
                        (signal 'annotate-query-parsing-error '("No more input after 'and'"))
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
                  (signal 'annotate-query-parsing-error
                          (list (format "Unknown operator: %s is not in '(and, or)"
                                        (annotate-summary-query-lexer-string operator-token))))))))))))))

(defun annotate-summary-filter-db (annotations-dump query remove-annotations-cutoff-point)
  "Filter an annotation database with a query.

The argument `QUERY' is a string that respect a simple syntax:

- [file-mask] [(and | or) [not] regex-note [(and | or) [not] regexp-note ...]]

where

- file-mask: is a regular expression that should match the filepath
 the annotation refers to;
- and, or, not : you guess? Classics logical operators;
- regex-note: the text of annotation must match this regular expression.

Examples:

- lisp$ and TODO
 matches the text 'TODO' in all lisp files

Parenthesis can be used for the expression related to the text of
annotation, like this:

- lisp$ and (TODO or important)
 the same as above but checks also for string 'important'

- /home/foo/
 matches all the annotation that refers to file in the directory
 '/home/foo'

- /home/foo/ and not minor
 matches all the annotation that refers to file in the directory
 '/home/foo' and that not contains the text 'minor'.

- .* and \"not\"
 the \" can be used to escape strings

If you want to remove from summary the annotations that appears
before a position in buffer set 'remove-annotations-cutoff-point' to said
position.

The annotations in each record are sorted by starting point in ascending order."
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
                                       (remq nil filtered-annotations))
                                 (when filtered-annotations
                                   (let ((filename (annotate-filename-from-dump
                                                    single-annotation))
                                         (checksum (annotate-checksum-from-dump
                                                    single-annotation)))
                                     (setf filtered-annotations
                                           (sort filtered-annotations
                                                 #'annotate-db-annotations-starts-before-p))
                                     (when remove-annotations-cutoff-point
                                       (setf filtered-annotations
                                             (cl-remove-if (lambda (a)
                                                             (< (annotate-ending-of-annotation a)
                                                                remove-annotations-cutoff-point))
                                                           filtered-annotations)))
                                     (annotate-make-annotation-dump-entry filename
                                                                          filtered-annotations
                                                                          checksum))))))
         (filtered           (remq nil (mapcar filter annotations-dump))))
    filtered))

;;;; end of filtering: parser, lexer, etc.

;;;; misc commands

(defun annotate-summary-of-file-from-current-pos ()
 "Shows a summary window that contains only the annotations in
the current buffer and that starts after the current cursor's
position."
  (interactive)
  (with-current-buffer (current-buffer)
    (when buffer-file-name
      (annotate-show-annotation-summary (annotate-wrap-text buffer-file-name "\"")
                                        (point)))))

;;; switching database

(defun annotate-buffers-annotate-mode ()
  "Return a list of all the buffers that have
annotate minor mode active."
  (let ((all-buffers (buffer-list)))
    (cl-labels ((annotate-mode-p (buffer)
                  (with-current-buffer buffer
                    (and (boundp 'annotate-mode) ;FIXME: Redundant test?
                         annotate-mode))))
      (cl-remove-if-not #'annotate-mode-p all-buffers))))

(cl-defun annotate-switch-db (&optional (force-load nil) (database-file-path nil))
 "Ask the user for a new annotation database files, load it and
refresh all the annotations contained in each buffer where
annotate minor mode is active.

if `DATABASE-FILE-PATH' is nil (the default) a prompt asking for
a file containing database is presented to the user, otherwise
the value of this argument is used.

If `FORCE-LOAD' is non nil no prompt asking user for confirmation
about loading the new file is shown.

Note: this function will attempt to load (compile and
eval/execute) the content of the file as it was elisp source
code, always use load files from trusted sources!"
  (interactive)
  (let ((new-db (or database-file-path
                    (read-file-name "Database file location: "))))
    (when (not (annotate-string-empty-p new-db))
      (if (file-exists-p new-db)
          (let* ((confirm-message "Loading elisp file from untrusted source may results in severe security problems. Load %S?")
                 (load-file-confirmed (or force-load
                                          (y-or-n-p (format confirm-message new-db)))))
            (if load-file-confirmed
                (progn
                  (setf annotate-file new-db)
                  (cl-loop for annotated-buffer in (annotate-buffers-annotate-mode) do
                           (with-current-buffer annotated-buffer
                             (with-silent-modifications
                               (annotate-mode -1)
                               (annotate-mode  1)))))
              (when annotate-use-messages
                (message "Load aborted by the user"))))
        (signal 'annotate-db-file-not-found (list new-db))))))

;; end of switching database

;;; merging database

(defun annotate--merge-interval (a b)
"Merge two annotation interval `A' and `B'.

The new interval is expanded so that includes `A' and `B'."
  (let ((new-left-limit  (min (annotate--interval-left-limit a)
                              (annotate--interval-left-limit b)))
        (new-right-limit (max (annotate--interval-right-limit a)
                              (annotate--interval-right-limit b))))
    (annotate--make-interval new-left-limit
                             new-right-limit)))

(defun annotate--db-annotations-overlaps-p (annotation-a annotation-b)
  "Return non nil if `ANNOTATION-A' and `ANNOTATION-B' overlaps."
  (let ((interval-a (annotate-annotation-interval annotation-a))
        (interval-b (annotate-annotation-interval annotation-b)))
    (not (or (< (annotate--interval-right-limit interval-b)
                (annotate--interval-left-limit interval-a))
             (> (annotate--interval-left-limit interval-b)
                (annotate--interval-right-limit interval-a))))))

(defun annotate--db-merge-annotations (host guest)
  "Merge annotation GUEST into annotation HOST.
Uses `annotate--merge-interval'."
  (when (annotate--db-annotations-overlaps-p host guest)
    (let* ((interval-host       (annotate-annotation-interval host))
           (interval-guest      (annotate-annotation-interval guest))
           (text-host           (annotate-annotation-string   host))
           (text-guest          (annotate-annotation-string   guest))
           (new-interval        (annotate--merge-interval     interval-host interval-guest))
           (new-annotation-text (concat text-host " " text-guest))
           (left                (annotate--interval-left-limit new-interval))
           (right               (1+ (annotate--interval-right-limit new-interval)))
           (new-annotated-text  (with-current-buffer (current-buffer)
                                  (buffer-substring-no-properties left right))))
      (annotate-make-annotation left right new-annotation-text new-annotated-text))))

(defun annotate--db-remove-overlap-annotations (annotations &optional accum)
"Recursively merges overlapping annotations in `ANNOTATIONS'
using `ANNOTATE--DB-MERGE-ANNOTATIONS'."
  (if (= (length annotations) 1)
      (push (cl-first annotations) accum)
    (let* ((probe            (cl-first annotations))
           (rest-annotations (cl-rest annotations))
           (position-overlap (cl-position-if (lambda (a)
                                               (annotate--db-annotations-overlaps-p probe a))
                                             rest-annotations)))
      (if position-overlap
          (let* ((annotation-overlapping    (elt rest-annotations position-overlap))
                 (annotations-before-merged (cl-subseq rest-annotations 0 position-overlap))
                 (annotations-after-merged  (cl-subseq rest-annotations (1+ position-overlap)))
                 (merged-annotation         (annotate--db-merge-annotations probe
                                                                            annotation-overlapping)))
            (annotate--db-remove-overlap-annotations (append annotations-before-merged
                                                             (list merged-annotation)
                                                             annotations-after-merged)
                                                     accum))
        (annotate--db-remove-overlap-annotations rest-annotations (push probe accum))))))

(defun annotate--db-merge-databases (db-1 db-2 &optional accum)
"Recursively merge database `DB-1' and `DB-2'."
  (cl-labels ((find-same-file-record (record annotations-db)
                (let ((record-filename (annotate-filename-from-dump record)))
                  (cl-find-if (lambda (a)
                                (let ((scanned-record-filename (annotate-filename-from-dump a)))
                                  (file-equal-p record-filename scanned-record-filename)))
                              annotations-db))))
    (if (null db-1)
        (append accum db-2)
      (let* ((first-record     (cl-first db-1))
             (same-file-record (find-same-file-record first-record db-2)))
        (if same-file-record
            (let* ((filename                 (annotate-filename-from-dump first-record))
                   (concatenated-annotations (append (annotate-annotations-from-dump first-record)
                                                     (annotate-annotations-from-dump same-file-record)))
                   (non-overlapped-annotations (annotate--db-remove-overlap-annotations concatenated-annotations))
                   (concatenated-checksum      (annotate-checksum-from-dump first-record))
                   (concatenated-record        (annotate-make-record filename
                                                                     non-overlapped-annotations
                                                                     concatenated-checksum))
                   (rest-of-db-2               (cl-remove-if
                                                (lambda (a)
                                                  (let ((record-filename (annotate-filename-from-dump a)))
                                                    (file-equal-p record-filename filename)))
                                                db-2)))
              (annotate--db-merge-databases (cl-rest db-1)
                                            rest-of-db-2
                                            (push concatenated-record accum)))
          (annotate--db-merge-databases (cl-rest db-1)
                                        db-2
                                        (push first-record accum)))))))

(defun annotate-import-annotations ()
"Prompt user for an annotation database file and merge it into
their personal database."
  (interactive)
  (cl-flet ((deserialize-db (file)
             (ignore-errors (annotate--deserialize-database-file file)))
            (remove-non-existing-files (annotations)
             (cl-remove-if-not (lambda (a)
                                 (let ((filename (annotate-filename-from-dump a)))
                                   (file-exists-p filename)))
                               annotations)))
  (let* ((confirm-message    (concat "Importing databases from untrusted source may cause severe "
                                     "security issues, continue?"))
         (import-confirmed-p (or (not annotate-database-confirm-import)
                                 (y-or-n-p confirm-message))))
    (when import-confirmed-p
      (let* ((imported-db-name (read-file-name "Choose the database to import: "))
             (imported-db      (remove-non-existing-files (deserialize-db imported-db-name)))
             (hosting-db       (deserialize-db annotate-file))
             (merged-db        (annotate--db-merge-databases hosting-db imported-db)))
        (annotate-dump-annotation-data merged-db)
        (annotate-switch-db t annotate-file)
        (when annotate-use-messages
          (message "Imported annotations from %s." imported-db-name)))))))

;;; end of merging datatase

(provide 'annotate)
;;; annotate.el ends here
