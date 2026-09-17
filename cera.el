;;; cera.el --- Write into a temporary field in another buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/cera
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `cera-read' opens a temporary, overlay-decorated field below the text
;; it is given and returns what was typed into it.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'org-faces)
(require 'subr-x)
(require 'text-mode)


;;;; Customization

(defgroup cera nil
  "Write into a temporary field in another buffer."
  :group 'text
  :prefix "cera-")

(defcustom cera-body-shade 10
  "Percent the field's background moves away from the theme's own.
Darker on a light theme and lighter on a dark one, so the field reads as
the same surface with the text set into it rather than as a slab laid
over it."
  :type 'natnum)

(defcustom cera-completion-function #'cera-complete-with-table
  "Function offering the completion to show at point in the field.
Called with BOUNDS, a cons of the field's positions, and the TABLE
`cera-read' was given.  It returns a `completion-at-point' list, or nil
to offer nothing.  A consumer replaces it to complete on its own terms:
what the table holds, and where in the field it applies."
  :type 'function)

(defcustom cera-input-prefix nil
  "Text drawn where the bracket closes into the input, or nil for none.
An icon or a short label: either the string to draw, carrying whatever
properties it should be drawn with, or a function of no arguments
returning one.  A consumer sets this in a buffer, or binds it around
`cera-read', to mark what the field is being read for.  It rides with
the closing bracket, and the field's lines above align behind it."
  :type '(choice (const :tag "None" nil) string function))

(defcustom cera-input-prefix-width 2
  "Columns kept for the input's prefix, whatever it is set to.
The bracket resumes past them and the input starts after that, as the
display measures the prefix rather than as its characters count: an icon
glyph covering two columns and a single character both leave the input
in the same place.  A prefix wider than this pushes the bracket along."
  :type 'natnum)

(defcustom cera-indent 0
  "Column the bracket is drawn at, counted from the window's text area.
Zero hangs the bracket off the left edge.  A larger column moves the
whole field right and draws the bracket over the leading whitespace of
the lines it brackets, which is taken out of the display: set it to
`cera-bracket-width' short of where indented text sits and that text
stays where it is, with the bracket in the indentation rather than
beside it.  Either the column, or a function of no arguments returning
one."
  :type '(choice natnum function))

(defcustom cera-completion-space 0
  "Lines kept free below the field while completion is in region.
A frontend that draws over the field, whether in a childframe or with
overlays, covers the text below it unless somewhere is made for it to
land; one that opens a window needs no room at all.  Zero reserves
nothing, and a frontend that overlays turns this on — loading its
integration file does."
  :type 'natnum)

(defun cera-complete-with-table (bounds table)
  "Offer TABLE across the whole of BOUNDS, to whoever asks for it.
The prefix counts as too short for a frontend that completes on its own,
so what the field was opened with is offered when it is asked for rather
than drawn over the text as it is written.  `cera-recall' reads the same
table in the minibuffer."
  (list (car bounds) (cdr bounds) table
        :exclusive 'no :company-prefix-length 0))


;;;; Faces

(defface cera-body
  '((t (:inherit org-block :extend t)))
  "Face of the temporary field.
Its background is moved off the theme's own, by `cera-body-shade'.")

(defface cera-border
  '((t (:inherit shadow)))
  "Face of the bracket connecting the source to the field below it.
It is drawn over the buffer's own text, so it carries no background of
its own.")

(defface cera-source
  '((t (:underline t)))
  "Face of the source text the field belongs to.")


;;;; State

(cl-defstruct (cera--session (:constructor cera--make-session))
  buffer begin end origin tail table overlays spacer source-face accepted
  group bindings base-bindings modified depth closed
  panes input static-overlays keymap width aligned)

(cl-defstruct (cera-pane (:constructor cera-pane))
  "A pane with ID, KIND, TEXT or BOUNDS, BRACKET, PREFIX and FACE.
KIND is `readonly' or `input'.  BRACKET draws the pane's left bracket,
and a pane with BRACKET nil shows its text alone.  PREFIX-POSITION is
`top' or `bottom'.  BOUNDS refer to existing text in this buffer;
supplied read-only TEXT is displayed without insertion.
An empty supplied read-only TEXT hides the pane."
  id kind text bounds (bracket t) prefix (prefix-position 'bottom) face
  connection)

(defun cera-set-pane-text (pane text)
  "Set PANE's supplied TEXT, and return PANE.
A consumer holds the struct but not its setters, which are known where
the struct is, so the text it wants a pane opened with is set here."
  (setf (cera-pane-text pane) text)
  pane)

(defvar cera-read-context-function nil
  "Optional function transforming the normalized panes of `cera-read'.
Called with PANES and returning PANES, before any document changes.")

(defvar cera-session-keymap nil
  "Optional keymap appended to the active session's input maps.")

(defvar cera-session-start-hook nil
  "Functions called with SESSION once its input is ready in the current buffer.")

(defvar cera-session-teardown-hook nil
  "Functions called with SESSION before its text and locals are restored.")

(defvar cera-session-restored-hook nil
  "Functions called with SESSION after its text and locals are restored.")

(defvar cera--drawing-static nil
  "Non-nil while drawing read-only panes rather than the input.")

(defvar-local cera--active nil
  "The inline field reader active in this buffer, or nil.")

(defvar cera--open-buffers nil
  "Buffers holding an open field.")

(defvar cera--released-depths nil
  "Recursion depths of released fields whose recursive edit is still to end.")

(defvar-local cera--source nil
  "Pair of markers bounding the text the field was opened for.")

(defvar-local cera--source-map nil
  "Original local keymap, wrapped in a list, when borrowing a text map.")

(defvar-local cera--emulation-map-alist nil
  "Save and cancel bindings active above the buffer's own maps.")

(defvar-local cera--trailing nil
  "Characters between the point and the end of the input, before a command.
The point comes back that far from the end when a command of the
buffer's takes it out of the field.")

(defun cera--live-p ()
  "Return non-nil when a field reader is active in this buffer."
  (and cera--active t))

(defun cera--field-bounds (&optional session)
  "Return SESSION's field as a cons of its BEGIN and END positions.
SESSION defaults to the reader active in this buffer."
  (let ((session (or session cera--active)))
    (cons (marker-position (cera--session-begin session))
          (marker-position (cera--session-end session)))))


;;;; Drawing

(defun cera--overlay (session begin end &rest properties)
  "Decorate BEGIN through END with PROPERTIES owned by SESSION."
  (let ((overlay (make-overlay begin end nil nil t)))
    (overlay-put overlay 'priority 1001)
    (overlay-put overlay 'cera t)
    (while properties
      (overlay-put overlay (pop properties) (pop properties)))
    (if cera--drawing-static
        (push overlay (cera--session-static-overlays session))
      (push overlay (cera--session-overlays session)))
    overlay))

(defconst cera--bracket-first
  (propertize "╭ " 'face 'cera-border)
  "Bracket opening the source's first line.")

(defconst cera--bracket-middle
  (propertize "│ " 'face 'cera-border)
  "Bracket opening a line the source or the field continues.")

(defconst cera--bracket-last
  (propertize "╰ " 'face 'cera-border)
  "Bracket opening the field's last line.")

(defconst cera--bracket-onward
  (propertize "─ " 'face 'cera-border)
  "Bracket carried on from the input's prefix into the input itself.
It meets the prefix directly and holds the input off by the space the
closing bracket holds the prefix off by.")

(defvar cera--aligned ""
  "Prefix the lines the field is drawn beside are positioned by.
Empty where they begin at the window's text area, which is where a
column counted from it means what it says.")

(defun cera--stretched-to (column &optional columns)
  "Return a space reaching to COLUMN of the window's text area.
Its width is what is left to COLUMN as the display measures it, so a
glyph drawn wider than the characters it counts for is still followed to
the same place.  Where the field is drawn behind a prefix of its own,
a column of the text area is not where the field is, and COLUMNS plain
ones are left instead."
  (if (string-empty-p cera--aligned)
      (propertize " " 'display `(space :align-to ,column))
    (make-string (max 0 (or columns 0)) ?\s)))

(defconst cera-bracket-width (string-width cera--bracket-last)
  "Columns the bracket takes before the text it opens.")

(defun cera--indent ()
  "Return the column the bracket is drawn at."
  (let ((indent (if (functionp cera-indent) (funcall cera-indent) cera-indent)))
    (if (natnump indent) indent 0)))

(defun cera--prefix-text (prefix)
  "Return PREFIX as text the bracket can be put in front of.
A line prefix is a string, an image, or a stretch of space, and the last
two are carried on a space of their own rather than concatenated."
  (cond
   ((null prefix) "")
   ((stringp prefix) prefix)
   (t (propertize " " 'display prefix))))

(defun cera--indented (prefix)
  "Return PREFIX held off to `cera--indent'."
  (let ((indent (cera--indent)))
    (if (zerop indent) prefix (concat (cera--stretched-to indent) prefix))))

(defun cera--line-number-pad ()
  "Return the space a suppressed line number leaves behind, or nil.
A line whose number is not drawn is not given the column the numbers
take either, so the field would stand left of the lines it belongs to.
The width is taken in pixels, which is the whole gutter; the column
count leaves out the padding the display puts around the number."
  (when (bound-and-true-p display-line-numbers)
    (let ((width (line-number-display-width t)))
      (when (> width 0)
        (propertize " " 'display `(space :width (,width)))))))

(defun cera--onward-column ()
  "Return the column the bracket resumes at, past the input's prefix."
  (+ (cera--indent) (string-width cera--bracket-last) cera-input-prefix-width))

(defun cera--input-column ()
  "Return the column the input itself begins at."
  (+ (cera--onward-column) (string-width cera--bracket-onward)))

(defun cera--absorb-indentation (session begin end)
  "Take the leading whitespace of each line of BEGIN to END out of the display.
The overlays belong to SESSION.  The bracket is drawn where that
whitespace was, so the text behind it resumes at the column the bracket
ends at rather than being pushed along by an indentation that is now
drawn twice."
  (unless (zerop (cera--indent))
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (let ((indentation (save-excursion (skip-chars-forward " \t") (point))))
          (when (> indentation (point))
            (cera--overlay session (point) indentation 'display "")))
        (forward-line 1)))))

(defun cera--input-prefix ()
  "Return the prefix drawn in front of the input, or nil for none.
It comes padded out to `cera-input-prefix-width' and followed by the
bracket carried on from it into the input."
  (let ((prefix (if (functionp cera-input-prefix)
                    (funcall cera-input-prefix)
                  cera-input-prefix)))
    (and (stringp prefix) (not (string-empty-p prefix))
         (concat prefix
                 (cera--stretched-to (cera--onward-column)
                                     (- cera-input-prefix-width
                                        (string-width prefix)))
                 cera--bracket-onward))))

(defun cera--shaded (color)
  "Return COLOR moved `cera-body-shade' percent away from the theme's own.
Which way is away follows the colour's own lightness, so the field reads
as the same surface on a light theme and on a dark one."
  (let* ((rgb (color-name-to-rgb color))
         (lightness (and rgb (nth 2 (apply #'color-rgb-to-hsl rgb)))))
    (if (and lightness (> lightness 0.5))
        (color-darken-name color cera-body-shade)
      (color-lighten-name color cera-body-shade))))

(defun cera--body-face ()
  "Return the field's face: the theme's background, moved towards the field."
  (let ((background (face-background 'default nil t)))
    (if (color-defined-p background)
        `(:inherit cera-body :background ,(cera--shaded background))
      'cera-body)))

(defun cera--pane-prefix (pane)
  "Return PANE's nonempty prefix, or nil."
  (let ((prefix (cera-pane-prefix pane)))
    (and (stringp prefix) (not (string-empty-p prefix)) prefix)))

(defun cera--pane-width (pane)
  "Return the columns occupied by PANE's bracket and optional prefix."
  (+ cera-bracket-width
     (if-let* ((prefix (cera--pane-prefix pane)))
         (+ (max cera-input-prefix-width (string-width prefix)) 2)
       0)))

(defun cera--pane-visible-p (pane)
  "Return non-nil unless PANE is supplied empty read-only text."
  (not (and (eq (cera-pane-kind pane) 'readonly)
            (not (cera-pane-bounds pane))
            (equal (cera-pane-text pane) ""))))

(defun cera--pane-endpoint (pane first last)
  "Choose PANE's corner for a row marked FIRST and LAST."
  (cond
   ((and first (eq (cera-pane-prefix-position pane) 'top)
         (not (eq (cera-pane-connection pane) 'previous))) 'first)
   ((and last (not (eq (cera-pane-connection pane) 'next))) 'last)
   ((and first (not (eq (cera-pane-connection pane) 'previous))) 'first)
   (t 'middle)))

(defun cera--pane-decoration (pane endpoint &optional aligned)
  "Return PANE's decoration at ENDPOINT, following ALIGNED text."
  (let* ((prefix (cera--pane-prefix pane))
         (label (and prefix
                     (eq endpoint (if (eq (cera-pane-prefix-position pane) 'top)
                                      'first 'last))))
         (bracket (pcase endpoint
                    ('first "╭ ")
                    ('last "╰ ")
                    (_ "│ "))))
    (let ((cera--aligned (or aligned ""))
          (cera-input-prefix prefix))
      (cera--indented
       (concat aligned (propertize bracket 'face 'cera-border)
               (when prefix
                 (if label (cera--input-prefix)
                   (cera--stretched-to
                    (+ (cera--indent) (cera--pane-width pane))
                    (- (cera--pane-width pane) cera-bracket-width)))))))))

(defun cera--draw-range (session pane begin end &optional aligned)
  "Decorate PANE's BEGIN to END in SESSION, following ALIGNED text.
A fixed number of overlays per window covers any length of input."
  (save-excursion
    (goto-char begin)
    (let* ((first-end (min end (1+ (line-end-position))))
           (last (save-excursion (goto-char (1- end)) (line-beginning-position)))
           (bracket (cera-pane-bracket pane))
           (continued (and bracket (cera--pane-decoration pane 'middle aligned)))
           (face (and (eq (cera-pane-kind pane) 'input)
                      (or (cera-pane-face pane) (cera--body-face)))))
      (cera--overlay session begin end 'face face
                     'line-prefix continued 'wrap-prefix continued
                     'display-line-numbers-disable
                     (eq (cera-pane-kind pane) 'input))
      (when bracket
        (dolist (range (if (<= last begin)
                           (list (list begin end t t))
                         (list (list begin first-end t nil)
                               (list last end nil t))))
          (pcase-let ((`(,start ,stop ,first ,final) range))
            (cera--overlay session start stop 'priority 1002
                           'line-prefix (cera--pane-decoration
                                         pane (cera--pane-endpoint pane first final)
                                         aligned))))))))

(defun cera--pane-lines (text width)
  "Split TEXT into display rows no wider than WIDTH columns."
  (let (rows)
    (dolist (line (split-string text "\n" nil))
      (while (> (string-width line) width)
        (let ((part (truncate-string-to-width line width)))
          (when (string-empty-p part) (setq part (substring line 0 1)))
          (push part rows)
          (setq line (substring line (length part)))))
      (push line rows))
    (nreverse rows)))

(defun cera--virtual-text (pane width)
  "Render PANE's supplied text within WIDTH columns."
  (let* ((bracket (cera-pane-bracket pane))
         (rows (cera--pane-lines
                (cera-pane-text pane)
                (max 1 (- width (cera--indent)
                          (if bracket (cera--pane-width pane) 0) 1))))
         (rows (if (and bracket (= (length rows) 1)
                        (not (cera-pane-connection pane)))
                   (append rows '(""))
                 rows))
         (count (length rows))
         (index 0))
    (propertize
     (mapconcat
      (lambda (row)
        (let* ((endpoint (cera--pane-endpoint pane (zerop index)
                                              (= (1+ index) count)))
               (body (if-let* ((face (cera-pane-face pane)))
                         (propertize (copy-sequence row) 'face face)
                       row)))
          (cl-incf index)
          (concat (and bracket (cera--pane-decoration pane endpoint))
                  body "\n")))
      rows "")
     'line-prefix "" 'wrap-prefix "")))

(defun cera--pane-start (session pane)
  "Return the position of PANE's first line in SESSION, or nil."
  (cond
   ((eq (cera-pane-kind pane) 'input) (cera--session-begin session))
   ((cera-pane-bounds pane)
    (save-excursion (goto-char (car (cera-pane-bounds pane)))
                    (line-beginning-position)))))

(defun cera--display-widths ()
  "Return the text widths of windows displaying this buffer."
  (mapcar (lambda (window)
            (cons window (window-body-width (or window (selected-window)))))
          (or (get-buffer-window-list (current-buffer) nil t) '(nil))))

(defun cera--draw-virtual (session panes anchor)
  "Display SESSION's virtual PANES in order at ANCHOR in each window.
A line carries its `line-prefix' at its start, so an ANCHOR opening one
would draw that line's bracket over the panes.  The overlay is put at the
end of the line above instead, where the panes fill lines of their own."
  (let ((origin (if (and (> anchor (point-min))
                         (save-excursion (goto-char anchor) (bolp)))
                    (1- anchor)
                  anchor)))
    (dolist (geometry (cera--session-width session))
      (cera--overlay session origin origin 'window (car geometry)
                     'before-string
                     (concat (unless (save-excursion (goto-char origin) (bolp)) "\n")
                             (mapconcat (lambda (pane)
                                          (cera--virtual-text pane (cdr geometry)))
                                        panes ""))))))

(defun cera--draw-static (session)
  "Refresh SESSION's read-only panes, leaving its input overlays intact."
  (mapc #'delete-overlay (cera--session-static-overlays session))
  (setf (cera--session-static-overlays session) nil
        (cera--session-width session) (cera--display-widths))
  (let ((cera--drawing-static t)
        (tail (cera--session-tail session)) pending)
    (dolist (pane (cl-remove-if-not #'cera--pane-visible-p
                                    (cera--session-panes session)))
      (if-let* ((start (cera--pane-start session pane)))
          (progn
            (when (eq (cera-pane-kind pane) 'input)
              (setq tail (cera--session-tail session)))
            (when pending
              (cera--draw-virtual session (nreverse pending) start)
              (setq pending nil))
            (when-let* ((bounds (cera-pane-bounds pane)))
              (save-excursion
                (goto-char (max (car bounds) (1- (cdr bounds))))
                (let ((end (min (point-max) (1+ (line-end-position)))))
                  (setq tail end)
                  (when (cera-pane-bracket pane)
                    (goto-char start)
                    (while (< (point) end)
                      (let* ((next (min end (1+ (line-end-position))))
                             (aligned (cera--prefix-text
                                       (get-char-property (point) 'line-prefix))))
                        (cera--overlay
                         session (point) next
                         'line-prefix (cera--pane-decoration
                                       pane (cera--pane-endpoint
                                             pane (= (point) start) (= next end))
                                       aligned)
                         'wrap-prefix (cera--pane-decoration
                                       pane 'middle (cera--prefix-text
                                                     (get-char-property (point) 'wrap-prefix))))
                        (goto-char next)))
                    (cera--absorb-indentation session start end)))
                (when-let* ((face (cera-pane-face pane)))
                  (goto-char (car bounds))
                  (while (< (point) (cdr bounds))
                    (let ((end (min (cdr bounds) (line-end-position))))
                      (when (< (point) end)
                        (cera--overlay session (point) end 'face face
                                       'cera-source-mark t))
                      (goto-char (min (cdr bounds) (1+ end)))))))))
        (push pane pending)))
    (when pending
      (cera--draw-virtual session (nreverse pending) tail))))

(defun cera--draw ()
  "Refresh only the writable pane's brackets and block face."
  (when-let* ((session cera--active))
    (let* ((space (when-let* ((spacer (cera--session-spacer session)))
                    (overlay-get spacer 'after-string)))
           (begin (cera--session-begin session))
           (end (cera--session-end session))
           (aligned (cera--session-aligned session)))
      (mapc #'delete-overlay (cera--session-overlays session))
      (setf (cera--session-overlays session) nil)
      (cera--draw-range session (cera--session-input session) begin (1+ end)
                        (concat (cera--line-number-pad) aligned))
      (setf (cera--session-spacer session)
            (cera--overlay session end (1+ end) 'after-string space)))))

(defun cera--resize (&rest _)
  "Reflow virtual panes after a change of display width."
  (when-let* ((session cera--active)
              ((not (equal (cera--display-widths) (cera--session-width session)))))
    (cera--draw-static session)))

(defun cera-update-pane (id text)
  "Replace the supplied read-only pane ID with TEXT in the current buffer.
Return t on success.  Signal `user-error' for closed or missing panes,
input panes, and panes displaying document bounds.  Draft text, point,
undo and completion are not changed.  Empty TEXT hides the pane."
  (unless (and cera--active (not (cera--session-closed cera--active)))
    (user-error "No field is open"))
  (let ((pane (cl-find id (cera--session-panes cera--active)
                       :key #'cera-pane-id :test #'equal)))
    (unless (and pane (eq (cera-pane-kind pane) 'readonly)
                 (not (cera-pane-bounds pane)) (stringp text))
      (user-error "Not a supplied read-only pane: %S" id))
    (setf (cera-pane-text pane) (copy-sequence text))
    (cera--draw-static cera--active)
    t))


;;;; Completion

(defun cera--capf ()
  "Complete at point through `cera-completion-function'."
  (when-let* ((session cera--active)
              (bounds (cera--field-bounds session))
              ((<= (car bounds) (point) (cdr bounds))))
    (funcall cera-completion-function bounds (cera--session-table session))))

(defun cera--reserve-for-completion ()
  "Hold room below the field while completion is in region.
`completion-in-region-mode' is set by every in-buffer completion
frontend, so this asks no frontend anything."
  (cera-reserve-space (if completion-in-region-mode cera-completion-space 0)))

(defun cera-reserve-space (lines)
  "Reserve LINES of display space below the active field.
Something drawn over the field needs somewhere to go: this pushes what
follows the field down by LINES, so a popup lands on empty space rather
than on top of the text.  LINES of zero gives the space back."
  (when-let* ((session cera--active)
              (spacer (cera--session-spacer session))
              ((overlay-buffer spacer)))
    (overlay-put spacer 'after-string
                 (when (> lines 0)
                   (propertize (make-string lines ?\n) 'face 'default)))))


;;;; Confining the edits

(defun cera--guard (begin end)
  "Reject a modification between BEGIN and END outside the field.
A deletion taking a line of the field may run one character past it,
over the newline closing it: a linewise command on the last line deletes
through there, and the newline is put back by `cera--after-change'."
  (when-let* ((session cera--active))
    (let ((field-begin (cera--session-begin session))
          (field-end (cera--session-end session)))
      (unless (and (<= field-begin begin)
                   (or (<= end field-end)
                       (and (= end (1+ field-end))
                            (or (< begin field-end)
                                (and (< field-begin field-end)
                                     (eq (char-before field-end) ?\n))))))
        (user-error "Only the field is editable")))))

(defun cera--restore-tail (session deleted)
  "Put back the newline closing SESSION's field if a deletion took it.
A deletion of that newline alone, DELETED characters being one, was
after the empty last line before it: that line goes instead."
  (let ((begin (cera--session-begin session))
        (end (cera--session-end session))
        (tail (cera--session-tail session)))
    (when (= end tail)
      (let ((inhibit-modification-hooks t))
        (save-excursion (goto-char end) (insert "\n"))
        (set-marker end (1- tail))
        (when (and (= deleted 1)
                   (< begin end)
                   (eq (char-before end) ?\n))
          (delete-region (1- end) end))))))

(defun cera--release (&rest _)
  "Cancel the field in this buffer ahead of a command that takes it whole.
Saving, reverting, killing the buffer, changing its major mode and
exiting Emacs all come through here: what was written goes on the kill
ring, the field is rolled back, and the command carries on over the
document as it was.  The field's recursive edit ends once the command
returns."
  (when-let* ((session cera--active))
    (let ((written (buffer-substring-no-properties
                    (cera--session-begin session)
                    (cera--session-end session))))
      (unless (string-empty-p written) (kill-new written)))
    (push (cera--session-depth session) cera--released-depths)
    (add-hook 'post-command-hook #'cera--exit-released)
    (cera--close session))
  t)

(defun cera--release-all ()
  "Cancel every open field, ahead of Emacs exiting."
  (dolist (buffer (copy-sequence cera--open-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (cera--release))))
  t)

(defun cera--exit-released ()
  "End the recursive edit of a released field once its command has returned."
  (let ((depth (recursion-depth)))
    (when (memq depth cera--released-depths)
      (setq cera--released-depths
            (cl-remove depth cera--released-depths :count 1))
      (unless cera--released-depths
        (remove-hook 'post-command-hook #'cera--exit-released))
      (throw 'exit nil))))

(defun cera--after-change (_begin _end deleted)
  "Refresh the field's decoration after a change replacing DELETED characters.
The buffer is shown as modified only if it was before the field opened:
the field's text is not a change to the document."
  (when-let* ((session cera--active))
    (cera--restore-tail session deleted)
    (set-buffer-modified-p (cera--session-modified session)))
  (cera--draw))

(defun cera--pre-command ()
  "Keep normal editing commands within the field."
  (when-let* ((session cera--active))
    ;; Emacs can remove a change hook after it signals an error.
    (add-hook 'before-change-functions #'cera--guard -90 t)
    (goto-char (max (cera--session-begin session)
                    (min (point) (cera--session-end session))))
    (setq-local cera--trailing (- (cera--session-end session) (point)))))

(defun cera--post-command ()
  "Take the point back into the field a command of the buffer's moved it out.
A buffer that puts the point where it wants it after every command - a
dashboard, a list - would otherwise hold it there while the field is
written in.  It comes back as far from the end of the input as it was
before the command, so a character just typed is still behind it."
  (when-let* ((session cera--active)
              (begin (marker-position (cera--session-begin session)))
              (end (marker-position (cera--session-end session)))
              ((not (<= begin (point) end))))
    (goto-char (max begin (- end (or cera--trailing 0))))))

(defun cera--insert (session initial)
  "Insert SESSION's INITIAL text below the last source line."
  (when cera--source (goto-char (car cera--source)))
  (setf (cera--session-origin session)
        (copy-marker (line-beginning-position)))
  (when cera--source
    ;; A selection ending at the next line's beginning excludes that line.
    (goto-char (max (car cera--source) (1- (cdr cera--source)))))
  (end-of-line)
  (if (eq (char-after) ?\n) (forward-char) (insert "\n"))
  (setf (cera--session-begin session) (copy-marker (point)))
  (insert (or initial ""))
  (setf (cera--session-end session) (copy-marker (point)))
  (insert "\n")
  (setf (cera--session-tail session) (copy-marker (point) t))
  ;; The field's final newline belongs to the temporary UI, not the field.
  (set-marker-insertion-type (cera--session-end session) t)
  (set-text-properties (cera--session-begin session) (point) nil)
  (goto-char (cera--session-end session)))

(defun cera--enable-input-map ()
  "Borrow text editing keys in buffers whose own map suppresses insertion."
  (when (derived-mode-p 'special-mode)
    (unless cera--source-map
      (setq-local cera--source-map (list (current-local-map))))
    ;; Special modes and their Evil auxiliary maps disable text entry.  Keep
    ;; global editing customizations without inheriting source action keys.
    (use-local-map text-mode-map)))


;;;; Borrowed settings

(defconst cera--local-variables
  '(cera--active cera--source cera--source-map
                 cera-mode cera--emulation-map-alist
                 cera--trailing
                 buffer-undo-list buffer-read-only buffer-auto-save-file-name
                 auto-save-visited-mode cursor-type cursor-in-non-selected-windows
                 before-change-functions after-change-functions
                 first-change-hook create-lockfiles before-save-hook
                 kill-buffer-hook change-major-mode-hook before-revert-hook
                 pre-command-hook post-command-hook window-size-change-functions
                 window-configuration-change-hook
                 truncate-lines truncate-partial-width-windows
                 completion-at-point-functions completion-in-region-function
                 completion-in-region-mode-hook cera-completion-space
                 cera-input-prefix cera-input-prefix-width cera-indent
                 emulation-mode-map-alists minor-mode-overriding-map-alist)
  "Buffer-local settings the field reader borrows.
A consumer arranges for its own through `cera-borrowed-locals'.")

(defconst cera--base-variables
  '(buffer-auto-save-file-name auto-save-visited-mode create-lockfiles
                               before-save-hook kill-buffer-hook
                               change-major-mode-hook before-revert-hook)
  "Settings borrowed in the base buffer of an indirect buffer holding a field.
The two share their text, so what would carry the field out through the
base is held off there as well.")

(defvar-local cera-borrowed-locals nil
  "Further buffer-local settings the reader borrows, beyond its own.
A consumer adds to this, globally or in a buffer, so that whatever it
sets up for the field is put back with the rest.")

(defun cera-register-borrowed-locals (&rest symbols)
  "Add SYMBOLS to the settings borrowed by every Cera session.
Existing consumer and adapter registrations are preserved."
  (dolist (symbol symbols)
    (cl-pushnew symbol (default-value 'cera-borrowed-locals))))

(defun cera--remember-locals (&optional symbols)
  "Return the original bindings of SYMBOLS, or of all the reader borrows."
  (mapcar (lambda (symbol)
            (list symbol (local-variable-p symbol)
                  (and (boundp symbol) (symbol-value symbol))))
          (or symbols (delete-dups
                       (append cera--local-variables
                               (default-value 'cera-borrowed-locals)
                               (copy-sequence cera-borrowed-locals))))))

(defun cera--restore-bindings (bindings)
  "Put back the buffer-local BINDINGS as `cera--remember-locals' found them."
  (dolist (binding bindings)
    (if (nth 1 binding)
        (set (make-local-variable (car binding)) (nth 2 binding))
      (kill-local-variable (car binding)))))

(defun cera--restore-locals (bindings)
  "Restore the buffer-local BINDINGS the reader borrowed."
  (when cera--source-map
    (use-local-map (car cera--source-map))
    (kill-local-variable 'cera--source-map))
  ;; Detach these here as well as restoring the binding, so that an active
  ;; reader can safely pick up new source decoration when this file is reloaded.
  (when cera--source
    (set-marker (car cera--source) nil)
    (set-marker (cdr cera--source) nil)
    (kill-local-variable 'cera--source))
  (cera--restore-bindings bindings))

(defun cera--protect-base (session)
  "Hold the base buffer of an indirect field buffer off carrying SESSION out."
  (when-let* ((base (buffer-base-buffer))
              (field (current-buffer)))
    (with-current-buffer base
      (setf (cera--session-base-bindings session)
            (cera--remember-locals cera--base-variables))
      (setq-local buffer-auto-save-file-name nil
                  auto-save-visited-mode nil
                  create-lockfiles nil)
      (let ((release (lambda (&rest _)
                       (when (buffer-live-p field)
                         (with-current-buffer field (cera--release)))
                       t)))
        (dolist (hook '(before-save-hook kill-buffer-hook
                                         change-major-mode-hook before-revert-hook))
          (add-hook hook release -100 t))))))

(defun cera--restore-base (session)
  "Put the base buffer of SESSION's indirect field buffer back."
  (when-let* ((bindings (cera--session-base-bindings session))
              (base (buffer-base-buffer))
              ((buffer-live-p base)))
    (with-current-buffer base (cera--restore-bindings bindings))))


;;;; Keys

(defun cera-input-text ()
  "Return what is written in the open field, or nil where none is open."
  (when-let* ((session cera--active)
              ((not (cera--session-closed session))))
    (buffer-substring-no-properties (cera--session-begin session)
                                    (cera--session-end session))))

(defun cera-accept ()
  "Keep what was written into the field and close the reader."
  (interactive)
  (unless cera--active (user-error "No field is open"))
  (setf (cera--session-accepted cera--active) t)
  (exit-recursive-edit))

(defun cera-cancel ()
  "Discard what was written into the field and close the reader."
  (interactive)
  (unless cera--active (user-error "No field is open"))
  (setf (cera--session-accepted cera--active) nil)
  (exit-recursive-edit))

(defun cera-recall ()
  "Read an entry of the field's table in the minibuffer and write it here.
What the field already holds is replaced, so a past entry is taken up
whole and edited from there.  Whichever frontend the minibuffer uses
draws the entries."
  (interactive)
  (unless cera--active (user-error "No field is open"))
  (let ((table (cera--session-table cera--active)))
    (unless table (user-error "This field was opened with nothing to recall"))
    (let ((entry (completing-read "Recall: " table nil nil nil t))
          (bounds (cera--field-bounds)))
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert entry))))

(defun cera--without-completion (command)
  "Use COMMAND only when the completion menu is closed."
  (unless completion-in-region-mode command))

(defvar-keymap cera-mode-map
  :doc "Keys active while the field is open."
  "RET" '(menu-item "" cera-accept :filter cera--without-completion)
  "<return>" '(menu-item "" cera-accept :filter cera--without-completion)
  "C-c C-c" #'cera-accept
  "C-c C-k" #'cera-cancel
  "C-g" #'cera-cancel
  "C-SPC" '(menu-item "" completion-at-point
                      :filter cera--without-completion)
  "C-r" #'cera-recall)

(define-minor-mode cera-mode
  "Indicate that a temporary field is being written in this buffer.
Use `cera-read' to open one."
  :lighter " field"
  :group 'cera
  :keymap cera-mode-map)

(defun cera--setup (session)
  "Install SESSION's input guard, completion, and modal key bindings."
  (setq-local cera--active session
              buffer-read-only nil
              buffer-auto-save-file-name nil
              auto-save-visited-mode nil
              cursor-type t
              truncate-lines nil
              truncate-partial-width-windows nil
              cursor-in-non-selected-windows nil
              first-change-hook nil
              before-change-functions '(cera--guard)
              after-change-functions '(cera--after-change)
              completion-at-point-functions '(cera--capf)
              minor-mode-overriding-map-alist
              (copy-tree minor-mode-overriding-map-alist)
              cera--emulation-map-alist
              `((cera-mode . ,(make-composed-keymap
                               (delq nil (list cera-mode-map
                                               (cera--session-keymap session)
                                               cera-session-keymap)))))
              emulation-mode-map-alists
              (cons 'cera--emulation-map-alist emulation-mode-map-alists))
  (add-hook 'window-size-change-functions #'cera--resize nil t)
  (add-hook 'window-configuration-change-hook #'cera--resize nil t)
  (add-hook 'pre-command-hook #'cera--pre-command -90 t)
  (add-hook 'post-command-hook #'cera--post-command 90 t)
  (add-hook 'completion-in-region-mode-hook #'cera--reserve-for-completion nil t)
  (dolist (hook '(before-save-hook kill-buffer-hook
                                   change-major-mode-hook before-revert-hook))
    (add-hook hook #'cera--release -100 t))
  (cera--protect-base session)
  (setf (cera--session-depth session) (1+ (recursion-depth)))
  (unless cera--open-buffers
    (add-hook 'kill-emacs-query-functions #'cera--release-all)
    (add-hook 'kill-emacs-hook #'cera--release-all))
  (cl-pushnew (current-buffer) cera--open-buffers)
  (cera-mode 1)
  (cera--enable-input-map)
  (setf (cera--session-aligned session)
        (cera--prefix-text (get-char-property (cera--session-origin session) 'line-prefix)))
  (cera--draw-static session)
  (cera--draw)
  (run-hook-with-args 'cera-session-start-hook session))

(defun cera--cleanup-hook (hook session)
  "Run every function on HOOK with SESSION, then resignal its first failure."
  (let (failure)
    (run-hook-wrapped
     hook (lambda (function)
            (condition-case err (funcall function session)
              ((error quit) (unless failure (setq failure err))))
            nil))
    (when failure (signal (car failure) (cdr failure)))))

(defun cera--close (session)
  "Roll SESSION's field back and put its buffer as it was.
Closing a session already closed does nothing."
  (unless (cera--session-closed session)
    (setf (cera--session-closed session) t)
    (unwind-protect
        (unwind-protect
            (cera--cleanup-hook 'cera-session-teardown-hook session)
          (when completion-in-region-mode (completion-in-region-mode -1)))
      (unwind-protect
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (mapc #'delete-overlay (cera--session-overlays session))
            (mapc #'delete-overlay (cera--session-static-overlays session))
            (when-let* ((group (cera--session-group session)))
              (cancel-change-group group)))
        (cera--restore-locals (cera--session-bindings session))
        (cera--restore-base session)
        (set-buffer-modified-p (cera--session-modified session))
        (dolist (pane (cera--session-panes session))
          (when-let* ((bounds (cera-pane-bounds pane)))
            (set-marker (car bounds) nil)
            (set-marker (cdr bounds) nil)))
        (dolist (marker (list (cera--session-begin session)
                              (cera--session-end session)
                              (cera--session-origin session)
                              (cera--session-tail session)))
          (when marker (set-marker marker nil)))
        (setq cera--open-buffers (delq (current-buffer) cera--open-buffers))
        (unless cera--open-buffers
          (remove-hook 'kill-emacs-query-functions #'cera--release-all)
          (remove-hook 'kill-emacs-hook #'cera--release-all))
        (cera--cleanup-hook 'cera-session-restored-hook session)))))


;;;; The reader

(cl-defun cera-read (table &optional initial bounds (source-face 'cera-source))
  "Read text into a temporary field below BOUNDS, completing on TABLE.
The field opens holding INITIAL, which defaults to nothing, and BOUNDS is
the source's (BEGIN . END) range, defaulting to the active region.  That
text is marked in SOURCE-FACE, which defaults to `cera-source', and its
first line is connected to the field below its last line.  A nil
SOURCE-FACE marks nothing, leaving a consumer to mark the source its own
way; the connection is drawn either way.  Without a range the field goes
below the current line.  `cera-input-prefix' puts an icon or a label in
front of the input, and `cera-indent' moves the whole field right, into
the indentation of the lines it connects to.

\\<cera-mode-map>\\[cera-accept] keeps what was written and
\\[cera-cancel] discards it.  While completion is active its own key
bindings apply, including RET; the direct accept binding still works.
Other editing keys are inherited from the buffer, using a temporary text
map in special modes.  Completion is offered through the ordinary one,
`completion-at-point', so whichever frontend the buffer uses draws it;
what completes, and where, is `cera-completion-function'.
\\[cera-recall] reads TABLE in the minibuffer instead and writes the
entry chosen into the field.

The temporary text is rolled back before anything is returned.  Point,
narrowing, undo, modification status, hooks and borrowed local settings
are restored on accept, cancel and error.  Cancelling signals `quit',
like a minibuffer.

While the field is open the buffer is not shown as modified, holds no
lock file, and is not auto-saved.  Saving or reverting it, killing it,
changing its major mode, or exiting Emacs cancels the field first, with
what was written put on the kill ring, and then goes ahead over the
document as it was."
  (let* ((range (or bounds (and (use-region-p)
                                (cons (region-beginning) (region-end)))
                    (cons (line-beginning-position) (line-end-position))))
         (source (cera-pane :id 'source :kind 'readonly :bounds range
                            :face (and (or bounds (use-region-p)) source-face)))
         (input (cera-pane :id 'input :kind 'input :text (or initial "")
                           :prefix (if (functionp cera-input-prefix)
                                       (funcall cera-input-prefix)
                                     cera-input-prefix)))
         (panes (list source input)))
    (setf (cera-pane-connection source) 'next
          (cera-pane-connection input) 'previous)
    (cera-read-stack (if cera-read-context-function
                         (funcall cera-read-context-function panes)
                       panes)
                     table)))

(defun cera--validate-panes (panes)
  "Validate PANES before changing any editor state and return owned copies."
  (unless (and (proper-list-p panes) panes)
    (user-error "A stack needs one input pane"))
  (let ((inputs 0) ids (previous (point-min)) input-seen preceding)
    (dolist (pane panes)
      (unless (and (cera-pane-p pane) (cera-pane-id pane)
                   (not (member (cera-pane-id pane) ids))
                   (memq (cera-pane-kind pane) '(input readonly))
                   (memq (cera-pane-bracket pane) '(nil t))
                   (memq (cera-pane-prefix-position pane) '(top bottom))
                   (or (null (cera-pane-prefix pane)) (stringp (cera-pane-prefix pane))))
        (user-error "Invalid or duplicate pane: %S" pane))
      (push (cera-pane-id pane) ids)
      (if (eq (cera-pane-kind pane) 'input)
          (progn
            (cl-incf inputs)
            (unless (and (not (cera-pane-bounds pane))
                         (or (null (cera-pane-text pane)) (stringp (cera-pane-text pane))))
              (user-error "An input pane must contain text, not bounds"))
            (setq input-seen t)
            (unless preceding
              (setq previous (save-excursion (end-of-line)
                                             (min (point-max) (1+ (point)))))))
        (let ((bounds (cera-pane-bounds pane)))
          (if bounds
              (progn
                (unless (and (null (cera-pane-text pane)) (consp bounds)
                             (cl-every (lambda (pos)
                                         (and (integer-or-marker-p pos)
                                              (or (integerp pos)
                                                  (eq (marker-buffer pos) (current-buffer)))))
                                       (list (car bounds) (cdr bounds)))
                             (<= (point-min) (car bounds) (cdr bounds) (point-max)))
                  (user-error "Invalid pane bounds: %S" bounds))
                (let ((start (save-excursion (goto-char (car bounds))
                                             (line-beginning-position))))
                  (when (< start previous)
                    (user-error "Bounded panes must follow document line order")))
                (setq previous (save-excursion
                                 (goto-char (max (car bounds) (1- (cdr bounds))))
                                 (min (point-max) (1+ (line-end-position))))
                      preceding (not input-seen)))
            (unless (stringp (cera-pane-text pane))
              (user-error "A read-only pane needs text or bounds"))))))
    (unless (= inputs 1) (user-error "A stack needs exactly one input pane")))
  (mapcar (lambda (pane)
            (let ((copy (copy-cera-pane pane)))
              (when-let* ((text (cera-pane-text pane)))
                (setf (cera-pane-text copy) (copy-sequence text)))
              copy))
          panes))

(defun cera-read-stack (panes table &optional keymap)
  "Read the single input of ordered PANES, completing on TABLE.
PANES are `cera-pane' descriptors.  Existing bounded panes stay in place
and must occur in document line order without overlapping lines.
The input opens after the preceding bounded pane, or the current line.
Supplied read-only panes are overlay text, displayed in list order.
KEYMAP and `cera-session-keymap' supplement the ordinary input bindings.
Return the input string; cancellation signals `quit'."
  (when cera--active (user-error "A field is already open"))
  (setq panes (cera--validate-panes panes))
  (unless (and (or (null keymap) (keymapp keymap))
               (or (null cera-session-keymap) (keymapp cera-session-keymap)))
    (user-error "Session bindings must be keymaps"))
  (when completion-in-region-mode (completion-in-region-mode -1))
  (let* ((buffer (current-buffer))
         (input (cl-find 'input panes :key #'cera-pane-kind))
         (session (cera--make-session :buffer buffer :table table
                                      :panes panes :input input :keymap keymap
                                      :bindings (cera--remember-locals)
                                      :modified (buffer-modified-p)))
         (undo-limit most-positive-fixnum)
         (undo-strong-limit most-positive-fixnum)
         (undo-outer-limit nil)
         (amalgamating-undo-limit 0)
         result)
    (save-window-excursion
      (save-mark-and-excursion
        (save-restriction
          (unwind-protect
              (progn
                (setq-local buffer-undo-list nil create-lockfiles nil)
                (let (after-input)
                  (dolist (pane panes)
                    (when (eq pane input) (setq after-input t))
                    (when-let* ((range (cera-pane-bounds pane)))
                      (setf (cera-pane-bounds pane)
                            (cons (copy-marker (car range) after-input)
                                  (copy-marker (cdr range) after-input))))))
                (let* ((before (cl-subseq panes 0 (cl-position input panes)))
                       (source (cl-find-if #'cera-pane-bounds (reverse before))))
                  (setq-local cera--source
                              (when source
                                (let ((bounds (cera-pane-bounds source)))
                                  (cons (copy-marker (car bounds))
                                        (copy-marker (cdr bounds)))))))
                (setf (cera--session-group session) (prepare-change-group))
                (activate-change-group (cera--session-group session))
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t))
                  (cera--insert session (cera-pane-text input)))
                (set-buffer-modified-p (cera--session-modified session))
                (undo-boundary)
                (deactivate-mark)
                (cera--setup session)
                (recursive-edit)
                (when (and (cera--session-accepted session)
                           (not (cera--session-closed session)))
                  (with-current-buffer buffer
                    (setq result
                          (buffer-substring-no-properties
                           (cera--session-begin session)
                           (cera--session-end session))))))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (cera--close session)))))))
    (or result (signal 'quit nil))))

(provide 'cera)
;;; cera.el ends here
