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
integration file does.  A function of no arguments is called for the
number instead, so a frontend that sometimes draws above the field can
ask for nothing when it is about to."
  :type '(choice natnum function))

(defcustom cera-space-below 0
  "Pixels left empty below the input, between it and the text that follows.
Room held for completion comes below them."
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
  panes input static-overlays keymap width aligned hl-line)

(cl-defstruct (cera-pane (:constructor cera-pane))
  "A pane with ID, KIND, TEXT or BOUNDS, BRACKET, PREFIX and FACE.
KIND is `readonly' or `input'.  BRACKET draws the pane's left bracket,
and a pane with BRACKET nil shows its text alone.  PREFIX-POSITION is
`top' or `bottom'.  BOUNDS refer to existing text in this buffer;
supplied read-only TEXT is displayed without insertion.
An empty supplied read-only TEXT hides the pane.  WRAP carries a line
too wide for the window onto the next row; a pane with WRAP nil cuts it
short instead, and keeps a line of text to a line of the pane.
ALIGN `input' starts a pane without BRACKET in the column the input's
text does, past its bracket and prefix.  INDENT sets every row of the
pane's text in by that many columns, the rows WRAP carries on included.
MAX-WIDTH caps a row, its INDENT included, at that many columns however
wide the window is; nil leaves the window to decide."
  id kind text bounds (bracket t) prefix (prefix-position 'bottom) face
  connection (wrap t) align (indent 0) max-width)

(defun cera-set-pane-text (pane text)
  "Set PANE's supplied TEXT, and return PANE.
A consumer holds the struct but not its setters, which are known where
the struct is, so the text it wants a pane opened with is set here."
  (setf (cera-pane-text pane) text)
  pane)

(cl-defstruct (cera-shown (:constructor cera--make-shown) (:copier nil))
  "Read-only PANES shown in BUFFER by the line ANCHOR is on.
They are drawn under that line, or beside it from COLUMN on.  OVERLAYS
draw them, one per window, laid out for the WIDTHS those windows had
when they were drawn."
  buffer anchor panes overlays widths column)

(defvar cera-read-context-function nil
  "Optional function transforming the normalized panes of `cera-read'.
Called with PANES and returning PANES, before any document changes.")

(defvar cera-session-keymap nil
  "Optional keymap appended to the active session's input maps.")

(defvar cera-session-start-hook nil
  "Functions called with SESSION once its input is ready in the current buffer.")

(defvar cera-update-pane-functions nil
  "Functions called with ID and TEXT for a pane the open session does not hold.
The first to answer non-nil has taken the update, and the pane is
drawn by whoever holds it.")

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

(defvar-local cera--field-buffer nil
  "Buffer the open field's input is written in, where that is another buffer.")

(defvar-local cera--origin-buffer nil
  "Buffer the field written here was opened for, where that is another buffer.")

(defun cera-origin-buffer ()
  "Return the buffer the open field was opened for.
A backend that writes the input somewhere else, as the child frame does,
puts the field's hooks and commands in that other buffer; a consumer
holding on to the buffer it called `cera-read' in compares with this
rather than with the current buffer."
  (or cera--origin-buffer (current-buffer)))

(defmacro cera--in-field (&rest body)
  "Run BODY in the buffer the open field's input is written in."
  (declare (indent 0) (debug t))
  `(with-current-buffer (or cera--field-buffer (current-buffer)) ,@body))

;;;###autoload
(defun cera-field-open-p (&optional buffer)
  "Return non-nil while a field is open over BUFFER, the current one by default.
A field brackets the lines it is read beside with overlays and holds
markers into them, so a buffer that rewrites its own text — a dashboard
redrawing itself, a mode re-rendering its report — asks this first and
puts that work off while the answer stands.  It is non-nil in the buffer
the field was read for, whether the input is written there or in a child
frame laid over it."
  (and (buffer-local-value 'cera--active (or buffer (current-buffer))) t))

(defun cera--field-bounds (&optional session)
  "Return SESSION's field as a cons of its BEGIN and END positions.
SESSION defaults to the reader active in this buffer."
  (let ((session (or session cera--active)))
    (cons (marker-position (cera--session-begin session))
          (marker-position (cera--session-end session)))))


;;;; Drawing

(defun cera--overlay (owner begin end &rest properties)
  "Decorate BEGIN through END with PROPERTIES owned by OWNER.
OWNER is a field's session or panes shown outside one."
  (let ((overlay (make-overlay begin end nil nil t)))
    (overlay-put overlay 'priority 1001)
    (overlay-put overlay 'cera t)
    (while properties
      (overlay-put overlay (pop properties) (pop properties)))
    (cond
     ((cera-shown-p owner) (push overlay (cera-shown-overlays owner)))
     (cera--drawing-static (push overlay (cera--session-static-overlays owner)))
     (t (push overlay (cera--session-overlays owner))))
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

(defun cera--text-column ()
  "Return the column the input's text begins at, with or without a prefix."
  (if (cera--input-prefix)
      (cera--input-column)
    (+ (cera--indent) cera-bracket-width)))

(defun cera--input-lead (&optional aligned)
  "Return blank space reaching the column the input begins at, following ALIGNED."
  (let ((cera--aligned (or aligned "")))
    (cera--indented
     (concat aligned (cera--stretched-to (cera--text-column)
                                         (- (cera--text-column) (cera--indent)))))))

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
            (cera--pane-empty-p pane))))

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

(defvar cera-input-fontifier)

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
                      (not cera-input-fontifier)
                      (or (cera-pane-face pane) (cera--body-face)))))
      (cera--overlay session begin end 'face face
                     'line-prefix continued 'wrap-prefix continued
                     'display-line-numbers-disable
                     (eq (cera-pane-kind pane) 'input))
      (when (and cera-input-fontifier (eq (cera-pane-kind pane) 'input))
        (cera--colour-input session begin end))
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

(defun cera--block-p (block)
  "Return non-nil when BLOCK is a run of lines a pane may stack.
A block is its text consed onto the pixels of blank space kept beneath
it, which is plain data a consumer builds without loading cera."
  (and (consp block) (stringp (car block)) (natnump (cdr block))))

(defun cera--pane-blocks (pane)
  "Return PANE's text as the blocks it is made of.
A pane given a string is one block of it, so a consumer with nothing to
stack says nothing about stacking."
  (let ((text (cera-pane-text pane)))
    (if (stringp text) (list (cons text 0)) text)))

(defun cera--pane-content-p (text)
  "Return non-nil when TEXT is what a supplied read-only pane may show."
  (or (stringp text)
      (and (consp text) (cl-every #'cera--block-p text))))

(defun cera--copy-content (text)
  "Return a copy of TEXT that a consumer cannot change under the pane."
  (if (stringp text)
      (copy-sequence text)
    (mapcar (lambda (block) (cons (copy-sequence (car block)) (cdr block)))
            text)))

(defun cera--pane-empty-p (pane)
  "Return non-nil when PANE was supplied nothing to show."
  (let ((text (cera-pane-text pane)))
    (if (stringp text)
        (string-empty-p text)
      (and (listp text)
           (cl-every (lambda (block) (string-empty-p (car block))) text)))))

(defun cera--pane-rows (blocks width &optional wrap)
  "Split BLOCKS into display rows no wider than WIDTH columns.
The newline ending a block asks for the block\='s gap as room beneath it,
which is where the display takes the space between blocks from.  A block
with no text stands as a blank line, and one with no gap either is
dropped: the display gives a line its full height or none at all, so
there is no blank line thinner than the rest.  WRAP is carried through
to `cera--text-rows\='."
  (let (rows)
    (dolist (block blocks)
      (let* ((text (car block))
             (gap (or (cdr block) 0))
             (lines (cond ((not (string-empty-p text))
                           (cera--text-rows text width wrap))
                          ((> gap 0) (list (cons "" "\n"))))))
        (when (and lines (> gap 0))
          (setcdr (car (last lines))
                  (apply #'propertize "\n" 'line-spacing gap
                         (text-properties-at 0 (cdr (car (last lines)))))))
        (setq rows (append rows lines))))
    rows))

(defun cera--text-rows (text width &optional wrap)
  "Split TEXT into display rows no wider than WIDTH columns.
A row comes with the newline that ended it, which is where text asks for
the room around its line; a row the width broke off ends in a plain one.
The width breaks a line at the last space that fits, and a word wider
than a row on its own is broken where the row ends.
Without WRAP a line too wide is cut short rather than carried on."
  (let ((start 0) (length (length text)) rows)
    (while (<= start length)
      (let* ((break (or (string-search "\n" text start) length))
             (line (substring text start break))
             (newline (if (< break length) (substring text break (1+ break)) "\n")))
        (if (not wrap)
            (setq line (truncate-string-to-width line width nil nil t))
          (while (> (string-width line) width)
            (let* ((part (truncate-string-to-width line width))
                   (part (if (string-empty-p part) (substring line 0 1) part))
                   (space (if (eq (aref line (length part)) ?\s)
                              (length part)
                            (cl-position ?\s part :from-end t))))
              (if (and space (> space 0))
                  (progn (push (cons (substring line 0 space) "\n") rows)
                         (setq line (substring line (1+ space))))
                (push (cons part "\n") rows)
                (setq line (substring line (length part)))))))
        (push (cons line newline) rows)
        (setq start (1+ break))))
    (nreverse rows)))

(defun cera--virtual-text (pane width &optional aligned)
  "Render PANE's supplied text within WIDTH columns, following ALIGNED text.
ALIGNED is the prefix the lines beside the field are drawn behind, and
every row the pane shows starts behind it as they do."
  (let* ((bracket (cera-pane-bracket pane))
         (lead (and (not bracket) (eq (cera-pane-align pane) 'input)
                    (cera--input-lead aligned)))
         (indent (make-string (cera-pane-indent pane) ?\s))
         (rows (cera--pane-rows
                (cera--pane-blocks pane)
                (cera--capped pane
                              (max 1 (- width (cera--indent) (length indent)
                                        (cond (bracket (cera--pane-width pane))
                                              (lead (- (cera--text-column)
                                                       (cera--indent)))
                                              (t 0))
                                        1)))
                (cera-pane-wrap pane)))
         (rows (if (and bracket (= (length rows) 1)
                        (not (cera-pane-connection pane)))
                   (append rows (list (cons "" "\n")))
                 rows))
         (count (length rows))
         (index 0))
    (propertize
     (mapconcat
      (lambda (row)
        (let* ((endpoint (cera--pane-endpoint pane (zerop index)
                                              (= (1+ index) count)))
               (body (if-let* ((face (cera-pane-face pane)))
                         (propertize (copy-sequence (car row)) 'face face)
                       (car row))))
          (cl-incf index)
          (concat (cond
                   (bracket (cera--pane-decoration pane endpoint aligned))
                   ((string-empty-p (car row)) nil)
                   (lead)
                   (aligned))
                  (unless (string-empty-p (car row)) indent)
                  body (cdr row))))
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

(defun cera--pane-face ()
  "Return the face a shown pane's text wears under its own.
A display string is drawn over the face of the text it is shown at, and
naming the default face overrides nothing there, so its colours are
spelled out, and carried to the window's edge past a row's end."
  (list :foreground (face-foreground 'default nil t)
        :background (face-background 'default nil t)
        :extend t))

(defun cera--draw-virtual (owner panes anchor widths aligned &optional opening)
  "Display OWNER's virtual PANES in order at ANCHOR in each window.
WIDTHS pairs each window with the columns it shows, and ALIGNED is the
prefix the lines beside the panes are drawn behind.
A line carries its `line-prefix' at its start, so an ANCHOR opening one
would draw that line's bracket over the panes.  The overlay is put at the
end of the line above instead, where the panes fill lines of their own.
The first line of a buffer has no line above to put them on, so OPENING
carries its bracket, drawn after the panes and before the line's text."
  (let ((origin (if (and (> anchor (point-min))
                         (save-excursion (goto-char anchor) (bolp)))
                    (1- anchor)
                  anchor)))
    (dolist (geometry widths)
      (let* ((shown (cera-shown-p owner))
             (body (concat (mapconcat (lambda (pane)
                                        (cera--virtual-text
                                         pane (cdr geometry) aligned))
                                      panes "")
                           opening))
             (text (concat (unless (save-excursion (goto-char origin) (bolp))
                             (if shown
                                 (propertize "\n" 'face (get-text-property origin 'face))
                               "\n"))
                           (if shown (cera--own-face body) body)))
             (closing (cera--closing-newline text origin))
             (overlay (cera--overlay owner origin (if closing (1+ origin) origin)
                                     'window (car geometry)
                                     'before-string
                                     (if closing
                                         (substring text 0 (1- (length text)))
                                       text))))
        (when closing
          (overlay-put overlay 'line-spacing closing)
          (overlay-put overlay 'evaporate nil)
          (when shown
            (overlay-put overlay 'face (cera--pane-face))))))))

(defun cera--own-face (text)
  "Return TEXT drawn in the default face wherever it wears none of its own.
Panes shown outside a field stand on lines of their own, but a character
of a display string with no face takes the face of the text it is shown
at, so the line they hang from would lend them its background."
  (let ((copy (copy-sequence text)))
    (add-face-text-property 0 (length copy) (cera--pane-face) t copy)
    copy))

(defun cera--closing-newline (text origin)
  "Return the room TEXT asks for under its last row, for ORIGIN to hold.
The panes end in a newline of their own, which leaves a blank line
standing between them and the text below.  Dropping it hands that row to
the buffer\='s own newline at ORIGIN, which then carries the room the row
asked for."
  (and (string-suffix-p "\n" text)
       (eq (char-after origin) ?\n)
       (or (get-text-property (1- (length text)) 'line-spacing text) 0)))

(defun cera--heads-the-buffer-p (panes start)
  "Return non-nil when PANES are drawn over the line at START.
Virtual panes hang off the end of the line above the one they precede,
which the first line of a buffer has not got."
  (and panes (= start (point-min))))

(defun cera--draw-static (session)
  "Refresh SESSION's read-only panes, leaving its input overlays intact."
  (mapc #'delete-overlay (cera--session-static-overlays session))
  (setf (cera--session-static-overlays session) nil
        (cera--session-width session) (cera--display-widths))
  (let ((cera--drawing-static t)
        (tail (cera--session-tail session)) pending)
    (dolist (pane (cl-remove-if-not #'cera--pane-visible-p
                                    (cera--session-panes session)))
      (if-let* ((start (or (cera--pane-start session pane)
                           (and (eq (cera-pane-kind pane) 'input)
                                (cera--session-tail session)))))
          (let ((opening nil))
            (when (eq (cera-pane-kind pane) 'input)
              (setq tail (cera--session-tail session)))
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
                                       (get-char-property (point) 'line-prefix)))
                             (decoration (cera--pane-decoration
                                          pane (cera--pane-endpoint
                                                pane (= (point) start) (= next end))
                                          aligned))
                             (heads (and (= (point) start)
                                         (cera--heads-the-buffer-p pending start))))
                        (when heads (setq opening decoration))
                        (let ((wrap (cera--pane-decoration
                                     pane 'middle (cera--prefix-text
                                                   (get-char-property (point) 'wrap-prefix)))))
                          (cera--overlay session (point) (1+ (point))
                                         'line-prefix (if heads "" decoration)
                                         'wrap-prefix wrap)
                          (when (< (1+ (point)) next)
                            (cera--overlay session (1+ (point)) next
                                           'line-prefix (cera--pane-decoration
                                                         pane 'middle aligned)
                                           'wrap-prefix wrap)))
                        (goto-char next)))
                    (cera--absorb-indentation session start end)))
                (when-let* ((face (cera-pane-face pane)))
                  (goto-char (car bounds))
                  (while (< (point) (cdr bounds))
                    (let ((end (min (cdr bounds) (line-end-position))))
                      (when (< (point) end)
                        (cera--overlay session (point) end 'face face
                                       'cera-source-mark t))
                      (goto-char (min (cdr bounds) (1+ end))))))))
            (when pending
              (cera--draw-virtual session (nreverse pending) start
                                  (cera--session-width session)
                                  (cera--session-aligned session) opening)
              (setq pending nil)))
        (push pane pending)))
    (when pending
      (cera--draw-virtual session (nreverse pending) tail
                          (cera--session-width session)
                          (cera--session-aligned session)))))

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
            (cera--overlay session end (1+ end) 'after-string space
                           'line-spacing (and (> cera-space-below 0)
                                              cera-space-below))))))

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
    (cond
     ((and (null pane) (cera--pane-content-p text)
           (run-hook-with-args-until-success 'cera-update-pane-functions id text))
      t)
     ((not (and pane (eq (cera-pane-kind pane) 'readonly)
                (not (cera-pane-bounds pane))
                (cera--pane-content-p text)))
      (user-error "Not a supplied read-only pane: %S" id))
     (t
      (setf (cera-pane-text pane) (cera--copy-content text))
      (cera--draw-static cera--active)
      t))))


;;;; Panes shown outside a field

(defvar-local cera--shown nil
  "Panes shown in this buffer outside a field.")

(defun cera-pane-show (panes position &optional column)
  "Show read-only PANES by the line POSITION is on, and return them.
PANES are `cera-pane' descriptors with supplied text, in list order.
Without COLUMN they are drawn under the line, the way a field draws its
read-only panes.  With COLUMN they are drawn beside it: the first row
from COLUMN on the line itself, or one space past its end when the line
reaches further, and every row after it from COLUMN on a line of its
own.  Each pane's INDENT and WRAP apply either way.

They stay without a field open until `cera-pane-remove', move with the
line as the text around it is edited, and are laid out again when a
window showing them changes width.  `cera-pane-update' replaces what
they show."
  (let ((shown (cera--make-shown :buffer (current-buffer)
                                 :anchor (copy-marker position)
                                 :panes panes
                                 :column column)))
    (unless cera--shown
      (add-hook 'window-size-change-functions #'cera--reflow-shown nil t)
      (add-hook 'window-configuration-change-hook #'cera--reflow-shown nil t))
    (push shown cera--shown)
    (cera--draw-shown shown)
    shown))

(defun cera-pane-update (shown panes)
  "Show PANES in place of what SHOWN shows, and return SHOWN."
  (setf (cera-shown-panes shown) panes)
  (cera--draw-shown shown)
  shown)

(defun cera-pane-remove (shown)
  "Take the panes of SHOWN off the display."
  (mapc #'delete-overlay (cera-shown-overlays shown))
  (setf (cera-shown-overlays shown) nil)
  (set-marker (cera-shown-anchor shown) nil)
  (when (buffer-live-p (cera-shown-buffer shown))
    (with-current-buffer (cera-shown-buffer shown)
      (setq cera--shown (delq shown cera--shown))
      (unless cera--shown
        (remove-hook 'window-size-change-functions #'cera--reflow-shown t)
        (remove-hook 'window-configuration-change-hook #'cera--reflow-shown t)))))

(defun cera--draw-shown (shown)
  "Draw the panes of SHOWN by the line its anchor is on."
  (with-current-buffer (cera-shown-buffer shown)
    (mapc #'delete-overlay (cera-shown-overlays shown))
    (setf (cera-shown-overlays shown) nil
          (cera-shown-widths shown) (cera--display-widths))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (cera-shown-anchor shown))
        (let ((panes (cl-remove-if-not #'cera--pane-visible-p
                                       (cera-shown-panes shown))))
          (if (cera-shown-column shown)
              (cera--draw-beside shown panes)
            (cera--draw-below shown panes)))))))

(defun cera--draw-below (shown panes)
  "Draw PANES of SHOWN on lines of their own under the line point is on.
The rows open the next line rather than hang off the end of this one:
every row of a display string stands for the position it is shown at,
so rows hung off a line's end would hold a cursor moving down there.
The last line of a buffer has no next line, and carries them after
itself."
  (let* ((next (min (point-max) (1+ (line-end-position))))
         (ending (not (save-excursion (goto-char next) (bolp))))
         (aligned (cera--prefix-text (get-char-property (line-beginning-position)
                                                        'line-prefix))))
    (dolist (geometry (cera-shown-widths shown))
      (let ((rows (cera--own-face
                   (mapconcat (lambda (pane)
                                (cera--virtual-text pane (cdr geometry) aligned))
                              panes "")))
            (overlay (make-overlay next next nil nil nil)))
        (overlay-put overlay 'cera t)
        (overlay-put overlay 'priority 1001)
        (overlay-put overlay 'window (car geometry))
        (if ending
            (overlay-put overlay 'after-string
                         (concat "\n" (string-remove-suffix "\n" rows)))
          (overlay-put overlay 'before-string rows))
        (push overlay (cera-shown-overlays shown))))))

(defun cera--capped (pane room)
  "Return ROOM, the columns PANE's text has, held to its MAX-WIDTH.
The cap counts the pane's indentation, so the text has what is left."
  (if-let* ((max-width (cera-pane-max-width pane)))
      (max 1 (min room (- max-width (cera-pane-indent pane))))
    room))

(defun cera--beside-text (panes width column start)
  "Return PANES as rows set out from COLUMN beside a line ending at START.
WIDTH is the columns the window shows.  The run up to the first row
carries no face, so whatever the line wears shows through it; the runs
opening the rows below are faced, since display-only lines have nothing
behind them to show.  A window too narrow to leave a row room past
COLUMN gets the rows unwrapped."
  (let* ((room (- width column 1))
         (rows (mapcan
                (lambda (pane)
                  (let ((indent (make-string (cera-pane-indent pane) ?\s))
                        (face (cera-pane-face pane)))
                    (mapcar (lambda (row)
                              (concat indent
                                      (if face
                                          (propertize (copy-sequence (car row))
                                                      'face face)
                                        (car row))))
                            (cera--pane-rows
                             (cera--pane-blocks pane)
                             (cera--capped pane
                                           (if (> room 8)
                                               (max 1 (- room (length indent)))
                                             most-positive-fixnum))
                             (cera-pane-wrap pane)))))
                panes)))
    (when rows
      (concat (make-string (max 1 (- column start)) ?\s)
              (cera--own-face
               (mapconcat #'identity rows
                          (concat "\n" (make-string column ?\s))))))))

(defun cera--draw-beside (shown panes)
  "Draw PANES of SHOWN beside the line point is on, from its column.
The overlay covers the line's newline and carries the rows in front of
it, starting after the line's text, so what is typed at the end of the
line stays in front of them.  The last line of a buffer has no newline
to cover, and carries them after itself instead."
  (let* ((eol (line-end-position))
         (start (progn (goto-char eol) (current-column)))
         (ending (= eol (point-max))))
    (dolist (geometry (cera-shown-widths shown))
      (when-let* ((text (cera--beside-text panes (cdr geometry)
                                           (cera-shown-column shown) start)))
        (let ((overlay (make-overlay eol (if ending eol (1+ eol)) nil t nil)))
          (unless ending
            (overlay-put overlay 'face (cera--pane-face)))
          (overlay-put overlay 'cera t)
          (overlay-put overlay 'priority 1001)
          (overlay-put overlay 'window (car geometry))
          (overlay-put overlay (if ending 'after-string 'before-string) text)
          (push overlay (cera-shown-overlays shown)))))))

(defun cera--reflow-shown (&optional window)
  "Lay the shown panes of WINDOW's buffer out again for its new width."
  (with-current-buffer (if (windowp window) (window-buffer window) (current-buffer))
    (let ((widths (cera--display-widths)))
      (dolist (shown cera--shown)
        (unless (equal widths (cera-shown-widths shown))
          (cera--draw-shown shown))))))


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
  (cera-reserve-space (if completion-in-region-mode (cera--completion-space) 0)))

(defun cera--completion-space ()
  "Return the lines `cera-completion-space' asks to be kept free."
  (let ((lines (if (functionp cera-completion-space)
                   (funcall cera-completion-space)
                 cera-completion-space)))
    (if (natnump lines) lines 0)))

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

(defun cera--input-suppressed-p ()
  "Return non-nil when the buffer's own map turns typing into nothing.
Suppression is a remap of `self-insert-command', which a mode inherits
through its map without descending from `special-mode': a compilation
mode carries it from a parent map alone."
  (when-let* ((map (current-local-map)))
    (memq (lookup-key map [remap self-insert-command]) '(undefined ignore))))

(defun cera--enable-input-map ()
  "Borrow text editing keys in buffers whose own map suppresses insertion."
  (when (or (derived-mode-p 'special-mode) (cera--input-suppressed-p))
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
                 cera-space-below emulation-mode-map-alists minor-mode-overriding-map-alist
                 font-lock-fontify-region-function global-hl-line-mode
                 global-hl-line-buffers)
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
  (cera--in-field
    (when-let* ((session cera--active)
                ((not (cera--session-closed session))))
      (buffer-substring-no-properties (cera--session-begin session)
                                      (cera--session-end session)))))

(defun cera-input-bounds ()
  "Return the open field's input as a cons of its positions, or nil.
The positions are in the buffer the input is written in, which
`cera-origin-buffer' tells from the one the field was opened for."
  (cera--in-field
    (when-let* ((session cera--active)
                ((not (cera--session-closed session))))
      (cera--field-bounds session))))

(defun cera-set-input (text)
  "Replace what is written in the open field with TEXT.
The point lands at the end of it, as it does after writing it."
  (cera--in-field
    (unless cera--active (user-error "No field is open"))
    (let ((bounds (cera--field-bounds)))
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert (or text "")))))

(defun cera-accept ()
  "Keep what was written into the field and close the reader."
  (interactive)
  (cera--in-field
    (unless cera--active (user-error "No field is open"))
    (setf (cera--session-accepted cera--active) t)
    (exit-recursive-edit)))

(defun cera-cancel ()
  "Discard what was written into the field and close the reader."
  (interactive)
  (cera--in-field
    (unless cera--active (user-error "No field is open"))
    (setf (cera--session-accepted cera--active) nil)
    (exit-recursive-edit)))

(defun cera-recall ()
  "Read an entry of the field's table in the minibuffer and write it here.
What the field already holds is replaced, so a past entry is taken up
whole and edited from there.  Whichever frontend the minibuffer uses
draws the entries."
  (interactive)
  (unless cera--active (user-error "No field is open"))
  (let ((table (cera--session-table cera--active)))
    (unless table (user-error "This field was opened with nothing to recall"))
    (cera-set-input (completing-read "Recall: " table nil nil nil t))))

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

(defcustom cera-input-fontifier nil
  "How the text written into a field is coloured, or nil to leave it plain.
The function is called with the first and last position of the field and
may put `face' properties on that text.  The buffer\='s own colouring is
held off there either way, so what it holds is never read as code."
  :type '(choice (const :tag "Plain text" nil) function)
  :group 'cera)

(defvar cera--markdown-buffer nil
  "The hidden buffer a field\='s text is coloured as Markdown in.")

(declare-function markdown-mode "ext:markdown-mode" ())
(defvar markdown-hide-markup)

(defun cera-fontify-input-as-markdown (begin end)
  "Colour the field between BEGIN and END as Markdown.
The text is coloured in a buffer of its own, and only the faces it comes
back with are put on the field, so the buffer holding it is untouched."
  (when (require 'markdown-mode nil t)
    (let ((text (buffer-substring-no-properties begin end))
          (target (current-buffer)))
      (unless (buffer-live-p cera--markdown-buffer)
        (setq cera--markdown-buffer (get-buffer-create " *cera-markdown*" t))
        (with-current-buffer cera--markdown-buffer
          (delay-mode-hooks (markdown-mode))
          (setq-local markdown-hide-markup nil)))
      (with-current-buffer cera--markdown-buffer
        (let ((inhibit-modification-hooks t))
          (erase-buffer)
          (insert text))
        (font-lock-ensure)
        (let ((position (point-min)))
          (while (< position (point-max))
            (let ((next (next-single-property-change
                         position 'face nil (point-max)))
                  (face (get-text-property position 'face)))
              (when face
                (with-current-buffer target
                  (put-text-property (+ begin (1- position)) (+ begin (1- next))
                                     'face face)))
              (setq position next))))))))

(defun cera--colour-input (session begin end)
  "Colour SESSION\='s field from BEGIN to END as the consumer asked.
The field is coloured as it is drawn rather than waiting on the display
to ask, which it does not do for text put in without modification hooks."
  (with-silent-modifications
    (remove-list-of-text-properties
     begin end '(face font-lock-face font-lock-multiline))
    (funcall cera-input-fontifier begin end)
    (cera--underlay-body session begin end)))

(defun cera--underlay-body (session begin end)
  "Put SESSION\='s field face under whatever coloured BEGIN through END.
The field carries its face as text rather than as an overlay wherever it
colours itself, since an overlay would cover those colours over."
  (let ((body (or (cera-pane-face (cera--session-input session))
                  (cera--body-face)))
        (position begin))
    (while (< position end)
      (let ((next (next-single-property-change position 'face nil end))
            (face (get-text-property position 'face)))
        (put-text-property position next 'face
                           (append (if (listp face) face (list face))
                                   (list body)))
        (setq position next)))))

(defun cera--unfontified (session)
  "Return a fontifier blind to SESSION\='s field.
What is written into the field is prose, and the buffer it is borrowing
would otherwise colour it as whatever language surrounds it."
  (let ((original font-lock-fontify-region-function))
    (lambda (begin end &optional loudly)
      (let ((from (cera--session-begin session))
            (to (cera--session-end session)))
        (if (or (cera--session-closed session) (<= to begin) (<= end from))
            (funcall original begin end loudly)
          (when (< begin from) (funcall original begin from loudly))
          (when (< to end) (funcall original to end loudly))
          (remove-list-of-text-properties
           (max begin from) (min end to)
           '(face font-lock-face font-lock-multiline))
          (when cera-input-fontifier
            (cera--colour-input session from to))
          ;; Report the whole field back: it is coloured as one piece, and
          ;; a caller told less would leave the rest of it holding old faces.
          `(jit-lock-bounds ,(min begin from) . ,(max end to)))))))

(declare-function hl-line-mode "hl-line" (&optional arg))
(declare-function global-hl-line-unhighlight "hl-line" ())
(defvar global-hl-line-buffers)

(defun cera--hold-off-hl-line (session)
  "Take the current line's highlight off SESSION's buffer while it is open.
The highlight is drawn over the field's own face; it comes back with
the rest of the buffer's settings.  The global mode's highlight kept
per window, which `global-hl-line-sticky-flag' `window' draws, asks
only `global-hl-line-buffers' whether a buffer takes it."
  (when (bound-and-true-p hl-line-mode)
    (setf (cera--session-hl-line session) t)
    (hl-line-mode -1))
  (when (bound-and-true-p global-hl-line-mode)
    (setq-local global-hl-line-mode nil
                global-hl-line-buffers nil)
    (global-hl-line-unhighlight)))

(defun cera--setup (session)
  "Install SESSION's input guard, completion, and modal key bindings."
  (cera--hold-off-hl-line session)
  (setq-local font-lock-fontify-region-function (cera--unfontified session))
  ;; The field is put in without modification hooks, so nothing has asked
  ;; for it to be coloured; ask here, or it stays plain until it is edited.
  (with-silent-modifications
    (put-text-property (cera--session-begin session) (cera--session-end session)
                       'fontified nil))
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
        (when (cera--session-hl-line session) (hl-line-mode 1))
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
                   (memq (cera-pane-align pane) '(nil input))
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
            (unless (cera--pane-content-p (cera-pane-text pane))
              (user-error "A read-only pane needs text or bounds"))))))
    (unless (= inputs 1) (user-error "A stack needs exactly one input pane")))
  (mapcar (lambda (pane)
            (let ((copy (copy-cera-pane pane)))
              (when-let* ((text (cera-pane-text pane)))
                (setf (cera-pane-text copy) (cera--copy-content text)))
              copy))
          panes))

(autoload 'cera-frame-read-stack "cera-frame")

(defcustom cera-input-backend 'auto
  "Where a field's input is written.
Its read-only panes are overlays in the buffer either way.
`buffer' writes the input into a line of the buffer itself, and every
line below it moves down by what is written.  `frame' keeps the buffer's
lines as they are and writes the input in a child frame laid over the
room it holds open for them, so the line numbers below the field stay
where they were; a terminal has no child frames, and reads in the buffer
whatever this says.  `auto' takes the frame wherever the display can
show one."
  :type '(choice (const :tag "Frame on a graphic display, buffer otherwise" auto)
                 (const :tag "A child frame over the buffer" frame)
                 (const :tag "A line of the buffer" buffer)))

(defun cera-toggle-input-backend ()
  "Swap the field between the buffer's own line and a child frame.
The next field opened takes the other reader than the one this display
would take now; a field already open stays as it is."
  (interactive)
  (setq cera-input-backend (if (eq (cera--input-backend) #'cera-frame-read-stack)
                         'buffer
                       'frame))
  (message "Fields open in %s" (if (eq cera-input-backend 'frame)
                                   "a child frame"
                                 "the buffer")))

(defun cera--input-backend ()
  "Return the reader `cera-input-backend' chooses for this display."
  (if (or (eq cera-input-backend 'frame)
          (and (eq cera-input-backend 'auto) (display-graphic-p) (not noninteractive)))
      #'cera-frame-read-stack
    #'cera-read-stack-in-buffer))

(defun cera-read-stack (panes table &optional keymap)
  "Read the single input of ordered PANES, completing on TABLE.
PANES are `cera-pane' descriptors.  Existing bounded panes stay in place
and must occur in document line order without overlapping lines.
The input opens after the preceding bounded pane, or the current line.
Supplied read-only panes are overlay text, displayed in list order.
KEYMAP and `cera-session-keymap' supplement the ordinary input bindings.
Return the input string; cancellation signals `quit'.
`cera-input-backend' decides where the field is drawn."
  (funcall (cera--input-backend) panes table keymap))

(defun cera-read-stack-in-buffer (panes table &optional keymap)
  "Read the input of PANES in a line put into the buffer, completing on TABLE.
This is the reader `cera-read-stack' describes, drawn in the buffer's
own text: KEYMAP supplements the input bindings and the result is what
was written.  The line is taken out again before it is returned."
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
