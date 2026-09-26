;;; cera-frame.el --- Write into a cera field held in a child frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/cera
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (cera "0.1.0"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `cera-frame-read-stack' reads a field without putting a line into the
;; buffer it is opened for.  The source is bracketed with overlays as it
;; is by the in-buffer reader, the rows the input takes are held open by
;; an overlay string below it, and the input itself is written in a child
;; frame laid over those rows, where it is an ordinary in-buffer field in
;; a buffer of its own.  The buffer keeps every line it had, so the line
;; numbers below the field stay where they were however long the input
;; grows.  `cera-input-backend' chooses it wherever a child frame can be shown.

;;; Code:

(require 'cera)
(require 'delsel)

(defcustom cera-frame-scroll-parent nil
  "Whether \"the other window\" from inside the field is the one it is read over.
The frame holds one window of its own, so the scroll commands take
whichever window Emacs reaches next — a neighbour of the frame, or one
on another visible frame.  Non-nil points them at the buffer the field
was opened over instead."
  :type 'boolean
  :group 'cera)

(defcustom cera-frame-host-modifiers '(super)
  "Modifiers whose global bindings run from the window the field is read over.
The field holds a frame of one window, so a global command that moves
between windows, switches workspaces or reads the buffer around it
would see only that frame.  A key carrying one of these modifiers runs
its global binding from the window the field is read over instead, as
it would with the field written in the buffer, and the keyboard comes
back to the field once that window is selected again.  A binding that
writes text, as `delete-selection-mode' marks one, still writes into
the field."
  :type '(repeat (choice (const super) (const hyper) (const alt)))
  :group 'cera)

(defcustom cera-frame-parameters '((persp-ignore-wconf . t))
  "Extra parameters put on the child frame the input is written in.
They tell the workspace managers that keep a layout per frame to leave
it alone: one restoring a saved layout into whatever frame is selected
reaches the child frame while the field holds the keyboard, and lays
the workspace's buffers over the input.  The default carries the flag
`persp-mode' reads; add what another manager reads beside it."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'cera)

(defun cera-frame--fringes (window)
  "Return the pixels of WINDOW's left and right fringes, as a list.
The child frame takes the same, so that its text area is the window's:
a window without a right fringe gives its last column to the
continuation mark, which moves the centre a line is aligned on."
  (let ((fringes (window-fringes window)))
    (list (or (nth 0 fringes) 0) (or (nth 1 fringes) 0))))

(cl-defstruct (cera-frame--field (:constructor cera-frame--make-field))
  "A field read over PARENT, shown in WINDOW, written in CHILD.
SESSION draws the panes above the input in PARENT.  INPUT is the pane
the rows are decorated for, and HOLDER the overlay holding ROWS rows
open under FRAME: it hangs off ANCHOR, the newline closing the line
above the rows, or the end of the buffer where that line has none, and
BELOW is non-nil when the rows follow ANCHOR's row rather than precede
the one after it.  ALIGNED is the prefix the rows sit behind in CHILD,
SOURCE-PREFIX the one they sit behind in PARENT, BINDINGS what PARENT
had before the field borrowed its settings, and PLACED what the
frame's position was last worked out from."
  parent window child frame session input holder anchor below rows
  aligned source-prefix bindings placed under under-rows)

(defvar-local cera-frame--field nil
  "The field written in a child frame over this buffer, or in it.")

(defun cera-frame--window ()
  "Return the window a frame field over the current buffer is laid on, or nil.
The selected window where it shows the buffer, or else any window on a
graphic display that does."
  (when (and (display-graphic-p) (not noninteractive))
    (if (eq (window-buffer (selected-window)) (current-buffer))
        (selected-window)
      (seq-find (lambda (window) (display-graphic-p (window-frame window)))
                (get-buffer-window-list (current-buffer) nil t)))))

(defun cera-frame--available-p ()
  "Return non-nil when a child frame can be laid over the current buffer."
  (and (cera-frame--window) t))

(defun cera-frame--anchor (panes input)
  "Return where the rows of INPUT hang in PANES, as a cons of (ANCHOR . BELOW).
ANCHOR is the newline closing the last line of the bounded pane before
INPUT, or of the current line, where the in-buffer reader's field opens;
where that line has no newline it is the end of the buffer.  BELOW is
non-nil in that case, the rows following ANCHOR's row rather than
preceding the row of the line after it."
  (let* ((before (cl-subseq panes 0 (cl-position input panes)))
         (source (cl-find-if #'cera-pane-bounds (reverse before)))
         (end (save-excursion
                (when source
                  (let ((bounds (cera-pane-bounds source)))
                    (goto-char (max (car bounds) (1- (cdr bounds))))))
                (line-end-position))))
    (cons end (= end (point-max)))))

(defun cera-frame--source-prefix (panes input)
  "Return the prefix the lines the rows of INPUT follow are drawn behind.
It is read off the first line of the bounded pane before INPUT in PANES,
or the current line, before that line is bracketed."
  (let* ((before (cl-subseq panes 0 (cl-position input panes)))
         (source (cl-find-if #'cera-pane-bounds (reverse before)))
         (origin (save-excursion
                   (when source (goto-char (car (cera-pane-bounds source))))
                   (line-beginning-position))))
    (cera--prefix-text (get-char-property origin 'line-prefix))))

(defconst cera-frame--copied-locals
  '(cera-input-prefix cera-input-prefix-width cera-indent
                      cera-completion-space cera-completion-function
                      cera-input-fontifier cera-borrowed-locals)
  "Settings the child buffer takes over from the buffer the field is read for.")

(defun cera-frame--display-through-parent (buffer alist)
  "Display BUFFER by ALIST from the window the field is read over.
A command run in the child frame would otherwise have `display-buffer'
choose a window on that frame, which cannot be split, and open a frame
of its own instead."
  (when-let* ((field cera-frame--field)
              (window (cera-frame--field-window field))
              ((window-live-p window)))
    (with-selected-window window
      (let ((display-buffer-overriding-action nil))
        (display-buffer buffer alist)))))

(defun cera-frame--scroll-window ()
  "Return the window the field written here is read over, or nil.
The frame holds one window of its own, so a command scrolling \"the
other window\" from inside the field would otherwise take whichever
window Emacs reaches next — a neighbour of the frame, or one on another
visible frame — rather than the buffer the field was opened over."
  (when-let* ((field cera-frame--field)
              (window (cera-frame--field-window field))
              ((window-live-p window)))
    window))

(defun cera-frame--in-host (field window command)
  "Run COMMAND from WINDOW, the one FIELD is read over, and come back.
The keyboard stays with the field's frame unless COMMAND needs it: a
minibuffer COMMAND opens is on the host window's frame, where the keys
typed into it would otherwise never arrive, and a window COMMAND leaves
selected keeps it.  Moving the keyboard switches the window system's
input method, which the macOS port crashes in when a held key repeats
it, so a command acting on the host alone never moves it."
  (let ((frame (cera-frame--field-frame field))
        (focused nil))
    (select-frame (window-frame window) t)
    (select-window window)
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq focused t)
              (select-frame-set-input-focus (window-frame window)))
          (call-interactively command))
      (cond ((not (eq (selected-window) window))
             (select-frame-set-input-focus (selected-frame)))
            ((frame-live-p frame)
             (if focused
                 (select-frame-set-input-focus frame)
               (select-frame frame t))
             (select-window (frame-root-window frame)))))))

(defun cera-frame--host-command ()
  "Run a global binding on `cera-frame-host-modifiers' from the host window."
  (when-let* ((field cera-frame--field)
              (window (cera-frame--field-window field))
              ((window-live-p window))
              (keys (this-command-keys-vector))
              ((> (length keys) 0))
              ((seq-intersection (event-modifiers (aref keys 0))
                                 cera-frame-host-modifiers))
              (command this-command)
              ((eq command (lookup-key (current-global-map) keys)))
              ((not (and (symbolp command) (get command 'delete-selection)))))
    (setq this-command
          (lambda ()
            (interactive)
            (setq this-command command)
            (cera-frame--in-host field window command)))))

(defun cera-frame--child (field)
  "Return a fresh buffer for FIELD's input, set up as the parent has it."
  (let ((parent (cera-frame--field-parent field))
        (child (generate-new-buffer " *cera-frame*" t)))
    (with-current-buffer child
      (text-mode)
      (dolist (symbol cera-frame--copied-locals)
        (set (make-local-variable symbol) (buffer-local-value symbol parent)))
      (add-hook 'pre-command-hook #'cera-frame--host-command nil t)
      (setq-local cera-frame--field field
                  cera--origin-buffer parent
                  cera-space-below 0
                  display-buffer-overriding-action
                  '(cera-frame--display-through-parent)
                  other-window-scroll-default
                  (and cera-frame-scroll-parent #'cera-frame--scroll-window)
                  mode-line-format nil
                  header-line-format nil
                  tab-line-format nil
                  display-line-numbers nil
                  left-margin-width 0
                  right-margin-width 0
                  indicate-empty-lines nil
                  show-trailing-whitespace nil
                  scroll-margin 0
                  maximum-scroll-margin 0
                  scroll-conservatively 0))
    child))

(defun cera-frame--background ()
  "Return the field's background, for the frame around its text."
  (let ((background (face-background 'default nil t)))
    (if (color-defined-p background)
        (cera--shaded background)
      (face-background 'cera-body nil t))))

(defun cera-frame--parameters (parent background &optional fringes)
  "Return the parameters of a child frame over PARENT, drawn on BACKGROUND.
FRINGES are the left and right fringe pixels, none by default.
`cera-frame-parameters' comes first, where a parameter it names is the
one the frame is made with."
  (append
   cera-frame-parameters
   `((parent-frame . ,parent)
     (font . ,(frame-parameter parent 'font))
     (line-spacing . ,(frame-parameter parent 'line-spacing))
     (background-color . ,background)
     (minibuffer . nil)
     (undecorated . t)
     (visibility . nil)
     (width . 1) (height . 1)
     (min-width . 0) (min-height . 0)
     (border-width . 0)
     (internal-border-width . 0)
     (child-frame-border-width . 0)
     (left-fringe . ,(or (nth 0 fringes) 0))
     (right-fringe . ,(or (nth 1 fringes) 0))
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (tab-bar-lines . 0)
     (no-special-glyphs . t)
     (unsplittable . t)
     (no-other-frame . t)
     (desktop-dont-save . t)
     (cursor-type . t)
     (no-accept-focus . nil)
     (no-focus-on-map . nil))))

(defun cera-frame--make-frame (field)
  "Create the child frame FIELD's input is written in, still invisible.
The frame hooks are held off: a workspace manager would take the frame
for a new workspace and put its own buffer in it."
  (let* ((window (cera-frame--field-window field))
         (parent (window-frame window))
         (background (cera-frame--background))
         (before-make-frame-hook nil)
         (after-make-frame-functions nil)
         (frame (make-frame (cera-frame--parameters
                             parent background (cera-frame--fringes window)))))
    (when background
      (set-face-background 'fringe background frame))
    (let ((view (frame-root-window frame)))
      (set-window-buffer view (cera-frame--field-child field))
      (set-window-dedicated-p view 'cera)
      (set-window-parameter view 'mode-line-format 'none)
      (set-window-parameter view 'header-line-format 'none))
    frame))

(defun cera-frame--content-height (field)
  "Return the pixels FIELD's input takes in its child frame, room and all.
The line before the input, which the window keeps scrolled out of
sight, is left out; the measure is taken with the window unscrolled,
since it comes back short of what is scrolled away."
  (let* ((frame (cera-frame--field-frame field))
         (view (frame-root-window frame))
         (limit (frame-pixel-height (frame-parent frame)))
         (vscroll (window-vscroll view t)))
    (with-current-buffer (cera-frame--field-child field)
      (set-window-vscroll view 0 t)
      (prog1 (cdr (window-text-pixel-size view (cera--session-begin cera--active)
                                          (point-max) nil limit))
        (set-window-vscroll view vscroll t)))))

(defun cera-frame--rows-text (field rows)
  "Return ROWS bracketed rows to hold open under FIELD's source.
They are drawn as the in-buffer reader draws the input's rows, with its
brackets, so a window the frame is not over still shows the field.
They are put before the newline closing the source line, or where the
buffer ends there after its last character, and open a row of their
own: a line the rows began would draw its prefix in front of them.
The panes under the input come after them, on the buffer's own ground
rather than the field's.  The newline or the end of the buffer closes
the last row, and `cera-space-below' is kept under it."
  (let* ((pane (cera-frame--field-input field))
         (aligned (cera-frame--field-source-prefix field))
         (below (cera-frame--field-below field))
         (text (mapconcat (lambda (index)
                            (cera--pane-decoration
                             pane (cera--pane-endpoint pane (zerop index)
                                                       (= (1+ index) rows))
                             aligned))
                          (number-sequence 0 (1- rows)) "\n"))
         (under (cera-frame--under-text field)))
    (setf (cera-frame--field-under-rows field)
          (if under (cl-count ?\n under) 0))
    (propertize
     (concat (unless (and below
                          (with-current-buffer (cera-frame--field-parent field)
                            (save-excursion
                              (goto-char (cera-frame--field-anchor field))
                              (bolp))))
               "\n")
             text
             (and under (concat "\n" (string-remove-suffix "\n" under))))
     'line-prefix "" 'wrap-prefix "")))

(defun cera-frame--under-text (field)
  "Return the panes under FIELD's rows as the parent draws them, or nil."
  (when-let* ((window (cera-frame--field-window field))
              ((window-live-p window))
              (panes (cl-remove-if-not #'cera--pane-visible-p
                                       (cera-frame--field-under field))))
    (with-current-buffer (cera-frame--field-parent field)
      (let ((cera--aligned (cera-frame--field-source-prefix field)))
        (mapconcat (lambda (pane)
                     (cera--virtual-text pane (window-body-width window)
                                         cera--aligned))
                   panes "")))))

(defun cera-frame--update-under (id text)
  "Put TEXT in the pane ID under the rows of the field over this buffer.
Return non-nil when the field holds a pane of that name."
  (when-let* ((field cera-frame--field)
              (pane (cl-find id (cera-frame--field-under field)
                             :key #'cera-pane-id :test #'equal)))
    (setf (cera-pane-text pane) (cera--copy-content text))
    (cera-frame--hold-rows field (cera-frame--field-rows field) t)
    t))

(defun cera-frame--hold-rows (field rows &optional force)
  "Hold ROWS rows open under FIELD's source, redrawn when the count is new.
FORCE redraws them with the count as it was, for a pane under them."
  (when (or force (not (eql rows (cera-frame--field-rows field))))
    (setf (cera-frame--field-rows field) rows)
    (let ((holder (cera-frame--field-holder field)))
      (overlay-put holder 'before-string (cera-frame--rows-text field rows))
      (overlay-put holder 'line-spacing (cera-frame--space-below field)))))

(defun cera-frame--space-below (field)
  "Return the pixels kept under FIELD's input rows, or nil for none."
  (with-current-buffer (cera-frame--field-parent field)
    (and (> cera-space-below 0) cera-space-below)))

(defun cera-frame--row-top (window offset)
  "Return the frame coordinate of the row OFFSET pixels down WINDOW.
`pos-visible-in-window-p' counts from the top of the window, a header
line and a tab line included, while `window-inside-pixel-edges' counts
from the top of the frame with those left out.  Adding the two counts a
header line twice and lays the frame a row below the rows held for it,
so the window's own top is what the offset is taken from."
  (+ (nth 1 (window-pixel-edges window)) offset))

(defun cera-frame--placement-key (field)
  "Return what FIELD's frame position depends on, or nil off the window.
The panes drawn above the source are part of it: a pane filled in
after the field opened moves the source line down."
  (let ((window (cera-frame--field-window field)))
    (and (window-live-p window)
         (eq (window-buffer window) (cera-frame--field-parent field))
         (list (window-start window)
               (window-vscroll window t)
               (window-inside-pixel-edges window)
               (marker-position (cera-frame--field-anchor field))
               (cera-frame--field-rows field)
               (cera--session-static-overlays (cera-frame--field-session field))
               (cera-frame--field-under-rows field)))))

(defun cera-frame--place (field &optional force)
  "Lay FIELD's frame over the rows held open for it, or hide it off-screen.
Nothing is worked out again while what the position depends on is as
it was, unless FORCE.  The rows are drawn before the anchor, after any
pane hung there too, and followed by the panes under them, so the row
the anchor closes is the last of those: the frame is counted back from
it."
  (let ((key (cera-frame--placement-key field))
        (frame (cera-frame--field-frame field)))
    (cond
     ((null key)
      (setf (cera-frame--field-placed field) nil)
      (when (frame-visible-p frame) (make-frame-invisible frame)))
     ((and (not force) (equal key (cera-frame--field-placed field))))
     (t
      (setf (cera-frame--field-placed field) key)
      (let* ((window (cera-frame--field-window field))
             (parent (cera-frame--field-parent field))
             (anchor (nth 3 key))
             (rows (or (nth 4 key) 1))
             (under (or (nth 6 key) 0))
             (visible (with-current-buffer parent
                        (pos-visible-in-window-p anchor window t))))
        (if (not (consp visible))
            (when (frame-visible-p frame) (make-frame-invisible frame))
          (let* ((edges (nth 2 key))
                 (line (window-default-line-height window))
                 (x (- (nth 0 edges) (car (cera-frame--fringes window))))
                 (y (- (cera-frame--row-top window (nth 1 visible))
                       (* line (+ (1- rows) under)))))
            (unless (equal (frame-position frame) (cons x y))
              (set-frame-position frame x y))
            (unless (frame-visible-p frame) (make-frame-visible frame)))))))))

(defun cera-frame--sync (field)
  "Size FIELD's frame to its input, hold that many rows open, and place it."
  (let ((frame (cera-frame--field-frame field))
        (window (cera-frame--field-window field))
        (child (cera-frame--field-child field)))
    (when (and (frame-live-p frame) (window-live-p window)
               (buffer-live-p child)
               (buffer-local-value 'cera--active child))
      (let* ((edges (window-inside-pixel-edges window))
             (width (+ (- (nth 2 edges) (nth 0 edges))
                       (apply #'+ (cera-frame--fringes window))))
             (line (window-default-line-height window)))
        (unless (= (frame-pixel-width frame) width)
          (set-frame-size frame width (frame-pixel-height frame) t))
        (let ((height (cera-frame--content-height field)))
          (cera-frame--hold-rows field (max 1 (round height line)))
          (unless (= (frame-pixel-height frame) height)
            (set-frame-size frame width height t))
          (cera-frame--pin field)))
      (cera-frame--place field)
      (cera-frame--keep-in-window field))))

(defun cera-frame--pin (field)
  "Show FIELD's input from its first row, the line before it scrolled away.
The display draws the window's first row taller than its text where
that row opens with the bracket, so the line before the input is left
on the screen as that row and scrolled out of sight."
  (let ((view (frame-root-window (cera-frame--field-frame field))))
    (with-current-buffer (cera-frame--field-child field)
      (set-window-start view (point-min))
      (set-window-vscroll view (window-default-line-height view) t))))

(defun cera-frame--keep-in-window (field)
  "Scroll the window FIELD is read over so its rows are within it.
The window's own point sits on the source line above the rows, so
nothing else would bring rows grown past its bottom into view."
  (let* ((window (cera-frame--field-window field))
         (frame (cera-frame--field-frame field))
         (line (window-default-line-height window))
         (overflow (and (frame-visible-p frame)
                        (- (+ (cdr (frame-position frame)) (frame-pixel-height frame))
                           (nth 3 (window-inside-pixel-edges window))))))
    (when (and overflow (> overflow 0))
      (with-current-buffer (cera-frame--field-parent field)
        (let ((start (save-excursion
                       (goto-char (window-start window))
                       (vertical-motion (ceiling overflow line) window)
                       (point))))
          (unless (= start (window-start window))
            (set-window-start window start)
            (cera-frame--place field t)))))))

(defun cera-frame--sync-command ()
  "Follow the input after a command written into it."
  (when-let* ((field cera-frame--field)
              ((eq (current-buffer) (cera-frame--field-child field))))
    (cera-frame--sync field)))

(defun cera-frame--follow (window &optional _start)
  "Keep the frame over its rows as WINDOW is scrolled, resized or redrawn."
  (when-let* ((field cera-frame--field)
              ((eq window (cera-frame--field-window field)))
              (frame (cera-frame--field-frame field))
              ((frame-live-p frame)))
    (cera-frame--place field)))

(defun cera-frame--scrolled (window start)
  "Move the frame with WINDOW, which is about to show START at its top."
  (when-let* ((field cera-frame--field)
              ((eq window (cera-frame--field-window field)))
              (frame (cera-frame--field-frame field))
              ((frame-live-p frame)))
    (ignore start)
    (cera-frame--place field t)))

(defun cera-frame--show (field)
  "Put FIELD's frame up over its rows and give it the keyboard."
  (let ((frame (cera-frame--make-frame field)))
    (setf (cera-frame--field-frame field) frame)
    (cera-frame--sync field)
    (make-frame-visible frame)
    (select-frame-set-input-focus frame)
    (select-window (frame-root-window frame))))

(defun cera-frame--start (field session)
  "Draw SESSION's input as the parent would and raise FIELD's frame over it.
Runs from `cera-session-start-hook' in the child buffer, where the
in-buffer reader has just put its field."
  (setf (cera--session-aligned session) (cera-frame--field-aligned field))
  (cera--draw)
  (cera-frame--hold-rows
   field (count-lines (cera--session-begin session) (cera--session-end session)))
  (add-hook 'post-command-hook #'cera-frame--sync-command 95 t)
  (cera-frame--show field))

(defun cera-frame--abandon ()
  "Cancel the field written over this buffer, ahead of the buffer going."
  (when-let* ((field cera-frame--field)
              (child (cera-frame--field-child field))
              ((buffer-live-p child)))
    (with-current-buffer child
      (when cera--active (cera--release)))))

(defvar cera-frame--fields nil
  "The fields being written in a child frame now, newest first.")

(defun cera-frame--taken-p (field)
  "Return the buffer laid over FIELD's input in its own frame, or nil.
The frame holds one window, dedicated to the buffer the input is
written in; a command reaching the frame while it has the keyboard —
a workspace manager restoring a layout into whatever frame is
selected — leaves another buffer there instead."
  (when-let* ((frame (cera-frame--field-frame field))
              ((frame-live-p frame))
              (child (cera-frame--field-child field))
              ((buffer-live-p child))
              ((not (get-buffer-window child frame)))
              (view (frame-selected-window frame)))
    (window-buffer view)))

(defvar cera-frame--reclaiming nil
  "Whether a field is being given back, the hook held off while it is.")

(defun cera-frame--reclaim ()
  "Give back a field whose frame another command has put its own buffer in.
The field is released as it is for a command that takes the buffer
whole: what was written goes on the kill ring and the reader returns
once the command does.  The buffer put in the frame is shown in the
window the field was read over, where the command meant it to go."
  (unless cera-frame--reclaiming
    (let ((cera-frame--reclaiming t))
      (dolist (field (copy-sequence cera-frame--fields))
        (when-let* ((taken (cera-frame--taken-p field)))
          (let ((child (cera-frame--field-child field))
                (window (cera-frame--field-window field)))
            (with-current-buffer child
              (when cera--active (cera--release)))
            (when (window-live-p window)
              (select-frame-set-input-focus (window-frame window))
              (select-window window)
              (unless (eq taken child)
                (set-window-buffer window taken)))))))))

(defun cera-frame--return (&rest _)
  "Give the keyboard back to the field whose window was selected again.
A key on `cera-frame-host-modifiers' left the field for the window it is
read over; the field is written there, so selecting that window is
selecting the field.  The frame is raised from a timer: raising it while
redisplay runs this hook crashes the macOS port mid input-method switch."
  (run-at-time 0 nil #'cera-frame--focus-field (selected-window)))

(defun cera-frame--focus-field (window)
  "Give the keyboard to the field read over WINDOW, if it is still selected."
  (when-let* (((eq window (selected-window)))
              (field (seq-find (lambda (field)
                                 (eq (cera-frame--field-window field) window))
                               cera-frame--fields))
              (frame (cera-frame--field-frame field))
              ((frame-live-p frame))
              (child (cera-frame--field-child field))
              ((buffer-local-value 'cera--active child)))
    (select-frame-set-input-focus frame)
    (select-window (frame-root-window frame))))

(defconst cera-frame--parent-locals
  '(cera--active cera--field-buffer cera-frame--field
                 cursor-in-non-selected-windows global-hl-line-mode
                 global-hl-line-buffers
                 kill-buffer-hook pre-redisplay-functions window-scroll-functions
                 window-size-change-functions window-configuration-change-hook
                 cera-update-pane-functions)
  "Settings borrowed in the buffer a frame field is read for.")

(defun cera-frame--open (field)
  "Draw FIELD's panes in its parent and hold the parent's settings for it."
  (setf (cera-frame--field-bindings field)
        (cera--remember-locals cera-frame--parent-locals))
  (unless cera-frame--fields
    (add-hook 'window-configuration-change-hook #'cera-frame--reclaim)
    (add-hook 'window-selection-change-functions #'cera-frame--return))
  (push field cera-frame--fields)
  (setq-local cera--active (cera-frame--field-session field)
              cera--field-buffer (cera-frame--field-child field)
              cera-frame--field field
              cursor-in-non-selected-windows nil)
  (cera--hold-off-hl-line (cera-frame--field-session field))
  (add-hook 'kill-buffer-hook #'cera-frame--abandon -100 t)
  (add-hook 'cera-update-pane-functions #'cera-frame--update-under nil t)
  (add-hook 'pre-redisplay-functions #'cera-frame--follow nil t)
  (add-hook 'window-scroll-functions #'cera-frame--scrolled nil t)
  (add-hook 'window-size-change-functions #'cera--resize nil t)
  (add-hook 'window-configuration-change-hook #'cera--resize nil t)
  (let ((anchor (cera-frame--field-anchor field)))
    (setf (cera-frame--field-holder field)
          (make-overlay anchor (if (cera-frame--field-below field)
                                   anchor
                                 (1+ anchor))
                        nil t nil))
    ;; Above the panes' own, so that where both hang off the end of the
    ;; buffer the rows come after them.
    (overlay-put (cera-frame--field-holder field) 'priority 1002))
  (cera--draw-static (cera-frame--field-session field)))

(defun cera-frame--close (field)
  "Take FIELD's frame down and put its parent back as it was."
  (let ((frame (cera-frame--field-frame field))
        (child (cera-frame--field-child field))
        (parent (cera-frame--field-parent field))
        (window (cera-frame--field-window field))
        (session (cera-frame--field-session field)))
    (setq cera-frame--fields (delq field cera-frame--fields))
    (unless cera-frame--fields
      (remove-hook 'window-configuration-change-hook #'cera-frame--reclaim)
      (remove-hook 'window-selection-change-functions #'cera-frame--return))
    (when (frame-live-p frame) (delete-frame frame t))
    (when (buffer-live-p child) (kill-buffer child))
    (when (buffer-live-p parent)
      (with-current-buffer parent
        (when-let* ((holder (cera-frame--field-holder field)))
          (delete-overlay holder))
        (mapc #'delete-overlay (cera--session-static-overlays session))
        (mapc #'delete-overlay (cera--session-overlays session))
        (cera--restore-bindings (cera-frame--field-bindings field))
        (when (cera--session-hl-line session) (hl-line-mode 1))))
    (when (window-live-p window)
      (save-current-buffer
        (select-frame-set-input-focus (window-frame window))
        (select-window window)))))

;;;###autoload
(defun cera-frame-read-stack (panes table &optional keymap)
  "Read the input of PANES in a child frame over the buffer, completing on TABLE.
The panes are as `cera-read-stack' describes them, and KEYMAP
supplements the input bindings the same way.  The buffer gets no line
put into it: the bounded panes are bracketed where they are, the rows
the input takes are held open below them, and the input is written in a
frame laid over those rows.  Supplied panes following the input up to
the next bounded one are drawn under the rows, outside the frame; the
rest stay where the buffer has them.  Where no frame can be shown the
field is read in the buffer instead."
  (if (not (cera-frame--available-p))
      (cera-read-stack-in-buffer panes table keymap)
    (when cera--active (user-error "A field is already open"))
    (setq panes (cera--validate-panes panes))
    ;; A buffer that redraws itself while the field is open moves the lines
    ;; the bounded panes bracket, and the brackets are redrawn from these.
    (dolist (pane panes)
      (when-let* ((range (cera-pane-bounds pane)))
        (setf (cera-pane-bounds pane)
              (cons (copy-marker (car range)) (copy-marker (cdr range))))))
    (let* ((input (cl-find 'input panes :key #'cera-pane-kind))
           (under (cl-loop for pane in (cdr (memq input panes))
                           until (cera-pane-bounds pane) collect pane))
           (anchor (cera-frame--anchor panes input))
           (field (cera-frame--make-field
                   :parent (current-buffer) :window (cera-frame--window)
                   :input input :under under
                   :anchor (copy-marker (car anchor)) :below (cdr anchor)
                   :source-prefix (cera-frame--source-prefix panes input)
                   :aligned (concat (cera--line-number-pad)
                                    (cera-frame--source-prefix panes input)))))
      (setf (cera-frame--field-session field)
            (cera--make-session
             :buffer (current-buffer) :input input
             :aligned (cera-frame--source-prefix panes input)
             :panes (cl-remove-if (lambda (pane) (memq pane under)) panes)
             :tail (copy-marker (car anchor))))
      (save-window-excursion
        (unwind-protect
            (progn
              (setf (cera-frame--field-child field) (cera-frame--child field))
              (cera-frame--open field)
              (with-current-buffer (cera-frame--field-child field)
                (let ((cera-session-start-hook
                       (cons (lambda (session) (cera-frame--start field session))
                             cera-session-start-hook)))
                  (cera-read-stack-in-buffer (list input) table keymap))))
          (cera-frame--close field)
          (set-marker (cera-frame--field-anchor field) nil)
          (dolist (pane panes)
            (when-let* ((range (cera-pane-bounds pane)))
              (set-marker (car range) nil)
              (set-marker (cdr range) nil))))))))

(provide 'cera-frame)
;;; cera-frame.el ends here
