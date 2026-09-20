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

(defconst cera-frame--fringe 8
  "Pixels of fringe kept at the right of the child frame.
A window without one draws a continuation mark in its last column, which
the field's rows are then one column short of the buffer's.")

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
  aligned source-prefix bindings placed)

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

(defun cera-frame--child (field)
  "Return a fresh buffer for FIELD's input, set up as the parent has it."
  (let ((parent (cera-frame--field-parent field))
        (child (generate-new-buffer " *cera-frame*" t)))
    (with-current-buffer child
      (text-mode)
      (dolist (symbol cera-frame--copied-locals)
        (set (make-local-variable symbol) (buffer-local-value symbol parent)))
      (setq-local cera-frame--field field
                  cera--origin-buffer parent
                  cera-space-below 0
                  display-buffer-overriding-action
                  '(cera-frame--display-through-parent)
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

(defun cera-frame--make-frame (field)
  "Create the child frame FIELD's input is written in, still invisible.
The frame hooks are held off: a workspace manager would take the frame
for a new workspace and put its own buffer in it."
  (let* ((window (cera-frame--field-window field))
         (parent (window-frame window))
         (background (cera-frame--background))
         (before-make-frame-hook nil)
         (after-make-frame-functions nil)
         (frame (make-frame
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
                   (left-fringe . 0)
                   (right-fringe . ,cera-frame--fringe)
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
                   (no-focus-on-map . nil)))))
    (when background
      (set-face-background 'fringe background frame))
    (let ((view (frame-root-window frame)))
      (set-window-buffer view (cera-frame--field-child field))
      (set-window-dedicated-p view t)
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
They follow the newline closing the source line, or, where the buffer
ends there, open a row after its last character, with `cera-space-below'
under the last of them."
  (let* ((pane (cera-frame--field-input field))
         (aligned (cera-frame--field-source-prefix field))
         (below (cera-frame--field-below field))
         (space (with-current-buffer (cera-frame--field-parent field)
                  (and (> cera-space-below 0) cera-space-below)))
         (text (mapconcat (lambda (index)
                            (cera--pane-decoration
                             pane (cera--pane-endpoint pane (zerop index)
                                                       (= (1+ index) rows))
                             aligned))
                          (number-sequence 0 (1- rows)) "\n")))
    (propertize
     (if below
         (concat (unless (with-current-buffer (cera-frame--field-parent field)
                           (save-excursion
                             (goto-char (cera-frame--field-anchor field))
                             (bolp)))
                   "\n")
                 text)
       (concat text (propertize "\n" 'line-spacing space)))
     'line-prefix "" 'wrap-prefix "")))

(defun cera-frame--hold-rows (field rows)
  "Hold ROWS rows open under FIELD's source, redrawn when the count is new."
  (unless (eql rows (cera-frame--field-rows field))
    (setf (cera-frame--field-rows field) rows)
    (overlay-put (cera-frame--field-holder field)
                 (if (cera-frame--field-below field) 'before-string 'after-string)
                 (cera-frame--rows-text field rows))))

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
               (cera--session-static-overlays (cera-frame--field-session field))))))

(defun cera-frame--place (field &optional force)
  "Lay FIELD's frame over the rows held open for it, or hide it off-screen.
Nothing is worked out again while what the position depends on is as
it was, unless FORCE.  The rows follow the newline closing the source
line, whose row is where the frame is counted on from.  Where the
buffer ends at the anchor the rows are drawn before it, so the row of
the character before the anchor is found instead; an empty buffer
shows the anchor after them."
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
             (below (cera-frame--field-below field))
             (trailing (and below (= anchor (with-current-buffer parent (point-min)))))
             (visible (with-current-buffer parent
                        (pos-visible-in-window-p
                         (if (and below (not trailing)) (1- anchor) anchor)
                         window t))))
        (if (not (consp visible))
            (when (frame-visible-p frame) (make-frame-invisible frame))
          (let* ((edges (nth 2 key))
                 (line (window-default-line-height window))
                 (x (nth 0 edges))
                 (y (+ (nth 1 edges) (nth 1 visible)
                       (if trailing (- (* line (1- rows))) line))))
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
             (width (+ (- (nth 2 edges) (nth 0 edges)) cera-frame--fringe))
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

(defconst cera-frame--parent-locals
  '(cera--active cera--field-buffer cera-frame--field
                 cursor-in-non-selected-windows global-hl-line-mode
                 kill-buffer-hook pre-redisplay-functions window-scroll-functions
                 window-size-change-functions window-configuration-change-hook)
  "Settings borrowed in the buffer a frame field is read for.")

(defun cera-frame--open (field)
  "Draw FIELD's panes in its parent and hold the parent's settings for it."
  (setf (cera-frame--field-bindings field)
        (cera--remember-locals cera-frame--parent-locals))
  (setq-local cera--active (cera-frame--field-session field)
              cera--field-buffer (cera-frame--field-child field)
              cera-frame--field field
              cursor-in-non-selected-windows nil)
  (cera--hold-off-hl-line (cera-frame--field-session field))
  (add-hook 'kill-buffer-hook #'cera-frame--abandon -100 t)
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
    (overlay-put (cera-frame--field-holder field) 'priority 1001))
  (cera--draw-static (cera-frame--field-session field)))

(defun cera-frame--close (field)
  "Take FIELD's frame down and put its parent back as it was."
  (let ((frame (cera-frame--field-frame field))
        (child (cera-frame--field-child field))
        (parent (cera-frame--field-parent field))
        (window (cera-frame--field-window field))
        (session (cera-frame--field-session field)))
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
supplements the input bindings the same way.  No pane may follow the
input.  The buffer gets no line put into it: the bounded panes are
bracketed where they are, the rows the input takes are held open below
them, and the input is written in a frame laid over those rows.  Where
no frame can be shown the field is read in the buffer instead."
  (if (not (cera-frame--available-p))
      (cera-read-stack-in-buffer panes table keymap)
    (when cera--active (user-error "A field is already open"))
    (setq panes (cera--validate-panes panes))
    (let* ((input (cl-find 'input panes :key #'cera-pane-kind))
           (anchor (progn
                     (when (cdr (memq input panes))
                       (user-error "A frame field takes no panes below its input"))
                     (cera-frame--anchor panes input)))
           (field (cera-frame--make-field
                   :parent (current-buffer) :window (cera-frame--window)
                   :input input
                   :anchor (copy-marker (car anchor)) :below (cdr anchor)
                   :source-prefix (cera-frame--source-prefix panes input)
                   :aligned (concat (cera--line-number-pad)
                                    (cera-frame--source-prefix panes input)))))
      (setf (cera-frame--field-session field)
            (cera--make-session
             :buffer (current-buffer) :input input
             :panes (remq input panes)
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
          (set-marker (cera-frame--field-anchor field) nil))))))

(provide 'cera-frame)
;;; cera-frame.el ends here
