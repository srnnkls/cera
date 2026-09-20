;;; corfu-cera.el --- Corfu integration for cera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/cera
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (cera "0.1.0") (corfu "1.0"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `corfu-cera' completes in a cera field with Corfu and holds the room
;; the popup takes, so it is not drawn over the text below the field.

;;; Code:

(require 'corfu)
;; Newer Corfu keeps its automatic completion options here.
(require 'corfu-auto nil t)
(require 'cera)

(cera-register-borrowed-locals
 'corfu-auto 'corfu-auto-prefix 'corfu-auto-trigger 'corfu-quit-at-boundary
 'corfu-cera--timer 'corfu-cera--enabled-before 'corfu-cera--session)

(defcustom corfu-cera-space 10
  "Lines kept free below the field while Corfu shows its candidates.
Corfu draws its popup over the buffer, so the field holds this much room
for it.  Raise it when a long candidate list still reaches past the room
into the text below."
  :type 'natnum
  :group 'cera)

(defvar-local corfu-cera--timer nil
  "Timer asking Corfu to complete in the newly opened field.")

(defvar-local corfu-cera--enabled-before nil
  "Whether Corfu was on before the field opened.
`corfu-mode' is global, so putting it back is not a local variable's job.")


(defvar-local corfu-cera--session nil
  "Session whose completion this adapter owns.")

(defun corfu-cera--start-completion (buffer session)
  "Ask Corfu to complete in BUFFER for SESSION, as after a character typed.
The field opens after Corfu has already run its own cancel for this
command, so the request has to be made again once the field is up."
  (when (and (buffer-live-p buffer)
             (eq buffer (window-buffer (selected-window)))
             (eq session (buffer-local-value 'corfu-cera--session buffer)))
    (with-current-buffer buffer
      ;; Source command hooks belong to real user commands: only Corfu's
      ;; own should see this request for automatic completion.
      (let ((this-command 'self-insert-command)
            (corfu-auto-delay 0))
        (run-hook-wrapped
         'post-command-hook
         (lambda (function)
           (when (and (symbolp function)
                      (string-prefix-p "corfu" (symbol-name function)))
             (funcall function))
           nil))))))


(defun corfu-cera--field-frame ()
  "Return the child frame the field is written in, or nil where it is not."
  (and (bound-and-true-p cera--origin-buffer)
       (frame-parent (selected-frame))
       (selected-frame)))

(defun corfu-cera--over-parent (show &rest arguments)
  "Draw the popup SHOW puts up with ARGUMENTS on the frame under the field.
Corfu hangs its popup off the frame the field is written in, and lays
it out inside that frame's few rows; here it is hung off that frame's
parent instead, at the same place on the screen."
  (if-let* ((field (corfu-cera--field-frame))
            (parent (frame-parent field))
            (offset (frame-position field)))
      (cl-letf* ((height (symbol-function 'frame-pixel-height))
                 (width (symbol-function 'frame-pixel-width))
                 (window-frame (symbol-function 'window-frame))
                 (make-frame (symbol-function 'corfu--make-frame))
                 ((symbol-function 'frame-pixel-height)
                  (lambda (&optional frame) (funcall height (or frame parent))))
                 ((symbol-function 'frame-pixel-width)
                  (lambda (&optional frame) (funcall width (or frame parent))))
                 ((symbol-function 'window-frame)
                  (lambda (&optional window)
                    (if window (funcall window-frame window) parent)))
                 ((symbol-function 'corfu--make-frame)
                  (lambda (frame x y w h)
                    (funcall make-frame frame
                             (+ x (car offset)) (+ y (cdr offset)) w h))))
        (apply show arguments))
    (apply show arguments)))

(defun corfu-cera--drawn-below ()
  "Return the lines the popup covers below the point, or 0 for none.
Corfu keeps the geometry of the popup it drew on the child frame, so
where it went is read rather than guessed: one drawn above the point
covers what is already behind it, and holding room below would only push
the field, and the popup with it, further down."
  (or (when-let* ((frame (bound-and-true-p corfu--frame))
                  ((frame-live-p frame))
                  ((frame-visible-p frame))
                  (geometry (frame-parameter frame 'corfu--geometry))
                  (position (posn-at-point))
                  (line (default-line-height)))
        (pcase-let* ((`(,_x ,y ,_width ,height) geometry)
                     (point-y (+ (window-pixel-top) (cdr (posn-x-y position))
                                 (if-let* ((field (corfu-cera--field-frame)))
                                     (cdr (frame-position field))
                                   0))))
          (when (> y point-y)
            (min corfu-cera-space (ceiling height line)))))
      0))

(defun corfu-cera--fit (&rest _)
  "Hold room below the field for the popup corfu has just drawn."
  (when (and (bound-and-true-p cera--active)
             (bound-and-true-p completion-in-region-mode))
    (cera-reserve-space (corfu-cera--drawn-below))))

(defun corfu-cera--setup (session)
  "Let Corfu complete in SESSION and hold room for its popup.
The room is asked for in this buffer alone: the reader puts the value
back when the field closes."
  (let ((buffer (current-buffer)))
    (setq-local corfu-cera--session session
                corfu-auto t
                ;; The field's completion supplies its own prefix, so the
                ;; automatic one stays as short as it can be.
                corfu-auto-prefix 1
                corfu-auto-trigger ""
                corfu-quit-at-boundary t
                cera-completion-space 0
                corfu-cera--enabled-before (bound-and-true-p corfu-mode))
    (corfu-mode 1)
    (setq corfu-cera--timer
          (run-at-time 0.1 nil #'corfu-cera--start-completion buffer session))))

(defun corfu-cera--teardown (session)
  "Give back SESSION's popup room and stop its start timer."
  (when (eq session corfu-cera--session)
    (when corfu-cera--timer
      (cancel-timer corfu-cera--timer)
      (setq corfu-cera--timer nil))
    (setq corfu-cera--session nil)
    (when completion-in-region-mode (completion-in-region-mode -1))
    (unless corfu-cera--enabled-before
      (corfu-mode -1))))

(advice-add 'corfu--popup-show :around #'corfu-cera--over-parent)
(advice-add 'corfu--popup-show :after #'corfu-cera--fit)

(add-hook 'cera-session-start-hook #'corfu-cera--setup)
(add-hook 'cera-session-teardown-hook #'corfu-cera--teardown)

(defun corfu-cera-unload-function ()
  "Remove this adapter's session hooks."
  (advice-remove 'corfu--popup-show #'corfu-cera--over-parent)
  (advice-remove 'corfu--popup-show #'corfu-cera--fit)
  (remove-hook 'cera-session-start-hook #'corfu-cera--setup)
  (remove-hook 'cera-session-teardown-hook #'corfu-cera--teardown)
  nil)

(provide 'corfu-cera)
;;; corfu-cera.el ends here
