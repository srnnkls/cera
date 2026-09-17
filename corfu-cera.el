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
                cera-completion-space corfu-cera-space
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

(add-hook 'cera-session-start-hook #'corfu-cera--setup)
(add-hook 'cera-session-teardown-hook #'corfu-cera--teardown)

(defun corfu-cera-unload-function ()
  "Remove this adapter's session hooks."
  (remove-hook 'cera-session-start-hook #'corfu-cera--setup)
  (remove-hook 'cera-session-teardown-hook #'corfu-cera--teardown)
  nil)

(provide 'corfu-cera)
;;; corfu-cera.el ends here
