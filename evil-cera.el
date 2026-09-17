;;; evil-cera.el --- Evil integration for cera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/cera
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (cera "0.1.0") (evil "1.14.0"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `evil-cera' writes into a cera field under Evil, entering insert state
;; for it and putting Evil's state back afterwards.

;;; Code:

(require 'evil)
(require 'cera)

(cera-register-borrowed-locals
 'evil-previous-state 'evil-previous-state-alist 'evil-next-state
 'evil-cera--state-before 'evil-cera--session)

(defvar-local evil-cera--state-before nil
  "Evil state preceding the current Cera session.")

(defvar-local evil-cera--session nil
  "Session whose Evil state this adapter owns.")

(defun evil-cera--setup (session)
  "Normalize Evil's keymaps for SESSION and start inserting into it."
  (when (bound-and-true-p evil-local-mode)
    (setq evil-cera--state-before evil-state
          evil-cera--session session)
    (evil-normalize-keymaps)
    (unless (eq evil-state 'emacs)
      (evil-insert-state))))

(defun evil-cera--restore-locals (&rest _)
  "Normalize Evil's keymaps once the borrowed local map is back."
  (when (bound-and-true-p evil-local-mode)
    (evil-normalize-keymaps)))

(defun evil-cera--state-after (state)
  "Return the state to come back to after a field opened from STATE.
Visual answers normal: the field is opened for the selection and consumes
it, the way an operator does, so coming back to visual would leave a
selection the field has already taken."
  (if (memq state '(visual operator)) 'normal state))

(defun evil-cera--teardown (session)
  "Restore the Evil state preceding the closing SESSION."
  (when (and (eq session evil-cera--session)
             (bound-and-true-p evil-local-mode) evil-cera--state-before)
    (evil-change-state (evil-cera--state-after evil-cera--state-before))))

(add-hook 'cera-session-start-hook #'evil-cera--setup)
(add-hook 'cera-session-teardown-hook #'evil-cera--teardown)
(add-hook 'cera-session-restored-hook #'evil-cera--restore-locals)

(defun evil-cera-unload-function ()
  "Remove this adapter's session hooks."
  (remove-hook 'cera-session-start-hook #'evil-cera--setup)
  (remove-hook 'cera-session-teardown-hook #'evil-cera--teardown)
  (remove-hook 'cera-session-restored-hook #'evil-cera--restore-locals)
  nil)

(provide 'evil-cera)
;;; evil-cera.el ends here
