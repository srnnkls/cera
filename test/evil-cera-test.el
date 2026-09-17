;;; evil-cera-test.el --- Tests for the Evil integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'evil-cera)
(require 'corfu-cera)
(require 'cera-test)



(ert-deftest cera-special-mode-field-supports-typing-and-evil-reentry ()
  "Status buffer maps do not suppress text entry, even after leaving insert."
  (dolist (evil '(nil t))
    (when evil (skip-unless (require 'evil nil t)))
    (dolist (cancel '(nil t))
      (save-window-excursion
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (insert "source\nnext")
          (special-mode)
          (use-local-map (copy-keymap (current-local-map)))
          (goto-char 2)
          (let ((source-map (current-local-map))
                (undo buffer-undo-list)
                (evil-insert-state-map
                 (and evil (copy-keymap evil-insert-state-map)))
                result)
            (when evil
              (setq-local emulation-mode-map-alists
                          (cons 'evil-mode-map-alist emulation-mode-map-alists))
              (keymap-set evil-insert-state-map "C-h" #'delete-backward-char)
              (evil-define-key* 'normal source-map
                [remap evil-insert] #'ignore [remap evil-append] #'ignore)
              (evil-local-mode 1)
              (evil-normal-state)
              (should (eq (key-binding "i") #'ignore)))
            (local-set-key [f5]
                           (lambda () (interactive)
                             (condition-case nil
                                 (setq result (cera-read nil))
                               (quit (setq result 'cancelled)))))
            (unwind-protect
                (progn
                  (execute-kbd-macro
                   (vconcat (if evil [f5 ?a ?b ?\C-h ?c escape ?a ?d escape ?i ?x]
                              [f5 ?a ?c ?d])
                            (if cancel [?\C-g] [return])))
                  (should (equal result (cond (cancel 'cancelled) (evil "acxd") (t "acd"))))
                  (should (eq (current-local-map) source-map))
                  (should (eq major-mode 'special-mode))
                  (should buffer-read-only)
                  (should (eq undo buffer-undo-list))
                  (should (equal (buffer-string) "source\nnext"))
                  (when evil
                    (should (eq evil-state 'normal))
                    (should (eq (key-binding "i") #'ignore))))
              (when evil (evil-local-mode -1)))))))))


(ert-deftest cera-evil-inherits-insert-keys-and-restores-state ()
  "Evil insert bindings work, ESC changes state, and C-g cancels the reader."
  (skip-unless (require 'evil nil t))
  (dolist (cancel '(nil t))
    (save-window-excursion
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (insert "source\nnext")
        (buffer-enable-undo)
        (goto-char 2)
        (let ((evil-insert-state-map (copy-keymap evil-insert-state-map))
              (undo buffer-undo-list)
              result)
          (setq-local emulation-mode-map-alists
                      (cons 'evil-mode-map-alist emulation-mode-map-alists))
          (keymap-set evil-insert-state-map "C-h" #'delete-backward-char)
          (evil-local-mode 1)
          (evil-normal-state)
          (local-set-key [f5]
                         (lambda () (interactive)
                           (condition-case nil
                               (setq result (cera-read nil))
                             (quit (setq result 'cancelled)))))
          (unwind-protect
              (progn
                (execute-kbd-macro
                 (vconcat [f5 ?a ?b ?\C-h ?c escape ?a ?d]
                          (if cancel [?\C-g] [return])))
                (should (equal result (if cancel 'cancelled "acd")))
                (should (eq evil-state 'normal))
                (should (eq undo buffer-undo-list))
                (should (equal (buffer-string) "source\nnext")))
            (evil-local-mode -1)))))))


(ert-deftest cera-evil-corfu-keyboard-inserts-then-saves ()
  "RET completes and then saves in a real Evil and Corfu command loop."
  (skip-unless (and (featurep 'corfu) (require 'evil nil t)))
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (setq-local emulation-mode-map-alists
                  (cons 'evil-mode-map-alist emulation-mode-map-alists))
      (let ((evil-insert-state-map (copy-keymap evil-insert-state-map))
            (corfu-map (copy-keymap corfu-map))
            (corfu-auto-delay 0)
            result)
        (keymap-set evil-insert-state-map "C-h" #'delete-backward-char)
        (keymap-set corfu-map "TAB" #'corfu-next)
        (keymap-set corfu-map "RET" #'corfu-insert)
        (evil-local-mode 1)
        (evil-normal-state)
        (local-set-key [f5]
                       (lambda () (interactive)
                         (setq result
                               (cera-read '("What next?" "What now?") "What"))))
        (local-set-key [f6]
                       (lambda () (interactive)
                         (should completion-in-region-mode)
                         (should (eq (key-binding (kbd "C-h") nil t)
                                     #'delete-backward-char))
                         (should (eq (key-binding (kbd "C-g")) #'cera-cancel))
                         (should (eq (key-binding (kbd "TAB")) #'corfu-next))
                         (should (eq (key-binding (kbd "RET")) #'corfu-insert))))
        (unwind-protect
            (cl-letf (((symbol-function 'corfu--popup-support-p) #'always)
                      ((symbol-function 'corfu--popup-show) #'ignore)
                      ((symbol-function 'corfu--popup-hide) #'ignore))
              (execute-kbd-macro
               (vconcat [f5 ?x ?\C-h] (kbd "C-SPC") [f6 tab return return]))
              (should (member result '("What next?" "What now?")))
              (should-not completion-in-region-mode)
              (should (eq evil-state 'normal))
              (should (equal (buffer-string) "source\nnext")))
          (evil-local-mode -1))))))

(ert-deftest cera-evil-a-field-opened-from-visual-comes-back-to-normal ()
  "A field opened over a selection leaves normal state, not visual.
The field is opened for the selection and consumes it, the way an
operator does, so the selection is gone by the time the field closes."
  (skip-unless (require 'evil nil t))
  (dolist (cancel '(nil t))
    (save-window-excursion
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (insert "alpha beta\nnext")
        (goto-char 1)
        (unwind-protect
            (progn
              (evil-local-mode 1)
              (evil-normal-state)
              (evil-visual-state)
              (goto-char 5)
              (should (eq evil-state 'visual))
              (cera-test--reading
                  (lambda ()
                    (should (eq evil-state 'insert))
                    (if cancel (cera-cancel) (cera-accept)))
                (condition-case nil (cera-read nil "note") (quit nil)))
              (should (eq evil-state 'normal))
              (should (equal (buffer-string) "alpha beta\nnext")))
          (evil-local-mode -1))))))

(ert-deftest cera-evil-dd-clears-the-last-line-of-the-field ()
  "`dd' on the field's last line empties it, as it does on the lines above."
  (skip-unless (require 'evil nil t))
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (setq-local emulation-mode-map-alists
                  (cons 'evil-mode-map-alist emulation-mode-map-alists))
      (evil-local-mode 1)
      (evil-normal-state)
      (let (result)
        (local-set-key [f5] (lambda () (interactive) (setq result (cera-read nil "one\ntwo"))))
        (execute-kbd-macro [f5 escape ?d ?d ?k ?d ?d return])
        (should (equal result ""))
        (should (equal (buffer-string) "source\nnext"))))))

(ert-deftest cera-adapters-compose-in-either-load-order-and-clean-up-on-errors ()
  "Adapter locals are additive and all teardown hooks run after an error."
  (dolist (order '(("evil-cera" "corfu-cera") ("corfu-cera" "evil-cera")))
    (let ((cera-borrowed-locals '(fill-column))
          (cera-session-start-hook nil)
          (cera-session-teardown-hook nil)
          (cera-session-restored-hook nil))
      (mapc #'load order)
      (dolist (symbol '(fill-column evil-previous-state evil-previous-state-alist
                                    evil-next-state corfu-auto corfu-auto-prefix
                                    corfu-auto-trigger corfu-quit-at-boundary))
        (should (memq symbol (default-value 'cera-borrowed-locals))))
      (with-temp-buffer
        (insert "source\nnext")
        (goto-char 2)
        (evil-local-mode 1)
        (evil-normal-state)
        (setq-local corfu-auto nil corfu-auto-prefix 9
                    corfu-auto-trigger "!" corfu-quit-at-boundary nil)
        (let ((bindings (cera--remember-locals))
              (enabled (bound-and-true-p corfu-mode)))
          (add-hook 'cera-session-teardown-hook
                    (lambda (_session) (error "Teardown failure")) -90)
          (cera-test--reading
              (lambda ()
                (should (eq evil-state 'insert))
                (should corfu-auto)
                (cera-accept))
            (should-error
             (cera-read-stack (list (cera-pane :id 'input :kind 'input :text "draft")) nil)
             :type 'error))
          (should (equal bindings (cera--remember-locals)))
          (should (eq evil-state 'normal))
          (should (eq (bound-and-true-p corfu-mode) enabled))
          (should-not corfu-cera--timer)
          (should (equal (buffer-string) "source\nnext"))
          (should-not cera--active))))))

(provide 'evil-cera-test)
;;; evil-cera-test.el ends here
