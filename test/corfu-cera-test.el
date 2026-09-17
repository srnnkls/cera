;;; corfu-cera-test.el --- Tests for the Corfu integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'corfu-cera)
(require 'evil-cera)
(require 'cera-test)



(ert-deftest cera-corfu-keeps-configured-keys-and-return-inserts ()
  "Corfu owns completion keys, while the editor keeps direct save and cancel."
  (skip-unless (featurep 'corfu))
  (dolist (apply-candidate '(nil t))
    (save-window-excursion
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (insert "source\nnext")
        (goto-char 2)
        (let ((corfu-map (copy-keymap corfu-map))
              selected)
          (keymap-set corfu-map "TAB" #'corfu-next)
          (keymap-set corfu-map "C-j" #'corfu-next)
          (keymap-set corfu-map "C-k" #'corfu-previous)
          (keymap-set corfu-map "RET" #'corfu-insert)
          (let ((original-map (copy-keymap corfu-map)))
            (cl-letf (((symbol-function 'corfu--popup-show) #'ignore)
                      ((symbol-function 'corfu--popup-hide) #'ignore))
              (cera-test--reading
                  (lambda ()
                    (should (eq (key-binding (kbd "RET")) #'cera-accept))
                    (should (eq (key-binding (kbd "C-g")) #'cera-cancel))
                    (let ((capf (cera--capf))
                          (completion-in-region-mode-predicate #'always))
                      (apply #'corfu--setup (append (seq-take capf 3) '(nil))))
                    (corfu--exhibit)
                    (should (equal (cera-test--input) "What"))
                    (should (eq (key-binding (kbd "TAB")) #'corfu-next))
                    (should (eq (key-binding (kbd "C-j")) #'corfu-next))
                    (should (eq (key-binding (kbd "C-k")) #'corfu-previous))
                    (should (eq (key-binding (kbd "RET")) #'corfu-insert))
                    (should (eq (key-binding (kbd "C-g")) #'cera-cancel))
                    (call-interactively (key-binding (kbd "TAB")))
                    (setq selected (nth corfu--index corfu--candidates))
                    (when apply-candidate
                      (call-interactively (key-binding (kbd "RET")))
                      (should-not completion-in-region-mode)
                      (should (eq (key-binding (kbd "RET")) #'cera-accept)))
                    (call-interactively (key-binding (kbd "C-c C-c"))))
                (should (equal (cera-read '("What next?" "What now?") "What")
                               (if apply-candidate selected "What")))))
            (should-not (bound-and-true-p corfu-mode))
            (should-not completion-in-region-mode)
            (should (equal original-map corfu-map))
            (should (equal (buffer-string) "source\nnext"))))))))


(ert-deftest cera-restores-preexisting-corfu-configuration ()
  "An existing Corfu setup remains enabled with its original local options."
  (skip-unless (featurep 'corfu))
  (with-temp-buffer
    (insert "source")
    (let ((corfu-auto nil)) (corfu-mode 1))
    (setq-local corfu-auto-prefix 4
                corfu-auto-trigger "."
                corfu-map (copy-keymap corfu-map))
    (keymap-set corfu-map "RET" #'ignore)
    (let ((locals (cera--remember-locals)))
      (cera-test--reading #'cera-accept
        (should (equal (cera-read nil "note") "note")))
      (should (equal locals (cera--remember-locals)))
      (should (bound-and-true-p corfu-mode))
      (should (eq (keymap-lookup corfu-map "RET") #'ignore)))))


(ert-deftest cera-corfu-history-waits-for-manual-completion ()
  "Opening and typing keep history closed until C-SPC requests completion."
  (skip-unless (featurep 'corfu))
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (let ((source-commands 0))
        (add-hook 'post-command-hook (lambda () (cl-incf source-commands)) nil t)
        (cl-letf (((symbol-function 'corfu--popup-support-p) #'always)
                  ((symbol-function 'corfu--popup-show) #'ignore)
                  ((symbol-function 'corfu--popup-hide) #'ignore))
          (cera-test--reading
              (lambda ()
                (let ((this-command 'cera-read))
                  (run-hooks 'post-command-hook))
                (sit-for 0.15)
                (should-not completion-in-region-mode)
                (should (= source-commands 1))
                (should (equal (cera-test--input) "What"))
                (let ((this-command 'self-insert-command)
                      (corfu-auto-delay 0))
                  (insert " n")
                  (run-hooks 'post-command-hook))
                (should-not completion-in-region-mode)
                (should-not (overlay-get
                             (cera--session-spacer cera--active)
                             'after-string))
                (should-not (eq (key-binding (kbd "TAB")) #'completion-at-point))
                (let ((this-command 'completion-at-point))
                  (call-interactively (key-binding (kbd "C-SPC")))
                  (run-hooks 'post-command-hook))
                (should completion-in-region-mode)
                (should (equal (sort (copy-sequence corfu--candidates) #'string<)
                               '("What next?" "What now?")))
                (cera-accept))
            (should (equal (cera-read '("What next?" "What now?") "What")
                           "What n"))))))))

(provide 'corfu-cera-test)