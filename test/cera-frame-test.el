;;; cera-frame-test.el --- Tests for cera-frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cera)
(require 'cera-frame)

(defmacro cera-frame-test--reading (interaction &rest body)
  "Run BODY with INTERACTION standing in for the keyboard, frames held off.
The child frame is never made: what would be laid over the buffer is
left in the child buffer, where INTERACTION finds it as the field."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'recursive-edit) ,interaction)
             ((symbol-function 'exit-recursive-edit) #'ignore)
             ((symbol-function 'cera-frame--window) (lambda () (selected-window)))
             ((symbol-function 'cera-frame--show) #'ignore))
     ,@body))

(defmacro cera-frame-test--should-quit (form)
  "Assert that FORM signals `quit'."
  (declare (indent 0) (debug (form)))
  `(should (eq (condition-case nil (progn ,form 'returned) (quit 'quit)) 'quit)))

(defun cera-frame-test--field-prefixes ()
  "Return the prefixes of the active field's first and last rows."
  (let ((bounds (cera--field-bounds)))
    (list (get-char-property (car bounds) 'line-prefix)
          (get-char-property (save-excursion (goto-char (cdr bounds))
                                             (line-beginning-position))
                             'line-prefix))))

(defun cera-frame-test--source-prefixes (buffer)
  "Return the prefixes drawn on the first two lines of BUFFER."
  (with-current-buffer buffer
    (list (get-char-property 1 'line-prefix)
          (get-char-property (save-excursion (goto-char 1) (forward-line 1) (point))
                             'line-prefix))))

(defun cera-frame-test--decorations (backend)
  "Read a two-line field over a two-line source in BACKEND, returning what was drawn.
The list holds the source prefixes, the field's row prefixes, and the
buffer's text while the field was open."
  (with-temp-buffer
    (insert "alpha\nbeta\nnext\n")
    (let ((cera-input-backend backend)
          (cera-input-prefix "X")
          (buffer (current-buffer))
          drawn)
      (cera-frame-test--reading
          (lambda ()
            (setq drawn (list (cera-frame-test--source-prefixes buffer)
                              (cera-frame-test--field-prefixes)
                              (with-current-buffer buffer (buffer-string))))
            (cera-accept))
        (should (equal (cera-read nil "one\ntwo" (cons 1 11)) "one\ntwo")))
      drawn)))

(ert-deftest cera-frame-draws-the-bytes-the-in-buffer-reader-draws ()
  "The brackets and prefix come out the same whichever reader draws them."
  (pcase-let ((`(,source ,field ,_) (cera-frame-test--decorations 'buffer))
              (`(,frame-source ,frame-field ,_) (cera-frame-test--decorations 'frame)))
    (should (string-suffix-p "╭ " (car source)))
    (should (cl-every #'equal-including-properties source frame-source))
    (should (cl-every #'equal-including-properties field frame-field))))

(ert-deftest cera-frame-leaves-the-buffer-its-lines ()
  "No line goes into the buffer: the rows are held open by an overlay string."
  (pcase-let ((`(,_ ,_ ,text) (cera-frame-test--decorations 'frame)))
    (should (equal text "alpha\nbeta\nnext\n")))
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (let ((cera-input-backend 'frame)
          (cera-space-below 8)
          (buffer (current-buffer))
          held)
      (cera-frame-test--reading
          (lambda ()
            (setq held (with-current-buffer buffer
                         (cl-some (lambda (overlay)
                                    (or (overlay-get overlay 'after-string)
                                        (overlay-get overlay 'before-string)))
                                  (overlays-in (point-min) (point-max)))))
            (cera-accept))
        (cera-read nil "note" (cons 1 6)))
      (should (string-match-p "╰ " held))
      (should (= (get-text-property (1- (length held)) 'line-spacing held) 8)))))

(ert-deftest cera-frame-puts-the-buffer-back-on-accept-and-cancel ()
  "The buffer's overlays, settings and child buffer go with the field."
  (dolist (leave (list #'cera-accept #'cera-cancel))
    (with-temp-buffer
      (insert "alpha\nnext\n")
      (setq-local cursor-in-non-selected-windows 'box)
      (let ((cera-input-backend 'frame)
            (before (buffer-list))
            child)
        (cera-frame-test--reading
            (lambda ()
              (setq child (current-buffer))
              (should cera--active)
              (should (equal (cera-input-text) "note"))
              (funcall leave))
          (if (eq leave #'cera-accept)
              (should (equal (cera-read nil "note" (cons 1 6)) "note"))
            (cera-frame-test--should-quit (cera-read nil "note" (cons 1 6)))))
        (should-not (buffer-live-p child))
        (should (equal (cl-set-difference (buffer-list) before) nil))
        (should-not cera--active)
        (should-not cera-frame--field)
        (should (eq cursor-in-non-selected-windows 'box))
        (should-not (overlays-in (point-min) (point-max)))
        (should (equal (buffer-string) "alpha\nnext\n"))))))

(ert-deftest cera-frame-answers-for-the-buffer-it-was-opened-for ()
  "The start hook and the parent's input API both reach the field.
A consumer holding the buffer it opened the field in finds it as
`cera-origin-buffer' in the child, and reads and writes the input from
the parent."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (let* ((cera-input-backend 'frame)
           (parent (current-buffer))
           origin
           (cera-session-start-hook
            (cons (lambda (_session) (setq origin (cera-origin-buffer)))
                  cera-session-start-hook)))
      (cera-frame-test--reading
          (lambda ()
            (should-not (eq (current-buffer) parent))
            (with-current-buffer parent
              (should (equal (cera-input-text) "note"))
              (should (equal (cera-input-bounds)
                             (with-current-buffer cera--field-buffer
                               (cera--field-bounds))))
              (cera-set-input "more")
              (cera-accept)))
        (should (equal (cera-read nil "note" (cons 1 6)) "more")))
      (should (eq origin parent))
      (should (eq (cera-origin-buffer) parent)))))

(ert-deftest cera-frame-holds-the-parent-s-line-highlight-off ()
  "The buffer the field is opened for loses its line highlight meanwhile."
  (require 'hl-line)
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (hl-line-mode 1)
    (let ((cera-input-backend 'frame)
          (parent (current-buffer)))
      (cera-frame-test--reading
          (lambda ()
            (should-not (buffer-local-value 'hl-line-mode parent))
            (cera-accept))
        (cera-read nil "note" (cons 1 6))))
    (should hl-line-mode)))

(ert-deftest cera-frame-displays-buffers-through-the-parent-s-window ()
  "A buffer displayed from the child goes up from the window the field is over.
The window layout is put back once the field closes."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (let ((cera-input-backend 'frame)
          (parent (current-buffer))
          (shown (generate-new-buffer "shown"))
          (layout (current-window-configuration))
          window)
      (unwind-protect
          (progn
            (cera-frame-test--reading
                (lambda ()
                  (should (eq (car display-buffer-overriding-action)
                              'cera-frame--display-through-parent))
                  (setq window (display-buffer shown))
                  (should (window-live-p window))
                  (should (eq (window-buffer window) shown))
                  (cera-accept))
              (cera-read nil "note" (cons 1 6)))
            (should (compare-window-configurations
                     layout (current-window-configuration)))
            (should-not (get-buffer-window shown)))
        (kill-buffer shown))
      (should (eq (current-buffer) parent)))))

(ert-deftest cera-frame-holds-its-rows-after-the-panes-of-an-empty-first-line ()
  "Rows hang on the newline closing the source, behind any pane drawn before it.
A pane before the source and the rows both hang off an empty first
line; the rows come after its newline, the pane before it."
  (with-temp-buffer
    (insert "\nnext\n")
    (goto-char 1)
    (let ((cera-input-backend 'frame)
          (buffer (current-buffer))
          (cera-read-context-function
           (lambda (panes)
             (cons (cera-pane :id 'above :kind 'readonly :text "a note"
                              :bracket nil :prefix nil)
                   panes)))
          drawn)
      (cera-frame-test--reading
          (lambda ()
            (setq drawn (with-current-buffer buffer
                          (mapcar (lambda (overlay)
                                    (list (overlay-start overlay) (overlay-end overlay)
                                          (and (overlay-get overlay 'before-string) 'before)
                                          (and (overlay-get overlay 'after-string) 'after)))
                                  (seq-filter (lambda (overlay)
                                                (or (overlay-get overlay 'before-string)
                                                    (overlay-get overlay 'after-string)))
                                              (overlays-in (point-min) (point-max))))))
            (cera-accept))
        (cera-read nil "note"))
      (should (member '(1 1 before nil) drawn))
      (should (member '(1 2 nil after) drawn)))))

(ert-deftest cera-frame-takes-any-window-showing-the-buffer ()
  "A buffer shown in a window other than the selected one still gets a frame."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
    (let ((noninteractive nil))
      (with-temp-buffer
        (let ((shown (current-buffer)))
          (with-temp-buffer
            (should-not (cera-frame--window))
            (let ((window (display-buffer shown)))
              (unwind-protect
                  (with-current-buffer shown
                    (should (eq (cera-frame--window) window)))
                (delete-window window)))))))))

(ert-deftest cera-frame-is-placed-again-when-a-pane-above-is-redrawn ()
  "Filling a pane above the source changes what the frame's position depends on."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (let ((cera-input-backend 'frame)
          (buffer (current-buffer))
          (cera-read-context-function
           (lambda (panes)
             (cons (cera-pane :id 'above :kind 'readonly :text ""
                              :bracket nil :prefix nil)
                   panes)))
          (shown (window-buffer (selected-window)))
          before after)
      (set-window-buffer (selected-window) buffer)
      (cera-frame-test--reading
          (lambda ()
            (with-current-buffer buffer
              (let ((field cera-frame--field))
                (setq before (cera-frame--placement-key field))
                (cera-update-pane 'above "filled in")
                (setq after (cera-frame--placement-key field))))
            (cera-accept))
        (cera-read nil "note"))
      (set-window-buffer (selected-window) shown)
      (should before)
      (should-not (equal before after)))))

(ert-deftest cera-frame-is-chosen-by-the-display ()
  "`auto' takes the frame on a graphic display and the buffer on a terminal."
  (let ((cera-input-backend 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
      (let ((noninteractive nil))
        (should (eq (cera--input-backend) #'cera-frame-read-stack))))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
      (let ((noninteractive nil))
        (should (eq (cera--input-backend) #'cera-read-stack-in-buffer)))))
  (let ((cera-input-backend 'buffer))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
      (should (eq (cera--input-backend) #'cera-read-stack-in-buffer)))))

(ert-deftest cera-frame-reads-in-the-buffer-where-no-frame-can-be-shown ()
  "Without a display to lay a frame over, the field goes into the buffer."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (let ((cera-input-backend 'frame)
          (buffer (current-buffer))
          text)
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (setq text (with-current-buffer buffer (buffer-string)))
                   (cera-accept)))
                ((symbol-function 'exit-recursive-edit) #'ignore)
                ((symbol-function 'cera-frame--window) #'ignore))
        (should (equal (cera-read nil "note" (cons 1 6)) "note")))
      (should (equal text "alpha\nnote\nnext\n")))))

(ert-deftest cera-frame-takes-no-panes-below-the-input ()
  "A pane after the input has nowhere to go under a frame."
  (with-temp-buffer
    (insert "alpha\n")
    (cera-frame-test--reading (lambda () (cera-accept))
      (should-error
       (cera-frame-read-stack
        (list (cera-pane :id 'source :kind 'readonly :bounds (cons 1 6))
              (cera-pane :id 'input :kind 'input :text "")
              (cera-pane :id 'after :kind 'readonly :text "below"))
        nil)
       :type 'user-error))
    (should-not cera--active)
    (should-not (overlays-in (point-min) (point-max)))))

(provide 'cera-frame-test)
;;; cera-frame-test.el ends here
