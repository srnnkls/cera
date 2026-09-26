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
                         (cl-find-if (lambda (overlay)
                                       (overlay-get overlay 'before-string))
                                     (overlays-in (point-min) (point-max)))))
            (setq held (list (overlay-start held)
                             (overlay-get held 'before-string)
                             (overlay-get held 'line-spacing)))
            (cera-accept))
        (cera-read nil "note" (cons 1 6)))
      ;; Before the newline closing the source line, which closes the last row.
      (should (= (nth 0 held) 6))
      (should (string-match-p "\\`\n.*╰ \\'" (nth 1 held)))
      (should (= (nth 2 held) 8)))))

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
      (should (member '(1 2 before nil) drawn)))))

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

;; A supplied pane under the input is drawn in the buffer under the rows
;; the frame covers, on the buffer's ground; a bounded one stays on its lines.
(ert-deftest cera-frame-draws-the-panes-under-the-input-below-its-rows ()
  "Panes follow the input in any order, drawn below the rows the frame is laid over."
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((buffer (current-buffer))
          (shown (window-buffer (selected-window)))
          child-ids parent-ids held bracketed)
      (set-window-buffer (selected-window) buffer)
      (cera-frame-test--reading
          (lambda ()
            (let* ((field (buffer-local-value 'cera-frame--field buffer))
                   (child (cera-frame--field-child field)))
              (with-current-buffer child
                (setq child-ids (mapcar #'cera-pane-id
                                        (cera--session-panes cera--active))))
              (with-current-buffer buffer
                (setq parent-ids (mapcar #'cera-pane-id
                                         (cera--session-panes cera--active)))
                (cera-update-pane 'under "shown under")
                (setq held (overlay-get (cera-frame--field-holder field) 'before-string)
                      bracketed (get-char-property 13 'line-prefix))))
            (cera-accept))
        (should
         (equal (cera-frame-read-stack
                 (list (cera-pane :id 'source :kind 'readonly :bounds (cons 1 6))
                       (cera-pane :id 'input :kind 'input :text "note")
                       (cera-pane :id 'under :kind 'readonly :text ""
                                  :bracket nil)
                       (cera-pane :id 'later :kind 'readonly :bounds (cons 13 18)))
                 nil)
                "note")))
      (set-window-buffer (selected-window) shown)
      (should (equal child-ids '(input)))
      (should (equal parent-ids '(source input later)))
      (should (string-match-p "╰ \n\\(.\\|\n\\)*shown under\\'" held))
      (should-not (get-text-property (string-search "shown" held) 'face held))
      (should bracketed))
    (should-not cera--active)
    (should-not (overlays-in (point-min) (point-max)))))

;; An empty buffer hangs the panes above the input and the rows under them
;; off the same position, where only priority puts one before the other.
(ert-deftest cera-frame-hangs-its-rows-after-the-panes-of-an-empty-buffer ()
  "The rows, and the panes under them, come after the panes above the input."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (shown (window-buffer (selected-window)))
          holder panes under-rows)
      (set-window-buffer (selected-window) buffer)
      (cera-frame-test--reading
          (lambda ()
            (with-current-buffer buffer
              (let ((field cera-frame--field))
                (setq holder (let ((overlay (cera-frame--field-holder field)))
                               (cons (overlay-start overlay)
                                     (overlay-get overlay 'priority)))
                      under-rows (cera-frame--field-under-rows field)
                      panes (mapcar (lambda (overlay)
                                      (cons (overlay-start overlay)
                                            (overlay-get overlay 'priority)))
                                    (seq-filter
                                     (lambda (overlay) (overlay-get overlay 'before-string))
                                     (cera--session-static-overlays cera--active))))))
            (cera-accept))
        (cera-frame-read-stack
         (list (cera-pane :id 'above :kind 'readonly :text "a note" :bracket nil)
               (cera-pane :id 'input :kind 'input :text "note")
               (cera-pane :id 'under :kind 'readonly :text "status" :bracket nil))
         nil))
      (set-window-buffer (selected-window) shown)
      (should panes)
      (dolist (pane panes)
        (should (= (car pane) (car holder)))
        (should (> (cdr holder) (cdr pane))))
      (should (= under-rows 1)))))

;; A dashboard redraws itself around an open field, moving the lines the
;; source is on; the brackets are redrawn over the lines, not the offsets.
(ert-deftest cera-frame-keeps-the-source-when-the-buffer-shifts-under-it ()
  "Text put in above the source moves its bracket with it."
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((buffer (current-buffer))
          prefixes)
      (cera-frame-test--reading
          (lambda ()
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-min))
                (let ((inhibit-read-only t)) (insert "\n\n")))
              (cera--draw-static cera--active)
              (setq prefixes
                    (mapcar (lambda (line)
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line line)
                                (let ((prefix (get-char-property (point) 'line-prefix)))
                                  (and prefix (substring-no-properties prefix)))))
                            '(2 3 4))))
            (cera-accept))
        (cera-frame-read-stack
         (list (cera-pane :id 'source :kind 'readonly :bounds (cons 7 11)
                          :connection 'next)
               (cera-pane :id 'input :kind 'input :text "" :connection 'previous))
         nil))
      (should-not (nth 0 prefixes))
      (should (string-match-p "╭" (nth 1 prefixes)))
      (should-not (nth 2 prefixes)))))

(defun cera-frame-test--scroll-window (enabled)
  "Return what the field's child answers for the other window, ENABLED or not."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (let ((cera-input-backend 'frame)
          (cera-frame-scroll-parent enabled)
          (buffer (current-buffer))
          (shown (window-buffer (selected-window)))
          scrolled)
      (set-window-buffer (selected-window) buffer)
      (cera-frame-test--reading
          (lambda ()
            (let ((field (buffer-local-value 'cera-frame--field buffer)))
              (with-current-buffer (cera-frame--field-child field)
                (setq scrolled (cons other-window-scroll-default
                                     (cera-frame--field-window field)))))
            (cera-accept))
        (cera-read nil "note"))
      (set-window-buffer (selected-window) shown)
      scrolled)))

(ert-deftest cera-frame-leaves-the-other-window-alone-unless-asked ()
  "The lookup stands as Emacs makes it until `cera-frame-scroll-parent'."
  (should-not (car (cera-frame-test--scroll-window nil))))

(ert-deftest cera-frame-scrolls-the-window-it-is-read-over ()
  "\"The other window\" from inside the field is the buffer it was opened for."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (let ((cera-input-backend 'frame)
          (cera-frame-scroll-parent t)
          (buffer (current-buffer))
          (shown (window-buffer (selected-window)))
          scrolled over)
      (set-window-buffer (selected-window) buffer)
      (cera-frame-test--reading
          (lambda ()
            (let ((field (buffer-local-value 'cera-frame--field buffer)))
              (setq over (cera-frame--field-window field))
              (with-current-buffer (cera-frame--field-child field)
                (setq scrolled (funcall other-window-scroll-default))))
            (cera-accept))
        (cera-read nil "note"))
      (set-window-buffer (selected-window) shown)
      (should (window-live-p scrolled))
      (should (eq scrolled over)))))

(ert-deftest cera-frame-counts-a-header-line-once ()
  "The rows are counted from the window's top, which a header line is part of.
`pos-visible-in-window-p' reports the first text row at the header
line's height, so that offset taken from the window's top is the text
area itself rather than a row below it."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (setq header-line-format " header")
    (let ((shown (window-buffer (selected-window))))
      (set-window-buffer (selected-window) (current-buffer))
      (unwind-protect
          (let* ((window (selected-window))
                 (inside (nth 1 (window-inside-pixel-edges window)))
                 (header (- inside (nth 1 (window-pixel-edges window)))))
            (should (> header 0))
            (should (= (cera-frame--row-top window header) inside)))
        (set-window-buffer (selected-window) shown)))))

(ert-deftest cera-frame-marks-its-frame-against-the-workspace-managers ()
  "The frame carries the flag telling a workspace manager to leave it alone.
A parameter named in `cera-frame-parameters' is the one the frame is
made with, whatever the default beneath it says."
  (let ((parameters (cera-frame--parameters (selected-frame) "#000000")))
    (should (eq (alist-get 'persp-ignore-wconf parameters) t))
    (should (eq (alist-get 'parent-frame parameters) (selected-frame)))
    (should (eq (alist-get 'unsplittable parameters) t)))
  (let* ((cera-frame-parameters '((unsplittable . nil)))
         (parameters (cera-frame--parameters (selected-frame) "#000000")))
    (should-not (alist-get 'unsplittable parameters)))
  ;; The frame's fringes are the window's, so both centre a line alike.
  (let ((parameters (cera-frame--parameters (selected-frame) "#000000" '(0 0))))
    (should (eql (alist-get 'left-fringe parameters) 0))
    (should (eql (alist-get 'right-fringe parameters) 0)))
  (let ((parameters (cera-frame--parameters (selected-frame) "#000000" '(8 8))))
    (should (eql (alist-get 'right-fringe parameters) 8))))

(ert-deftest cera-frame-gives-back-a-frame-another-command-took ()
  "A buffer laid over the input releases the field and goes to the window below.
The field is cancelled as it is for a command taking the buffer whole:
what was written is on the kill ring, and the buffer the command meant
to show is in the window the field was read over."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (let ((cera-input-backend 'frame)
          (shown (generate-new-buffer "shown"))
          (layout (current-window-configuration))
          (kill-ring nil))
      (unwind-protect
          (progn
            (cera-frame-test--should-quit
              (cera-frame-test--reading
                  (lambda ()
                    (insert "typed")
                    (cl-letf (((symbol-function 'cera-frame--taken-p)
                               (lambda (_field) shown)))
                      (cera-frame--reclaim))
                    (should-not cera--active)
                    (should (equal (current-kill 0) "typed"))
                    (should (eq (window-buffer (selected-window)) shown)))
                (cera-read nil)))
            (should-not cera-frame--fields)
            (should-not (memq #'cera-frame--reclaim
                              window-configuration-change-hook)))
        (set-window-configuration layout)
        (kill-buffer shown)))))

(defvar cera-frame-test--focused nil
  "The frames the keyboard was given to, newest first, while a key was routed.")

(defun cera-frame-test--routed (key command &optional local)
  "Return what `cera-frame--host-command' makes of KEY running COMMAND in a field.
COMMAND is bound to KEY globally, and LOCAL, when given, over it in the
child.  The answer is the command that runs, whether the host window is
selected once it has, and the frames the keyboard was given to, in the
order it went to them.  The frame the field stands in is the one the
test runs on, so what the command reaches is a live frame."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (let ((cera-input-backend 'frame)
          (host (selected-window))
          (parent (current-buffer))
          (global (current-global-map))
          (map (make-composed-keymap nil (current-global-map)))
          ran)
      (setq cera-frame-test--focused nil)
      (define-key map key command)
      (use-global-map map)
      (unwind-protect
          (cera-frame-test--reading
              (lambda ()
                (let ((field (buffer-local-value 'cera-frame--field parent)))
                  (with-current-buffer (cera-frame--field-child field)
                    (when local
                      (let ((own (make-sparse-keymap)))
                        (define-key own key local)
                        (use-local-map (make-composed-keymap own (current-local-map)))))
                    (let ((this-command (key-binding key)))
                      (cl-letf (((symbol-function 'this-command-keys-vector)
                                 (lambda () key))
                                ((symbol-function 'cera-frame--taken-p) #'ignore)
                                ((symbol-function 'select-frame-set-input-focus)
                                 (lambda (frame &rest _)
                                   (push frame cera-frame-test--focused))))
                        (should (memq #'cera-frame--host-command pre-command-hook))
                        (cera-frame--host-command)
                        (setf (cera-frame--field-frame field) (selected-frame))
                        (unwind-protect
                            (setq ran (list this-command
                                            (and (not (symbolp this-command))
                                                 (progn (funcall this-command)
                                                        (selected-window)))))
                          (setf (cera-frame--field-frame field) nil))))))
                (cera-accept))
            (cera-read nil "note"))
        (use-global-map global))
      (list (car ran) (and (cadr ran) (eq (cadr ran) host))
            (reverse cera-frame-test--focused)))))

(ert-deftest cera-frame-runs-a-super-binding-from-the-window-it-is-read-over ()
  "A global key on a host modifier acts from the host window, as in the buffer."
  (let* ((ran nil)
         (command (lambda () (interactive) (setq ran (selected-window))))
         (routed (cera-frame-test--routed [?\s-j] command)))
    (should (functionp (car routed)))
    (should-not (eq (car routed) command))
    (should (cadr routed))
    (should (windowp ran))))

(ert-deftest cera-frame-hands-the-host-the-keyboard-when-the-command-reads ()
  "A host command opening the minibuffer gives the host's frame the keyboard.
The minibuffer opens on that frame, and the keys typed into it go to
whichever frame the window system points at; the field takes the
keyboard back once the command is done."
  (let* ((during nil)
         (command (lambda ()
                    (interactive)
                    (run-hooks 'minibuffer-setup-hook)
                    (setq during (reverse cera-frame-test--focused))))
         (routed (cera-frame-test--routed [?\s-l] command)))
    (should (equal during (list (selected-frame))))
    (should (equal (nth 2 routed) (list (selected-frame) (selected-frame))))))

(ert-deftest cera-frame-keeps-the-keyboard-through-a-host-command-that-stays ()
  "A host command acting on the host window alone never moves the keyboard.
Each move switches the window system's input method, and a held key
repeating that crashes the macOS port."
  (let* ((host (selected-window))
         (ran nil)
         (command (lambda () (interactive) (setq ran (selected-window))))
         (routed (cera-frame-test--routed [?\s-l] command)))
    (should (eq ran host))
    (should (cadr routed))
    (should-not (nth 2 routed))))

(ert-deftest cera-frame-takes-the-keyboard-back-from-a-command-that-quits ()
  "A host command left by `keyboard-quit' in its minibuffer hands the keyboard back."
  (let ((command (lambda ()
                   (interactive)
                   (run-hooks 'minibuffer-setup-hook)
                   (signal 'quit nil))))
    (cera-frame-test--should-quit (cera-frame-test--routed [?\s-l] command))
    (should (= (length cera-frame-test--focused) 2))))

(ert-deftest cera-frame-gives-the-keyboard-where-a-host-command-went ()
  "A host command selecting another window leaves the keyboard with it."
  (let* ((layout (current-window-configuration))
         (command (lambda () (interactive) (select-window (split-window))))
         (routed (unwind-protect (cera-frame-test--routed [?\s-j] command)
                   (set-window-configuration layout))))
    (should-not (cadr routed))
    (should (equal (nth 2 routed) (list (selected-frame))))))

(ert-deftest cera-frame-keeps-writing-and-local-keys-in-the-field ()
  "Text-writing, unmodified and locally bound keys stay with the field."
  (should (eq (car (cera-frame-test--routed [?\s-y] #'yank)) #'yank))
  (should (eq (car (cera-frame-test--routed [?\C-x ?9] #'ignore)) #'ignore))
  (should (eq (car (cera-frame-test--routed [?\s-j] #'ignore #'beginning-of-line))
              #'beginning-of-line)))

(ert-deftest cera-frame-takes-the-keyboard-back-when-its-window-is-selected ()
  "Selecting the window a field is read over hands the keyboard to the field."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (let ((cera-input-backend 'frame)
          focused)
      (cera-frame-test--reading
          (lambda ()
            (should (memq #'cera-frame--return window-selection-change-functions))
            (let ((field (car cera-frame--fields)))
              (setf (cera-frame--field-frame field) 'child-frame)
              (cl-letf (((symbol-function 'frame-live-p) (lambda (frame) (eq frame 'child-frame)))
                        ((symbol-function 'frame-root-window) (lambda (_) (selected-window)))
                        ((symbol-function 'select-frame-set-input-focus)
                         (lambda (frame) (setq focused frame))))
                (cl-letf (((symbol-function 'run-at-time)
                           (lambda (_time _repeat function &rest args)
                             (apply function args))))
                  (save-current-buffer (cera-frame--return)))))
            (cera-accept))
        (cera-read nil "note"))
      (should (eq focused 'child-frame))
      (should-not (memq #'cera-frame--return window-selection-change-functions)))))

(ert-deftest cera-frame-input-reaches-as-far-as-the-panes-stacked-with-it ()
  "The widest supplied pane sets how far the input reaches, at the least.
A pane of the document and one hidden empty take no part in it."
  (with-temp-buffer
    (insert "source\n")
    (let* ((cera-input-prefix nil)
           (status (cera-pane :id 'status :kind 'readonly :bracket nil
                              :align 'input :wrap nil
                              :text (make-string 120 ?s)))
           (context (cera-pane :id 'context :kind 'readonly :bracket nil
                               :text "short"))
           (hidden (cera-pane :id 'hidden :kind 'readonly :text ""))
           (source (cera-pane :id 'source :kind 'readonly
                              :bounds (cons (point-min) (point-max))))
           (input (cera-pane :id 'input :kind 'input))
           (field (cera-frame--make-field
                   :input input :under (list status)
                   :session (cera--make-session
                             :panes (list context source hidden input)))))
      (should (= (cera--pane-reach status 200)
                 (+ (cera--text-column) 120)))
      (should (= (cera--pane-reach context 200) 5))
      (should (= (cera-frame--stacked-reach field 200)
                 (+ (cera--text-column) 120)))
      (setf (cera-frame--field-under field) nil)
      (should (= (cera-frame--stacked-reach field 200) 5)))))

(provide 'cera-frame-test)
;;; cera-frame-test.el ends here
