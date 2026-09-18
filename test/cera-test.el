;;; cera-test.el --- Tests for cera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cera)

(when (require 'corfu nil t) (require 'corfu-auto nil t))

(defvar corfu-auto)
(defvar corfu-auto-delay)
(defvar corfu-auto-trigger)
(defvar corfu-map)
(defvar corfu--index)
(defvar corfu--candidates)
(defvar corfu-count)
(defvar corfu-min-width)
(defvar corfu-max-width)
(defvar evil-state)
(defvar evil-insert-state-map)
(defvar evil-mode-map-alist)
(declare-function corfu-mode "ext:corfu" (&optional arg))
(declare-function corfu-next "ext:corfu" (&optional n))
(declare-function corfu-previous "ext:corfu" (&optional n))
(declare-function corfu-insert "ext:corfu" ())
(declare-function corfu-quit "ext:corfu" ())
(declare-function corfu--setup "ext:corfu" (beg end table pred))
(declare-function corfu--exhibit "ext:corfu" ())
(declare-function evil-local-mode "ext:evil-core" (&optional arg))
(declare-function evil-normal-state "ext:evil-states" (&optional arg))
(declare-function evil-define-key* "ext:evil-core" (state keymap key def &rest bindings))

(defun cera-test--input ()
  "Return the active reader's literal input."
  (buffer-substring-no-properties
   (cera--session-begin cera--active)
   (cera--session-end cera--active)))

(defmacro cera-test--reading (interaction &rest body)
  "Run BODY with INTERACTION standing in for recursive keyboard input."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'recursive-edit) ,interaction)
             ((symbol-function 'exit-recursive-edit) #'ignore))
     ,@body))

(defmacro cera-test--should-quit (form)
  "Assert that FORM signals `quit', which is not an error condition."
  (declare (indent 0) (debug (form)))
  `(should (eq (condition-case nil (progn ,form 'returned) (quit 'quit)) 'quit)))

(defun cera-test--reserved-lines ()
  "Return the lines the active field is holding free, or nil for none."
  (when-let* ((spacer (cera--session-spacer cera--active))
              (text (overlay-get spacer 'after-string)))
    (length text)))

(defun cera-test--source-ranges ()
  "Return the sorted ranges visibly underlined by the active input reader."
  (sort (cl-loop for overlay in (overlays-in (point-min) (point-max))
                 when (eq (overlay-get overlay 'face) 'cera-source)
                 collect (cons (overlay-start overlay) (overlay-end overlay)))
        (lambda (a b) (< (car a) (car b)))))

(defun cera-test--draw-allocations ()
  "Return how many overlays one redraw of the active field allocates."
  (let ((calls 0)
        (original (symbol-function 'make-overlay)))
    (cl-letf (((symbol-function 'make-overlay)
               (lambda (&rest arguments)
                 (setq calls (1+ calls))
                 (apply original arguments))))
      (cera--draw))
    calls))

(ert-deftest cera-redraw-allocation-is-independent-of-field-length ()
  "A redraw allocates no more overlays for a long field than a short one.
Every line of the field is bracketed the same way but its last, so the
decoration is bounded whatever the field runs to, and a keystroke in a
long field costs what a keystroke in a short one does."
  (let ((allocations nil))
    (dolist (lines '(2 20))
      (with-temp-buffer
	(insert "alpha\nbravo\n")
	(goto-char 1)
	(let ((calls 0))
	  (cera-test--reading
	      (lambda ()
		(dotimes (_ lines) (insert "word word word\n"))
		(setq calls (cera-test--draw-allocations))
		(cera-cancel))
	    (condition-case nil (cera-read nil "") (quit nil)))
	  (push calls allocations))))
    (should (= (nth 0 allocations) (nth 1 allocations)))))

(ert-deftest cera-keeps-the-point-a-buffer-puts-back-where-it-wants-it ()
  "A buffer holding the point after every command does not hold the field's.
A dashboard repositions the point on `post-command-hook'; the field puts
it back into what is being written, and shows a cursor where the buffer
had hidden one."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (setq-local cursor-type nil)
    (add-hook 'post-command-hook (lambda () (goto-char (point-min))) 50 t)
    (goto-char 2)
    (cera-test--reading
	(lambda ()
	  (should (eq cursor-type t))
	  (insert "note")
	  (let ((bounds (cera--field-bounds)))
	    (run-hook-wrapped 'post-command-hook
			      (lambda (function)
				(when (functionp function) (funcall function))
				nil))
	    (should (equal (cera--field-bounds) bounds))
	    (should (= (point) (cdr bounds))))
	  (should (equal (cera-test--input) "note"))
	  (cera-accept))
      (should (equal (cera-read nil "") "note")))
    (should-not cursor-type)
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest cera-follows-the-prefix-the-lines-it-brackets-are-drawn-behind ()
  "A buffer centring its text is followed there, bracket, field and all.
A dashboard puts a display spec in `line-prefix' where another buffer
puts a string; the field is drawn behind the same one, so it stands
under the text it belongs to instead of at the window's edge."
  (let ((centred '(space :align-to (- center 4))))
    (with-temp-buffer
      (insert (propertize "centred\n" 'line-prefix centred))
      (insert "next\n")
      (goto-char 2)
      (cera-test--reading
	  (lambda ()
	    (let ((source (get-char-property 1 'line-prefix))
		  (field (get-char-property (point) 'line-prefix)))
	      (should (equal (get-text-property 0 'display source) centred))
	      (should (string-suffix-p "╭ " source))
	      (should (equal (get-text-property 0 'display field) centred))
	      (should (string-suffix-p "╰ " field)))
	    (cera-accept))
	(should (equal (cera-read nil "note") "note")))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest cera-previews-exact-selection-from-its-first-line ()
  "Partial words and multiline selections are previewed before any input."
  (dolist (fixture '(("before words after\nnext\n" (8 . 13)
		      ((8 . 13)) "before words after\nnote\nnext\n" (1))
		     ("first words\nmiddle text\nlast words\nnext\n" (7 . 29)
		      ((7 . 12) (13 . 24) (25 . 29))
		      "first words\nmiddle text\nlast words\nnote\nnext\n" (1 13 25))
		     ("first words\nmiddle text\nlast words\nnext\n" (7 . 25)
		      ((7 . 12) (13 . 24))
		      "first words\nmiddle text\nnote\nlast words\nnext\n" (1 13))))
    (pcase-let ((`(,text ,bounds ,ranges ,expected ,lines) fixture))
      (dolist (reverse '(nil t))
	(with-temp-buffer
	  (insert text)
	  (let ((transient-mark-mode t))
	    (goto-char (if reverse (car bounds) (cdr bounds)))
	    (set-mark (if reverse (cdr bounds) (car bounds)))
	    (setq mark-active t)
	    (cera-test--reading
		(lambda ()
		  (should (equal (buffer-string) expected))
		  (should (equal (cera-test--source-ranges) ranges))
		  (dolist (line lines)
		    (should (equal (get-char-property line 'line-prefix)
				   (if (= line 1) "╭ " "│ "))))
		  (should (equal (get-char-property (point) 'line-prefix) "╰ "))
		  (insert " extra")
		  (should (equal (cera-test--source-ranges) ranges))
		  (cera-accept))
	      (should (equal (cera-read nil "note") "note extra")))
	    (should (equal (buffer-string) text))
	    (should-not (overlays-in (point-min) (point-max)))))))))

(ert-deftest cera-save-restores-source-and-borrowed-state ()
  "A real input region rolls back without touching source, hooks, or undo."
  (dolist (readonly '(nil t))
    (dolist (undo-enabled '(nil t))
      (with-temp-buffer
	(insert (propertize "alpha\nnext\n" 'face 'bold))
	(when undo-enabled
	  (buffer-enable-undo)
	  (insert "last"))
	(goto-char 2)
	(set-mark 4)
	(setq mark-active t)
	(let* ((original (buffer-string))
	       (undo buffer-undo-list)
	       (modified (buffer-modified-p))
	       (existing (list (make-overlay 1 6) (make-overlay 7 (point-max))))
	       (ranges (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
			       existing))
	       (changes 0)
	       (before (lambda (&rest _) (cl-incf changes)))
	       (after (lambda (&rest _) (cl-incf changes))))
	  (setq-local buffer-read-only readonly
		      before-change-functions (list before)
		      after-change-functions (list after))
	  (let ((locals (cera--remember-locals)))
	    (cera-test--reading
		(lambda ()
		  (should (equal (cera-test--input) "initial"))
		  (insert " text\nsecond line")
		  (should (equal (cera-test--input) "initial text\nsecond line"))
		  (should (= (line-number-at-pos
			      (save-excursion (search-forward "next") (point)))
			     4))
		  (cera-accept))
	      (should (equal (cera-read '("initial candidate") "initial")
			     "initial text\nsecond line")))
	    (should (equal locals (cera--remember-locals))))
	  (should (equal-including-properties original (buffer-string)))
	  (should (eq undo buffer-undo-list))
	  (should (eq modified (buffer-modified-p)))
	  (should (= (point) 2))
	  (should (= (mark) 4))
	  (should mark-active)
	  (should (= changes 0))
	  (should (equal ranges
			 (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
				 existing)))
	  (should-not (seq-some (lambda (ov) (overlay-get ov 'cera))
				(overlays-in (point-min) (point-max)))))))))

(ert-deftest cera-cancel-and-error-leave-no-temporary-state ()
  "Both cancellation paths and unexpected errors unwind the full reader."
  (dolist (finish '(cera-cancel keyboard-quit error))
    (with-temp-buffer
      (insert "last line without newline")
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (let ((source (buffer-string))
	    (locals (cera--remember-locals))
	    outcome)
	(cera-test--reading
	    (lambda ()
	      (insert "discard this")
	      (if (eq finish 'error)
		  (error "Reader failure")
		(funcall finish)))
	  (condition-case nil
	      (cera-read nil)
	    (quit (setq outcome 'quit))
	    (error (setq outcome 'error))))
	(should (eq outcome (if (eq finish 'error) 'error 'quit)))
	(should (equal source (buffer-string)))
	(should (equal locals (cera--remember-locals)))
	(should-not (buffer-modified-p))
	(should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest cera-rejects-document-edits ()
  "The field is the only writable range, including at both empty boundaries."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (cera-test--reading
	(lambda ()
	  (let ((begin (cera--session-begin cera--active))
		(end (cera--session-end cera--active)))
	    (should (= begin end))
	    (insert "abc")
	    (should (equal (cera-test--input) "abc"))
	    (dolist (range (list (cons (1- begin) begin)
				 (cons (point-min) (point-max))))
	      (should-error (delete-region (car range) (cdr range)) :type 'user-error)
	      (cera--pre-command))
	    (goto-char begin)
	    (insert "prefix ")
	    (goto-char end)
	    (insert " suffix")
	    (should (equal (cera-test--input) "prefix abc suffix"))
	    (cera-accept)))
      (should (equal (cera-read nil) "prefix abc suffix")))
    (should (equal (buffer-string) "alpha\nnext\n"))))

(ert-deftest cera-linewise-deletion-clears-the-last-line ()
  "A deletion through the newline closing the field empties its last line.
The newline is the field's own chrome and comes straight back, so a
linewise command works on the last line as it does on the ones above,
and on an empty last line takes that line away.  Nothing may be written
past the newline, and deleting it alone from the end of a line with
text, or from an empty field, is still refused."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (cera-test--reading
	(lambda ()
	  (let ((end (cera--session-end cera--active)))
	    (should-error (delete-region end (1+ end)) :type 'user-error)
	    (cera--pre-command)
	    (insert "one\ntwo")
	    (should-error (delete-region end (1+ end)) :type 'user-error)
	    (cera--pre-command)
	    (delete-region (line-beginning-position) (1+ end))
	    (should (equal (cera-test--input) "one\n"))
	    (should (equal (buffer-string) "alpha\none\n\nnext\n"))
	    (should (= (cera--session-tail cera--active) (1+ end)))
	    (delete-region end (1+ end))
	    (should (equal (cera-test--input) "one"))
	    (should (equal (buffer-string) "alpha\none\nnext\n"))
	    (should (= (cera--session-tail cera--active) (1+ end)))
	    (should-error (progn (goto-char (1+ end)) (insert "x"))
			  :type 'user-error)
	    (cera--pre-command)
	    (goto-char end)
	    (insert "\ntwo"))
	  (cera-accept))
      (should (equal (cera-read nil) "one\ntwo")))
    (should (equal (buffer-string) "alpha\nnext\n"))))

(defun cera-test--end-released-edit (&optional depth)
  "Run the command loop's exit of a released field, returning whether it left.
The field's recursive edit runs at DEPTH, one level deeper than the test
does unless given.  Nothing is left behind once every released field has
been let out."
  (prog1 (not (eq (catch 'exit
                    (cl-letf (((symbol-function 'recursion-depth)
                               (lambda () (or depth 1))))
		      (run-hooks 'post-command-hook))
                    'stayed)
                  'stayed))
    (unless cera--released-depths
      (should-not (memq #'cera--exit-released post-command-hook)))))

(defmacro cera-test--with-file-buffer (text &rest body)
  "Run BODY in a buffer visiting a fresh file holding TEXT, bound as `file'."
  (declare (indent 1) (debug (form body)))
  `(let* ((file (make-temp-file "cera-test-"))
          (make-backup-files nil)
          (buffer (progn (with-temp-file file (insert ,text))
                         (find-file-noselect file))))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file file))))

(defun cera-test--file-text (file)
  "Return what FILE holds."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest cera-save-releases-the-field-and-saves-the-document ()
  "Saving cancels the field, keeps the writing, and writes the document.
The field's text is not part of what is saved, and the buffer's other
save hooks run over the document rather than being skipped."
  (cera-test--with-file-buffer "alpha\nnext\n"
    (let ((seen nil))
      (add-hook 'before-save-hook (lambda () (setq seen (buffer-string))) nil t)
      (goto-char (point-max))
      (insert "tail")
      (goto-char 2)
      (let ((locals (cera--remember-locals))
	    (kill-ring nil))
	(cera-test--should-quit
          (cera-test--reading
	      (lambda ()
		(insert "typed")
		(save-buffer)
		(should (equal (buffer-string) "alpha\nnext\ntail"))
		(should (equal seen "alpha\nnext\ntail"))
		(should-not (buffer-modified-p))
		(should (equal (current-kill 0) "typed"))
		(should-not cera--active)
		(should (cera-test--end-released-edit)))
	    (cera-read nil)))
	(should (equal (cera-test--file-text file) "alpha\nnext\ntail"))
	(should (equal locals (cera--remember-locals)))
	(should-not (overlays-in (point-min) (point-max)))
	(should-not cera--open-buffers)))))

(ert-deftest cera-kill-buffer-releases-the-field ()
  "Killing the buffer cancels the field and takes the exit hooks with it."
  (let ((buffer (generate-new-buffer " *cera-kill*")))
    (with-current-buffer buffer
      (insert "source\nnext")
      (goto-char 2)
      (cera-test--should-quit
        (cera-test--reading
	    (lambda ()
	      (insert "typed")
	      (kill-buffer)
	      (should-not (buffer-live-p buffer))
	      (should-not cera--open-buffers)
	      (should-not (memq #'cera--release-all kill-emacs-hook))
	      (should-not (memq #'cera--release-all kill-emacs-query-functions))
	      (should (cera-test--end-released-edit)))
	  (cera-read nil))))))

(ert-deftest cera-major-mode-change-and-revert-release-the-field ()
  "Changing the major mode or reverting cancels the field first."
  (dolist (command '(fundamental-mode revert))
    (cera-test--with-file-buffer "alpha\nnext\n"
      (goto-char 2)
      (let ((locals (cera--remember-locals)))
	(cera-test--should-quit
          (cera-test--reading
	      (lambda ()
		(insert "typed")
		(if (eq command 'revert) (revert-buffer t t) (funcall command))
		(should (equal (buffer-string) "alpha\nnext\n"))
		(should-not cera--active)
		(should (cera-test--end-released-edit)))
	    (cera-read nil)))
	(should (equal locals (cera--remember-locals)))
	(should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest cera-kill-emacs-hooks-release-every-open-field ()
  "Emacs exiting cancels every field open anywhere, before saving anything."
  (let ((first (generate-new-buffer " *cera-first*"))
	(second (generate-new-buffer " *cera-second*")))
    (unwind-protect
	(progn
	  (dolist (buffer (list first second))
	    (with-current-buffer buffer (insert "source\nnext") (goto-char 2)))
	  (with-current-buffer first
	    (cera-test--should-quit
              (cera-test--reading
		  (lambda ()
		    (insert "one")
		    (with-current-buffer second
		      (cera-test--should-quit
                        (cl-letf (((symbol-function 'recursion-depth) (lambda () 1)))
			  (cera-test--reading
			      (lambda ()
				(insert "two")
				(should (equal (length cera--open-buffers) 2))
				(should (memq #'cera--release-all kill-emacs-hook))
				(should (run-hook-with-args-until-failure
					 'kill-emacs-query-functions))
				(dolist (buffer (list first second))
				  (with-current-buffer buffer
				    (should (equal (buffer-string) "source\nnext"))
				    (should-not cera--active)))
				(should-not cera--open-buffers)
				(should (cera-test--end-released-edit 2)))
			    (cera-read nil)))))
		    (should (cera-test--end-released-edit 1))
		    (should-not cera--released-depths))
		(cera-read nil)))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest cera-field-never-shows-the-buffer-modified ()
  "The field's text leaves the modified flag as it was when it opened."
  (dolist (modified '(nil t))
    (with-temp-buffer
      (insert "source\nnext")
      (set-buffer-modified-p modified)
      (goto-char 2)
      (cera-test--reading
	  (lambda ()
	    (should (eq (buffer-modified-p) modified))
	    (insert "typed")
	    (should (eq (buffer-modified-p) modified))
	    (cera-accept))
	(should (equal (cera-read nil) "typed")))
      (should (eq (buffer-modified-p) modified)))))

(ert-deftest cera-field-takes-no-lock-file ()
  "Writing into the field does not lock the file the buffer visits."
  (cera-test--with-file-buffer "alpha\nnext\n"
    (setq-local create-lockfiles t)
    (goto-char 2)
    (cera-test--reading
	(lambda ()
	  (insert "typed")
	  (should-not (file-locked-p file))
	  (cera-accept))
      (should (equal (cera-read nil) "typed")))
    (should (eq create-lockfiles t))))

(ert-deftest cera-indirect-field-protects-the-base-buffer ()
  "Saving the base of an indirect field buffer writes the document alone."
  (cera-test--with-file-buffer "alpha\nnext\n"
    (auto-save-mode 1)
    (let ((base (current-buffer))
          (indirect (clone-indirect-buffer " *cera-indirect*" nil)))
      (goto-char (point-max))
      (insert "tail")
      (unwind-protect
	  (with-current-buffer indirect
	    (goto-char 2)
	    (cera-test--should-quit
              (cera-test--reading
		  (lambda ()
		    (insert "typed")
		    (should-not (with-current-buffer base buffer-auto-save-file-name))
		    (with-current-buffer base (save-buffer))
		    (should (equal (buffer-string) "alpha\nnext\ntail"))
		    (should-not cera--active)
		    (should (cera-test--end-released-edit)))
		(cera-read nil)))
	    (should (equal (cera-test--file-text file) "alpha\nnext\ntail"))
	    (should (with-current-buffer base
		      (and buffer-auto-save-file-name
			   (not (memq #'cera--release before-save-hook))))))
	(kill-buffer indirect)))))

(ert-deftest cera-preserves-narrowing-and-indirect-source ()
  "A narrowed indirect source gets its original text and restriction back."
  (with-temp-buffer
    (insert "outside\nalpha\nnext\noutside")
    (let* ((base (current-buffer))
	   (source (buffer-string))
	   (indirect (clone-indirect-buffer " *cera-inline-test*" nil)))
      (unwind-protect
	  (with-current-buffer indirect
	    (narrow-to-region 9 20)
	    (goto-char 10)
	    (let ((undo buffer-undo-list))
	      (cera-test--reading
		  (lambda () (insert "note") (cera-accept))
		(should (equal (cera-read nil) "note")))
	      (should (eq undo buffer-undo-list)))
	    (should (= (point-min) 9))
	    (should (= (point-max) 20))
	    (should (= (point) 10))
	    (should (equal (with-current-buffer base (buffer-string)) source)))
	(kill-buffer indirect)))))

(ert-deftest cera-capf-honors-table-protocol-and-input-bounds ()
  "Completion keeps its category and affixes and never includes the chrome."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (let* ((annotation (lambda (_) "  ask for clarification"))
	   (table (lambda (string predicate action)
		    (if (eq action 'metadata)
			`(metadata (category . cera-field)
				   (annotation-function . ,annotation))
		      (complete-with-action action '("Question" "Suggestion")
					    string predicate)))))
      (cera-test--reading
	  (lambda ()
	    (pcase-let ((`(,begin ,end ,collection . ,properties) (cera--capf)))
	      (should (equal (buffer-substring-no-properties begin end) "Que"))
	      (should (eq (plist-get properties :exclusive) 'no))
	      (should (equal (all-completions "Que" collection) '("Question")))
	      (should (eq (completion-metadata-get
			   (completion-metadata "" collection nil) 'annotation-function)
			  annotation))
	      (goto-char (1- begin))
	      (should-not (cera--capf))
	      (goto-char end))
	    (cera-accept))
	(should (equal (cera-read table "Que") "Que"))))))

(ert-deftest cera-input-undo-cannot-consume-document-history ()
  "Undoing input and a rejected undo at the field boundary still roll back."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "source\nnext")
    (undo-boundary)
    (let ((undo buffer-undo-list)
	  (source (buffer-string)))
      (goto-char 2)
      (cera-test--reading
	  (lambda ()
	    (insert "draft")
	    (undo-boundary)
	    (let ((last-command nil)) (undo-only 1))
	    (should (equal (cera-test--input) ""))
	    (let ((last-command 'undo))
	      (should-error (undo-only 1) :type 'user-error))
	    (cera--pre-command)
	    (insert "saved")
	    (cera-accept))
	(should (equal (cera-read nil) "saved")))
      (should (eq undo buffer-undo-list))
      (should (equal source (buffer-string))))))

(ert-deftest cera-preserves-editing-maps-hooks-and-escape-prefix ()
  "Local editing keys and command hooks continue to run during input."
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (let ((before 0) (after 0) result)
	(local-set-key (kbd "C-h") #'delete-backward-char)
	(local-set-key (kbd "M-!") #'backward-char)
	(add-hook 'pre-command-hook (lambda () (cl-incf before)) nil t)
	(add-hook 'post-command-hook (lambda () (cl-incf after)) nil t)
	(local-set-key [f5]
		       (lambda () (interactive)
			 (setq result (cera-read nil))))
	(execute-kbd-macro [f5 ?a ?b ?\C-h ?c ?\M-! ?x return])
	(should (equal result "axc"))
	(should (> before 5))
	(should (> after 5))
	(should (equal (buffer-string) "source\nnext"))))))

(ert-deftest cera-keyboard-accepts-multiline-input ()
  "The actual recursive command loop supports typing, newlines, and RET."
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext\n")
      (goto-char 2)
      (let (result)
	(local-set-key [f5]
		       (lambda () (interactive)
			 (setq result (cera-read nil))))
	(execute-kbd-macro [f5 ?f ?i ?r ?s ?t ?\C-j ?l ?i ?n ?e return])
	(should (equal result "first\nline"))
	(should (equal (buffer-string) "source\nnext\n"))
	(should (= (point) 2))))))


(ert-deftest cera-keeps-room-below-the-field-while-completing ()
  "The field holds room for the popup exactly while completion is in region.
The room follows `completion-in-region-mode', so it is asked for by any
in-buffer completion frontend and given back the moment completion ends."
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (let ((completion-in-region-mode-predicate #'always))
	(cera-test--reading
	    (lambda ()
	      (setq-local cera-completion-space 6)
	      (should (equal (buffer-string) "source\nWhat\nnext"))
	      (should-not (overlay-get (cera--session-spacer cera--active)
				       'after-string))
	      (completion-in-region-mode 1)
	      (should (equal (overlay-get (cera--session-spacer cera--active)
					  'after-string)
			     (make-string 6 ?\n)))
	      (should (equal (buffer-string) "source\nWhat\nnext"))
	      (completion-in-region-mode -1)
	      (should-not (overlay-get (cera--session-spacer cera--active)
				       'after-string))
	      (cera-accept))
	  (should (equal (cera-read '("What next?" "What now?") "What")
			 "What")))))))


(ert-deftest cera-no-source-face-marks-nothing-and-connects-anyway ()
  "A nil source face leaves the region unmarked, the field connected.
Marking the source is the consumer's to do its own way, but where the
field came from is the reader's to show."
  (with-temp-buffer
    (insert "alpha beta\nnext\n")
    (goto-char 1)
    (let ((transient-mark-mode t))
      (goto-char 6)
      (set-mark 1)
      (setq mark-active t)
      (cera-test--reading
	  (lambda ()
	    (should-not (cera-test--source-ranges))
	    (should (string= (get-char-property 1 'line-prefix) "╭ "))
	    (cera-accept))
	(should (equal (cera-read nil "note" nil nil) "note"))))))

(defun cera-test--field-prefixes ()
  "Return the line prefix of every line of the active field."
  (let ((bounds (cera--field-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (cl-loop collect (get-char-property (point) 'line-prefix)
               while (< (line-end-position) (cdr bounds))
               do (forward-line)))))

(defun cera-test--alignments (prefix)
  "Return the columns the stretching spaces of PREFIX reach to."
  (cl-loop for index below (length prefix)
           for display = (get-text-property index 'display prefix)
           when (eq (car-safe display) 'space)
           collect (plist-get (cdr display) :align-to)))

(ert-deftest cera-input-prefix-is-drawn-in-front-of-the-input ()
  "The field carries no prefix until one is set, then aligns behind it.
The closing bracket carries the prefix and resumes at the column kept
for it, however wide the display draws it, and the lines above the input
reach the column the input starts at."
  (dolist (fixture '((nil 2 "one" ("╰ ") (()))
		     (nil 2 "one\ntwo" ("│ " "╰ ") (() ()))
		     ("*" 2 "one" ("╰ * ─ ") ((4)))
		     ("ab" 2 "one" ("╰ ab ─ ") ((4)))
		     ("*" 0 "one" ("╰ * ─ ") ((2)))
		     ("*" 5 "one" ("╰ * ─ ") ((7)))
		     ("*" 2 "one\ntwo" ("│  " "╰ * ─ ") ((6) (4)))
		     ("*" 2 "one\ntwo\nthree"
		      ("│  " "│  " "╰ * ─ ") ((6) (6) (4)))))
    (pcase-let ((`(,prefix ,width ,initial ,drawn ,alignments) fixture))
      (with-temp-buffer
	(insert "source\nnext")
	(goto-char 2)
	(let ((cera-input-prefix prefix)
	      (cera-input-prefix-width width))
	  (cera-test--reading
	      (lambda ()
		(let ((prefixes (cera-test--field-prefixes)))
		  (should (equal prefixes drawn))
		  (should (equal (mapcar #'cera-test--alignments prefixes)
				 alignments)))
		(cera-accept))
	    (should (equal (cera-read nil initial) initial))))
	(should (equal (buffer-string) "source\nnext"))
	(should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest cera-indent-moves-the-bracket-into-the-source-indentation ()
  "The bracket is held off to the indent and takes the source's own away.
The lines it opens keep their text where the indentation had put it, and
the field's own lines and the column the input starts at follow along."
  (with-temp-buffer
    (insert "      source\n  second\nnext")
    (goto-char 8)
    (let ((cera-indent 4)
	  (cera-input-prefix "*")
	  (cera-input-prefix-width 2))
      (cera-test--reading
	  (lambda ()
	    (should (equal (cera-test--field-prefixes) '(" ╰ * ─ ")))
	    (should (equal (cera-test--alignments
			    (car (cera-test--field-prefixes)))
			   '(4 8)))
	    (should (equal (cera-test--alignments
			    (get-char-property 1 'line-prefix))
			   '(4)))
	    (should (equal (get-char-property 1 'display) ""))
	    (cera-accept))
	(should (equal (cera-read nil "one") "one"))))
    (should (equal (buffer-string) "      source\n  second\nnext"))
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest cera-indent-defaults-to-hanging-the-bracket-off-the-edge ()
  "Without an indent the bracket keeps the left edge and no text is hidden."
  (with-temp-buffer
    (insert "      source\nnext")
    (goto-char 8)
    (cera-test--reading
	(lambda ()
	  (should (equal (cera-test--field-prefixes) '("╰ ")))
	  (should (equal (get-char-property 1 'line-prefix) "╭ "))
	  (should-not (get-char-property 1 'display))
	  (cera-accept))
      (should (equal (cera-read nil "one") "one")))))

(ert-deftest cera-recall-writes-a-table-entry-the-minibuffer-chose ()
  "C-r reads the table in the minibuffer and puts the entry in the field.
The table is offered when it is asked for rather than drawn over the
text as it is written: its prefix counts as too short for a frontend
that completes on its own."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (cera-test--reading
	(lambda ()
	  (should (equal (plist-get (nthcdr 3 (cera--capf)) :company-prefix-length) 0))
	  (should (eq (keymap-lookup cera-mode-map "C-r") #'cera-recall))
	  (cl-letf (((symbol-function 'completing-read)
		     (lambda (_prompt table &rest _)
		       (should (equal table '("earlier" "older")))
		       "older")))
	    (cera-recall))
	  (should (equal (cera-test--input) "older"))
	  (cera-accept))
      (should (equal (cera-read '("earlier" "older") "draft") "older")))
    (should (equal (buffer-string) "source\nnext"))
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest cera-recall-needs-a-field-and-a-table ()
  "Recall outside a field, or in one opened with nothing, says so."
  (with-temp-buffer
    (should-error (cera-recall) :type 'user-error)
    (insert "source\nnext")
    (goto-char 2)
    (cera-test--reading
	(lambda ()
	  (should-error (cera-recall) :type 'user-error)
	  (cera-accept))
      (should (equal (cera-read nil "draft") "draft")))))

(ert-deftest cera-input-prefix-may-be-computed ()
  "A function standing in for the prefix is called to draw it."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (let ((cera-input-prefix (lambda () (propertize ">" 'face 'cera-border))))
      (cera-test--reading
	  (lambda ()
	    (should (equal (cera-test--field-prefixes) '("╰ > ─ ")))
	    (cera-accept))
	(should (equal (cera-read nil "one") "one"))))))

(ert-deftest cera-abort-key-dismisses-the-field ()
  "C-c C-k discards the field, pairing with C-c C-c accepting it."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (cera-test--reading
	(lambda ()
	  (should (eq (key-binding (kbd "C-c C-c")) #'cera-accept))
	  (should (eq (key-binding (kbd "C-c C-k")) #'cera-cancel))
	  (insert "typed and abandoned")
	  (call-interactively (key-binding (kbd "C-c C-k"))))
      (condition-case nil (cera-read nil "") (quit nil)))
    (should (equal (buffer-string) "source\nnext"))
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest cera-stack-validates-before-changing-the-document ()
  "A stack has unique ids and exactly one writable, unbounded pane."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (let ((text (buffer-string)) (undo buffer-undo-list))
      (dolist (panes
	       (list nil
		     (list (cera-pane :id 'a :kind 'readonly :text "context"))
		     (list (cera-pane :id 'a :kind 'input)
			   (cera-pane :id 'b :kind 'input))
		     (list (cera-pane :id 'a :kind 'input)
			   (cera-pane :id 'a :kind 'readonly :text "context"))
		     (list (cera-pane :id 'a :kind 'input :bounds '(1 . 3)))
		     (list (cera-pane :id 'a :kind 'input :bracket 'maybe))
		     (list (cera-pane :id 'a :kind 'input :prefix-position 'left))
		     (list (cera-pane :id 'a :kind 'input)
			   (cera-pane :id 'b :kind 'readonly :text "x"
				      :bounds '(1 . 3)))))
	(should-error (cera-read-stack panes nil) :type 'user-error)
	(should (equal (buffer-string) text))
	(should (eq buffer-undo-list undo))
	(should (= (point) 2))
	(should-not cera--active)))))

(ert-deftest cera-stack-update-is-display-only-and-ordered ()
  "Updating context cannot alter the draft, point, undo or completion."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (let* ((map (make-sparse-keymap))
	   (cera-session-keymap map)
	   (cera-read-context-function
	    (lambda (panes)
	      (append (list (cera-pane :id 'first :kind 'readonly :text "FIRST")
			    (cera-pane :id 'second :kind 'readonly :text "SECOND"))
		      panes)))
	   (started nil)
	   (cera-session-start-hook
	    (list (lambda (_session) (setq started (current-buffer))))))
      (keymap-set map "C-c C-v" #'ignore)
      (cera-test--reading
	  (lambda ()
	    (should (eq started (current-buffer)))
	    (should (eq (key-binding (kbd "C-c C-v")) #'ignore))
	    (should-not (string-match-p "FIRST\\|SECOND" (buffer-string)))
	    (let ((shown (mapconcat
			  (lambda (o) (or (overlay-get o 'before-string) ""))
			  (append (car (overlay-lists)) (cdr (overlay-lists))) "")))
	      ;; Each pane closes on its own row, so SECOND follows the
	      ;; first pane's rows rather than its text line.
	      (should (string-match-p "FIRST" shown))
	      (should (< (string-match "FIRST" shown)
	                 (string-match "SECOND" shown))))
	    (goto-char (1+ (car (cera--field-bounds))))
	    (let ((position (point)) (undo buffer-undo-list)
		  (modified (buffer-modified-p))
		  (capf (cera--capf))
		  (completion-in-region-mode t)
		  (input-overlays (copy-sequence (cera--session-overlays cera--active))))
	      (should (cera-update-pane 'first "UPDATED\nCONTEXT"))
	      (should (= (point) position))
	      (should (equal (cera-test--input) "draft"))
	      (should (eq buffer-undo-list undo))
	      (should (eq (buffer-modified-p) modified))
	      (should completion-in-region-mode)
	      (should (equal (cera--capf) capf))
	      (should (equal (cera--session-overlays cera--active) input-overlays))
	      (should-error (cera-update-pane 'missing "x") :type 'user-error))
	    (cera-accept))
	(should (equal (cera-read '("draft") "draft") "draft"))))
    (should (equal (buffer-string) "source\nnext"))
    (should-error (cera-update-pane 'first "late") :type 'user-error)
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest cera-empty-supplied-panes-hide-and-restore ()
  "Empty context occupies no rows, bracketed or not, including after updates."
  (dolist (bracket '(t nil))
    (save-window-excursion
      (with-temp-buffer
        (insert "source\nnext")
        (goto-char 2)
        (set-window-buffer (selected-window) (current-buffer))
        (let ((cera-indent 0)
              (stride (if bracket 2 1)))
          (cera-test--reading
              (lambda ()
                (should-not (cera--session-static-overlays cera--active))
                (goto-char (1+ (car (cera--field-bounds))))
                (let ((position (point)) (undo buffer-undo-list)
                      (text (buffer-string)) (modified (buffer-modified-p))
                      (capf (cera--capf)) (completion-in-region-mode t)
                      (input-overlays (cera--session-overlays cera--active)))
                  (dolist (step '((first "FIRST" "FIRST")
                                  (second "SECOND" "FIRST\nSECOND")
                                  (first "" "SECOND")
                                  (second "" "")
                                  (first "AGAIN" "AGAIN")
                                  (first "" "")))
                    (pcase-let ((`(,id ,replacement ,expected) step))
                      (should (cera-update-pane id replacement))
                      (let* ((overlays (cera--session-static-overlays cera--active))
                             (shown (mapconcat
                                     (lambda (o) (or (overlay-get o 'before-string) ""))
                                     (reverse overlays) ""))
                             (rows (split-string shown "\n" t))
                             (lines (split-string expected "\n")))
                        (if (string-empty-p expected)
                            (should-not overlays)
                          (should (= (length rows) (* stride (length lines))))
                          (cl-loop for content in lines
                                   for index from 0 by stride
                                   do (should (string-match-p content (nth index rows)))))))
                    (should (= (point) position))
                    (should (equal (buffer-string) text))
                    (should (eq buffer-undo-list undo))
                    (should (eq (buffer-modified-p) modified))
                    (should completion-in-region-mode)
                    (should (equal (cera--capf) capf))
                    (should (eq (cera--session-overlays cera--active) input-overlays))))
                (cera-accept))
            (should (equal
                     (cera-read-stack
                      (list (cera-pane :id 'first :kind 'readonly :text ""
                                       :bracket bracket :prefix "CONTEXT")
                            (cera-pane :id 'second :kind 'readonly :text ""
                                       :bracket bracket)
                            (cera-pane :id 'input :kind 'input :text "draft"))
                      '("draft"))
                     "draft"))))))))

(ert-deftest cera-empty-context-keeps-bounded-and-input-panes ()
  "Empty supplied context does not hide bounded panes or empty input."
  (save-window-excursion
    (with-temp-buffer
      (insert "source\nnext")
      (goto-char 2)
      (set-window-buffer (selected-window) (current-buffer))
      (cera-test--reading
          (lambda ()
            (should (get-char-property 1 'line-prefix))
            (should (get-char-property (car (cera--field-bounds)) 'line-prefix))
            (should-not (cl-some (lambda (o) (overlay-get o 'before-string))
                                 (cera--session-static-overlays cera--active)))
            (cera-update-pane 'context "shown")
            (cera-update-pane 'context "")
            (should (get-char-property 1 'line-prefix))
            (should (get-char-property (car (cera--field-bounds)) 'line-prefix))
            (cera-accept))
        (should (equal
                 (cera-read-stack
                  (list (cera-pane :id 'bounded :kind 'readonly :bounds '(1 . 1))
                        (cera-pane :id 'context :kind 'readonly :text "")
                        (cera-pane :id 'input :kind 'input :text "")) nil)
                 ""))))))

(ert-deftest cera-single-line-context-keeps-its-lower-corner ()
  "Separate one-line panes each retain a lower corner, with or without labels."
  (dolist (position '(top bottom))
    (with-temp-buffer
      (insert "source\nnext")
      (goto-char 2)
      (cera-test--reading
          (lambda ()
            (let* ((shown (overlay-get
                           (car (cera--session-static-overlays cera--active))
                           'before-string))
                   (rows (split-string shown "\n" t))
                   (corners '("╭" "╰" "╭" "╰")))
              (should (= (length rows) (length corners)))
              (cl-mapc (lambda (row corner)
                         (should (string-prefix-p corner row)))
                       rows corners)
              (should (string-match-p "FIRST" (car rows)))
              (should (string-match-p "SECOND" (nth 2 rows)))
              (should (string-prefix-p "╰" (get-char-property
                                              (car (cera--field-bounds)) 'line-prefix))))
            (cera-accept))
        (cera-read-stack
         (list (cera-pane :id 'first :kind 'readonly :text "FIRST"
                          :prefix (and (eq position 'top) "A")
                          :prefix-position position)
               (cera-pane :id 'second :kind 'readonly :text "SECOND"
                          :prefix (and (eq position 'top) "B")
                          :prefix-position position)
               (cera-pane :id 'input :kind 'input)) nil)))))

(ert-deftest cera-pane-layout-mirrors-endpoints-without-empty-icon-space ()
  "Prefix endpoints work for input and virtual panes."
  (dolist (position '(top bottom))
    (dolist (prefix '(nil "*"))
      (with-temp-buffer
	(insert "source\nnext")
	(goto-char 2)
	(cera-test--reading
	    (lambda ()
	      (let* ((begin (car (cera--field-bounds)))
		     (end (cdr (cera--field-bounds)))
		     (first (get-char-property begin 'line-prefix))
		     (last (get-char-property end 'line-prefix))
		     (drawn (if (eq position 'top) first last)))
		(should (string-match-p (if (eq position 'top) "╭" "╰") drawn))
		(if prefix
		    (should (string-match-p "\\*" drawn))
		  (should (= (string-width drawn) 2)))
		(should (equal (cera-test--input) "one\ntwo")))
	      (should-error (cera-update-pane 'input "x") :type 'user-error)
	      (cera-accept))
	  (should
	   (equal (cera-read-stack
		   (list (cera-pane :id 'context :kind 'readonly :text "context"
				    :prefix prefix :prefix-position position)
			 (cera-pane :id 'input :kind 'input :text "one\ntwo"
				    :prefix prefix :prefix-position position))
		   nil)
		  "one\ntwo")))))))

(ert-deftest cera-stack-keeps-bounded-panes-in-document-order ()
  "Bounds on either side of the input track their original document text."
  (with-temp-buffer
    (insert "before\nafter\n")
    (goto-char 2)
    (let* ((before (cera-pane :id 'before :kind 'readonly :bounds '(1 . 7)
                              :face 'bold :prefix "B" :prefix-position 'top))
           (after (cera-pane :id 'after :kind 'readonly :bounds '(8 . 13)
                             :face 'italic :prefix "A"))
           (panes (list before (cera-pane :id 'input :kind 'input :text "draft")
                        (cera-pane :id 'virtual :kind 'readonly :text "VIRTUAL") after
                        (cera-pane :id 'trailing :kind 'readonly :text "TRAILING"))))
      (cera-test--reading
          (lambda ()
            (should (equal (buffer-string) "before\ndraft\nafter\n"))
            (should (eq (get-char-property 1 'face) 'bold))
            (should (eq (get-char-property 14 'face) 'italic))
            (let ((overlay (cl-find-if
                            (lambda (o) (let ((text (overlay-get o 'before-string)))
                                          (and text (string-match-p "TRAILING" text))))
                            (cera--session-static-overlays cera--active))))
              (should (= (overlay-start overlay) (1- (point-max)))))
            (should-error (cera-update-pane 'before "replacement") :type 'user-error)
            (insert " more")
            (should (eq (get-char-property 19 'face) 'italic))
            (should (cera-update-pane 'virtual "CHANGED"))
            (should (equal (cera-pane-bounds before) '(1 . 7)))
            (should (equal (cera-pane-bounds after) '(8 . 13)))
            (cera-accept))
        (should (equal (cera-read-stack panes nil) "draft more"))))
    (should (equal (buffer-string) "before\nafter\n"))))

(ert-deftest cera-unbracketed-panes-show-their-text-alone ()
  "A pane without a bracket draws its rows in its face and nothing else."
  (save-window-excursion
    (with-temp-buffer
      (insert "source\nnext\n")
      (goto-char 2)
      (set-window-buffer (selected-window) (current-buffer))
      (cera-test--reading
          (lambda ()
            (let* ((overlay (cl-find-if
                             (lambda (o) (overlay-get o 'before-string))
                             (cera--session-static-overlays cera--active)))
                   (shown (overlay-get overlay 'before-string))
                   (rows (split-string shown "\n" t)))
              (should (equal rows '("short" "rows")))
              (should (eq (get-text-property 0 'face (car rows)) 'bold))
              (should-not (string-match-p "[╭╰│╮╯]" shown)))
            (cera-accept))
        (should (equal
                 (cera-read-stack
                  (list (cera-pane :id 'context :kind 'readonly :text "short\nrows"
                                   :bracket nil :face 'bold)
                        (cera-pane :id 'input :kind 'input :text "draft"))
                  nil)
                 "draft"))))))

(ert-deftest cera-stack-wraps-for-each-window-and-reflows-on-resize ()
  "Virtual rows respect each window's width without inheriting source prefixes."
  (save-window-excursion
    (with-temp-buffer
      (insert "source\nnext\n")
      (goto-char 2)
      (set-window-buffer (selected-window) (current-buffer))
      (let* ((first (selected-window))
             (second (split-window-right 24))
             (text (make-string 100 ?x)))
        (set-window-buffer second (current-buffer))
        (cera-test--reading
            (lambda ()
              (let ((draft (cera-test--input)) (position (point))
                    (undo buffer-undo-list)
                    (input-overlays (cera--session-overlays cera--active))
                    counts)
                (dolist (window (list first second))
                  (let* ((overlay (cl-find-if
                                   (lambda (o) (and (eq (overlay-get o 'window) window)
                                                    (overlay-get o 'before-string)))
                                   (cera--session-static-overlays cera--active)))
                         (shown (overlay-get overlay 'before-string))
                         (rows (split-string shown "\n" t)))
                    (push (length rows) counts)
                    (should (equal (get-text-property 0 'line-prefix shown) nil))
                    (should (equal (get-text-property 1 'line-prefix shown) ""))
                    (should (equal (get-text-property 1 'wrap-prefix shown) ""))
                    (should (string-prefix-p "╭" (car rows)))
                    (should (string-prefix-p "╰" (car (last rows))))
                    (dolist (row rows)
                      (should (<= (string-width row) (window-body-width window))))))
                (should (> (cadr counts) (car counts)))
                (window-resize first 8 t)
                (run-hooks 'window-configuration-change-hook)
                (let* ((overlay (cl-find-if
                                 (lambda (o) (and (eq (overlay-get o 'window) first)
                                                  (overlay-get o 'before-string)))
                                 (cera--session-static-overlays cera--active)))
                       (shown (overlay-get overlay 'before-string)))
                  (should (< (length (split-string shown "\n" t)) (cadr counts))))
                (should (= (point) position))
                (should (equal (cera-test--input) draft))
                (should (eq buffer-undo-list undo))
                (should (eq (cera--session-overlays cera--active) input-overlays)))
              (cera-accept))
          (should (equal
                   (cera-read-stack
                    (list (cera-pane :id 'context :kind 'readonly :text text)
                          (cera-pane :id 'input :kind 'input :text "draft")) nil)
                   "draft")))))))

(ert-deftest cera-input-closes-on-its-final-row ()
  "Wrapped input closes on its last row and has bounded redraw work."
  (save-window-excursion
    (with-temp-buffer
      (insert "source\nnext\n")
      (goto-char 2)
      (set-window-buffer (selected-window) (current-buffer))
      (split-window-right 30)
      (cera-test--reading
          (lambda ()
            (let ((begin (car (cera--field-bounds)))
                  (noninteractive nil))
              (cera--draw)
              (should (string-match-p
                       "╰" (get-char-property begin 'line-prefix))))
            (insert "\nsecond")
            (let ((short (cera-test--draw-allocations))
                  (static (cera--session-static-overlays cera--active)))
              (dotimes (_ 100) (insert "\nanother row"))
              (should (= short (cera-test--draw-allocations)))
              (should (eq static (cera--session-static-overlays cera--active))))
            (cera-accept))
        (cera-read-stack
         (list (cera-pane :id 'input :kind 'input
                          :prefix "*" :text (make-string 100 ?x))) nil)))))

(ert-deftest cera-input-lines-carry-no-line-numbers ()
  "The field's own lines are left out of the line numbers, the source's are not."
  (with-temp-buffer
    (insert "source\nnext\n")
    (goto-char 2)
    (cera-test--reading
        (lambda ()
          (insert "\nsecond\nthird")
          (let ((bounds (cera--field-bounds)))
            (save-excursion
              (goto-char (car bounds))
              (while (< (point) (cdr bounds))
                (should (get-char-property (line-beginning-position)
                                           'display-line-numbers-disable))
                (forward-line 1))))
          (should-not (get-char-property (point-min) 'display-line-numbers-disable))
          (cera-accept))
      (should (equal (cera-read nil "first") "first\nsecond\nthird")))
    (should (equal (buffer-string) "source\nnext\n"))))

(ert-deftest cera-leaves-the-field-uncoloured-by-the-buffer-s-language ()
  "The field holds prose, so the surrounding language must not colour it."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; a comment\n")
    (font-lock-mode 1)
    (goto-char (point-max))
    (cera-test--reading
        (lambda ()
          (insert ";; not a comment, prose\n(defun nor-code ())")
          (let ((bounds (cera--field-bounds)))
            (font-lock-fontify-region (point-min) (point-max))
            (should-not (text-property-not-all (car bounds) (cdr bounds) 'face nil))
            (should (eq (get-text-property 6 'face) 'font-lock-comment-face)))
          (cera-accept))
      (should (string-prefix-p ";; not a comment" (cera-read nil ""))))
    ;; The buffer colours its own text again once the field is gone.
    (font-lock-fontify-region (point-min) (point-max))
    (should (eq (get-text-property 6 'face) 'font-lock-comment-face))))

(ert-deftest cera-a-fontifier-colours-the-field-and-nothing-else ()
  "A consumer may colour the field itself, as prose rather than code."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; a comment\n")
    (font-lock-mode 1)
    (goto-char (point-max))
    (let ((cera-input-fontifier
           (lambda (begin end)
             (put-text-property begin (min end (+ begin 4)) 'face 'bold))))
      (cera-test--reading
          (lambda ()
            (insert "**bold** rest")
            (font-lock-fontify-region (point-min) (point-max))
            (let* ((bounds (cera--field-bounds))
                   (body (car (last (get-text-property (car bounds) 'face)))))
              ;; The field's own face stays under what the fontifier put on,
              ;; where an overlay would have covered it over.
              (should (equal (get-text-property (car bounds) 'face)
                             (list 'bold body)))
              (should (equal (get-text-property (+ 6 (car bounds)) 'face)
                             (list body)))
              (should-not (cl-find-if (lambda (overlay) (overlay-get overlay 'face))
                                      (overlays-at (car bounds))))
              (should (eq (get-text-property 6 'face) 'font-lock-comment-face)))
            (cera-accept))
        (should (equal (cera-read nil "") "**bold** rest"))))))

(ert-deftest cera-asks-for-the-field-to-be-coloured-when-it-opens ()
  "The field goes in without modification hooks, so nothing else asks."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; a comment\n")
    (font-lock-mode 1)
    (put-text-property (point-min) (point-max) 'fontified t)
    (goto-char (point-max))
    (cera-test--reading
        (lambda ()
          (let ((bounds (cera--field-bounds)))
            (should (equal (text-property-not-all (car bounds) (cdr bounds)
                                                  'fontified nil)
                           nil))
            ;; What the buffer had coloured already is left as it was.
            (should (get-text-property 6 'fontified)))
          (cera-accept))
      (should (equal (cera-read nil "already written") "already written")))))

(ert-deftest cera-colours-the-field-as-soon-as-it-is-drawn ()
  "The display is never asked to colour the field, so drawing must."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; a comment\n")
    (font-lock-mode 1)
    (goto-char (point-max))
    (let ((cera-input-fontifier
           (lambda (begin end)
             (put-text-property begin (min end (+ begin 4)) 'face 'bold))))
      (cera-test--reading
          (lambda ()
            ;; No fontification pass has run, and the field is coloured anyway.
            (let ((bounds (cera--field-bounds)))
              (should (equal (get-text-property (car bounds) 'face)
                             (list 'bold (cera--body-face)))))
            (insert "more")
            (let ((bounds (cera--field-bounds)))
              (should (equal (get-text-property (- (cdr bounds) 1) 'face)
                             (list (cera--body-face)))))
            (cera-accept))
        (should (equal (cera-read nil "already written") "already writtenmore"))))))

(ert-deftest cera-completion-space-may-be-asked-for-when-it-is-needed ()
  "A frontend that draws above the field can ask for no room at all."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (cera-test--reading
        (lambda ()
          (let ((cera-completion-space (lambda () 4)))
            (should (= (cera--completion-space) 4))
            (cera--reserve-for-completion)
            (should-not (cera-test--reserved-lines)))
          (let ((completion-in-region-mode t))
            (let ((cera-completion-space (lambda () 4)))
              (cera--reserve-for-completion)
              (should (= (cera-test--reserved-lines) 4)))
            (let ((cera-completion-space (lambda () 0)))
              (cera--reserve-for-completion)
              (should-not (cera-test--reserved-lines))))
          (cera-accept))
      (should (equal (cera-read nil "note") "note")))))

(provide 'cera-test)
