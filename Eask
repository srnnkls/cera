;; -*- mode: eask; lexical-binding: t -*-

(package "cera"
         "0.1.0"
         "Write into a temporary field in another buffer")

(website-url "https://github.com/srnnkls/cera")

(keywords "convenience" "tools")

(package-file "cera.el")

(source 'gnu)
(source 'nongnu)
(source 'melpa)

(depends-on "emacs" "29.1")

(development
 (depends-on "corfu")
 (depends-on "evil"))

(load-paths "./test")

(defconst cera-source-file-spec
  '("cera*.el" "evil-cera.el" "corfu-cera.el")
  "File spec for the package sources.")

(defconst cera-test-file-spec
  '("test/cera*-test.el" "test/evil-cera-test.el" "test/corfu-cera-test.el")
  "File spec for the ERT suites, which `files' deliberately does not ship.")

(apply #'files cera-source-file-spec)

(defun cera-eask-command (command files)
  "Return COMMAND followed by shell-quoted FILES."
  (mapconcat #'identity
             (cons command (mapcar #'shell-quote-argument files))
             " "))

(script "test"
        ;; `eask-file-root' is only assigned after this file finishes
        ;; loading, so the Eask file's own directory comes from here.
        (let* ((default-directory (file-name-directory load-file-name))
               (files (eask-expand-file-specs cera-test-file-spec)))
          (unless files
            (error "No test file matches %S under %s"
                   cera-test-file-spec default-directory))
          (cera-eask-command "eask test ert" files)))

(script "indent"
        (cera-eask-command
         "eask lint indent --strict"
         (append (eask-expand-file-specs cera-source-file-spec)
                 (eask-expand-file-specs cera-test-file-spec))))

(script "checkdoc"
        (cera-eask-command
         "eask lint checkdoc --strict"
         (eask-expand-file-specs cera-source-file-spec)))

(script "clean-generated"
        (concat
         "eask clean all && emacs -Q --batch --eval "
         (shell-quote-argument
          (prin1-to-string
           '(dolist (directory (list default-directory "test"))
              (dolist (file (directory-files directory t "\\.elc\\'"))
                (delete-file file)))))))

(add-hook 'eask-before-compile-hook
          (lambda ()
            (setq byte-compile-error-on-warn t)))
