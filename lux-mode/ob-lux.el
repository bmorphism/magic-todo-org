;;; ob-lux.el --- Org-babel functions for Lux -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bmorphism
;; Author: bmorphism
;; Keywords: literate programming, Lux, org-babel
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;; Org-babel support for the Lux programming language (LuxLang/lux).
;; Lux is a functional, statically-typed Lisp targeting JVM/JS/Python/Lua/Ruby.
;;
;; Uses lux-tvraman batch primitives for structural analysis,
;; and lux-mode for font-lock + indentation inside src blocks.

;;; Code:

(require 'ob)
(require 'org-macs)

(declare-function lux-mode "lux-mode" ())
(declare-function lux-tvraman/fontify-file "lux-tvraman" (file))
(declare-function lux-tvraman/lint-file "lux-tvraman" (file))
(declare-function lux-tvraman/list-forms "lux-tvraman" (file))

(defgroup ob-lux nil
  "Org-babel for the Lux programming language."
  :group 'org-babel
  :prefix "ob-lux-")

(defcustom ob-lux-backend "jvm"
  "Default Lux compilation backend.
One of: jvm, js, python, lua, ruby."
  :type '(choice (const "jvm") (const "js") (const "python")
                 (const "lua") (const "ruby"))
  :group 'ob-lux)

(defcustom ob-lux-lein-command "lein"
  "Command to invoke Leiningen for Lux compilation."
  :type 'string
  :group 'ob-lux)

(defcustom ob-lux-timeout 60
  "Timeout in seconds for Lux evaluation."
  :type 'integer
  :group 'ob-lux)

(defvar ob-lux--tvraman-loaded nil)

(defun ob-lux--ensure-tvraman ()
  "Load lux-tvraman if not already loaded."
  (unless ob-lux--tvraman-loaded
    (let ((dir (file-name-directory (or load-file-name buffer-file-name
                                       (locate-library "ob-lux")
                                       ""))))
      (when dir
        (add-to-list 'load-path dir)
        (require 'lux-tvraman nil t)
        (require 'lux-mode nil t)
        (setq ob-lux--tvraman-loaded t)))))

(defvar org-babel-default-header-args:lux
  '((:results . "output")
    (:backend . nil)
    (:lint . "yes")
    (:module . nil))
  "Default header arguments for Lux src blocks.")

(defun org-babel-expand-body:lux (body params)
  "Expand BODY with PARAMS for Lux.
Wraps bare expressions in a module if :module is specified."
  (let ((module (cdr (assq :module params)))
        (vars (org-babel--get-vars params)))
    (when vars
      (setq body
            (concat
             (mapconcat
              (lambda (v)
                (format "(the .Any (the .Any %S))\n... %s = %s"
                        (symbol-name (car v))
                        (symbol-name (car v))
                        (org-babel-lux--val-to-lux (cdr v))))
              vars "\n")
             "\n" body)))
    (if module
        (format "(.using [library/lux])\n\n%s" body)
      body)))

(defun org-babel-lux--val-to-lux (val)
  "Convert Elisp VAL to Lux literal."
  (cond
   ((integerp val) (format "%+d" val))
   ((floatp val) (format "%+f" val))
   ((stringp val) (format "%S" val))
   ((eq val t) ".true")
   ((null val) ".false")
   ((listp val)
    (format "(list %s)"
            (mapconcat #'org-babel-lux--val-to-lux val " ")))
   (t (format "%S" val))))

(defun org-babel-execute:lux (body params)
  "Execute Lux BODY with PARAMS via org-babel.

Supported header args:
  :backend  jvm|js|python|lua|ruby (default: ob-lux-backend)
  :lint     yes|no  (default: yes, run structural lint first)
  :results  output|value
  :module   module-name (wraps body in .using preamble)
  :eval     lint|fontify|forms|indent (tvraman analysis modes)"
  (ob-lux--ensure-tvraman)
  (let* ((eval-mode (cdr (assq :eval params)))
         (backend (or (cdr (assq :backend params)) ob-lux-backend))
         (do-lint (not (string= "no" (or (cdr (assq :lint params)) "yes"))))
         (expanded (org-babel-expand-body:lux body params))
         (tmp (org-babel-temp-file "lux-" ".lux")))
    (with-temp-file tmp (insert expanded))
    (cond
     ;; tvraman analysis modes
     ((string= eval-mode "lint")
      (ob-lux--tvraman-run "lint-file" tmp))
     ((string= eval-mode "fontify")
      (ob-lux--tvraman-run "fontify-file" tmp))
     ((string= eval-mode "forms")
      (ob-lux--tvraman-run "list-forms" tmp))
     ((string= eval-mode "indent")
      (ob-lux--tvraman-run "indent-file" tmp))
     ;; actual compilation/evaluation
     (t
      (let ((lint-result (when do-lint (ob-lux--tvraman-run "lint-file" tmp))))
        (when (and lint-result (string-match "^ERROR:" lint-result))
          (error "Lux lint failed:\n%s" lint-result))
        (ob-lux--compile-and-run tmp backend expanded))))))

(defun ob-lux--tvraman-run (fn file)
  "Run lux-tvraman/FN on FILE via batch Emacs, return stdout."
  (let* ((tvraman-el (expand-file-name
                      "lux-tvraman.el"
                      (file-name-directory
                       (or load-file-name
                           (locate-library "ob-lux")
                           ""))))
         (cmd (format "emacs --batch -l %s --eval '(lux-tvraman/%s %S)' 2>/dev/null"
                      (shell-quote-argument tvraman-el)
                      fn
                      file)))
    (string-trim (shell-command-to-string cmd))))

(defun ob-lux--compile-and-run (file backend body)
  "Compile FILE for BACKEND and run it. Return output string."
  (pcase backend
    ("jvm"
     (ob-lux--run-via-jvm file))
    ((or "js" "python" "lua" "ruby")
     (ob-lux--run-via-script backend file body))
    (_
     (format "Unknown backend: %s\n\n--- Structural Analysis (tvraman) ---\n%s"
             backend
             (ob-lux--tvraman-run "lint-file" file)))))

(defun ob-lux--run-via-jvm (file)
  "Compile and run Lux FILE via JVM backend."
  (let* ((dir (file-name-directory file))
         (cmd (format "cd %s && %s lux auto build 2>&1 | head -50"
                      (shell-quote-argument dir)
                      ob-lux-lein-command))
         (output (with-timeout (ob-lux-timeout "Lux compilation timed out")
                   (shell-command-to-string cmd))))
    (if (string-match "Compilation error" output)
        (format "COMPILATION ERROR:\n%s" output)
      output)))

(defun ob-lux--run-via-script (backend file body)
  "For script backends, show structural analysis + the code.
Full compilation requires a Lux project; for now, analyze structurally."
  (let ((lint (ob-lux--tvraman-run "lint-file" file))
        (forms (ob-lux--tvraman-run "list-forms" file))
        (fontify (ob-lux--tvraman-run "fontify-file" file)))
    (format "--- Lux (%s) Structural Analysis ---\n\n%s\n\n--- Forms ---\n%s\n\n--- Font-lock ---\n%s"
            backend lint forms fontify)))

(defun org-babel-prep-session:lux (_session _params)
  "Lux does not support sessions."
  (error "Lux does not currently support sessions"))

(add-to-list 'org-src-lang-modes '("lux" . lux))

(provide 'ob-lux)
;;; ob-lux.el ends here
