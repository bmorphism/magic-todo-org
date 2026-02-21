;;; lux-elisp.el --- Maximal Lux ↔ Elisp bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bmorphism
;; Author: bmorphism
;; Keywords: languages, lux, org-babel, tvraman
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; lux-elisp.el is the integration hub connecting:
;;
;;   lux-mode.el     — font-lock, indentation, syntax table (upstream LuxLang)
;;   lux-tvraman.el  — batch/non-visual structural analysis (T.V. Raman mode)
;;   ob-lux.el       — org-babel src block evaluation
;;   magic-todo-org  — MLX-LM task decomposition (zubyul)
;;
;; The bridge provides:
;;   1. Unified API for all Lux analysis from Elisp
;;   2. magic-todo-org integration (decompose Lux tasks into org checklists)
;;   3. org-babel Lux block support with tvraman lint/fontify/forms
;;   4. Programmatic access to Lux structural data as Elisp objects
;;   5. GF(3) trit annotation via Gay.jl color semantics

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar lux-elisp--dir
  (file-name-directory (or load-file-name buffer-file-name ""))
  "Directory containing the lux-elisp package.")

(add-to-list 'load-path lux-elisp--dir)
(require 'lux-mode)
(require 'lux-tvraman)

(autoload 'org-babel-execute:lux "ob-lux"
  "Execute Lux code via org-babel." t)

;; ══════════════════════════════════════════════════════════════════
;; 1. Unified analysis API (returns Elisp data, not printed text)
;; ══════════════════════════════════════════════════════════════════

(defun lux-elisp/lint (file)
  "Lint FILE, return plist (:ok BOOL :forms N :lines N :error STRING)."
  (let ((output (lux-elisp--capture #'lux-tvraman/lint-file file)))
    (let ((ok (string-match "^OK:" output))
          (forms (when (string-match "forms: \\([0-9]+\\)" output)
                   (string-to-number (match-string 1 output))))
          (lines (when (string-match "lines: \\([0-9]+\\)" output)
                   (string-to-number (match-string 1 output))))
          (err (when (string-match "^ERROR: \\(.+\\)" output)
                 (match-string 1 output))))
      (list :ok (not (null ok)) :forms (or forms 0)
            :lines (or lines 0) :error err))))

(defun lux-elisp/forms (file)
  "Return list of (LINE . HEAD) for top-level forms in FILE."
  (let ((output (lux-elisp--capture #'lux-tvraman/list-forms file)))
    (cl-loop for line in (split-string output "\n" t)
             when (string-match "\\`\\([0-9]+\\) \\(.+\\)\\'" line)
             collect (cons (string-to-number (match-string 1 line))
                           (match-string 2 line)))))

(defun lux-elisp/fontify (file)
  "Return list of plists for font-lock faces in FILE.
Each: (:line N :col N :face SYMBOL :text STRING)"
  (let ((output (lux-elisp--capture #'lux-tvraman/fontify-file file)))
    (cl-loop for line in (split-string output "\n" t)
             when (string-match "\\`\\([0-9]+\\):\\([0-9]+\\) \\(\\S-+\\) \\(.+\\)\\'" line)
             collect (list :line (string-to-number (match-string 1 line))
                           :col (string-to-number (match-string 2 line))
                           :face (intern (match-string 3 line))
                           :text (match-string 4 line)))))

(defun lux-elisp/types (file)
  "Return list of (LINE KIND TYPE-EXPR) for type annotations in FILE."
  (let ((output (lux-elisp--capture #'lux-tvraman/extract-types file)))
    (cl-loop for line in (split-string output "\n" t)
             when (string-match "\\`\\([0-9]+\\) \\(\\S-+\\) \\(.+\\)\\'" line)
             collect (list (string-to-number (match-string 1 line))
                           (match-string 2 line)
                           (match-string 3 line)))))

(defun lux-elisp/modules (file)
  "Return list of (LINE MODULE-PATH) for imports in FILE."
  (let ((output (lux-elisp--capture #'lux-tvraman/extract-modules file)))
    (cl-loop for line in (split-string output "\n" t)
             when (string-match "\\`\\([0-9]+\\) \\(.+\\)\\'" line)
             collect (cons (string-to-number (match-string 1 line))
                           (match-string 2 line)))))

(defun lux-elisp/definitions (file)
  "Return list of plists for definitions in FILE.
Each: (:line N :kind STRING :name STRING :signature STRING)"
  (let ((output (lux-elisp--capture #'lux-tvraman/extract-definitions file)))
    (cl-loop for line in (split-string output "\n" t)
             when (string-match "\\`\\([0-9]+\\) \\(\\S-+\\) \\(\\S-+\\)\\(?: \\(.+\\)\\)?\\'" line)
             collect (list :line (string-to-number (match-string 1 line))
                           :kind (match-string 2 line)
                           :name (match-string 3 line)
                           :signature (or (match-string 4 line) "")))))

(defun lux-elisp/indent (file)
  "Return canonically indented content of FILE as a string."
  (lux-elisp--capture #'lux-tvraman/indent-file file))

(defun lux-elisp/digest (file)
  "Return full structural digest of FILE as a plist."
  (list :lint (lux-elisp/lint file)
        :forms (lux-elisp/forms file)
        :types (lux-elisp/types file)
        :modules (lux-elisp/modules file)
        :definitions (lux-elisp/definitions file)
        :fontify (lux-elisp/fontify file)))

;; ══════════════════════════════════════════════════════════════════
;; 2. magic-todo-org bridge: decompose Lux tasks
;; ══════════════════════════════════════════════════════════════════

(defvar lux-elisp-magic-todo-available nil
  "Non-nil if magic-todo-org is loadable.")

(defun lux-elisp--try-load-magic-todo ()
  "Try to load magic-todo-org from known locations."
  (unless lux-elisp-magic-todo-available
    (let ((candidates '("~/i/magic-todo-org/emacs/magic-todo-org.el"
                         "~/.emacs.d/el_files/magic-todo-org.el")))
      (cl-dolist (path candidates)
        (let ((expanded (expand-file-name path)))
          (when (file-exists-p expanded)
            (add-to-list 'load-path (file-name-directory expanded))
            (require 'magic-todo-org nil t)
            (setq lux-elisp-magic-todo-available t)
            (cl-return t)))))))

(defun lux-elisp/decompose-lux-task (task &optional spice)
  "Use magic-todo-org to decompose a Lux programming TASK.
Returns the plan as an alist, or nil if magic-todo-org unavailable.
SPICE is 1-5 granularity (default 3)."
  (lux-elisp--try-load-magic-todo)
  (if (not lux-elisp-magic-todo-available)
      (progn (message "magic-todo-org not available") nil)
    (let ((spice (or spice 3)))
      (condition-case err
          (funcall (intern "magic-todo-org--call-json")
                   (format "[Lux language] %s" task)
                   spice
                   (symbol-value (intern "magic-todo-org-default-model")))
        (error (message "magic-todo-org error: %s" err) nil)))))

(defun lux-elisp/analyze-and-decompose (file &optional spice)
  "Analyze FILE structurally, then decompose improvement tasks via MLX.
Returns plist with :digest and :plan."
  (let* ((digest (lux-elisp/digest file))
         (forms (plist-get digest :forms))
         (types (plist-get digest :types))
         (defs (plist-get digest :definitions))
         (summary (format "Lux file with %d forms, %d type annotations, %d definitions: %s"
                          (length forms)
                          (length types)
                          (length defs)
                          (mapconcat (lambda (d) (plist-get d :name)) defs ", ")))
         (plan (lux-elisp/decompose-lux-task
                (format "Review and improve this Lux code: %s" summary)
                (or spice 3))))
    (list :digest digest :plan plan)))

;; ══════════════════════════════════════════════════════════════════
;; 3. GF(3) trit annotation
;; ══════════════════════════════════════════════════════════════════

(defconst lux-elisp--trit-table
  '(;; Lux special forms → trit by semantic role
    ;; MINUS (-1): validation, checking, constraints
    ("the" . -1) ("is" . -1) ("as" . -1) ("as_expected" . -1)
    ("type_of" . -1) ("pre" . -1) ("post" . -1)
    ;; ERGODIC (0): control flow, coordination
    ("when" . 0) ("if" . 0) ("unless" . 0) ("exec" . 0) ("let" . 0)
    ("loop" . 0) ("and" . 0) ("or" . 0) (".using" . 0)
    ("comment" . 0) ("open" . 0)
    ;; PLUS (+1): construction, generation
    ("function" . 1) ("macro" . 1) ("method" . 1)
    ("the" . -1) ("every" . 1) ("alias" . 1)
    ("Union" . 1) ("Variant" . 1) ("Record" . 1) ("Tuple" . 1)
    ("Nominal" . 1) ("Interface" . 1) ("type" . 1)
    ("list" . 1) ("sequence" . 1) ("tree" . 1))
  "GF(3) trit assignment for Lux special forms.")

(defun lux-elisp/trit-of (form-head)
  "Return GF(3) trit (-1, 0, +1) for FORM-HEAD."
  (or (cdr (assoc form-head lux-elisp--trit-table)) 0))

(defun lux-elisp/trit-balance (file)
  "Compute GF(3) trit balance of FILE.
Returns plist (:sum N :mod3 N :balanced BOOL :counts (MINUS ZERO PLUS))."
  (let ((forms (lux-elisp/forms file))
        (minus 0) (zero 0) (plus 0) (sum 0))
    (dolist (f forms)
      (let ((trit (lux-elisp/trit-of (cdr f))))
        (cl-incf sum trit)
        (cond ((< trit 0) (cl-incf minus))
              ((> trit 0) (cl-incf plus))
              (t (cl-incf zero)))))
    (list :sum sum :mod3 (mod (+ (mod sum 3) 3) 3)
          :balanced (= 0 (mod (+ (mod sum 3) 3) 3))
          :counts (list minus zero plus))))

;; ══════════════════════════════════════════════════════════════════
;; 4. JSON export (for DuckDB / external tooling)
;; ══════════════════════════════════════════════════════════════════

(defun lux-elisp/digest-json (file)
  "Return full digest of FILE as a JSON string."
  (let ((digest (lux-elisp/digest file))
        (trit (lux-elisp/trit-balance file)))
    (let ((lint-pl (plist-get digest :lint)))
      (json-encode
       `((file . ,file)
         (lint . ((ok . ,(if (plist-get lint-pl :ok) t json-false))
                  (forms . ,(plist-get lint-pl :forms))
                  (lines . ,(plist-get lint-pl :lines))
                  (error . ,(or (plist-get lint-pl :error) json-null))))
         (forms . ,(vconcat
                    (mapcar (lambda (f)
                              `((line . ,(car f))
                                (head . ,(cdr f))
                                (trit . ,(lux-elisp/trit-of (cdr f)))))
                            (plist-get digest :forms))))
         (types . ,(vconcat
                    (mapcar (lambda (tp)
                              `((line . ,(nth 0 tp))
                                (kind . ,(nth 1 tp))
                                (expr . ,(nth 2 tp))))
                            (plist-get digest :types))))
         (modules . ,(vconcat
                      (mapcar (lambda (m)
                                `((line . ,(car m)) (path . ,(cdr m))))
                              (plist-get digest :modules))))
         (definitions . ,(vconcat
                          (mapcar (lambda (d)
                                    `((line . ,(plist-get d :line))
                                      (kind . ,(plist-get d :kind))
                                      (name . ,(plist-get d :name))
                                      (signature . ,(plist-get d :signature))))
                                  (plist-get digest :definitions))))
         (trit_balance . ((sum . ,(plist-get trit :sum))
                          (mod3 . ,(plist-get trit :mod3))
                          (balanced . ,(if (plist-get trit :balanced) t json-false)))))))))

(defun lux-elisp/digest-json-file (file &optional output)
  "Write JSON digest of FILE to OUTPUT (default: FILE.json)."
  (let ((output (or output (concat file ".json")))
        (json-str (lux-elisp/digest-json file)))
    (with-temp-file output (insert json-str))
    (message "Wrote %s" output)
    output))

;; ══════════════════════════════════════════════════════════════════
;; 5. Batch CLI entry points
;; ══════════════════════════════════════════════════════════════════

(defun lux-elisp/batch-digest (file)
  "Batch entry: print full digest to stdout."
  (lux-tvraman/digest file))

(defun lux-elisp/batch-json (file)
  "Batch entry: print JSON digest to stdout."
  (princ (lux-elisp/digest-json file)))

(defun lux-elisp/batch-trit (file)
  "Batch entry: print GF(3) trit balance to stdout."
  (let ((b (lux-elisp/trit-balance file)))
    (princ (format "sum: %d\nmod3: %d\nbalanced: %s\nminus: %d  zero: %d  plus: %d\n"
                   (plist-get b :sum)
                   (plist-get b :mod3)
                   (if (plist-get b :balanced) "YES" "NO")
                   (nth 0 (plist-get b :counts))
                   (nth 1 (plist-get b :counts))
                   (nth 2 (plist-get b :counts))))))

;; ══════════════════════════════════════════════════════════════════
;; Internal
;; ══════════════════════════════════════════════════════════════════

(defun lux-elisp--capture (fn &rest args)
  "Call FN with ARGS, capture all `princ' output as a string."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (apply fn args))
    (buffer-string)))

(provide 'lux-elisp)
;;; lux-elisp.el ends here
