;;; lux-tvraman.el --- T.V. Raman mode: batch Emacs driver for lux-mode -*- lexical-binding: t; -*-
;;
;; Named after T.V. Raman, who proved that Emacs is the
;; ultimate non-visual interface to structured text.
;;
;; Usage:
;;   emacs --batch -l lux-tvraman.el --eval '(lux-tvraman/indent-file "FILE")'
;;   emacs --batch -l lux-tvraman.el --eval '(lux-tvraman/fontify-file "FILE")'
;;   emacs --batch -l lux-tvraman.el --eval '(lux-tvraman/lint-file "FILE")'
;;   emacs --batch -l lux-tvraman.el --eval '(lux-tvraman/list-forms "FILE")'
;;   emacs --batch -l lux-tvraman.el --eval '(lux-tvraman/imenu-index "FILE")'

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'lux-mode)

(defun lux-tvraman/indent-file (file)
  "Indent FILE according to lux-mode and write result to stdout."
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (indent-region (point-min) (point-max))
    (princ (buffer-string))))

(defun lux-tvraman/indent-file-inplace (file)
  "Indent FILE in-place according to lux-mode."
  (find-file file)
  (lux-mode)
  (indent-region (point-min) (point-max))
  (save-buffer)
  (message "Indented: %s" file)
  (kill-buffer))

(defun lux-tvraman/fontify-file (file)
  "Fontify FILE and print face annotations to stdout.
Output format: LINE:COL FACE TEXT"
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((result '()))
      (while (not (eobp))
        (let ((face (get-text-property (point) 'face))
              (start (point)))
          (if face
              (let ((end (next-single-property-change (point) 'face nil (point-max))))
                (push (format "%d:%d %s %s"
                              (line-number-at-pos start)
                              (- start (line-beginning-position))
                              face
                              (buffer-substring-no-properties start end))
                      result)
                (goto-char end))
            (forward-char))))
      (dolist (line (nreverse result))
        (princ line)
        (princ "\n")))))

(defun lux-tvraman/sexp-at-point (file line col)
  "Print the sexp at LINE COL in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
      (if bounds
          (princ (buffer-substring-no-properties (car bounds) (cdr bounds)))
        (princ "NO-SEXP")))))

(defun lux-tvraman/list-forms (file)
  "List top-level forms in FILE. Output: LINE FORM-HEAD"
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (condition-case nil
          (let ((start (point)))
            (when (eq (char-after) ?\()
              (let ((line (line-number-at-pos)))
                (forward-char)
                (let ((head-start (point)))
                  (forward-sexp)
                  (let ((head (buffer-substring-no-properties head-start (point))))
                    (princ (format "%d %s\n" line head))))))
            (forward-sexp)
            (skip-chars-forward " \t\n"))
        (scan-error (goto-char (point-max)))))))

(defun lux-tvraman/lint-file (file)
  "Basic structural lint: check balanced parens, report form counts."
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (condition-case err
        (progn
          (check-parens)
          (princ "OK: parens balanced\n"))
      (error
       (princ (format "ERROR: %s\n" (error-message-string err)))))
    (goto-char (point-min))
    (let ((forms 0) (lines 0))
      (while (not (eobp))
        (condition-case nil
            (progn (forward-sexp) (setq forms (1+ forms)))
          (scan-error (goto-char (point-max)))))
      (setq lines (count-lines (point-min) (point-max)))
      (princ (format "forms: %d\nlines: %d\n" forms lines)))))

(defun lux-tvraman/imenu-index (file)
  "Print imenu index (definitions) for FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (let ((index (funcall imenu-create-index-function)))
      (dolist (entry index)
        (princ (format "%s @ %d\n" (car entry) (cdr entry)))))))

;; ══════════════════════════════════════════════════════════════════
;; Deep structural analysis
;; ══════════════════════════════════════════════════════════════════

(defun lux-tvraman/extract-types (file)
  "Extract type definitions and annotations from FILE.
Output: LINE KIND NAME"
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (goto-char (point-min))
    (while (re-search-forward
            "(\\(?:the\\|type\\|Nominal\\|Interface\\|Union\\|Variant\\|Record\\|Tuple\\)\\>" nil t)
      (let ((kind (match-string 0))
            (line (line-number-at-pos (match-beginning 0))))
        (save-excursion
          (goto-char (match-end 0))
          (skip-chars-forward " \t\n")
          (let ((start (point)))
            (condition-case nil
                (progn
                  (forward-sexp)
                  (princ (format "%d %s %s\n" line
                                 (string-trim kind "(")
                                 (buffer-substring-no-properties start (point)))))
              (scan-error nil))))))))

(defun lux-tvraman/extract-modules (file)
  "Extract .using module imports from FILE.
Output: LINE MODULE"
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (goto-char (point-min))
    (while (re-search-forward "\\.using" nil t)
      (let ((line (line-number-at-pos)))
        (skip-chars-forward " \t\n")
        (when (eq (char-after) ?\[)
          (let ((start (point)))
            (condition-case nil
                (progn
                  (forward-sexp)
                  (let ((imports (buffer-substring-no-properties (1+ start) (1- (point)))))
                    (dolist (mod (split-string imports "[\n\t ]+" t))
                      (when (string-match "\\[?\\([a-z/._]+\\)" mod)
                        (princ (format "%d %s\n" line (match-string 1 mod)))))))
              (scan-error nil))))))))

(defun lux-tvraman/extract-definitions (file)
  "Extract function and value definitions from FILE.
Output: LINE DEF-TYPE NAME SIGNATURE"
  (with-temp-buffer
    (insert-file-contents file)
    (lux-mode)
    (goto-char (point-min))
    (let ((in-the nil) (the-line 0) (the-sig ""))
      (while (not (eobp))
        (condition-case nil
            (progn
              (skip-chars-forward " \t\n")
              (when (eq (char-after) ?\()
                (let ((form-start (point))
                      (line (line-number-at-pos)))
                  (forward-char)
                  (let ((head-start (point)))
                    (forward-sexp)
                    (let ((head (buffer-substring-no-properties head-start (point))))
                      (cond
                       ((string= head "the")
                        (skip-chars-forward " \t\n")
                        (let ((sig-start (point)))
                          (condition-case nil
                              (progn
                                (forward-sexp)
                                (setq in-the t
                                      the-line line
                                      the-sig (buffer-substring-no-properties sig-start (point))))
                            (scan-error nil))))
                       ((string= head "function")
                        (skip-chars-forward " \t\n")
                        (when (eq (char-after) ?\()
                          (forward-char)
                          (let ((name-start (point)))
                            (forward-sexp)
                            (let ((name (buffer-substring-no-properties name-start (point))))
                              (princ (format "%d function %s %s\n"
                                             (if in-the the-line line)
                                             name
                                             (if in-the the-sig "")))
                              (setq in-the nil the-sig "")))))
                       ((string-match "\\`\\(?:the\\|every\\|alias\\)\\'" head)
                        nil)
                       (t (setq in-the nil the-sig "")))))))
              (condition-case nil (forward-sexp) (scan-error (forward-char))))
          (scan-error (goto-char (point-max))))))))

(defun lux-tvraman/sexp-tree (file &optional max-depth)
  "Print the s-expression tree of FILE up to MAX-DEPTH.
Output: DEPTH FORM-HEAD"
  (let ((max-depth (or max-depth 3)))
    (with-temp-buffer
      (insert-file-contents file)
      (lux-mode)
      (goto-char (point-min))
      (lux-tvraman--walk-sexps 0 max-depth))))

(defun lux-tvraman--walk-sexps (depth max-depth)
  "Walk s-expressions at DEPTH, recurse to MAX-DEPTH."
  (while (not (eobp))
    (skip-chars-forward " \t\n")
    (when (eobp) (cl-return))
    (condition-case nil
        (cond
         ((eq (char-after) ?\()
          (let ((start (point))
                (line (line-number-at-pos)))
            (forward-char)
            (skip-chars-forward " \t\n")
            (let ((head-start (point)))
              (condition-case nil
                  (progn
                    (forward-sexp)
                    (let ((head (buffer-substring-no-properties head-start (point))))
                      (princ (format "%s%d %s\n"
                                     (make-string (* 2 depth) ?\s)
                                     line head))
                      (when (< depth max-depth)
                        (lux-tvraman--walk-sexps (1+ depth) max-depth))))
                (scan-error nil)))
            (condition-case nil
                (progn (goto-char start) (forward-sexp))
              (scan-error (goto-char (point-max))))))
         (t (forward-sexp)))
      (scan-error (goto-char (point-max))))))

(defun lux-tvraman/digest (file)
  "Full structural digest of FILE: lint + forms + types + modules + definitions.
Single-call summary for programmatic consumers."
  (princ "=== LINT ===\n")
  (lux-tvraman/lint-file file)
  (princ "\n=== FORMS ===\n")
  (lux-tvraman/list-forms file)
  (princ "\n=== TYPES ===\n")
  (lux-tvraman/extract-types file)
  (princ "\n=== MODULES ===\n")
  (lux-tvraman/extract-modules file)
  (princ "\n=== DEFINITIONS ===\n")
  (lux-tvraman/extract-definitions file))

(provide 'lux-tvraman)
