;;; snap-doctest.el --- Inline doctests for snap -*- lexical-binding: t -*-

;; Copyright (c) 2025 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: testing, docs, ert
;; URL: https://github.com/alhassy/snap

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;;
;; Inline doctests extracted from fenced code blocks in docstrings (or
;; any string literal).  A doctest lives inside a ``` fenced block:
;;
;;   (defun my-double (x)
;;     "Double X.
;;
;;   ```
;;   (my-double 3) => 6
;;   (my-double 0) => 0
;;   ```"
;;     (* 2 x))
;;
;; Every line of the form `X => Y' inside a fence becomes an ERT
;; assertion `(should (equal Y X))'.  Multiple assertions per fence and
;; multiple fences per string are both supported.
;;
;; Three entry points:
;;
;;   (snap-doctest-on-string "<some string>")
;;     Run doctests from a string literal at macro-expansion time.
;;
;;   (snap-doctest #'some-fn)          ; or 'some-fn
;;     Look up the docstring of SOME-FN, run its doctests at runtime.
;;
;;   (snap-doctest "path/to/file.el")
;;     Scan every docstring in the file, run all doctests.
;;
;; Typical usage inside an ert-deftest:
;;
;;   (ert-deftest my-double-doctests ()
;;     (snap-doctest #'my-double))
;;
;; Or via snap's `deftest' shorthand:
;;
;;   (deftest "my-double passes its inline doctests" [my-double]
;;     (snap-doctest #'my-double))

;;; Code:

(require 'cl-lib)
(require 'ert)

;; ═══════════════════════════════════════════════════════════════════
;; Layer 1 --- parse fenced blocks out of a string
;; ═══════════════════════════════════════════════════════════════════

(defun snap-doctest--parse-fences (string)
  "string → list of (X-str . Y-str) from all ```...``` blocks in STRING.
Each line inside a fence matching `X => Y' (with optional surrounding
whitespace) becomes one cons cell.  Lines without `=>' are silently
skipped.  Returns nil when STRING contains no fenced blocks."
  (when (stringp string)
    (let ((pairs nil)
          (pos 0))
      (while (string-match "```\\(\\(?:.\\|\n\\)*?\\)```" string pos)
        (let ((block (match-string 1 string)))
          (setq pos (match-end 0))
          (dolist (line (split-string block "\n"))
            (when (string-match
                   "^[ \t]*\\(.*?\\)[ \t]*=>[ \t]*\\(.*?\\)[ \t]*$"
                   line)
              (let ((lhs (string-trim (match-string 1 line)))
                    (rhs (string-trim (match-string 2 line))))
                (unless (or (string-empty-p lhs) (string-empty-p rhs))
                  (push (cons lhs rhs) pairs)))))))
      (nreverse pairs))))

;; ═══════════════════════════════════════════════════════════════════
;; Layer 2 --- macro: compile-time assertions from a string literal
;; ═══════════════════════════════════════════════════════════════════

(defmacro snap-doctest-on-string (string &optional context-label)
  "Expand into ERT assertions for every `X => Y' pair in STRING's fences.
STRING must be a string literal (evaluated at macro-expansion time).
Each pair becomes `(should (equal Y X))'.  When CONTEXT-LABEL is
given, each assertion is wrapped in `(ert-info (CONTEXT-LABEL) ...)'
for readable failure messages."
  (let ((pairs (snap-doctest--parse-fences string)))
    (if (null pairs)
        '(progn)                        ; nothing to assert
      `(progn
         ,@(mapcar
            (lambda (pair)
              (let* ((lhs-form (car (read-from-string (car pair))))
                     (rhs-form (car (read-from-string (cdr pair))))
                     (assertion `(should (equal ,rhs-form ,lhs-form))))
                (if context-label
                    `(ert-info (,context-label) ,assertion)
                  assertion)))
            pairs)))))

;; ═══════════════════════════════════════════════════════════════════
;; Layer 3 --- runtime runner (used when the string is not a literal)
;; ═══════════════════════════════════════════════════════════════════

(defun snap-doctest--run-on-string (string label)
  "string, label → run ERT assertions for all fenced doctests in STRING.
Unlike `snap-doctest-on-string', this operates at runtime and is used
when the string is not known at macro-expansion time (e.g. a function's
docstring looked up via `documentation')."
  (dolist (pair (snap-doctest--parse-fences string))
    (let ((lhs-form (car (read-from-string (car pair))))
          (rhs-form (car (read-from-string (cdr pair)))))
      (ert-info (label)
        (should (equal (eval rhs-form t) (eval lhs-form t)))))))

;; ═══════════════════════════════════════════════════════════════════
;; Layer 4 --- file-level docstring extractor
;; ═══════════════════════════════════════════════════════════════════

(defun snap-doctest--extract-file-strings (path)
  "path → list of (docstring . label) pairs from every defun/defvar/... in PATH.
Walks the top-level forms of PATH, extracts the docstring from
`defun', `defmacro', `cl-defun', `cl-defmacro', `defvar',
`defcustom', and `defconst' forms.  The label is
\"NAME (path:N)\" where N is a running form index."
  (let ((buf (find-file-noselect (expand-file-name path) t t))
        (results nil))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((form-index 0))
          (condition-case nil
              (while t
                (let ((form (read (current-buffer))))
                  (cl-incf form-index)
                  (when (consp form)
                    (let* ((head (car form))
                           (name (and (consp (cdr form))
                                      (symbolp (cadr form))
                                      (cadr form)))
                           (label (format "%s (%s:%d)" name path form-index))
                           (doc
                            (cond
                             ;; defun/defmacro/cl-defun/cl-defmacro:
                             ;; (defun NAME ARGS DOCSTRING ...)
                             ((memq head '(defun defmacro cl-defun cl-defmacro))
                              (let ((maybe (nth 3 form)))
                                (when (stringp maybe) maybe)))
                             ;; defvar/defcustom/defconst:
                             ;; (defvar NAME VALUE DOCSTRING ...)
                             ((memq head '(defvar defcustom defconst defvar-local))
                              (let ((maybe (nth 3 form)))
                                (when (stringp maybe) maybe))))))
                      (when (and doc (snap-doctest--parse-fences doc))
                        (push (cons doc label) results))))))
            (end-of-file nil)))))
    (nreverse results)))

;; ═══════════════════════════════════════════════════════════════════
;; Layer 5 --- top-level entry point
;; ═══════════════════════════════════════════════════════════════════

(defmacro snap-doctest (target)
  "Run doctests from TARGET's fenced ```...``` blocks.

TARGET may be:

  #\\='SYMBOL or \\='SYMBOL
    Look up the docstring of SYMBOL at runtime and assert all `X => Y'
    pairs in its fenced blocks.

  \"path/to/file.el\"
    Scan every `defun'/`defvar'/... docstring in the file and assert all
    fenced doctests.  The file is scanned at macro-expansion time."
  (cond
   ;; (snap-doctest #'foo) or (snap-doctest 'foo) --- runtime lookup
   ((and (consp target)
         (memq (car target) '(function quote)))
    (let ((sym (cadr target)))
      `(let* ((--doc (or (ignore-errors (documentation ',sym))
                         (ignore-errors (documentation ',sym t))
                         (error "snap-doctest: no docstring for `%s'" ',sym)))
              (--label (format "docstring of %s" ',sym)))
         (snap-doctest--run-on-string --doc --label))))

   ;; (snap-doctest "file.el") --- file scan at macro-expansion time
   ((stringp target)
    (let ((pairs (snap-doctest--extract-file-strings target)))
      (if (null pairs)
          '(progn)
        `(progn
           ,@(mapcar (lambda (pair)
                       `(snap-doctest--run-on-string ,(car pair) ,(cdr pair)))
                     pairs)))))

   (t
    (error "snap-doctest: TARGET must be #'SYMBOL, 'SYMBOL, or a file path string"))))

(provide 'snap-doctest)
;;; snap-doctest.el ends here
