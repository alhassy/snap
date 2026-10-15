;;; snap.el --- Snapshot Testing for Emacs Lisp. -*- lexical-binding: t -*-

;; Copyright (c) 2025 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.1
;; Package-Requires: ((s "1.13.1") (emacs "27.1"))
;; Keywords: testing, snapshot, ert
;; URL: https://github.com/alhassy/snap

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;;
;; Snapshot testing infrastructure for ERT.  Three layers, plus an
;; optional doctest layer (`snap-doctest.el').  The full progression
;; from "one example in a docstring" to "a full snapshot test file":
;;
;;   ┌─────────────────────────────────────────────────────────────────┐
;;   │  Write a docstring with a fenced example                        │
;;   │                                                                 │
;;   │    (defun double (x) "2× X.                                     │
;;   │    ```                                                          │
;;   │    (double 3) => 6                                              │
;;   │    ```"  (* 2 x))                                               │
;;   │                          │                                      │
;;   │                          ▼  snap-doctest                        │
;;   │  Run as an ERT test ---one line in any ert-deftest              │
;;   │                                                                 │
;;   │    (deftest "double doctests" (snap-doctest #'double))          │
;;   │                          │                                      │
;;   │                          │  too many examples for one docstring │
;;   │                          ▼  → move to a dedicated test file     │
;;   │                                                                 │
;;   │  snap-define-fixture / deftest --- everyday unit tests          │
;;   │                                                                 │
;;   │    (deftest "double 3" (should (= (double 3) 6)))               │
;;   │                          │                                      │
;;   │                          │  expected value is complex /         │
;;   │                          │  drifts when the function changes    │
;;   │                          ▼  → let snap maintain the snapshot    │
;;   │                                                                 │
;;   │  snap-define-relation --- C-u C-x C-e auto-fills :expected      │
;;   │                                                                 │
;;   │    (define-double-test "small" :input 3 :expected 6)            │
;;   └─────────────────────────────────────────────────────────────────┘
;;
;; The layers in brief:
;;
;; 0. `snap-doctest' (snap-doctest.el) — extract `X => Y' assertions
;;    from ```...``` fenced blocks in any docstring or string literal.
;;    Zero-ceremony: examples live right next to the code they document.
;;
;; 1. `snap-define-fixture' — macro-generating macro.  Wraps test bodies in
;;    a fixture (setup/teardown), generates a test macro with
;;    string-based naming and auto-derived tags, and registers it so
;;    `C-x C-e' both defines and runs the test in one keystroke.
;;
;; 2. `snap-define-relation' — relational test fixtures with snapshot
;;    updating.  Declares a relation among parameters (some inputs,
;;    some outputs), generates a test macro, a compute function (with
;;    `ert-fail' suppressed for snapshot update), `C-u C-x C-e'
;;    plumbing, and [Show Diff] / [Update Snapshot] buttons on failure.
;;
;; Users typically define a convenience test shorthand:
;;   (snap-define-fixture deftest)
;; This is the identity fixture — no wrapping, readable ERT names.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 's)
(require 'seq)
(require 'fancy-C-x-C-e)

;; magit is optional: used only for pretty diffs in `snap-define-relation--show-diff'.
;; Declare the two functions we call so the byte-compiler does not warn.
(declare-function magit-diff-mode "magit-diff" ())
(declare-function magit-diff-wash-diffs "magit-diff" (args &optional limit))

;; ═══════════════════════════════════════════════════════════════════
;; C-x C-e extensions (via fancy-C-x-C-e)
;; ═══════════════════════════════════════════════════════════════════
;;
;; Each macro defined by `snap-define-fixture' / `snap-define-relation'
;; registers a C-x C-e extension via `fancy-C-x-C-e-register'.  The
;; extensions are installed at macro-definition time (inside the
;; `snap-define-fixture' / `snap-define-relation' bodies below) so
;; they exist as soon as the user evaluates the fixture declaration.

;; ═══════════════════════════════════════════════════════════════════
;; snap-define-fixture — macro-generating macro for test fixtures
;; ═══════════════════════════════════════════════════════════════════

(defvar snap-deftest-space "·"
  "Character used in place of whitespace in test names.
The default is interpunct style (middle dot).  Alternatives:
  - Hyphens:      OSBE-org-source-exports
  - Snakecase:    OSBE_org_source_exports
  - Underbracket: OSBE␣org␣source␣exports")

(defun snap--name-replacements ()
  "Return the alist used to sanitise description strings into ERT name-safe form."
  `((" " . ,snap-deftest-space)
    ("`" . "") ("'" . "")
    ("," . "︐") ("`" . "‵")
    (";" . "︔") ("[" . "⁅") ("]" . "⁆")))

(defun snap--test-name (description provided-tags)
  "Compute the ERT test name string from DESCRIPTION and PROVIDED-TAGS."
  (let ((replacements (snap--name-replacements)))
    (concat
     (if provided-tags
         (format "%s··⇨··"
                 (s-join "︐" (mapcar #'prin1-to-string provided-tags)))
       "")
     (thread-last description s-trim s-collapse-whitespace
                  (s-replace-all replacements)))))

(defun snap--method-tag (description)
  "Extract the first word of DESCRIPTION as the auto-derived ERT tag."
  (thread-last description s-trim (s-split " ") car
               (s-replace-all (snap--name-replacements))))

(defun snap--fixture-keys (arglist)
  "The keywords of the `&key' ARGLIST; e.g., (&key text (at \"x\")) ↦ (:text :at)."
  (mapcar (lambda (arg) (intern (format ":%s" (if (consp arg) (car arg) arg))))
          (cdr arglist)))

(defun snap--split-fixture-args (keys body)
  "(KEYS, BODY) ↦ (ARGS . REST): BODY's leading KEY VALUE pairs, for KEYS, and the rest of BODY.
  (:text \"hi\" :at \"h\" (should t)) ↦ ((:text \"hi\" :at \"h\") (should t))
Other leading keywords, such as ERT's :expected-result, stay in REST."
  (let (args)
    (while (memq (car body) keys)
      (setq args (append args (list (pop body) (pop body)))))
    (cons args body)))

(cl-defmacro snap-define-fixture (name &rest spec)
  "Define a new test macro NAME that wraps each test body in FIXTURE-FORM.
A test fixture is the fixed baseline state that tests run against --
the setup that must be in place before the test logic executes.  By
way of example: a temp directory, a database connection, or config
variable bindings.  The name reads as \"define a test-with-fixture
macro\", which is precisely what it does.

ERT (6.2) deliberately omits fixture syntax, noting that
`with-my-fixture' macros are the idiomatic Lisp approach and that
\"special syntax for them could be added but would provide only a
minor simplification.\"  This macro is that minor simplification:
it defines the `with-my-fixture' wrapper and wires it into
`ert-deftest' -- with string-based naming and auto-derived tags --
in one step.

Each generated macro is also registered so that `C-x C-e'
\(`eval-last-sexp') both defines and runs the test in one
keystroke -- no separate `M-x ert' invocation needed.

NAME is the symbol for the generated macro.  SPEC is
  [DOCSTRING] [ARGLIST] FIXTURE-FORM
each part optional.  With no FIXTURE-FORM, the generated macro has no
fixture -- its body is spliced directly into `ert-deftest'.

FIXTURE-FORM is an expression with a hole for the body -- represented
by the symbol `&body'.

ARGLIST, of the shape (&key ARG …) with each ARG a symbol or (SYMBOL
DEFAULT), lets each test pass the fixture its own values; FIXTURE-FORM
sees them as variables.  A test passes them as keyword arguments right
after its TAGS, before its body.

The generated macro has the signature:
  (NAME DESCRIPTION &optional TAGS [:KEY VALUE]… &rest BODY)
where DESCRIPTION is a string and TAGS is an optional vector.

Users typically define their own convenience shorthand:
  (snap-define-fixture deftest)
This is the identity fixture — no wrapping, readable ERT names.
Unlike raw `ert-deftest', test names are natural-language strings
(not hand-mangled symbols), tags are auto-derived from the first
word, and the boilerplate docstring + :tags declaration disappears.

Example -- define `defworkitemtest' with a docstring and fixture:
  (snap-define-fixture defworkitemtest
    \"Bind Gerrit/Jira config vars to safe test defaults.\"
    (let ((my\\\\jira-ticket-regex ...)
          (my\\\\gerrit-base-url ...))
      &body))

Then:
  (defworkitemtest \"work-item shows Jira ticket\" [work-item]
    (should ...))

expands to an `ert-deftest' wrapped in the fixture bindings.

Example -- a fixture taking arguments: a fresh Org buffer holding TEXT,
with point just after AT:
  (snap-define-fixture org-deftest
    (&key (text \"\") at)
    (with-temp-buffer
      (org-mode)
      (insert text)
      (goto-char (point-min))
      (when at (search-forward at))
      &body))

Then:
  (org-deftest \"a heading is a heading\" :text \"* Hi\" :at \"H\"
    (should (org-at-heading-p)))

Snapshot testing pattern:
For relational tests -- where inputs produce expected outputs and
you want `C-u C-x C-e' to auto-fill the expectations -- use
`snap-define-relation' instead.  It generates a test macro with
snapshot update, magit-styled [Show Diff], and [Update Snapshot]
buttons in ERT failure output."
  (declare (indent 1))
  ;; SPEC is [DOCSTRING] [ARGLIST] FIXTURE-FORM; an ARGLIST is recognised by its leading `&key'.
  (let* ((user-docstring (when (stringp (car spec)) (pop spec)))
         (arglist        (when (eq (car-safe (car spec)) '&key) (pop spec)))
         (fixture-keys   (snap--fixture-keys arglist))
         (fixture-form   (car spec))
         (ert-usage "\n\n\
BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not', `should-error', `skip-when', and
`skip-unless' are useful for assertions in BODY.

Use `ert' to run tests interactively.  Even easier: `C-x C-e'
at the end of a test form both defines and runs it in one
keystroke, courtesy of `snap--registry'.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

Macros in BODY are expanded when the test is defined, not when it
is run.  If a macro (possibly with side effects) is to be tested,
it has to be wrapped in `(eval (quote ...))'.")
         (docstring
          (concat
           (or user-docstring
               (format "Test macro generated by `snap-define-fixture' (snap).%s"
                       (if fixture-form
                           (format "\nFixture: %S" fixture-form)
                         "")))
           ert-usage))
         (assert-msg
          (format "\"%s\" expects its first argument to be a string literal"
                  name))
         (wrap-expr
          (cond (arglist
                 ;; The test's leading :KEY VALUE pairs, evaluated as it runs, bind the ARGLIST.
                 `(list
                   `(cl-destructuring-bind ,',arglist (list ,@fixture-args)
                      ,(cl-subst (cons 'progn actual-body) '&body ',fixture-form))))
                (fixture-form
                 `(list
                   (cl-subst (cons 'progn actual-body) '&body
                             ',fixture-form)))
                (t 'actual-body))))
    `(progn
       (fancy-C-x-C-e-register ',name
         (lambda (form)
           (let ((expanded (macroexpand-1 form)))
             (when (and (consp expanded)
                        (eq (car expanded) 'ert-deftest))
               (message "Running test: %s" (cadr expanded))
               (ert (cadr expanded))))))
       (cl-defmacro ,name (description &optional tags &rest body)
         ,docstring
         (declare (indent defun))
         (cl-assert (stringp description) nil ,assert-msg)
         (let*
             ((provided-tags (when (vectorp tags) (append tags nil)))
              (test-name (snap--test-name description provided-tags))
              (method-being-tested (snap--method-tag description))
              ;; The body's leading keyword pairs are the fixture's arguments, or ERT's own; e.g., :expected-result.
              (split (snap--split-fixture-args ',(cons :expected-result fixture-keys)
                                               (if (vectorp tags) body (cons tags body))))
              (fixture-args (cl-loop for (key value) on (car split) by #'cddr
                                     unless (eq key :expected-result) append (list key value)))
              (ert-keywords (cl-loop for (key value) on (car split) by #'cddr
                                     when (eq key :expected-result) append (list key value)))
              (actual-body (cdr split))
              (wrapped-body ,wrap-expr))
           `(ert-deftest ,(intern test-name) ()
              :tags ',(cons method-being-tested provided-tags)
              ,@ert-keywords
              ,@wrapped-body))))))

;; ═══════════════════════════════════════════════════════════════════
;; snap-define-relation -- relational test fixtures with snapshot updating
;; ═══════════════════════════════════════════════════════════════════

(defun snap-define-relation--show-diff (expected actual)
  "Show a diff of EXPECTED vs ACTUAL in a dedicated buffer.
When magit is available, uses `magit-diff-mode' and
`magit-diff-wash-diffs' for syntax highlighting, collapsible hunks,
and magit keybindings.  Falls back to plain diff output otherwise."
  (let ((file-a (make-temp-file "snapshot-expected-"))
        (file-b (make-temp-file "snapshot-actual-")))
    (with-temp-file file-a (insert expected))
    (with-temp-file file-b (insert actual))
    (let ((buf (get-buffer-create "*Snapshot Diff*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (shell-command-to-string
                   (format "git diff --no-index --color=never -- %s %s"
                           (shell-quote-argument file-a)
                           (shell-quote-argument file-b))))
          (goto-char (point-min))
          (when (require 'magit-diff nil t)
            (magit-diff-mode)
            (magit-diff-wash-diffs '("--no-index")))))
      (delete-file file-a)
      (delete-file file-b)
      (pop-to-buffer buf))))

(defun snap-equal-modulo (expected actual patterns)
  "Non-nil iff EXPECTED and ACTUAL are `equal' after each regex in
PATTERNS is replaced with a sentinel in both strings.

PATTERNS may be a single regex string or a list of them.  Every
match of every regex in each argument is collapsed to the same
sentinel before comparison, so drifting substrings (timestamps,
UUIDs, random IDs) are neutralised.  When EXPECTED or ACTUAL is
not a string, falls back to plain `equal'."
  (if (and (stringp expected) (stringp actual))
      (let* ((pats (if (listp patterns) patterns (list patterns)))
             (sentinel "\0SNAP-MODULO\0")
             (norm (lambda (s)
                     (cl-reduce
                      (lambda (acc re)
                        (replace-regexp-in-string re sentinel acc))
                      pats :initial-value s))))
        (equal (funcall norm expected) (funcall norm actual)))
    (equal expected actual)))

(cl-defmacro snap-define-relation (name params docstring &rest body)
  "Define a relational test fixture named NAME with PARAMS.

This is the declarative counterpart to `snap-define-fixture': where
`snap-define-fixture' wraps arbitrary test bodies in a fixture,
`snap-define-relation' declares a *relation* among parameters -- some
inputs, some outputs -- and generates a test macro, a compute
function (for snapshot update), and all the `C-u C-x C-e'
plumbing automatically.

NAME is a symbol (e.g., `work-item').  The generated test macro
is `define-NAME-test'; the internal checker is `NAME--check'.
Every generated test is automatically tagged with NAME, so you
can run the whole relation with:

  M-x ert RET (tag work-item) RET

PARAMS is a list of symbols.  Each becomes a keyword argument in
the generated test macro (`:stack', `:results-in', etc.).  The
body receives them as lexical bindings.

DOCSTRING documents the relation.

BODY performs the relational check.  It may call `should' for
assertions and MUST return a plist whose keys (a keyword subset
of PARAMS) are the **output keys** -- the expectations that
`C-u C-x C-e' can auto-fill from the actual computed values.
By way of example, `(list :results-in rendered)' declares
`:results-in' as the output key with `rendered' as the actual
value.

During snapshot update (`C-u C-x C-e'), the body executes with
`ert-fail' suppressed (`cl-letf'), so `should' becomes a no-op
and the body runs to completion -- returning the output plist
even when the assertion would fail.

Example:

  (snap-define-relation widget (input expected-output)
    \"Verify INPUT transforms to EXPECTED-OUTPUT.\"
    (let ((actual (transform input)))
      (should (equal expected-output actual))
      (list :expected-output actual)))

  ;; Generates `define-widget-test'; run all widget tests with:
  ;;   M-x ert RET (tag widget) RET
  (define-widget-test \"round-trip preserves data\"
    :input \"hello\"
    :expected-output \"HELLO\")

  ;; C-u C-x C-e rewrites :expected-output from actual."
  (declare (indent defun))
  (let* ((check-fn    (intern (format "%s--check" name)))
         (compute-fn  (intern (format "%s--compute" name)))
         (update-at   (intern (format "%s--update-at" name)))
         (update-fn   (intern (format "%s--update" name)))
         (update-by   (intern (format "%s--update-by-name" name)))
         (test-macro  (intern (format "define-%s-test" name)))
         (param-keywords (mapcar (lambda (p) (intern (format ":%s" p))) params))
         (assert-msg  (format "\"%s\" expects its first argument to be a string literal"
                              test-macro)))
    `(progn
       ;; -- 1. The checker: body verbatim, params as arguments --
       (defun ,check-fn ,params ,docstring ,@body)

       ;; -- 2. Compute: call checker with ert-fail suppressed --
       (defun ,compute-fn (plist)
         ,(format "Call `%s' with `should' suppressed; return output plist." check-fn)
         (cl-letf (((symbol-function 'ert-fail) #'ignore)
                   ((symbol-function 'ert--signal-should-execution) #'ignore))
           (,check-fn ,@(cl-loop for kw in param-keywords
                                 collect `(plist-get plist ,kw)))))

       ;; -- 3. Update helpers --
       (defun ,update-at (form-start)
         ,(format "Rewrite output keys of the `%s' form at FORM-START." test-macro)
         (save-excursion
           (goto-char form-start)
           (let* ((form (sexp-at-point))
                  (rest (cddr form))
                  (plist (if (vectorp (car rest)) (cdr rest) rest))
                  (outputs (,compute-fn plist)))
             ;; For each output key, find and replace its value in the buffer.
             (cl-loop for (key val) on outputs by #'cddr do
                      (goto-char form-start)
                      (search-forward (format "%s" key))
                      (forward-sexp)
                      (let ((end (point)))
                        (backward-sexp)
                        (delete-region (point) end)
                        (insert (prin1-to-string val))))
             ;; Re-eval the updated form and run the generated ERT test.
             ;; We eval the sexp directly (not via eval-last-sexp) to avoid
             ;; re-triggering the fancy-C-x-C-e dispatch.  "Test redefined"
             ;; errors are suppressed since we intentionally re-register the
             ;; updated snapshot values.
             (goto-char form-start)
             (let* ((updated-form (sexp-at-point))
                    (expanded (macroexpand-1 updated-form)))
               (condition-case nil
                   (eval updated-form t)
                 (error nil))           ; ignore ERT "test redefined" noise
               (when (and (consp expanded)
                          (eq (car expanded) 'ert-deftest))
                 (message "Running test: %s" (cadr expanded))
                 (ert (cadr expanded))))
             (message "Snapshot updated."))))

       (defun ,update-fn ()
         ,(format "Rewrite output keys of the `%s' form at point." test-macro)
         (save-excursion
           (backward-sexp)
           (,update-at (point))))

       (defun ,update-by (description)
         ,(format "Find the `%s' with DESCRIPTION in any buffer, update its snapshot." test-macro)
         (let ((needle (format ,(format "(%s %%S" test-macro) description))
               (found nil))
           (cl-loop for buf in (buffer-list)
                    when (buffer-file-name buf) do
                    (with-current-buffer buf
                      (save-excursion
                        (goto-char (point-min))
                        (when (search-forward needle nil t)
                          (backward-char (length needle))
                          (,update-at (point))
                          (setq found t)
                          (cl-return)))))
           (unless found
             (user-error "Could not find %s %S in any open buffer"
                         ',test-macro description))))

       ;; -- 4. Test macro --
       (cl-defmacro ,test-macro (description &optional tags &rest plist-body)
         ,docstring
         (declare (indent defun))
         (cl-assert (stringp description) nil ,assert-msg)
         (let*
             ((provided-tags (when (vectorp tags) (append tags nil)))
              (test-name (snap--test-name description provided-tags))
              (method-being-tested (snap--method-tag description))
              (actual-plist (if (vectorp tags) plist-body (cons tags plist-body)))
              ;; Bind function symbols so the inner backquote can
              ;; reference them with a single comma (,--check-fn)
              ;; instead of double-comma (,,check-fn) which would
              ;; try to evaluate the symbol as a variable.
              (--check-fn ',check-fn)
              (--compute-fn ',compute-fn)
              (--update-by ',update-by)
              ;; Build the argument list for the checker.
              ;; We quote each value so that symbols (:to latex) and
              ;; lists (:using ((org-defblock ...))) survive splicing
              ;; into the ert-deftest expansion.  Quoting strings and
              ;; nil is a no-op, so this is backward-compatible.
              (call-args
               (cl-loop for kw in ',param-keywords
                        collect `',(plist-get actual-plist kw)))
              ;; Collect output keys: call compute at macro-expand time
              ;; to discover which keys are outputs.  We pass the plist
              ;; values as-is (they're literals in snapshot tests).
              (output-keys
               (condition-case nil
                   (cl-loop for (k _) on (,compute-fn actual-plist) by #'cddr
                            collect k)
                 (error nil))))
           `(ert-deftest ,(intern test-name) ()
              :tags ',(cl-list* ',name method-being-tested provided-tags)
              ;; Happy path: check-fn runs should internally.
              ;; On failure: catch it, compute actual outputs, re-assert
              ;; with ert-info buttons for [Show Diff] / [Update Snapshot].
              (condition-case nil
                  (funcall ',--check-fn ,@call-args)
                (ert-test-failed
                 (let ((--outputs (funcall ',--compute-fn
                                           (list ,@(cl-loop
                                                    for kw in ',param-keywords
                                                    append (list kw `',(plist-get actual-plist kw)))))))
                   ,@(cl-loop
                      for kw in output-keys
                      for expected = (plist-get actual-plist kw)
                      collect
                      `(ert-info
                           ;; This backquote runs with the test, so macro-time values are spliced
                           ;; via ,', and runtime ones ---the actual output--- via a lone comma.
                           (`(lambda ()
                               (when (stringp ',',expected)
                                 (insert-button "[Show Diff]"
                                                'action (lambda (_)
                                                          (snap-define-relation--show-diff
                                                           ',',expected
                                                           ',(plist-get --outputs ,kw)))
                                                'follow-link t
                                                'help-echo "Show diff of expected vs actual")
                                 (insert "  "))
                               (insert-button "[Update Snapshot]"
                                              'action (lambda (_)
                                                        (funcall ',',--update-by ,',description))
                                              'follow-link t
                                              'help-echo "Rewrite output keys in source")
                               ""))
                         (should (equal ,expected (plist-get --outputs ,kw)))))))))))

       ;; -- 5. Register C-x C-e extension via fancy-C-x-C-e --
       ;; Plain C-x C-e → eval the form, expand it, run the ERT test.
       ;; C-u   C-x C-e → update the snapshot (rewrite output keys).
       (fancy-C-x-C-e-register ',test-macro
         (lambda (form)
           (if current-prefix-arg
               (funcall #',update-fn)
             (let ((expanded (macroexpand-1 form)))
               (when (and (consp expanded)
                          (eq (car expanded) 'ert-deftest))
                 (message "Running test: %s" (cadr expanded))
                 (ert (cadr expanded))))))))))

(provide 'snap)
;;; snap.el ends here
