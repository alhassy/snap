;;; snap-tests.el --- Tests for snap.el -*- lexical-binding: t -*-

;; Copyright (c) 2025 Musa Al-hassy
;; Author: Musa Al-hassy <alhassy@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Exhaustive ERT suite for `snap.el' -- dogfooded through `deftest'
;; itself.  Each section corresponds to one public primitive; the
;; whole suite runs cleanly in batch:
;;
;;   emacs --batch -l ~/snap/snap.el \
;;                 -l ~/snap/snap-tests.el \
;;                 -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load snap.el from the same directory as this file -- robust whether
;; loaded via `load-file', `load', or batch `-l'.
(load-file
 (expand-file-name
  "snap.el"
  (file-name-directory (or load-file-name buffer-file-name
                           (expand-file-name "snap-tests.el" default-directory)))))

;; ═══════════════════════════════════════════════════════════════════
;; 1. `deftest-space' -- the naming knob
;; ═══════════════════════════════════════════════════════════════════

(deftest "deftest-space defaults to interpunct" [snap deftest-space]
  (should (equal deftest-space "·")))

(deftest "deftest-space rebinding changes generated test name" [snap deftest-space]
  (let ((deftest-space "-"))
    (let ((expanded (macroexpand-1 '(deftest "two words" (should t)))))
      ;; (ert-deftest two-words nil :tags '("two") (should t))
      (should (eq (nth 1 expanded) 'two-words)))))

;; ═══════════════════════════════════════════════════════════════════
;; 2. `deftest' -- the identity fixture
;; ═══════════════════════════════════════════════════════════════════

(deftest "deftest expands to an ert-deftest form" [snap deftest]
  (let ((expanded (macroexpand-1 '(deftest "hello" (should t)))))
    (should (consp expanded))
    (should (eq (car expanded) 'ert-deftest))
    (should (symbolp (nth 1 expanded)))      ; test name
    (should (null   (nth 2 expanded)))       ; arglist: ()
    (should (eq     (nth 3 expanded) :tags))
    (should (equal  (nth 5 expanded) '(should t)))))

(deftest "deftest replaces spaces with deftest-space" [snap deftest name-mangling]
  (let ((expanded (macroexpand-1 '(deftest "a b c" (should t)))))
    (should (eq (nth 1 expanded) 'a·b·c))))

(deftest "deftest maps special characters to unicode lookalikes" [snap deftest name-mangling]
  ;; , → ︐   ; → ︔   [ → ⁅   ] → ⁆
  (let ((expanded (macroexpand-1 '(deftest "special ,; [] chars" (should t)))))
    (should (eq (nth 1 expanded) 'special·︐︔·⁅⁆·chars))))

(deftest "deftest strips ASCII apostrophes from names" [snap deftest name-mangling]
  (let ((expanded (macroexpand-1 '(deftest "don't panic" (should t)))))
    ;; Apostrophe is removed; the rest is spaced normally.
    (should (eq (nth 1 expanded) 'dont·panic))))

(deftest "deftest collapses runs of whitespace" [snap deftest name-mangling]
  (let ((expanded (macroexpand-1 '(deftest "  hi   there  " (should t)))))
    (should (eq (nth 1 expanded) 'hi·there))))

(deftest "deftest auto-tags the first word of the description" [snap deftest tags]
  (let* ((expanded (macroexpand-1 '(deftest "addition is commutative" (should t))))
         (tags (nth 4 expanded)))
    ;; :tags is quoted at macro-expand time; tags = '(quote ("addition")).
    (should (equal (cadr tags) '("addition")))))

(deftest "deftest accepts an explicit tags vector" [snap deftest tags]
  (let* ((expanded (macroexpand-1 '(deftest "x" [alpha beta] (should t))))
         (tags (cadr (nth 4 expanded))))
    ;; first-word auto-tag is prepended, explicit tags follow as symbols.
    (should (equal tags '("x" alpha beta)))))

(deftest "deftest prefixes the name with tag·⇨ when tags given" [snap deftest tags]
  (let ((expanded (macroexpand-1 '(deftest "blah" [alpha beta] (should t)))))
    (should (eq (nth 1 expanded) 'alpha︐beta··⇨··blah))))

(deftest "deftest signals cl-assert on non-string description" [snap deftest validation]
  (should-error (macroexpand-1 '(deftest 42 (should t)))))

(deftest "deftest is registered in snap--registry" [snap deftest registry]
  (should (eq (gethash 'deftest snap--registry) t)))

(deftest "deftest bodies are run -- should fires as expected" [snap deftest smoke]
  ;; If this test body were ignored, the failing `should' below would pass.
  (should (= (+ 1 1) 2))
  (should-not (= 1 2)))

;; ═══════════════════════════════════════════════════════════════════
;; 3. `deftestfixture' -- macro-generating macro
;; ═══════════════════════════════════════════════════════════════════

;; A fixture that simply binds a dynamic var around BODY.
(defvar snap-tests--fixture-flag nil)
(deftestfixture snap-tests--with-flag
  "Bind `snap-tests--fixture-flag' to t for the duration of BODY."
  (let ((snap-tests--fixture-flag t))
    &body))

(deftest "deftestfixture registers the generated macro" [snap deftestfixture registry]
  (should (eq (gethash 'snap-tests--with-flag snap--registry) t)))

(deftest "deftestfixture splices body at &body hole" [snap deftestfixture]
  (let* ((expanded (macroexpand-1
                    '(snap-tests--with-flag "flag is set" (should snap-tests--fixture-flag))))
         (body (nthcdr 5 expanded)))
    ;; Expanded shape: (ert-deftest NAME () :tags ... <wrapped-body>)
    ;; The let from the fixture should be present.
    (should (eq (caar body) 'let))
    (should (equal (cadar body) '((snap-tests--fixture-flag t))))
    ;; Inside the let, the &body hole got replaced with a (progn ...).
    (let ((inner (caddar body)))
      (should (eq (car inner) 'progn))
      (should (equal (cadr inner) '(should snap-tests--fixture-flag))))))

(snap-tests--with-flag "flag is set at runtime" [snap deftestfixture smoke]
  ;; Sanity: the fixture actually binds the flag when the test runs.
  (should snap-tests--fixture-flag))

(deftest "the flag is nil outside the fixture" [snap deftestfixture smoke]
  (should-not snap-tests--fixture-flag))

;; Fixture with no docstring -- docstring arg omitted, fixture is positional arg 2.
(deftestfixture snap-tests--no-doc-fixture
  (let ((x 1)) &body))

(deftest "deftestfixture accepts a fixture without a docstring" [snap deftestfixture]
  (should (eq (gethash 'snap-tests--no-doc-fixture snap--registry) t))
  (let ((expanded (macroexpand-1
                   '(snap-tests--no-doc-fixture "no-doc case" (should (= x 1))))))
    (should (eq (car expanded) 'ert-deftest))))

;; Fixture referencing `&body' twice -- `cl-subst' must hit every occurrence.
(deftestfixture snap-tests--double-body
  "Wrap body in a `progn' that evaluates it twice (to exercise cl-subst)."
  (progn &body &body))

(deftest "deftestfixture splices &body at every occurrence" [snap deftestfixture]
  (let* ((expanded (macroexpand-1
                    '(snap-tests--double-body "twice" (should t))))
         (body (nth 5 expanded)))
    ;; body = (progn (progn (should t)) (progn (should t)))
    (should (eq (car body) 'progn))
    (should (= (length body) 3))
    (should (equal (nth 1 body) '(progn (should t))))
    (should (equal (nth 2 body) '(progn (should t))))))

(deftest "deftestfixture with no fixture at all acts like deftest" [snap deftestfixture]
  (deftestfixture snap-tests--identity)
  (should (eq (gethash 'snap-tests--identity snap--registry) t))
  (let ((expanded (macroexpand-1
                   '(snap-tests--identity "plain" (should t)))))
    ;; No wrapping: body appears verbatim.
    (should (equal (nthcdr 5 expanded) '((should t))))))

(deftest "deftestfixture redefinition replaces cleanly" [snap deftestfixture]
  (deftestfixture snap-tests--redef "v1" (let ((v 1)) &body))
  (deftestfixture snap-tests--redef "v2" (let ((v 2)) &body))
  (let ((expanded (macroexpand-1
                   '(snap-tests--redef "check" (should (= v 2))))))
    (should (equal (cadr (nth 5 expanded)) '((v 2))))))

;; ═══════════════════════════════════════════════════════════════════
;; 4. `define-relation' -- relational fixtures with snapshot updating
;; ═══════════════════════════════════════════════════════════════════

;; A minimal relation used throughout section 4.
(define-relation snap-tests--double (input expected)
  "EXPECTED = 2 × INPUT."
  (let ((actual (* 2 input)))
    (should (equal expected actual))
    (list :expected actual)))

(deftest "define-relation generates the expected function quintet" [snap define-relation]
  (should (fboundp 'snap-tests--double--check))
  (should (fboundp 'snap-tests--double--compute))
  (should (fboundp 'snap-tests--double--update-at))
  (should (fboundp 'snap-tests--double--update))
  (should (fboundp 'snap-tests--double--update-by-name))
  (should (fboundp 'define-snap-tests--double-test)))

(deftest "define-relation registers the test macro with :updater and :compute" [snap define-relation registry]
  (let ((entry (gethash 'define-snap-tests--double-test snap--registry)))
    (should (consp entry))
    (should (eq (plist-get entry :updater)
                'snap-tests--double--update))
    (should (eq (plist-get entry :compute)
                'snap-tests--double--compute))))

(deftest "compute-fn returns the actual output plist even when the assertion fails"
    [snap define-relation compute]
  ;; :expected 7 is wrong; without should-suppression the assertion would abort.
  ;; With `ert-fail' let-bound to `ignore' inside compute, the body runs through.
  (should (equal (snap-tests--double--compute '(:input 3 :expected 7))
                 '(:expected 6))))

(deftest "check-fn raises ert-test-failed on mismatch" [snap define-relation check]
  (should-error (snap-tests--double--check 3 7) :type 'ert-test-failed))

(deftest "check-fn passes silently on a correct snapshot" [snap define-relation check]
  (should (equal (snap-tests--double--check 3 6) '(:expected 6))))

(deftest "generated test macro expands to an ert-deftest form" [snap define-relation]
  (let ((expanded (macroexpand-1
                   '(define-snap-tests--double-test "three" :input 3 :expected 6))))
    (should (eq (car expanded) 'ert-deftest))
    (should (eq (nth 1 expanded) 'three))))

;; The happy and failing paths at runtime -- wrap in ert-run-tests-batch
;; so we observe ERT's own bookkeeping rather than raw condition signals.

(define-snap-tests--double-test "happy path 2×3=6" [snap define-relation runtime]
  :input 3 :expected 6)

(deftest "define-relation happy-path test is registered and passes when run"
    [snap define-relation runtime]
  ;; `snap' interns the test via a custom-mangled symbol; we look it up by name.
  (let ((test (ert-get-test 'snap︐define-relation︐runtime··⇨··happy·path·2×3=6)))
    (should (ert-test-p test))
    (let ((result (ert-run-test test)))
      (should (ert-test-passed-p result)))))

;; Quoting regression: the nested-backquote pitfall fix means list-
;; literal inputs survive splicing.  This relation takes a list and
;; flattens it.  If quoting were wrong, we'd see `void-variable' or
;; mis-evaluation at expand time.
(define-relation snap-tests--listy (items joined)
  "JOINED is ITEMS concatenated with hyphens."
  (let ((actual (mapconcat #'symbol-name items "-")))
    (should (equal joined actual))
    (list :joined actual)))

(define-snap-tests--listy-test "list survives splicing" [snap define-relation regression]
    :items (a b c)
    :joined "a-b-c")

(deftest "list-valued inputs round-trip through the generated macro"
    [snap define-relation regression]
  (let ((test (ert-get-test
               'snap︐define-relation︐regression··⇨··list·survives·splicing)))
    (should (ert-test-p test))
    (should (ert-test-passed-p (ert-run-test test)))))

;; ── Updater: in-buffer snapshot rewrite ──────────────────────────

;; Each updater test uses a distinct description so that `update-at''s
;; re-eval-and-run step doesn't redefine the same `ert-deftest' symbol
;; across tests (Emacs 30+ ERT signals `Test redefined' under
;; `ert-debug-on-error', which is on during batch suite runs).

(deftest "update-at rewrites wrong output keys in the source buffer"
    [snap define-relation updater]
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(define-snap-tests--double-test \"updater-rewrite-case\" :input 3 :expected 7)")
    (goto-char (point-min))
    (snap-tests--double--update-at (point))
    ;; After the rewrite :expected 7 should have become :expected 6.
    (should (string-match-p ":expected 6" (buffer-string)))
    (should-not (string-match-p ":expected 7" (buffer-string)))))

(deftest "update-at preserves surrounding content verbatim" [snap define-relation updater]
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; a comment before\n"
            "(define-snap-tests--double-test \"updater-preserves-case\" :input 4 :expected 99)\n"
            ";; a comment after\n")
    (goto-char (point-min))
    (search-forward "(define-snap")
    (goto-char (match-beginning 0))
    (snap-tests--double--update-at (point))
    (let ((s (buffer-string)))
      (should (string-match-p ";; a comment before" s))
      (should (string-match-p ";; a comment after"  s))
      (should (string-match-p ":expected 8"         s)))))

(deftest "update-by-name locates the form across open file buffers"
    [snap define-relation updater]
  (let ((tmp (make-temp-file "snap-test-" nil ".el")))
    (unwind-protect
        (let ((buf (find-file-noselect tmp)))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (erase-buffer)
                  (insert "(define-snap-tests--double-test \"updater-by-name-case\" :input 5 :expected 0)")
                  (save-buffer))
                (snap-tests--double--update-by-name "updater-by-name-case")
                (with-current-buffer buf
                  (should (string-match-p ":expected 10" (buffer-string)))))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
      (delete-file tmp))))

(deftest "update-by-name raises user-error when the form is not found"
    [snap define-relation updater]
  (should-error (snap-tests--double--update-by-name
                 "updater-no-such-form-anywhere-in-any-buffer")
                :type 'user-error))

;; Multi-output relation: two keys, both should be updated.
(define-relation snap-tests--pair (n squared cubed)
  "SQUARED = n² and CUBED = n³."
  (let ((a (* n n)) (b (* n n n)))
    (should (equal squared a))
    (should (equal cubed b))
    (list :squared a :cubed b)))

(deftest "update-at rewrites every output key in a multi-key relation"
    [snap define-relation updater multi-key]
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(define-snap-tests--pair-test \"multikey-n3-case\" :n 3 :squared 0 :cubed 0)")
    (goto-char (point-min))
    (snap-tests--pair--update-at (point))
    (let ((s (buffer-string)))
      (should (string-match-p ":squared 9"  s))
      (should (string-match-p ":cubed 27"   s)))))

;; ═══════════════════════════════════════════════════════════════════
;; 5. The `eval-last-sexp' advice
;; ═══════════════════════════════════════════════════════════════════

(deftest "advice snap--eval-last-sexp is installed on eval-last-sexp"
    [snap advice]
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn 'snap--eval-last-sexp)
                     (setq found t)))
                 'eval-last-sexp)
    (should found)))

(defvar snap-tests--passthrough-counter 0
  "Bumped by the passthrough test's evaluated form.")

(deftest "advice passes through plain expressions -- no ERT run, no error"
    [snap advice passthrough]
  ;; If the advice wrongly kicked in, it would try to macroexpand the form
  ;; and either error or run a nonexistent test.  We use a side-effecting
  ;; form so we can observe that it was evaluated exactly once.
  (setq snap-tests--passthrough-counter 0)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(setq snap-tests--passthrough-counter (1+ snap-tests--passthrough-counter))")
    (goto-char (point-max))
    (eval-last-sexp nil))
  (should (= snap-tests--passthrough-counter 1)))

(defvar snap-tests--sentinel nil
  "Bumped by a test body to prove the eval-and-run advice fired.")

(deftest "C-x C-e on a registered deftest form runs the generated ERT test"
    [snap advice eval-and-run]
  (setq snap-tests--sentinel 0)
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; A deftest whose body bumps the sentinel.  After eval-last-sexp,
    ;; the advice auto-runs the test, which bumps the sentinel again.
    ;; The description is uniquely named to avoid `Test redefined' on
    ;; repeated suite runs.
    (insert "(deftest \"advice-sentinel-bump-case\" (setq snap-tests--sentinel (1+ snap-tests--sentinel)) (should t))")
    (goto-char (point-max))
    (eval-last-sexp nil))
  ;; One bump comes from the auto-run -- the top-level `deftest' call
  ;; only *defines* the ert-deftest; the advice runs it once after.
  (should (>= snap-tests--sentinel 1)))

(deftest "C-u on a define-relation form invokes the updater"
    [snap advice updater]
  (let ((tmp (make-temp-file "snap-advice-" nil ".el")))
    (unwind-protect
        (let ((buf (find-file-noselect tmp)))
          (unwind-protect
              (with-current-buffer buf
                (erase-buffer)
                (insert "(define-snap-tests--double-test \"advice-via-C-u-case\" :input 7 :expected 0)")
                (save-buffer)
                (goto-char (point-max))
                ;; Non-nil prefix-arg triggers the updater path.
                (eval-last-sexp 4)
                (should (string-match-p ":expected 14" (buffer-string))))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
      (delete-file tmp))))

;; ═══════════════════════════════════════════════════════════════════
;; 6. `define-relation--show-diff' -- lightly covered
;; ═══════════════════════════════════════════════════════════════════

(deftest "define-relation--show-diff is defined and callable" [snap show-diff]
  (should (fboundp 'define-relation--show-diff)))

(deftest "show-diff creates a *Snapshot Diff* buffer and cleans up temp files"
    [snap show-diff]
  ;; Requires both magit-diff (the mode) and magit-base (for `magit--any').
  ;; In batch, these are not auto-loaded; skip when not fully available.
  (skip-unless (and (featurep 'magit-diff) (fboundp 'magit--any)))
  (let ((before (seq-filter
                 (lambda (f) (string-match-p "^snapshot-\\(expected\\|actual\\)-" f))
                 (directory-files temporary-file-directory))))
    (define-relation--show-diff "alpha\n" "beta\n")
    (should (get-buffer "*Snapshot Diff*"))
    ;; Temp files must be deleted -- no leaks.
    (let ((after (seq-filter
                  (lambda (f) (string-match-p "^snapshot-\\(expected\\|actual\\)-" f))
                  (directory-files temporary-file-directory))))
      (should (equal (length before) (length after))))
    (kill-buffer "*Snapshot Diff*")))

;; ═══════════════════════════════════════════════════════════════════
;; 7. Integration -- end-to-end smoke
;; ═══════════════════════════════════════════════════════════════════

(define-relation snap-tests--upcase (input expected)
  "EXPECTED is the uppercase form of INPUT."
  (let ((actual (upcase input)))
    (should (equal expected actual))
    (list :expected actual)))

(define-snap-tests--upcase-test "integration hello to HELLO" [snap integration]
  :input "hello" :expected "HELLO")

(define-snap-tests--upcase-test "integration already upper stays upper" [snap integration]
  :input "LOUD" :expected "LOUD")

(deftest "integration: define-relation tests defined at top level are runnable"
    [snap integration]
  ;; Look each one up by its mangled symbol and run it standalone.
  (dolist (sym '(snap︐integration··⇨··integration·hello·to·HELLO
                 snap︐integration··⇨··integration·already·upper·stays·upper))
    (let ((test (ert-get-test sym)))
      (should (ert-test-p test))
      (should (ert-test-passed-p (ert-run-test test))))))

;;; snap-tests.el ends here
