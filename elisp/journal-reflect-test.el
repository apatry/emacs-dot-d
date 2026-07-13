;;; journal-reflect-test.el --- ERT tests for journal-reflect -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run interactively: load both files, then `M-x ert RET t RET'.
;;
;; Run in batch:
;;   emacs -Q --batch -L . -l journal-reflect -l journal-reflect-test \
;;     -f ert-run-tests-batch-and-exit
;;
;; The async tests stub the backend to `cat' instead of `claude', so they
;; exercise the real make-process/stdin/sentinel pipeline deterministically
;; and without needing network access or API credits.

;;; Code:

(require 'ert)
(require 'journal-reflect)

(defmacro journal-reflect-test--with-org-buffer (contents &rest body)
  "Run BODY in a temporary `org-mode' buffer seeded with CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     ,@body))

(defmacro journal-reflect-test--with-cat-backend (&rest body)
  "Run BODY with the journal-reflect backend stubbed to `cat'.
`cat' echoes stdin back to stdout, so it stands in for a backend that
replies with the prompt it was sent."
  (declare (indent 0))
  `(let ((journal-reflect-claude-executable "cat")
         (journal-reflect-claude-args nil))
     ,@body))

(defun journal-reflect-test--wait-for (buf &optional timeout)
  "Block until `journal-reflect--in-progress' is nil in BUF or TIMEOUT elapses."
  (with-current-buffer buf
    (let ((deadline (+ (float-time) (or timeout 10))))
      (while (and journal-reflect--in-progress (< (float-time) deadline))
        (accept-process-output nil 0.1))
      (when journal-reflect--in-progress
        (ert-fail "journal-reflect: timed out waiting for process")))))

;;; Pure buffer-manipulation logic

(ert-deftest journal-reflect-test-buffer-without-reflection/no-heading ()
  (journal-reflect-test--with-org-buffer "Just some notes.\n"
    (should (equal (journal-reflect--buffer-without-reflection) "Just some notes."))))

(ert-deftest journal-reflect-test-buffer-without-reflection/strips-existing-heading ()
  (journal-reflect-test--with-org-buffer
      "Body text.\n* Reflection\nOld reflection.\n"
    (should (equal (journal-reflect--buffer-without-reflection) "Body text."))))

(ert-deftest journal-reflect-test-buffer-without-reflection/ignores-unrelated-headings ()
  (journal-reflect-test--with-org-buffer
      "* Not a reflection\nSome body.\n"
    (should (equal (journal-reflect--buffer-without-reflection)
                    "* Not a reflection\nSome body."))))

(ert-deftest journal-reflect-test-insert-reflection/fresh ()
  (journal-reflect-test--with-org-buffer "Body text.\n"
    (cl-letf (((symbol-function 'save-buffer) #'ignore))
      (journal-reflect--insert-reflection "New reflection."))
    (should (equal (buffer-string) "Body text.\n* Reflection\nNew reflection.\n"))))

(ert-deftest journal-reflect-test-insert-reflection/replaces-not-duplicates ()
  (journal-reflect-test--with-org-buffer
      "Body text.\n* Reflection\nOld reflection.\n"
    (cl-letf (((symbol-function 'save-buffer) #'ignore))
      (journal-reflect--insert-reflection "New reflection."))
    (should (equal (buffer-string) "Body text.\n* Reflection\nNew reflection.\n"))
    (goto-char (point-min))
    (should (= 1 (how-many "^\\* Reflection\\b")))))

(ert-deftest journal-reflect-test-insert-reflection/preserves-content-after ()
  ;; org-end-of-subtree should only remove the Reflection subtree itself,
  ;; leaving whatever follows it (e.g. a later heading) in place -- guards
  ;; against widening/scoping regressions.
  (journal-reflect-test--with-org-buffer
      "Body text.\n* Reflection\nOld reflection.\n* Other\nKeep me.\n"
    (cl-letf (((symbol-function 'save-buffer) #'ignore))
      (journal-reflect--insert-reflection "New reflection."))
    (should (equal (buffer-string)
                    "Body text.\n* Reflection\nNew reflection.\n* Other\nKeep me.\n"))))

;;; Command building

(ert-deftest journal-reflect-test-build-command/claude-cli ()
  (let ((journal-reflect-backend 'claude-cli)
        (journal-reflect-claude-executable "claude")
        (journal-reflect-claude-args '("-p" "--tools" "")))
    (should (equal (journal-reflect--build-command) '("claude" "-p" "--tools" "")))))

(ert-deftest journal-reflect-test-build-command/hermes-not-implemented ()
  (let ((journal-reflect-backend 'hermes))
    (should-error (journal-reflect--build-command) :type 'user-error)))

(ert-deftest journal-reflect-test-build-command/unknown-backend ()
  (let ((journal-reflect-backend 'bogus))
    (should-error (journal-reflect--build-command) :type 'user-error)))

;;; journal-reflect--run: skip conditions

(ert-deftest journal-reflect-test-run/skips-empty-entry ()
  (journal-reflect-test--with-org-buffer "   \n\t\n"
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (ert-fail "make-process should not be called for an empty entry"))))
      (journal-reflect-now))
    (should-not journal-reflect--in-progress)))

(ert-deftest journal-reflect-test-run/skips-when-already-in-progress ()
  (journal-reflect-test--with-org-buffer "Body text.\n"
    (setq journal-reflect--in-progress t)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (ert-fail "make-process should not be called while already in progress"))))
      (journal-reflect-now))))

;;; journal-reflect--run: full async pipeline against a `cat' stand-in backend

(ert-deftest journal-reflect-test-run/round-trips-through-real-subprocess ()
  (journal-reflect-test--with-cat-backend
    (journal-reflect-test--with-org-buffer "Body text.\n"
      (let ((journal-reflect-prompt-template "PROMPT:%s")
            (buf (current-buffer)))
        (cl-letf (((symbol-function 'save-buffer) #'ignore))
          (journal-reflect-now)
          (journal-reflect-test--wait-for buf))
        (should (equal (buffer-string) "Body text.\n* Reflection\nPROMPT:Body text.\n"))))))

(ert-deftest journal-reflect-test-run/in-progress-guards-buffer-not-general ()
  ;; `journal-reflect--in-progress' is buffer-local: a run in one buffer
  ;; must not block a run in another. The `save-buffer' stub must stay in
  ;; effect for both async sentinels, not just the synchronous kickoff, so
  ;; it wraps both `journal-reflect-now' calls *and* both waits.
  (journal-reflect-test--with-cat-backend
    (journal-reflect-test--with-org-buffer "Buffer one.\n"
      (let ((buf1 (current-buffer))
            (journal-reflect-prompt-template "%s"))
        (journal-reflect-test--with-org-buffer "Buffer two.\n"
          (let ((buf2 (current-buffer)))
            (cl-letf (((symbol-function 'save-buffer) #'ignore))
              (with-current-buffer buf1 (journal-reflect-now))
              (with-current-buffer buf2 (journal-reflect-now))
              (journal-reflect-test--wait-for buf1)
              (journal-reflect-test--wait-for buf2))
            (should (equal (with-current-buffer buf1 (buffer-string))
                            "Buffer one.\n* Reflection\nBuffer one.\n"))
            (should (equal (with-current-buffer buf2 (buffer-string))
                            "Buffer two.\n* Reflection\nBuffer two.\n"))))))))

(provide 'journal-reflect-test)
;;; journal-reflect-test.el ends here
