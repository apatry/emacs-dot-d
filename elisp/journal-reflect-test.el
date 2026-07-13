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

(defmacro journal-reflect-test--with-org-journal-dir (entries &rest body)
  "Run BODY with a temp `org-journal-dir' populated from ENTRIES.
ENTRIES is a list of (YEAR MONTH DAY CONTENT) lists; each is written to
the path org-journal itself would use for that date (via
`org-journal--get-entry-path'), so this exercises journal-reflect's
real org-journal date-lookup API rather than a hand-rolled stand-in.
`org-journal' is required for real; `features' and the dates cache are
restored afterward so unrelated tests aren't affected by run order."
  (declare (indent 1))
  `(progn
     (require 'org-journal)
     (let* ((org-journal-dir (make-temp-file "journal-reflect-test-" t))
            (org-journal-file-type 'daily)
            (org-journal-file-format "%Y%m%d")
            (org-journal--dates (make-hash-table :test 'equal))
            (org-journal--sorted-dates nil)
            (already-featured (featurep 'org-journal)))
       (unwind-protect
           (progn
             (dolist (e ,entries)
               (let ((year (nth 0 e)) (month (nth 1 e)) (day (nth 2 e)) (content (nth 3 e)))
                 (with-temp-file (org-journal--get-entry-path
                                  (org-journal--calendar-date->time (list month day year)))
                   (insert content))))
             ,@body)
         (unless already-featured
           (setq features (delq 'org-journal features)))
         (delete-directory org-journal-dir t)))))

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

;;; Prior-day context: labeled and distinguished from today's entry

(ert-deftest journal-reflect-test-previous-context/empty-without-org-journal ()
  (let ((journal-reflect-context-days 3))
    (should (equal (journal-reflect--previous-context) ""))))

(ert-deftest journal-reflect-test-previous-context/labels-days-and-strips-their-reflections ()
  (journal-reflect-test--with-org-journal-dir
      '((2026 7 10 "Yesterday's notes.\n* Reflection\nOld AI reflection.\n"))
    (let* ((journal-reflect-context-days 1)
           (context (journal-reflect--previous-context)))
      (should (string-match-p "for spotting patterns only" context))
      (should (string-match-p "2026-07-10" context))
      (should (string-match-p "Yesterday's notes\\." context))
      (should-not (string-match-p "Old AI reflection" context)))))

(ert-deftest journal-reflect-test-previous-context/excludes-current-and-future-dates ()
  ;; Regression guard for the switch to org-journal's date index: exclusion
  ;; of "today" must compare calendar dates, not just the current file path,
  ;; so it still works regardless of `org-journal-file-format'.
  (journal-reflect-test--with-org-journal-dir
      '((2026 7 9 "Two days ago.\n")
        (2026 7 10 "Yesterday.\n")
        (2026 7 11 "Today.\n"))
    (let* ((journal-reflect-context-days 5)
           (today-file (org-journal--get-entry-path
                        (org-journal--calendar-date->time '(7 11 2026)))))
      (with-temp-buffer
        (setq buffer-file-name today-file)
        (let ((context (journal-reflect--previous-context)))
          (should (string-match-p "Two days ago\\." context))
          (should (string-match-p "Yesterday\\." context))
          (should-not (string-match-p "Today\\." context)))))))

(ert-deftest journal-reflect-test-entry-text/unlabeled-when-no-context ()
  ;; Default `journal-reflect-context-days' is 0: entry-text stays exactly
  ;; the bare body, so the prompt template's own wording is unaffected.
  (journal-reflect-test--with-org-buffer "Body text.\n"
    (should (equal (journal-reflect--entry-text) "Body text."))))

(ert-deftest journal-reflect-test-entry-text/marks-today-apart-from-context ()
  (journal-reflect-test--with-org-journal-dir
      '((2026 7 10 "Yesterday's notes.\n"))
    (let ((journal-reflect-context-days 1))
      (journal-reflect-test--with-org-buffer "Today's notes.\n"
        (let ((text (journal-reflect--entry-text)))
          (should (string-match-p "for spotting patterns only" text))
          (should (string-match-p "Yesterday's notes\\." text))
          (should (string-match-p "Today's entry" text))
          (should (string-match-p "Today's notes\\." text))
          ;; The context block must come before the today's-entry marker.
          (should (< (string-match "for spotting patterns only" text)
                     (string-match "Today's entry" text))))))))

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

(ert-deftest journal-reflect-test-backend-name/known-backends ()
  (let ((journal-reflect-backend 'claude-cli))
    (should (equal (journal-reflect--backend-name) "Claude")))
  (let ((journal-reflect-backend 'hermes))
    (should (equal (journal-reflect--backend-name) "Hermes"))))

(ert-deftest journal-reflect-test-backend-name/falls-back-to-symbol-name ()
  (let ((journal-reflect-backend 'bogus))
    (should (equal (journal-reflect--backend-name) "bogus"))))

;;; journal-reflect--after-save

(ert-deftest journal-reflect-test-after-save/runs-immediately ()
  (journal-reflect-test--with-org-buffer "Body text.\n"
    (let ((run-count 0))
      (cl-letf (((symbol-function 'journal-reflect--run)
                 (lambda () (setq run-count (1+ run-count)))))
        (journal-reflect--after-save)
        (should (= run-count 1))))))

(ert-deftest journal-reflect-test-after-save/suppressed-hook-does-nothing ()
  (journal-reflect-test--with-org-buffer "Body text.\n"
    (let ((journal-reflect--suppress-hook t)
          (run-count 0))
      (cl-letf (((symbol-function 'journal-reflect--run)
                 (lambda () (setq run-count (1+ run-count)))))
        (journal-reflect--after-save)
        (should (= run-count 0))))))

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
