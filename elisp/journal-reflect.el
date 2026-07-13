;;; journal-reflect.el --- AI reflection for org-journal entries -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; journal-reflect hooks into org-journal (or any org buffer) and, on save,
;; asynchronously asks Claude to write a short reflection under a
;; `* Reflection' heading. The call runs in a subprocess so it never blocks
;; Emacs, and it rewrites the Reflection subtree in place rather than piling
;; up duplicates on every save.
;;
;; Setup:
;;
;;   (add-to-list 'load-path "/path/to/this/file")
;;   (require 'journal-reflect)
;;   (add-hook 'org-journal-mode-hook #'journal-reflect-mode)
;;
;; Manual trigger (works even without the minor mode enabled):
;;
;;   M-x journal-reflect-now
;;
;; Backend:
;;
;;   Ships wired to the `claude' CLI (Claude Code) in non-interactive
;;   "print" mode (`claude -p "prompt"'). Check `claude --help` on your
;;   machine and adjust `journal-reflect-claude-args' if your version's
;;   flags differ.
;;
;;   `journal-reflect-backend' is the seam for swapping in something else
;;   later (e.g. Hermes) without touching the rest of the logic -- see
;;   `journal-reflect--build-command'. Add a new `pcase' branch there and a
;;   corresponding `hermes' choice becomes real instead of a stub.
;;
;; Notes / known limitations (v1):
;;
;;  - Operates on the *whole buffer* when stripping/inserting the
;;    Reflection subtree. This is correct as long as `org-journal-file-type'
;;    is `daily' (one file per day). If you ever switch to a datetree
;;    (multiple days per file), this will need to be scoped to the entry at
;;    point instead of the whole buffer -- flag if you want that added.
;;  - `journal-reflect-context-days' > 0 pulls in prior org-journal files
;;    as extra context for pattern-spotting, only when org-journal is
;;    loaded and `org-journal-dir' is set. Each prior day is labeled with
;;    its date and has its own Reflection subtree stripped (so past AI
;;    output doesn't get fed back in as if it were the user's writing),
;;    and today's entry is clearly marked as the one to actually reflect
;;    on -- so the model isn't left guessing which part of the text is
;;    background vs. what it's responding to.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'cl-lib)

(defgroup journal-reflect nil
  "AI-assisted reflection for org journal entries."
  :group 'org)

(defcustom journal-reflect-backend 'claude-cli
  "Which backend to use to generate reflections.
Currently supported: `claude-cli'. Reserved for future: `hermes'."
  :type '(choice (const :tag "Claude Code CLI" claude-cli)
                 (const :tag "Hermes (not yet implemented)" hermes))
  :group 'journal-reflect)

(defcustom journal-reflect-claude-executable "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'journal-reflect)

(defcustom journal-reflect-claude-args
  '("-p" "--tools" "" "--no-session-persistence")
  "Arguments passed to the Claude CLI. The prompt itself is sent over
stdin (see `journal-reflect--run'), not as an argv string, so it isn't
subject to argv length limits.

`-p'/`--print' runs Claude Code non-interactively and prints the result
to stdout. `--tools \"\"' disables all tool use, since we only want a
text reflection back, not an agent poking at the filesystem.
`--no-session-persistence' keeps these one-off calls out of the
session/resume list. Verify against `claude --help` on your machine."
  :type '(repeat string)
  :group 'journal-reflect)

(defcustom journal-reflect-heading "Reflection"
  "Heading text used for the AI-generated reflection subtree."
  :type 'string
  :group 'journal-reflect)

(defcustom journal-reflect-context-days 0
  "How many previous journal entries to include as context.
0 means only the current entry is sent. Only meaningful when org-journal
is loaded, since it's used to locate previous entry files."
  :type 'integer
  :group 'journal-reflect)

(defcustom journal-reflect-prompt-template
  "You are a thoughtful, low-key journaling companion. Below is a journal \
entry written in org-mode, possibly preceded by labeled context from \
earlier entries. If a \"Today's entry\" section is marked, reflect only \
on that section -- the rest is background for spotting patterns, not \
something to comment on directly. Respond with 2-4 short paragraphs of \
genuine reflection: notice patterns, gently ask one or two questions \
worth sitting with, and avoid generic affirmations or therapy-speak. Do \
not repeat the entry back to me. Plain prose only, no headings or \
bullet points.\n\nJournal entry:\n\n%s"
  "Template used to build the prompt sent to Claude.
`%s' is replaced with the entry text (plus labeled context, if any --
see `journal-reflect--entry-text')."
  :type 'string
  :group 'journal-reflect)

(defvar-local journal-reflect--in-progress nil
  "Non-nil while a reflection process is running for this buffer.")

(defvar-local journal-reflect--suppress-hook nil
  "Bound non-nil while we programmatically save the buffer ourselves,
so `after-save-hook' doesn't re-trigger a reflection on our own edit.")

;;;###autoload
(define-minor-mode journal-reflect-mode
  "Minor mode: auto-generate an AI reflection after saving this journal entry."
  :lighter " Reflect"
  (if journal-reflect-mode
      (add-hook 'after-save-hook #'journal-reflect--after-save nil t)
    (remove-hook 'after-save-hook #'journal-reflect--after-save t)))

(defun journal-reflect--after-save ()
  "Trigger a reflection after save, unless we caused this save ourselves.
`save-buffer' is a no-op on an unmodified buffer, so a reflexive re-save
right after saving never re-fires `after-save-hook' in the first place --
no debouncing needed here."
  (unless journal-reflect--suppress-hook
    (journal-reflect--run)))

;;;###autoload
(defun journal-reflect-now ()
  "Manually (re)generate a reflection for the current entry, regardless of mode."
  (interactive)
  (journal-reflect--run))

(defun journal-reflect--entry-text ()
  "Text to reflect on: buffer minus any existing Reflection subtree,
plus optional labeled prior-day context. When context is included,
today's entry is wrapped in its own clearly marked section so the
model can tell it apart from the background context."
  (let ((body (journal-reflect--buffer-without-reflection)))
    (if (> journal-reflect-context-days 0)
        (let ((context (journal-reflect--previous-context)))
          (if (string-empty-p context)
              body
            (concat context "\n\n"
                    (format "=== Today's entry (%s) -- reflect on this one ===\n\n"
                            (journal-reflect--today-label))
                    body)))
      body)))

(defun journal-reflect--today-label ()
  "Short label for today's entry, from the visited file name if any."
  (if (buffer-file-name)
      (file-name-base (buffer-file-name))
    "today"))

(defun journal-reflect--strip-reflection-subtree ()
  "Delete the Reflection subtree (if any) from the current buffer, in place."
  (goto-char (point-min))
  (let ((heading-re (format "^\\*+ %s\\b.*$" (regexp-quote journal-reflect-heading))))
    (when (re-search-forward heading-re nil t)
      (goto-char (match-beginning 0))
      (let ((start (point)))
        (org-end-of-subtree t t)
        (delete-region start (point))))))

(defun journal-reflect--buffer-without-reflection ()
  "Buffer contents with the existing Reflection subtree stripped out."
  (save-excursion
    (save-restriction
      (widen)
      (let ((full (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (org-mode)
          (insert full)
          (journal-reflect--strip-reflection-subtree)
          (string-trim (buffer-string)))))))

(defun journal-reflect--file-without-reflection (file)
  "Contents of FILE with its own Reflection subtree stripped."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (journal-reflect--strip-reflection-subtree)
    (string-trim (buffer-string))))

(defun journal-reflect--previous-context ()
  "Best-effort, labeled text of the previous `journal-reflect-context-days'
entries, marked clearly as background context (not today's entry) with
each day's own Reflection subtree stripped out. Empty string if
org-journal isn't available or nothing is found."
  (if (not (featurep 'org-journal))
      ""
    (condition-case nil
        (let ((entries (journal-reflect--recent-journal-entries journal-reflect-context-days)))
          (if (null entries)
              ""
            (concat
             "=== Context from previous entries (for spotting patterns only -- do not reflect on these directly) ===\n\n"
             (string-join
              (mapcar (lambda (e)
                        (format "--- %s ---\n%s"
                                (format-time-string "%Y-%m-%d" (org-journal--calendar-date->time (car e)))
                                (journal-reflect--file-without-reflection (cdr e))))
                      entries)
              "\n\n"))))
      (error ""))))

(defun journal-reflect--recent-journal-entries (n)
  "Up to N (DATE . FILE) conses for the most recent org-journal entries
before the current one, newest first. DATE is a calendar date
(MONTH DAY YEAR). Delegates to org-journal's own date index
(`org-journal--list-dates') rather than re-deriving file order by
treating filenames as sortable strings, so this follows whatever
`org-journal-file-type' and `org-journal-file-format' are actually
configured (including non-daily file types and encrypted journals)."
  (when (bound-and-true-p org-journal-dir)
    (let* ((dates (reverse (org-journal--list-dates))) ; newest first
           (current (ignore-errors
                      (and (buffer-file-name)
                           (org-journal--file-name->calendar-date (buffer-file-name)))))
           (prior (if current
                      (seq-drop-while
                       (lambda (d) (not (org-journal--calendar-date-compare d current)))
                       dates)
                    dates)))
      (mapcar (lambda (d) (cons d (org-journal--get-entry-path (org-journal--calendar-date->time d))))
              (seq-take prior n)))))

(defun journal-reflect--run ()
  "Kick off an asynchronous Claude call for the current buffer's entry."
  (cond
   (journal-reflect--in-progress
    (message "journal-reflect: already running for this buffer"))
   ((string-empty-p (string-trim (journal-reflect--entry-text)))
    (message "journal-reflect: nothing to reflect on yet"))
   (t
    (let* ((buf (current-buffer))
           (prompt (format journal-reflect-prompt-template (journal-reflect--entry-text)))
           (out-buf (generate-new-buffer " *journal-reflect-output*")))
      ;; `set-in-progress' is scoped to this call via `cl-labels' rather
      ;; than a separate top-level defun, so the flag's t/nil transitions
      ;; -- kickoff here, completion in the sentinel below -- both live
      ;; textually inside this one function instead of being reachable
      ;; (and settable) from anywhere else.
      (cl-labels ((set-in-progress (value)
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (setq journal-reflect--in-progress value)))))
        (set-in-progress t)
        (message "journal-reflect: asking %s..." (journal-reflect--backend-name))
        (let ((proc
               (make-process
                :name "journal-reflect"
                :buffer out-buf
                :command (journal-reflect--build-command)
                ;; Explicit pipe: with the default pty connection, `claude -p'
                ;; doesn't recognize stdin as piped input and errors out.
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal))
                    (let ((output (with-current-buffer out-buf (string-trim (buffer-string))))
                          (status (process-exit-status proc)))
                      (kill-buffer out-buf)
                      (set-in-progress nil)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (if (zerop status)
                              (journal-reflect--insert-reflection output)
                            (message "journal-reflect: %s exited %s: %s"
                                     (journal-reflect--backend-name) status output))))))))))
          (journal-reflect--send-prompt proc prompt)))))))

(defun journal-reflect--build-command ()
  "Build the process command list (without the prompt), per
`journal-reflect-backend'. The prompt is sent separately, over stdin,
by `journal-reflect--send-prompt'."
  (pcase journal-reflect-backend
    ('claude-cli
     (cons journal-reflect-claude-executable journal-reflect-claude-args))
    ('hermes
     (user-error "journal-reflect: Hermes backend not implemented yet"))
    (_ (user-error "journal-reflect: unknown backend %s" journal-reflect-backend))))

(defun journal-reflect--backend-name ()
  "Human-readable name for the current `journal-reflect-backend', for
status messages."
  (pcase journal-reflect-backend
    ('claude-cli "Claude")
    ('hermes "Hermes")
    (backend (symbol-name backend))))

(defun journal-reflect--send-prompt (proc prompt)
  "Send PROMPT to PROC per `journal-reflect-backend' and signal end of input."
  (pcase journal-reflect-backend
    ('claude-cli
     (process-send-string proc prompt)
     (process-send-eof proc))
    (_ (user-error "journal-reflect: unknown backend %s" journal-reflect-backend))))

(defun journal-reflect--insert-reflection (text)
  "Replace (or append) the Reflection subtree with TEXT, then save quietly."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((heading-re (format "^\\*+ %s\\b.*$" (regexp-quote journal-reflect-heading))))
        (if (re-search-forward heading-re nil t)
            (progn
              (goto-char (match-beginning 0))
              (let ((start (point)))
                (org-end-of-subtree t t)
                (delete-region start (point))))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))))
      (insert (format "* %s\n%s\n" journal-reflect-heading text))))
  (let ((journal-reflect--suppress-hook t))
    (save-buffer))
  (message "journal-reflect: reflection updated"))

(provide 'journal-reflect)

;; Local Variables:
;; eval: (add-hook 'after-save-hook
;;                  (lambda ()
;;                    (let* ((dir (file-name-directory (buffer-file-name)))
;;                           (base (file-name-base (buffer-file-name)))
;;                           (test-file (expand-file-name (concat base "-test.el") dir)))
;;                      (when (file-exists-p test-file)
;;                        (compilation-start
;;                         (format "emacs -Q --batch -L %s -l %s -l %s-test -f ert-run-tests-batch-and-exit"
;;                                 (shell-quote-argument dir) base base)
;;                         nil
;;                         (lambda (_mode) (format "*%s-tests*" base))))))
;;                  nil t)
;; End:
;;; journal-reflect.el ends here
