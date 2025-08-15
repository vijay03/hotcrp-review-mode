;;; hotcrp-review-mode.el --- Standalone HotCRP review editor with counts  -*- lexical-binding: t; -*-

;; Author: Vijay Chidambara + ChatGPT
;; Version: 0.1
;; Keywords: outlines, tools
;; Package-Requires: ((emacs "26.1"))
;; URL: (local)

;;; Commentary:
;; A single-file major mode for editing HotCRP offline text review forms.
;; Features:
;; - Form prompts vs. your responses are visually distinct.
;; - Per-review word counts that ignore the form text.
;; - Inline badges and a modeline readout, with a warning when under threshold.
;;
;; It recognizes sections like:
;;   ==+== Paper #123
;;   ==-== Title: ...
;;   Overall merit:
;;   Your text here...
;;
;; Nothing here depends on any other HotCRP modes or curl; this is a pure editor.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defgroup hotcrp-review nil
  "Standalone HotCRP review editor."
  :group 'text)

(defcustom hotcrp-review-word-threshold 500
  "Minimum words required for a review before it is considered complete."
  :type 'integer :group 'hotcrp-review)

(defcustom hotcrp-review-show-header-line-warning t
  "When non-nil, show a header-line warning if the current review is under threshold."
  :type 'boolean :group 'hotcrp-review)

(defcustom hotcrp-review-show-modeline t
  "When non-nil, show a modeline segment with the current review word count."
  :type 'boolean :group 'hotcrp-review)

(defcustom hotcrp-review-ignore-line-regexps
  '("^==[+-]== "                 ; HotCRP headings
    "^\\s-*\\[.*\\]\\s*$"        ; bracketed notes/instructions
    "^[^ \t\n].*:\\s*$"          ; prompt lines ending with colon
    "^\\s-*$")                   ; empty lines
  "Regexps for lines to ignore when counting words."
  :type '(repeat (string)) :group 'hotcrp-review)

(defface hotcrp-review-form-face
  '((((class color) (min-colors 88)) :inherit shadow :slant italic)
    (t :inherit shadow))
  "Face used for form prompts/headings."
  :group 'hotcrp-review)

(defface hotcrp-review-badge-ok
  '((t :inherit success :weight bold))
  "Face for OK word-count badge."
  :group 'hotcrp-review)

(defface hotcrp-review-badge-warn
  '((t :inherit warning :weight bold))
  "Face for warning word-count badge."
  :group 'hotcrp-review)

;; -----------------------------------------------------------------------------
;; Internals (helpers defined first to avoid forward-ref warnings)
;; -----------------------------------------------------------------------------

(defvar-local hotcrp-review--badge-overlays nil)
(defvar-local hotcrp-review--modeline-string "")
(defvar-local hotcrp-review--refresh-timer nil)

(defun hotcrp-review--clear-badge-overlays ()
  (mapc #'delete-overlay hotcrp-review--badge-overlays)
  (setq hotcrp-review--badge-overlays nil))

(defun hotcrp-review--line-matches-any (regexps)
  (save-excursion
    (beginning-of-line)
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (cl-some (lambda (re) (string-match-p re line)) regexps))))

(defun hotcrp-review--count-range (start end)
  "Count words in region START..END, ignoring form lines."
  (save-excursion
    (let ((words 0)
          (resyntax (syntax-table)))
      (with-syntax-table (copy-syntax-table resyntax)
        ;; Treat underscore as word constituent (common in identifiers).
        (modify-syntax-entry ?_ "w")
        (goto-char start)
        (while (< (point) end)
          (if (hotcrp-review--line-matches-any hotcrp-review-ignore-line-regexps)
              (forward-line 1)
            (let ((here (point)))
              (while (and (< (point) end)
                          (not (eobp))
                          (not (bolp))
                          (not (eolp))
                          (not (hotcrp-review--line-matches-any hotcrp-review-ignore-line-regexps)))
                ;; Count with forward-word; this roughly matches \\w+ tally.
                (let ((p (point)))
                  (ignore-errors (forward-word 1))
                  (when (> (point) p)
                    (cl-incf words))))
              ;; Move to next line (avoid tight loops on edge cases).
              (goto-char here)
              (forward-line 1)))))
      words)))

(defun hotcrp-review--paper-bounds-at (pos)
  "Return plist (:paper :start :end) for the review surrounding POS, or nil."
  (save-excursion
    (goto-char pos)
    (when (re-search-backward "^==\\+== Paper #\\([0-9]+\\)" nil t)
      (let* ((paper (match-string-no-properties 1))
             (start (line-beginning-position))
             (end (save-excursion
                    (if (re-search-forward "^==\\+== Paper #\\([0-9]+\\)" nil t)
                        (match-beginning 0)
                      (point-max)))))
        (list :paper paper :start start :end end)))))

(defun hotcrp-review--scan-papers ()
  "Return list of plists (:paper :start :end) for all reviews in buffer."
  (save-excursion
    (goto-char (point-min))
    (let (out)
      (while (re-search-forward "^==\\+== Paper #\\([0-9]+\\)" nil t)
        (let* ((paper (match-string-no-properties 1))
               (start (line-beginning-position))
               (end (save-excursion
                      (if (re-search-forward "^==\\+== Paper #\\([0-9]+\\)" nil t)
                          (match-beginning 0)
                        (point-max)))))
          (push (list :paper paper :start start :end end) out)))
      (nreverse out))))

(defun hotcrp-review--format-badge (words)
  "Return propertized badge string for WORDS."
  (let* ((need hotcrp-review-word-threshold)
         (ok   (>= words need))
         (remain (max 0 (- need words)))
         (text (if ok
                   (format "[HC:%dw ✓]" words)
                 (format "[HC:%dw ⚠ -%d]" words remain)))
         (face (if ok 'hotcrp-review-badge-ok 'hotcrp-review-badge-warn)))
    (propertize text 'face face)))

(defun hotcrp-review--put-badge-at-eol (pos badge)
  "Attach BADGE overlay after end of line at POS."
  (save-excursion
    (goto-char pos)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (ov  (make-overlay eol eol)))
      (overlay-put ov 'after-string (concat " " badge))
      (overlay-put ov 'hotcrp-review-badge t)
      (push ov hotcrp-review--badge-overlays))))

(defun hotcrp-review--refresh-badges ()
  "Refresh inline word-count badges after each Paper # header."
  (save-excursion
    (hotcrp-review--clear-badge-overlays)
    (dolist (pl (hotcrp-review--scan-papers))
      (let* ((start (plist-get pl :start))
             (end   (plist-get pl :end))
             (words (hotcrp-review--count-range start end))
             (badge (hotcrp-review--format-badge words)))
        (hotcrp-review--put-badge-at-eol start badge)))))

(defun hotcrp-review--update-modeline-and-header ()
  "Update modeline and optional header line for the current review."
  (let* ((pl (hotcrp-review--paper-bounds-at (point)))
         (need hotcrp-review-word-threshold))
    (if (not pl)
        (setq hotcrp-review--modeline-string "")
      (let* ((words (hotcrp-review--count-range (plist-get pl :start)
                                                (plist-get pl :end)))
             (ok    (>= words need))
             (rem   (max 0 (- need words)))
             (paper (plist-get pl :paper)))
        (setq hotcrp-review--modeline-string
              (if hotcrp-review-show-modeline
                  (format " HC P#%s:%dw%s"
                          paper words (if ok "" (format " (-%d)" rem)))
                ""))
        (when hotcrp-review-show-header-line-warning
          (setq header-line-format
                (when (not ok)
                  (propertize
                   (format "  ⚠ Review %s under threshold by %d words"
                           paper rem)
                   'face 'hotcrp-review-badge-warn))))))))

(defun hotcrp-review--debounced-refresh (_beg _end _len)
  "Debounced refresh after buffer edits."
  (when (timerp hotcrp-review--refresh-timer)
    (cancel-timer hotcrp-review--refresh-timer))
  (setq hotcrp-review--refresh-timer
        (run-with-idle-timer
         0.2 nil
         (lambda ()
           (when (derived-mode-p 'hotcrp-review-mode)
             (save-excursion
               (hotcrp-review--refresh-badges)
               (hotcrp-review--update-modeline-and-header)
               (force-mode-line-update)))))))

;; -----------------------------------------------------------------------------
;; User commands
;; -----------------------------------------------------------------------------

(defun hotcrp-review-count-current ()
  "Echo the response-only word count for the current review."
  (interactive)
  (let ((pl (hotcrp-review--paper-bounds-at (point))))
    (if (not pl)
        (message "Not on a Paper # section")
      (let* ((w (hotcrp-review--count-range (plist-get pl :start)
                                            (plist-get pl :end))))
        (kill-new (number-to-string w))
        (message "Review P#%s: %d words (copied)"
                 (plist-get pl :paper) w)))))

(defun hotcrp-review-summarize ()
  "Show a summary buffer listing word counts for all reviews."
  (interactive)
  (let ((papers (hotcrp-review--scan-papers)))
    (if (null papers)
        (message "No Paper # sections found")
      (with-current-buffer (get-buffer-create "*HotCRP Review Summary*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "%-10s %-8s %s\n" "Paper" "Words" "Status"))
        (insert (make-string 40 ?-) "\n")
        (dolist (pl papers)
          (let* ((p (plist-get pl :paper))
                 (w (hotcrp-review--count-range (plist-get pl :start)
                                                (plist-get pl :end)))
                 (ok (>= w hotcrp-review-word-threshold)))
            (insert (format "%-10s %-8d %s\n"
                            p w (if ok "OK" (format "UNDER by %d"
                                                    (- hotcrp-review-word-threshold w)))))))
        (goto-char (point-min))
        (view-mode 1)
        (pop-to-buffer (current-buffer))))))

(defun hotcrp-review-toggle-header-line ()
  "Toggle header-line warning visibility."
  (interactive)
  (setq hotcrp-review-show-header-line-warning
        (not hotcrp-review-show-header-line-warning))
  (hotcrp-review--update-modeline-and-header)
  (message "Header-line warnings %s"
           (if hotcrp-review-show-header-line-warning "on" "off")))

;; -----------------------------------------------------------------------------
;; Major mode definition
;; -----------------------------------------------------------------------------

(defvar hotcrp-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-w") #'hotcrp-review-count-current)
    (define-key map (kbd "C-c C-s") #'hotcrp-review-summarize)
    (define-key map (kbd "C-c C-h") #'hotcrp-review-toggle-header-line)
    map)
  "Keymap for `hotcrp-review-mode'.")

(defconst hotcrp-review--font-lock
  `(( "^==\\+== .*" . 'hotcrp-review-form-face)
    ( "^==[-+]== .*" . 'hotcrp-review-form-face)
    ( "^[^ \t\n].*:\\s*$" . 'hotcrp-review-form-face)
    ( "^\\s-*\\[.*\\]\\s*$" . 'hotcrp-review-form-face))
  "Font-lock rules that treat form text as de-emphasized.")

(define-derived-mode hotcrp-review-mode text-mode "HotCRP-Review"
  "Major mode for editing HotCRP offline reviews with response-only word counts.
- Form prompts/headers are visually distinct.
- Word counts ignore form text and blank lines.
- Inline badges per Paper #, plus optional modeline and header-line warning."
  (setq-local font-lock-defaults '(hotcrp-review--font-lock))
  ;; Keep paragraphs separate across form separators.
  (setq-local paragraph-separate (concat "^==.== \\|" paragraph-separate))
  ;; Modeline hook.
  (when hotcrp-review-show-modeline
    (setq mode-line-format
          (append mode-line-format
                  '((:eval hotcrp-review--modeline-string)))))
  ;; Initial pass.
  (hotcrp-review--refresh-badges)
  (hotcrp-review--update-modeline-and-header)
  ;; Live updates.
  (add-hook 'after-change-functions #'hotcrp-review--debounced-refresh nil t)
  (add-hook 'post-command-hook #'hotcrp-review--update-modeline-and-header nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\`==\\+== .* Paper Review Form" . hotcrp-review-mode))

(provide 'hotcrp-review-mode)
;;; hotcrp-review-mode.el ends here
