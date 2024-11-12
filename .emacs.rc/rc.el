(defvar rc/package-refreshed nil)

; Add more modes to activate format on save here
; 'python-ts-mode 'other-mode 'other-mode-2
(defun rc/lsp-format-on-save ()
  (when (derived-mode-p 'python-ts-mode)
	(lsp-format-buffer)))

(defun rc/highlighter (level responsive display)
  (if (> 1 level)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))

(defun rc/refresh ()
  (unless rc/package-refreshed
    (setq rc/package-refreshed t)
    (package-refresh-contents)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (rc/refresh)
      (package-install package))))

(defun rc/require-theme (theme)
  (let ((theme-package (intern (concat (symbol-name theme) "-theme"))))
    (rc/require theme-package)
    (load-theme theme t)))


; Taken from: https://github.com/rexim/dotfiles/blob/master/.emacs.rc/misc-rc.el
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun rc/open-file-vertically ()
  (interactive)
  (let ((file (ido-read-file-name "Select file: "))) ;; Prompt for a file
    (if file
        (progn
          (split-window-right)
          (other-window 1)
          (find-file file)))))

(defvar rc/highlight-patterns
  '("TODO" "FIXME" "XXX" "DONE" "NOTE")
  "List of regex patterns to highlight.")

(defvar rc/hare-highlight-patterns "[!?]")

(defun rc/hare-setup ()
  (when (eq major-mode 'hare-ts-mode)
    (local-set-key (kbd "S-<backspace>") (lambda () (interactive) (delete-backward-char 3)))
    (hi-lock-mode 1)
	(setq fill-column 80)
	(display-fill-column-indicator-mode)
	(highlight-regexp rc/hare-highlight-patterns "hi-red-b")
    (highlight-regexp (regexp-opt rc/highlight-patterns 'words) "hi-red-b")
    ;; Reapply highlighting after changes
    (add-hook 'after-change-functions #'rc/reapply-highlighting nil t)))

(defun rc/python-setup ()
  (when (eq major-mode 'python-ts-mode)
	(hi-lock-mode 1)
	(setq fill-column 80)
	(display-fill-column-indicator-mode)
    (highlight-regexp (regexp-opt rc/highlight-patterns 'words) "hi-red-b")
    ;; Reapply highlighting after changes
    (add-hook 'after-change-functions #'rc/reapply-highlighting nil t)))


(defun rc/reapply-highlighting (beg end length)
  "Reapply the highlighting for TODO, FIXME, and XXX and !? when in hare-mode."
  (when (eq major-mode 'hare-ts-mode)
	(highlight-regexp rc/hare-highlight-patterns "hi-red-b")
	(highlight-regexp (regexp-opt rc/highlight-patterns 'words) "hi-red-b"))
  ;; Reapply the highlight-regexp for each defined pattern
  (highlight-regexp (regexp-opt rc/highlight-patterns 'words) "hi-red-b"))

; Remove minor modes in here from cluttering up the modeline
(defvar hidden-minor-modes
  '(abbrev-mode
	lsp-mode
	highlight-indent-guides-mode
	eldoc-mode
	hi-lock-mode
    auto-fill-function
    flyspell-mode))

(defun rc/purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(provide 'rc)
