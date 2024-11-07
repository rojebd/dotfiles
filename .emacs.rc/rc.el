(defvar rc/package-refreshed nil)

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

(provide 'rc)
