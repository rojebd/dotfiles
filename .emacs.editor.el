; Completion
(rc/require 'corfu)
(global-corfu-mode)
(setq-default corfu-auto nil)
(rc/require 'cape)
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(define-key corfu-map (kbd "RET") nil)
(define-key corfu-map (kbd "S-<return>") 'corfu-insert)

; Electric Pair Mode (autocompletes matching delimeters like parentheses)
(electric-pair-mode t)

; Indent Guides
(rc/require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-auto-character-face-perc 55)
(setq highlight-indent-guides-responsive "top")
(defun my-highlighter (level responsive display)
  (if (> 1 level)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))

(setq highlight-indent-guides-highlighter-function 'my-highlighter)

; Indentation
; Emacs by default uses tabs
; (setq tab-width 4)
(setq-default backward-delete-char-untabify-method nil)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

; Duplicate line
(global-set-key (kbd "C-,") 'rc/duplicate-line)

; Multi-Cursors
(rc/require 'multiple-cursors)
(global-set-key (kbd "C-M-f")       'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-;")         'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

