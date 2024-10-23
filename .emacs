; Load and use Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; load rc.el
(load "~/.emacs.rc/rc.el")

; Remove Menu-Bar, Tool-Bar, Scroll-Bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

; Backup files directory
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

; Shell Path
(setenv "PATH" (concat (getenv "PATH") ":/home/roniell/.local/bin"))
(setq exec-path (append exec-path '("/home/roniell/.local/bin")))

; Line Numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

; Disable splash startup screen
(setq inhibit-startup-screen t)

; Font
(set-frame-font "Hack Nerd Font Mono-14")

; Theme (Colorscheme)
(rc/require-theme 'gruvbox)

; Set minium warning level
(setq warning-minimum-level :error)

; Ido
(ido-mode 1)

; Smex
(rc/require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

; Cursor
;(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

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
(setq backward-delete-char-untabify-method nil)
(setq tab-width 4)

; Duplicate line
(global-set-key (kbd "C-,") 'rc/duplicate-line)

; Multi-Cursors
(rc/require 'multiple-cursors)
(global-set-key (kbd "C-M-f") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)
