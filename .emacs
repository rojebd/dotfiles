; Load and use Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; load rc.el
(add-to-list 'load-path "~/.emacs.rc/")
(require 'rc)

; Remove Menu-Bar, Tool-Bar, Scroll-Bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

; ANSI Colorize compilation buffer
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

; Set emacs custom file location to output (custom-variables)
(setq custom-file "~/.emacs.custom.el")
(load custom-file)

; Ido
(ido-mode 1)

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

; Keyboard Binds
; Binds C-x 3 C-f to rc/open-file-vertically
(define-key global-map (kbd "C-x 3") 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") 'rc/open-file-vertically)
    map))

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
(set-frame-font "Hack Nerd Font Mono-16")

; Theme (Colorscheme)
(rc/require-theme 'srcery)

; Set minium warning level
(setq warning-minimum-level :error)

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
