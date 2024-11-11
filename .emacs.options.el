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

