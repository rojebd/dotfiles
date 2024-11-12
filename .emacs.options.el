(use-package emacs
  :config
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (blink-cursor-mode 0)
  (ido-mode 1)
  (setq inhibit-startup-screen t)
  (setq custom-file "~/.emacs.custom.el")
  (setq display-line-numbers-type 'relative)
  (setq warning-minimum-level :error)
  (setq backup-directory-alist `(("." . "~/.emacs.saves")))
  (setq exec-path (append exec-path '("/home/roniell/.local/bin")))
  (load custom-file)
  (global-display-line-numbers-mode 1)
  (set-frame-font "Hack Nerd Font Mono-15")
  (setenv "PATH" (concat (getenv "PATH") ":/home/roniell/.local/bin"))

  ; Rebind C-w to kill-word-backwards
  (global-set-key (kbd "C-w") 'backward-kill-word)
  
  ; Rebind kill-region to C-x C-k
  (global-set-key (kbd "C-x C-k") 'kill-region)

  ; NOTE: This makes the normal C-x 3 not work
  (define-key global-map (kbd "C-x 3")
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-f") 'rc/open-file-vertically)
	map)))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

