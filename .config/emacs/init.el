;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Load modes and require them
(add-to-list 'load-path "~/.config/emacs/modes")
(require 'simpc-mode)

;; Useless GUI stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Paren mode
(show-paren-mode 1)

;; Custom file
(setq custom-file "~/.config/emacs/custom.el")
(load-file "~/.config/emacs/custom.el")

;; Warnings disable
(setq warning-minimum-level :error)

;; Ido mode
(ido-mode 1)
(ido-everywhere 1)
(use-package ido-completing-read+
  :ensure t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Smex
(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; Theme/Colorscheme
(load-theme 'gruber-darker t)

;; Font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-14" )
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-14" ))

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Modeline display (line, column) instead of L{line}
(setq column-number-mode t)

;; Column Ruler
(setopt display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Non blinking cursor
(blink-cursor-mode 0)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method 'all)
(setq tab-width 4)

;; Startup screen disable
(setq inhibit-startup-screen t)

;; Disable backup files and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Scroll off and sidescroll like options from vim into emacs
(setq scroll-step 1)
(setq scroll-margin 8)

;; Indent Bars
(use-package indent-bars
  :ensure t
  :hook ((c-mode) . indent-bars-mode))

;; Duplicate line and go to next line, this overwrites suspend-frame keybind.
(bind-key "C-c C-z"
          '(lambda ()
            (interactive)
            (duplicate-line)
            (next-line)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  ;; Overwrite global keybinds
  ;; https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
  (bind-key* "C-c C-e"         'mc/edit-lines)
  (bind-key* "C-c C-n"         'mc/mark-next-like-this)
  (bind-key* "C-c C-p"         'mc/mark-previous-like-this)
  (bind-key* "C-c C-a"         'mc/mark-all-like-this)
  (bind-key* "C-c C-s n"       'mc/skip-to-next-like-this)
  (bind-key* "C-c C-s p"       'mc/skip-to-previous-like-this))

;; Corfu for comppletion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.5)
  (corfu-preselect-first nil)
  (corfu-preview-current nil)
  :bind (:map corfu-map
              ("RET" . nil)
              ("C-p" . corfu-previous)
              ("C-n" . corfu-next)
              ("C-y" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Needed for corfu
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file))

;; C mode
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
(setq-default c-basic-offset tab-width
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "k&r")))
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         ;; Line comments
                         (c-toggle-comment-style -1)))

;; whitespace
(defun whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(setq whitespace-style '(face tabs spaces trailing space-before-tab
                              newline indentation empty space-after-tab
                              space-mark tab-mark))

(add-hook 'c-mode-hook 'whitespace-handling)
(add-hook 'simpc-mode-hook 'whitespace-handling)

;; Dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-mouse-drag-files t)

;; Pinentry for gpg signing (use with magit)
(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

;; Magit
(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-mode nil)
  :bind (("C-c g s" . magit-status)
         ("C-c g l" . magit-log)))

;;; Move Text
(use-package move-text
  :ensure t
  :bind (("M-p"  . move-text-up)
         ("M-n" . move-text-down)))

;; TODO FOR SOME THINGS THAT I COULD DO BELOW:
;; lsp emacs, Clang format, LSP (using LSP or clang-format-hook?), eglot
;; and lsp-ui can be used together?
;; search about eglot and lsp-ui usage? i did not know they could be mixed and
;; used together
;; make modeline slim to make sure it does not show too many modes
;; see tsoding vides for him configuring his emacs
;; https://git.sr.ht/~oldfashionedcow/dotfiles/tree/master/item/config/emacs/init.el#L159
;; https://youtu.be/81MdyDYqB-A?t=6893
