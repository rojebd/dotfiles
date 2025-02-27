; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

; UI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

; Line Numbers
(setq display-line-numbers 'relative)
(global-display-line-numbers-mode t)

; Disable backup files and auto saves
(setq make-backup-files nil) ; stop creating ~ files
(setq auto-save-default nil)

; Make scrolling easier for me, no teleporting like behavior
(setq scroll-step 1)
(setq scroll-margin 8)

; Ido mode
(ido-mode t)

; Smex
(use-package smex
    :ensure t
    :init (smex-initialize)
    :bind (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)
           ("C-c C-c M-x" . execute-extended-command)))

; Ruler at 80
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode 80)

; Load theme
; autothemer is a depedency
(use-package autothemer
    :ensure t)
(load-theme 'oxocarbon t)
(enable-theme 'oxocarbon)

; Custom file so it does not polute the init.el file
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

; Disable startup screen
(setq inhibit-startup-screen t)

; Font
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font-14:style=Regular"))
; Disable bold and italic just use normal/regular
(mapc
    (lambda (face)
        (set-face-attribute face nil :weight 'normal :underline nil))
    (face-list))

; Tree-Sitter
; Treesitter parsers/modes installed: C, Python
(setq major-mode-remap-alist
  '(((c-mode . c-ts-mode)))

; Indent Bars (highlight indent lines)
(use-package indent-bars
    :ensure t
    ; For both the tree-sitter mode and the normal mode
    :hook ((c-ts-mode c-mode) . indent-bars-mode))

; Electrir Pair Mode (autopairs)
(setq-default electric-indent-inhibit nil)
(electric-pair-mode)

; Disable warnings
(setq warning-minimum-level :emergency)

; Indentation
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method 'all)
(global-set-key (kbd "DEL") 'backward-delete-char-untabify)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-ts-mode-indent-offset 4)
;(setq python-indent-offset 4)
;(setq python-indent-guess-indent-offset nil)
;(setq python-indent-guess-indent-offset t)  
;(setq python-indent-guess-indent-offset-verbose nil)

; Company
(use-package company
    :ensure t
    :init (global-company-mode)
    :config
    (add-hook 'after-init-hook 'global-company-mode))

; Clang format individually without clangd eglot
;(load-file "/usr/share/clang/clang-format.el")
;(require 'clang-format)

;(defun clang-format-hook ()
;  (when (member (file-name-extension buffer-file-name) '("c" "cc" "cxx" "cpp" "h" "hxx" "hpp"))
;    (add-hook 'before-save-hook 'clang-format-buffer nil t)))

;(add-hook 'c-mode-hook 'clang-format-hook)
;(add-hook 'c-ts-mode-hook 'clang-format-hook)

; Eglo (LSP)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c-mode c-ts-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)

(defun eglot-custom-format-hook ()
  (when (member (file-name-extension buffer-file-name) '("c" "cc" "cxx" "cpp" "h" "hxx" "hpp"))
    (add-hook 'before-save-hook 'eglot-format-buffer)))

(add-hook 'c-mode-hook 'eglot-custom-format-hook)
(add-hook 'c-ts-mode-hook 'eglot-custom-format-hook)
