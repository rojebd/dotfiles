(use-package emacs
  :bind (("C-," . rc/duplicate-line)
		 ("M-p" . move-text-up)
		 ("M-n" . move-text-down))
  :hook ((after-change-major-mode . rc/hare-setup)
		 (after-change-major-mode . rc/python-setup)
		 (before-save             . rc/lsp-format-on-save)
		 (after-change-major-mode . rc/purge-minor-modes))
  :config
  (electric-pair-mode t)
  (setq-default backward-delete-char-untabify-method nil)
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
 )

;; (use-package corfu
;;   :ensure t
;;   :init
;;   (global-corfu-mode)
;;   :bind (:map corfu-map
;; 		  ("RET" . nil)
;; 		  ("S-<return>" . corfu-insert))
;;   :config
;;   (setq-default corfu-auto nil))

;; (use-package cape
;;   :ensure t
;;   :init
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 55)
  (setq highlight-indent-guides-responsive "top")
  (setq highlight-indent-guides-highlighter-function 'rc/highlighter))


(use-package multiple-cursors
  :ensure t
  :bind (("C-M-f" . mc/edit-lines)
		 ("C->"   . mc/mark-next-like-this)
		 ("C-<"   . mc/mark-previous-like-this)
		 ("C-;"   . mc/mark-all-like-this)
		 ("C-\""  . mc/skip-to-next-like-this)
		 ("C-:"   . mc/skip-to-previous-like-this)))

(require 'treesit)
(require 'hare-ts-mode)

; Tree Sitter
""" Tree Sitter Grammars Installed
IMPORTANT: When installing a new grammar disable the git config
setting  that enforces ssh, since asks for my password and then
emacs cannot download the grammars

- Python: default one
- Hare: https://git.sr.ht/~ecs/tree-sitter-hare, branch: tsh
"""

(setq major-mode-remap-alist
  '((python-mode  . python-ts-mode)
	(hare-mode    . hare-ts-mode)))


(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c")
  :hook ((python-ts-mode . lsp-deferred)
		 )
  :commands lsp-deferred
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-file-watch-threshold 100)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-segments '(count))
  )

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-ts-mode . (lambda ()
							(require 'lsp-pyright)
							(lsp-deferred)))
  :config
  (setq lsp-pyright-type-checking-mode "basic")
  )


(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'left-fringe)
  (setq flycheck-highlighting-mode 'symbols)
  )

