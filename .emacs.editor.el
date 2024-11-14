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
  (setq read-process-output-max (* 1024 1024)))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-character-face-perc 55)
  (highlight-indent-guides-responsive "top")
  (highlight-indent-guides-highlighter-function 'rc/highlighter))

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
