(use-package emacs
  :bind (("C-," . rc/duplicate-line)
		 ("M-p" . move-text-up)
		 ("M-n" . move-text-down))
  :hook ((after-change-major-mode . rc/hare-setup)
		 (after-change-major-mode . rc/python-setup))
  :config
  (electric-pair-mode t)
  (setq-default backward-delete-char-untabify-method nil)
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
		  ("RET" . nil)
		  ("S-<return>" . corfu-insert))
  :config
  (setq-default corfu-auto nil))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

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
