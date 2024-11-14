'; Load and use Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; add common modules to load-pat and load rc
(add-to-list 'load-path "~/.emacs.rc/")

(require 'rc)
(require 'move-text)
(require 'hare-mode)

; Emacs Options like Keybinds and Editor GUI Configuration
(load-file "~/.emacs.options.el")

; Configuration regarding code (completion, indentation, etc)
(load-file "~/.emacs.editor.el")
