;; UI
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkgray")
(setq fci-rule-column 80)
(add-hook 'after-change-major-mode-hook 'turn-on-fci-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-to-list 'load-path "~/.emacs.d/themes")

(load-theme 'zenburn t)

(set-face-attribute 'default nil :height 140)

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(blink-cursor-mode 0)

(setq-default frame-title-format "%b (%f)")

(global-set-key (kbd "s-t") '(lambda () (interactive)))

(setq ring-bell-function 'ignore)
