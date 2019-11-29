;;; Smartparens

(use-packages '(smartparens))

(require 'smartparens-config)

(smartparens-global-mode)
(show-smartparens-global-mode)
(sp-use-paredit-bindings)
