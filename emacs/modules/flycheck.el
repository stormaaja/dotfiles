(use-packages '(flycheck flycheck-popup-tip))

(global-flycheck-mode)

;(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))
