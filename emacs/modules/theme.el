(use-packages '(solarized-theme))

(setq custom/theme-dark-enabled nil)

(defun toggle-theme ()
  (setq custom/theme-dark-enabled (not custom/theme-dark-enabled))
  (if custom/theme-dark-enabled
      (load-theme 'solarized-dark t)
    (load-theme 'solarized-light t)))

(global-set-key (kbd "C-x t") (lambda () (interactive) (toggle-theme)))

(toggle-theme)
