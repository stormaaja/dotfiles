;; JS

(use-packages
 '(js2-mode
   json-mode
   rjsx-mode))

(defun js-mode ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-to-list 'auto-mode-alist '("\\.m?js$" . js2-mode))
(add-hook 'js-mode-hook 'clj-mode)
;(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'html-mode-hook 'subword-mode)


(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-strict-missing-semi-warning nil)

; (eval-after-load "sgml-mode"
;   '(progn
;      (require 'tagedit)
;      (tagedit-add-paredit-like-keybindings)
;      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

;; React (rjsx)
(setq js2-strict-missing-semi-warning nil)

; React (Thanks Tuomas)
;;; RJSX: js2-mode with jsx

(defun enable-rjsx-mode-hook ()
  (setq mode-name "RJSX"))

(add-hook 'rjsx-mode-hook #'enable-rjsx-mode-hook)
