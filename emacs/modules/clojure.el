;; Clojure

(use-packages
 '(clj-refactor
   clojure-mode-extra-font-locking
   clojure-mode
   flycheck-clojure
   cider))

(add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)

(add-hook 'clojure-mode-hook 'subword-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(require 'clojure-mode-extra-font-locking)
(require 'cider)

;(setq cider-inject-dependencies-at-jack-in nil)

(setq cider-latest-middleware-version "0.21.2-SNAPSHOT")

(defun clj-mode ()
  (setq inferior-lisp-program "lein repl")
  (font-lock-add-keywords
   nil
   '(("(\\(facts?\\)"
      (1 font-lock-keyword-face))
     ("(\\(background?\\)"
      (1 font-lock-keyword-face))))

  (define-clojure-indent (fact 1))
  (define-clojure-indent (facts 1))
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'clojure-mode-hook 'clj-mode)

(setq clojure-indent-style :align-arguments)

(add-hook 'cider-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'cider-mode-hook 'turn-on-fci-mode)
(add-hook 'cider-mode-hook 'subword-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'clj-mode)

(setq cider-repl-pop-to-buffer-on-connect t)

(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

(setq cider-repl-history-file "~/.emacs.d/cider-history")

(setq cider-repl-wrap-history t)

(add-hook 'cider-repl-mode-hook 'turn-on-smartparens-strict-mode)
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
;(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(defun set-clojure-indents ()
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)
    (defschema 0)
    (defq 0)
    (defq-hid 0)
    (route-middleware 0)
    (form-to 1)
    (match 1)
    (are 2)
    (checking 2)
    (async 1)
    (select 1)
    (insert 1)
    (update 1)
    (delete 1)
    (run* 1)
    (fresh 1)
    (extend-freeze 2)
    (extend-thaw 1)
    (go-loop 1)
    (this-as 1)
    (specify 1)
    (specify! 1)
    (s/fdef 1)))

(add-hook 'clojure-mode-hook 'set-clojure-indents)

(require 'clj-refactor)

(defun set-clj-refactor ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'set-clj-refactor)

;; Flycheck

(eval-after-load 'cider '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

;(eval-after-load 'flycheck '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))
