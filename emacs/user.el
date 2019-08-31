(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

(defvar my-packages
  '(
    ace-window
    avy
    blank-mode
    cider
    clj-refactor
    clojure-mode-extra-font-locking
    clojure-mode
    dash
    exec-path-from-shell
    fill-column-indicator
    flycheck
    flycheck-clojure
    git-commit
    ghub
    highlight-indent-guides
    ido-ubiquitous
    ido-completing-read+
    js2-mode
    json-mode
    less-css-mode
    let-alist
    magit
    magit-popup
    markdown-mode
    memoize
    nasm-mode
    projectile
    pkg-info
    epl
    queue
    rainbow-delimiters
    restclient
    rjsx-mode
    s
    seq
    smartparens
    smex
    spinner
    tagedit
    tide
    web-mode
    with-editor async
    xclip
    zenburn
   ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(require 'xclip)
(xclip-mode 1)
;(turn-on-xclip)

;; Navigation
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t)

(setq ido-use-filename-at-point nil)

(setq ido-auto-merge-work-directories-length -1)

(setq ido-use-virtual-buffers t)

(ido-ubiquitous-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'ace-window)

(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(projectile-mode)

;; UI
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkgray")
(setq fci-rule-column 80)
(add-hook 'after-change-major-mode-hook 'turn-on-fci-mode)

(menu-bar-mode -1)

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

;; Editing
(global-set-key (kbd "M-/") 'hippie-expand)
(standard-display-ascii ?\t "^I")
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(show-paren-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default fill-column 80)

(global-hl-line-mode 1)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq-default indent-tabs-mode nil)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(setq electric-indent-mode nil)

;; Misc

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(setq create-lockfiles nil)

(setq inhibit-startup-message t)

;;; Smartparens

(require 'smartparens-config)

(smartparens-global-mode)
(show-smartparens-global-mode)
(sp-use-paredit-bindings)

;; Elisp

(autoload 'turn-on-smartparens-strict-mode "smartparens" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'turn-on-smartparens-strict-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'turn-on-smartparens-strict-mode)
(add-hook 'ielm-mode-hook             #'turn-on-smartparens-strict-mode)
(add-hook 'lisp-mode-hook             #'turn-on-smartparens-strict-mode)
(add-hook 'lisp-interaction-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'scheme-mode-hook           #'turn-on-smartparens-strict-mode)


;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Clojure

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

;; JS

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

;; Latex

(setq TeX-quote-after-quote 0)
(setq flycheck-check-syntax-automatically '(save mode-enable))

;; React (rjsx)
;(setq js2-strict-missing-semi-warning nil)

; React (Thanks Tuomas)
;;; RJSX: js2-mode with jsx

; (defun enable-rjsx-mode-hook ()
;   (setq mode-name "RJSX"))

; (add-hook 'rjsx-mode-hook #'enable-rjsx-mode-hook)

;;; TypeScript

; (require 'tide)

; (customize-set-variable 'typescript-indent-level 2)

; (defun tide-common-setup ()
;   (interactive)
;   (tide-setup)
;   (flycheck-mode)
;   (eldoc-mode)
;   (tide-hl-identifier-mode)
;   (company-mode))

;;; TypeScript: .ts sources

; (add-hook 'typescript-mode-hook #'tide-common-setup)

;;; TypeScript: .js sources

; (add-hook 'js2-mode-hook #'tide-common-setup)

;;; TypeScript: .tsx sources

; (defun tide-tsx-setup ()
;   (when (string-equal "tsx" (file-name-extension buffer-file-name))
;     (tide-common-setup)))

; (require 'web-mode)
; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
; (add-hook 'web-mode-hook
;           (lambda ()
;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;               (setup-tide-mode))))

;; enable typescript-tslint checker
; (flycheck-add-mode 'typescript-tslint 'web-mode)

;;; TypeScript: .jsx sources

; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;;; Java
;(setq jdee-server-dir "/home/matti/tmp/jdee-server/target/")

;; nasm

(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

;;; Custom syntax

(require 'generic-x)

(defun set-emfun-indent ()
  (setq tab-width 2)
  (setq-default indent-tabs-mode nil)
  (setq c-default-style "bsd"
        c-basic-offset 2))

(define-generic-mode
    'emfun-mode
  '("#")
  '("->" "print" "true" "false" "map" "reduce" "if" "cond" "inc" "str"
    "int" "double" "string" "boolean" "any")
  '()
  '("\\.emf$")
  '(set-emfun-indent)
  "A mode for emFun files")

(define-generic-mode
      'emcee-mode                         ;; name of the mode to create
      '("//")                           ;; comments start with '!!'
      '("print" "return"
        "int" "string" "double" "bool" "void")        ;; some keywords
      '(("=" . 'font-lock-operator)     ;; '=' is an operator
        (";" . 'font-lock-builtin))     ;; ';' is a built-in
      '("\\.emc$")                      ;; files for which to activate this mode
       nil                              ;; other functions to call
      "A mode for emCee files"            ;; doc string for this mode
    )
