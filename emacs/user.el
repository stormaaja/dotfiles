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

(defvar my-packages
  '(
    ace-window
    avy
    blank-mode
    dash
    fill-column-indicator
    highlight-indent-guides
    let-alist
    markdown-mode
    memoize
    projectile
    pkg-info
    epl
    queue
    rainbow-delimiters
    s
    seq
    smex
    spinner
    tagedit
    use-package
    with-editor
    async))

(dolist (p my-packages)
      (when (not (package-installed-p p))
        (package-install p)))

(require 'use-package)

(load "~/.emacs.d/functions.el")
;; Misc

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(setq create-lockfiles nil)

(setq inhibit-startup-message t)


;; Load modules
(when (eq system-type 'gnu/linux)
  (load "./modules/linux.el"))

;; Shell
(when (memq window-system '(mac ns))
  (load-module "osx.el"))

(load-modules
 '(
   "ui.el"
   "flycheck.el"
   "theme.el"
   ;"javascript.el"
   ;"typescript.el"
   "ido.el"
   ;"clojure.el"
   ;"custom-syntax.el"
   "smartparens.el"
   "elisp.el"
   ;"git.el"
   "navigation.el"
   "typescript2.el"
   "autocomplete.el"
   ))

