(use-packages '(ido-ubiquitous
                ido-completing-read+))

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t)

(setq ido-use-filename-at-point nil)

(setq ido-auto-merge-work-directories-length -1)

(setq ido-use-virtual-buffers t)

(ido-ubiquitous-mode 1)
