;; Linux

(use-packages '(xclip exec-path-from-shell))

(require 'xclip)
(xclip-mode 1)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH"))
