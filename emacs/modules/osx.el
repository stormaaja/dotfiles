;; Shell

(use-packages '(exec-path-from-shell))

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH"))
