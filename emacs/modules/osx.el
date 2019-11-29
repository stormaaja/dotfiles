;; Shell

(use-packages '(exec-path-from-shell))

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH"))

;; Set correct carriage returns
(set-buffer-file-coding-system 'mac)
