;; Shell

(use-packages '(exec-path-from-shell))

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH"))

;; Set correct carriage returns
(set-buffer-file-coding-system 'mac)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-option-modifier nil)

(setq mac-function-modifier 'control)
(setq mac-command-modifier 'meta)
