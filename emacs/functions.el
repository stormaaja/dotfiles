(defun load-module (file-name)
  (load (concat "~/.emacs.d/modules/" file-name)))

(defun load-modules (module-list)
  (dolist (m module-list)
    (load-module m)))

(defun use-packages (package-list)
  (dolist (p package-list)
    (when (not (package-installed-p p))
      (package-install p))))
