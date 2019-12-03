(require 'generic-x)

(defun set-simli-indent ()
  (setq tab-width 2)
  (setq-default indent-tabs-mode nil))

(define-generic-mode
    'simli-mode
  '(";")
  '("->" "prn" "true" "false" "map" "reduce" "if" "cond" "inc" "str"
    "int" "double" "string" "boolean" "defn")
  '()
  '("\\.simli$")
  '(set-simli-indent)
  "A mode for simli files")
