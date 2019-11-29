
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
