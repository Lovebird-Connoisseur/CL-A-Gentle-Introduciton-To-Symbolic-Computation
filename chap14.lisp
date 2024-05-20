;; ex 14.1:  (macroexpand '(pop x))
;; -> (LET ((#:G296 (CAR X)))
;;      (PROGN (SETQ X (CDR X)) #:G296))

;; ex 14.2: a page and a half of magic stuff, apparently

;; ex 14.3
(defmacro set-nil (var)
  "Sets a variable to NIL."
  (list 'setq var NIL))

;; ex 14.4
(defmacro simple-rotatef (var1 var2)
  "Assigns var1s value to var2 and vice-versa."
  `(let ((tmp ,var1))
     (setq ,var1 ,var2)
     (setq ,var2 tmp)))

;; ex 14.5 ???
(defmacro set-mutual (var1 var2)
  "Assigns each var to the name of eachother."
  `(progn (setq ,var1 ',var2)
          (setq ,var2 ',var1)))

;; ex 14.6 (wrong! this one was complicated! macros are hard stuff!)
(defmacro variable-chain (&rest vars)
  `(progn ,@(mapcar #'(lambda (var1 var2)
                        (progn (list 'setf var1 'var2)
                               (list 'setf var2 'var1)))
                    vars)))

;; ex 14.8: because they might get expanded in different situations, depending on the situation

;; ex 14.9:
;; my guesses: lambda, let?, block,

;; ex 14.10: 10 to 100 times faster
