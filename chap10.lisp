;; ex 10.1
;; 1: It will probably give us a unitialized variable error
;; 2: It will throw a type error (when trying to add n to *total-glasses*

;; ex 10.2
(defun sell (n)
  (incf *total-glasses* n)
  (format t
          "~&That makes ~s glasses so for todat."
          *total-glasses*))

;; ex 10.3
(defun meet (person)
  (cond ((equal person (first *frieds*))
         (incf *already-met*)
         'we-just-met)
        ((member person *friends*)
         (incf *already-met*)
         'we-know-each-other)
        (t (push person *friends)
           'pleased-to-meet-you)))

;; ex 10.4
(defun forget (person)
  "Removes a person from the friendslist."
  (cond ((member person *friends*) (setf *friends* (remove person *friends*)))
        (t (format t "~&~s isn't on the friendslist." person))))

;; ex 10.5
(defun beautiful (x y)
  (let* ((avg (/ (+ x y) 2.0))
         (pct (* 100 (/ avg (max x y)))))
    (list 'average avg 'is pct 'percent 'of 'max (max x y))))

;; ex 10.6
;; 0. ()
;; 1. (())
;; 2. ((())())
;; 3. (((())())(())())

;; ex 10.7
;; because (length x) computes down to a numerical value, which can hold no pointers to other objects (i.e. because its not a symbol!)

;; more debugger goodies
(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result
           (cond ((> commission 100) 'rich)
                 ((< commission 100) 'poor))))
    (break "Value of RESULT is ~S" result)
    (format t "~&I predict you will be: ~S"
            result)
    result))

;; yet more debugger goodies!
(defun average (x y)
  (unless (and (numberp x) (numberp y))
    (error "Arguments must be numbers: ~S, ~S"
           x y))
  (/ (+ x y) 2.0))

;; ex 10.9
(defun chop (l)
  "Removes everything but the first element from a list."
  (cond ((null l) nil)
        (t (setf (cdr l) nil))))

;; ex 10.10
(defun ntack (l x)
  "Tacks a symbol onto a list (in a destructive manner, mind you)."
  (cond ((null l) (setf l (list x)))
        (t (setf (cdr (last l)) (list x)))))

;; ex 10.11
;; (a b c a b c ...)

;; ex 10.12
;; append h h returns a new list whose contents are (hi ho hi ho)
;; nconc modifies h such as its contents are (hi ho hi ho ...)
