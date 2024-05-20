;; ex 11.1
(defun it-member (x l)
  "Returns T if x is present in l, NIL otherwise."
  (dolist (elem l nil)
    (when (equal elem x) (return t))))

;; ex 11.2
(defun it-assoc (key table)
  "Returns the first (key value) entry if it exists on the table, NIL otherwise."
  (dolist (entry table nil)
    (when (equal key (car entry))
      (return entry))))

;; ex 11.3
(defun check-all-odd (list-of-numbers)
  (let ((x (car list-of-numbers)))
    (cond ((null x) t)
          ((not (oddp x)) (format t "~&Checking ~S..." x))
          (t (format t "~&Checking ~S..." x)
               (check-all-odd (cdr list-of-numbers))))))

;; it appears the solutions proposed to exercises 11.5 and 11.6 modify at least one of their elements, my version only return a new element, they don't modify any lists or sets.

;; ex 11.4
(defun it-length (l)
  "Iterative version of length."
  (let ((result 0))
    (dolist (elem l result)
      (incf result))))

;; ex 11.5
(defun it-nth (n l)
  "Iterative version of nth."
  (dolist (elem l NIL)
    (if (eql 0 n)
        (return elem)
        (decf n))))
;; note: here dotimes would be more elegant of a solution

;; ex 11.6
(defun it-union (set1 set2)
  "Iterative version of union."
  (let ((result set2))
    (dolist (element set1 result)
      (unless (member element set2)
        (push element result)))))

;; ch11.6 misc
(defun square-list (l)
  (cond ((null l) NIL)
        (T (cons (* (car l) (car l))
                 (square-list (cdr l))))))

(defun it-square-list (l)
  (let ((result '()))
    (dolist (element l (reverse result))
      (push (* element element)
            result))))

;; ex 11.7: because it uses the function to push, which pushes an item onto the stack, creating a list/stack/whatever using push assures the last item pushed is the first item on the list
;; the resulting list is inverted compared to the one supplied to the function, to correct this we can simply reverse the return value by changing result to (reverse result) inside the function dolist

;; ex 11.8
(defun it-reverse (l)
  "Iterative version of reverse."
  (let ((result '()))
    (dolist (element l result)
      (push element result))))

;; ex 11.9 (I couldn't do this one... gimme a break I made a week (or 2) long pause!)
;(defun check-all-odd (l)
;  "Checks if all elements in a list are odd."
;  (do ((lst l (cdr lst)))
;      ((or
;        (null lst)
;        (oddp (car lst)))
;       )
;    (format t "~&Checking ~S..." (car lst))))

;; ex 11.10
(defun launch (n)
  "Prints a countdown to stdout starting from n and ending in 1, at the end prints \"Blast off!\""
  (dotimes (i n (format t "Blast off!"))
    (format t "~S..." (- n i))))

(defun it-intersection (x y)
  (do ((x1 x (rest x1))
       (result '()))
      ((null x1) result)
    (when (member (car x1) y)
      (push (car x1) result))))

;; note: cons doesn't work above, the push function must be used instead (why?)

;; ex 11.11
(defun find-largest (list-of-numbers)
  "Returns the largest number in a list."
  (do* ((lst list-of-numbers (rest lst))
        (largest (first list-of-numbers))
        (cur (first lst) (first lst)))
       ((null lst) largest)
    (when (> cur largest)
      (setf largest cur))))

;; ex 11.12
(defun power-of-2 (n)
  "Returns 2 to the Nth power."
  (do ((i n (- i 1))
       (result 1 (* result 2)))
      ((zerop i) result)))

;; ex 11.13
(defun first-non-integer (l)
  "Returns the first non-integer of l."
  (dolist (element l nil)
    (when (not (integerp element))
      (return element))))

;; ex 11.14: e wouldn't initialize to anything

;; ex 11.15
(defun ffo-with-do (x)
  (do* ((z x (rest z))
        (e (first x) (first z)))
       ((null z) nil)
     (if (oddp e) (return e))))

;; problem: it doesn't verify the last element of the list
;; e depends on z, but the function uses the do macro
;; solution: switch to do*? Correct!

;; ex 11.16: I'm dum ;-;

;; ex 11.17: 4 (wrong, it was 5)

;; ex 11.18
(defun my-dotimes (n)
 (do ((i 0 (+ i 1)))
     ((eql n i) i)
   (format t "~&I = ~S" i)))

;; yes, it does :>

;; ex 11.19
;; no, because unlike the do* macro, variables that are part of the variable list don't depend on eachother (hopefully)

;; ex 11.20
;; yes, if by index variable you mean a single variable that is declared on the variable list

;; ex 11.21
(defun it-fib (n)
  "Computes the nth fibbonaci sequence element iteractively."
  (do* ((last 1 penultimate)
        (penultimate 1 cur)
        (cur 1 (+ last penultimate))
        (iter 1 (+ iter 1)))
       ((when (or (eql iter n)
                  (eql n 0))
          (return cur)))))

;; the solution for this exercise started with (fib 0), (fib 1) and (fib 2) as its variables, its a bit more concise and a funny solution, but I think I prefer mine

;; Bleh, biology on my book :P

;; 11.14
(defun average (&rest args)
  (/ (reduce #'+ args)
     (length args)))
