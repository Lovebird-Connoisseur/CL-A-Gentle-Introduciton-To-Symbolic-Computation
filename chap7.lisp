;; ex 7.1
(defun add1 (n)
  "Adds 1 to its input."
  (+ 1 n))

;; ex 7.2
(defvar daily-planet
  '((olsen jimmy 123-76-4535 cub-reporter)
    (kent clark 089-52-6787 reporter)
    (lane lois 951-26-1438 reporter)
    (white perry 355-16-7439 editor)))

;; ans: (mapcar #'third daily-planet)

;; 7.3: (mapcar #'zerop '(2 0 3 4 0 -5 -6))

;; ex 7.4
(defun greater-than-five-p (n)
  "Returns T if a number is greater than 5, NIL otherwise."
  (> n 5))

;; (mapcar #'greater-than-five-p '(2 0 3 4 0 -5 -6))

;; ex 7.5: (lambda (n) (- n 7))

;; ex 7.6: (lambda (x) (or (eq x T) (eq x NIL)))

;; ex 7.7
(defun flip (l)
  "Swaps UP for DOWN and vice-versa."
  (mapcar #'(lambda (token) (if (eq 'up token) 'down 'up)) l))

(defun my-assoc (key table)
  (find-if #'(lambda (entry) (equal (car entry) key)) table))

;; ex 7.9
;; note: I didn't innitially understand what the exercise wanted

(defun find-nested (l)
  (find-if #'(lambda (sublist) (not (null sublist))) l))

;; ex 7.11
(defun between-1-and-5 (l)
  "Returns the numbers in a list between (but not including) 1 and 5."
  (remove-if #'(lambda (n) (or (< n 2) (> n 4))) l))

;; ex 7.12
(defun count-the (l)
  "Counts the number of ocurrences of the word \"the\""
  (length (remove-if-not #'(lambda (n) (eq n 'the)) l)))

;; ex 7.13
(defun get-length-2-sublists (l)
  "Returns a list containing all the sublists of length 2."
  (remove-if-not #'(lambda (sublist) (eql (length sublist) 2)) l))

;; ex 7.14
(defun my-intersection (set1 set2)
  "Returns the intersection of 2 sets."
  (remove-if-not #'(lambda (n) (member n set2)) set1))

;; ex 7.17: (reduce #'+ (mapcar #'length))

;; ex 7.18: because the base case for #'+ is 1 and 0 for #'*

;; ex 7.19
(defun all-odd (l)
  "Returns T if every element on a list is odd, NIL otherwise."
  (every #'oddp l))

;; ex 7.20
(defun all-even (l)
  "Returns T if every element on a list is not odd, NIL otherwise."
  (every #'evenp l))

;; ex 7.21
(defun not-all-odd (l)
  "Returns T if not every number on a list is odd."
  (not (every #'oddp l)))

;; ex 7.22
(defun not-none-odd (l)
  "Returns T if at least one element in the list is odd, NIL otherwise."
  (not (every #'evenp l)))

;; ex 7.23
;; not-all-odd -> not-every-odd
;; not-none-odd -> not-every-even or not-all-even

;; ex 7.26: (defun find-if (fn l) (car (remove-if-not fn l)))
;; ex 7.27: (defun every (fn l) (null (length (remove-if fn l))))
