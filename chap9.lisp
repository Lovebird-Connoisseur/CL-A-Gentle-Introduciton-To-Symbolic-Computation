;; ex 9.2
(defun draw-line (size)
  (cond ((zerop size) (format t "~%"))
        (t (format t "*")
           (draw-line (- size 1)))))

;; ex 9.3
(defun draw-box (width height)
  "Draws a box with the specified dimensions."
  (cond ((zerop height) nil)
        (t (draw-line width)
           (draw-box width (- height 1)))))

;; ex 9.4
(defun ninety-nine-bottles (n)
  (cond ((zerop n) (format t "Aww, no more beer :<~%~%"))
        (t (format t "~s bottles of beer on the wall,~%" n)
           (format t "~s bottles of beer!~%" n)
           (format t "Take one down,~%")
           (format t "Pass it around,~%")
           (ninety-nine-bottles (- n 1)))))

;; ex 9.5
(defun print-board (l)
  "Prints a tic tac toe board filled with all elements of the given arg."
  (print-board-rec 1 l))

(defun print-board-rec (n l)
  (cond ((eql n 9) (format t " ~a ~%" (if (null (car l)) " " (car l))))
        ((zerop (mod n 3))
         (format t " ~a ~%" (if (null (car l)) " " (car l)))
         (format t "-----------~%")
         (print-board-rec (+ n 1) (cdr l)))
        (t (format t " ~a |" (if (null (car l)) " " (car l)))
           (print-board-rec (+ n 1) (cdr l)))))

;; ex 9.6
(defun gross-pay ()
  "Calculates a workers gross income given his hourly salary and his total work hours."
  (format t "~&Hourly wage (in dollars) and number of hours worked: ")
  (let ((wage (read))
        (hours (read)))
    (format t "~&Your gross income is: ~s~%" (* wage hours))))

;; ex 9.7
(defun cookie-monster ()
  "Reads from stdin until it reads the symbol cookie."
  (format t "~&Give me cookie!!!")
  (format t "~&Cookie? ")
  (let ((input (read)))
    (cond ((eq 'cookie input)
           (format t "Thank you!...Munch munch munch...BURP"))
          (t (format t "No want ~a...~%~%" input)
             (cookie-monster)))))

(defun my-first-file ()
  (with-open-file (stream "/home/johny/Projects/road_to_cl/hello.txt"
                          :direction :output)
    (format stream "I should be careful as to not overwrite any important files...~%")))

;; ex 9.8
;; 1. They are different data structures, symbolses names are actually strings
;; 2. Strings are case sensitive and evaluate to themselves, while symbols evaluate to their variables and are case insensitive

;; ex 9.9
;; 1. aB
;; 2. always
;;    broke
;; 3. alphabet

;; ex 9.10
(defun space-over (n)
  "Prints n spaces."
  (cond ((< n 0) (format t "Error!~%"))
        ((zerop n) (format t ""))
        (t (format t " ")
           (space-over (- n 1)))))

(defun plot-one-point (plotting-string y-val)
  "Writes plotting-string on the y-valth column."
  (space-over y-val)
  (format t "~a~%" plotting-string))

(defun plot-points (plotting-string columns)
  "Writes plotting-string on each column."
  (mapcar #'(lambda (n) (plot-one-point plotting-string n)) columns))

(defun generate (m n)
  "Returns the interval of numbers from m to n."
  (generate-aux m n '()))

(defun generate-aux (m n ans)
  (cond ((eql m n) (cons m ans))
        (t (generate-aux m (- n 1) (cons n ans)))))

(defun square (n)
  "Computes the square of n."
  (* n n))

(defun make-graph ()
  "Reads func, start, end and plotting-string from stdin and prints a graph."
  (format t "~&Func: ")
  (let ((func (read)))
    (format t "~&Start: ")
    (let ((start (read)))
      (format t "~&End: ")
      (let ((end (read)))
        (format t "~&Plotting-string: ")
        (let ((plotting-string (read)))
          (plot-points plotting-string (mapcar func
                                               (generate start end))))))))

;; ex 9.11 (has errors!)
(defun dot-print (l)
  "Prints the given list so stdout using dot notation."
  (format t "(")
  (cond ((null (cdr l)) (format t "~s . NIL" (car l)))
        ((not (atom (car l)))
              (dot-print (car l))
              (format t " . ")
              (dot-print (cdr l)))
        (t (format t "~s . " (car l))
           (dot-print (cdr l))))
  (format t ")"))

;; ex 9.13: (a . b)

;; ex 9.14
;; 1. (foo . (foo . (foo . ...
;; 2. ((((((((((((((((...

;; ex 9.15
(defun hybrid-print (l)
  "Prints a given list, using dot notation only when necessary."
  (format t "(")
  (hybrid-print-aux l))

(defun hybrid-print-aux (l)
  (cond ((null (cdr l)) (format t "~s)" (car l)))
        ((atom (cdr l)) (format t "~s . ~s)" (car l) (cdr l)))
        (t (format t "~s " (car l))
           (hybrid-print-aux (cdr l)))))
