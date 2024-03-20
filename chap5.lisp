;; ex 5.6
(defun throw-die ()
  "Selects a random number between 1 and 5 (inclusive)"
  (+ 1 (random 6)))

(defun throw-dice ()
  "returns a list with 2 throws"
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  "returns T if both throws are 1, NIL otherwise"
  (and (equal 1 (first throw)) (equal 1 (second throw))))

(defun boxcars-p (throw)
  "returns T if both throws are 6, NIL otherwise"
  (and (equal 6 (first throw)) (equal 6 (second throw))))

(defun instant-win-p (throw)
  "returns T if the throw is 7 or 11, NIL otherwise"
  (cond ((equal throw 7) T)
        ((equal throw 11) T)
        (T NIL)))

(defun instant-loss-p (throw)
  "returns T if the throw is 2, 3 or 12, NIL otherwise"
  (cond ((equal throw 2) T)
        ((equal throw 3) T)
        ((equal throw 12) T)
        (T NIL)))

(defun say-throw (throw)
  "returns the sum of both throws, unless its a snake-eye or boxcar, in that case, returns those symbols"
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (T (+ (first throw) (second throw)))))

(defun throw-value (throw)
  "Sum of both throws"
  (+ (first throw) (second throw)))

(defun craps ()
  (let ((throw (throw-dice)))
     (append
        (list 'throw (first throw)
               'and (second throw)
               '--
               (say-throw throw)
               '--)
        (cond ((instant-win-p throw) '(you win))
              ((instant-loss-p throw) '(you lose))
              (t (list 'your 'point 'is (throw-value throw)))))))

(defun try-for-point (point)
  (let ((throw (throw-dice)))
    (append
     (list 'throw (first throw)
           'and (second throw)
           '--
           (say-throw throw)
           '--)
        (cond ((equal (throw-value throw) point) '(you win))
              ((equal (throw-value throw) 7) '(you lose))
              (t '(throw again))))))

