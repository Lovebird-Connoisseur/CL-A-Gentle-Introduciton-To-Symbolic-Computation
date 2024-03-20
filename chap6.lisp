;; Chapter 6

;; ex 6.5

(setf line '(roses are red)) ; why the warning?

;; ex 6.6

(defun last-element (l)
  "Returns the last element of a list"
  (car (last l)))

(defun last-element* (l)
  "Returns the last element of a list"
  (car (reverse l))) ; correct way to do it?

(defun last-element** (l)
  "Returns the last element of a list"
  (nth (- (length l) 1) l))

;; ex 6.7

(defun next-to-last (l)
  "Returns the penultimate element of a list"
  (second (reverse l)))

(defun next-to-last* (l)
  "Returns the penultimate element of a list"
  (nth (- (length l) 2) l))

;; ex 6.8

(defun my-butlast (l)
  "Removes the last element of a list"
  (reverse (cdr (reverse l))))

;; ex 6.9

(defun mystery (x) (first (last (reverse x))))

;; returns the first element of a list, equivalent to the car function

;; ex 6.10

(defun palindromep (l)
  "Returns T if the list is a palindrome, NIL otherwise."
  (equal l (reverse l)))

;; ex 6.11

(defun make-palindrome (l)
  "Takes a list as its input and returns a palindrome version."
  (if (palindromep l) l (append l (reverse l))))

;; ex 6.12

;; no, because there is no need to do so, if member returns the sublist beggining with the item sought, then it simply needs to go from node to node until it finds (or not!) the item, and then simply return a pointer to that cons cell

;; ex 6.13
;; NIL

;; ex 6.14
;; the original set

;; ex 6.15

(defun contains-article-p (sent)
  "Verifies if a given set contains either the, a or and"
  (intersection '(the a an) sent))

(defun contains-article-p* (sent)
  "Verifies if a given set contains either the, a or and"
  (or (member 'the sent) (member 'a sent) (member 'an sent)))

;; the same function coult be written using ands and nots, since nand is a universal logical operator

;; ex 6.16
;; the original set

;; ex 6.17
;; the original set

;; ex 6.18

(defun add-vowels (l)
  (union '(a e i o u) l))

;; ex 6.19
;; as the first option: the result is always NIL
;; as the second option: the result is the first set

;; ex 6.20
;; the first one, since that's where elements will be (potentially) removed from

;; ex 6.21
(defun my-subsetp (l1 l2)
  "Returns T if l1 is a subset of l2, NIL otherwise."
  (equal (set-difference l1 l2) '()))

;; ex 6.22

;; (no soap radio water)
;; (soap water)
;; (soap)
;; ()
;; (soap water)
;; (water)
;; ()

;; ex 6.23
;; length

;; ex 6.24

(defun set-equal (l1 l2)
  "Returns T if both sets are equal (i.e. they contain the same elements), NIL otherwise."
  (and (my-subsetp l1 l2) (my-subsetp l2 l1)))

;; ex 6.25

(defun proper-subsetp (l1 l2)
  "Returns T if l1 is a subset (but not equal to) l2, NIL otherwise."
  (and (my-subsetp l1 l2) (not (set-equal l1 l2))))

;; ex 6.26

(defun right-side (l)
  (cdr (member '-vs- l)))

(defun left-side (l)
  (right-side (reverse l)))

(defun count-common (l)
  (intersection (left-side l) (right-side l)))

(defun compare (l)
  (cons (length (count-common l)) (count-common l)))

;; ex 6.27
;; yes, since it always returns a NIL or non-nil value depending on its args

;; ex 2.28

(setf produce
      '((apple . fruit)
        (celery . veggie)
        (banana . fruit)
        (lettuce . veggie)))

;; (banana . fruit)
;; (apple . fruit)
;; (lettuce . veggie)
;; (celery . veggies)

;; ex 6.29
;; (length)

;; ex 6.30
(setf books
      '((war-and-peace leo-tolstoy)
        (twilight-of-democracy anne-applebaum)
        (sicp sussman)
        (cl-a-gentle-introduction david-touretzky)
        (the-little-schemer daniel-friedman)))

;; ex 6.31
(defun who-wrote (book-name)
  (cdr (assoc book-name books)))

;; ex 6.32
;; nothing, since reverse only reverses the order the sublists appear, and not the order of the sublists themselves

;; 6.33
;; it would need to contain dotted sublists

;; ((pennsylvania (pittsburgh johnstown))
;;  (new-jersey (newark princeton ternton))
;;  (ohio (columbus)))

;; ex 6.35
;; a: a circular list

;; b
(setf nerd-states
      '((sleeping . eating)
        (eating . waiting-for-a-computer)
        (waiting-for-a-computer . programming)
        (programming . debugging)
        (debugging . sleeping)))

(defun nerdus (current-state)
  "Returns the next state of the creature"
  (cdr (assoc current-state nerd-states)))

;; c: NIL, apparently

;; d
(defun sleepless-nerd (current-state)
  "Returns the next state of the creature, skipping the sleeping state."
  (if (equal (nerdus current-state) 'sleeping) (nerdus (nerdus current-state)) (nerdus current-state)))

;; e
(defun nerd-on-caffeine (current-state)
  "Returns the next state of the creature, in this case the next state jumps 2 steps."
  (nerdus (nerdus current-state)))

;; f: 3

;; ex 6.36
(defun swap-first-last (l)
  "Swaps the first and last elements of a list."
  (cons (car (last l))
        (append (reverse (cdr (reverse (cdr l)))) (list (car l)))))

;; ex 3.37
(defun rotate-left (l)
  "Rotates the elements of the list."
  (append (cdr l)
          (list (car l))))

(defun rotate-right (l)
  "Rotates the elements of the list"
  (append (last l)
          (reverse (cdr (reverse l)))))

;; ex 6.38?

;; ex 6.42
(defun royal-we (l)
  (subst 'we 'i l))
