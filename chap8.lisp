;; ex 8.2
(defun anyoddp* (l)
  "Returns T if any element of the list is odd, NIL otherwise."
  (if (null l) nil
      (if (oddp (car l)) t
          (anyoddp* (rest l)))))

(defun factorial (n)
  "Computes the factorial of n."
  (if (eql n 0) 1 (* n (factorial (- n 1)))))

(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))

;; ex 8.3: because one of them is a float and the other a integer, 0.0 and 0 are too small to be affected by (im)precision, and also both satisfy the first condition on the first call

;; ex 8.4
(defun laugh (n)
  (cond ((> 1 n) '())
        (t (cons 'ha (laugh (- n 1))))))

;; ex 8.5
(defun add-up (l)
  "Adds up all element in a list."
  (cond ((null l) 0)
        (t (+ (car l) (add-up (rest l))))))

;; if version
(defun add-up* (l)
  "Adds up all elements in a list."
  (if (null l) 0 (+ (car l) (add-up* (rest l)))))

;; ex 8.6
(defun alloddp (l)
  "Returns T if all values on a list are odd, NIL otherwise."
  (cond ((null l) t)
        ((not (oddp (car l))) nil)
        (t (alloddp (cdr l)))))

;; if version
(defun alloddp* (l)
  "Returns T if all values on a list are odd, NIL otherwise."
  (if (null l) t (and (oddp (car l)) (alloddp* (cdr l)))))

;; ex 8.7
(defun rec-member (x l)
  "Returns the sublist whose first element is x, NIL otherwise."
  (cond ((null l) nil)
        ((equal x (car l)) l)
        (t (rec-member x (cdr l)))))

;; ex 8.8
(defun rec-assoc (key table)
  "Returns the first sublist whose first element corresponds to key."
  (cond ((null table) nil)
        ((equal (caar table) key) (car table))
        (t (rec-assoc key (cdr table)))))

;; ex 8.9
(defun rec-nth (n l)
  "Returns the nth element of a list."
  (cond ((null l) nil)
        ((eql n 0) (car l))
        (t (rec-nth (- n 1) (cdr l)))))

;; ex 8.10
(defun sub1 (n)
  "Subtracts 1 from its input."
  (- n 1))

(defun rec-plus (x y)
  "Adds x and y."
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

;; ex 8.11
(defun fib (n)
  "Returns the fibonacci of n."
  (cond ((zerop n) 1)
        ((eql n 1) 1)
        (t (combine (fib (- n 1))
              (fib (- n 2))))))

;; ex 8.12
;; works right: any list with the number 7
;; recurs infinitely: any list without the number 7

;; ex 8.13: a negative number

;; ex 8.14
(defun infinite ()
  (infinite))

;; ex 8.15
;; car: x
;; cdr: the same list/cons cell
;; recur infinitely

;; ex 8.16: it would return an error when the list was empty, since it would pass nil onto oddp, which only accepts numbers

;; ex 8.17
(defun find-first-odd (l)
  "Returns the first odd number in the list."
  (cond ((null l) nil)
        ((oddp (car l)) (car l))
        (t (find-first-odd (cdr l)))))

;; ex 8.18
(defun last-element (l)
  "Returns the last element of a list."
  (if (null (cdr l)) (car l) (last-element (cdr l))))

;; ex 8.19
;; where it would still work: when the list add atleast a odd number
;; where it would not: in all other cases
;; what would fail: it would recr infinitely (x)

;; ex 8.20 fact: double-test and augmentative

;; ex 8.21
(defun add-nums (n)
  "Adds all numbers from 0 to n."
  (if (zerop n) 0 (+ n (add-nums (- n 1)))))

;; ex 8.22
(defun all-equal (l)
  "Returns T if all elements of the list are equal, NIL otherwise."
  (cond ((null l) t)
        ((atom (cdr l)) t)
        ((atom (cddr l)) (equal (car l) (cadr l)))
        (t (and (equal (car l) (cadr l)) (all-equal (cddr l))))))

;; augmentative (and) and with 3 tests

;; note: my factorial and all-equal functions have 1 useless test case :<

;; ex 8.24
(defun count-down (n)
  "Returns a list of the countdown, starting from n."
  (if (eql 0 n) nil (cons n (count-down (- n 1)))))

;; ex 8.25
; (reduce #'* (count-down n))

;; ex 8.26
(defun count-down* (n)
  "Returns a list of the countdown, starting from n."
  (if (eql 0 n) (list 0) (cons n (count-down* (- n 1)))))

;; second way, base case returns (cons 0 nil)

;; ex 8.27
(defun square-list (l)
  "Computes the square of each element in the list."
  (if (null l) nil (cons (* (car l) (car l))
                         (square-list (cdr l)))))

;; ex 8.28 solved previously (on accident :p)

;; ex 8.29 solved privously too?

;; likewise?

;; ex 8.31
(defun compare-lengths (l1 l2)
  "Compares the lengths of 2 lists."
  (cond ((and (null l1) (null l2)) 'same-length)
        ((null l2) 'first-is-longer)
        ((null l1) 'second-is-longer)
        (t (compare-lengths (cdr l1) (cdr l2)))))

;; ex 8.32
(defun sum-numeric-elements (l)
  "Sums all numbers in a list, ignoring all non-numbers."
  (cond ((null l) 0)
        ((numberp (car l)) (+ (car l)
                              (sum-numeric-elements (cdr l))))
        (t (sum-numeric-elements (cdr l)))))

;; ex 8.33
(defun my-remove (x l)
  "Removes all instances of a given element from the list."
  (cond ((null l) nil)
        ((equal x (car l))
         (my-remove x (cdr l)))
        (t (cons (car l)
                 (my-remove x (cdr l))))))

;; ex 8.34
(defun my-intersection (set1 set2)
  "Returns the intersection of 2 sets."
  (cond ((or (null set1) (null set2)) nil)
        ((member (car set1) set2)
         (cons (car set1)
               (my-intersection (cdr set1) set2)))
        (t (my-intersection (cdr set1) set2))))

;; ex 8.35
(defun my-set-difference (set1 set2)
  "Returns the difference between 2 sets."
  (cond ((null set2) set1)
        ((null set1) nil)
        ((member (car set1) set2) (my-set-difference (cdr set1) set2))
        (t (cons (car set1)
                 (my-set-difference (cdr set1) set2)))))

;; ex 8.36
(defun count-odd (l)
  "Returns the number of odd numbers on a list."
  (cond ((null l) 0)
        ((oddp (car l))
         (+ 1 (count-odd (cdr l))))
        (t (count-odd (cdr l)))))

(defun count-odd* (l)
  "Returns the number of odd numbers on a list."
  (cond ((null l) 0)
        ((not (oddp (car l))) (count-odd* (cdr l)))
        (t (+ 1 (count-odd* (cdr l))))))

;; ex 8.37
(defun combine (x y)
  "Returns the sum of both numbers."
  (+ x y))

;; ex 8.38: the lit would recur infinitely once its input was an empty list (wrong!)

;; ex 3.39
(defun count-atoms (x)
  "Counts the number of atoms in a list, including the list terminators."
  (cond ((atom x) 1)
        (t (+ (count-atoms (car x))
              (count-atoms (cdr x))))))

;; ex 8.40
(defun count-cons (x)
  "Counts the number of cons cells in a tree."
  (cond ((atom x) 0)
        (t (+ 1
              (count-cons (car x))
              (count-cons (cdr x))))))

;; ex 8.41
(defun sum-tree (x)
  "Sums all numbers in a tree."
  (cond ((numberp x) x)
        ((atom x) 0)
        (t (+ (sum-tree (car x))
              (sum-tree (cdr x))))))

;; ex 8.42
(defun my-subst (new old tree)
  "Replaces all instances of the second arg with the first one."
  (cond ((equal tree old) new)
        ((atom tree) tree)
        (t (cons (my-subst new old (car tree))
                 (my-subst new old (cdr tree))))))

;; ex 8.43
(defun flatten (x)
  "Flattens a tree."
  (cond ((atom x) (list x))
        (t (append (flatten (car x))
                   (flatten (cdr x))))))

;; ex 8.44
(defun tree-depth (x)
  "Returns the depth of a binary tree."
  (cond ((atom x) 0)
        (t (+ 1
              (max (tree-depth (car x))
                   (tree-depth (cdr x)))))))

;; ex 8.45
(defun paren-depth (x)
  "Returns the depth of the innermost sublist."
  (cond ((atom x) 0)
        (t (+ 1 (max (paren-depth (car x))
                     (- (paren-depth (cdr x)) 1))))))


;; ex 8.46
(defun count-up (n)
  "Counts up from 1 up to n."
  (cond ((zerop n) nil)
        (t (append (count-up (- n 1))
                   (list n)))))

;; ex 8.47
(defun make-loaf (n)
  "Makes a loaf of length n."
  (if (zerop n) nil (append (make-loaf (- n 1)) '(x))))

;; ex 8.48
(defun bury (item level)
  "Buries an item n levels deep."
  (cond ((zerop level) item)
        (t (list (bury item (- level 1))))))

;; ex 8.49
(defun pairings (l1 l2)
  "Takes 2 lists as input and pairs them together."
  (cond ((or (null l1) (null l2)) nil)
        (t (cons (list (car l1)
                       (car l2))
                 (pairings (cdr l1)
                           (cdr l2))))))

;; ex 8.50
(defun sublists (l)
  "Returns the successive sublists of a list."
  (cond ((null l) nil)
        (t (cons l
                 (sublists (cdr l))))))

;; ex 8.55: a recursive function calls itself (sometimes!), unlike a nonrecursive one.

;; ex 5.56
(defun every-other (l)
  "Returns all elements whose index is odd."
  (every-other-aux 0 l))

(defun every-other-aux (index l)
  (cond ((null l) nil)
        (t (if (evenp index) (cons (car l)
                                   (every-other-aux (+ 1 index)
                                                    (cdr l)))
               (every-other-aux (+ 1 index)
                                (cdr l))))))

;; ex 5.57
(defun left-half (l)
  "Returns the left half of a given list."
  (cond ((oddp (length l)) (left-half-aux (/ (+ 1 (length l)) 2) l))
        (t (left-half-aux (/ (length l) 2) l))))

(defun left-half-aux (n l)
  "Returns the first n elements of a list."
  (cond ((zerop n) nil)
        (t (cons (car l)
                 (left-half-aux (- n 1)
                                (cdr l))))))

;; ex 5.58
(defun merge-lists (l1 l2)
  "Merges lists of 2 numbers."
  (cond ((and (null l1) (null l2)) nil) ; redundant!
        ((null l1) l2)
        ((null l2) l1)
        ((< (car l1) (car l2))
         (cons (car l1)
               (merge-lists (cdr l1) l2)))
        (t (cons (car l2)
                 (merge-lists l1 (cdr l2))))))

;; ex 8.59
(defun factorial* (n)
  "(doesn't!) Computes the factorial of n."
  (cond ((zerop n) 1)
        (t (/ (factorial (+ n 1))
              (+ n 1)))))

;; debugger test func
(defun fact-debug (n)
  (cond ((zerop n) (break "N is zero."))
        (t (* n (fact-debug (- n 1))))))

(setf family
  '((colin nil nil)
    (deirdre nil nil)
    (arthur nil nil)
    (kate nil nil)
    (frank nil nil)
    (linda nil nil)
    (suzanne colin deirdre)
    (bruce arthur kate)
    (charles arthur kate)
    (david arthur kate)
    (ellen arthur kate)
    (george frank linda)
    (hillary frank linda)
    (andre nil nil)
    (tamara bruce suzanne)
    (vincent bruce suzanne)
    (wanda nil nil)
    (ivan george ellen)
    (julie george ellen)
    (marie george ellen)
    (nigel andre hillary)
    (frederick nil tamara)
    (zelda vincent wanda)
    (joshua ivan wanda)
    (quentin nil nil)
    (robert quentin julie)
    (olivia nigel marie)
    (peter nigel marie)
    (erica nil nil)
    (yvette robert zelda)
    (diane peter erica)))
;; format: name father mother

;; ex 8.60
(defun father (name)
  "Returns the father of arg."
  (if (null name) nil (cadr (assoc name family))))

(defun mother (name)
  "Returns the mother of arg."
  (if (null name) nil (caddr (assoc name family))))

(defun parents (name)
  "Returns a list of args parents."
  (cdr (assoc name family)))

(defun children (name)
  "Returns the list of children of arg."
  (children-aux name family))

(defun children-aux (name l)
  (cond ((null name) nil)
        ((null l) nil)
        ((or (equal name (cadar l))
             (equal name (caddar l)))
         (cons (caar l) (children-aux name (cdr l))))
        (t (children-aux name (cdr l)))))

(defun siblings (name)
  "Returns a list of args siblings."
  (remove name (remove-duplicates (append (children (father name))
                                          (children (mother name))))))

(defun mapunion (fn l)
  "Applies a function to all items in a list."
  (cond ((null l) nil)
        (t (reduce #'union (mapcar fn l)))))

(defun grandparents (name)
  "Returns a list of all grandparents of arg."
  (mapunion #'parents (parents name)))

(defun cousins (name)
  "Returns a list of al 1st degree cousins of arg."
  (mapunion #'children (mapunion #'siblings (parents name))))

(defun descended-from (descendent name)
  "Returns T if the first person descends from the second, NIL otherwise."
  (cond ((member name (parents descendent)) t)
        ((and (null (car (parents descendent)))
              (null (cadr (parents descendent))))
         nil)
        (t (or (descended-from (father descendent) name)
               (descended-from (mother descendent) name)))))

(defun ancestors (name)
  "Returns a list of ones ancestors."
  (cond ((null name) nil)
        (t (remove-duplicates (append (remove nil (parents name))
                                      (ancestors (father name))
                                      (ancestors (mother name)))))))

(defun tr-count-slices (loaf)
  (tr-cs1 loaf 0))

(defun tr-cs1 (loaf count)
  (if (null loaf) count
                  (tr-cs1 (cdr loaf) (+ 1 count))))

;; ex 8.61
(defun tr-count-up (n)
  (tr-cu1 n 1 nil))

(defun tr-cu1 (n cur result)
  (cond ((< n cur) result)
        (t (tr-cu1 n (+ 1 cur) (cons cur result)))))

;; ex 8.62
(defun tr-fact (n)
  (tr-ft1 n 1 1))

(defun tr-ft1 (n cur result)
  (cond ((> cur n) result)
        (t (tr-ft1 n (+ 1 cur) (* cur result)))))
