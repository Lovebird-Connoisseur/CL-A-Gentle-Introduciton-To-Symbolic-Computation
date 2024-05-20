;; ex 13.1
(defun subdrop (sym elem prop)
  "Removes elem from s's prop property."
  (delete elem (get sym prop)))

;; ex 13.2
(defun forget-meeting (p1 p2)
  "Forgets a meeting between 2 people."
  (subdrop p1 p2 'has-met)
  (subdrop p2 p1 'has-met))

;; note: solution used the do function, which was my first idea, but for some reason I always chose the worst option... VVV

;; ex 13.3
(defun my-get (sym prop)
  "My version of the get function."
  (my-get-aux prop (symbol-plist sym)))

(defun my-get-aux (prop prop-list)
  (cond ((null prop-list) NIL)
        ((equal prop (first prop-list))
         (first (rest prop-list)))
        (t (my-get-aux prop (rest (rest prop-list))))))

;; ex 13.4
(defun hasprop (sym prop)
  "Returns T if the symbol sym has the property prop."
  (hasprop-aux prop (symbol-plist sym)))

(defun hasprop-aux (prop prop-list)
  (cond ((null prop-list) NIL)
        ((equal prop (first prop-list)) T)
        (T (hasprop-aux prop (rest (rest prop-list))))))

;; ex 13.5: Efficiency (both in time and memory)
;; lists have to jump from cons cell to cons cell to find the next element in the sequence, the problem with this is that cons cells aren't next to eachother in memory, these are often in different cache layers and also suffer from problems such as cache misses
;; in constract, arrays/vectors are stored sequentially in memory, this makes the much more efficient when it comes to accessing a random element, this different is much more noticeable with may elements, especially if the one we want to find is stored in the end a data structure (on a array it simply has to get the starting point and compute the offset to obtain the elements index and access it, meanwhile the list had to jump every cons cell until it reaches the end)

;; additionally array usually use half the memory size of a list with the same number of elements, since list cons cells have to store 2 symbols (which point to 2 separate addresses) instead of a single symbol per array element.

;; ex 13.6: lists are much more flexible data structures, they are the simplest immutable data structure and can be easly resized without problem (if an array is allocated and needs to be resized, it popentially needs to allocate a bigger array on a completely different region of memory, since there may be other stuff stored in front of the existing array)

;; ex 13.7
;; prop list: (property value-of-property)
;; assoc list: ((property . value-of-property))
;; answer: assoc lists need at least a (sub)list per property, unlike property lists which don't need any

;; ex 13.8

(defun new-histogram (size)
  "Creates a vector of size elements."
  (setf *HISTOGRAM* (make-array size :initial-element 0))
  (setf *TOTAL* 0))

(defun record-value (value)
  "Increments the nth value of the histogram."
  (incf (aref *HISTOGRAM* value))
  (incf *TOTAL*))

(defun print-histogram ()
  "Prints the histogram."
  (dotimes (i (length *HISTOGRAM*) (format t "~&~7D total" *TOTAL*))
    (print-histogram-line i)))

(defun print-histogram-line (line)
  "Prints a line of the histogram."
  (format t "~&~2D [~3D] " line (aref *HISTOGRAM* line))
  (dotimes (i (aref *HISTOGRAM* line) (format t "~%"))
    (format t "*")))

;; 13.9 yet another ceaser cypher-like exercise...
;; too lazy to do one of those
