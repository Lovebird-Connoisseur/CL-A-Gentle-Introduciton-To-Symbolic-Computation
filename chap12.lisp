;; ex 12.1
; captain: name of the component
; :captain: keyword argument, used for selecting the captain component, either to set it or (just to set it up)
; starship-captain: function that takes in a struct of type starship and returns its captain component

;; ex 12.2: nope, 'STARSHIP is simply a symbol, not a s#starship

;; ex 12.3
; 1. symbol
; 2. function object
; 3. a starship object

;; ex 12.4
(defstruct node
  (name nil)
  (question nil)
  (yes-case nil)
  (no-case nil))

(defun init ()
    "Initializes the network."
  (defvar *NODE-LIST* nil))

(defun add-node (name question yes-case no-case)
  (push (make-node
         :name name
         :question question
         :yes-case yes-case
         :no-case no-case)
        *NODE-LIST*)
  name)

;; it gives me an error here, even if using the exact same function as the solution?
(defun find-node (name)
  (dolist (element *NODE-LIST* nil)
    (when (equal (node-name element) name)
      (return element))))

(defun find-node (name)
  (find-if #'(lambda (node)
               (equal (node-name node) name))
           *NODE-LIST*))

(defun process-node (name)
  (cond ((not (find-node name)) (format t "Node ~S hasn't yet been defined." name))
        (t (format t "~&~S" (node-question name))
           (let ((answer (read)))
             (if (equal answer 'yes)
                 (node-yes-case name)
                 (node-no-case name))))))

;; yea I can't figure out how to solve that weird error, not even the proposed solution works, weird...

(defstruct starship
  (name "Enterprise")
  (captain nil)
  (speed 0)
  (shields 'down))

(defun print-captain (x stream depth)
  (format stream "#<CAPTAIN ~A>"
          (captain-name x)))

(defstruct (captain
            (:print-function print-captain))
  (name nil)
  (age nil)
  (ship nil))
