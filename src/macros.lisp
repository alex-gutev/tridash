;;;; macros.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019  Alexander Gutev
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Implementation of user-defined macros.

(in-package :tridash.frontend)

(in-readtable lol-syntax)


(defvar *operand-vars* nil
  "The operands list of the meta-node currently being compiled to
   CL.")

(defvar *current-meta-node* nil
  "The meta-node currently being compiled to CL.")


;;;; Macro Attributes

(defmethod process-attribute (node (attribute (eql (id-symbol "MACRO"))) value (module t))
  "Sets the internal :MACRO-FUNCTION attribute to a function that
   compiles the META-NODE to a CL function, calls it and calls
   PROCESS-DECLARATION on the result."

  (when (bool-value value)
    (setf (node-macro-function node)
          (make-macro-function node))
    value))


;;;; Macro Function

(defun make-macro-function (meta-node)
  "Creates the macro-node function, which compiles META-NODE to a CL
   function, calls the function and processes the result."

  (lambda (operator operands module)
    (declare (ignore operator))
    (-> (call-tridash-meta-node meta-node operands)
        (process-declaration module :level *level*))))


;;;; Compiling CL function from Meta-Node

(defun call-tridash-meta-node (meta-node args)
  "Calls the meta-node META-NODE with arguments ARGS. If the meta-node
   has been compiled to a CL function, the function is called
   otherwise it is compiled to a CL function by
   COMPILE-META-NODE-FUNCTION."

  (stream-map
   (or (meta-node-cl-function meta-node)
       (compile-meta-node-function meta-node))
   args))

(defun compile-meta-node-function (meta-node)
  "Compiles the META-NODE meta-node to a CL function. Stores the
   compiled CL function in the :CL-FUNCTION attribute."

  (build-meta-node meta-node)
  (finish-build-meta-node meta-node)

  (setf (meta-node-cl-function meta-node)
        (compile nil (tridash->cl-function meta-node))))


(defun meta-node-cl-function (meta-node)
  "Returns the compiled CL function of the meta-node."

  (get :cl-function (attributes meta-node)))

(defun (setf meta-node-cl-function) (fn meta-node)
  "Sets the compiled CL function of META-NODE to FN."

  (setf (get :cl-function (attributes meta-node)) fn))


;;;; Compiling Tridash Expressions to CL

(defconstant +tridash-recur-tag+ 'recur
  "TAGBODY tag for tail-recursive self calls.")

(defconstant +fail-catch-tag+ 'evaluate-fail
  "The CATCH tag symbol for `CATCH-EXPRESSIONS' which are compiled to
   CL CATCH expressions.")

(define-condition tridash-fail (error)
  ()

  (:documentation
   "Condition representing a Tridash node failing to evaluate to a
    value."))


(defun tridash->cl-function (meta-node)
  "Returns a CL LAMBDA expression which is compiled from META-NODE."

  (let ((*current-meta-node* meta-node)
        (*operand-vars* (make-hash-map)))

    (flet ((add-operand (operand)
             (ensure-get operand *operand-vars* (gensym (mkstr operand)))))

      `(lambda ,(map #'add-operand (operands meta-node))
         (block nil
           (tagbody
              ,+tridash-recur-tag+
              (return
                ,(->
                  (contexts meta-node)
                  map-values
                  first
                  value-function
                  (tridash->cl :tail-position-p t)))))))))


(defvar *tridash-cl-functions*
  (make-hash-map)

  "Map mapping Tridash meta-node identifiers to the corresponding CL
   function identifiers.")


(defgeneric tridash->cl (expression &key &allow-other-keys)
  (:documentation
   "Compiles the Tridash expression EXPR to a CL expression. If the
    :TAIL-POSITION-P argument is provided and is true, then EXPRESSION
    appears in tail position."))

(defmethod tridash->cl ((link node-link) &key)
  "Returns the variable corresponding to the linked node by looking up
   the node's identifier in *OPERAND-VARS*"

  (with-slots (name) (node-link-node link)
    (ensure-get name *operand-vars* (gensym (mkstr name)))))


(defmethod tridash->cl ((functor functor-expression) &key)
  "If the operator of the functor is an `EXTERNAL-META-NODE',
   generates a CL expression which calls the corresponding CL
   function, found by looking up the name of the meta-node in
   *TRIDASH-CL-FUNCTIONS*. If there is no corresponding CL function an
   error condition is signalled.

   If the operator is a `META-NODE', generates a
   CALL-TRIDASH-META-NODE expression."

  (with-struct-slots functor-expression- (meta-node arguments)
      functor

    (match meta-node
      ((external-meta-node name)
       (aif (get name *tridash-cl-functions*)
            (list* it (map #'tridash->cl arguments))
            (error "External meta-node ~a not supported." name)))

      ((type meta-node)
       `(call-tridash-meta-node ,meta-node (list ,@(map #'tridash->cl arguments))))

      (_
       `(funcall ,(tridash->cl meta-node) ,@(map #'tridash->cl arguments))))))

(defmethod tridash->cl ((if if-expression) &key)
  (with-struct-slots if-expression- (condition then else)
      if

    `(,(id-symbol "if")
       ,(tridash->cl condition)
       ,(tridash->cl then)
       ,(tridash->cl else))))


(defmethod tridash->cl ((object object-expression) &key)
  "Generates a CL expression that creates a `HASH-MAP'."

  (with-struct-slots object-expression- (entries)
      object

    `(make-tridash-dict (list ,@(map #`(quote ,(first a1)) entries))
                        (list ,@(map (compose #'tridash->cl #'second) entries)))))

(defmethod tridash->cl ((member member-expression) &key)
  "Generates a GET CL function expression."

  (with-struct-slots member-expression- (object key)
      member

    `(tridash-dict-get ,(tridash->cl object) ',key)))


(defmethod tridash->cl ((expr catch-expression) &key)
  "Generates a CL CATCH expression with the tag symbol given by
   +FAIL-CATCH-TAG+. If the CATCH expression returns the catch tag
   identifier, the expression in the `CATCH' slot is evaluate."

  (with-struct-slots catch-expression- (main catch)
      expr

    `(,(id-symbol "catch") ,(tridash->cl main) ,(tridash->cl catch))))

(defmethod tridash->cl ((fail fail-expression) &key)
  "Generates an expression which creates a thunk that signals the
   `TRIDASH-FAIL' error condition.."

  '(thunk (error 'tridash-fail)))


(defmethod tridash->cl ((group expression-group) &key tail-position-p)
  (tridash->cl (expression-group-expression group) :tail-position-p tail-position-p))


(defmethod tridash->cl ((ref meta-node-ref) &key)
  (with-struct-slots meta-node-ref- (node outer-nodes)
      ref

    (when outer-nodes
      (error "Cannot reference outer-nodes in CL backend."))

    (match node
      ((external-meta-node name)
       (aif (get name *tridash-cl-functions*)
            `#',name
            (error "External meta-node ~a not supported." name)))

      ((type meta-node)
       (with-gensyms (args)
         `(lambda (&rest ,args)
            (call-tridash-meta-node ,node ,args)))))))

(defmethod tridash->cl (literal &key)
  (match literal
    ((or (type number) (type string))
     literal)

    (_ `',literal)))


;;;; Macro-Writing

(defmethod process-functor ((operator (eql +quote-operator+)) args module)
  "Returns the raw argument unprocessed."

  (declare (ignore module))

  (match-syntax (operator any) args
    ((list thing)
     thing)))


;;;; Tridash CL Runtime

;;; Thunk

(defstruct thunk
  "Stores a function which computes a value that is only called when
   the value is required.

   COMPUTE is the function which computes the thunk's value. If
   COMPUTE has been called, COMPUTED-P is true and the result is
   stored in RESULT."

  compute
  computed-p
  result)

(defun compute-thunk (thunk)
  "Computes the value of THUNK, if it hasn't already been computed,
   sets its COMPUTED-P slot to true and stores the result in RESULT."

  (with-struct-slots thunk- (compute computed-p result)
      thunk

    (if computed-p
        result
        (prog1 (setf result (funcall compute))
          (setf computed-p t)))))

(defmacro thunk (expression)
  "Creates a `THUNK' with a compute function that evaluates
   EXPRESSION."

  `(make-thunk :compute (lambda () ,expression)))

(defun resolve (thing)
  "If THING is a `THUNK' calls its COMPUTE function to compute its
   value, otherwise returns THING."

  (nlet-tail resolve ((x thing))
    (typecase x
      (thunk (resolve (compute-thunk x)))
      (otherwise x))))


;;; Streams

(defstruct tridash-stream
  "Base struct representing a stream of values.")

(defstruct (lazy-stream (:include tridash-stream))
  "A lazy stream of values which are computed only when required. HEAD
   is the first value in the stream and TAIL is the stream of the
   remaining values, which is NIL if there are no more values."

  head
  tail)

(defstruct (list-stream (:include tridash-stream))
  "Stream of the values in LIST."

  list)


(defmacro lazy-stream (head tail)
  "Creates a `LAZY-STREAM' with HEAD being the first element and TAIL
   being the stream of remaining values."

  `(make-lazy-stream :head ,head :tail (thunk ,tail)))

(defmacro ignore-fail (form)
  "Wraps form in a HANDLER-CASE that ignores `TRIDASH-FAIL' error
   conditions."

  `(handler-case ,form
     (tridash-fail ())))

(defgeneric stream-first (stream)
  (:documentation
   "Returns the first element in the stream.")

  (:method ((s lazy-stream))
    (lazy-stream-head s))

  (:method ((s list-stream))
    (car (list-stream-list s)))

  (:method (thing)
    thing))

(defgeneric stream-rest (stream)
  (:documentation
   "Returns a stream containing the rest of the elements in the
    stream.")

  (:method ((s lazy-stream))
    (resolve (lazy-stream-tail s)))

  (:method ((s list-stream))
    (awhen (cdr (list-stream-list s))
      (make-list-stream :list it)))

  (:method ((s null))
    nil)

  (:method (thing)
    thing))


(defun stream-map (fn streams)
  "Returns a new stream (`LAZY-STREAM') which contains the result of
   applying FN on each element of all streams in STREAMS.

   If an element of STREAMS is not a `TRIDASH-STREAM' FN is applied on
   it directly (along with the value from the other streams) as though
   it is an infinite stream containing just that value.

   If STREAMS does not contain a `TRIDASH-STREAM' a THUNK is returned
   which applies FN on all values in STREAMS."

  (unless (some #'null streams)
    (if (some #'tridash-stream-p streams)

        (lazy-stream
         (thunk (apply fn (map #'stream-first streams)))
         (stream-map fn (map #'stream-rest streams)))

        (thunk (apply fn streams)))))

(defun stream-foreach (fn streams)
  "Applies FN on each element of all streams in STREAMS. Unlike
   STREAM-MAP, all elements of `LAZY-STREAM's are evaluated fully."

  (nlet-tail foreach ((streams streams))
    (unless (some #'null streams)
      (ignore-fail
       (apply fn (map #'stream-first streams)))
      (foreach (map #'stream-rest streams)))))


(defmethod coerce ((stream lazy-stream) (type (eql 'list)))
  "Returns a list containing all values in the `LAZY-STREAM' stream."

  (let ((c (make-collector nil)))
    (stream-foreach
     (lambda (item)
       (accumulate c (resolve item)))
     (list stream))
    (collector-sequence c)))

(defmethod coerce ((stream list-stream) (type (eql 'list)))
  "Returns the underlying list of elements in the `LIST-STREAM'
   STREAM."

  (list-stream-list stream))


;;;; Builtin Functions

;;; Convenience Macros

(defmacro! define-tridash-function% (name (&rest args) &body body)
  "Defines a function with name, being the result of interning NAME in
   the TRIDASH.SYMBOLS package, lambda-list ARGS and body BODY. A
   mapping for the function is added to the *TRIDASH-CL-FUNCTIONS*
   map."

  (let ((name (id-symbol (string name))))
    `(progn
       (defun ,name ,args
         (macrolet ((self (&rest ,g!args) `(,',name ,@,g!args)))
          ,@body))

       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name *tridash-cl-functions*) ',name)))))

(defmacro! define-tridash-function (name (&rest args) &body body)
  "Defines an external Tridash function, and adds a mapping for it to
   the *TRIDASH-CL-FUNCTIONS* map.

   The body of the function applies a LAMBDA expression with
   lambada-list ARGS, and body BODY, to each value in the streams, by
   STREAM-MAP, passed as arguments to the function.

   If BODY is a symbol it is interpreted as the name of a function
   which is applied on each value in the streams. The values are
   evaluated, by RESOLVE before the function is applied on them."

  `(define-tridash-function% ,name (&rest ,g!args)
     (stream-map
      ,(match body
         ((list (guard fn (symbolp fn)))
          `(lambda (&rest ,g!args)
             (apply #',fn (map #'resolve ,g!args))))

         (_
          `(lambda ,args ,@body)))

      ,g!args)))


(define-tridash-function |if| (cond then else)
  (if (bool-value (resolve cond))
      then
      else))

(define-tridash-function |catch| (main catch)
  (handler-case (resolve main)
    (tridash-faill () catch)))


(define-tridash-function |and| (a b)
  (if (bool-value (resolve a)) b 0))

(define-tridash-function |or| (a b)
  (or (bool-value (resolve a)) b))

(define-tridash-function |not| (x) tnot)

(defun tnot (x) (if (bool-value x) 0 1))


(define-tridash-function + (a b) +)
(define-tridash-function - (a b) -)
(define-tridash-function * (a b) *)
(define-tridash-function / (a b) /)

(define-tridash-function < (a b) <)
(define-tridash-function <= (a b) <=)
(define-tridash-function > (a b) >)
(define-tridash-function >= (a b) >=)
(define-tridash-function = (a b) =)
(define-tridash-function != (a b) /=)


(define-tridash-function |int?| (x) integerp)
(define-tridash-function |real?| (x) floatp)
(define-tridash-function |string?| (x) stringp)

(define-tridash-function |cons| (x list) cons)
(define-tridash-function |list| (&rest xs) list)


(define-tridash-function% |each| (list)
  (make-list-stream :list (resolve list)))

(define-tridash-function% |delay| (stream)
  (lazy-stream nil stream))

(define-tridash-function% |advance| (stream)
  (stream-rest stream))

(define-tridash-function% |pack| (stream)
  (thunk (coerce stream 'list)))

(define-tridash-function% |skip-fail| (stream)
  (nlet-tail next ((stream stream))
    (when stream
      (handler-case
          (lazy-stream
           (resolve (stream-first stream))
           (self (stream-rest stream)))
        (tridash-fail () (next (stream-rest stream)))))))


(defun make-tridash-dict (keys values)
  "Creates a dictionary with keys KEYS and corresponding values
   VALUES. If one of VALUES is a stream returns a stream where each
   value is a dictionary."

  (stream-map
   (lambda (&rest values)
     (alist-hash-map (map #'cons keys (map #'resolve values))))
   values))

(defun tridash-dict-get (dict key)
  "Returns the value of the entry with key KEY in the dictionary
   DICT. If DICT is a stream returns a stream where each value is the
   value of the entry in the corresponding dictionary value in the
   stream."

  (stream-map
   (lambda (dict)
     (get key (resolve dict)))
   (list dict)))
