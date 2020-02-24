;;;; backend.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2020  Alexander Gutev
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

;;;; Wasm32 Backend Tests

(defpackage tridash.test.backend.wasm
  (:use :generic-cl
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :optima
        :prove
        :named-readtables

        :babel
        :babel-encodings

        :tridash.parser
        :tridash.frontend
        :tridash.interface

        :tridash.test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:shadowing-import-from :prove :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax)

  (:import-from :tridash
                :*module-search-paths*
                :build-program)

  (:import-from :tridash.backend.wasm
                :*backend-state*
                :backend-state
                :thunk-functions
                :data-section
                :meta-node-ref-functions

                :meta-node-id
                :compile-function
                :compile-meta-node

                :make-value-block

                :make-function-body
                :optimize-unboxing
                :optimize-boxing
                :fold-constant-locals

                :wasm-function-spec
                :wasm-function-spec-p
                :wasm-function-spec-params
                :wasm-function-spec-results
                :wasm-function-spec-export-name
                :wasm-function-spec-locals
                :wasm-function-spec-code
                :make-wasm-function-spec)

  (:import-from :tridash.test.builder
                :with-module-table
                :with-modules
                :with-nodes

                :build
                :build-core-module
                :finish-build))

(in-package :tridash.test.backend.wasm)

(in-readtable lol-syntax)

(plan nil)


;;; Object Type Constants

(defconstant +type-thunk+ 0)

(defconstant +type-i32+ 2)
(defconstant +type-f32+ 3)

(defconstant +type-string+ 4)

(defconstant +type-fail+ 5)

(defconstant +type-funcref+ 6)
(defconstant +type-funcref-args+ 7
  "Function reference with optional/outer node arguments.")

(defconstant +type-array+ 8)

(defconstant +type-symbol+ 9)
(defconstant +type-charecter+ 10)

(defconstant +tag-mask+ 3)
(defconstant +tag-int+ 3)
(defconstant +tag-fail+ 3)
(defconstant +word-alignment+ 4)

(defconstant +max-immediate-int+ (1- (expt 2 29)))
(defconstant +min-immediate-int+ (- (expt 2 29)))


;;; Instruction Equality Comparisons

(defvar *alias-map* nil
  "Hash map mapping symbolic aliases to the corresponding
   index/label/instruction.")

(defun instruction= (got expected)
  "Returns true if the instruction GOT is equal to the instruction
   EXPECTED."

  (match* (got expected)
    ((_ (list '$ alias))
     (= got (ensure-get alias *alias-map* got)))

    (((type symbol) (type symbol))
     (= (symbol-name got)
        (symbol-name expected)))

    (((type string) (type string))
     (= got expected))

    (((type list) (type list))
     (and (length= got expected)
          (every #'instruction= got expected)))

    ((_ _) (= got expected))))

(defmacro $ (alias)
  "Convenience macro expanding to `($ ,ALIAS). When this appears in an
   expected argument to INSTRUCTION=, it is interpreted as a label for
   an index or identifier. In the first occurrence the alias, with
   identifier ALIAS, for the GOT argument is created. On subsequent
   occurrences the GOT argument is compared to the value of the GOT
   argument, for which the identifier ALIAS is an alias."

  `($ ,alias))

(defmacro $$ (alias)
  "Retrieves the label/instruction with alias ALIAS (not evaluated)."

  `(get ',alias *alias-map*))

(defun get-alias (alias)
  "Retrieves the label/instruction with alias ALIAS."

  (get alias *alias-map*))


;;; Test Mock Utilities

(defmacro mock-backend-state (&body body)
  "Evaluates the forms in BODY in an environment with a mocked backend
   state."

  `(let ((*backend-state* (make-instance 'backend-state)))
     ,@body))

(defun mock-meta-nodes% (names)
  "Returns a list of mock meta-nodes with names NAMES."

  (labels
      ((split-args (args)
         (aif (position :outer args)
              (values (subseq args 0 it) (subseq args (1+ it)))
              (values args nil)))

       (make-operand (operand)
         (match operand
           ((or (list 'optional symb value)
                (list 'optional symb))
            (list +optional-argument+ (make-instance 'node :name symb) value))

           ((list 'rest symb)
            (list +rest-argument+ (make-instance 'node :name symb)))

           (_ (make-instance 'node :name operand))))

       (make-outer-operand (operand)
         (cons operand (make-instance 'node :name operand)))

       (make-meta-node (opts)
         (match opts
           ((list name operands strict-operands)


            (multiple-value-bind (operands outer-nodes)
                (split-args operands)

              (aprog1
                  (make-instance 'final-meta-node
                                 :name name
                                 :operands (map #'make-operand operands))

                (setf (outer-nodes it)
                      (map-to 'hash-map #'make-outer-operand outer-nodes))

                (setf (attribute :strictness it)
                      (list* 'or strict-operands)))))
           (name
            (make-instance 'final-meta-node :name name)))))
    (map #'make-meta-node names)))

(defmacro mock-meta-nodes ((&rest names) &body body)
  "Evaluates the forms in BODY with each symbol in NAMES bound to an
   mock meta-node created by MOCK-META-NODES%."

  `(destructuring-bind ,(map #'ensure-car names) (mock-meta-nodes% ',names)
     ,@body))

(defmacro! mock-context ((&rest operands) value-function)
  "Creates a context (the return value of the form) with operands
   OPERANDS and value VALUE-FUNCTION.

   VALUE-FUNCTION is evaluated in an environment in which each symbol
   in OPERANDS is bound to the `NODE-LINK' object of the
   dependency. If an element of OPERANDS is a CONS with the CAR being
   the symbol ASYNC an asynchronous `NODE-LINK' is created and bound
   to the symbol in the CDR."

  (flet ((make-binding (operand)
           (match operand
             ((or (cons 'async dep)
                  dep)

              `(,dep (setf (get ',dep (operands ,g!context))
                               (node-link ',operand)))))))

    `(let ((,g!context (make-instance 'node-context)))
       (let ,(map #'make-binding operands)
         (setf (value-function ,g!context) ,value-function))
       ,g!context)))

(defmacro! mock-contexts ((&rest contexts) &body body)
  "Evaluates the forms in BODY with a number of mocked
   `NODE-CONTEXT's. Each element of CONTEXTS is of the form (SYMBOL
   . ARGS) where SYMBOL is the variable to which the context is bound
   and ARGS are the arguments passed to MOCK-CONTEXT."

  (flet ((make-binding (context)
           `(,(first context) (mock-context ,@(rest context)))))
    `(let ,(map #'make-binding contexts) ,@body)))


;;; Test Function Utilities

(defmacro with-alias-map (&body body)
  "Creates an environment with a newly initialized empty alias
   map. Aliases created within BODY are added to the map and only
   visible to the forms in BODY."

  `(let ((*alias-map* (make-hash-map)))
     ,@body))

(defun test-compute-function% (got expected)
  "Test that the generated function (WASM-FUNCTION-SPEC) GOT is
   equivalent to the function EXPECTED."

  (flet ((intern-syms (list)
           (map (compose (rcurry #'intern :tridash.backend.wasm) #'symbol-name) list)))

    (is (wasm-function-spec-params got)
        (intern-syms (wasm-function-spec-params expected))

        "Parameter Types Match")

    (is (wasm-function-spec-results got)
        (intern-syms (wasm-function-spec-results expected))

        "Result Types Match")

    (is (wasm-function-spec-locals got)
        (intern-syms (wasm-function-spec-locals expected))

        "Local Variable Types Match")

    (is (wasm-function-spec-code got)
        (wasm-function-spec-code expected)
        :test #'instruction=

        "Instructions Match")))

(defun test-context-function (context function)
  "Test that the WASM-FUNCTION-SPEC generated for the function of
   CONTEXT is equivalent to FUNCTION.

   The following, when present in the instructions of FUNCTION, has a
   special meaning:

   A CONS with the car being the symbol $ creates an alias, with the
   identifier in the CDR, for the corresponding instruction/index of
   the generated function. The first occurrence of ($ A) always
   compares equal to the corresponding instruction/index of the
   generated function and establishes an alias, with
   identifier. Subsequent occurrences are replaced with the aliased
   instructions/index."

  (with-slots (value-function operands) context
    (subtest "Test Context Function"
      (test-compute-function%
       (compile-function value-function (map-keys operands))
       function))))

(defun test-thunk-function (thunk function)
  "Tests that the function generated for the thunk with index THUNK is
   equivalent to FUNCTION"

  (subtest (format nil "Test thunk ~a" thunk)
    (let ((fn (get thunk (thunk-functions *backend-state*))))
      (ok! fn "Thunk Function ~a exits" thunk)
      (test-compute-function% fn function))))

(defun test-meta-node-ref-function (meta-node function)
  "Test that the generated meta-node reference function for META-NODE
   is equivalent to FUNCTION."

  (subtest (format nil "Test meta-node reference function for ~a" meta-node)
    (let ((ref-function (get meta-node (meta-node-ref-functions *backend-state*))))
      (ok! ref-function "Meta-Node Reference function created for ~a" meta-node)
      (test-compute-function% ref-function function))))

(defun test-meta-node-function (meta-node function)
  "Test that the function generated for META-NODE is equivalent to
   FUNCTION."

  (test-compute-function%
   (compile-meta-node meta-node)
   function))


;;; Data Section Tests

(defun test-data-bytes (offset byte-array &key (description "Data Section Bytes Equal"))
  "Test that the constant data section contains the bytes in
   BYTE-ARRAY starting at OFFSET."

  (is (mod offset +word-alignment+) 0
      "Data offset aligned to word boundary")

  (is (subseq (data-section *backend-state*) offset (+ offset (length byte-array)))
      byte-array

      :test #'equalp
      description))

(defun byte-encode (value)
  "Encode an integer VALUE as a sequence of bytes."

  (check-type value integer)

  (vector
   (ldb (byte 8 0) value)
   (ldb (byte 8 8) value)
   (ldb (byte 8 16) value)
   (ldb (byte 8 24) value)))

(defun test-data-string (offset string &key (type +type-string+))
  "Test that the constant data section contains the string STRING in
   UTF-8 encoding starting at OFFSET."

  (let ((octet-string
         (string-to-octets
          string
          :encoding :utf-8
          :use-bom nil)))

   (test-data-bytes
    offset

    (concatenate
     (byte-encode type)
     (byte-encode (length octet-string))
     octet-string))))


;;; Intermediate Expression Construction Utilities

(defun functor (meta-node &rest arguments)
  "Create a `FUNCTOR-EXPRESSION' with META-NODE applied to ARGUMENTS."

  (functor-expression meta-node arguments))

(defmacro object (&rest entries)
  "Create an `OBJECT-EXPRESSION' with entries ENTRIES."

  `(object-expression (list ,@(map #`(list ',(id-symbol (first a1)) ,(second a1)) entries))))


;;; Internal Unit Test Utilities

(defun test-optimize-unboxing (locals instructions expected)
  "Test that performing unboxing optimizations on INSTRUCTIONS results
   in the instructions EXPECTED. LOCALS is the list of local variable
   labels/indices. An element of locals may also be of the form (LABEL
   IMMEDIATE STRICT) where IMMEDIATE and STRICT indicate whether the
   local variable holds an immediate value and a resolved value,
   respectively."

  (labels ((make-local (label)
             (destructuring-bind (label &optional immediate-p strict-p)
                 (ensure-list label)

               (cons
                label

                (make-value-block :label label
                                  :immediate-p immediate-p
                                  :strict-p strict-p)))))

    (test-optimization
     (rcurry #'optimize-unboxing
             (map-to 'hash-map #'make-local locals))

     instructions
     expected)))

(defun test-optimize-boxing (instructions expected)
  "Test that performing boxing optimizations on INSTRUCTIONS results
   in the instructions EXPECTED."

  (test-optimization #'optimize-boxing instructions expected))

(defun test-optimization (fn instructions expected)
  "Test that applying the optimization function FN on instructions
   results in the instructions EXPECTED.

   Prior to applying FN, each symbol in INSTRUCTIONS is replaced with
   the symbol, with the same name, in the TRIDASH.BACKEND.WASM
   package."

  (labels ((get-symbol (thing)
             (match thing
               ((type symbol)
                (intern (symbol-name thing) :tridash.backend.wasm))

               ((type list)
                (map #'get-symbol thing))

               (_ thing))))

    (is (funcall fn (get-symbol instructions))
        expected :test #'instruction=)))


;;; Tests

(subtest "Node Context Function Code Generation"
  (subtest "Literals"
    (subtest "Strings"
      (mock-backend-state
        (mock-contexts
            ((hello () "hello")
             (world () "world"))

          (with-alias-map
            (test-context-function
             hello

             (make-wasm-function-spec
              :results '(i32)
              :code '((i32.const (data 0)))))

            (test-context-function
             world

             (make-wasm-function-spec
              :results '(i32)
              :code '((i32.const (data ($ world))))))

            (test-data-string 0 "hello")
            (test-data-string ($$ world) "world")))))

    (subtest "Symbols"
      (mock-backend-state
        (mock-contexts
            ((some-key () '|some-key|)
             (field-a () '|field-a|))

          (with-alias-map
            (test-context-function
             some-key

             (make-wasm-function-spec
              :results '(i32)
              :code '((i32.const (data 0)))))

            (test-context-function
             field-a

             (make-wasm-function-spec
              :results '(i32)
              :code '((i32.const (data ($ field-a))))))

            (test-data-string 0 "some-key" :type +type-symbol+)
            (test-data-string ($$ field-a) "field-a" :type +type-symbol+)))))

    (subtest "Integers"
      (subtest "Small - Tagged Pointer"
        (subtest "Positive"
          (mock-backend-state
            (mock-contexts
                ((context () 17))

              (with-alias-map
                (test-context-function
                 context
                 (make-wasm-function-spec
                  :results '(i32)
                  :code `((i32.const ,(+ (* 17 4) 1)))))))))

        (subtest "Negative"
          (mock-backend-state
            (mock-contexts
                ((context () -103))

              (with-alias-map
                (test-context-function
                 context

                 (make-wasm-function-spec
                  :results '(i32)
                  :code `((i32.const ,(+ (* -103 4) 1))))))))))

      (subtest "Large - Heap Object"
        (subtest "Positive"
          (mock-backend-state
            (mock-contexts
                ((context () (+ (expt 2 32) 22)))

              (with-alias-map
                (test-context-function
                 context

                 (make-wasm-function-spec
                  :results '(i32)
                  :locals '(i32)
                  :code
                  `((i32.const 8)
                    (call (import "runtime" "alloc"))
                    (local.tee 0)

                    (i32.const ,+type-i32+)
                    i32.store

                    (local.get 0)
                    (i32.const ,(+ (expt 2 32) 22))
                    (i32.store (offset 4))

                    (local.get 0))))))))

        (subtest "Negative"
          (mock-backend-state
            (mock-contexts
                ((context () (- (- (expt 2 33)) 100)))

              (with-alias-map
                (test-context-function
                 context

                 (make-wasm-function-spec
                  :results '(i32)
                  :locals '(i32)
                  :code
                  `((i32.const 8)
                    (call (import "runtime" "alloc"))
                    (local.tee 0)

                    (i32.const ,+type-i32+)
                    i32.store

                    (local.get 0)
                    (i32.const ,(- (- (expt 2 33)) 100))
                    (i32.store (offset 4))

                    (local.get 0))))))))))

    (subtest "Reals"
      (mock-backend-state
        (mock-contexts
            ((context () 36.125))

          (with-alias-map
            (test-context-function
             context

             (make-wasm-function-spec
              :results '(i32)
              :locals '(i32 i32)

              :code
              `((f32.const 36.125)
                i32.reinterpret_f32
                (local.set ($ value))

                (i32.const 8)
                (call (import "runtime" "alloc"))
                (local.tee ($ ptr))

                (i32.const ,+type-f32+)
                i32.store

                (local.get ($ ptr))
                (local.get ($ value))
                (i32.store (offset 4))

                (local.get ($ ptr)))))))))

    (subtest "Characters"
      (mock-backend-state
        (mock-contexts
            ((context () #\h))

          (with-alias-map
            (test-context-function
             context

             (make-wasm-function-spec
              :results '(i32)
              :locals '(i32)

              :code
              `((i32.const 8)
                (call (import "runtime" "alloc"))
                (local.tee ($ ptr))

                (i32.const ,+type-charecter+)
                i32.store

                (local.get ($ ptr))
                (i32.const ,(char-code #\h))
                (i32.store (offset 4))

                (local.get ($ ptr))))))))))

  (subtest "Function Calls"
    (subtest "Positional Arguments"
      (subtest "Constants"
        (mock-backend-state
          (mock-meta-nodes ((f (a b) (a b)))
            (mock-contexts
                ((context () (functor f 7 23)))

              (with-alias-map
                (test-context-function
                 context

                 (make-wasm-function-spec
                  :results '(i32)
                  :locals '(i32)

                  :code
                  `((i32.const ,(+ (* 7 4) 1))
                    (i32.const ,(+ (* 23 4) 1))
                    (call (meta-node ,f))
                    (local.set ($ result))
                    (local.get ($ result))))))))))

      (subtest "Context Operands"
        (mock-backend-state
          (mock-meta-nodes ((f (a b) (a b)))
            (mock-contexts
                ((context (a b) (functor f a b)))

              (with-alias-map
                (test-context-function
                 context

                 (make-wasm-function-spec
                  :params '(i32 i32)
                  :results '(i32)
                  :locals '(i32)

                  :code
                  `((local.get ($ a))
                    (local.get ($ b))
                    (call (meta-node ,f))
                    (local.set ($ result))
                    (local.get ($ result)))))))))))

    (subtest "Optional Argument with No Default"
      (mock-backend-state
        (mock-meta-nodes ((f (a (optional b)) (a b)))
          (mock-contexts
              ((context (a) (functor f a nil)))

            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32)

                :code

                 ;; Reserve Stack Space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 4)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out stack elements
                  (local.get ($ stack))
                  (i32.const 0)
                  (i32.store (offset 0))

                  ;; Save parameter on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Get No Value Failure
                  (call (import "runtime" "fail_no_value"))
                  (local.set ($ b))

;;; Call Meta-Node f

                  ;; Refresh parameter 0 from stack
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)

                  (local.get ($ b))
                  (call (meta-node ,f))

                  ;; Return Value
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore global stack pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 4)
                  i32.add
                  i32.store))))))))

    (subtest "Rest Arguments"
      (mock-backend-state
        (mock-meta-nodes ((f (a (rest b)) (a b)))
          (mock-contexts
              ((context (a) (functor f #\a (argument-list (list a 1)))))

            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32 i32)

                :code

                 ;; Reserve Stack Space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 8)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero Out Stack
                  (local.get ($ stack))
                  (i64.const 0)
                  (i64.store (offset 0))

                  ;; Save parameter `a`
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Create Character
                  (i32.const 8)
                  (call (import "runtime" "alloc"))
                  (local.set ($ char))
                  (local.get ($ stack))
                  (local.get ($ char))
                  (i32.store (offset 4)) ; Save Reference on stack
                  (local.get ($ char))
                  (i32.const 10)         ; Set Object Type = Character
                  i32.store
                  (local.get ($ char))
                  (i32.const 97)         ; Set Character Code
                  (i32.store (offset 4))

                  ;; Allocate space for array
                  (i32.const 16)
                  (call (import "runtime" "alloc"))
                  (local.tee ($ array))
                  (i32.const 8)          ; Set Object Type = Array
                  i32.store

                  ;; Set Size = 2
                  (local.get ($ array))
                  (i32.const 2)
                  (i32.store (offset 4))

                  ;; Store first rest argument (parameter `a`)
                  (local.get ($ array))
                  (local.get ($ stack))
                  (i32.load (offset 0))  ; Refresh reference to `a`
                  (local.tee 0)
                  (i32.store (offset 8))

                  ;; Store second rest argument (immediate integer 1)
                  (local.get ($ array))
                  (i32.const ,(+ (* 4 1) 1)) ; Encode 1 in pointer
                  (i32.store (offset 12))

                  ;; Call meta-node f
                  (local.get ($ stack))
                  (i32.load (offset 4))
                  (local.tee ($ char)) ; Refresh reference to boxed character
                  (local.get ($ array))
                  (call (meta-node ,f))

                  ;; Store return value
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore Stack Pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 8)
                  i32.add
                  i32.store))))))))

    (subtest "Empty Rest Argument List"
      (mock-backend-state
        (mock-meta-nodes ((f (a (rest b)) (a b)))
          (mock-contexts
              ((context (a) (functor f a (argument-list nil))))

            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32)

                :code

                 ;; Reserve Stack Space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 4)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out Stack
                  (local.get ($ stack))
                  (i32.const 0)
                  (i32.store (offset 0))

                  ;; Save parameter `a` on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Create Empty List
                  (call (import "runtime" "empty_list"))
                  (local.set ($ args))

                  ;; Call meta-node `f`
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)      ; Refresh parameter `a` from stack
                  (local.get ($ args))
                  (call (meta-node ,f))

                  ;; Set result
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore stack pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 4)
                  i32.add
                  i32.store))))))))

    (subtest "With Outer Node References"
      (subtest "Simple Node Reference"
        (mock-backend-state
          (mock-meta-nodes ((f (a :outer b) (a b)))

            (mock-contexts
                ((context (a b)
                          (->> (alist-hash-map `((b . ,b)))
                               (functor-expression f (list a) :outer-nodes))))

              (with-alias-map
                (test-context-function
                 context

                 (make-wasm-function-spec
                  :params '(i32 i32)
                  :results '(i32)
                  :locals '(i32)

                  :code
                  `((local.get ($ a))
                    (local.get ($ b))
                    (call (meta-node ,f))
                    (local.set ($ result))
                    (local.get ($ result))))))))))

      (subtest "Complex Expression"
        (subtest "Strict Argument"
          (mock-backend-state
            (mock-meta-nodes ((f (a :outer b) (a b))
                              (+ (a b) (a b)))

              (mock-contexts
                  ((context (a b c)
                            (->> (alist-hash-map `((b . ,(functor + b c))))
                                 (functor-expression f (list a) :outer-nodes))))

                (with-alias-map
                  (test-context-function
                   context

                   (make-wasm-function-spec
                    :params '(i32 i32 i32)
                    :results '(i32)
                    :locals '(i32 i32 i32)

                    :code

                     ;; Reserve Stack Space
                    `((i32.const ($ global-stack))
                      (i32.const ($ global-stack))
                      i32.load
                      (i32.const 4)
                      i32.sub
                      (local.tee ($ stack))
                      i32.store

                      ;; Zero out reserved stack space
                      (local.get ($ stack))
                      (i32.const 0)
                      (i32.store (offset 0))

                      ;; Save parameter `a` on stack
                      (local.get ($ stack))
                      (local.get ($ a))
                      (i32.store (offset 0))

                      ;; Call Meta-Node `+`
                      (local.get ($ b))
                      (local.get ($ c))
                      (call (meta-node ,+))
                      (local.set ($ a+b))

                      ;; Call Meta-Node `f`
                      (local.get ($ stack))
                      (i32.load (offset 0))
                      (local.tee ($ a)) ; Refresh parameter `a` from stack
                      (local.get ($ a+b))
                      (call (meta-node ,f))

                      ;; Set result
                      (local.set ($ result))
                      (local.get ($ result))

                      ;; Restore stack pointer
                      (i32.const ($ global-stack))
                      (local.get ($ stack))
                      (i32.const 4)
                      i32.add
                      i32.store))))))))

        (subtest "Lazy Argument"
          (mock-backend-state
            (mock-meta-nodes ((f (a :outer b) (a))
                              (+ (a b) (a b)))

              (mock-contexts
                  ((context (a)
                            (->> (alist-hash-map `((b . ,(functor + a 3))))
                                 (functor-expression f (list 1) :outer-nodes))))

                (with-alias-map
                  (test-context-function
                   context

                   (make-wasm-function-spec
                    :params '(i32)
                    :results '(i32)
                    :locals '(i32 i32 i32)

                    :code

                     ;; Reserve Stack Space
                    `((i32.const ($ global-stack))
                      (i32.const ($ global-stack))
                      i32.load
                      (i32.const 4)
                      i32.sub
                      (local.tee ($ stack))
                      i32.store

                      ;; Zero out reserved stack space
                      (local.get ($ stack))
                      (i32.const 0)
                      (i32.store (offset 0))

                      ;; Save parameter `a` on stack
                      (local.get ($ stack))
                      (local.get ($ a))
                      (i32.store (offset 0))

                      ;; Create Thunk
                      (i32.const 16)
                      (call (import "runtime" "alloc"))
                      (local.tee ($ thunk))
                      (i32.const 0)
                      i32.store          ; Set type = thunk
                      (local.get ($ thunk))
                      (i32.const (thunk 0))
                      (i32.store (offset 4)) ; Set thunk function = 0
                      (local.get ($ thunk))
                      (i32.const 1)
                      (i32.store (offset 8)) ; Set closure size
                      (local.get ($ thunk))
                      (local.get ($ stack))
                      (i32.load (offset 0)) ; Refresh a from stack
                      (local.tee ($ a))
                      (i32.store (offset 12))

                      ;; Call Meta-Node `f`
                      (i32.const ,(+ (* 4 1) 1))
                      (local.get ($ thunk))
                      (call (meta-node ,f))

                      ;; Set result
                      (local.set ($ result))
                      (local.get ($ result))

                      ;; Restore stack pointer
                      (i32.const ($ global-stack))
                      (local.get ($ stack))
                      (i32.const 4)
                      i32.add
                      i32.store))))

                (with-alias-map
                  (test-thunk-function
                   0

                   (make-wasm-function-spec
                    :params '(i32)
                    :results '(i32)
                    :locals '(i32 i32)

                    :code

                     ;; Load parameter first parameter from closure
                    `((local.get 0)
                      (i32.load (offset 4))
                      (local.set ($ p1))

                      ;; Call meta-node `+`
                      (local.get ($ p1))
                      (i32.const ,(+ (* 4 3) 1))
                      (call (meta-node ,+))

                      ;; Result
                      (local.set ($ result))
                      (local.get ($ result)))))))))))))

  (subtest "Objects"
    (subtest "Simple Field Value Expressions"
      (mock-backend-state
        (mock-meta-nodes ((+ (a b) (a b)))
          (mock-contexts
              ((context (a) (object ("x" a)
                                    ("y" (functor + a 10)))))

            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32)

                :code

                 ;; Reserve Stack Space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 8)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero Out Stack Space
                  (local.get ($ stack))
                  (i64.const 0)
                  (i64.store (offset 0))

                  ;; Save parameter `a` on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Allocate space for thunk `x + 10`
                  (i32.const 16)
                  (call (import "runtime" "alloc"))
                  (local.set ($ thunk))

                  ;; Save reference to thunk on stack
                  (local.get ($ stack))
                  (local.get ($ thunk))
                  (i32.store (offset 4))

                  ;; Create Thunk
                  (local.get ($ thunk))
                  (i32.const 0)
                  i32.store              ; Set type = thunk
                  (local.get ($ thunk))
                  (i32.const (thunk 0))
                  (i32.store (offset 4)) ; Set thunk function
                  (local.get ($ thunk))
                  (i32.const 1)
                  (i32.store (offset 8)) ; Set closure size
                  (local.get ($ thunk))
                  (local.get ($ stack))
                  (i32.load (offset 0))  ; Refresh parameter `a`
                  (local.tee 0)
                  (i32.store (offset 12)) ; Store `a` in thunk closure

                  ;; Allocate space for object
                  (i32.const 12)
                  (call (import "runtime" "alloc"))
                  (local.tee ($ object))
                  (i32.const (data ($ desc)))
                  i32.store              ; Set Object descriptor

                  ;; Store Field `x`
                  (local.get ($ object))
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)          ; Refresh parameter `a`
                  (i32.store (offset 4)) ; Store field x = `a`

                  ;; Store Field `y`
                  (local.get ($ object))
                  (local.get ($ stack))
                  (i32.load (offset 4))  ; Refresh thunk pointer
                  (local.tee ($ thunk))
                  (i32.store (offset 8)) ; Store field y = thunk

                  ;; Set return value
                  (local.get ($ object))

                  ;; Restore Stack Pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 8)
                  i32.add
                  i32.store)))

              ;; Test that an object descriptor with 2 fields has been
              ;; created.
              (test-data-bytes
               ($$ desc) #(2 0 0 0)

               :description
               "Number of fields in object descriptor equal")))))))

  (subtest "Meta-Node References"
    (subtest "Without Outer Nodes"
      (mock-backend-state
        (mock-meta-nodes ((map (f l) (f l))
                          (f (x) (x)))

          (mock-contexts
              ((context (a) (functor map (meta-node-ref f) a)))

            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32)

                :code

                `((i32.const (meta-node-ref ,f))
                  (i32.const 2)
                  i32.shl
                  (i32.const 2)
                  i32.or

                  (local.set ($ ref))
                  (local.get ($ ref))
                  (local.get 0)
                  (call (meta-node ,map))

                  (local.set ($ result))
                  (local.get ($ result))))))

            (with-alias-map
              (test-meta-node-ref-function
               f

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32 i32)

                :code

                 ;; Reserve Stack Space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 4)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out stack
                  (local.get ($ stack))
                  (i32.const 0)
                  (i32.store (offset 0))

                  ;; Save arguments list on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Load number of arguments in argument list
                  (local.get 0)
                  i32.load
                  (local.set ($ num-args))

                  ;; Check Arity
                  (block
                      (local.get ($ num-args))
                    (i32.const 1)
                    i32.eq
                    (br_if 0)
                    (call (import "runtime" "arity_error"))
                    return)

                  ;; Refresh argument list from stack
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)

                  ;; Load parameter `x` from argument list
                  (i32.load (offset 4))
                  (local.set ($ x))

                  ;; Call meta-node
                  (local.get ($ x))
                  (call (meta-node ,f))

                  ;; Set return value
                  (local.set ($ result))
                  (local.get ($ result))

                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 4)
                  i32.add
                  i32.store))))))))

    (subtest "With Optional Arguments"
      (mock-backend-state
        (mock-meta-nodes ((map (f l) (f l))
                          (1+ (n (optional d)) (n d)))

          (mock-contexts
              ((context (a)
                        (functor map (meta-node-ref 1+ :optional (list 6)) a)))


            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32)

                :code

                 ;; Reserve stack space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 4)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out stack elements
                  (local.get ($ stack))
                  (i32.const 0)
                  (i32.store (offset 0))

                  ;; Save parameter `a` on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Allocate Meta-Node Reference Object
                  (i32.const 16)
                  (call (import "runtime" "alloc"))
                  (local.tee ($ ref))
                  (i32.const 7)
                  i32.store            ; Set type = meta-node reference
                  (local.get ($ ref))
                  (i32.const (meta-node-ref ,1+))
                  (i32.store (offset 4)) ; Set meta-node reference function

                  ;; Store Optional Arguments
                  (local.get ($ ref))
                  (i32.const 1)
                  (i32.store (offset 8)) ; Set number of optional arguments
                  (local.get ($ ref))
                  (i32.const ,(+ (* 4 6) 1))
                  (i32.store (offset 12)) ; Store argument d = 6

                  ;; Call Meta-Node `map`
                  (local.get ($ ref))
                  (local.get ($ stack))
                  (i32.load (offset 0)) ; Refresh parameter a from stack
                  (local.tee 0)
                  (call (meta-node ,map))

                  ;; Set return value
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore stack pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 4)
                  i32.add
                  i32.store))))

            (with-alias-map
              (test-meta-node-ref-function
               1+

               (make-wasm-function-spec
                :params '(i32 i32)
                :results '(i32)
                :locals '(i32 i32 i32 i32 i32)

                :code

                 ;; Reserve stack space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 8)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out stack elements
                  (local.get ($ stack))
                  (i64.const 0)
                  (i64.store (offset 0))

                  ;; Save argument list on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Save optional arguments on stack
                  (local.get ($ stack))
                  (local.get 1)
                  (i32.store (offset 4))

                  ;; Load number of arguments
                  (local.get 0)
                  i32.load
                  (local.set ($ num-args))

                  ;; Check Arity
                  (block
                      (local.get ($ num-args))
                    (i32.const 1)
                    i32.ge_u
                    (local.get ($ num-args))
                    (i32.const 2)
                    i32.le_u
                    i32.and
                    (br_if 0)
                    (call (import "runtime" "arity_error"))
                    return)

                  ;; Load Required Arguments
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)         ; Refresh argument list pointer
                  (i32.load (offset 4))
                  (local.set ($ n))      ; Load argument `n`

                  ;; Load Optional Arguments
                  (block
                      (block
                          (block
                              (local.get ($ num-args))
                            (i32.const 1)
                            i32.sub
                            (br_table 1 0))

                        ;; Load `d` from argument list
                        (local.get 0)
                        (i32.load (offset 8))
                        (local.set ($ d))
                        (br 1))

                    ;; Load `d` from default values
                    (local.get ($ stack))
                    (i32.load (offset 4))
                    (local.tee 1)      ; Refresh default values pointer
                    (i32.load (offset 0)) ; Load default `d` value
                    (local.set ($ d))
                    (br 0))

                  ;; Call meta-node
                  (local.get ($ n))
                  (local.get ($ d))
                  (call (meta-node ,1+))

                  ;; Set return value
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore stack pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 8)
                  i32.add
                  i32.store))))))))

    (subtest "With Rest Arguments"
      (mock-backend-state
        (mock-meta-nodes ((map (f l) (f l))
                          (list (x (rest xs)) (x xs)))

          (mock-contexts
              ((context (a) (functor map (meta-node-ref list) a)))

            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32)

                :code

                `((i32.const (meta-node-ref ,list))
                  (i32.const 2)
                  i32.shl
                  (i32.const 2)
                  i32.or
                  (local.set ($ ref))

                  (local.get ($ ref))
                  (local.get 0)
                  (call (meta-node ,map))

                  (local.set ($ result))
                  (local.get ($ result))))))

            (with-alias-map
              (test-meta-node-ref-function
               list

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32 i32 i32 i32)

                :code

                 ;; Reserve stack space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 8)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out stack elements
                  (local.get ($ stack))
                  (i64.const 0)
                  (i64.store (offset 0))

                  ;; Save parameter argument list on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Load number of arguments
                  (local.get 0)
                  i32.load
                  (local.set ($ num-args))

                  ;; Check arity
                  (block
                      (local.get ($ num-args))
                    (i32.const 1)
                    i32.ge_u
                    (br_if 0)
                    (call (import "runtime" "arity_error"))
                    return)

                  ;; Refresh argument list from stack
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)

                  ;; Unpack parameter `x` from argument list
                  (i32.load (offset 4))
                  (local.set ($ x))
                  (local.get ($ stack))
                  (local.get ($ x))
                  (i32.store (offset 4)) ; Store on stack

                  ;; Unpack rest arguments
                  (block
                      (block
                          ;; Check whether rest argument list is empty
                          (local.get ($ num-args))
                        (i32.const 1)
                        i32.le_u
                        (br_if 0)

                        ;; Compute size of rest argument array in bytes
                        (local.get ($ num-args))
                        (i32.const 1)
                        i32.sub
                        (i32.const 4)
                        i32.mul
                        (local.tee ($ rest-size))
                        (i32.const 8)  ; Add 8 for type and size fields
                        i32.add
                        (call (import "runtime" "alloc"))
                        (local.tee ($ rest-array))

                        ;; Set object type = array
                        (i32.const ,+type-array+)
                        i32.store

                        (local.get ($ rest-array))
                        (local.get ($ num-args))
                        (i32.const 1)
                        i32.sub
                        (i32.store (offset 4)) ; Store size of array

                        ;; Get pointer to start of rest array
                        (local.get ($ rest-array))
                        (i32.const 8)
                        i32.add

                        ;; Get pointer to start of rest arguments in
                        ;; argument list
                        (local.get ($ stack))
                        (i32.load (offset 0))
                        (local.tee 0)   ; Refresh argument list pointer
                        (i32.const 8)
                        i32.add
                        (local.get ($ rest-size))
                        (call (import "runtime" "memcopy")) ; Copy arguments
                        (br 1))

                    (call (import "runtime" "empty_list"))
                    (local.set ($ rest-array)))

                  ;; Call meta-node
                  (local.get ($ stack))
                  (i32.load (offset 4))
                  (local.tee ($ x))      ; Refresh pointer to `x`
                  (local.get ($ rest-array))
                  (call (meta-node ,list))

                  ;; Set return value
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore stack pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 8)
                  i32.add
                  i32.store))))))))

    (subtest "With Outer Node Arguments"
      (mock-backend-state
        (mock-meta-nodes ((map (f l) (f l))
                          (f (a :outer x) (a x)))

          (mock-contexts
              ((context (a)
                        (functor map (meta-node-ref f :outer-nodes (alist-hash-map '((x . 3)))) a)))


            (with-alias-map
              (test-context-function
               context

               (make-wasm-function-spec
                :params '(i32)
                :results '(i32)
                :locals '(i32 i32 i32)

                :code

                 ;; Reserve stack space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 4)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out stack elements
                  (local.get ($ stack))
                  (i32.const 0)
                  (i32.store (offset 0))

                  ;; Save parameter `a` on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Allocate function reference object
                  (i32.const 16)
                  (call (import "runtime" "alloc"))
                  (local.tee ($ funcref))
                  (i32.const ,+type-funcref-args+)
                  i32.store              ; Set type = funcref

                  ;; Set meta-node
                  (local.get ($ funcref))
                  (i32.const (meta-node-ref ,f))
                  (i32.store (offset 4))

                  ;; Set number of argument values
                  (local.get ($ funcref))
                  (i32.const 1)
                  (i32.store (offset 8)) ; Store number of outer nodes

                  ;; Store outer node argument value 3
                  (local.get ($ funcref))
                  (i32.const ,(+ (* 4 3) 1))
                  (i32.store (offset 12))

                  ;; Call meta-node `map`
                  (local.get ($ funcref))
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)      ; Refresh pointer to parameter `a`
                  (call (meta-node ,map))

                  ;; Set result
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore stack pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 4)
                  i32.add
                  i32.store))))

            (with-alias-map
              (test-meta-node-ref-function
               f

               (make-wasm-function-spec
                :params '(i32 i32)
                :results '(i32)
                :locals '(i32 i32 i32 i32 i32)

                :code

                 ;; Reserve stack space
                `((i32.const ($ global-stack))
                  (i32.const ($ global-stack))
                  i32.load
                  (i32.const 8)
                  i32.sub
                  (local.tee ($ stack))
                  i32.store

                  ;; Zero out stack elements
                  (local.get ($ stack))
                  (i64.const 0)
                  (i64.store (offset 0))

                  ;; Save argument list on stack
                  (local.get ($ stack))
                  (local.get 0)
                  (i32.store (offset 0))

                  ;; Save function reference object on stack
                  (local.get ($ stack))
                  (local.get 1)
                  (i32.store (offset 4))

                  ;; Load number of arguments
                  (local.get 0)
                  i32.load
                  (local.set ($ num-args))

                  ;; Check arity
                  (block
                      (local.get ($ num-args))
                    (i32.const 1)
                    i32.eq
                    (br_if 0)
                    (call (import "runtime" "arity_error"))
                    return)

                  ;; Refresh argument list pointer
                  (local.get ($ stack))
                  (i32.load (offset 0))
                  (local.tee 0)

                  ;; Load first argument `a`
                  (i32.load (offset 4))
                  (local.set ($ a))

                  ;; Refresh function reference pointer
                  (local.get ($ stack))
                  (i32.load (offset 4))
                  (local.tee 1)

                  ;; Load outer node `x` value
                  (i32.load (offset 0))
                  (local.set ($ x))

                  ;; Call meta-node
                  (local.get ($ a))
                  (local.get ($ x))
                  (call (meta-node ,f))

                  ;; Set return value
                  (local.set ($ result))
                  (local.get ($ result))

                  ;; Restore stack pointer
                  (i32.const ($ global-stack))
                  (local.get ($ stack))
                  (i32.const 8)
                  i32.add
                  i32.store)))))))))

  (subtest "Higher Order Meta-Nodes"
    (subtest "Single Argument"
      (mock-backend-state
        (mock-contexts
            ((context (f) (functor f 53)))

          (with-alias-map
            (test-context-function
             context

             (make-wasm-function-spec
              :params '(i32)
              :results '(i32)
              :locals '(i32 i32 i32 i32 i32)

              :code

               ;; Reserve Stack Space
              `((i32.const ($ global-stack))
                (i32.const ($ global-stack))
                i32.load
                (i32.const 8)
                i32.sub
                (local.tee ($ stack))
                i32.store

                ;; Zero out stack elements
                (local.get ($ stack))
                (i64.const 0)
                (i64.store (offset 0))

                (block                   ; To Epilogue
                    (block               ; To Type Error
                        (block           ; To failure
                            ;; Resolve function `f`
                            (local.get 0)
                          (call (import "runtime" "resolve"))
                          (local.set 0)

                          ;; Save reference on stack
                          (local.get ($ stack))
                          (local.get 0)
                          (i32.store (offset 0))

                          (block         ; To Indirect Call
                              (block     ; To Immediate
                                  ;; Check Tag
                                  (block ; To Pointer
                                      (local.get 0)
                                    (i32.const ,+tag-mask+)
                                    i32.and
                                    (br_table 0 4 1 3))

                                ;; Pointer to object

                                ;; Load Type
                                (local.get 0)
                                i32.load

                                ;; Check if function reference object
                                (local.tee ($ f-type))
                                (i32.const ,+type-funcref-args+)
                                i32.ne
                                (br_if 3)
                                (br 1))

                            ;; Immediate function reference

                            (i32.const ,+type-funcref+)
                            (local.set ($ f-type))

                            ;; Decode function index
                            (local.get 0)
                            (i32.const 2)
                            i32.shr_s
                            (local.tee ($ f))

                            (i32.const 2)
                            i32.lt_u
                            (br_if 2))

                          ;; Call indirect meta-node

                          ;; Allocate argument list
                          (i32.const 12)
                          (call (import "runtime" "alloc"))
                          (local.set ($ arg-list))
                          (local.get ($ stack))
                          (local.get ($ arg-list))
                          (i32.store (offset 4))

                          (local.get ($ arg-list))
                          (i32.const ,+type-array+)
                          i32.store      ; Set type = array

                          (local.get ($ arg-list))
                          (i32.const 1)
                          (i32.store (offset 4)) ; Store number of arguments

                          ;; Store argument 53 in array
                          (local.get ($ arg-list))
                          (i32.const ,(+ (* 53 4) 1))
                          (i32.store (offset 8))

                          (local.get ($ f-type))
                          (i32.const ,+type-funcref+)
                          i32.eq
                          (if (result i32)
                              (then      ; Simple function reference
                               (local.get ($ arg-list))
                               (i32.const 4)
                               i32.add
                               (local.get ($ f))
                               (call_indirect (type (func (param i32) (result i32)))))

                              (else ; Function reference with optional arguments
                               (local.get ($ stack))
                               (i32.load (offset 4))
                               (local.tee ($ arg-list))
                               (i32.const 4)
                               i32.add

                               ;; Refresh function reference from stack
                               (local.get ($ stack))
                               (i32.load (offset 0))

                               ;; Get pointer to default value array
                               (local.tee 0)
                               (i32.const 8)
                               i32.add

                               (local.get 0)
                               (i32.load (offset 4))
                               (call_indirect (type (func (param i32 i32) (result i32))))))

                          (local.set ($ result))
                          (br 2))

                      ;; Set failure type
                      (local.get ($ stack))
                      (i32.load (offset 0))
                      (local.tee 0)
                      (local.set ($ result))
                      (br 1))

                  (call (import "runtime" "fail_type_error"))
                  (local.set ($ result)))

                ;; Set return value
                (local.get ($ result))

                ;; Restore stack pointer
                (i32.const ($ global-stack))
                (local.get ($ stack))
                (i32.const 8)
                i32.add
                i32.store)))))))))

(subtest "Optimizations"
  (subtest "Unboxing Elimination"
    (subtest "Single Block Repeated Unboxing"
      (subtest "Same Variable"
        (test-optimize-unboxing
         '(0)

         '((resolve 0)
           (unbox 0 (type number))
           (i32.const 1)
           i32.add
           (local.set 1)

           (resolve 0)
           (unbox 0 (type number))
           (local.get 1)
           i32.mul)

         '((resolve 0)
           (unbox 0 (type number))
           (i32.const 1)
           i32.add
           (local.set 1)

           (check-type 0 number)
           (local.get 1)
           i32.mul)))

      (subtest "Different Variables"
        (test-optimize-unboxing
         '(0 2)

         '((resolve 0)
           (unbox 0 (type number))
           (i32.const 1)
           i32.add
           (local.set 1)

           (resolve 2)
           (unbox 2 (type number))
           (local.get 1)
           i32.mul)

         '((resolve 0)
           (unbox 0 (type number))
           (i32.const 1)
           i32.add
           (local.set 1)

           (resolve 2)
           (unbox 2 (type number))
           (local.get 1)
           i32.mul)))

      (subtest "Same Variable Different Types"
        (test-optimize-unboxing
         '(0)

         '((resolve 0)
           (unbox 0 (type number))
           (i32.const 1)
           i32.add
           (local.set 1)

           (unbox 0 (type character)))

         '((resolve 0)
           (unbox 0 (type number))
           (i32.const 1)
           i32.add
           (local.set 1)

           (check-type 0 character)))))

    (subtest "Multiple Blocks Repeated Unboxing"
      (subtest "Non-Branching"
        (test-optimize-unboxing
         '(0)

         '((local.get 1)
           (local.set 2)

           (block $out
             (resolve 0)
             (unbox 0 (type number))
             (i32.const 1)
             i32.add
             (local.set 3)
             (br $out))

           (resolve 0)
           (unbox 0 (type number))
           (local.get 3)
           i32.mul

           (local.set 4))

         '((local.get 1)
           (local.set 2)

           (block $out
             (resolve 0)
             (unbox 0 (type number))
             (i32.const 1)
             i32.add
             (local.set 3)
             (br $out))

           (check-type 0 number)
           (local.get 3)
           i32.mul

           (local.set 4))))

      (subtest "Branch Before Unboxing"
        ;; Test that repeated `unbox` instructions for the same
        ;; variable are not removed if there is a branch instruction
        ;; which might result in the first `unbox` instruction being
        ;; skipped however the second being executed.

        (subtest "Unconditional Branch"
          (test-optimize-unboxing
           '(0)

           '((block $out
               (local.get 1)
               (local.set 2)

               (br $out)

               (resolve 0)
               (unbox 0 (type number))
               (i32.const 1)
               i32.add

               (local.set 3))

             (resolve 0)
             (unbox 0 (type number))
             (local.get 3)
             i32.mul

             (local.set 4))

           '((block $out
               (local.get 1)
               (local.set 2)

               (br $out)

               (resolve 0)
               (unbox 0 (type number))
               (i32.const 1)
               i32.add

               (local.set 3))

             (resolve 0)
             (unbox 0 (type number))
             (local.get 3)
             i32.mul

             (local.set 4))))

        (subtest "Conditional Branch"
          (test-optimize-unboxing
           '(0)

           '((block $out
               (local.get 1)
               (i32.const 10)
               i32.le_u
               (br_if $out)

               (resolve 0)
               (unbox 0 (type number))
               (i32.const 1)
               i32.add

               (local.set 3))

             (resolve 0)
             (unbox 0 (type number))
             (local.get 3)
             i32.mul

             (local.set 4))

           '((block $out
               (local.get 1)
               (i32.const 10)
               i32.le_u
               (br_if $out)

               (resolve 0)
               (unbox 0 (type number))
               (i32.const 1)
               i32.add

               (local.set 3))

             (resolve 0)
             (unbox 0 (type number))
             (local.get 3)
             i32.mul

             (local.set 4))))

        (subtest "Branch Table"
          (test-optimize-unboxing
           '(0)

           '((block $out
               (block $b
                 (block $a
                   (local.get 1)
                   (br_table $a $b $out))

                 (local.get 2)
                 (i32.const 10)
                 i32.sub
                 (local.tee 2))

               (resolve 0)
               (unbox 0 (type number))
               (i32.const 1)
               i32.add

               (local.set 3))

             (resolve 0)
             (unbox 0 (type number))
             (local.get 3)
             i32.mul

             (local.set 4))

           '((block $out
               (block $b
                 (block $a
                   (local.get 1)
                   (br_table $a $b $out))

                 (local.get 2)
                 (i32.const 10)
                 i32.sub
                 (local.tee 2))

               (resolve 0)
               (unbox 0 (type number))
               (i32.const 1)
               i32.add

               (local.set 3))

             (resolve 0)
             (unbox 0 (type number))
             (local.get 3)
             i32.mul

             (local.set 4)))))

      (subtest "Repeated Unboxing in Same Block"
        ;; Test that the second `unbox` instruction is optimized out
        ;; if the branch instruction, which skips over the first
        ;; `unbox` instruction, will also skip over the second `unbox`
        ;; instruction.

        (subtest "Unconditional Branch"
          (test-optimize-unboxing
           '(0)

           '((block $out
               (block $a
                 (local.get 1)
                 (local.set 2)

                 (br $out)

                 (resolve 0)
                 (unbox 0 (type number))
                 (i32.const 1)
                 i32.add

                 (local.set 3))

               (resolve 0)
               (unbox 0 (type number))
               (local.get 3)
               i32.mul

               (local.set 4)))

           '((block $out
               (block $a
                 (local.get 1)
                 (local.set 2)

                 (br $out)

                 (resolve 0)
                 (unbox 0 (type number))
                 (i32.const 1)
                 i32.add

                 (local.set 3))

               (check-type 0 number)
               (local.get 3)
               i32.mul

               (local.set 4)))))

        (subtest "Conditional Branch"
          (test-optimize-unboxing
           '(0)

           '((block $out
               (block $a
                 (local.get 1)
                 (br_if $out)

                 (resolve 0)
                 (unbox 0 (type number))
                 (i32.const 1)
                 i32.add

                 (local.set 3))

               (resolve 0)
               (unbox 0 (type number))
               (local.get 3)
               i32.mul

               (local.set 4)))

           '((block $out
               (block $a
                 (local.get 1)
                 (br_if $out)

                 (resolve 0)
                 (unbox 0 (type number))
                 (i32.const 1)
                 i32.add

                 (local.set 3))

               (check-type 0 number)
               (local.get 3)
               i32.mul

               (local.set 4)))))

        (subtest "Branch Table"
          (test-optimize-unboxing
           '(0)

           '((block $out
               (block $c
                 (block $b
                   (block $a
                     (local.get 1)
                     (br_table $a $b $out))

                   (local.get 2)
                   (i32.const 10)
                   i32.sub
                   (local.tee 2))

                 (resolve 0)
                 (unbox 0 (type number))
                 (i32.const 1)
                 i32.add

                 (local.set 3))

               (resolve 0)
               (unbox 0 (type number))
               (local.get 3)
               i32.mul

               (local.set 4)))

           '((block $out
               (block $c
                 (block $b
                   (block $a
                     (local.get 1)
                     (br_table $a $b $out))

                   (local.get 2)
                   (i32.const 10)
                   i32.sub
                   (local.tee 2))

                 (resolve 0)
                 (unbox 0 (type number))
                 (i32.const 1)
                 i32.add

                 (local.set 3))

               (check-type 0 number)
               (local.get 3)
               i32.mul

               (local.set 4)))))))

    (subtest "Immediate Operands"
      ;; Test that `unbox` instructions are eliminated when unboxing
      ;; operands in which the immediate unboxed value is available in
      ;; a local variable

      (test-optimize-unboxing
       '((0 t))

       '((unbox 0 (type number))
         (i32.const 1)
         i32.add
         (local.set 1))

       '((check-type 0 number)
         (i32.const 1)
         i32.add
         (local.set 1))))

    (subtest "Strict Operands"
      ;; Test that `resolve` instructions are eliminated when unboxing
      ;; operands which are strictly computed.

      (test-optimize-unboxing
       '((0 nil t))

       '((resolve 0)
         (unbox 0 (type number))
         (i32.const 1)
         i32.add
         (local.set 1))

       '((unbox 0 (type number))
         (i32.const 1)
         i32.add
         (local.set 1)))))

  (subtest "Boxing Elimination"
    (subtest "Unused Boxed Values"
      ;; Tests that BOX instructions are removed when only the
      ;; immediate value is actually referenced.

      (subtest "Integer Type"
        (test-optimize-boxing
         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (local.get 2)
           (resolve 1)
           (unbox 1 (type number))
           (local.get (value 1))
           i32.mul
           (box 3 (type i32))

           (local.get (ref 3)))

         `((local.get 0)
           (i32.const 1)
           i32.add

           (local.set (value 1))
           (i32.const ,+type-i32+)
           (local.set (type 1))

           (local.get 2)
           (resolve 1)
           (unbox 1 (type number))
           (local.get (value 1))
           i32.mul
           (box 3 (type i32))

           (local.get (ref 3)))))

      (subtest "Floating Point Type"
        (test-optimize-boxing
         '((local.get 0)
           (f32.const 1)
           f32.add
           (box 1 (type f32))

           (local.get 2)
           (resolve 1)
           (unbox 1 (type number))
           (local.get (value 1))
           f32.reinterpret_i32
           f32.mul
           (box 3 (type f32))

           (local.get (ref 3)))

         `((local.get 0)
           (f32.const 1)
           f32.add

           i32.reinterpret_f32
           (local.set (value 1))
           (i32.const ,+type-f32+)
           (local.set (type 1))

           (local.get 2)
           (resolve 1)
           (unbox 1 (type number))
           (local.get (value 1))
           f32.reinterpret_i32
           f32.mul
           (box 3 (type f32))

           (local.get (ref 3)))))

      (subtest "Failure Value"
        (test-optimize-boxing
         '((local.get 0)
           (box 1 (type fail))

           (block $out
             (local.get 2)

             (block $fail
               (resolve 1)
               (unbox 1 (type number))

               (local.get (value 1))
               i32.mul
               (box 3 (type i32))
               (br $out))

             (get-fail 1)
             (box 3 (type fail)))

           (local.get (ref 3)))

         '((local.get 0)
           (local.set (ref 1))

           (block $out
             (local.get 2)

             (block $fail
               (resolve 1)
               (unbox 1 (type number))

               (local.get (value 1))
               i32.mul
               (box 3 (type i32))
               (br $out))

             (get-fail 1)
             (box 3 (type fail)))

           (local.get (ref 3)))))

      (subtest "In Block"
        (test-optimize-boxing
         '((local.get 0)
           (box 1 (type fail))

           (block $out
             (local.get 2)

             (block $fail
               (resolve 1)
               (unbox 1 (type number))

               (local.get (value 1))
               i32.mul
               (box 3 (type i32))
               (br $out))

             (get-fail 1)
             (box 3 (type fail))))

         `((local.get 0)
           (local.set (ref 1))

           (block $out
             (local.get 2)

             (block $fail
               (resolve 1)
               (unbox 1 (type number))

               (local.get (value 1))
               i32.mul
               (local.set (value 3))
               (i32.const ,+type-i32+)
               (local.set (type 3))
               (br $out))

             (get-fail 1)
             (local.set (ref 3)))))))

    (subtest "Used Boxed Values"
      ;; Tests that BOX instructions are not removed if the boxed
      ;; object is referenced.

      (subtest "Simple Reference"
        (test-optimize-boxing
         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (local.get (ref 2))
           (local.get (ref 1))
           (i32.store (offset 4)))

         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (local.get (ref 2))
           (local.get (ref 1))
           (i32.store (offset 4)))))

      (subtest "In block"
        (test-optimize-boxing
         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (block
               (local.get (ref 2))
             (local.get (ref 1))
             (i32.store (offset 4))))

         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (block
               (local.get (ref 2))
             (local.get (ref 1))
             (i32.store (offset 4))))))

      (subtest "In Multiple blocks"
        (test-optimize-boxing
         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (block $a
             (local.get $x)
             (br_if $a)

             (block $b
               (local.get (ref 2))
               (local.get (ref 1))
               (i32.store (offset 4)))))

         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (block $a
             (local.get $x)
             (br_if $a)

             (block $b
               (local.get (ref 2))
               (local.get (ref 1))
               (i32.store (offset 4)))))))

      (subtest "In If - Then"
        (test-optimize-boxing
         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (local.get $x)
           (if (then
                (local.get (ref 2))
                (local.get (ref 1))
                (i32.store (offset 8)))

               (else
                (local.get (ref 2))
                (local.get $x)
                i64.store)))

         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (local.get $x)
           (if (then
                (local.get (ref 2))
                (local.get (ref 1))
                (i32.store (offset 8)))

               (else
                (local.get (ref 2))
                (local.get $x)
                i64.store)))))

      (subtest "In If - Else"
        (test-optimize-boxing
         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (local.get $x)
           (if (result i32)

               (then (result i32)
                (local.get (ref 1)))

               (else
                (local.get $x)))
           i32.store)

         '((local.get 0)
           (i32.const 1)
           i32.add
           (box 1 (type i32))

           (local.get $x)
           (if (result i32)

               (then (result i32)
                (local.get (ref 1)))

               (else
                (local.get $x)))
           i32.store))))))

(finalize)
