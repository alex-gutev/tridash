;;;; misc.lisp
;;;;
;;;; Metalink Programming Language.
;;;; Copyright (C) 2017  Alexander Gutev
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

(in-package :metalink.util)

(defun same-length? (l1 l2)
  "Checks if two lists are of the same length."

  (do ((l1 l1 (rest l1))
       (l2 l2 (rest l2)))
      ((or (endp l1) (endp l2))
       (eq l1 l2))))

(defun adjoin-hash (key hash)
  "Adds KEY to HASH with the default value NIL if it is not already in
   HASH. Returns true if KEY was already present in HASH."
  (nth-value 1 (ensure-gethash key hash)))

(defun in-hash? (key hash)
  "Returns true if there is a value for KEY in the hash table HASH."
  (nth-value 1 (gethash key hash)))

(defun merge-hash (fn result hash)
  "All key-value pairs in RESULT which have a corresponding (with the
   same key) key-value pair in HASH are replaced with the result
   of (FUNCALL FN VAL1 VAL2), where VAL1 is the value in the hash-table
   RESULT and VAL2 is the value, under the same key, in HASH. Returns
   RESULT."

  (when hash
    (iter (for (key new-val) in-hashtable hash)
          (for old-val = (gethash key result))
          (setf (gethash key result)
                (funcall fn old-val new-val))))
  result)

(defun partition (test sequence &key (key #'identity))
  "Applies the predicate function TEST passing each element of
   SEQUENCE and partitions SEQUENCE into a list containing all the
   elements for which TEST returned true (first return value) and a
   list containing all the elements for which TEST returned
   false (second return value). The function KEY is applied on each
   element, the result of which is passed to TEST."

  (loop
     for element in sequence
     for x = (funcall key element)
     if (funcall test x)
     collect element into true
     else collect element into false
     finally (return (values true false))))

(defun hash-table-keys-values (hash-table)
  "Returns two values: a list of the keys in HASH-TABLE and a list of
   the corresponding values in HASH-TABLE."

  (iter (for (key value) in-hashtable hash-table)
        (collect key into keys)
        (collect value into values)
        (finally (return (values keys values)))))
