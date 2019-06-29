;;;; builder-interface.lisp
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

;;;; Generic function interface to the node builder.

(in-package :tridash.frontend)


;;;; Flattened Node Symbol Table

(defclass flat-node-table ()
  ((nodes
    :accessor nodes
    :initarg :nodes
    :documentation
    "Set of all `NODE's, excluding `META-NODE's.")

   (meta-nodes
    :accessor meta-nodes
    :initarg :meta-nodes
    :documentation
    "Set of all `META-NODE's.")

   (input-nodes
    :accessor input-nodes
    :initarg :input-nodes
    :documentation
    "Set of all input nodes."))

  (:documentation
   "Set of all nodes in all modules, with `NODE's and `META-NODE's in
    separate sets. Does not contain pseudo-nodes."))


;;;; Declaration Processing Interface

(defgeneric process-declaration (decl module &key &allow-other-keys)
  (:documentation
   "Processes the declaration, creates the node(s) specified by the
    declaration and adds them to MODULE. Returns the node created, if
    any.

    If the :TOP-LEVEL keyword argument is provided and is T, the
    declaration is processed as though it appears at top-level
    otherwise it is processed as though it appears at the level
    (1+ *LEVEL*).

    If :ADD-OUTER is true (the default), *META-NODE* is not NIL and
    the node return by calling the next PROCESS-DECLARATION method is
    not in MODULE it is added to the outer node references of
    *META-NODE*."))

(defgeneric process-functor (operator operands module)
  (:documentation
   "Processes a functor node declaration. Creates the node(s)
    specified by the declaration and adds them to MODULE. Returns the
    node created, if any."))

;;;; Processing Attributes

(defgeneric process-attribute (node attribute value module)
  (:documentation
   "Applies special processing on setting the attribute ATTRIBUTE, of
    NODE, to VALUE. The attribute is set to the value returned by the
    method.")

  (:method ((node t) (attribute t) (value t) (module t))
    "Pass-through method. Returns the attribute."
    value))
