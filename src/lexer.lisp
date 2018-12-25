;;;; lexer.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018  Alexander Gutev
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

(in-package :tridash.parser)

(defparameter *line-term* nil
  "Flag for whether newlines should be treated as :TERMINATE tokens
   or :SKIP tokens.")

(defun make-lexer (stream)
  (make-instance 'lexer-input-stream
                 :stream stream
                 :rules '(("(\\r\\n|\\n|\\r)+" . :newline)
                          ("\\s" . :skip)
                          ("#.*" . :skip)
                          (";" . :terminate)
                          ("," . :comma)
                          ("\\(" . :open-paren)
                          ("\\)" . :close-paren)
                          ("{" . :open-brace)
                          ("}" . :close-brace)
                          ("-?[0-9]*\\.[0-9]+([dDeEfFlLsS][0-9]+)?(?=[\\s\\r\\n;,(){}\"]|$)" . :real)
                          ("-?[0-9]*[dDeEfFlLsS][0-9]+(?=[\\s\\r\\n;,(){}\"]|$)" . :real)
                          ("-?[0-9]+(?=[\\s\\r\\n;,(){}\"]|$)" . :integer)
                          ("\"(?:[^\"\\\\]|\\\\.)*\"" . :string)
                          ("\\.(?!\\.)" . :id)
                          ("\\.+" . :id)
                          ("[^\\s;.,(){}\"]+" . :id))))

(defun next-token (lex &key peek (line-term *line-term*))
  "Returns the next token as two values: the first value is a keyword
   symbol identifying the token, the second value is the lexeme making
   up the token. If the end of stream is reached NIL is returned. If
   PEEK is true the next token is returned however is not removed from
   the lexer's buffer. If a :LINE-TERM argument is supplied and is
   true a :TERNIMATE token is returned when a newline is read, if it
   is false newlines are treated as skip tokens and hence not
   returned. If a :LINE-TERM argument is not provided it defaults to
   the value of *LINE-TERM*."

  (loop
     for (type lxm) = (multiple-value-list (stream-read-token lex peek))
     for tok-type = (convert-token type line-term)
     while (skip? tok-type lex peek)
     finally (return (values tok-type lxm))))

(defun convert-token (type line-term)
  "Converts :NEWLINE tokens to either :TERMINATE, if LINE-TERM is
   true, or :SKIP tokens, if LINE-TERM is false. Returns the new token
   type, if TYPE is not :NEWLINE it is simply returned."

  (case type
    (:newline
     (if line-term
         :terminate
         :skip))
    (otherwise type)))

(defun skip? (type lex peek)
  "Returns true if TYPE is :SKIP, i.e. the token is a skip token. If
   PEEK is true and the token is a skip token it is removed from the
   lexer's buffer."

  (when (eq type :skip)
    (if peek (stream-read-token lex))
    t))

(defmethod print-object ((e unmatched-lexing-sequence) stream)
  (format stream "~&Parse Error: Invalid Token: '~a' at position ~a:~a~%"
          (unmatched-sequence e) (unmatched-sequence-row e) (unmatched-sequence-column e)))
