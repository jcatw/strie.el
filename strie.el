;;; strie.el --- A simple implementation of the trie data structure
;;;              which uses native elisp data structures.  No
;;;              dependencies!

;; Author: James Atwood <jatwood@cs.umass.edu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Tries are an efficient data structure for the storage of strings.
;; Addition, deletion, and query functions all have O(m) performance,
;; where m is the length of the string to be added/deleted/queried.
;;
;; This implementation supports key-value storage, which means that a trie
;; can act as a substitute for a dictionary / hash-table.
;;
;; See http://en.wikipedia.org/wiki/Trie for more details.

;;; Setup:

;; Simply add strie.el to your load path and (require 'trie) to your .emacs.

;;; Usage:

;; (setq a-trie (strie-new))
;;
;; (strie-add a-trie "one" 1)
;; (strie-add a-trie "two" "2")
;;
;; (strie-contains? a-trie "one") -> t
;; (strie-contains? a-trie "on" ) -> nil
;;
;; (strie-get a-trie "one") -> 1
;; (strie-get a-trie "two") -> "2"
;; (strie-get a-trie "twomore") -> nil
;;
;; (strie-delete a-trie "one")
;; (strie-contains? a-trie "one") -> nil
;; (strie-get a-trie "one") -> nil

;;; Code:
(defun strie-new ()
  "Creates and returns a new trie."
  (list nil ;;terminal
	nil ;;value
	nil ;;children
	))
(defun strie-terminal? (trie)
  "Trie node terminal? getter."
  (car trie))

(defun strie-set-terminal (trie terminal)
  "Trie node terminal? setter."
  (setf (car trie) terminal))

(defun strie-value (trie)
  "Trie node value getter."
  (cadr trie))

(defun strie-set-value (trie value)
  "Trie node value setter."
  (setf (cadr trie) value))

(defun strie-children (trie)
  "Trie node children getter."
  (caddr trie))

(defun strie-add-child (trie child)
  "Adds a child to the trie node.  Child a (key . trie) pair."
  (setf (caddr trie) (cons child (caddr trie))))

(defun strie-get-child (trie key)
  "Gets child with key from a trie node."
  (cdr (assoc key (strie-children trie))))

(defun strie-add (trie str val)
  "Adds str to trie with value val."
  (cond
   ((string= "" str)
    (strie-set-terminal trie t)
    (strie-set-value trie val))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (if (strie-get-child trie next-char)
	  (strie-add (strie-get-child trie next-char) rest-chars val)
	  (let ((new-trie (strie-new)))
	    (strie-add-child trie `(,next-char . ,new-trie))
	    (strie-add new-trie rest-chars val)))))))

(defun strie-contains? (trie str)
  "Returns t if trie contains str, nil otherwise."
  (cond
   ((string= "" str) (strie-terminal? trie))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (let ((next-char-trie (strie-get-child trie next-char)))
	(if next-char-trie
	    (strie-contains? next-char-trie rest-chars)
	  nil))))))

(defun strie-get (trie str)
  "Returns the value associated with str if trie contains str,
nil otherwise."
  (cond
   ((string= "" str) (strie-value trie))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (let ((next-char-trie (strie-get-child trie next-char)))
	(if next-char-trie
	    (strie-get next-char-trie rest-chars)
	  nil))))))

(defun strie-delete (trie str)
  "If trie contains str, deletes str from trie.  Otherwise,
no action is taken.  Note that the trie structure is left intact; the
terminal? flag and value field for the correct node are simply set to nil."
  (cond
   ((string= "" str)
    (strie-set-value trie nil)
    (strie-set-terminal trie nil))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (let ((next-char-trie (strie-get-child trie next-char)))
	(if next-char-trie
	    (strie-delete next-char-trie rest-chars)
	  nil))))))


(provide 'strie)
;;; strie.el ends here
