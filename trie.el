;;; trie.el --- A simple implementation of the trie data structure
;;;             which uses native elisp data structures.  No
;;;             dependencies!

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

;; Simply add trie.el to your load path and (require 'trie) to your .emacs.

;;; Usage:

;; (setq a-trie (trie-new))
;;
;; (trie-add a-trie "one" 1)
;; (trie-add a-trie "two" "2")
;;
;; (trie-contains? a-trie "one") -> t
;; (trie-contains? a-trie "on" ) -> nil
;;
;; (trie-get a-trie "one") -> 1
;; (trie-get a-trie "two") -> "2"
;; (trie-get a-trie "twomore") -> nil
;;
;; (trie-delete a-trie "one")
;; (trie-contains? a-trie "one") -> nil
;; (trie-get a-trie "one") -> nil

;;; Code:
(defun trie-new ()
  "Creates and returns a new trie."
  (list nil ;;terminal
	nil ;;value
	nil ;;children
	))
(defun trie-terminal? (trie)
  "Trie node terminal? getter."
  (car trie))

(defun trie-set-terminal (trie terminal)
  "Trie node terminal? setter."
  (setf (car trie) terminal))

(defun trie-value (trie)
  "Trie node value getter."
  (cadr trie))

(defun trie-set-value (trie value)
  "Trie node value setter."
  (setf (cadr trie) value))

(defun trie-children (trie)
  "Trie node children getter."
  (caddr trie))

(defun trie-add-child (trie child)
  "Adds a child to the trie node.  Child a (key . trie) pair."
  (setf (caddr trie) (cons child (caddr trie))))

(defun trie-get-child (trie key)
  "Gets child with key from a trie node."
  (cdr (assoc key (trie-children trie))))

(defun trie-add (trie str val)
  "Adds str to trie with value val."
  (cond
   ((string= "" str)
    (trie-set-terminal trie t)
    (trie-set-value trie val))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (if (trie-get-child trie next-char)
	  (trie-add (trie-get-child trie next-char) rest-chars val)
	  (let ((new-trie (trie-new)))
	    (trie-add-child trie `(,next-char . ,new-trie))
	    (trie-add new-trie rest-chars val)))))))

(defun trie-contains? (trie str)
  "Returns t if trie contains str, nil otherwise."
  (cond
   ((string= "" str) (trie-terminal? trie))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (let ((next-char-trie (trie-get-child trie next-char)))
	(if next-char-trie
	    (trie-contains? next-char-trie rest-chars)
	  nil))))))

(defun trie-get (trie str)
  "Returns the value associated with str if trie contains str,
nil otherwise."
  (cond
   ((string= "" str) (trie-value trie))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (let ((next-char-trie (trie-get-child trie next-char)))
	(if next-char-trie
	    (trie-get next-char-trie rest-chars)
	  nil))))))

(defun trie-delete (trie str)
  "If trie contains str, deletes str from trie.  Otherwise,
no action is taken.  Note that the trie structure is left intact; the
terminal? flag and value field for the correct node are simply set to nil."
  (cond
   ((string= "" str)
    (trie-set-value trie nil)
    (trie-set-terminal trie nil))
   (t
    (let ((next-char (substring str 0 1))
	  (rest-chars (substring str 1 nil)))
      (let ((next-char-trie (trie-get-child trie next-char)))
	(if next-char-trie
	    (trie-delete next-char-trie rest-chars)
	  nil))))))


(provide 'trie)
;;; trie.el ends here
