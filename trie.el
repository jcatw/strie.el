(defun trie-new ()
  "Creates and returns a new trie."
  (list nil ;;terminal
	nil ;;value
	nil ;;children
	))
(defun trie-terminal? (trie)
  (car trie))

(defun trie-set-terminal (trie terminal)
  (setf (car trie) terminal))

(defun trie-value (trie)
  (cadr trie))

(defun trie-set-value (trie value)
  (setf (cadr trie) value))

(defun trie-children (trie)
  (caddr trie))

(defun trie-add-child (trie child)
  (setf (caddr trie) (cons child (caddr trie))))

(defun trie-get-child (trie key)
  (cdr (assoc key (trie-children trie))))

(defun trie-add (trie str val)
  "Add str to trie with value val."
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
