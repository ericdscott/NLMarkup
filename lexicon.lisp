#| This is intended to represent the lexicon based on some number of discourses that have been annotated.
|#

(in-package :user)



(in-package :natural-lexicon.lexicon)

(defclass lexicon ()
  ()
)

(defmethod user::demo ((demo (eql :lexicon)) &rest initargs)
  (loop for s across (sentences-of *discourse*)
       do 
       (maphash (lambda (k v)
		  (print (cannonically-indexed-category (getf v :category)))
		  )
		(annotations-of s))))

(defmethod user::demo ((demo (eql :annotations)) &rest initargs)
  (loop for s across (sentences-of *discourse*)
       do 
       (maphash (lambda (k v)
		  (print v)
		  )
		(annotations-of s))))


(defmethod cannonically-indexed-category ((pattern string) 
					  &key 
					  (lex-table (make-hash-table :test #'equalp))
					  (next-s-index 0)
					  (next-n-index 0))
  "returns a new category with cannonical indexing, the better to match equivalent categories. Returns also the lexical context, useful in cases where we need to match multiple categories."
  (let* ((categories (split-sequence #\: pattern))
	 (response ""))
    (labels ((new-category-for (cat)
	       (ecase (aref cat 0)
		 ((#\s #\S)
		  (concatenate 'string "S" (princ-to-string (incf next-s-index))))
		 ((#\n #\N)
		  (concatenate 'string "N" (princ-to-string (incf next-n-index)))))))
      (loop
	 for cat in categories
	 for newCat = (or (gethash cat lex-table)
			  (setf (gethash cat lex-table)
				(new-category-for cat)))
	 do
	   (setq response (concatenate 'string response (if (not (string= response "")) ":" "") newCat))))
    (list response :lex-table lex-table :next-s-index next-s-index :next-n-index next-n-index)))

      
(defmethod cannonical-syntax-for (annotation annotations)
  (destructuring-bind (cat &rest lexical-context) (cannonically-indexed-category (getf annotation :category))
    (cons cat (mapcar (lambda (element)
			(typecase element
			  (cons ;;this is a sub-category, need to update the lexical context
			   (destructuring-bind (sub-cat &rest new-lexical-context)
			       (apply #'cannonically-indexed-category 
				      (cons (getf (gethash element annotations) :category) 
					    lexical-context))
			     
			     (setq lexical-context new-lexical-context)
			     (list sub-cat)))
			  (string (string-trim #(#\space #\newline #\page)  element))
			  ))
		      (getf annotation :elements)))))


(defmethod user::demo ((demo (eql :cannonical)) &rest initargs)
  (loop for sentence across (sentences-of *discourse*)
     do
       (let ((annotations (annotations-of sentence))
	     )
	 (maphash (lambda (k v)
		    (print (cannonical-syntax-for v annotations)))
		  annotations))))
	   

