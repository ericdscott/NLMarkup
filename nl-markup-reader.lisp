(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :agraph))

(asdf:load-system "split-sequence")

(asdf:load-system "cl-ppcre")


(in-package :natural-lexicon.markup-reader)

(defvar user::*trace-stack* nil)

(TRIPLE-STORE:ENABLE-!-READER)
(register-namespace "natLex" "http://www.naturalLexicon.com/rdf/dummy.rdfs#") ;;for rdf quicknames


(defclass sentence-model ()
  ((source-uri :initarg :source-uri :reader source-uri-of 
	       :documentation "The URI of the source from which the text is taken of which this is one sentence.")
   (beginning-offset :accessor beginning-offset-of 
		     :documentation "The beginning offset of the sentence within the text.")
   (end-offset :accessor end-offset-of
	       :documentation "Then end offset of the sentence within the text.")
   (text :initform "" :initarg :text :accessor text-of
	 :documentation "The string representing the text from which this sentence was taken.")
   (annotations :initform (make-hash-table :test #'equalp) :accessor annotations-of
		:documentation "A hash table mapping a cons cell to a getf list. The cons-cell key is of the form (beginning-offset . end-offset), and stands for offsets of either the sentence itself or of one of the sub-elements within the sentence. Each key's associated getf list holds key-value pairs with keys including :semantics and :elements. :semantics contains representations of the semantic content in bracketed annotations. :elements are either words associated with the lexical entry for this tag, or the cons cells which are keys to sub-elements, whose representations will also be found in this table. There should be one entry in this table for every element in this sentence."
		)

   )
  (:documentation   "In order to mark up text, it is pretty much essential to break it up into sentences. Each top-level sentence is modeled as coming from some source document with a 'source-uri'. From this uri is a marked up 'text', with 'beginning-' and 'end-' '-offsets'. Also within this sentence are annotations for that sentence's components, each of which is annotated in 'annotations'. "))
  

(defmethod widest-span-of ((sentence sentence-model))
  "Returns a cons cell which indexes the annotation for the whole sentence within the sentence model (as opposed to sub-elements of the sentence). Typically passed as the 'span' parameter to interpret-semantic-spec."
  (cons 0 (length (text-of sentence))))


(defclass discourse-model ()
  ((db-path :initarg :db-path :accessor db-path-of
	    :documentation "The path to the triple-store which represents the discourse.")
   (uri :initarg :uri :accessor uri-of
	:documentation "Identifies the discourse.")
   (quickname :initarg :quickname :reader quickname-of
	      :documentation "Typically used to construct uris for local elements in the rdf representation of the discourse.")
   (sentences :initform (make-array 5 :element-type 'sentence-model :adjustable t :fill-pointer 0)
 
	      :accessor sentences-of
	      :documentation "The vector of sentence models corresponding to sentences parsed from the discourse.")
   (declarations :initform (make-hash-table :test #'equalp) :accessor declarations-of
		 :documentation "Allows you to make declarations within the semantic markup with the pattern 'declaration' <key> <value> instead of a triple.")
   )
  (:documentation   "In order to analyze text, it's important to have a model of the discourse so that entities within the conversation can be referenced across sentences, and relations between entities can be asserted. In this representation, semantics is represented as a set of rdf triples stored at 'db-path'. The disocurse itself has a 'uri' which anchors the other triples associated with this discourse, and such triples are assigned to the namespace associated with 'quickname'. Each discourse is broken out into a vector of top-level 'sentences', each an instance of the class _sentence_model_. "))


(defvar *discourse* nil "The current discourse being modelled.")


(defmethod add-sentence ((discourse discourse-model) (sentence sentence-model))
  "Adds the next parsed sentence to the discourse model."
  (vector-push-extend sentence (sentences-of discourse)))

(defmethod next-sentence-index-of ((discourse discourse-model))
  "Returns the next available sentence index from the discourse. Typically called when constructing uris for entities in the sentence currently being parsed."
  (length (sentences-of discourse)))

(defmethod graph-id-of ((discourse discourse-model))
  "An ID for this discourse to serve as the :g argument to each triple constructed."
  ;;TODO: make this more pricipalled.
  (intern-resource "natLex:Discourse1"))


;;BEGINNING OF PARSE SUITE
;;The operations of each of these functions is tightly bound to each other, and to the variables in the 'let'...

(let ((parse-stream nil) ;; the stream from which marked up text is read.
      (parse-stack nil) ;; a list of parse frames, pushed on open-paren and popped on close-paren
      (category (make-array 5 :element-type 'character :fill-pointer 0 :adjustable t)) ;; the category tag of the current tag
      (text (make-array 5 :element-type 'character :fill-pointer 0 :adjustable t)) ;; the characters that make up the text of the sentence currently being parsed
      (semantics (make-array 5 :element-type 'character :fill-pointer 0 :adjustable t)) ;;semantic spex for current tag
      (test nil) ;;just there for fun. TODO: remove this when no longer needed.
      )

  (defun reset-parse-state (stream)
    "Typically called before parsing the next top-level sentence. Resets the parse state to be consistent with the 'start' state label"
    (setq parse-stream stream)
    (setq parse-stack nil)
    (setf (fill-pointer category) 0)
    (setf (fill-pointer text) 0)
    (setf (fill-pointer semantics) 0)
    )
  (defun read-char-from-parse-stream ()
    "Typcially called at the start of each non-epsilon state transition. Used to determine the next delta."
    (let ((c (read-char-no-hang parse-stream nil :eof nil)))
      ;;(push (format nil "read-char:~a" c) user::*trace-stack*)
      c))

  (defun append-text (c)
    "Typically called when in a state that processes the actual text of the sentence being parsed. Appends a character to the parsed text."
    (check-type c character)
    (vector-push-extend c text)
    )
  (defun get-parsed-text ()
    "Typically called when we want to annotate, or otherwise look at the text processed so far."
    text)

  (defun append-to-category (c)
    "Typically called when in the states that deal with the category specification of the markup. Appends a character to the representation of the current category."
    (vector-push-extend c category))


  (defun get-and-clear-category() 
    "Typically called when we've finished reading in the category specification. Returns the category spec and clears it for a fresh start."
    (prog1 (copy-seq (coerce category 'string))
      (setf (fill-pointer category) 0)))

  (defun record-category ()
    "Typically called just after reading the category of the tag. Annotates the current frame with its category"
    (setf (car parse-stack) 
	  (append (list :category (get-and-clear-category)) 
		  (car parse-stack))))

  (defun append-to-semantics (c)
    "Typcially called when in states that deal with reading semantic markup."
    (vector-push-extend c semantics))

  (defun get-and-clear-semantics ()
    "Typically called just after reading the semantic markup."
    (prog1 (copy-seq (coerce semantics 'string))
      (setf (fill-pointer semantics) 0)))

  (defun record-semantics ()
    "Typically called just after reading the semantic markup of the tag. Annotates the current frame with its semantics"
    (setf (car parse-stack) 
	  (append (list :semantics (get-and-clear-semantics)) 
		  (car parse-stack))))

  (defun |start| () 
    "The beginning state when about to parse markup. Returns next state."
    (if (null parse-stream)
	(error "no stream at start"))

    (let ((c (read-char-from-parse-stream)))
      (ecase c
	;;skip whitespace...
	((#\space #\tab #\newline #\page)
	'|start|)
	;; open paren...
	(#\(
	 (push (list 
		:beginning (length text)
		)
	       parse-stack)
	 '|<open paren>|)
	 ;; it's over as soon as it started
	(:eof 
	 '|end|)
	)))

  (defun |<open paren>| () ;;just read open paren
    "We've just read an open paren for whichever frame is currently is the top of the parse stack."
    (let ((c (read-char-from-parse-stream)))
      (case c
	;; First thing should be either N or S...
	((#\N #\S) ;;this is a category
	 (append-to-category c)
	 '|<open paren><some category>|)
	(t (error "Open paren must be followed by S or N. Read ~a instead. (pos ~a)"
		  (file-position parse-stream))))))



  (defun |<open paren><some category>| () ;;just read <open paren> <category>
    "We're somewhere in the middle of reading the category spec"
    (let ((c (read-char-from-parse-stream)))
      (case c
	;; whitespace, done with category label
	((#\space #\tab #\newline #\page)
	 (record-category)
	 '|<open paren><category>|)
	;; number index...
	((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	   (append-to-category c)
	   '|<open paren><some category>|)
	;;colon separators for latent arguments....
	(#\:
	 (append-to-category c)
	 '|<open paren><category+colon>|)
	;; beginning semantic markup, done with category label...
	(#\{
	 (record-category)
	 '|<open paren><category><open brace>|
	 )
	;;starting a gloss like [author] for the reader
	(#\[
	 '|<open paren><category><some gloss>|)
	(t
	 (error "~a is not a number, colon, S or N (pos ~a)" 
		c
		(file-position parse-stream))))))

  (defun |<open paren><category><some gloss>|()
    (let ((c (read-char-from-parse-stream)))
      (case c
	(#\]
	 '|<open paren><some category>|
	 )
	;;just skip everything until you hit ]
	(t 
	 '|<open paren><category><some gloss>|
	))))
  (defun |<open paren><category+colon>|()
    "We've just read a colon in the category spec. Next up should be S or N"
    (let ((c (read-char-from-parse-stream)))
      (case c
	;;S or N....
	((#\S #\N)
	 (append-to-category c)
	 '|<open paren><some category>|)
	(t
	 (error "Expecting N or S, not ~a (pos ~a)" c (file-position parse-stream))))))

  (defun |<open paren><category>| ()
    "We've just finished reading the category spec"
    (let ((c (read-char-from-parse-stream)))
      (case c
	;;skip whitespace....
	((#\space #\tab #\newline #\page)
	 '|<open paren><category>|)
	;; open brace, start reading semantic spex...
	(#\{
	 '|<open paren><category><open brace>|
	 )
	;;open paren on new tag, mark beginning offset and push a new parse frame...
	(#\( 
	 (push (list :beginning (length text)) parse-stack)
	 '|<open paren>|)
	;;close paren - shouldn't happen here.
	(#\)
	 (error "not expecting a paren without at least some content (pos ~a)"
		(file-position parse-stream)))
	(t (append-text c)
	   '|<open paren><category><maybe semantics><some content>|))))

  (defun |<open paren><category><open brace>| ()
    (let ((c (read-char-from-parse-stream)))
      (case c
	((#\space #\tab #\newline #\page)
	 '|<open paren><category><open brace>|)
	(#\}
	 '|<open paren><category><open brace><maybe triples><close brace>|)
	(t 
	 (append-to-semantics c)
	 '|<open paren><category><open brace><some semantics>|
	 ))))
  
  (defun |<open paren><category><open brace><some semantics>|()
    (let ((c (read-char-from-parse-stream)))
      (case c
	(#\}
	 (record-semantics)
	 '|<open paren><category><open brace><maybe triples><close brace>|)
	(t
	 (append-to-semantics c)
	 '|<open paren><category><open brace><some semantics>|
	 ))))
    

  (defun |<open paren><category><open brace><maybe triples><close brace>| ()
    "an epsilon that goes directly to the equivalent state"
    '|<open paren><category><semantics>|
    )

  (defun |<open paren><category><semantics>| ()
    (let ((c (read-char-from-parse-stream)))
      ;;(print (format nil "read ~a" c))
      (case c
	((#\space #\tab #\newline #\page)
	 '|<open paren><category><semantics>|)
	(#\( ;;open paren on new tag
	 (push (list :beginning (length text)) parse-stack)
	 '|<open paren>|)
	(#\)
	 (error "not expecting a right paren without at least some content(pos ~a)"
		(file-position parse-stream)))
	(t (append-text c)
	   '|<open paren><category><maybe semantics><some content>|))))
	

  (defun |<open paren><category><maybe semantics><some content>| () 
    "We're in the middle of reading the language content of the markup."
    (let ((c (read-char-from-parse-stream)))
      (case c
	(#\( ;;open paren on new tag
	 (push (list :beginning (length text)) parse-stack)
	 '|<open paren>|)
    	(#\)
	 (setf (getf (car parse-stack) :end) (length text))
	 ;;(push (format nil "top of parse stack after reading close paren:~a" (copy-seq (car parse-stack))) user::*trace-stack*)
	 '|<open paren><category><maybe semantics><content><close paren>|)
	(:eof
	 (error "Premature eof after reading ~a (pos ~a)" 
		(get-parsed-text)
		(file-position parse-stream)))
	(t 
	 (append-text c)
	 ;;(push (format nil "parsed-text:~a" (get-parsed-text)) user::*trace-stack*)
	 '|<open paren><category><maybe semantics><some content>|))))

  (defun |<open paren><category><maybe semantics><content><close paren>| ()
    "We've just read an entire tag from ( to ). This is an epsilon state that goes directly to 'end' or '<open paren>...<some content>' without reading from the stream"
    (let ((sub-element (pop parse-stack)))

      (if (= (length parse-stack) 0)
	  (progn
	    (setq test sub-element)
	    '|end|)
	  ;;else record the element we just popped as a sub-element of the current tag. (Sub-elements are id'd by their offsets.)
	  (progn
	     (setf (getf (car parse-stack) :sub-elements)
		   (append (getf (car parse-stack) :sub-elements)
			   (list (cons (getf sub-element :beginning)
				       (getf sub-element :end)))))
	     '|<open paren><category><maybe semantics><some content>|))))
  (defun get-parse-stack () "Returns all stack frames" parse-stack)
  (defun get-test () "returns whatever you put in 'test'" test)

 ;; END OF PARSING SUITE
)


(defmethod append-string ((str string) (c character))
  "Utility function to append a character to a string"
  (concatenate 'string str (make-string 1 :initial-element c)))


(defmethod acquire-local-uri ((discourse discourse-model) (spec string))
  "Gets a uri for some entity encountered in the semantic specs. These were marked in the spec with a leading ?"
  (concatenate 'string (quickname-of discourse) ":" (prin1-to-string (next-sentence-index-of discourse)) "/" spec))


(defmethod acquire-uri ((spec string))
  (cond 
    ((char= (aref spec 0) #\?)
     (acquire-local-uri *discourse* (subseq spec 1)))
    (t
     spec)))

(defun translate-char-code-entity-escapes (input-string)
  "translates &#<number>; into the ascii characters corresponding to <number>. Typically called to 'undo' escapes for parentheses and such."
  (let ((response nil)
	(end-of-last-match 0)
	)
    (cl-ppcre:do-scans (match-start
			match-end
			register-starts
			register-ends
			'(:SEQUENCE 
			  (:NON-GREEDY-REPETITION 0 NIL :EVERYTHING) 
			  (:REGISTER ;;index 0
			   (:SEQUENCE
			    "&#"
			    (:REGISTER ;;index 1
			     (:GREEDY-REPETITION 0 NIL (:CHAR-CLASS (:RANGE #\0 #\9))))
			    #\;)))
			input-string)

      (setq response (concatenate 'string
				  response
				  (subseq input-string match-start (aref register-starts 0))
				  (make-array 1 :initial-element (code-char 
								  (parse-integer 
								   (subseq input-string 
									   (aref register-starts 1) 
									   (aref register-ends 1)))))
				  (subseq input-string 
					  (aref register-ends 0) 
					  match-end)))
      (setq end-of-last-match match-end))
    ;;append the rest of the string after the last match...
    (concatenate 'string response (subseq input-string end-of-last-match))))

(defmethod user::demo ((demo (eql :xml-entities)) &rest args)
  (let ((test-string "blah &#40; qwer &#41; kdk&#40;dkkd&#41;kdd"))
    (translate-char-code-entity-escapes test-string)))


(defvar *add-triple-hook* nil "(lambda (discourse triple)...) Called whenever we add a triple in acquire-triple")
(defmethod acquire-triple ((discourse discourse-model) (s-uri string) (p-uri string) (o-uri string) &optional (o-literal nil))
  "Retrieves or creates a triple per the arguments."
  (let ((s-resource (intern-resource s-uri))
	(p-resource (intern-resource p-uri))
	(o-resource (or (and o-literal (intern-literal (translate-char-code-entity-escapes o-literal)))
			(intern-resource o-uri)))

	)

    (let ((triple-spec (list :s s-resource :p p-resource :o o-resource :g (graph-id-of discourse))))
      (or (apply #'get-triple triple-spec)
	  (let* ((new-triple-id
		  (add-triple s-resource p-resource o-resource :g (graph-id-of discourse)))
		 (hook-response
		  (if *add-triple-hook*
		      (prog1
			  (funcall *add-triple-hook* discourse (get-triple-by-id new-triple-id))))))

	    (values new-triple-id hook-response))))))


(defmethod declare-triples ((discourse discourse-model) (triple-spex list))
  (ensure-directories-exist (db-path-of *discourse*))
  (let ((db 
	 (if (not (triple-store-exists-p (db-path-of discourse)))
	     (create-triple-store (db-path-of discourse))
	     ;;else
	     (open-triple-store (db-path-of discourse)))))
    (with-triple-store (db db)
      (dolist (triple-spec triple-spex)
	(apply #'acquire-triple (cons discourse triple-spec))))
    (close-triple-store :db db)))


(defconstant +literal-reference-marker+ #\@)

(defmethod interpret-semantic-spec ((discourse discourse-model)(sentence sentence-model) (span cons))
  "Adds rdf triples per the semantic spec for the tag indexed by <span>, and all sub-elements of said tag."
  (let* ((annotations (gethash span (annotations-of sentence)))
	 (semantics (getf annotations :semantics))
	 (triples (and semantics (split-sequence #\& semantics)))
	 (sub-elements (remove-if-not #'consp (getf annotations :elements)))
	 )

    (loop for triple in triples
       do 
	 (destructuring-bind (s-uri p-uri o-uri) 
	     (mapcar #'acquire-uri
		     (remove-if #'(lambda (s)
				    (string= s ""))
				(split-sequence #\space triple)))

	   (cond
	     ((string= (string-downcase s-uri) "declare")
	      (setf (gethash (string-downcase p-uri)(declarations-of discourse))
		    (string-downcase o-uri)))
	     ((string= o-uri "...")
	       (acquire-triple discourse s-uri p-uri "" 
			       (reduce (lambda (so-far s)
					 (concatenate 'string so-far s))
				       (remove-if #'consp (getf annotations :elements)))))
	     ((string= o-uri "_")
	      (acquire-triple discourse s-uri p-uri "" 
			      (subseq (text-of sentence) (car span)(cdr span))))
	     ((char= (aref o-uri 0) +literal-reference-marker+)
	      (setq user::*annotations* annotations)
	      (setq user::*sentence* sentence)
	      (let ((target
		     (find-if (lambda (sub-span) 
				(let ((sub-element (gethash sub-span (annotations-of sentence))))
				  (string= (getf sub-element :category) (subseq o-uri 1))))
			      sub-elements)))
		(setq user::*target* target)
		(if target
		    (acquire-triple discourse s-uri p-uri "" (subseq (text-of sentence) (car target) (cdr target)))
		    ;; else the length is wrong
		    (error "how do we find the literal reference? (see *annotations*)")))
	      )
	     (t
	       ;;else
	      (acquire-triple discourse s-uri p-uri o-uri)))))
    (if sub-elements 
	(loop for sub-element in sub-elements
	   do
	     (interpret-semantic-spec discourse sentence sub-element)))))



  
(defmacro with-discourse-db ((db discourse) &rest forms)
  "Opens the triple store associated with DISCOURSE as DB, executes forms, and closes db."
  `(unwind-protect 
	(let ()
	  (setq ,db  (if (not (triple-store-exists-p (db-path-of ,discourse)))
			 (create-triple-store 
			  (db-path-of ,discourse))
			 ;;else 
			 (open-triple-store (db-path-of ,discourse))))
	  ,@forms)
     (let ()
       (close-triple-store :db ,db))))

(defmethod user::demo ((demo (eql :with-discourse-db)) &rest args)
  (with-discourse-db (db *discourse*)
    ;;(print-triples db)))
    (let ((cursor (get-triples :db db)))
      (iterate-cursor (triple cursor)
	(print-triple triple)))))

(defmethod user::demo ((demo (eql :serialize-to-xml)) &rest args)
  (with-discourse-db (db *discourse*)
    ;;(print-triples db)))
    (let ((cursor (get-triples :db db)))
      (serialize-rdf/xml cursor "C:/Data/RDF/test-rdf.xml" :if-exists :supersede))))

(defmethod user::demo ((demo (eql :serialize-to-n3)) &rest args)
  (with-discourse-db (db *discourse*)
    ;;(print-triples db)))
    (let ((cursor (get-triples :db db)))
      (serialize-rdf-n3 cursor "C:/Data/RDF/test-rdf.n3" :if-exists :supersede))))

;; TESTING....


;;(register-namespace "cv-rdfs" "http://kaste.lv/~captsolo/semweb/resume/cv.rdfs#")
;;(register-namespace "cv-base" "http://kaste.lv/~captsolo/semweb/resume/base.rdfs#")

(defmethod user::demo ((demo (eql :sem-spec)) &rest args)
  (let ((*discourse* (user::demo :scan)))
    (let ((sentence (aref (sentences-of *discourse*) 0))
	  (triple-store-name "test.rdf")
	  (directory (ensure-directories-exist "C:/data/rdf/"))
	  )
      (let ((db 
	     (funcall (if 
		       (triple-store-exists-p triple-store-name)
		       #'create-triple-store
		       #'open-triple-store)
		      triple-store-name :directory directory)))


      (with-triple-store (db db)
	(interpret-semantic-spec *discourse* sentence (widest-span-of sentence))
	(print-triples (get-triples-list)))))))

			       


(defmethod annotate ((table hash-table) (text string) (parse-frame list))
  "Adds whatever entries make sense for the current parse frame. Typically called whenever we've finished reading a complete tag during parsing, in #'read-next-sentence."
  (let ((b (getf parse-frame :beginning))
	(e (getf parse-frame :end)))
    (when (gethash (cons b e) table)
      (error "Duplicate entries: ~a and ~a"
	     (gethash (cons b e) table)
	     parse-frame))
    (labels ((elements (so-far index token sub-element remaining-sub-elements)
	       "recursively builds up a list of elements between b and e, depending on sub-elements"
	       (if (>= index e) ;;we're done
		   (if (> (length token) 0)
		       (elements (append so-far (list token)) index "" sub-element nil)
		       ;;else there is no token
		       (if sub-element
			   (elements (append so-far (list sub-element)) index "" nil nil)
			   ;;else there is neither token nor sub-element
			   so-far))
		   ;;else we're not done yet
		   (labels ((element-beginning (element) ;;elements are represented by cons cells (b . e)
			      (car element))
			    (element-end (element)
			      (cdr element)))
		     (if sub-element ;;we're working on a sub-element...
			 (if (>= index (element-end sub-element)) ;;we're at the end of the sub-element
			     (elements (append so-far (list sub-element)) index "" nil remaining-sub-elements)
			     ;;else index is still within current sub-element
			     (elements so-far (+ index 1) "" sub-element remaining-sub-elements))
			 ;;else there's no sub-element
			 (if (> (length token) 0)
			     ;;we're building a token
			     (if (and remaining-sub-elements (>= index (element-beginning (car remaining-sub-elements))))
				 ;; we've just hit the next sub-element
				 (elements (append so-far (list token)) (+ index 1) "" (car remaining-sub-elements) (cdr remaining-sub-elements))
				 ;;else we're still appending characters to the token
				 (elements so-far (+ index 1) (append-string token (aref text index))  nil remaining-sub-elements))
			     ;;else the token is empty.
			     (if (and remaining-sub-elements (>= index (element-beginning (car remaining-sub-elements))))
				 ;; we've just hit the next sub-element
				 (elements so-far (+ index 1) "" (car remaining-sub-elements) (cdr remaining-sub-elements))
				 ;;else we are starting a fresh token
				 (elements so-far (+ index 1) (append-string "" (aref text index)) nil remaining-sub-elements)))))))
	     )
      (setf (gethash (cons b e) table)
	    (list :category (getf parse-frame :category)
		  :elements (elements nil b "" nil (getf parse-frame :sub-elements))
		  :semantics (getf parse-frame :semantics)))
      table)))


(defmethod read-next-sentence ((discourse discourse-model) (input stream))
  "Reads the next top-level sentence of markup from the input stream, and installs it in the discourse model. Returns the new sentence model."
  (let ((sentence (make-instance 'sentence-model))
	)
    (reset-parse-state input)
    (setf (beginning-offset-of sentence) (file-position input))
    (loop for state = '|start| then (funcall (symbol-function state))
       while (not (eq state '|end|))
       do 
	 ;;(push (format nil "State:~a" state) user::*trace-stack*)
	 (if (eq state '|<open paren><category><maybe semantics><content><close paren>|)
	     ;; After finishing each tag, add an entry in the annotation table....
	     (let ((text (get-parsed-text)))
	       (annotate (annotations-of sentence) text (car (get-parse-stack))))))
    ;;at this point we're in the |end| state...
    (setf (text-of sentence) (copy-seq (get-parsed-text))
	  (end-offset-of sentence) (file-position input))
    (add-sentence discourse sentence)
    sentence))

(defmethod user::demo ((demo (eql :scan)) &rest args)
  (let ((*discourse* (make-instance 'discourse-model
				    :quickname "scan-discourse"))
	)
    
    (with-input-from-string (str "(S (S1{?S1 rdf:isa natLex:weepEvent2 & ?S1 natLex:Weeper ?N1 & ?S1 natLex:expressedAs ...} (N1{?N1 natLex:hasFirstName ...} jesus) wept) (S2 blah))")
      (loop while (not (eq (peek-char t str nil :eof nil) :eof))
	 do (read-next-sentence *discourse* str)))
    *discourse*))


(defmethod user::demo ((demo (eql :sem-spec)) &rest args)
  (let ((*discourse* (user::demo :scan)))
    (let ((sentence (aref (sentences-of *discourse*) 0))
	  (triple-store-name "test.rdf")
	  (directory (ensure-directories-exist "C:/data/rdf/"))
	  )
      (if (not (triple-store-exists-p triple-store-name :directory directory))
	  (create-triple-store triple-store-name :directory directory))
      (with-triple-store (db triple-store-name)
	(interpret-semantic-spec *discourse* sentence (widest-span-of sentence))
	(print-triples (get-triples-list))))))


(defvar user::*log-stack* nil)
(defun read-natural-language-markup (stream c1 args)

  (setq user::*log-stack* nil)
  (when (not *discourse*)
      (error "Need to initialize *discourse*"))
  (when (not (db-path-of *discourse*))
    (error "*discourse* must have a db path"))
  (labels ((at-whitespace? ()
	     (let ((c (peek-char t stream nil :eof nil)))
	       (case c
		 ((#\Space #\Newline #\Tab #\Page #\null #\Newline)
		  t)
		 (t nil))))
	   (skip-whitespace ()
	     (loop while (at-whitespace?)
		do  (read-char stream t nil nil)))
	   (end-of-markup ()
	     (skip-whitespace)
	     (let ((c (peek-char t stream nil :eof nil)))
	       (or (eq c :eof)
		   (and (char= c #\]))))))
    ;; set up the discourse per target filem
    (if (not (triple-store-exists-p (db-path-of *discourse*)))
      (create-triple-store (db-path-of *discourse*))
      ;;else
      (open-triple-store (db-path-of *discourse*)))
    (with-triple-store (db (ensure-directories-exist (db-path-of *discourse*)))
      (loop while (not (end-of-markup))
	 for sentence = (read-next-sentence *discourse* stream)
	 do 
	   (interpret-semantic-spec *discourse* sentence (widest-span-of sentence))))
    (skip-whitespace)
    
    (when (char= #\] (peek-char t stream nil :eof nil))
      ;; if we encountered a quote at top level, read quote + pound sign, or flag a syntax error.
      (read-char stream t nil nil) 
      (when (not (char= #\# (peek-char t stream t nil nil)))
	(error "syntax error with read-natural language markup: expected a #"))
      (read-char stream t nil nil))))

  ;;(setq user::*just-kidding* (read stream t nil t)))

(defun enable-natural-language-markup ()
  (set-dispatch-macro-character #\# #\[ #'read-natural-language-markup))

(defun initialize-discourse (&key db-path uri quickname)
  (prog1
      (setq *discourse* (make-instance 'discourse-model
				       :uri uri
				       :quickname quickname
				       :db-path db-path
				       ))
    (register-namespace quickname uri)))
	

(defmethod user::demo ((demo (eql :read-nl-markup)) &rest args)
  (enable-natural-language-markup)
  (initialize-discourse :db-path "C:/Data/rdf/TESTDB")
  (read-from-string "#[
(S (N{?N rdf:isType Person & ?N rdf:FirstName Jesus} Jesus) wept) (S So did (N Mary))"))


(defmethod user::demo ((demo (eql :triple-store)) &rest args)
  (let ((db-file "C:/data/rdf/just-kidding"))
    
    (if (not (triple-store-exists-p (db-path-of *discourse*)))
      (create-triple-store db-file :if-exists :supersede)
      ;;else
      (open-triple-store db-file))


    (with-triple-store (db db-file)
      (add-triple (intern-resource "blah")
		  (intern-resource "blih")
		  (intern-resource "bleh")))

    (with-triple-store (db db-file)
      (add-triple (intern-resource "blah1")
		  (intern-resource "blih2")
		  (intern-resource "bleh3")))))





