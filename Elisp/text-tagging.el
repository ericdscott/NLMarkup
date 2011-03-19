(defvar text-tagging-hook nil "*Hook for text-tagging mode")
(defvar text-tagging-mode-map nil "Keymap for text-tagging major mode")
(defvar text-tagging-syntax-table nil "Syntax table for text-tagging major mode")
(defvar top-level-tags nil "A list of symbols expressing tags with no parent")
(defvar tag-index 0 "Used to assign unique name to the next tag")

;;each tag has fields for 
;; :parent
;; :children
;; :left-marker
;; :right-marker

(defun text-tagging-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'text-tagging-mode)
  (setq text-tagging-mode-map (make-sparse-keymap))
  (set-keymap-parent text-tagging-mode-map lisp-mode-map)
  (define-key text-tagging-mode-map "\C-tn" 'tag-N)
  (define-key text-tagging-mode-map "\C-ts" 'tag-S)
  (define-key text-tagging-mode-map "\C-t." 'seek-next-unmarked-sentence-boundary)
  (define-key text-tagging-mode-map "\C-tu" 'tag-unmarked-sentence)
  (define-key text-tagging-mode-map "\C-tp" (lambda ()
					      (interactive)
					      (save-excursion
						(text-tagging-mode))))
  (define-key text-tagging-mode-map "\C-\M-tn" 'tag-N)
  (define-key text-tagging-mode-map "\C-\M-ts" 'tag-S)
  (use-local-map text-tagging-mode-map)
  (setq mode-name "Text Tagging")
  (copy-face 'default 'current-tag-overlay-face)
  (set-face-attribute 'current-tag-overlay-face nil :foreground "red")
  (copy-face 'default 'current-tag-children-overlay-face)
  (set-face-attribute 'current-tag-children-overlay-face nil :foreground "green")
  (copy-face 'default 'current-tag-semantic-overlay-face)
  (set-face-attribute 'current-tag-semantic-overlay-face nil :foreground "blue")
  (parse-tagged-text-buffer)
  (transient-mark-mode 1) ;;highlight regions.
  (add-hook 'post-command-hook 'update-overlays)
  (run-hooks 'text-tagging-hook)
)

(defun old-reset-text-tagging ()
  (interactive)
  (setq tag-index 0)
  ;;clear all the top-level tags.
  (while top-level-tags
    (let ((current-tag (car top-level-tags))
	  )
      (let ((clear-marker (function (lambda (field)
				      (let ((marker (get current-tag field)))
					(if marker (set-marker marker nil)))
				      (put current-tag field nil)))))
      (put current-tag :parent nil)
      (put current-tag :children nil)
      (funcall clear-marker :left-marker)
      (funcall clear-marker :right-marker)
      (setq top-level-tags (cdr top-level-tags))))))
		  
      
(defun reset-text-tagging ()
  (interactive)
  (labels ((clear-tags (siblings)
		       (while siblings
			 (let ((current-tag (car siblings))
			       )			 
			   (let ((clear-marker (function (lambda (tag field)
							   (let ((marker (get current-tag field)))
							     (if marker (set-marker marker nil)))
							   (put current-tag field nil)))))
			     (put current-tag :parent nil)
			     (let ((children (get current-tag :children)))
			       (if children
				   (clear-tags children)))
			     (put current-tag :children nil)
			     (funcall clear-marker current-tag :left-marker)
			     (funcall clear-marker current-tag :right-marker)
			     (setq siblings (cdr siblings)))))))
	   
    (clear-tags top-level-tags)
    (setq top-level-tags nil)
    ))
	   

(defun end-of-marked-text ()
  (if (null top-level-tags)
      0
    (+ 1 (marker-position (get (car (last top-level-tags)) :right-marker)))))


(defun seek-end-of-marked-text ()
  (interactive)
  (goto-char (end-of-marked-text)))

(defun seek-next-unmarked-sentence-boundary()
  (interactive)
  (seek-end-of-marked-text)
  (skip-syntax-forward "^\\.")
  (forward-char))


(defun add-marker (position)
  (let ((marker (make-marker)))
    (set-marker marker position)
    marker))

(defun next-tag-symbol-name ()
  (let ((response (concat "tag" (prin1-to-string (setq tag-index (+ tag-index 1))))))
    (unintern response)
    response))

(defun acquire-tag-instance (parent left right)
  "Creates the tag, and installs it in place."
  (let ((tag-instance (intern (next-tag-symbol-name)))
	)
    (if parent
	(let ()
	  (put parent :children
	       (append (get parent :children) (list tag-instance)))
	  (put tag-instance :parent parent))
      ;;else there is no parent, this is top-level
      (setq top-level-tags (append top-level-tags (list tag-instance))))
    
    (let ((left-marker (add-marker left))
	  (right-marker (add-marker right)))
      (put tag-instance :left-marker left-marker)
      (put tag-instance :right-marker right-marker))
    tag-instance))

(defun declare-tag (tag-label parent region-beginning region-end)
  "Acquires the tag instance and inserts parens and a label"
  (let ((tag-instance (acquire-tag-instance parent region-beginning region-end))
	)
      (goto-char region-beginning)
      (insert (concat "(" tag-label " "))
      (goto-char (get tag-instance :right-marker))
      (insert ")")
      (goto-char (get tag-instance :left-marker))
      tag-instance))


(defun tag-unmarked-sentence ()
  "Declares an S tag between the end of marked text and the point."
  (interactive)
  (let ((sentence-position (point)))
    (seek-end-of-marked-text)
    (skip-syntax-forward " ")
    (let ((new-tag (declare-tag "S" nil (point) sentence-position)))
      (setq end-of-marked-text (+ 1 (get new-tag :right-marker))))))



(defconst whitespace-syntax (char-syntax ?\ ))
;;(defconst open-paren-syntax (char-syntax ?\( ))
;;(defconst close-paren-syntax (char-syntax ?\) ))


(defvar tag-level nil "Reflects the level of nesting during a scan.")


;;DELTA TABLE FOR SCANNING
  ;;O := open paren
  ;;C := close paren
  ;;T := Tag Label
  ;;B := open brace
  ;;b := close brace
  ;;_ := white space
  ;;X := everything else
(defun |start| () ;;should be looking at beginning of new tag or unmarked
  "scan state. looking at beginning of a tag, or unmarked text"

  (if (null (char-after))
      '|end|
    ;;else
    (let ((s (char-syntax (char-after))))
      (cond
       ;; skip whitespace
       ((= s whitespace-syntax) ;; whitespace
	(forward-char)
	'|start|)
       ;; check for comments
       ((= (char-after) ?\;)
	'|commenting|)
       ;; go to ( on paren go down a level
       ((= s ?\() ;;open paren
	(setq tag-level (+ 1 tag-level))
	(forward-char)
	'|O|)
       ;; end of marked-text  otherwise
       (t ;; some other syntax
	(if (> tag-level 0)
	    (error "unbalanced parentheses"))
	;;(forward-char)
	'|end|)))		      
    ))
(defun |commenting| ()
  (cond 
  ((null (char-after))
   '|end|)
  ((eq (char-after) ?\n)
   (forward-char)
   '|start|)
  (t
   (forward-char)
   '|commenting|)))

(defun |O| ()
  "Scan state. Just matched open paren"
  ;; there should be a tag that we read until whitespace.
  (if (or (= (char-after) ?N)
	  (= (char-after) ?S))
      (progn
	(forward-char)
	'|OT+|)
    ;;else
    (error "expecting tag label S or N"))
  )
(defun |OT+| ()
  "Scan state. Have just read at least one character of a tag."
  (let ((s (char-syntax (char-after))))
    (cond
     ((= (char-after) ?\{)
      (forward-char)
      '|OTB|)
     ((= s whitespace-syntax)
      (skip-syntax-forward " ")
      '|OT_|)
     (t
      (forward-char)
      '|OT+|)))
  )

(defun |OTB| ()
  "Scan state. Just matched up to ( + tag + { and then zero or more chars other than }"
  (cond
   ((= (char-after) ?\})
    (forward-char)
    '|OTBb|)
   (t 
    (forward-char)
    '|OTB|)))

(defun |OTBb| ()
  "Scan state. Just read ( + tag + { ... }"
  (let ((s (char-syntax (char-after)))
	)
    (cond 
     ((= s whitespace-syntax)
      (skip-syntax-forward " ")
      '|OT_|)
     ((= s ?\( ) ;;we encountered the open paren directly with no space. Pretend there was...
      '|OT_|)
     (t
      (forward-char)
      '|OT_x|))))

(defun |OT_| ()
  "Scan state. Just matched open paren + tag + space"
  (let ((s (char-after)) ;;(char-syntax (char-after)))
	)
    (cond
     ;; if we see an (, go to start
     ((= s ?\()
      '|start|)
     ;;open brace
     ((= s ?\{)
      (forward-char)
      '|OTB|)
     ;; otherwise go to OT_x
     (t
      (forward-char)
      '|OT_x|))))
		  
(defun |OT_x| ()
  "Scan state. Just matched open paren + tag + space + content"
  (let ((s (char-after)) ;;(char-syntax (char-after)))
	)
    (cond
     ;; if we see (, go to start
     ((= s ?\()
      '|start|)
     ;; if we see ), go to OT_xC
     ((= s ?\))
      (forward-char)
      '|OT_xC|)
     ;; otherwise go to OT_x
     (t
      (forward-char)
      '|OT_x|))))

(defun |OT_xC| ()
  "Scan state. Just matched open paren + tag + space + content + close paren"
  ;; go up a level and go to OT_x if level > 0, else go to start
  (setq tag-level (- tag-level 1))
  (if (> tag-level 0)
      '|OT_x|
    '|start|))
;;END OF DELTA TABLE FOR SCANNING;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-tagged-text-buffer ()
  "Scan the entire text of a buffer. Typically called when entering text-tagging mode on a newly loaded file. Establishes tags for each top-level entry, but does not parse them fully. We put that off until a given tag becomes current."
  (interactive)
  (reset-text-tagging)
  (setq tag-level 0)
  (beginning-of-buffer)
  (search-forward "#[")
  (let ((current-state '|start|)
	(top-level-left -1)
	(top-level-right -1)
	(safety 0))
    (while (or (eq current-state '|start|) 
	       (eq current-state '|O|)
	       (eq current-state '|OT+|)
	       (eq current-state '|OTB|)
	       (eq current-state '|OTBb|)
	       (eq current-state '|OT_|)
	       (eq current-state '|OT_x|)
	       (eq current-state '|OT_xC|)
	       (eq current-state '|commenting|)
	       )
      (cond
       ((eq current-state '|start|)
	(if (= tag-level 0)
	    (setq top-level-left (point))))
       ((eq current-state '|OT_xC|)
	(if (= tag-level 1) ;; just finished off a level-one tag, about to return to level 0
	    (let ()
	      (setq top-level-right (point))
	      (acquire-tag-instance nil top-level-left top-level-right)
	      ))))
	  
      (setq current-state (funcall (symbol-function current-state)))
      ;(print current-state)
      (setq safety (+ safety 1))
      (if (> safety 1000000) (error "engaged safety brake after 100000"))
      )
    (setq end-of-marked-text (point))))

		  
(defun fully-parse (tag)
  "Fully parses a tag and assigns children to it."
  (if (null tag)
      (error "null tag in fully-parse"))
  (save-excursion
	(setq tag-level (calc-tag-level tag))
	(goto-char (get tag :left-marker))

	(let ((target-level (+ 2 tag-level))
	      (current-state '|start|)
	      (children nil)
	      (child-left -1)
	      (child-right -1)
	      )
	  ;; reiteratively call state transitions and respond appropriately to each state...
	  (while (and (not (eq current-state '|end|))
		      (<= (point) (get tag :right-marker)))
	    (cond
	     ((eq current-state '|O|)
	      (if (= tag-level target-level)
		  (let ()
		    (setq child-left (- (point) 1)))))
	     ((eq current-state '|OT_xC|)
	      (if (eq tag-level target-level)
		  (let (child)
		    (setq child-right (point))
		    (setq child (acquire-tag-instance tag child-left child-right))
		    (push child children)))))
	    (setq current-state (funcall (symbol-function current-state))))
	  (while children
	    (fully-parse (car children))
	    (setq children (cdr children))
	    )
	  tag)))

(defun get-semantic-boundaries (tag)
  (setq gsb-test nil)
  (if (null tag)
      (error "null tag in get-semantic-boundaries"))
  (save-excursion
	(goto-char (get tag :left-marker))
	(let (
	      (current-state '|start|)
	      (left-semantic-boundary nil)
	      (right-semantic-boundary nil)
	      (has-read-tag? nil)
	      )
	  ;; reiteratively call state transitions and respond appropriately to each state...
	  (while (and (not (eq current-state '|end|))
		      (not (and (eq current-state '|O|) has-read-tag?))
		      (<= (point) (get tag :right-marker))
		      (not (and left-semantic-boundary
				right-semantic-boundary)))
	    (push current-state gsb-test)
	    (cond
	     ((eq current-state '|OT+|)
	      (setq has-read-tag? t))
	     ((and (not left-semantic-boundary)
		   (eq current-state '|OTB|))
		   
	      (setq left-semantic-boundary (- (point) 1)))

	     ((eq current-state '|OTBb|)
	      (setq right-semantic-boundary (point))))

	    (setq current-state (funcall (symbol-function current-state))))
	  ;;return...
	  (and left-semantic-boundary
	       right-semantic-boundary
	       (list :left-semantic-boundary left-semantic-boundary
		     :right-semantic-boundary right-semantic-boundary)))))
  
(defun calc-tag-level (tag)
  "Returns the level of nesting associated with TAG. Typically called when scanning an arbitrary tag."
  (let ((count 0)
	(subject tag))
    (while subject
      (let ((parent (get subject :parent)))
	(if parent (setq count (+ 1 count)))
	(setq subject parent)))
    count))

(defun tag-contains-position? (tag position)
  "True when TAG strattles POSITION"
  (and (>= position (marker-position (get tag :left-marker)))
       (< position (marker-position (get tag :right-marker)))))

(defun position-belongs-to-tag? (position tag)
  "True when TAG contains POSITION, but none of its children do."
  (and (tag-contains-position? tag position)
       (let ((assumption t)
	     (children (get tag :children)))
	 (while (and children assumption)
	   (if (tag-contains-position? (car children) position)
	       (setq assumption nil))
	   (setq children (cdr children)))
	 assumption)))

(defun associated-tag-of-position (position candidates)
  "Returns the tag amongst CANDIDATES or children of CANDIDATES such that (position-belongs-to POSITION <said tag>)."
  (if candidates
      (let ((found nil))
	(while (and candidates (not found))
	  (if (tag-contains-position? (car candidates) position)
	      (setq found (car candidates)))
	  (setq candidates (cdr candidates)))
	(let ((better-candidate (associated-tag-of-position position (get found :children))))
	  (or better-candidate
	  found)))))
	  
(defun tag-N ()
  (interactive)
  (declare-tag "N" (associated-tag-of-position (region-beginning) top-level-tags) (region-beginning) (region-end)))

(defun tag-S ()
  (interactive)
  (declare-tag "S" (associated-tag-of-position (region-beginning) top-level-tags) (region-beginning) (region-end)))

	   
(defvar current-tag-overlay nil)
(defun acquire-current-tag-overlay (tag)
  (if (and current-tag-overlay
	   (overlay-matches-tag? current-tag-overlay tag))
      current-tag-overlay
    ;;else the current overlay is no longer current.
    (progn()
	  (if current-tag-overlay
	      (delete-overlay current-tag-overlay))
	  (prog1
	      (setq current-tag-overlay
		    (make-overlay (get tag :left-marker)
				  (get tag :right-marker)))
	    (overlay-put current-tag-overlay 'face 'current-tag-overlay-face)
	    (overlay-put current-tag-overlay 'priority 1)))))


(defvar current-tag-semantic-overlay nil)
(defun semantic-overlay-matches-tag? (overlay tag)
  (let ((boundaries (get-semantic-boundaries tag)))
    (and boundaries
	 overlay
	 (= (overlay-start overlay) (getf boundaries :left-semantic-boundary))
	 (= (overlay-end overlay) (getf boundaries :right-semantic-boundary)))))


(defun acquire-current-tag-semantic-overlay (tag)
  (if (and current-tag-semantic-overlay
	   (semantic-overlay-matches-tag? current-tag-semantic-overlay tag))
      current-tag-semantic-overlay
    ;;else we're no longer current
    (let ()
      (if current-tag-semantic-overlay
	  (delete-overlay current-tag-semantic-overlay))
      (let ((semantic-boundaries (get-semantic-boundaries tag)))
	(setq actso-test semantic-boundaries)
	(if semantic-boundaries
	    (prog1
		(setq current-tag-semantic-overlay
		      (make-overlay (getf semantic-boundaries :left-semantic-boundary)
				    (getf semantic-boundaries  :right-semantic-boundary)))
	      (overlay-put current-tag-semantic-overlay 'face 'current-tag-semantic-overlay-face)
	      (overlay-put current-tag-semantic-overlay 'priority 2))
	  ;;else no braces for this tag...
	  (setq current-tag-semantic-overlay (make-overlay 0 0)))))))


(defun overlay-matches-tag? (overlay tag)
  "Typically called when we want to know whether an overlay is still valid."
  (and overlay
       (overlay-start overlay)
       (overlay-end overlay)
       (= (overlay-start overlay) (get tag :left-marker))
       (= (overlay-end overlay) (get tag :right-marker))))

(defvar current-child-tag-overlays nil)
(defun acquire-current-child-tag-overlays (tag)
  "Returns the child overlays of the children of the current TAG, and removes any overlays acquired in previous calls."
  ;;TODO do nothing if tag is unchanged from last call.
  (if current-child-tag-overlays
      (let ((overlays current-child-tag-overlays))
	(while overlays
	  (delete-overlay (car overlays))
	  (setq overlays (cdr overlays)))))
  (setq current-child-tag-overlays (mapcar (lambda (child-tag)
					     (let ((overlay
						    (make-overlay (get child-tag :left-marker)
								  (get child-tag :right-marker))))
					       (overlay-put overlay 'face 'current-tag-children-overlay-face)
					       (overlay-put overlay 'priority 2)
					       overlay))
					   (setq test-children (get tag :children)))))

(defvar current-top-level-tag nil "The top-level tag that is currently in focus.")
(defun acquire-top-level-tag-for-position (position)
  (if (and current-top-level-tag
	   (get current-top-level-tag :left-marker)
	   (get current-top-level-tag :right-marker)
	   (tag-contains-position? current-top-level-tag position))
      current-top-level-tag
    ;;else the position is not within the current top-level tag...
    (let ((to-search top-level-tags)
	  (found nil))
      ;;TODO: consider (genocide current-top-level-tag) if we have too many markers floating around.
      (while (and to-search (not found))
	(if (tag-contains-position? (car to-search) position)
	     (setq found (car to-search)))
	(setq to-search (cdr to-search)))
      (setq current-top-level-tag found))))

(defun overlay-current-tag (position)
  "Acquires overlays for the tag associated with the current position and its children"
  (let ((target-tag (associated-tag-of-position position top-level-tags))
	)
    (acquire-current-tag-overlay target-tag)
    (acquire-current-tag-semantic-overlay target-tag)
    (acquire-current-child-tag-overlays target-tag)))
    

  
(defun update-overlays ()
  (if (eq major-mode 'text-tagging-mode)
      (let ((top-level-tag (acquire-top-level-tag-for-position (point))))
	(if top-level-tag
	    (let ()
	      (fully-parse top-level-tag)
	      (overlay-current-tag (point)))))))

(defun play ()
  (interactive)
  (setq test (get-semantic-boundaries (associated-tag-of-position (point) top-level-tags))))

(defvar test-overlay nil)
(defun overlay-play ()
  (interactive)
  (copy-face 'default 'test-face)
  (set-face-attribute 'test-face nil :foreground "red")
  (setq test-overlay (make-overlay 1 10))
  (overlay-put test-overlay 'face 'test-face)
  (overlay-current-tag 10))


(provide 'text-tagging)