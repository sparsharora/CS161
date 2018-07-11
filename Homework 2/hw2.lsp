; @param FRINGE		A representation of a tree defined as either an atom or a list of subtrees.
;
; @returns A list of leaves visited in a BFS traversal of the tree.
(defun BFS (FRINGE)
    (cond
		; If its just a leaf, return the leaf
        ((atom FRINGE) FRINGE)

		; If no tree, return nil
        ((null FRINGE) nil)

		; If the first subtree is a leaf, visit it
        ((atom (car FRINGE))
            (cons (car FRINGE) (BFS (cdr FRINGE)))
        )

		; Get subtree's subtrees, and append to our list to process later (queue like processing for BFS)
        (t
            (BFS (append (cdr FRINGE) (car FRINGE)))
        )
    
    )
)


;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S) (equal S '(t t t t)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (cond
		; Move homer, baby not with dog and baby not with poison
		((and (equal A 'h) (not (equal (cadr S) (caddr S))) (not (equal (cadr S) (cadddr S))))
			(if (equal nil (car S)) 
				(list (cons t (cdr S)))		; Move to West
				(list (cons nil (cdr S)))	; Move to East
			)
		)

		; Move homer & baby
		((and (equal A 'b) (equal (car S) (cadr S)))
			(if (equal nil (car S)) 
				(list (cons t (cons t (cddr S))))		; Move to West
				(list (cons nil (cons nil (cddr S))))	; Move to East
			)		
		)

		; Move homer & dog, baby not with poison
		((and (equal A 'd) (equal (car S) (caddr S)) (not (equal (cadr S) (cadddr S))))
			(if (equal nil (car S))
				(list (cons t (cons (cadr S) (cons t (cdddr S)) ) ))	; Move to West
				(list (cons nil (cons (cadr S) (cons nil (cdddr S)))))	; else part
			)
		)

		; Move homer and poison, baby not with dog
		((and (equal A 'p) (equal (car S) (cadddr S)) (not (equal (cadr S) (caddr S))))
			(if (equal nil (car S))
				(list (list t (cadr S) (caddr S) t))		; Move to West
				(list (list nil (cadr S) (caddr S) nil))	; else part
		
			)
		
		)
		(t nil)
	)
)


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
	; Try every possible move and amass into list
	(append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	; Standard linear recursion through STATES list to see if S is in STATES
	(cond
		((null STATES) nil)
		((equal (car STATES) S) t)
		(t (ON-PATH S (cdr STATES)))
	)
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond
		((null STATES) nil)
		(t
			; Expand each new state using DFS; Return if successful, attempt other states otherwise
			(if (equal (DFS (car STATES) PATH) nil) (MULT-DFS (cdr STATES) PATH) (DFS (car STATES) PATH))
		)
	)
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to nil. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or nil otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
	(cond
		((FINAL-STATE S) (append PATH (list S)))
		((ON-PATH S PATH) nil)
		(t
			; If neither final state nor repeated state, expand it and explore
			(MULT-DFS (SUCC-FN S) (append PATH (list S)))
		)
	)
)
    
