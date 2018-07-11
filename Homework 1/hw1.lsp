; Finding the nth term in the Padovan series. It takes into input a number N and returns the number in the series at that position.
( defun PAD (N)
  (
   cond ((or (= N 0) (= N 1) (= N 2)) 1)
					; base case of the recursive function: We return 1 in case N has value 0, 1, or 2.
	(T (+ (PAD (- N 2)) (PAD (- N 3))))
					; recursive function to calculate the nth term. We adjust the formula by subtracting 1 to account for index 0.
	)
  )

; Finding the number of additions to get to a term in the Padovan series. Takes into input a number N and returns the number of additions required to get to that value from the Padovan series.
( defun SUMS (N)
  (
   cond ((or (= N 0) (= N 1) (= N 2)) 0)
					; base case of funtion: 0 additions to get to N=0,1 and 2.
    	(T (+ (SUMS (- N 2)) (SUMS (- N 3)) 1))
					; To calculate total additions, it is the sum of the additions made by the left and right side of the equation and 1 (current). This is done by a recursive function. We adjust the formula by subtracting 1 to account for index 0.
	)
  )


;Replace all values of a tree/atom with a ?. Takes into input a list (tree). We return the list with all elements replaced with a ?.
( defun ANON (TREE)
  (
   cond ((null TREE) '())
					;base case: if empty list, return empty list
	((atom TREE) '?)
					;base case: if only atom, replace that with a ?
	(T (cons (ANON(car TREE)) (ANON(cdr TREE))))
					; To replace them all, we seperate the first element, send it to the function, and combine it with the rest.
	)
  )
