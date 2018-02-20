;;------------------------------------------------------------------
(defun isPalindrome1 (lst)
      (equal lst (revList lst))
)

(defun isPalindrome (lst)
  (cond ((null lst) t)
        ((equal lst (revList lst)) t)
        (t nil)
   )
)

(defun revList (lst)
  (cond ((null lst) lst)
        (t (append (revList (cdr lst)) (list (car lst))))
   )
)

;;--------------------------------------------------------------------

(defun count1 (x lst)
  (cond ((null lst) 0)
        ((equal x (car lst)) (+ 1 (count1 x (cdr lst))))
        (t (count1 x (cdr lst)))
   )
)

;;--------------------------------------------------------------------

(defun count2 (lst1 lst2)
  (cond ((null lst2) 0)
        ((equal lst1 (car lst2)) (+ 1 (count2 lst1 (cdr lst2))))
        (t (count2 lst1 (cdr lst2)))
  )
)

;;--------------------------------------------------------------------

(defun doRemove (x lst)
  (cond ((null lst) nil)
        ((equal x (car lst)) (doRemove x (cdr lst)))
        (t (cons (car lst) (doRemove x (cdr lst))))
   )
)

;;--------------------------------------------------------------------

(defun doReplace (x1 x2 lst)
  (cond ((null lst) nil)
        ((equal x1 (car lst)) (cons x2 (doReplace x1 x2 (cdr lst))))
        (t (cons (car lst) (doReplace x1 x2 (cdr lst))))
   )
)

;;--------------------------------------------------------------------

(defun is_sorted (lst)
  (cond((null lst) nil)
       ((equal 0 (count_inversions lst)) t)
       (t nil)
   )
)

(defun count_inversions(lst)

  (cond((<(length lst) 2) 0)

       ((> (car lst) (car(cdr lst))) 
        (+ 1 (count_inversions (cdr lst)) ) )

       (t (count_inversions (cdr lst)))
   )
)

;;-------------------------------------------------------------------

(defun hasdups1 (lst)
  (cond ((null lst) nil)
        ((equal (car lst) (car(cdr lst))) t)
        (t (hasdups1 (cdr lst)))
   )
)

;;-------------------------------------------------------------------

(defun hasdups2 (lst)
  (cond ((null lst) nil)
        ((> (do_find (car lst) (cdr lst)) 0) t)
        (t (hasdups2 (cdr lst)))
   )
)

(defun do_find(x lst)
  (cond((null lst) 0)
       ((equal x (car lst)) (+ 1 (do_find x (cdr lst))))
       (t (do_find x (cdr lst)))
   )
)

;;--------------------------------------------------------------------

(defun remove_dups (lst)
  (cond ((null lst) nil)
        ((cons (car lst) (remove_dups (eliminater (car lst) (cdr lst))) ))
   )
)

(defun eliminater (x lst)
  (cond ((null lst) nil)
        ((equal x (car lst)) (eliminater x (cdr lst)))
        (t (cons (car lst) (eliminater x (cdr lst))))
   )
)

;;--------------------------------------------------------------------

(defun sorter(lst)
   (append (removeB lst) (removeA lst))
)

(defun removeA(lst)
  (cond
       ((null lst) ())
       ((equal 'a (car lst)) (removeA (cdr lst)))
       (t (cons (car lst) (removeA (cdr lst))))
  )
)

(defun removeB(lst)
  (cond
       ((null lst) ())
       ((equal 'b (car lst)) (removeB (cdr lst)))
       (t (cons (car lst) (removeB (cdr lst))))
  )
)

;;----------------------------------------------------------------------

(sorter '(b b a b a b a a a b))




