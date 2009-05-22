; recursive lambda with label
(apply 
 (label f 
  (lambda 'n 'acc
   '(if (< n '2)
       '(return acc)
       '(applytc f (- n '1) (* acc n))
    )
  )
 )
 '5 '1
 )
