
(define function_pass (lambda (f x y) (f x y)))
function_pass

(function_pass  + 1 2)  ; problem at 217 - ish