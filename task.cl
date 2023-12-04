;gnu clisp  2.49.60



(defun gen_list5()
    (append `(+) (list `(+ 1 2) `2))
)

(defun gen_list6()
    (append `(+) (list `(+ 1 2) `3))
)

#|
returns list of all possible arithmetic expressions
which start from left_sub_tree and have n_count extra operands
|#
(defun gen_all_expressions(left_sub_tree n_count)
    (cond
        ((null left_sub_tree) (gen_all_expressions `2 (- n_count 1)))
        ((eq n_count 0)       (list left_sub_tree))
        (T                    (gen_all_expressions (append `(+) (list left_sub_tree `3)) (- n_count 1)))
    )
)

(defun gen_expression(value)
    (find_expression value (append (gen_all_expressions `2 4) (gen_all_expressions `22 3)))
)

(defun find_expression(value expr_list)
    (cond
        ((null expr_list)                      ())
        ((check_result value (car expr_list))  (car expr_list))
        (T                                     (find_expression value (cdr expr_list)))
    )
)

(defun print_all_expressions(expr_list)
    (cond
        ((null expr_list)                      ())
        (T                                     (append (print (car expr_list) (print_all_expressions (cdr expr_list)))))
    )
)

(defun check_result(value expression)
    (eq value (eval expression))
)

(defun test(value)
    (cond
        ((check_result value (gen_expression value)) (format T "PASSED: ~d == ~S ~%" value (gen_expression value)))
        (T                                           (format T "FAILED: ~d != ~S ~%" value (gen_expression value)))
    )
)

(defun all_tests(start_v max_v)
    (cond
        ((> start_v max_v) ())
        (T (append (test start_v) (all_tests (+ 1 start_v) max_v)))
    )
)

#|(print (check_result 5 (gen_list5)))
(print (find_expression 6 (list (gen_list5) (gen_list6))))|#
(all_tests 0 5)
#|(print (append `(+) (list `(+ 1 2) `2)))|#
(print "print_all_expressions")
(print_all_expressions (gen_all_expressions () 2))
(print (gen_all_expressions `2 3))
(print (append (gen_all_expressions `2 4) (gen_all_expressions `22 3)))
(print (list `(1 2)))