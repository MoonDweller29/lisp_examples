;gnu clisp  2.49.60



(defun gen_list5()
    (append `(+) (list `(+ 1 2) `2))
)

(defun gen_list6()
    (append `(+) (list `(+ 1 2) `3))
)

(defun gen_all_expressions(value)
    (list (gen_list5))
)

(defun gen_expression(value)
    (find_expression value (gen_all_expressions value))
)

(defun find_expression(value expr_list)
    (cond
        ((null expr_list)                      ())
        ((check_result value (car expr_list))  (car expr_list))
        (T                                     (find_expression value (cdr expr_list)))
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