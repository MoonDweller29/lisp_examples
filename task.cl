;gnu clisp  2.49.60

(defun gen_list5()
    (append `(+) (list `(+ 1 2) `2))
)

(defun gen_list12()
    (list `+ `(expt 2 3) `4)
)

(defun make_node_safe(op left_child right_child)
    (cond
        ((or (eq op `+)
             (eq op `-)
             (eq op `*)
             (eq op `expt)
             (and (eq op `/)
                  (/= 0 (eval right_child)))) (list op left_child right_child))
        (T                                    ())
         
    )
)

(defun make_all_possible_nodes(left_child right_child_list)
    (cond
        ((null right_child_list) ())
        (T                       (append
                                     (mapcar #'(lambda(op) (make_node_safe op left_child (car right_child_list))) `(+ - * / expt))
                                     (make_all_possible_nodes left_child (cdr right_child_list))
                                 ))
    )
)

(defun mapcar_concat(F L)
    (cond
        ((null L) NIL)
        (T        (append (funcall F (car L)) (mapcar_concat F (cdr L))))
    )
)


(defun gen_all_expressions_internal(left_sub_tree n_count right_n_count)
    (cond
        ((> right_n_count n_count) ())
        (T                         (append
                                       (mapcar_concat
                                           #'(lambda(new_left_child) (gen_all_expressions new_left_child (- n_count right_n_count)))
                                           (make_all_possible_nodes left_sub_tree (gen_all_expressions `2 (- right_n_count 1)))
                                       )
                                       (gen_all_expressions_internal left_sub_tree n_count (+ right_n_count 1))
                                   ))
    )
)

#|
returns list of all possible arithmetic expressions
which start from left_sub_tree and have n_count extra operands
|#
(defun gen_all_expressions(left_sub_tree n_count)
    (cond
        ((null left_sub_tree) (gen_all_expressions `2 (- n_count 1)))
        ((eq n_count 0)       (list left_sub_tree))
        ((< n_count 0)        ())
        (T                    (gen_all_expressions_internal left_sub_tree n_count 1))
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
        (T                                     (append (print (car expr_list)) (print_all_expressions (cdr expr_list))))
    )
)

(defun check_result(value expression)
    (eq value (eval expression))
)

(defun op_to_string(op)
    (cond
        ((eq op `expt) "**")
        (T             (write-to-string op))
    )
)

(defun print_infix_notation(expression)
    (cond
        ((null expression) "NULL EXPRESSION")
        ((atom expression) (write-to-string expression))
        (T                 (concatenate 'string
                                        "(" (print_infix_notation (nth 1 expression)) ")"
                                        (op_to_string (car expression))
                                        "(" (print_infix_notation (nth 2 expression)) ")"
                                        ))
    )
)

(defun test(value)
    (cond
        ((check_result value (gen_expression value)) (format T "PASSED: ~2d == ~30S | infix_notation: ~A ~%" value (gen_expression value) (print_infix_notation (gen_expression value))))
        (T                                           (format T "FAILED: ~2d != ~30S | infix_notation: ~A ~%" value (gen_expression value) (print_infix_notation (gen_expression value))))
    )
)

(defun all_tests(start_v max_v)
    (cond
        ((> start_v max_v) ())
        (T (append (test start_v) (all_tests (+ 1 start_v) max_v)))
    )
)

#|(print (check_result 5 (gen_list5)))
(print (find_expression 6 (list (gen_list5) (gen_list12))))|#
(all_tests 0 26)
#|(print (append `(+) (list `(+ 1 2) `2)))|#
(print "print_all_expressions")
#|(print_all_expressions (gen_all_expressions `2 2))|#
#|(print "====================")
(print (append (gen_all_expressions `2 4) (gen_all_expressions `22 3)))
(print (list `(1 2)))
(print (print_infix_notation `(+ 1 2)))
(print (print_infix_notation (gen_list12)))
(print (eval (gen_list12)))|#
#|(print `(+ - * / expt)) all operations |#