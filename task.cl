#|
Для решения задачи был выбран язык Лисп, поскольку напрашивающееся решение задачи -- полный перебор всех возможных арифметических выражений.
Для генерации арифметических выражений гораздо удобнее использовать префиксную запись.
Кроме того, в Лиспе списки гетерогенны, и позволяют хранить в себе арифметическое выражение в польской записи прямо "из коробки".
Так же все эти выражения являются нативными вычислимыми выражениями Лиспа.
Поэтому, чтобы проверить, что выражение действительно равно указанному значению, достаточно вызвать функцию eval от него.
Всё это в совокупности избавляет от необходимости создавать кастомное внутреннее представление арифметических выражений.
|#


#|
соединяет левое поддерево left_child и правое поддерево right_child с помощью операции op.
При этом для операции возведения в степень стоят ограничения (из условия) на генерацию только возведения в квадрат.
Так же для операции деления стоит ограничение на создание выражений с делением на 0.
|#
(defun make_node_safe(op left_child right_child)
    (cond
        ((or (eq op `+)
             (eq op `-)
             (eq op `*)
             (and (eq op `/)
                  (/= 0 (eval right_child))
             )
             (and (eq op `expt) (atom right_child))
             ) (list op left_child right_child))
        (T     ())
    )
)

#|
генерирует список всех возможных выражений, полученных объединением
левого поддерева left_child и каждого элемента списка right_child_list в качестве правого поддерева.
Для каждых таких двух поддеревьев генерируется список выражений, полученный объединением поддеревьев с помощью всех возможных операций.
|#
(defun make_all_possible_nodes(left_child right_child_list)
    (cond
        ((null right_child_list) ())
        ((and (eq `2 left_child) (eq `2 (car right_child_list))) (append
                                                                `((+ 2 2) (- 2 2) (/ 2 2)) ; оптимизация для уменьшения числа перебираемых опций
                                                                (make_all_possible_nodes left_child (cdr right_child_list))
                                                            ))
        (T                       (append
                                     (mapcar #'(lambda(op) (make_node_safe op left_child (car right_child_list))) `(+ - * / expt))
                                     (make_all_possible_nodes left_child (cdr right_child_list))
                                 ))
    )
)

; как mapcar, но конкатенирует результаты вызова функционала в один список
; (вместо того, чтобы создавать список списков)
(defun mapcar_concat(F L)
    (cond
        ((null L) NIL)
        (T        (append (funcall F (car L)) (mapcar_concat F (cdr L))))
    )
)


#|
Используется для взаимной рекурсии с основной функцией gen_all_expressions
Содержит накапливающий параметр right_n_count -- количество двоек, которое можно использовать для генерации правого поддерева.
На каждом шаге генерирует все возможные правые поддеревья размера right_n_count, после чего объединяет их с left_sub_tree, получая new_left_child.
Для каждого new_left_child запускает дальнейшую генерацию выражений gen_all_expressions с новым левым поддеревом.
И ко всему этому добавляет свой же рекурсивный вызов с увеличенным накапливающим параметром right_n_count,
за счёт чего перебирает все возможные варианты размеров правых поддеревьев.
|#
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
генерирует список всех возможных арифметических выражений, каждое из которых:
* задано в виде вычислимого списка (АСД)
* начинается с левого поддерева left_sub_tree
* нехватает n_count двоек, которыми необходимо дополнить left_sub_tree до построенного выражения
|#
(defun gen_all_expressions(left_sub_tree n_count)
    (cond
        ((null left_sub_tree) (gen_all_expressions `2 (- n_count 1)))
        ((eq n_count 0)       (list left_sub_tree))
        ((< n_count 0)        ())
        (T                    (gen_all_expressions_internal left_sub_tree n_count 1))
    )
)

; генерирует арифметическое выражение в виде вычислимого списка, равное заданному значению value
(defun gen_expression(value)
    (find_expression value (append (gen_all_expressions `2 4) (gen_all_expressions `22 3)))
)

; в списке expr_list ищет выражение, равное value
(defun find_expression(value expr_list)
    (cond
        ((null expr_list)                      ())
        ((check_result value (car expr_list))  (car expr_list))
        (T                                     (find_expression value (cdr expr_list)))
    )
)

; debug only
; печатает список всех сгенерированных выражений
(defun print_all_expressions(expr_list)
    (cond
        ((null expr_list)                      ())
        (T                                     (append (print (car expr_list)) (print_all_expressions (cdr expr_list))))
    )
)

; проверяет, что ариметическое выражение expression равно value
(defun check_result(value expression)
    (eq value (eval expression))
)

; операция умножения в степень записана в нотации, интерпретируемой питоном
; т.е. "**" вместо "^"
; так просто удобнее, чтобы проверить, что распечатанные выражения в инфиксной записи корректны
(defun op_to_string(op)
    (cond
        ((eq op `expt) "**")
        (T             (write-to-string op))
    )
)

; функция, опускающие скобки в тривиальных случаях
(defun print_left_op(left_op op)
    (cond
        ((or (atom left_op)
             (eq `+ op)
             (eq `- op)
         )                   (print_infix_notation left_op))
        (T                   (concatenate 'string "(" (print_infix_notation left_op) ")"))
    )
)

; функция, опускающие скобки в тривиальных случаях
(defun print_right_op(right_op op)
    (cond
        ((or (atom right_op)
             (eq `+ op)
         )                   (print_infix_notation right_op))
        (T                   (concatenate 'string "(" (print_infix_notation right_op) ")"))
    )
)

; функция печати, переводящая выражение из польской записи в инфиксную
(defun print_infix_notation(expression)
    (cond
        ((null expression) "NULL EXPRESSION")
        ((atom expression) (write-to-string expression))
        (T                 (concatenate 'string
                                        (print_left_op (nth 1 expression) (car expression))
                                        (op_to_string (car expression))
                                        (print_right_op (nth 2 expression) (car expression))
                                        ))
    )
)

; генерирует выражение для заданного значения value, проверяет его и печатает результат
(defun test(value)
    (cond
        ((check_result value (gen_expression value)) (format T "PASSED: ~2d == ~30S | infix_notation: ~A ~%" value (gen_expression value) (print_infix_notation (gen_expression value))))
        (T                                           (format T "FAILED: ~2d != ~30S | infix_notation: ~A ~%" value (gen_expression value) (print_infix_notation (gen_expression value))))
    )
)

; запускает все тесты
; start_v -- первое значение, для которого нужно сгенерировать выражение
; max_v -- последнее значение, для которого нужно сгенерировать выражение
(defun all_tests(start_v max_v)
    (cond
        ((> start_v max_v) ())
        (T (append (test start_v) (all_tests (+ 1 start_v) max_v)))
    )
)

(all_tests 1 26)