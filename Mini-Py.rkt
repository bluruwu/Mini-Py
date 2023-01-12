#lang eopl

;;Juan Diego Arias Bahamon - 1842423
;;David Andres Moreno - 2226590
;;Juan Manuel Castillo Molina - 1941563
;;Brayan Stiven Acuña Vanegas 1940805
;;Victor Daniel Valencia Ñañez - 2026608

;;GITHUB: https://github.com/bluruwu/Mini-Py


;Especificación Léxica
(define scanner-spec-simple-interpreter
'(
  (white-sp (whitespace) skip)
  (comentario ("//" (arbno (not #\newline))) skip)
  (identificador (letter (arbno (or letter digit "?"))) symbol)
  (cadena (#\" any (arbno (not #\")) #\") string)
  (numero (digit (arbno digit)) number)
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    ;Expresiones basicas
    (programa ((arbno clase-declarada) expresion) a-programa)

    ;Identificadores
    (expresion (identificador) id-exp)

    ;Definir variables
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion) const-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion) letrec-exp)
    
    ;Datos
    (expresion (numero) numero-lit)
    (expresion (cadena) cadena-exp)
    (expresion (expr-bool) exp-bool)
    (expresion ("x8" "(" (arbno numero)")") oct-exp)
    (expresion ("x16" "("(arbno numero) ")" ) hex-exp)
    (expresion ("x32" "(" (arbno numero)")" ) bignum-exp)

    ;Constructores de datos predefinidos
    (expresion ("["(separated-list expresion ",")"]" ) list-exp)
    (expresion ("tupla" "[" (separated-list expresion ";") "]" ) tupla-exp)
    (expresion ("{" identificador "=" expresion (arbno "," identificador "=" expresion)"}" ) registro-exp)
    (expr-bool (pred-prim "(" expresion "," expresion ")") pred-prim-exp) ;DrRacket
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-bin-exp) ;DrRacket
    (expr-bool (oper-un-bool "("expr-bool")") oper-un-exp)
    (expr-bool (bool) bool-exp)
    (bool ("true") bool-true)
    (bool ("false") bool-false)
    
    ;Estructuras de control
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp) 
    (expresion ("if" expr-bool ":" expresion "else" ":" expresion "end" ) if-exp) ;python
    (expresion ("while" expr-bool ":" expresion "done") while-exp) ;python
    (expresion ("for" "(" identificador "=" expresion iterador expresion ")" "{" expresion "}") for-exp)
    (iterador ("to") iter-to)
    (iterador ("downto") iter-down)

    ;Primitivas aritmeticas para enteros
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    
    ;Primitivas sobre cadenas
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-binaria ("concat") primitiva-concat)

    ;Primitivas sobre listas
    (expresion ("vacio?" "(" expresion ")") vacio?-exp)
    (expresion ("vacio") vacio-exp)
    (expresion ("crear-lista" "(" expresion (arbno "," expresion) ")" ) crear-lista-exp)
    (expresion ("lista?" "(" expresion ")") list?-exp)
    (expresion ("cabeza" "(" expresion ")") cabeza-exp)
    (expresion ("cola" "(" expresion ")") cola-exp)
    (expresion ("append" "(" expresion "," expresion ")") append-exp)
    (expresion ("ref-list" "(" expresion "," expresion ")") ref-list-exp)
    (expresion ("set-list" "(" expresion "," expresion "," expresion ")") set-list-exp)
    
    ;Primitivas sobre tuplas 
    (expresion ("crear-tupla" "(" expresion (arbno "," expresion) ")" ) crear-tupla-exp)
    (expresion ("tupla?" "(" expresion ")") tupla?-exp)
    (expresion ("ref-tupla" "(" expresion "," expresion ")" ) ref-tupla-exp)
    (expresion ("cabeza-tupla""("expresion")") cabeza-tupla-exp)
    (expresion ("cola-tupla""(" expresion")")cola-tupla-exp)
    
    ;Primitivas sobre registros
    (expresion ("registro?" "(" expresion ")") registro?-exp)
    (expresion ("crear-registro" "(" identificador "=" expresion (arbno "," identificador "=" expresion) ")" ) crear-registro-exp)
    (expresion ("ref-registro" "(" expresion "," expresion")") ref-registro-exp)
    (expresion ("set-registro" "(" expresion "," expresion "," expresion")") set-registro-exp)

    ;Invocación de procedimientos
    (expresion ("function" "("(separated-list identificador",")")" "{" expresion "}") procedimiento-exp) ;javascript
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")") evaluar-exp) ;java
    (expresion ("&" identificador) referencia-exp)

    ;Variables actualizables
    (expresion ("set" identificador "=" expresion) set-exp)

    ;<pred-prim>
    (pred-prim (">") mayor-exp)
    (pred-prim (">=") mayor-igual-exp)
    (pred-prim ("<") menor-exp)
    (pred-prim ("<=") menor-igual-exp)
    (pred-prim ("==") igual-exp)
    (pred-prim ("!=") diferente-exp) ;c++

    ;<oper−bin−bool>
    (oper-bin-bool ("and") primitiva-and)
    (oper-bin-bool ("or") primitiva-or)
    
    ;<oper−un−bool>
    (oper-un-bool ("not") primitiva-not)
    
    ;enteros
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta) 
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("%") primitiva-mod) 
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    ;octales
    (primitiva-binaria ("+(x8)") oct-suma)
    (primitiva-binaria ("~(x8)") oct-resta)
    (primitiva-binaria ("*(x8)") oct-multi)
    (primitiva-unaria ("add1(x8)") oct-add1)
    (primitiva-unaria ("sub1(x8)") oct-sub1)
    
    ;hexadecimales
    (primitiva-binaria ("+(x16)") hex-suma)
    (primitiva-binaria ("~(x16)") hex-resta)
    (primitiva-binaria ("*(x16)") hex-multi)
    (primitiva-unaria ("add1(x16)") hex-add1)
    (primitiva-unaria ("sub1(x16)") hex-sub1)
    
    ;base 32
    (primitiva-binaria ("+(x32)") big-suma)
    (primitiva-binaria ("~(x32)") big-resta)
    (primitiva-binaria ("*(x32)") big-multi)
    (primitiva-unaria ("add1(x32)") big-add1)
    (primitiva-unaria ("sub1(x32)") big-sub1)
    
    ;Imprimir
    (expresion ("print" "(" expresion ")") print-exp)

    ;^;;;;;;;;;;;;;;; new productions for oop ;;;;;;;;;;;;;;;;

    (clase-declarada                         
      ("clase " identificador 
        "extends" identificador ":"
         (arbno "field" identificador)
         (arbno method-decl)
         )
      a-clase-declarada)

    (method-decl
      ("define" identificador 
        "("  (separated-list identificador ",") ")"
        expresion 
        )
      a-method-decl)
    (expresion ("mostrar") mostrar-exp)

    (expresion 
      ("new" identificador "(" (separated-list expresion ",") ")")
      new-object-exp)

    (expresion
      ("send" expresion identificador
        "("  (separated-list expresion ",") ")")
      method-app-exp)

    (expresion                                
      ("super" identificador    "("  (separated-list expresion ",") ")")
      super-call-exp)

    ))


;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
;;letrec @fact(@n) = Si @n entonces (@n * (@fact sub1(@n))) sino 1 finSi in (@fact 20)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))


;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program 
  (lambda (pgm)
    (cases programa pgm
      (a-programa (c-decls exp)
        (elaborate-class-decls! c-decls)
        (evaluar-expresion exp (empty-env))))))

;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (const-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**********************************Applies y Evaluar-expresion****************************************************

;eval-expression: <expression> <enviroment> -> numero | string
; evalua la expresión en el ambiente de entrada

(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      
      (mostrar-exp () the-class-env)

      ;Numeros
      (numero-lit (numero) numero)
      (oct-exp (numero) numero)
      (hex-exp (numero) numero)
      (bignum-exp (numero) numero)
      
      ;Texto
      (cadena-exp (cadena) (substring cadena 1 (- (string-length cadena) 1)))

      ;Identificadores
      (id-exp (id) (apply-env env id))

      (var-exp (vars rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body (extended-env-record vars (list->vector args) env))))

      (const-exp (ids rands body)
                (let ((args (map (lambda (x) (const-target (evaluar-expresion x env))) rands)))
                   (evaluar-expresion body (extended-env-record ids (list->vector args) env))))
      
      ;Referencia
      (referencia-exp (id) (apply-env-ref env id))
      
      ;Set
      (set-exp (id exp)(begin
                         (setref! (apply-env-ref env id)
                                  (evaluar-expresion exp env))
                        "Se ha actualizado la variable"))
      
      ;Primitivas unarias
      (primapp-un-exp (prim rand)
                   (apply-primitive-un prim (evaluar-expresion rand env)))
      
      ;Primitivas binarias
      (primapp-bin-exp (rand1 prim rand2)
                   (apply-primitive-bin prim (evaluar-expresion rand1 env) (evaluar-expresion rand2 env)))
      
      ;Primitivas booleanas
      (exp-bool (expr-bool)
                (eval-expr-bool expr-bool env))
      
      ;Begin
      (begin-exp (exp exps)
                 (let loop ((acc (evaluar-expresion exp env))
                            (exps exps))
                   (if (null? exps)
                       acc
                       (loop (evaluar-expresion (car exps) env)
                             (cdr exps)))))
      
      ;If
      (if-exp (expr-bool true-exp false-exp)
              (if (eval-expr-bool expr-bool env)
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env)))
      
      ;While
      (while-exp (expr-bool body)
                 (while expr-bool body env))
      
      ;for
      (for-exp (var value way x body ) (forfunction-verify var (evaluar-expresion value env) way (evaluar-expresion x env) body env)  )

      ;Listas
      (list-exp (list) (evaluar-lista list env))
      (crear-lista-exp (ca co)
                (cons (evaluar-expresion ca env) (evaluar-lista co env))
                )
      (vacio-exp () '())
      (vacio?-exp (list) (eqv? (evaluar-expresion list env) '()))
      (list?-exp (list) (list? (evaluar-expresion list env)))
      (cabeza-exp (list) (car (evaluar-expresion list env)) )
      (cola-exp (list) (cdr (evaluar-expresion list env)))
      (append-exp (list1 list2)
                 (append (evaluar-expresion list1 env) (evaluar-expresion list2 env)))
      (ref-list-exp (l p) (list-ref (evaluar-expresion l env) (evaluar-expresion p env)))
      (set-list-exp (l p exp)
                    (let
                        (
                         (le (evaluar-expresion l env))
                         (pe (evaluar-expresion p env))
                         (expe (cons(evaluar-expresion exp env) '()))
                         )
                      (append (append (head-to-position '() le pe 0) expe) (list-tail le (+ pe 1)))
                      ))

      ;Tuplas
      (tupla-exp (list) (list->vector (map (lambda (arg) (evaluar-expresion arg env)  ) list )))
      (crear-tupla-exp (head tail)
                       (list->vector (map (lambda (arg) (evaluar-expresion arg env)  ) (cons head tail)))
                       )
      (tupla?-exp (body) (vector? (evaluar-expresion body env)))
      (ref-tupla-exp (tupla index)
                     (vector-ref (evaluar-expresion tupla env) (evaluar-expresion index env)))
      (cabeza-tupla-exp (tupla)(car (vector->list (evaluar-expresion tupla env))))
      (cola-tupla-exp (tupla) (list->vector (cdr (vector->list (evaluar-expresion tupla env)))))

      ;Registros
      (registro-exp (id exp ids exps)
                (list (cons id ids) (evaluar-lista (cons exp exps) env))
                )
      (registro?-exp (reg) ((list-of list?) (evaluar-expresion reg env)))
      (crear-registro-exp (id exp ids exps) (list (cons id ids) (evaluar-lista (cons exp exps) env)))
      (ref-registro-exp (lis reg)
                        (cases expresion reg
                          (id-exp (x)
                                  (let (
                                        (list (evaluar-expresion lis env))
                                        )
                                   (list-ref (car(cdr list)) (pos-registro (car list) x 0))                                    
                                   )
                                  )
                          (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                         )
               )
      (set-registro-exp (lis reg exp)
                        (cases expresion reg
                          (id-exp (x)
                                  (let* (
                                        (le (evaluar-expresion lis env))
                                        (expe (cons(evaluar-expresion exp env) '()))
                                        (pe (pos-registro (car le) x 0))
                                        (listval (car(cdr le)))
                                        )
                                     (cons (car le) (cons (append (append (head-to-position '() listval pe 0) expe) (list-tail listval (+ pe 1)))'()))
                                   ))
                          (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                         ))

      ;procedimientos
      
      (procedimiento-exp (ids body)
                (closure ids body env))
      
      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      
      (evaluar-exp (rator rands)
               (let ((proc (evaluar-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procval proc args)
                     (eopl:error 'evaluar-expresion
                                 "Attempt to apply non-procedure ~s" proc))))
      
      ;Imprimir
      (print-exp (txt) (display (evaluar-expresion txt env)) (newline))

       ;
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (evaluar-expresion obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))

      (else (eopl:error 'invalid-register "No es un indice de registro valido"))
      )))
      

(define eval-rands
  (lambda (exps env)
    (map
      (lambda (exp) (evaluar-expresion exp env))
      exps)))

;apply-primitive-bin: <expresion> <primitiva> <expresion> -> numero
(define apply-primitive-bin
  (lambda (prim args1 args2)
    (cases primitiva-binaria prim

      ;decimales
      (primitiva-suma () (+ args1 args2))     
      (primitiva-resta () (- args1 args2))
      (primitiva-multi () (* args1 args2))
      (primitiva-div () (/ args1 args2))
      (primitiva-mod () (modulo args1 args2))
      (primitiva-concat () (string-append args1 args2))

      ;octales
      (oct-suma () (suma-base args1 args2 8) )
      (oct-resta () (resta-base args1 args2 8) )
      (oct-multi () (multi-base args1 args2 8) )
          
      ;hexadecimales
      (hex-suma () (suma-base args1 args2 16) )
      (hex-resta () (resta-base args1 args2 16) )
      (hex-multi () (multi-base args1 args2 16) )
      
      ;base 32
      (big-suma () (suma-base args1 args2 32) )
      (big-resta () (resta-base args1 args2 32) )
      (big-multi () (multi-base args1 args2 32) )
      )))

;apply-primitive-un: <primitiva> <expresion> -> numero
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (primitiva-longitud () (string-length args))
      (oct-add1 () (sucesor-base args 8) )
      (oct-sub1 () (predecesor-base args 8) )
      (hex-add1 () (sucesor-base args 16) )
      (hex-sub1 () (predecesor-base args 16) )
      (big-add1 () (sucesor-base args 32) )
      (big-sub1 () (predecesor-base args 32) )
      )))

;apply-pred-prim: <primitiva>
(define apply-pred-prim
  (lambda (prim args1 args2)
    (cases pred-prim prim
      (menor-exp () (< args1 args2))
      (mayor-exp () (> args1 args2))
      (menor-igual-exp () (<= args1 args2))
      (mayor-igual-exp () (>= args1 args2))
      (igual-exp () (eqv? args1 args2))
      (diferente-exp () (not (eqv? args1 args2))))))

;apply-oper-bin-bool: <primitiva>
(define apply-oper-bin-bool
  (lambda (prim args1 args2)
    (cases oper-bin-bool prim
      (primitiva-and () (and args1 args2))
      (primitiva-or () (or args1 args2)))))

;apply-oper-un-bool: <primitiva>
(define apply-oper-un-bool
  (lambda (prim args1)
    (cases oper-un-bool prim
      (primitiva-not () (not args1)))))


;***********************************************numeros no decimales*************************
;operaciones aritmeticas para numeros no decimales

(define sucesor-base
  (lambda (num base)
    (if(null? num)
      '(1)
      (if (< (car num) (- base 1 ))
           (cons (+ 1 (car num)) (cdr num)  )
          (cons 0 (sucesor-base(cdr num) base ) )
          ))))

(define predecesor-base
  (lambda (num base)
    (if(null? num)
       (eopl:error "limite alcanzado")
       (if (> (car num) 0)
           (if (and (eq? (- (car num) 1) 0) (null? (cdr num)))
               '()
               (cons (- (car num) 1) (cdr num)))
           (cons (- base 1) (predecesor-base (cdr num) base)))
           )))

(define suma-base
 (lambda (elem1 elem2 base)
   (if(null? elem2)
      elem1
      (suma-base (sucesor-base elem1 base) (predecesor-base elem2 base) base))))


(define resta-base
 (lambda (elem1 elem2 base)
   (if (null? elem2)
       elem1
       (predecesor-base (resta-base elem1 (predecesor-base elem2 base) base) base))))

(define multi-base
 (lambda (elem1 elem2 base)
   (if (null? elem2 )
       elem1
       (suma-base elem1 (multi-base elem1 (predecesor-base elem2 base) base) base))))


;(define init-env 
 ;  (extend-env
  ;    '(i v x)
   ;   '(1 5 10)
    ;  (empty-env))))

;^;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;WHILE
(define while
      (lambda (expr-bool body env)
        (if (eval-expr-bool expr-bool env)
            (begin (evaluar-expresion body env)
                   (while expr-bool body env))
             'done)))

;for
(define forfunction-verify
  (lambda (var val way x body env)
    (cases iterador way 
      (iter-to () (forfunction-up var val x body env) )
      (iter-down () (forfunction-down var val x body env) ))
  ))

(define forfunction-up
  (lambda (var val x body env)    
    (if
     (< val x)
     (begin
       (evaluar-expresion body env)
       (forfunction-up var (+ 1 val) x body env )
       )
     'done-for)
  ))

(define forfunction-down
  (lambda (var val x body env)
    (if
     (> val x)
     (begin
       (evaluar-expresion body env)
       (forfunction-down var (- val 1) x body env )

       )
     1)
  ))

;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;funcion auxiliar para listas
(define evaluar-lista
  (lambda (list env)
    (if (null? list)
        '()
        (cons(evaluar-expresion (car list) env) (evaluar-lista (cdr list) env))
        )
    ))

(define head-to-position
  (lambda (list2 list position counter)
    (if (eqv? position counter)
        (reverse list2)
        (head-to-position (cons (car list) list2) (cdr list) position (+ counter 1))))
  )

;funcion auxiliar para registros
(define pos-registro
  (lambda (lis reg counter)
    (if (= counter (+ (length lis) 1))
        (eopl:error 'out-of-register "No existe el registro ~s" reg)
        (if (eqv? reg (car lis))
            counter
            (pos-registro (cdr lis) reg (+ counter 1))
         )
     )
   )
 )

;Booleanos
(define eval-expr-bool
  (lambda (exp env)
    (cases expr-bool exp
      (pred-prim-exp (prim args1 args2)
                     (apply-pred-prim prim
                                      (evaluar-expresion args1 env)
                                      (evaluar-expresion args2 env)))
      (oper-bin-exp  (prim args1 args2)
                     (apply-oper-bin-bool prim
                                          (eval-expr-bool args1 env)
                                          (eval-expr-bool args2 env)))
      (oper-un-exp (prim args1)
                   (apply-oper-un-bool prim
                                       (eval-expr-bool args1 env)))   
      (bool-exp (arg)
                (cases bool arg
                  (bool-true () #t)
                  (bool-false () #f))))))


;****************************************Blancos y Referencias **********************************************************************

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (string? x) (list? x) (vector? x))))
;
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (const-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref 
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val)))
    ))

;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;

(define clase-declarada->class-name
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        class-name))))

(define clase-declarada->super-name
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        super-name))))

(define clase-declarada->field-ids
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        field-ids))))

(define clase-declarada->method-decls
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
        
;^;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expresion?)
    (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (evaluar-expresion body (extend-env ids args env))))))
               

;^;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))


;; evaluar
(define aux
   (lambda (x)
     x))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (clase-declarada->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (clase-declarada->class-name c-decl)
      (make-vector (length (clase-declarada->field-ids c-decl))))))

;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (evaluar-expresion body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))
      
;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (clase-declarada->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (clase-declarada->field-ids (part->clase-declarada part))))

(define part->clase-declarada
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (clase-declarada->method-decls (part->clase-declarada part))))

(define part->super-name
  (lambda (part)
    (clase-declarada->super-name (part->clase-declarada part))))

(define class-name->method-decls
  (lambda (class-name)
    (clase-declarada->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (clase-declarada->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

;;

(interpretador)

;******************************************* Pruebas ****************************************************
;Enteros
(scan&parse "7")


;Flotantes
(scan&parse "4.4")

;Numerosen base 8
(scan&parse "x8(4 5 6)")
;numerosen base 16
(scan&parse "x16(15 4 13)")
;numeros en base 32
(scan&parse "x32( 31 24 2 14)")
;Cadena
(scan&parse "\"hola\"")

;Boleanos
(scan&parse "true")
(scan&parse "false")

;Identificador
(scan&parse "p")
(scan&parse "q")

;Referencia
(scan&parse "&p")

;Variable
(scan&parse "var x = 6 in add1(x)")

;Actualización de variable
(scan&parse "var l = 7 in begin set l = 10; l end")

;procedimientos
(scan&parse "function(a, b, c) {((a + b)*c)}")

;invocación de procedimientos

;por valor
(scan&parse "var p = function(r) {add1(r)} in var f = 2 in evaluar p(f)")
(scan&parse "var p = function(r) {set-list(r,0,15)} in var f = [1,2,3] in evaluar p(f)")

(scan&parse "var p = function(r) {set r = 6} in var f = 2 in begin evaluar p(f); f end")

;por referencia
(scan&parse "var p = function(r) {set r = 6} in var f = 2 in begin evaluar p(&f); f end")

;listas
(scan&parse "crear-lista(5,4,3)") ;crear lista
(scan&parse "var y = crear-lista(1,2,3) in set-list(y,1,8)") ;set list
(scan&parse "lista? (crear-lista(5,4,3))") ;lista? 
(scan&parse "cabeza (crear-lista(5,4,3))") ;cabeza
(scan&parse "cola (crear-lista(5,4,3))") ;cola
(scan&parse "append ([5,7,9], [9,4,10])") ;append
(scan&parse "ref-list([0,7,15],2)") ;ref
;tupla
(scan&parse "crear-tupla(2,3,4,5,6)")
(scan&parse "tupla?(crear-tupla(2,3,4,5,6))")
(scan&parse "ref-tupla(crear-tupla(2,3,4,5,6),3)")
(scan&parse "cabeza-tupla(tupla[2,3,4,5])")
(scan&parse "cabeza-tupla(crear-tupla(2,3,4,5))")
(scan&parse "cola-tupla(crear-tupla(2,3,4,5))")
(scan&parse "cola-tupla(tupla[2,3,4,5])")


;registros
(scan&parse "crear-registro(a = 5, b = 6)") ;crear registro
(scan&parse "registro?(crear-registro(g=8,h=6))") ;registro?
(scan&parse "ref-registro({f = 7, g = 5}, g)") ;ref registro
(scan&parse "set-registro({f = 7, g = 5},f, 1)") ;set registro


;;;;estructuras de control
(scan&parse "if <(3,5) : (6+7) else : (7~6) end")
(scan&parse "while true :print (true) done ")
(scan&parse "for(hw = 5 to 10 ){ print(5) }")
;operaciones aritmeticas
(scan&parse "(1 + 1)")
(scan&parse "(1 ~ 2)")
(scan&parse "(1 * 2)")
(scan&parse "(1 / 2)")
(scan&parse "(4 % 2)")
(scan&parse "(x8(3) +(x8) x8(4))")
(scan&parse "(x16(15 5) +(x16) x16(10 3))")
(scan&parse "(x32(23) +(x32) x32(4))")
(scan&parse "(x8(3) *(x8) x8(4))")
(scan&parse "(x16(15) *(x16) x16(1))")
(scan&parse "(x32(14 3) *(x32) x32(24))")
(scan&parse "(x8(7) ~(x8) x8(4))")
(scan&parse "(x16(15) ~(x16) x16(15))")
(scan&parse "(x32(31) ~(x32) x32(3))")
;primitivas unarias
(scan&parse "sub1(4)")
(scan&parse "add1(2)")
(scan&parse "add1(x8)(x8(2))")
(scan&parse "add1(x16)(x16(14))")
(scan&parse "add1(x16)(x16(0 1))")
;primitivas sobre cadenas
(scan&parse "sub1(4)")
;invocacion de procedimientos
(scan&parse "function (a,b){(a+b)}")
;funciones booleanas
(scan&parse "> (3,5)")
(scan&parse "< (4,2)")
(scan&parse "<= (6,8)")
(scan&parse ">= (7,9)")
(scan&parse "== (7,7)")
(scan&parse "!= (2,0)")
(scan&parse "or(true,false)")
(scan&parse "and(true,true)")
(scan&parse "not(true)")
;funcion print
(scan&parse "print(true)")

;ejemplos objetos 
(scan&parse "
clase c1 extends object:
  field x
  field y
  define initialize()
    begin
      set x = 1;
      set y = 2
    end

  define m1() x
  define m2() y

  var
    o1 = new c1()
  in

  send o1 m1()
")


(scan&parse "
clase c1 extends object:
  field x
  field y
  define initialize()
    begin
      set x = 1;
      set y = 2
    end

  define m1() x
  define m2() y

clase c2 extends c1:
  field x
  field y
  define initialize()
    begin
      super initialize();
      set x = 2;
      set y = 3
    end

  define m1() x

  var
    o1 = new c1(),
    o2 = new c2()
  in

  send o2 m2()
")

(scan&parse "
clase c1 extends object:
  field x
  field y
  define initialize()
    begin
      set x = 1;
      set y = 2
    end

  define m1() x
  define m2() y

clase c2 extends c1:
  field x
  field y
  define initialize()
    begin
      super initialize();
      set x = 2;
      set y = 3
    end

  define m1() x

  var
    o1 = new c1(),
    o2 = new c2()
  in

  send o2 m2()
")


(scan&parse "
clase c1 extends object:
  field x
  field y
  define initialize()
    begin
      set x = 1;
      set y = 2
    end
  
  define m1() x
  define m2() send self m1()

clase c2 extends c1:
  field x
  field y
  define initialize()
    begin
      super initialize();
      set x = 9;
      set y = 10
    end

  define m1() x

var
  o1 = new c1(),
  o2 = new c2()
in

send o2 m2()
")
