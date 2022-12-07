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
  (identificador (letter (arbno (or letter digit))) symbol)
  (texto ((arbno letter)) string)
  (texto (letter (arbno (or letter digit #\- #\:))) string)
  (texto ( "-" letter (arbno (or letter digit #\- #\:))) string)
  (numero (digit (arbno digit)) number)
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    (programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion (identificador) var-exp)
    (expresion ("\""texto"\"") texto-lit)
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)

    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) let-exp)
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion) const-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion) letrec-exp)

    (expresion ("true") bool-lit-true)
    (expresion ("false") bool-litt-false)
    
    (expresion ("["(separated-list expresion ",")"]" ) list-exp)
    (expresion ("registro" "[" identificador ":" expresion (arbno "," identificador ":" expresion)"]" ) registro-exp)
    (expresion ("tupla" "(" (separated-list expresion ",") ")" ) tupla-exp)

    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)
    (expresion ("if" expresion "then" expresion "else" expresion ) if-exp)
    (expresion ("while" expresion "do" expresion) while-exp)
    (expresion ("for" identificador "=" expresion iterador expresion "do" expresion) for-exp)
    (expresion (iterador expresion "do" expresion "done") do-exp)
    
    (expresion ("vacio?" "(" expresion ")") vacio?-exp)
    (expresion ("vacio") vacio-exp)
    (expresion ("crear-lista" "(" expresion (arbno "," expresion) ")" ) crear-lista-exp)
    (expresion ("lista?" "(" expresion ")") list?-exp)
    (expresion ("cabeza" "(" expresion ")") cabeza-exp)
    (expresion ("cola" "(" expresion ")") cola-exp)
    ;FALTA APPEND 
    (expresion ("ref-lis" "(" expresion "," expresion ")") ref-list-exp)
    (expresion ("set-lis" "(" expresion "," expresion "," expresion ")") set-list-exp)
  
    (expresion ("crear-tupla" "(" expresion (arbno "," expresion) ")" ) crear-tupla-exp)
    (expresion ("tupla?" "(" expresion ")") tupla?-exp)
    (expresion ("ref-tupla" "(" expresion "," expresion ")" ) ref-tupla-exp)
    
    (expresion ("registro?" "(" expresion ")") registro?-exp)
    (expresion ("crear-registro" "(" identificador "=" expresion (arbno "," identificador "=" expresion) ")" ) crear-registro-exp)
    (expresion ("ref-register" "(" expresion "," expresion")") ref-registro-exp)
    (expresion ("set-register" "(" expresion "," expresion "," expresion")") set-registro-exp)

    ;cambiar forma de procedimiento y evaluar, ponerlo parecido a algun lenguaje conocido, ej: c++
    (expresion ("procedimiento" "("(separated-list identificador",")")" "haga" expresion "finProc") procedimiento-exp)
    (expresion ("evaluar" expresion "("(separated-list expresion ",")")" "finEval") app-exp)
        
    ;no se si sea necesario hacer un recproc, por favor VERIFICAR.


    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("==") primitiva-igual)
    (primitiva-binaria ("<>") primitiva-no-es-igual)
    
    (primitiva-binaria ("and") primitiva-and)
    (primitiva-binaria ("or") primitiva-or)
    
    (primitiva-unaria ("not") primitiva-not)
    
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta) 
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("%") primitiva-mod) 
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)


    ;HEXADECIMALES FALTA
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-binaria ("concat") primitiva-concat)
    
    (iterador ("to") iter-to)
    (iterador ("downto") iter-down)
    
   )
  )
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

(define show-the-datatypes
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
      (un-programa (body)
                 (evaluar-expresion body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

;FALTA TODO EL EVAL EXPRESION!!!!!!!!!!!!!!!!!!!!!!!!!!!



;eval-expression: <expression> <enviroment> -> numero | string
; evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (datum) datum)
      (texto-lit (txt) txt)
      (var-exp (id) (apply-env env id))
      (primapp-un-exp (prim rand)
                   (apply-primitive-un prim (evaluar-expresion rand env)))
      (primapp-bin-exp (rand1 prim rand2)
                   (apply-primitive-bin prim (evaluar-expresion rand1 env) (evaluar-expresion rand2 env))
                   )
      (if-exp (test-exp true-exp false-exp)
              (if (valor-verdad? (evaluar-expresion test-exp env))
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env)))                   
   ;   (variableLocal-exp (ids rands body)
   ;            (let ((args (eval-let-exp-rands rands env)))
    ;             (evaluar-expresion body
   ;                               (extend-env ids args env))))
      (procedimiento-exp (ids body)
                (cerradura ids body env))
      (app-exp (rator rands)
               (let ((proc (evaluar-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (evaluar-expresion rhs-exp env))
                 1))
      (begin-exp (exp exps)
                 (let loop ((acc (evaluar-expresion exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (evaluar-expresion (car exps) 
                                               env)
                              (cdr exps)))))
      (else (eopl:error 'invalid-register "No es un indice de registro valido"))
      )))


;apply-primitive-bin: <primitiva> <expresion> <expresion> -> numero | string
(define apply-primitive-bin
  (lambda (prim args1 args2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ args1 args2))
      (primitiva-resta () (- args1 args2))
      (primitiva-multi () (* args1 args2))
      (primitiva-div () (/ args1 args2))
      (primitiva-concat () (string-append args1 args2))
      (else (eopl:error 'invalid-register "No es un indice de registro valido"))
      )))

;apply-primitive-un: <primitiva> <expresion> <expresion> -> numero
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length args))
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (else (eopl:error 'invalid-register "No es un indice de registro valido"))
      )))

;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procVal? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;******************************************** Ambientes *******************************************
;****Gramatica*******
;<env-exp> ::= (empty-env)
;          ::= (extend-env <list-of symbol>
;                          <list-of scheme-value> <env-exp>)

;Representación
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (cerradura ids body env))))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))
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
                                 
;; función para probar booleanos
(define valor-verdad?
  (lambda(x)
    (not (zero? x))))
    
;;;;funciones auxiliares para la expresion declare

;;eval-rands evalua los operandos y los convierte en un ambiente
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))
;;eval-rand ingresa el operando y lo llama para evaluar la expresion
(define eval-rand
  (lambda (rand env)
    (cases expresion rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (evaluar-expresion x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (evaluar-expresion rand env))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

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

;******************************************** Procedimientos *******************************************
(define-datatype procVal procVal?
  (cerradura
   (lista-ID(list-of symbol?))
   (exp expresion?)
   (amb environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (evaluar-expresion body (extend-env ids args env))))))

;(interpretador)
    
