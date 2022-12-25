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
  (identificador ("@" letter (arbno (or letter digit))) symbol)
  (cadena (letter (arbno (or letter digit #\- #\:))) string)
  (cadena ( "-" letter (arbno (or letter digit #\- #\:))) string)
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
    (programa (expresion) un-programa)
    
    ;Identificadores
    (expresion (identificador) id-exp)
    
    ;Definir variables
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion) const-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion) letrec-exp)
    
    ;Datos
    (expresion (numero) numero-lit)
    (expresion ("\""cadena "\"") cadena-exp)
    (expresion (expr-bool) exp-bool)

    ;Constructores de datos predefinidos
    (expresion ("["(separated-list expresion ";")"]" ) list-exp)
    (expresion ("tupla" "[" (separated-list expresion ";") "]" ) tupla-exp)
    (expresion ("{" identificador "=" expresion (arbno ";" identificador "=" expresion)"}" ) registro-exp)
    (expr-bool (pred-prim "(" expresion "," expresion ")") pred-prim-exp) ;DrRacket
    (expr-bool ("true") bool-lit-true)
    (expr-bool ("false") bool-lit-false)
    ;faltan mas boleanos


    ;Estructuras de control
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("if" expr-bool "then" expresion "else" expresion ) if-exp)
    (expresion ("while" expr-bool "do" expresion) while-exp)
    (expresion ("for" identificador "=" expresion iterador expresion "do" "{" expresion "}" "done") for-exp)
    (iterador ("to") iter-to)
    (iterador ("downto") iter-down)
    
    ;Primitivas aritmeticas para enteros
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    
    ;Primitivas aritmeticas para hexodecimales [FALTA]
    ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    ;FALTA APPEND
    ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (expresion ("ref-lis" "(" expresion "," expresion ")") ref-list-exp)
    (expresion ("set-lis" "(" expresion "," expresion "," expresion ")") set-list-exp)

    ;Primitivas sobre tuplas 
    (expresion ("crear-tupla" "(" expresion (arbno "," expresion) ")" ) crear-tupla-exp)
    (expresion ("tupla?" "(" expresion ")") tupla?-exp)
    (expresion ("ref-tupla" "(" expresion "," expresion ")" ) ref-tupla-exp)
    ;ES POSIBLE QUE TOQUE CREAR VACIO, CABEZA Y COLA PROPIOS PARA TUPLAS. DEJAR ESTE MEN POR SI ALGO
    ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ;Primitivas sobre registros
    (expresion ("registro?" "(" expresion ")") registro?-exp)
    (expresion ("crear-registro" "(" identificador "=" expresion (arbno "," identificador "=" expresion) ")" ) crear-registro-exp)
    (expresion ("ref-register" "(" expresion "," expresion")") ref-registro-exp)
    (expresion ("set-register" "(" expresion "," expresion "," expresion")") set-registro-exp)

    ;Invocación de procedimientos
    ;CAMBIAR FORMA DE PROCEDIMIENTO Y EVALUAR, PONERLO PARECIDO A ALGUN LENGUAJE CONOCIDO, EJ: C++
    ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (expresion ("procedimiento" "("(separated-list identificador",")")" "haga" expresion "finProc") procedimiento-exp)
    (expresion ("evaluar" expresion "("(separated-list expresion ",")")" "finEval") app-exp)
        
    ;no se si sea necesario hacer un recproc, por favor VERIFICAR.
    ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
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
    (oper−bin−boo ("and") primitiva-and)
    (oper−bin−boo ("or") primitiva-or)
    
    ;<oper−un−bool>
    (oper−un−bool ("not") primitiva-not)
    
    ;enteros
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta) 
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("%") primitiva-mod) 
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    ;FALTA HEXADECIMALES
    ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ;Imprimir
    (expresion ("print" "(" expresion ")") print-exp)

    
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
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**********************************Apply y Evaluar-expresion****************************************************

;FALTA TODO EL EVAL EXPRESION!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;eval-expression: <expression> <enviroment> -> numero | string
; evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (datum) datum)
      (cadena-exp (cad) cad)
      (id-exp (id) (apply-env env id))
      (var-exp (vars rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body (extend-env vars args env))))
      (primapp-un-exp (prim rand)
                   (apply-primitive-un prim (evaluar-expresion rand env)))
      (primapp-bin-exp (rand1 prim rand2)
                   (apply-primitive-bin prim (evaluar-expresion rand1 env) (evaluar-expresion rand2 env))
                   );exp-bool
      (else (eopl:error 'invalid-register "No es un indice de registro valido"))
      )))


;apply-primitive-bin: <expresion> <primitiva> <expresion> -> numero
(define apply-primitive-bin
  (lambda (prim args1 args2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ args1 args2))
      (primitiva-resta () (- args1 args2))
      (primitiva-multi () (* args1 args2))
      (primitiva-div () (/ args1 args2))
      (primitiva-mod () (modulo args1 args2))
      (primitiva-concat () (string-append args1 args2)))))
       ;aqui iria append
    

;apply-primitive-un: <primitiva> <expresion> -> numero
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (primitiva-longitud () (length args)))))

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


;****************************************Blancos y Referencias **********************************************************************

(define expval?
  (lambda (x)
    (or (number? x) (procVal? x))))
;
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))
;
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
;
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


;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
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
                                 
;; función para probar booleanos
(define valor-verdad?
  (lambda(x)
    (not (zero? x))))
    
;;;;funciones auxiliares para la expresion declare

;;eval-rands evalua los operandos y los convierte en un ambiente
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))

;Booleanos
(define eval-expr-bool
  (lambda (exp env)
    (cases expr-bool exp
      (pred-prim-exp (prim args1 args2)(apply-pred-prim prim (evaluar-expresion args1 env) (evaluar-expresion args2 env)))
      (bool-lit-true () 'true)  
      (bool-lit-false () 'false))))

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

(interpretador)
    
