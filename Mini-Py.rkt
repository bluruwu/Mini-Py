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
    (programa (expresion) un-programa)

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
    (expresion ("append" "(" expresion "," expresion ")") append-exp)
    (expresion ("ref-list" "(" expresion "," expresion ")") ref-list-exp)
    (expresion ("set-list" "(" expresion "," expresion "," expresion ")") set-list-exp)

    ;Primitivas sobre tuplas 
    (expresion ("crear-tupla" "(" expresion (arbno "," expresion) ")" ) crear-tupla-exp)
    (expresion ("tupla?" "(" expresion ")") tupla?-exp)
    (expresion ("ref-tupla" "(" expresion "," expresion ")" ) ref-tupla-exp)
    
    ;Primitivas sobre registros
    (expresion ("registro?" "(" expresion ")") registro?-exp)
    (expresion ("crear-registro" "(" identificador "=" expresion (arbno "," identificador "=" expresion) ")" ) crear-registro-exp)
    (expresion ("ref-registro" "(" expresion "," expresion")") ref-registro-exp)
    (expresion ("set-registro" "(" expresion "," expresion "," expresion")") set-registro-exp)

    ;Invocación de procedimientos
    ;CAMBIAR FORMA DE PROCEDIMIENTO Y EVALUAR, PONERLO PARECIDO A ALGUN LENGUAJE CONOCIDO, EJ: C++
    ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (expresion ("procedimiento" "("(separated-list identificador",")")" "haga" expresion "finProc") procedimiento-exp)
    (expresion ("evaluar" expresion "("(separated-list expresion ",")")" "finEval") app-exp)
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
                 (evaluar-expresion body init-env)))))


;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (const-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**********************************Apply y Evaluar-expresion****************************************************

;eval-expression: <expression> <enviroment> -> numero | string
; evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (numero) numero)
      (cadena-exp (cadena) (substring cadena 1 (- (string-length cadena) 1)))
      (id-exp (id) (apply-env env id))    
      (var-exp (vars rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body (extended-env-record vars (list->vector args) env))))
      (const-exp (ids rands body)
                (let ((args (map (lambda (x) (const-target (evaluar-expresion x env))) rands)))
                   (evaluar-expresion body (extended-env-record ids (list->vector args) env))))
      (referencia-exp (id) (apply-env-ref env id))
      (set-exp (id exp)(begin
                         (setref! (apply-env-ref env id)
                                  (evaluar-expresion exp env))
                        "Se ha actualizado la variable"))
      (primapp-un-exp (prim rand)
                   (apply-primitive-un prim (evaluar-expresion rand env)))
      (primapp-bin-exp (rand1 prim rand2)
                   (apply-primitive-bin prim (evaluar-expresion rand1 env) (evaluar-expresion rand2 env))
                   )
      (exp-bool (expr-bool)
                (eval-expr-bool expr-bool env))
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
                                   )
                                  )
                          (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                         )
               )
      (crear-tupla-exp (head tail)
                       (list->vector (map (lambda (arg) (evaluar-expresion arg env)  ) (cons head tail)))
                       )
      (tupla?-exp (body) (vector? (evaluar-expresion body env)))
      (ref-tupla-exp (tupla index)
                     (vector-ref (evaluar-expresion tupla env) (evaluar-expresion index env)))
      
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
      (primitiva-longitud () (string-length args)))))

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
    (cases expresion rand
      (referencia-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (const-target (expval) ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion rand env))))))

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
    (or (number? x) (procVal? x) (string? x) (list? x) (vector? x))))
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
;
(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (const-target (expval) expval)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (const-target (expval) expval)
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
                (const-target (expval1) (eopl:error "No se puede cambiar el valor de una variable CONST"))
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

; Ambiente inicial
(define init-env  
  (extended-env-record (list '@x '@y '@z '@a)
              (list->vector
                (list (direct-target 4)
                      (direct-target 2)
                      (direct-target 5)
                      (indirect-target (a-ref 0 (list->vector (list (direct-target 4)))))))
              (empty-env)))
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
   (body expresion?)
   (amb environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (evaluar-expresion body (extended-env-record ids (list->vector args) env))))))

(interpretador)
    
