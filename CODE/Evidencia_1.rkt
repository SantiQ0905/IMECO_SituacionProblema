#lang racket
;; automata.rkt — Lee un archivo DSL, verifica léxico/sintaxis,
;; simula el autómata y genera un HTML formateado.

(require racket/string)

;; ——————— Esquema de tokens ———————
(define token-specs
  (list
   (cons 'whitespace  #\"^[ \\t\\r\\n]+\")
   (cons 'comment     #\"^//.*\")
   (cons 'keyword     #\"^(alfabeto|estados|inicio|aceptaci[oó]n|transiciones|en|cadenas)\\\\b\")
   (cons 'arrow       #\"^->\")
   (cons 'colon       #\"^:\")
   (cons 'comma       #\"^,\")
   (cons 'literal     #\"^'(?:[^'\\\\]|\\\\.)'\")
   (cons 'string      #\"^\\\"[^\\\"]*\\\"\")
   (cons 'identifier  #\"^[A-Za-z][A-Za-z0-9_]*\")))

;; Tokenizador
(define (tokenize src)
  (let loop ([s src] [out '()])
    (cond
      [(string-empty? s) (reverse out)]
      [else
       (let find ([specs token-specs])
         (when (null? specs)
           (error "Error léxico en:" (substring s 0 10)))
         (define tk (car specs))
         (define m  (regexp-match (cdr tk) s))
         (if m
             (let ([lex (first m)]
                   [typ (car tk)])
               (define rest (substring s (string-length lex)))
               (define out2
                 (if (member typ '(whitespace comment))
                     out
                     (cons (cons typ lex) out)))
               (loop rest out2))
             (find (cdr specs)))))])))

;; Parser simple por secciones
(struct automata (alpha states init finals trans tests))

(define (parse tokens)
  (define idx 0)
  (define n   (length tokens))
  (define (peek) (and (< idx n) (list-ref tokens idx)))
  (define (next) (set! idx (add1 idx)) (peek))
  (define vals (make-hash))

  ;; inicializar secciones (¡aquí estaba el bracket faltante!)
  (for ([sec '(alfabeto estados inicio aceptación transiciones cadenas)])
    (hash-set! vals (symbol->string sec) '()))

  (let parse-sections ()
    (when (< idx n)
      (match (peek)
        [(list 'keyword kw)
         (set! idx (add1 idx))
         (unless (and (< idx n) (equal? (second (peek)) ":"))
           (error "Se esperaba ':' después de" kw))
         (set! idx (add1 idx))
         (define items '())

         (cond
           [(regexp-match? #px"^cadenas$" kw)
            (let collect-strings ()
              (when (< idx n)
                (match (peek)
                  [(list 'string s)
                   (set! items (cons s items))
                   (set! idx (add1 idx))
                   (when (and (< idx n) (equal? (first (peek)) ","))
                     (set! idx (add1 idx)))
                   (collect-strings)]
                  [_ #f])))]
           [(regexp-match? #px"^transiciones$" kw)
            (let collect-tr ()
              (when (< idx n)
                (match (peek)
                  [(list 'identifier q1)
                   (set! idx (add1 idx))
                   (unless (and (< idx n) (equal? (first (peek)) "->"))
                     (error "Falta '->'"))
                   (set! idx (add1 idx))
                   (define q2 (second (peek)))
                   (unless (and (< idx n) (eq? (first (peek)) 'identifier))
                     (error "Estado destino inválido"))
                   (set! idx (add1 idx))
                   (unless (and (< idx n) (regexp-match? #px"^en$" (second (peek))))
                     (error "Falta 'en'"))
                   (set! idx (add1 idx))
                   (define sym (second (peek)))
                   (unless (and (< idx n) (eq? (first (peek)) 'literal))
                     (error "Símbolo inválido"))
                   (set! idx (add1 idx))
                   (set! items (cons (list q1 q2 sym) items))
                   (collect-tr)]
                  [_ #f])))]
           [else
            (let collect-items ()
              (when (< idx n)
                (match (peek)
                  [(list typ v)
                   (when (member typ '(literal identifier))
                     (set! items (cons v items))
                     (set! idx (add1 idx))
                     (when (and (< idx n) (equal? (first (peek)) ","))
                       (set! idx (add1 idx)))
                     (collect-items))]
                  [_ #f])))])

         (hash-set! vals kw (reverse items))
         (parse-sections)]
        [_ (set! idx (add1 idx)) (parse-sections)])))

  (automata
   (hash-ref vals "alfabeto")
   (hash-ref vals "estados")
   (first   (hash-ref vals "inicio"))
   (hash-ref vals "aceptación")
   (hash-ref vals "transiciones")
   (hash-ref vals "cadenas")))

;; Resto de funciones (verify, simulate, generate-html, etc.) queda igual…
