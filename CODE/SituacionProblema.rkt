#lang racket

;;; Definición de estructuras
(struct automata (alphabet states initial accepting transitions))
(struct transition (from to on))

;;; Expresiones regulares CORRECTAS
(define keywords-regexp #px"\\b(alfabeto|estados|inicio|aceptación|transiciones|en)\\b")
(define symbols-regexp #px"(:|,|->)")
(define state-id-regexp #px"[A-Za-z][A-Za-z0-9_]*")
(define literal-regexp #px"'([^']*'?)'")  ; Permite comillas vacías '' y contenido
(define comment-regexp #px"//.*")
(define whitespace-regexp #px"[ \t\r\n]+")  ; Versión corregida

;;; Tokenización (versión final funcional)
(define (tokenize-line line)
  (let loop ([line (string-trim line)] [tokens '()])
    (cond
      [(string=? "" line) (reverse tokens)]
      
      [(regexp-match? comment-regexp line) (reverse tokens)]
      
      [(regexp-match whitespace-regexp line)
       => (λ (m) 
            (loop (string-trim (substring line (string-length (car m)))
                  tokens))]  ; Correctamente cerrado
      
      [(regexp-match keywords-regexp line)
       => (λ (m) (loop (substring line (string-length (car m)))
                        (cons (list 'keyword (car m)) tokens))]
      
      [(regexp-match symbols-regexp line)
       => (λ (m) (loop (substring line (string-length (car m)))
                        (cons (list 'symbol (car m)) tokens))]
      
      [(regexp-match state-id-regexp line)
       => (λ (m) (loop (substring line (string-length (car m)))
                        (cons (list 'state (car m)) tokens))]
      
      [(regexp-match literal-regexp line)
       => (λ (m) (loop (substring line (string-length (car m)))
                        (cons (list 'literal (cadr m)) tokens))]
      
      [else 
       (error (format "Error léxico: Carácter no reconocido '~a' en posición ~a"
                     (substring line 0 1)
                     (- (string-length line) 
                        (string-length (string-trim line)))))])))

;;; Análisis sintáctico y construcción del autómata
(define (parse-automata tokens)
  (define sections (make-hash))
  (define current-section #f)
  
  (for ([token tokens])
    (match token
      [(list 'keyword kw)
       (set! current-section kw)
       (hash-set! sections kw '())]
      [(list 'symbol ":") (void)]
      [else
       (when current-section
         (hash-update! sections current-section (λ (v) (cons token v))))]))
  
  ;; Construir estructura del autómata
  (automata
   (get-alphabet (reverse (hash-ref sections 'alfabeto '())))
   (get-states (reverse (hash-ref sections 'estados '())))
   (get-initial (reverse (hash-ref sections 'inicio '())))
   (get-accepting (reverse (hash-ref sections 'aceptación '())))
   (get-transitions (reverse (hash-ref sections 'transiciones '())))))

;;; Funciones auxiliares de análisis
(define (get-alphabet tokens)
  (filter-map (λ (t) (and (eq? (car t) 'literal) (cadr t))) tokens))

(define (get-states tokens)
  (filter-map (λ (t) (and (eq? (car t) 'state) (cadr t))) tokens))

(define (get-initial tokens)
  (and (not (null? tokens)) (cadar tokens)))

(define (get-accepting tokens)
  (filter-map (λ (t) (and (eq? (car t) 'state) (cadr t))) tokens))

(define (get-transitions tokens)
  (let loop ([tokens tokens][trans '()])
    (if (null? tokens)
        trans
        (match tokens
          [(list (list 'state from) (list 'symbol "->") (list 'state to) 
           (list 'keyword "en") (list 'literal sym) rest ...)
           (loop rest (cons (transition from to sym) trans))]
          [_ (error "Error en transiciones")]))))

;;; Validación semántica
(define (validate-automata aut)
  (define (check-exists lst msg)
    (for ([item lst])
      (unless (member item (automata-states aut))
        (error (format "~a no definido: ~a" msg item)))))
  
  (unless (member (automata-initial aut) (automata-states aut))
    (error "Estado inicial no definido"))
  
  (check-exists (automata-accepting aut) "Estado de aceptación")
  
  (for ([t (automata-transitions aut)])
    (unless (member (transition-on t) (automata-alphabet aut))
      (error (format "Símbolo '~a' no está en el alfabeto" (transition-on t)))))
    (check-exists (list (transition-from t) (transition-to t)) "Estado")))

;;; Simulación del autómata
(define (simulate aut input)
  (let ([trans-map (for/hash ([t (automata-transitions aut)])
                     (values (cons (transition-from t) (transition-on t)) 
                            (transition-to t)))])
    (let loop ([state (automata-initial aut)] [chars (string->list input)])
      (cond
        [(null? chars) (member state (automata-accepting aut))]
        [(hash-ref trans-map (cons state (car chars)) #f) => (λ (next) (loop next (cdr chars)))]
        [else #f]))))
    
;;; Generación de HTML
(define (format-automata aut)
  (string-append
   "Alfabeto: " (string-join (automata-alphabet aut) ", ") "\n"
   "Estados: " (string-join (automata-states aut) ", ") "\n"
   "Estado inicial: " (automata-initial aut) "\n"
   "Estados de aceptación: " (string-join (automata-accepting aut) ", ") "\n"
   "Transiciones:\n"
   (string-join 
    (map (λ (t) 
          (format "~a -> ~a en '~a'" 
                 (transition-from t)
                 (transition-to t)
                 (transition-on t)))
        (automata-transitions aut))
    "\n")))

(define (generate-html aut input output-file)
  (define html-content
    `(html
      (head (title "Autómata"))
      (body
       (h1 "Autómata")
       (pre ,(format-automata aut))
       (h2 "Simulación")
       (ul ,@(map (λ (s) 
                   `(li ,(format "~a: ~a" s 
                                (if (simulate aut s) "Acepta" "Rechaza"))))
                 input)))))
  
  (with-output-to-file output-file
    (λ ()
      (display "<!DOCTYPE html>\n")
      (displayln (string-join (flatten (map ~a html-content)) "\n")))
    #:exists 'replace))

;;; Función principal
(define (main input-file output-file)
  (define lines (file->lines input-file))
  (define tokens (append-map (λ (line) (tokenize-line line)) lines))
  (define aut (parse-automata tokens))
  (validate-automata aut)
  (generate-html aut '("a-123" "ax-4") output-file))

;; Ejecutar con el archivo de entrada
(main "Automata_2.txt" "salida.html")