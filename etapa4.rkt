#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

          
; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection             
(define (longest-common-prefix-of-collection words)
 'your-code-here)

(define (match-pattern-with-label st pattern)
  'your-code-here)

(define (st-has-pattern? st pattern)
  'your-code-here)

(define (get-suffixes text)
  (if (collection-empty? text)
      text
      (collection-cons text (get-suffixes (collection-rest text)))))

(define (get-ch-words words ch)
  (collection-filter (lambda (C)
            (and (not (collection-empty? C)) (equal? (collection-first C) ch)))
          words))


(define (ast-func suffixes)
  (cons (list (collection-first (collection-first suffixes))) (collection-map (lambda (C)
                                           (collection-rest C))
                                         suffixes)))


(define (cst-func suffixes)
  'your-code-here)


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  'your-code-here)


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  'your-code-here)


(define text->ast
  'your-code-here)


(define text->cst
  'your-code-here)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
;; functie auxiliara care ne indica daca se poate potrivi textul si pattern ul 
(define (is-fit? text pattern pos)
    (cond
      ((null? pattern) #t)
      ((null? text) #f)  
      ((equal? (car text) (car pattern))
       (is-fit? (cdr text) (cdr pattern) (+ pos 1))) 
      (else #f)))  

;;functie auxiliara care incearca sa potriveasca textul si pattern ul, parcurgand textul convenabil  
(define (fit-verify text pattern)
    (if (null? text)
        #f
        (if (is-fit? text pattern 0)
            #t
            (fit-verify (cdr text) pattern))))

(define (substring? text pattern)
  (fit-verify text pattern))



; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  'your-code-here)