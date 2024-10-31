#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.

(require (lib "trace.ss"))

;;functie auxiliara pentru a vedea daca se pune problema existentei unui eventual prefix 
(define (possible-prefix? W1 W2)
  (equal? (car W1) (car W2)))

;;functie auxiliara care gaseste prefixul comun a doua cuvinte
(define (find-prefix W1 W2 PrefixW)
  (if (or (null? W1) (null? W2))
      PrefixW
      (if (equal? (car W1) (car W2))
          (find-prefix (cdr W1) (cdr W2) (append PrefixW (list (car W1))))
          PrefixW)))

;;functie auxiliara care gaseste partile care nu sunt comune din cele doua cuvinte
(define (find-not-common W1 W2 NotCommonW)
  (if (or (null? W1) (null? W2))
      (append NotCommonW (list W1) (list W2))
      (if (equal? (car W1) (car W2))
          (find-not-common (cdr W1) (cdr W2) NotCommonW)
          (append NotCommonW (list W1) (list W2)))))

;;functie wrapper pentru functia ceruta
(define (longest-common-prefix-wrapper W1 W2)
  (cons (find-prefix W1 W2 '()) (find-not-common W1 W2 '())))
  
(define (longest-common-prefix W1 W2)
  (if (possible-prefix? W1 W2)
      (longest-common-prefix-wrapper W1 W2)
      (append (list '()) (list W1) (list W2))))
   

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

;;functie wrapper pentru functia ceruta
(define (longest-common-prefix-of-list-wrapper words IntermediarPrefix FinalPrefix)
  (if (null? words)
      (append FinalPrefix IntermediarPrefix)
      (if (null? (car words))
          (append FinalPrefix IntermediarPrefix)
          (longest-common-prefix-of-list-wrapper (cdr words) (find-prefix IntermediarPrefix (car words) '()) '()))))
          
          
(define (longest-common-prefix-of-list words)
  (if (= (length words) 1)
      (car words)
          (longest-common-prefix-of-list-wrapper (cdr (cdr words)) (find-prefix (car words) (car (cdr words)) '()) '())))
      
      
      
  
      ;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

;; functie auxiliara care verifica daca un cuvant de lungime mai mica este inclus in altul de lungime mai mare
(define (is-partially-included shortw longw)
  (if (null? shortw)
          longw
          (if (equal? (car shortw) (car longw))
              (is-partially-included (cdr shortw) (cdr longw))
              '())))
      
(define (match-pattern-with-label st pattern)
  (if (st-empty? st)
      (append (list #f) (list '()))
      (if (not (equal? (car (get-branch-label (first-branch st))) (car pattern)))
          (match-pattern-with-label (other-branches st) pattern)
          (if (equal? (find-prefix pattern (get-branch-label (first-branch st)) '()) pattern) 
              #t
              (if (not (equal? (find-prefix pattern (get-branch-label (first-branch st)) '()) (get-branch-label (first-branch st))))
                      (append (list #f) (list (find-prefix (get-branch-label (first-branch st)) pattern '())))
                      (append (list (get-branch-label (first-branch st))) (list (is-partially-included (get-branch-label (first-branch st)) pattern)) (list (get-branch-subtree (first-branch st)))))))))
      
          
; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  'your-code-here)