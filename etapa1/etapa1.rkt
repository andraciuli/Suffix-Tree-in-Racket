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
(define (longest-common-prefix w1 w2)
  (longest-common-prefix-helper '() w1 w2))

(define (longest-common-prefix-helper prefix rest1 rest2)
    (cond ((or (null? rest1) (null? rest2))
           (list (reverse prefix) rest1 rest2))
          ((char=? (car rest1) (car rest2))
           (longest-common-prefix-helper (cons (car rest1) prefix)
                 (cdr rest1) (cdr rest2)))
          (else
           (list (reverse prefix) rest1 rest2))))

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  (if (null? words) '()
      (helper (car words) (cdr words))))

(define (helper common-prefix remaining-words)
    (if (null? remaining-words)
        common-prefix
        (helper (car (longest-common-prefix common-prefix (car remaining-words)))
                (cdr remaining-words))))


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
(define (match-pattern-with-label st pattern)
  (cond ((st-empty? st) #f)
        (else
          (match-pattern-helper (get-ch-branch st (car pattern)) pattern))))


(define (match-pattern-helper branch pattern)
  (cond ((not branch) (list #f '()))
        ((equal? (car (longest-common-prefix pattern (get-branch-label branch))) pattern) #t)
        (else
         (cond ((equal? (car (longest-common-prefix pattern (get-branch-label branch))) (get-branch-label branch))            
               (list (car (longest-common-prefix pattern (get-branch-label branch)))
                     (remove-prefix (car (longest-common-prefix pattern (get-branch-label branch))) pattern)
                     (get-branch-subtree branch)) )    
               (else
                (list #f (car (longest-common-prefix pattern (get-branch-label branch)))))))))

(define (remove-prefix prefix word)
  (cond
    ((and (not (empty? prefix))
          (char=? (car prefix) (car word)))
     (remove-prefix (cdr prefix) (cdr word)))
    (else
     word)))

; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
   (has-pattern-helper st pattern))

(define (has-pattern-helper st pattern)
  (cond ((st-empty? st) #f)
        ((equal? (match-pattern-with-label st pattern) #t)
         #t)
        ((equal? (car (match-pattern-with-label st pattern)) #f)
         #f)
        (else
         (has-pattern-helper (last (match-pattern-with-label st pattern)) (car (cdr (match-pattern-with-label st pattern)))))))
