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


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (if (collection-empty? words) '()
      (prefix-helper (collection-first words) (collection-rest words))))

(define (prefix-helper common-prefix remaining-words)
    (if (collection-empty? remaining-words)
        common-prefix
        (prefix-helper (car (longest-common-prefix common-prefix (collection-first remaining-words)))
                (collection-rest remaining-words))))


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


(define (get-suffixes text)  
  (get-suffixes-helper text '()))

(define (get-suffixes-helper text suffix)
    (if (null? text) '()
        (collection-cons text
              (get-suffixes-helper (cdr text) suffix))))



(define (get-ch-words words ch)
  (collection-filter (lambda (word)
            (and (not (null? word))
                 (char=? (collection-first word) ch)))
          words))


(define (ast-func suffixes)
     (if (collection-empty? suffixes) '()
         (let ((list (list (car (collection-first suffixes)))))
           (cons list (collection-map (lambda (x) (cdr x)) suffixes)))))

(define (cst-func suffixes)
  (let ((common-prefix (longest-common-prefix-of-collection suffixes)))
    (cons common-prefix
          (collection-map (lambda (suffix)
                 (remove-n-chars suffix (length common-prefix)))
               suffixes))))

(define (remove-n-chars list n)
  (if (<= n 0)
      list
      (remove-n-chars (cdr list) (- n 1))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (build-branch ch suffixes labeling-func alphabet)
  (let ((suffixes-ch (get-ch-words suffixes ch)))
    (cond ((collection-empty? suffixes-ch) '())
          (else
           (let ((label (car (labeling-func suffixes-ch)))
               (suffixes-rest (cdr (labeling-func suffixes-ch))))              
          (cons label (suffixes->st labeling-func suffixes-rest alphabet)))))))

(define (suffixes->st labeling-func suffixes alphabet)
  (cond ((collection-empty? suffixes) empty-st)
        (else
         (let* ((branches (collection-map (lambda (ch)
                              (build-branch ch suffixes labeling-func alphabet))
                            alphabet))
             (filtered-branches (collection-filter not-null branches)))
           filtered-branches))))

(define (not-null branch)
  (not (null? branch)))


; nu uitați să convertiți alfabetul într-un flux
(define (text->st text)
  (lambda (labeling-func)
    (let* ((text-with-$ (append text '(#\$)))
           (alphabet (sort (remove-duplicates text-with-$) char<?))
           (alphabet-stream (list->stream alphabet)))
      (suffixes->st labeling-func (get-suffixes text-with-$) alphabet-stream))))

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define text->ast
  (lambda (text)
    ((text->st text) ast-func)))


(define text->cst
  (lambda (text)
    ((text->st text) cst-func)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
   (let ((ast (text->ast text)))
      ((lambda (one-pattern) (st-has-pattern? ast one-pattern)) pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (helper (text->cst text) len))

(define (helper st len)
  (cond
    ((collection-empty? st) #f)
    ((not (collection-empty? (cdr (first-branch st))))
     (let* ((curr-branch (first-branch st)))
       (if (>= (length (get-branch-label curr-branch)) len)
           (take (get-branch-label curr-branch) len)
           (let* ((new-len (- len (length (get-branch-label curr-branch))))
                 (next (helper (get-branch-subtree curr-branch) new-len))) 
             (if next
                 (append (get-branch-label curr-branch) next)
                 (helper (other-branches st) len))))))
    (else
     (helper (other-branches st) len))))