#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et closed queue) #:transparent)

(define (empty-counter index)
  (counter index 0 0 0 empty-queue))


(define (update f counters index)
  (map (lambda (x) (if ( = (counter-index x) index)
                        (f x)
                        x)) counters ))

(define tt+
  (lambda (C)
    (lambda (minutes_to_be_added)
      (struct-copy counter C [ tt (+ minutes_to_be_added (counter-tt C) )]))))

(define et+
  (lambda (C)
    (lambda (minutes_to_be_added_et)
      (struct-copy counter C [ et (+ minutes_to_be_added_et (counter-et C) )]))))

(define et+_tt+
  (lambda (C)
    (lambda (minutes_to_be_added_et_tt)
      (struct-copy counter C [ tt (+ minutes_to_be_added_et_tt (counter-tt C) )] [ et (+ minutes_to_be_added_et_tt (counter-et C) )]))))


(define (add-to-counter name items)    
  (λ (C)
    (if (queue-empty? (counter-queue C))
        (make-counter (counter-index C) (+ items (counter-tt C)) (+ items (counter-et C)) (counter-closed C) ( enqueue (cons name items) (counter-queue C)))
        (make-counter (counter-index C) (+ items (counter-tt C)) (counter-et C) (counter-closed C) ( enqueue (cons name items) (counter-queue C))))
       ))
  
(define (first-opencounter counters)
  (if (null? counters)
      '()
      (if (= 0 (counter-closed (car counters)))
          (car counters)
          (first-opencounter (cdr counters)))))

(define (min-function f counters)
  (if(null? counters)
     '()
     (if (null? (first-opencounter counters))
         (cons -1 999)
         (cons (counter-index (helepr-min-function f counters (first-opencounter counters))) ( f (helepr-min-function f counters (first-opencounter counters)))))))
     
(define (helepr-min-function f counters acc)
    ( if (null? counters)
         acc
         (if (= 0 (counter-closed (car counters)))
             (if  (< (f (car counters)) (f acc))
                  (helepr-min-function f (cdr counters) (car counters) )
                  (helepr-min-function f (cdr counters) acc))
             ( helepr-min-function f (cdr counters) acc))))
     

(define (min-tt counters) (min-function (lambda (C) (counter-tt C)) counters))
(define (min-et counters) (min-function (lambda (C) (counter-et C)) counters))


(define (remove-first-from-counter C)
  (cond
   ((queue-empty? (counter-queue C)) (struct-copy counter C [tt 0] [et 0] [queue empty-queue]))
   ((queue-empty? (dequeue (counter-queue C)))(struct-copy counter C [tt 0] [et 0] [queue empty-queue]))             
   (else (struct-copy counter C [tt (- (counter-tt C) (counter-et C))] [et (cdr (top (dequeue (counter-queue C)))) ] [queue (dequeue (counter-queue C))]) ))) 




(define (pass-time-through-counter minutes)
  (λ (C)
    (if (<= minutes (counter-et C))
        (struct-copy counter C [tt (- (counter-tt C) minutes)] [et (- (counter-et C) minutes)] )
        (struct-copy counter C [tt 0] [et 0]))))
  



(define (min-function-zero-diff f counters)
  (cond
    ((null? counters) '())
    ((= 1 (length counters)) (if (zero? (length (counter-queue (car counters))))
                                 null
                                 (cons ( counter-index (car counters)) ( f (car counters)))
                                 ))
    ( else (if (zero? (length (counter-queue (car counters))))
                                 (min-function-zero-diff f (cdr counters))
                                 (cons (counter-index (helepr-min-function-zero-diff f counters (car counters) )) ( f (helepr-min-function-zero-diff f counters (car counters))))
                                 )
      )))

(define (helepr-min-function-zero-diff f counters acc)
  (cond
    ((null? counters) acc)
    ((zero? (length (counter-queue (car counters)))) (helepr-min-function-zero-diff f (cdr counters) acc ))
    ((zero? (f (car counters))) (helepr-min-function-zero-diff f (cdr counters) (cadr counters) ) )
    (( < (f (car counters)) (f acc)) (helepr-min-function-zero-diff f (cdr counters) (car counters) ))
    ( else ( helepr-min-function-zero-diff f (cdr counters) acc))))

(define (min-tt-zero-diff counters) (min-function-zero-diff (lambda (C) (counter-tt C)) counters)) ; folosind funcția de mai sus
(define (min-et-zero-diff counters) (min-function-zero-diff (lambda (C) (counter-et C)) counters)) ; folosind funcția de mai sus





(define (avrage counters )
  (/ (avrage-helper-sum counters) (length counters)))

(define (avrage-helper-sum counters)
   (if (empty? counters) 0
      (+ (counter-tt (first counters)) (avrage-helper-sum (rest counters)))))
  
(define (add-all counters average fast-counters)
  (if (> (avrage (filter-open-counters (append counters fast-counters) '())) average)
      (add-all (append counters (list (empty-counter (+ (+ 1 (length counters)) (length fast-counters) )))) average fast-counters)
      counters))


(define (filter-open-counters counters acc)
  (if (null? counters)
      acc
      (if (= 0 (counter-closed (car counters)))
          (filter-open-counters (cdr counters) (append acc (list (car counters))))
          (filter-open-counters (cdr counters) acc))))


(define (find-list-eliminated C minutes acc)
  (if (>= minutes (counter-et C))
      (if (queue-empty? (counter-queue C))
          acc
          (find-list-eliminated (remove-first-from-counter C) (- minutes (counter-et C)) (append acc (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (- minutes (counter-et C))) ))))
      acc))

(define (concatanate-all-eliminations counters minutes acc )
  (cond
    ((null? counters) acc)
    ( else (concatanate-all-eliminations (cdr counters) minutes (append acc (find-list-eliminated (car counters) minutes '())) ))))



(define ( time-pass minutes C)
    (cond
     (( >= minutes (counter-tt C) )
      (struct-copy counter C [tt 0] [et 0] [queue empty-queue])) 
     (( >= minutes (counter-et C) ) ( time-pass (- minutes (counter-et C) ) (remove-first-from-counter C)) )
     (( < minutes (counter-et C) ) ((pass-time-through-counter minutes)C ))))


(define (update-all counters minutes acc)
  (if (null? counters)
      acc
      (update-all (cdr counters) minutes (append acc (list (time-pass minutes (car counters)))) )))

(define (sort-cresc-by-et list)
  (sort list
        (lambda (c1 c2) (< (counter-et c1) (counter-et c2)))))

(define (sort-cresc-by-turn list)
  (sort list
        (lambda (c1 c2) (if (= (cdr c1) (cdr c2))
                            (< (caar c1) (caar c2))
                            (> (cdr c1) (cdr c2))))))
(define (what-is-ok L acc)
  (if (null? L)
      acc
      (what-is-ok (cdr L) (append acc (list (caar L))))))

(define (cast counters acc)
  (if (null? counters)
      acc
      (if (queue-empty? (counter-queue (car counters))) ;aici e problema
          (cast (cdr counters) acc)
          (cast (cdr counters) (append acc (list (cons (counter-index (car counters))  (counter-queue (car counters)) )) ))
          )))

(define close
  (λ (C)
    (struct-copy counter C [closed 1])))
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (serve-helper requests fast-counters slow-counters acc)
  
  (if (null? requests)
      (cons  acc (append (cast fast-counters '() )  (cast slow-counters '() ) ))
      (match (car requests)
        [(list 'close index) (serve-helper (cdr requests) (update (lambda (C) (close  C)) fast-counters index) (update (lambda (C) (close C)) slow-counters index) acc) ]
        [(list 'ensure average) (serve-helper (cdr requests) fast-counters (add-all slow-counters average fast-counters) acc) ]
        [(list 'delay index minutes) (serve-helper (cdr requests) (update (lambda (C) ((et+_tt+ C) minutes)) fast-counters index ) (update (lambda (C) ((et+_tt+ C) minutes)) slow-counters index ) acc)]
        [(list name n-items) ( cond
                             ((<= n-items ITEMS) (if ( <= (cdr(min-tt fast-counters)) (cdr(min-tt slow-counters)))
                                                  (serve-helper (cdr requests) (update (lambda (C) ((add-to-counter name n-items) C)) fast-counters (car(min-tt fast-counters)) ) slow-counters acc)
                                                  (serve-helper (cdr requests) fast-counters (update (lambda (C) ((add-to-counter name n-items) C)) slow-counters (car(min-tt slow-counters)) ) acc)))
                                             
                             (else  (serve-helper (cdr requests) fast-counters (update (lambda (C) ((add-to-counter name n-items) C)) slow-counters (car(min-tt slow-counters)) ) acc )))]
        [(list 'remove-first) (cond
                                ((not (null? (min-et-zero-diff fast-counters))) ( if (null? (min-et-zero-diff slow-counters))
                                                                                          (serve-helper (cdr requests) (update (lambda (C) (remove-first-from-counter C) ) fast-counters (car(min-et-zero-diff fast-counters)) ) slow-counters acc)
                                                                                          (cond
                                                                                            (( <= (cdr(min-et-zero-diff fast-counters)) (cdr(min-et-zero-diff slow-counters))) (serve-helper (cdr requests) (update (lambda (C) (remove-first-from-counter C) ) fast-counters (car(min-et-zero-diff fast-counters)) ) slow-counters acc))
                                                                                            (else (serve-helper (cdr requests) fast-counters (update (lambda (C) (remove-first-from-counter C) ) slow-counters (car(min-et-zero-diff slow-counters)) ) acc)))))
                                ( (not (null? (min-et-zero-diff slow-counters))) (serve-helper (cdr requests) fast-counters (update (lambda (C) (remove-first-from-counter C) ) slow-counters (car(min-et-zero-diff slow-counters)) )acc))
                                ( else (serve-helper (cdr requests) fast-counters slow-counters acc)))]
                                
        [ minutes  (serve-helper (cdr requests) (update-all fast-counters minutes '() ) (update-all slow-counters minutes '() )  (append acc (what-is-ok (sort-cresc-by-turn (concatanate-all-eliminations (append fast-counters slow-counters)  minutes '() )) '()) )) ])))


(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))