#lang racket

(require unstable/logging
         (for-syntax syntax/parse))

(provide struct-profile)

(define-syntax (struct-profile stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(let ([events* '()])

         (define start-time (current-process-milliseconds (current-thread)))

         ;; Run user code
         (with-intercepted-logging
          (λ (m) (set! events* (cons (vector-ref m 2) events*)))
          (λ () body ...)
          'info)

         (define end-time (current-process-milliseconds (current-thread)))

         ;; Calculate feature times
         (define events (reverse events*))
         (define struct-allocate-times
           (for/fold ([acc (hash 'total 0)])
                     ([i events])
             (match i
               [(vector src t-start t-end)
                (define t-delta (- t-end t-start))
                (hash-set* acc
                           src (+ (hash-ref acc src 0) t-delta)
                           'total (+ (hash-ref acc 'total) t-delta))]
               [else (error (format "Expected feature, got: ~a" events))])))

         ;; Output Results
         (printf "Total time: ~a~n" (- end-time start-time))
         (printf "Total events: ~a~n~n" (length events))
         (printf "Struct Allocation: ~a ms~n" (hash-ref struct-allocate-times 'total))
         (for ([(k v) struct-allocate-times]
               #:unless (equal? k 'total))
           (printf "  ~a ms : ~a~n" k v)))]))
