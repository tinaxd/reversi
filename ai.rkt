#lang racket

(require "reversi.rkt")

(provide
 ai-decide)

(define (children-boards2 board next-disk)
  (let ([children (filter (lambda (ls) ls)
                          (for/list ([pos (in-range 64)])
                            (define diff (board-test board pos next-disk ))
                            (if (null? diff)
                                #f
                                `(,pos ,(board-apply-test-result board diff next-disk)))))])
    (if (null? children)
        (if (not (board-who-wins board))
            (cons (list -1 board) null)
            null)
        children)))

(define (children-boards board next-disk)
  (map second (children-boards2 board next-disk)))

(define (minimax board depth ai next-disk static-evaluate)
  (let ([verybig 2147483647])
    (alpha-beta board depth ai next-disk (- verybig) verybig static-evaluate)))

(define (count-evaluator ai)
  (lambda (board)
    (length (filter (lambda (i) (= ai (board-get board i))) (stream->list (in-range 64))))))

(define (alpha-beta board depth ai next-disk alpha beta static-evaluate)
  (let ([children (children-boards board next-disk)])
    (if (or (null? children) (= depth 0))
        (static-evaluate board)
        (if (= ai next-disk)
            ; my turn
            (let ([my-alpha alpha])
              (for ([child children])
                (set! my-alpha (max my-alpha (alpha-beta child (- depth 1) ai (opposite-disk next-disk) my-alpha beta static-evaluate)))
                #:break (>= my-alpha beta)
                #f)
              my-alpha)
            ; opponent turn
            (let ([my-beta beta])
              (for ([child children])
                (set! my-beta (min my-beta (alpha-beta child (- depth 1) ai (opposite-disk next-disk) alpha my-beta static-evaluate)))
                #:break (>= alpha my-beta)
                #f)
              my-beta)))))

(define (ai-decide board ai depth)
  (let ([children (children-boards2 board ai)]
        [first-if (lambda (ls) (if (not ls) #f (first ls)))]
        [argmax-if (lambda (f ls) (if (null? ls) #f (argmax f ls)))])
    (first-if (argmax-if second (map (lambda (ls)
                                       (list (first ls) (minimax (second ls) (- depth 1) ai (opposite-disk ai) (count-evaluator ai))))
                                     children)))))
