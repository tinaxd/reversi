#lang racket

(define disk-none 0)
(define disk-black 1)
(define disk-white 2)

(define (opposite-disk disk)
  (cond
    [(= disk disk-none) disk-none]
    [(= disk disk-black) disk-white]
    [(= disk disk-white) disk-black]))

(define init-board
  (let ([board (make-vector 64 disk-none)])
    (vector-set! board 27 disk-white)
    (vector-set! board 36 disk-white)
    (vector-set! board 28 disk-black)
    (vector-set! board 35 disk-black)
    board))

(define (board-count board)
  (let ([pred (lambda (x) (lambda (y) (= x y)))])
    (let ([black-count (vector-count (pred disk-black) board)]
          [white-count (vector-count (pred disk-white) board)]
          [none-count (vector-count (pred disk-none) board)])
      (list none-count black-count white-count))))

(define (board-get board pos)
  (vector-ref board pos))

(define (board-set! board pos disk)
  (vector-set! board pos disk))

(define (straight-lines pos)
  (let ([col (modulo pos 8)]
        [row (floor (/ pos 8))]
        [ok (lambda (n) (and (<= 0 n) (< n 8)))])
    (define (repeat proc1 proc2 cond1 cond2 v1 v2 collect)
      (if (and (cond1 v1) (cond2 v2))
          (cons (collect v1 v2) (repeat proc1 proc2 cond1 cond2 (proc1 v1) (proc2 v2) collect))
          null))
    (define (make-line rowproc colproc)
      (repeat rowproc colproc ok ok row col (lambda (r c) (+ (* r 8) c))))
    (let ([d (lambda (diff) (lambda (x) (+ x diff)))])
      (list
       (make-line (d -1) (d -1))
       (make-line (d -1) (d 0))
       (make-line (d -1) (d 1))
       (make-line (d 0) (d -1))
       (make-line (d 0) (d 1))
       (make-line (d 1) (d -1))
       (make-line (d 1) (d 0))
       (make-line (d 1) (d 1))))))

(define (board-test board pos disk)
  (let ([lines (straight-lines pos)])
    (define (check-line board pos disk line)
      (define (check-line2 board pos disk line contact acc)
        (if (null? line)
            null
            (if contact
                (let ([h (car line)])
                  (cond
                    [(= (board-get board h) disk-none) null]
                    [(= (board-get board h) disk) (cons h acc)]
                    [else (check-line2 board pos disk (cdr line) contact (cons h acc))]))
                (if (= (board-get board (car line)) (opposite-disk disk))
                    (check-line2 board pos disk (cdr line) #t (car line))
                    null))))
      (check-line2 board pos disk line #f null))
    (if (= (board-get board pos) disk-none)
        (let ([res (flatten (map (lambda (l) (check-line board pos disk (cdr l))) lines))])
          (if (null? res)
              null
              (cons pos res)))
        null)))

(define (board-put! board pos disk)
  (let ([result (board-test board pos disk)])
    (if (null? result)
        0
        (begin
          (for-each (lambda (p)
                      (board-set! board p disk))
                    result)
          (length result)))))

(define (board-who-wins board)
  (define (board-finish? board)
    (let ([all-pos (build-list 64 values)])
      (and
       (andmap (lambda (p) (null? (board-test board p disk-black))) all-pos)
       (andmap (lambda (p) (null? (board-test board p disk-white))) all-pos))))
  (if (board-finish? board)
      (match (board-count board)
        [(list n b w) (cond
                        [(= b w) 'draw]
                        [(> b w) 'black]
                        [else 'white])])
      #f))
