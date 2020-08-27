#lang racket

(require "./reversi.rkt")
(require "./ai.rkt")
(require racket/gui/base)

(define frame (new frame% [label "Tinax Reversi"]
                   [width 800]
                   [height 800]))

(define current-turn disk-black)
(define (change-turn!)
  (if (= current-turn disk-black)
      (set! current-turn disk-white)
      (set! current-turn disk-black)))

(define board-canvas%
  (class canvas%
    (define current-board init-board)
    (define cell-size 80)
    (define/public (get-board) current-board)
    (define/public (set-board b) (set! current-board b))
    (define/public (aiturn)
      (let ([ai-choice (ai-decide current-board current-turn 4)])
        (when (and ai-choice (not (= ai-choice -1))) (board-put! current-board ai-choice current-turn)))
      (change-turn!)
      (send this refresh))
    (define/override (on-event event)
      (when (send event button-down? 'left)
          (let* ([raw-x (send event get-x)]
                 [raw-y (send event get-y)]
                 [row (floor (/ raw-y cell-size))]
                 [col (floor (/ raw-x cell-size))]
                 [idx (+ (* row 8) col)])
            (when (and (<= 0 idx) (< idx 64))
              (when (< 0 (board-put! current-board idx current-turn))
                (send this refresh)
                (change-turn!)
                (aiturn))))))
    ; main draw callback
    (define/override (on-paint)
      (let ([dc (send this get-dc)])
        ; background
        (send dc set-brush (new brush% [color "gray"]))
        (send dc draw-rectangle 0 0 (* 8 cell-size) (* 8 cell-size))
        ; vertical lines
        (for ([x (in-range 9)])
          (send dc draw-line (* x cell-size) 0 (* x cell-size) (* 8 cell-size)))
        ; horizontal lines
        (for ([y (in-range 9)])
          (send dc draw-line 0 (* y cell-size) (* 8 cell-size) (* y cell-size)))
        ; disks
        (define radius (* cell-size 0.4))
        (for ([i (in-range 64)])
          (let ([row (floor (/ i 8))]
                [col (modulo i 8)])
            (define (draw-disk brush)
              (send dc set-brush brush)
              (let ([x (- (+ (* cell-size col) (/ cell-size 2)) radius)]
                    [y (- (+ (* cell-size row) (/ cell-size 2)) radius)])
                (send dc draw-ellipse x y (* radius 2) (* radius 2))))
            (let ([d (board-get current-board i)])
              (cond
                [(= d disk-black) (draw-disk (new brush% [color "black"]))]
                [(= d disk-white) (draw-disk (new brush% [color "white"]))]))))))
    (super-new)))

(define canvas (new board-canvas% [parent frame]))

(new button%
     [label "skip"]
     [parent frame]
     [callback (lambda (b e)
                 (change-turn!)
                 (send canvas aiturn))])

(send frame show #t)
