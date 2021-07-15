#lang racket

(require racket/gui/base)
(require racket/list)

(struct posn (x y))

(define up '(0 -1))
(define down '(0 1))
(define left '(-1 0))
(define right '(1 0))


(struct snake (head body max-length direction))
(struct world (snake))

(define (take n list)
  ;; takes n elements from the front of the list
  ;; (take 1 '(1 2 3)) => '(1)
  ;; (take 1 '()) => '()
  ;; (take 3 '(1 2)) => '(1 2)

  (define (iter i list result)
    (cond [(zero? i) (reverse result)]
          [(null? list) (reverse result)]
          [else (iter (- i 1) (rest list) (cons (first list) result))]))

  (iter n list '()))


(define width 500)
(define height 500)
(define square-size 10)

;; Updating Functions

(define (update-snake a-snake)
  (let* [(head (snake-head a-snake))
         (body (snake-body a-snake))
         (max-length (snake-max-length a-snake))
         (direction (snake-direction a-snake))
         (dx (first direction))
         (dy (second direction))
         (new-body (cons head body))
         (new-x (+ (posn-x head) (* dx square-size)))
         (new-y (+ (posn-y head) (* dy square-size)))
         (new-head (posn new-x new-y))]
    (snake new-head
           (if (> (+ 1 (length new-body)) max-length)
               (take (- max-length 1) new-body)
               new-body)
           max-length
           direction)))

(define (update-world a-world)
  (let* [(snake (world-snake a-world))]
    (world (update-snake snake))))

;; Drawing functions

(define frame (new frame%
                   [label "Snake"]
                   [width 500]
                   [height 500]))

(define (draw-background dc)
  (send dc set-brush (new brush% [color "black"] [style 'solid]))
  (send dc draw-rectangle 0 0 500 500))

(define (draw-snake a-snake dc)
  (let* [(head (snake-head a-snake))
         (x (posn-x head))
         (y (posn-y head))]
    (send dc set-brush (new brush% [color "green"] [style 'solid]))
    (send dc draw-rectangle x y square-size square-size)))



(define s (snake (posn (/ width 2) (/ height 2)) (list) 5 left))
(define w (world s))

(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda (canvas dc)
                       ((draw-world w) dc))]))

(define ((draw-world a-world) dc)
  (match-define (world a-snake) a-world)
  (draw-background dc)
  (draw-snake a-snake dc))

(define (run-game-loop world)
  (sleep/yield .5)
  (let [(new-world (update-world world))
        (dc (send canvas get-dc))]
    ((draw-world new-world) dc)
    (run-game-loop new-world)))


(send frame show #t)
(run-game-loop w)

