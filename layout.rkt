#lang racket/base


(provide layout-engine%)

(require racket/class
         racket/list
         racket/match
         x11/x11)


(define (shift-left xs)
  (when (> (length xs) 1) (cons (last xs) (drop-right xs 1))))

(define (shift-right xs)
  (when (> (length xs) 1) (append (rest xs) (list (first xs)))))


(struct rect (x y w h) #:transparent)


(define workspace%
  (class object%
    (super-new)

    (field [ws '()])

    (define/public (get-ws) ws)

    (define/public (push-win   w) (set! ws (append ws (list w))))
    (define/public (remove-win w) (set! ws (remove w ws)))

    (define/public (rotate-left)  (set! ws (shift-left  ws)))
    (define/public (rotate-right) (set! ws (shift-right ws)))))


(define layout-engine%
  (class object%
    (super-new)

    (init-field xdisplay screen display-w display-h)

    (field [layout-t   '(horizontal vertical fullscreen)]
           [root-rect  (rect 0 0 display-w display-h)]
           [current-ws (new workspace%)]
           [wss        (hash 0 current-ws)])

    (define/public (switch-workspace ws-id)
      (define new-ws (hash-ref wss ws-id
                               (λ ()
                                 (let ([new-ws (new workspace%)])
                                   (set! wss (hash-set wss ws-id new-ws))
                                   new-ws))))
      (when (not (eq? current-ws new-ws))
        (hide-all (send current-ws get-ws))
        (set! current-ws new-ws)
        (layout)))

    (define/public (prev-layout)
      (set! layout-t (shift-left layout-t))
      (layout))
    (define/public (next-layout)
      (set! layout-t (shift-right layout-t))
      (layout))

    (define/public (push-win w)
      (send current-ws push-win w)
      (layout))
    (define/public (remove-win w)
      (send current-ws remove-win w)
      (layout))

    (define/public (rotate-left)
      (send current-ws rotate-left)
      (layout))
    (define/public (rotate-right)
      (send current-ws rotate-right)
      (layout))

    ;; Private
    (define (layout)
      (match (send current-ws get-ws)
        [(list)   (void)]
        [(list w) (fullscreen w)]
        [(list-rest m ws)
         (case (first layout-t)
           [(fullscreen) (fullscreen m)
                         (hide-all   ws)]
           [else
            (define-values (s1 s2)
              (if (eq? (first layout-t) 'horizontal)
                  (values split-horizontal split-vertical)
                  (values split-vertical   split-horizontal)))
            (match-define (list r1 r2) (s1 root-rect 2))
            (size m r1)
            (for ([w ws]
                  [r (s2 r2 (length ws))])
              (size w r))])]))

    (define (size-and-show-window win r)
      (match-define (rect x y w h) r)
      (XMoveResizeWindow xdisplay win x y w h)
      (XMapWindow        xdisplay win))

    (define (fullscreen w)   (size-and-show-window w root-rect))
    (define (size       w r) (size-and-show-window w r))
    (define (hide       w)   (XUnmapWindow xdisplay w))
    (define (hide-all   ws)  (for ([w ws]) (hide w)))

    (define (split-horizontal r c)
      (match-define  (rect x y w h) r)
      (define-values (q rem) (quotient/remainder w c))
      (for/list ([i c])
        (let ([x (+ x (* i q) (if (< i rem) i 0))]
              [w (+ q (if (< i rem) 1 0))])
          (rect x y w h))))

    (define (split-vertical r c)
      (match-define  (rect x y w h) r)
      (define-values (q rem) (quotient/remainder h c))
      (for/list ([i c])
        (let ([y (+ y (* i q) (if (< i rem) i 0))]
              [h (+ q (if (< i rem) 1 0))])
          (rect x y w h))))))

