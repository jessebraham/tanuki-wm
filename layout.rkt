#lang racket/base


(provide layout-engine%)

(require racket/class
         racket/list
         racket/match
         x11/x11)


(define (shift-left xs)
  (when (> (length xs) 1) (append (rest xs) (list (first xs)))))

(define (shift-right xs)
  (when (> (length xs) 1) (cons (last xs) (drop-right xs 1))))


(struct rect (x y w h) #:transparent)


(define workspace%
  (class object%
    (super-new)

    (field [ws    '()]
           [focus null])

    (define/public (get-wins) ws)

    (define/public (push-win   w) (set! ws (append ws (list w))))
    (define/public (remove-win w) (set! ws (remove w ws)))

    (define/public (rotate-left)  (set! ws (shift-right ws)))
    (define/public (rotate-right) (set! ws (shift-left  ws)))

    (define/public (has-focus?)  (not (null? focus)))
    (define/public (get-focus)   focus)
    (define/public (set-focus w) (set! focus w))))


(define layout-engine%
  (class object%
    (super-new)

    (init-field xdisplay screen display-w display-h)

    (field [layout-t  '(horizontal vertical fullscreen)]
           [root-rect  (rect 0 0 display-w display-h)]
           [current-ws (new workspace%)]
           [wss        (hash 0 current-ws)])

    ;; Eventually these values will be loaded from a configuration file or
    ;; something, but until then we'll just define them here for convenience.
    (define BORDER   4)
    (define PADDING 20)

    ;; These should be temporary, and will be removed when configuration is a
    ;; thing.
    (define black-pixel (BlackPixel xdisplay screen))
    (define white-pixel (WhitePixel xdisplay screen))

    ;; ------------------------------------------------------------------------
    ;; Public Methods

    (define/public (push-win w)
      (XSetWindowBorderWidth xdisplay w BORDER)
      (send current-ws push-win w)
      (layout)
      (focus-window w))

    (define/public (remove-win w)
      (send current-ws remove-win w)
      (layout)
      (let ([ws (send current-ws get-wins)])
        (if (empty? ws)
            (send current-ws set-focus null)
            (focus-window (last ws)))))

    (define/public (switch-workspace ws-id)
      (let ([new-ws (wss-ref ws-id)])
        (when (not (eq? current-ws new-ws))
          (hide-all (send current-ws get-wins))
          (set! current-ws new-ws)
          (layout)
          (when (send current-ws has-focus?)
            (focus-window (send current-ws get-focus))))))

    (define/public (move-to-workspace ws-id)
      (let* ([new-ws (wss-ref ws-id)]
             [w      (send current-ws get-focus)])
        (unless (or (eq? current-ws new-ws) (null? w))
          (hide       w)
          (remove-win w) ; invokes `layout`
          (send new-ws push-win  w)
          (send new-ws set-focus w)
          (let ([ws (send current-ws get-wins)])
            (if (empty? ws)
                (send current-ws set-focus null)
                (focus-window (last ws)))))))

    (define/public (prev-layout)
      (set! layout-t (shift-right layout-t))
      (layout))

    (define/public (next-layout)
      (set! layout-t (shift-left layout-t))
      (layout))

    (define/public (rotate-ccw)
      (send current-ws rotate-right)
      (layout))

    (define/public (rotate-cw)
      (send current-ws rotate-left)
      (layout))

    (define/public (focus-prev) (shift-focus shift-right))
    (define/public (focus-next) (shift-focus shift-left))

    ;; ------------------------------------------------------------------------
    ;; Private Methods

    (define/private (wss-ref ws-id)
      (hash-ref wss ws-id (λ ()
                            (let ([new-ws (new workspace%)])
                              (set! wss (hash-set wss ws-id new-ws))
                              new-ws))))

    (define/private (layout)
      (match (send current-ws get-wins)
        [(list)    (void)]
        [(list  w) (fullscreen w)]
        [(list* m ws)
         (case (first layout-t)
           [(horizontal) (tile-horizontal m ws)]
           [(vertical)   (tile-vertical m ws)]
           [(fullscreen) (hide-all   ws)
                         (fullscreen m)])]))

    (define/private (tile-horizontal m ws)
      (match-let* ([(list r1 r2)       (split-rect root-rect 2 'horizontal)]
                   [(rect x1 y1 w1 h1) r1]
                   [(rect x2 y2 w2 h2) r2])
        (let* ([hp (/ PADDING 2)]
               [vp (/ PADDING (length ws))])
          (size m (rect x1 y1 (+ w1 hp) h1))
          (for ([win ws]
                [i   (range (length ws))]
                [r   (split-rect r2 (length ws) 'vertical)])
            (match-let ([(rect x y w h) r])
              (size win (rect (- x hp)
                              (- y (* i vp))
                              (+ w hp)
                              (+ h (if (eq? (length ws) 1) 0 vp)))))))))

    (define/private (tile-vertical m ws)
      (match-let* ([(list r1 r2)       (split-rect root-rect 2 'vertical)]
                   [(rect x1 y1 w1 h1) r1]
                   [(rect x2 y2 w2 h2) r2])
        (let* ([hp (/ PADDING 2)]
               [vp (/ PADDING (length ws))])
          (size m (rect x1 y1 w1 (+ h1 hp)))
          (for ([win ws]
                [i   (range (length ws))]
                [r   (split-rect r2 (length ws) 'horizontal)])
            (match-let ([(rect x y w h) r])
              (size win (rect (- x (* i vp))
                              (- y hp)
                              (+ w (if (eq? (length ws) 1) 0 vp))
                              (+ h hp))))))))

    (define/private (split-rect r n axis)
      (match-let* ([horizontal?    (eq? axis 'horizontal)]
                   [(rect x y w h) r])
        (let-values ([(q rem) (quotient/remainder (if horizontal? w h) n)])
          (for/list ([i n])
            (let-values ([(wx ww) (axis-position-dimension x q rem i)]
                         [(wy wh) (axis-position-dimension y q rem i)])
              (if horizontal? (rect wx y ww h) (rect x wy w wh)))))))

    (define/private (axis-position-dimension p q rem i)
      (let* ([ii (if (< i rem) i 0)]
             [a  (+ p (* i q) ii)]
             [b  (+ q ii)])
        (values a b)))

    (define/private (hide       w)  (XUnmapWindow xdisplay w))
    (define/private (hide-all   ws) (for ([w ws]) (hide w)))
    (define/private (fullscreen w)  (size w root-rect))

    (define/private (size win r)
      (match-let* ([bb (* 2 BORDER)]
                   [pp (* 2 PADDING)]
                   [(rect x y w h) r])
        (XMoveResizeWindow
         xdisplay win (+ x PADDING) (+ y PADDING) (- w bb pp) (- h bb pp))
        (XMapWindow xdisplay win)))

    (define/private (shift-focus shift-fn)
      (let ([ws (send current-ws get-wins)])
        (match ws
          [(list)   (void)]
          [(list w) (focus-window w)]
          [else
           (if (send current-ws has-focus?)
               (let* ([cf (send current-ws get-focus)]
                      [nf (list-ref (shift-fn ws) (index-of ws cf))])
                 (focus-window nf))
               (focus-window (last ws)))]))
      (layout))

    (define/private (focus-window w)
      (let ([focus (send current-ws get-focus)])
        (unless (null? focus) (set-focus-state focus #f)))
      (set-focus-state w #t))

    (define/private (set-focus-state w active?)
      (let* ([focus-w (if active? w null)]
             [input-w (if active? w None)])
        (send current-ws set-focus focus-w)
        (XSetInputFocus   xdisplay input-w 'RevertToParent CurrentTime)
        (XSetWindowBorder xdisplay w (if active? white-pixel black-pixel))))

    ;; end of layout-engine%
    ))

