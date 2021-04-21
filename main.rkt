#lang racket/base


(require ffi/unsafe/port
         racket/class
         x11/keysymdef
         x11/x11
         "layout.rkt")


(define (log message) (displayln message (current-error-port)))

(define (panic! message)
  (log  message)
  (exit 1))

(define (config-root-window xdisplay root display-w display-h)
  (XClearArea   xdisplay root 0 0 display-w display-h #f)
  (XSelectInput xdisplay root '(EnterWindowMask
                                LeaveWindowMask
                                KeyPressMask
                                SubstructureNotifyMask
                                SubstructureRedirectMask)))

(define (grab-key xdisplay root key)
  (let ([k (XKeysymToKeycode xdisplay key)])
    (XGrabKey xdisplay k 'ControlMask root #t 'GrabModeAsync 'GrabModeAsync)))

(define (handle-x11-event xdisplay engine)
  (define event (XNextEvent* xdisplay))
  (case (XEvent-type event)
    ;; Keyboard
    [(KeyPress)
     ; CTRL
     (when (equal? '(ControlMask) (XKeyEvent-state event))
       (define xk-key (XKeycodeToKeysym xdisplay (XKeyEvent-keycode event) 0))
       (case xk-key
         ; F1 - F12
         [(65470 65471 65472 65473 65474 65475 65476 65477
                 65478 65479 65480 65481)
          (send engine switch-workspace (- xk-key 65470))] ; zero-indexed
         ; space
         [(32)  (send engine next-layout)]
         ; j
         [(106) (send engine rotate-left)]
         ; k
         [(107) (send engine rotate-right)]
         [else
          (log (format "key-event ~a ~a ~a"
                       (XKeyEvent-state   event)
                       (XKeyEvent-keycode event)
                       xk-key))]))]
    [(KeyRelease) (void)]
    ;; Window Crossing
    [(EnterNotify) (void)]
    [(LeaveNotify) (void)]
    ;; Structure Control
    [(ConfigureRequest) (void)]
    [(MapRequest)
     (send engine push-win (XMapRequestEvent-window event))]
    [(ConfigureNotify) (void)]
    [(CreateNotify)    (void)]
    [(DestroyNotify)
     (send engine remove-win (XDestroyWindowEvent-window event))]
    ;; Window State Notification
    [(MapNotify)     (void)]
    [(MappingNotify) (void)]
    [(UnmapNotify)   (void)]
    ;; Unhandled events
    (else
     (log (format "event: ~a" (XEvent-type event))))))


(define (tanuki-wm)
  ;; Open a connection to the X server that controls the display. If the
  ;; connection cannot be established we cannot continue any further.
  (define xdisplay (XOpenDisplay #f))
  (unless xdisplay (panic! "unable to open display"))

  ;; Since we only support a single display at this point in time, we use the
  ;; `DefaultScreen` macro to retrieve the screen number.
  (define screen    (DefaultScreen xdisplay))
  (define display-w (DisplayWidth  xdisplay screen))
  (define display-h (DisplayHeight xdisplay screen))

  ;; Initialize and configure the root window.
  (define root-window (RootWindow xdisplay screen))
  (config-root-window xdisplay root-window display-w display-h)
  (log "root window has been configured")

  ;; Enable 'KeyPress' events for all relevant key combinations.
  ;; TODO: one day keyboard shortcuts should be configurable...
  (for ([key (list XK-F1 XK-F2 XK-F3 XK-F4 XK-F5 XK-F6 XK-F7 XK-F8
                   XK-F9 XK-F10 XK-F11 XK-F12 XK-space XK-j XK-k)])
    (grab-key xdisplay root-window key))

  ;; Initialize the tiling window layout engine. Handles sizing and placement
  ;; of windows.
  (define engine
    (make-object layout-engine% xdisplay screen display-w display-h))

  ;; Open the X11 port in 'read' mode.
  (define x11-port
    (unsafe-file-descriptor->port
     (XConnectionNumber xdisplay) "x11-port" '(read)))
  (log "x11 port has been opened for reading")

  ;; Run the event loop until the application terminates.
  (let event-loop ()
    (sync/enable-break
     (handle-evt x11-port
                 (λ (e)
                   (let x11-port-loop ()
                     (unless (zero? (XPending xdisplay))
                       (handle-x11-event xdisplay engine)
                       (x11-port-loop)))))
     (handle-evt (current-input-port)
                 (λ (e) (log (format "input ~a ~a" e (read-line e))))))
    (event-loop)))


(module+ main
  (log "it's tanuki time!")
  (tanuki-wm))

