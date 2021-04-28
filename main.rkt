#lang racket/base


(require (only-in ffi/unsafe/port
                  [unsafe-file-descriptor->port unsafe-fd->port])
         racket/class
         x11/keysymdef
         x11/x11
         "layout.rkt")


(define (log message) (displayln message (current-error-port)))

(define (panic! message)
  (log  message)
  (exit 1))


(define (config-root-window xdisplay screen root display-w display-h)
  (XClearArea   xdisplay root 0 0 display-w display-h #f)
  (XSelectInput xdisplay root '(KeyPressMask
                                SubstructureNotifyMask
                                SubstructureRedirectMask))
  ;; Set the background color to something vibrant while we're developing.
  ;; This will eventually be user-configurable I'd imagine.
  (let* ([black   (BlackPixel xdisplay screen)]
         [crimson (AllocNamedColor xdisplay screen "crimson" black)])
    (XSetWindowBackground xdisplay root crimson)
    (XClearWindow         xdisplay root)))

(define (grab-keys xdisplay root mask keys)
  (for ([key keys])
    (let ([k (XKeysymToKeycode xdisplay key)])
      (XGrabKey xdisplay k mask root #t 'GrabModeAsync 'GrabModeAsync))))


(define (xk-key->ws-id key) (if (eq? key 48) 9 (- key 49)))

(define (handle-keypress-event xdisplay engine event)
  (let ([xk-key (XKeycodeToKeysym xdisplay (XKeyEvent-keycode event) 0)])
    (case (XKeyEvent-state event)
      ;; META + SHIFT
      [((Mod1Mask ShiftMask))
       (case xk-key
         ;; SPACE
         [(32)
          (send engine prev-layout)]
         ;; 0 - 9
         [(48 49 50 51 52 53 54 55 56 57)
          (send engine move-to-workspace (xk-key->ws-id xk-key))])]
      ;; META
      [((Mod1Mask))
       (case xk-key
         ;; SPACE
         [(32)
          (send engine next-layout)]
         ;; 0 - 9
         [(48 49 50 51 52 53 54 55 56 57)
          (send engine switch-workspace (xk-key->ws-id xk-key))]
         ;; h
         [(104)
          (send engine rotate-ccw)]
         ;; l
         [(108)
          (send engine rotate-cw)])])))

(define (handle-x11-event xdisplay engine)
  (let ([event (XNextEvent* xdisplay)])
    (case (XEvent-type event)
      ;; Keyboard
      [(KeyPress)
       (handle-keypress-event xdisplay engine event)]
      ;; Structure Control
      [(MapRequest)
       (send engine push-win (XMapRequestEvent-window event))]
      [(DestroyNotify)
       (send engine remove-win (XDestroyWindowEvent-window event))])))


(define (tanuki-wm)
  ;; Open a connection to the X server that controls the display. If the
  ;; connection cannot be established we cannot continue any further.
  (define xdisplay (XOpenDisplay #f))
  (unless xdisplay (panic! "unable to open display"))
  (log (format "connected to x server display ~a" (XDisplayString xdisplay)))

  ;; Since we only support a single display at this point in time, we use
  ;; `DefaultScreen` to retrieve the screen number.
  (define screen    (DefaultScreen xdisplay))
  (define display-w (DisplayWidth  xdisplay screen))
  (define display-h (DisplayHeight xdisplay screen))

  ;; Initialize and configure the root window. This applies the relevant event
  ;; masks and sets the background.
  (define root-window (RootWindow xdisplay screen))
  (config-root-window xdisplay screen root-window display-w display-h)
  (log "root window has been configured")

  ;; Enable 'KeyPress' events for all relevant key combinations. We must be
  ;; careful to only mask the key combinations we want to capture, otherwise
  ;; we may prevent certain key combinations in other applications.
  (define alpha-keys (list XK-h XK-l))
  (define num-keys   (list XK-0 XK-1 XK-2 XK-3 XK-4 XK-5 XK-6 XK-7 XK-8 XK-9))
  (define other-keys (list XK-space))

  (define mod-shift-keys (append            num-keys other-keys))
  (define mod-keys       (append alpha-keys num-keys other-keys))

  (grab-keys xdisplay root-window '(Mod1Mask ShiftMask) mod-shift-keys)
  (grab-keys xdisplay root-window '(Mod1Mask)           mod-keys)
  (log "required keys have been grabbed")

  ;; Open the X11 port in 'read' mode.
  (define x11-port
    (unsafe-fd->port (XConnectionNumber xdisplay) "x11-port" '(read)))
  (log "x11 port has been opened for reading")

  ;; Initialize the tiling window layout engine. Handles sizing and placement
  ;; of windows.
  (define engine
    (make-object layout-engine% xdisplay screen display-w display-h))

  ;; Run the event loop until the application terminates (or forever!).
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
    (event-loop))

  ;; Close the connection to the X server if/when the event loops breaks.
  (log "tanuki-wm is shutting down")
  XCloseDisplay(xdisplay))


(module+ main
  (log "tanuki-wm is starting up")
  (tanuki-wm))

