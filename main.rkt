#lang racket/base


(require (only-in ffi/unsafe/port unsafe-file-descriptor->port)
         racket/class
         x11/keysymdef
         x11/x11
         "layout.rkt")


(define-syntax-rule (log message args ...)
  (begin
    (display   (format "[~a] " (current-seconds)) (current-error-port))
    (displayln (format message args ...)          (current-error-port))))

(define (panic! message args ...)
  (log  message args ...)
  (exit 1))


(define (config-root-window xdisplay screen root)
  ;; Clear the display to ensure everything is in a good state before we begin.
  (define display-w (DisplayWidth  xdisplay screen))
  (define display-h (DisplayHeight xdisplay screen))
  (XClearArea xdisplay root 0 0 display-w display-h #f)
  ;; Capture all required input events. Most event types can be ignored.
  (XSelectInput
   xdisplay
   root
   '(KeyPressMask SubstructureNotifyMask SubstructureRedirectMask))
  ;; Set the background color to the configured value, defaulting to black if
  ;; the provided color name is invalid.
  ;; FIXME: allow for this value to actually be configured :)
  (define bg-color
    (AllocNamedColor xdisplay screen "crimson" (BlackPixel xdisplay screen)))
  (XSetWindowBackground xdisplay root bg-color)
  (XClearWindow         xdisplay root))

(define (grab-keys xdisplay root mask keys)
  ;; Grab each key specified in the `keys` list using the specified `mask`.
  (for ([key keys])
    (define keycode (XKeysymToKeycode xdisplay key))
    (XGrabKey xdisplay keycode mask root #t 'GrabModeAsync 'GrabModeAsync)))

(define (xk-key->ws-id xk-key)
  ;; Given a keycode, convert its value to the appropriate workspace ID. Since
  ;; '0' is at the end of the row of digits, but its code comes first, we must
  ;; handle it as a special case.
  (if (eq? xk-key 48) 9 (- xk-key 49)))

(define (handle-x11-event xdisplay engine)
  (define event (XNextEvent* xdisplay))
  (case (XEvent-type event)
    ;; Keyboard
    [(KeyPress)
     (define xk-key (XKeycodeToKeysym xdisplay (XKeyEvent-keycode event) 0))
     (case (XKeyEvent-state event)
       ;; SHIFT + META
       [((ShiftMask Mod1Mask))
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
          ;; j
          [(106)
           (send engine focus-prev)]
          ;; k
          [(107)
           (send engine focus-next)]
          ;; l
          [(108)
           (send engine rotate-cw)])])]
    ;; Structure Control
    [(MapRequest)
     (send engine push-win (XMapRequestEvent-window event))]
    [(DestroyNotify)
     (send engine remove-win (XDestroyWindowEvent-window event))]))


(define (tanuki-wm)
  ;; Open a connection to the X server that controls the display. If the
  ;; connection cannot be established we cannot continue any further so we must
  ;; terminate.
  (define xdisplay (XOpenDisplay #f))
  (unless xdisplay (panic! "unable to open display"))
  (log "connected to x server display ~a" (XDisplayString xdisplay))

  ;; Since we only support a single display at this point in time, we use
  ;; `DefaultScreen` to retrieve the screen number. With that, we can
  ;; initialize and configure the root window. This currently applies the
  ;; relevant event masks and sets the background colour.
  (define screen      (DefaultScreen xdisplay))
  (define root-window (RootWindow xdisplay screen))
  (config-root-window xdisplay screen root-window)
  (log "root window has been configured")

  ;; Enable 'KeyPress' events for all relevant key combinations. We must be
  ;; careful to only mask the key combinations we want to capture, otherwise
  ;; we may block certain key combinations in other applications.
  ;; TODO: eventually, these values should be user-configurable
  (define alpha-keys (list XK-h XK-j XK-k XK-l))
  (define num-keys   (list XK-0 XK-1 XK-2 XK-3 XK-4 XK-5 XK-6 XK-7 XK-8 XK-9))
  (define other-keys (list XK-space))

  (define mod-shift-keys (append            num-keys other-keys))
  (define mod-keys       (append alpha-keys num-keys other-keys))

  (grab-keys xdisplay root-window '(Mod1Mask ShiftMask) mod-shift-keys)
  (grab-keys xdisplay root-window '(Mod1Mask)           mod-keys)
  (log "required keys have been grabbed")

  ;; Open the X11 port in 'read' mode.
  (define conn     (XConnectionNumber xdisplay))
  (define x11-port (unsafe-file-descriptor->port conn "x11-port" '(read)))
  (log "x11 port has been opened for reading")

  ;; Initialize the tiling window layout engine, then run the event loop until
  ;; the application terminates (or forever!).
  (define engine (make-object layout-engine% xdisplay screen))
  (let event-loop ()
    (sync/enable-break
     (handle-evt x11-port
                 (λ (e)
                   (let x11-port-loop ()
                     (unless (zero? (XPending xdisplay))
                       (handle-x11-event xdisplay engine)
                       (x11-port-loop)))))
     (handle-evt (current-input-port)
                 (λ (e) (log "input ~a ~a" e (read-line e)))))
    (event-loop))

  ;; Close the connection to the X server if/when the event loops breaks.
  (log "tanuki-wm is shutting down")
  XCloseDisplay(xdisplay))


(module+ main
  (displayln
   (string-append "\n****************************************\n"
                  "* tanuki-wm                            *\n"
                  "****************************************\n")
   (current-error-port))
  (tanuki-wm))

