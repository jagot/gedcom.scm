;; Taken from
;; http://www.cs.rpi.edu/courses/fall00/ai/scheme/reference/scheme-workshop/stacks.html
(define make-stack
  (lambda ()
    (let ((stk '()))
      (lambda (message . args)
        (case message

          ;; If the message is empty?, the stack returns #t if its
          ;; storage location contains the null object, #f if it
          ;; contains a non-empty list. 

          ((empty?) (null? stk))

          ;; The push! message should be accompanied by an extra
          ;; argument -- the value to be pushed onto the stack.  This
          ;; value is simply added to the front of the list in the
          ;; private storage location. 

          ((push!) (set! stk (cons (car args) stk)))

          ;; If the message is top, the stack returns the first
          ;; element of the list in private storage, or signals an
          ;; error if that list is empty.

          ((top) (if (null? stk)
                     (error "top: The stack is empty.")
                     (car stk)))

          ;; If the message is pop!, the stack returns the first
          ;; element of the list in private storage after removing
          ;; that element from that list.

          ((pop!) (if (null? stk)
                      (error "pop!: The stack is empty.")
                      (let ((result (car stk)))
                        (set! stk (cdr stk))
                        result)))

          ;; Comment out any of the following operations that are not
          ;; wanted in a particular application of stacks.

          ;; When it receives the size message, the stack reports the
          ;; number of elements in the list in private storage.

          ((size) (length stk))

          ;; If the message is nth, there should be an extra argument
          ;; -- the (zero-based) position of the item desired, or in
          ;; other words the number of values on the stack that
          ;; precede the one to be returned.

          ((nth) (list-ref stk (car args)))

          ;; When it receives the print message, the stack displays
          ;; the elements of the list in private storage, each
          ;; followed by a space, then starts a new line.

          ((print) (for-each (lambda (element)
                               (display element)
                               (display " "))
                             stk)
           (newline))

          ((pretty-print) (apply pretty-print stk args))

          ;; It is an error to send the stack any other message.

          (else (error "stack: unrecognized message")))))))
