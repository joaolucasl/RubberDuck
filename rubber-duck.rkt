#lang racket

(require htdp/dir)
(require racket/dict)

;; Function to retrieve .QTest files from a directory
(define (list-test-files dir)
  (if (not(directory-exists? dir)) ;; Checks for existence of given directory
      (error "Directory not found") ;; Throws if non existent
      (sequence->list ;; Performs search otherwise
       (sequence-filter
        (lambda (file)
          (string-suffix? (path->string file) ".qtest"))
        (in-directory dir)))))

;; Splits the file into sections
(define (group-sections targetFile)
  (define token null)
  (define describe '())
  (define source '())
  (define expect '())
  (for-each (lambda (line) ;; Run through each line checking for section definitions
         (cond
           [(equal? line "%%describe")
             (set! token "describe")]
           [(equal? line "%%source")
            (set! token "source")]
           [(equal? line "%%expect")
            (set! token "expect")]
           [true
            (cond
             [(equal? token "describe") ;; If token is set to Describe, appends line to the list
               (set! describe (append describe (list line)))]
             [(equal? token "source") ;; If token is set to Source, appends line to the list
               (set! source (append source (list line)))]
              [(equal? token "expect") ;; If token is set to Source, appends line to the list
               (set! expect (append expect (list line)))]
             )]
          )
       )
       (file->lines targetFile)) ;; Working upon a target file, split by its lines into a list
  (display source)) 
