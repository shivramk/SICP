(import json)
(require-library json)

(define (get-key obj key))

(define (heading output-port text level)
  (if (and (and (>= level 1) (<= level 6))
           (not (string-contains text "\n")))
    (write-string text output-port)))

(map (lambda (x) (display x) (display "\n")) jsonobj)

(define (makemd tocfile outfile)
  (define tocjson (json-read (open-input-file tocfile)))
  (display "makemd"))

(define (main argv)
  (if (not (= (length argv) 3))
    (display "Usage: build.scm <toc file> <output file>\n")
    (makemd (cadr argv) (caddr argv))))

(main (argv))

(main '("main" "TOC.json" "foo.md"))
