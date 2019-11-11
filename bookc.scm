(import json)
(import srfi-13)
(import (chicken process-context))
(import (chicken format))
(import (chicken io))
(import (chicken file))
(import (chicken pathname))
(import (chicken string))
(import (chicken irregex))

;;; Utility methods
(define (get-key vec key)
  (define (find-key idx)
    (if (>= idx (vector-length vec))
      (error "Could not find key in vector")
      (let* ((elem (vector-ref vec idx))
             (curkey (car elem))
             (curval (cdr elem)))
        (if (string=? curkey key)
          curval
          (find-key (+ idx 1))))))
  (find-key 0))

(define (string-repeat str count)
  (cond ((= count 0) "")
        ((= count 1) str)
        (else (string-append str (string-repeat str (- count 1))))))

(define (read-file-contents fname)
  (let* ((file-port (open-input-file fname))
         (file-contents (read-string #f file-port)))
    (begin (close-input-port file-port)
           file-contents)))

(define (mapi func l)
  (define (map-index idx li)
    (if (null? li) '()
      (cons (func (car li) idx)
            (map-index (+ idx 1) (cdr li)))))
  (map-index 0 l))

;;; Pandoc writing methods
(define (heading output-port text level)
  (if (and (and (>= level 1) (<= level 6))
           (not (string-contains text "\n")))
    (begin
      (write-string (string-repeat "#" level) #f output-port)
      (write-string (string-append " " text "\n") #f output-port))))

(define h1 (lambda (output-port text) (heading output-port text 1)))
(define h2 (lambda (output-port text) (heading output-port text 2)))
(define h3 (lambda (output-port text) (heading output-port text 3)))
(define h5 (lambda (output-port text) (heading output-port text 4)))
(define h5 (lambda (output-port text) (heading output-port text 5)))
(define h6 (lambda (output-port text) (heading output-port text 6)))

(define (verbatim output-port outformat text)
  (let ((lines (string-split text "\n" #t)))
    (begin
      (write-string "\n" #f output-port)
      (map (lambda (line)
             (write-string (string-append (proc-markdown-line outformat line) "\n")
                           #f output-port)) lines)
      (write-string "\n" #f output-port))))

(define (proc-markdown-line outformat line)
  (let ((val (irregex-match "!\\[(.*)\\]\\((.*)\\)" line)))
    (if (and (irregex-match-data? val)
             (= 2 (irregex-match-num-submatches val)))
      (let ((title (irregex-match-substring val 1))
            (fname (irregex-match-substring val 2)))
        (if (string=? outformat "html")
          (sprintf "![~A](~A.png)" title fname)
          (sprintf "![~A](~A.pdf)" title fname)))
      line))) ;; return line as it is

(define (source output-port outformat text)
  (define writing-code #f)
  (define (end-code)
    (write-string (string-append (string-repeat "~" 20) "\n") #f output-port)
    (set! writing-code #f))
  (define (start-code lang number_lines) 
    (set! writing-code #t)
    (let ((nlines (if number_lines ".numberLines" ""))) 
      (write-string (string-append "\n"
                    (string-repeat "~" 4) " {#mycode ." lang " " nlines "}\n")
                    #f output-port)))
  (define (write-markdown text)
    (if writing-code
      (begin (end-code) (set! writing-code #f)))
    (write-string (string-append 
                    (proc-markdown-line outformat text) "\n") #f output-port))
  (define (write-code line)
    (if (not writing-code)
      (start-code "scheme" #f))
    (write-string (string-append line "\n") #f output-port))
  (let ((lines (string-split text "\n" #t)))
    (map (lambda (line)
           (cond ((eq? (substring-index ";; " line) 0) 
                  (write-markdown (substring line 3)))
                 ((eq? (substring-index ";;" line) 0) 
                  (write-markdown ""))
                 (else (write-code line)))) lines))
  (if writing-code (end-code)))

;;; Book generation
(define (proc-problem output-port outformat folder chapter problem)
  (let* ((files (glob (sprintf "~A/E~A.~A.*" folder chapter problem)))
         (flen (length files)))
    (cond ((= flen 0)
           (display (sprintf "Warning: No solution found for exercise ~A.~A\n" 
                             chapter problem)))
          ((not (= flen 1))
           (error (sprintf "Found ~A files for problem ~A.~A. Expecting 1\n"
                             flen chapter problem)))
          (else (begin 
                  (display (sprintf "Processing: ~A\n" (car files))) 
                  (add-solution output-port outformat chapter problem (car files)))))))

(define (procchapter output-port outformat chapterjson chapter)
  (let ((name (get-key chapterjson "name"))
        (folder (get-key chapterjson "folder"))
        (problems (get-key chapterjson "problems")))
    (begin
      (h1 output-port (sprintf "Chapter ~A" chapter))
      (h2 output-port name)
      (map (lambda (problem)
             (proc-problem output-port outformat folder chapter problem)) problems))))

(define (add-solution output-port outformat chapter problem filepath)
  (define-values (basename fname ext) (decompose-pathname filepath))
  (h3 output-port (sprintf "Problem ~A.~A" chapter problem))
  (let ((extlower (string-downcase ext)))
    (cond ((string=? extlower "scm") 
           (source output-port outformat (read-file-contents filepath)))
          ((string=? extlower "md") 
           (verbatim output-port outformat (read-file-contents filepath)))
          (else (display (sprintf 
                           "Warning: Don't know how to handle file '~A' of type '~A'\n" 
                           filepath ext))))))

(define (makemd tocfile outformat outfile)
  (define tocjson (json-read (open-input-file tocfile)))
  (define outport (open-output-file outfile))
  (mapi (lambda (json idx) (procchapter outport outformat json (+ idx 1)))
        tocjson))

(define (main argv)
  (if (not (= (length argv) 4))
    (display "Usage: build <toc file> {pdf|html} <output file>\n")
    (makemd (cadr argv) (caddr argv) (cadddr argv))))

(main (argv))
