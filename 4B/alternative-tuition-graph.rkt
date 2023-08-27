;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alternative-tuition-graph-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants

(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")

;; Data Definition

;; School is one of:
;; - false
;; - (make-school String Natural School)
;; interp. an arbitrary number of schools, where for each school we have
;; its name and its tuition in USD

(define-struct school (name tuition next))

(define S3 (make-school "School3" 28500 false))
(define S2 (make-school "School2" 23300 S3))
(define S1 (make-school "School1" 27797 S2))

;; Functions

;; School -> Image
;; Produce bar chart showing names and tuitions of consumed schools

;(define (chart los) (square 0 "solid" "white")) - stub

(check-expect (chart S3)
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "School3" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 28500 Y-SCALE) "outline" "black")              
                                           (rectangle BAR-WIDTH (* 28500 Y-SCALE) "solid" BAR-COLOR))
                                           (square 0 "solid" "white")))

(check-expect (chart S2)
              (beside/align "bottom"
                            (overlay/align "center" "bottom" (rotate 90 (text "School2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 23300 Y-SCALE) "outline" "black")              
                                           (rectangle BAR-WIDTH (* 23300 Y-SCALE) "solid" BAR-COLOR))

                                           (overlay/align "center" "bottom" (rotate 90 (text "School3" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 28500 Y-SCALE) "outline" "black")              
                                           (rectangle BAR-WIDTH (* 28500 Y-SCALE) "solid" BAR-COLOR))
                                           (square 0 "solid" "white")))


(define (chart s)
  (cond [(false? s) (square 0 "solid" "white")]
       [else
        (beside/align "bottom"
                      (make-bar s)
                      (chart (school-next s)))]))

;; School -> Image
;; produce a bar for a single school in the bar chart

(check-expect (make-bar (make-school "S1" 8000 false))
              (overlay/align "center" "bottom" (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
              (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")              
              (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR)))

;; (define (make-bar s) (square 0 "solid" "white"))

(define (make-bar s)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                 (rectangle BAR-WIDTH (* Y-SCALE (school-tuition s)) "outline" "black")
                 (rectangle BAR-WIDTH (* Y-SCALE (school-tuition s)) "solid" BAR-COLOR)))
