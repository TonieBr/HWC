;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =====================
;; Functions
;; =====================

;; Game --> Game
;; Start the world with (main G0)

(define (main game)
  (big-bang game                 
            (on-tick   tock)          ; Game -> Game
            (to-draw   render)        ; Game -> Image
            (stop-when lost?)         ; Game -> Boolean        
            (on-key    handle-key)))  ; Game KeyEvent -> Game

;; Game -> Game
;; Consume game and produces progressed game

(define (tock g)
  (collide? (make-game (random-saucer  (tock-invaders (game-invaders g)))
                       (prune-missiles (tock-missiles (game-missiles g)))
                       (reverse-tank (tock-tank (game-tank g))))))

;; Tank -> Tank
;; Consumes Tank and produces progressed Tank

(check-expect (tock-tank T0) (make-tank (+ (/ WIDTH 2) 1) 1))
(check-expect (tock-tank T1) (make-tank 51 1))
(check-expect (tock-tank T2) (make-tank 49 -1))

(define (tock-tank t)
  (make-tank
   (+ (tank-x t) (tank-dir t))
   (tank-dir t)))

;; listofInvader -> listofInvader
;; Consumes list of Invader and produces progressed list of invader

(check-expect (tock-invaders empty) empty)
(check-expect (tock-invaders (list I1)) (list
                                            (make-invader (+ 12 150) (+ INVADER-Y-SPEED 100) 12)))

(check-expect (tock-invaders (list
                               I1
                              (make-invader (+ WIDTH 5) 5 5)))
              (list
               (make-invader (+ 12 150) (+ INVADER-Y-SPEED 100) 12)
               (make-invader WIDTH (+ INVADER-Y-SPEED 5) (- 5))))
               
              
; (define (tock-invaders loi) empty)

(define (tock-invaders loi)
  (cond [(empty? loi) empty]                   
        [else (cons  (reverse-invader (progress-invader (first loi)))
                     (tock-invaders (rest loi)))]))

;; Invader -> Invader
;; Consumes Invader and produces progressed Invader

(define (progress-invader i)
  (make-invader (+ (invader-dx i) (invader-x i))
                (+ INVADER-Y-SPEED (invader-y i))
                (invader-dx i)))

;; Invader -> Invader
;; Reverses direction of Invader if it has reached the edge of the X-axis

(check-expect (reverse-invader (make-invader 5 5 5)) (make-invader 5 5 5))
(check-expect (reverse-invader (make-invader (+ WIDTH 5) 5 5)) (make-invader WIDTH 5 (- 5)))
(check-expect (reverse-invader (make-invader -5 5 -5)) (make-invader 0 5 5))

(define (reverse-invader i)
  (cond [(> (invader-x i) WIDTH) (make-invader WIDTH (invader-y i) (- (invader-dx i)))]
        [(< (invader-x i) 0) (make-invader 0 (invader-y i) (- (invader-dx i)))]
        [else i]))

;; listofinvader -> listofinvader
;; adds (randomly) new Invaders to the listofinvaders

;; Will cause unforeseen check-expect errors 

(define (random-saucer loi)
  (if (= (random 10) 5)
      (cons (make-invader (random WIDTH) 0 (random 12)) loi)
      loi))

;; listofMissile -> listofMissile
;; Consumes list of Missile and produces progressed list of Missile

(check-expect (tock-missiles empty) empty)

; (define (tock-missiles lom) empty)

(define (tock-missiles lom)
  (cond [(empty? lom) empty]                   
        [else (cons  (progress-missile (first lom))
                     (tock-missiles (rest lom)))]))

;; Missile -> Missile
;; Consumer Missile and produces progressed Missile

(check-expect (progress-missile (make-missile 150 300)) (make-missile 150 290))

(define (progress-missile l)
  (make-missile (missile-x l) (- (missile-y l) MISSILE-SPEED)))

;; listOfMissile listOfInvader --> ListOfMissile listOfInvader
;; Checks and removes invaders and missile that are in collission

(check-expect (collide?
               (make-game
                (list (make-invader 150 300 12) (make-invader 100 100 2))
                (list (make-missile 150 300) (make-missile 200 200))
                T1))
              
              (make-game
               (list (make-invader 100 100 2))
               (list (make-missile 150 300) (make-missile 200 200))
               T1))

(check-expect (collide?
               (make-game
                (list (make-invader 155 305 12) (make-invader 100 100 2))
                (list (make-missile 150 300) (make-missile 200 200))
                T1))
              
              (make-game
               (list (make-invader 100 100 2))
               (list (make-missile 150 300) (make-missile 200 200))
               T1))
                            
(define (collide? g)
  (make-game
   (prune-invaders (game-missiles g) (game-invaders g))
   (game-missiles g)
   (game-tank g)))

;; listofmissile listofinvader -> listofinvader
;; Retrieves one value of listofmissiles to compare with the listofinvaders

(define (prune-invaders lom loi)
  (cond [(empty? lom) loi]
        [else (prune-invaders (rest lom) (check-crash (first lom) loi))]))

(check-expect (check-crash
               (make-missile 150 300)
               (list (make-invader 150 300 12) (make-invader 100 100 2)))
              (list (make-invader 100 100 2))) 

;; missile listofinvader -> listofinvader
;; Removes invaders that 'collide' with missile

(define (check-crash m loi)
  (cond [(empty? loi) empty]
        [(not (and
          (< (abs (- (invader-x (first loi)) (missile-x m))) HIT-RANGE)
          (< (abs (- (invader-y (first loi)) (missile-y m))) HIT-RANGE)))
         (cons (first loi) (check-crash m (rest loi)))]
        [else (check-crash m (rest loi))]))

;; listofmissile -> listofmissile
;; Clears missile from list if outside screen

(check-expect (prune-missiles
               (list
                (make-missile 5 5)
                (make-missile 5 -15)))
               (list (make-missile 5 5)))

; (define (prune-missiles lom) lom)

(define (prune-missiles lom)
    (cond [(empty? lom) empty]
          [(> (missile-y (first lom)) -10)
           (cons (first lom) (prune-missiles (rest lom)))]
          [else (prune-missiles (rest lom))]))

;; Tank -> Tank
;; Reverses Tank if hitting the edge of scene

(check-expect (reverse-tank (make-tank -1 -1))
              (make-tank 0 1))
(check-expect (reverse-tank (make-tank (+ WIDTH 1) 1))
              (make-tank WIDTH -1))

; (define (reverse-tank t) t)

(define (reverse-tank t)
  (cond [(< (tank-x t) 0) (make-tank 0 1)]
        [(> (tank-x t) WIDTH) (make-tank WIDTH -1)]
        [else t]))

;; ===================================
;; RENDERING
;; ===================================

;; Game -> Image
;; Consume game and produces image of current state

(check-expect (render
               (make-game
                (list (make-invader 100 100 2) (make-invader 200 200 5) (make-invader 50 50 -4))
                (list (make-missile 5 5) (make-missile 70 70) (make-missile 300 300))
                (make-tank 50 1)))
              (place-image
               TANK
               50 (- HEIGHT TANK-HEIGHT/2)
              (place-image
               MISSILE
               300 300
              (place-image
               MISSILE
               70 70
              (place-image
               MISSILE
               5 5
              (place-image
               INVADER
               50 50
              (place-image
               INVADER
               200
               200
              (place-image
               INVADER
               100 100
               BACKGROUND))))))))

;(define (render g) (rectangle 0 0 "solid" "white"))

(define (render g)
  (place-image
   TANK
   (tank-x (game-tank g))
   (- HEIGHT TANK-HEIGHT/2)
   (render-missiles (game-missiles g)
                    (render-invaders (game-invaders g)))))


;; listofinvader -> Image
;; Helper Function to render the list of invaders

(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-invaders (rest loi)))]))

;; listofmissile -> Image
;; Helper Function to render the list of missiles

(define (render-missiles lom scene)
  (cond [(empty? lom) scene]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom) scene))]))

;; Game -> Boolean
;; Produces true if invades has reached bottom

(check-expect (lost-helper (list (make-invader 50 (+ 1 HEIGHT) 12)))
              true)

(define (lost? g)
  (lost-helper (game-invaders g)))

;; listofInvader -> Boolean
;; Helper Function that operates on the list of invaders

(define (lost-helper loi)
    (cond [(empty? loi) false]
          [(> (invader-y (first loi)) HEIGHT) true]
          [else (lost-helper (rest loi))]))
                                             
;; Game KeyEvent -> Game
;; Changes state of game when certain buttons are pushed

(define (handle-key g event)
  (cond [(key=? event " ")
         (make-game
          (game-invaders g)
          (cons (make-missile (tank-x (game-tank g)) HEIGHT) (game-missiles g))
          (game-tank g))]
        [(key=? event "right")
         (make-game
          (game-invaders g)
          (game-missiles g)
          (make-tank (tank-x (game-tank g)) 1))]
        [(key=? event "left")
         (make-game
          (game-invaders g)
          (game-missiles g)
          (make-tank (tank-x (game-tank g)) -1))]
        [(key=? event "r")
         (make-game
          empty
          empty
          (game-tank g))]
        [else g]))


