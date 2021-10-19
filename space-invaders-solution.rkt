;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invaders-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define INVADE-RATE 100)

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

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define Loi0 empty)
(define Loi1 (cons I1 empty))
(define Loi2 (cons I2 (cons I1 empty)))

#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (....(fn-for-invader (first loi))
              (fn-for-loinvader (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct : empty
;; - compound : (cons Invader ListOfInvader)
;; - reference : (first loi) is Invader
;; - self reference : (rest loi) is ListOfInvader 

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of invaders

(define Lom0 empty)
(define Lom1 (cons M1 empty))
(define Lom2 (cons M2 (cons M1 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (....(fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct : empty
;; - compound : (cons Missile ListOfMissile)
;; - reference : (first loi) is Missile
;; - self reference : (rest loi) is ListOfMissile 


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions:

;;GAME -> GAME
;; start the game with (main (make-game  empty empty T0))

(define (main s)
  (big-bang s              ; GAME
    (on-key    handle-key) ; GAME KeyEvent  -> GAME
    (to-draw   render)     ; GAME -> Image
    (on-tick   tock)       ; GAME -> GAME
    ))    

;; GAME KeyEvent  -> GAME
;; tank - changes position when arrrow keys are pressed and fires a missle when space key is pressed

(check-expect (handle-key G1 "left") (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key G1 "right") (make-game empty empty (make-tank 50 1)))
(check-expect (handle-key G1 " ") (make-game empty (cons (make-missile 50 TANK-HEIGHT/2) empty) T1))

;(define (handle-key s ke) (make-game empty empty (make-tank 0 0))) ;stub
 
(define (handle-key s ke)
  (make-game (game-invaders s)
             (fire-missile (game-missiles s)
                           (game-tank s)
                           ke)
             (move-tank (game-tank s)
                        ke)))

;; Helping functions for handle-key

;; Tank KeyEvent -> Tank
;; move tank left and right when arrows are pressed
(check-expect (move-tank T2 "left") T2)                  ; left -> left
(check-expect (move-tank T1 "right") T1)                 ; right-> right
(check-expect (move-tank T2 "right") (make-tank 50 1))   ; left -> right
(check-expect (move-tank T1 "left") (make-tank 50 -1))   ; right -> left

;(define (move-tank t ke) t ) ; stub

(define (move-tank t ke)
  (cond[(and (key=? ke "left")(= 1 (tank-dir t)))
        (make-tank (tank-x t) (-(tank-dir t)))]
       [(and (key=? ke "right")(= -1 (tank-dir t)))
        (make-tank (tank-x t) (-(tank-dir t)))]
       [else
        t]))

;; ListOfMissiles Tank KeyEvent -> ListOfMissills
;; if ke is "" fire a missile at that position
(check-expect (fire-missile empty T0 "a") empty)
(check-expect (fire-missile empty T0 " ") (cons (make-missile (/ WIDTH 2)  TANK-HEIGHT/2) empty))
;(define (fire-missile lom t ke) empty ) ;stub

; template from ListOfMissile
(define (fire-missile lom t ke)
  (cond [(key=? ke " ")
         (cons (make-missile (tank-x t)  TANK-HEIGHT/2) lom)]
        [else
         lom]))


;; GAME -> Image
;; render the game with the invaders, missiles and tank at the appropriete position on screen
;; !!!
(define (render s) BACKGROUND)


;; GAME -> GAME
;; produce the next state of a space invaders game
;; with the next position of the invaders, missiles and tank position
;; tank - moves left and right at the bottom of the screen 1 px per tick
;; invader - 
;; missiles -

(check-expect (tock (make-game empty empty (make-tank 0 -1)))
              (make-game empty empty (tick-tank (make-tank 0 -1))))   ; no invaders

(check-expect (tock (make-game I1 empty (make-tank 0 -1)))
              (make-game (tick-invader I1) empty (tick-tank (make-tank 0 -1))))   ; one invader 

;(check-expect (tock (make-game (list I1 I2) empty (make-tank 0 -1)))
;              (make-game empty empty (tick-tank (make-tank 0 -1)))) ; one invader , mo missle
;
;(check-expect (tock (make-game empty empty (make-tank 0 -1)))
;              (make-game empty empty (tick-tank (make-tank 0 -1))))

;(define (tock s) (make-game empty empty (make-tank 0 1)))

; Used template from Game

(define (tock s)
  (make-game (tick-invaders (game-invaders s))
             (tick-missiles (game-missiles s))
             (tick-tank (game-tank s))))

;; ListOfInvader -> ListOfInvader
;; interp. produce filtered and ticked list of invaders
(check-expect (tick-invaders empty) empty)

(check-expect (tick-invaders (cons (make-invader 150 100 12)
                                   (cons (make-invader 90 HEIGHT)) empty))
              (cons (make-invader (+ 150 INVADER-X-SPEED)
                                  (+ 100 INVADER-Y-SPEED)
                                  12) empty))


; <template as function composition>
(define (tick-invaders loi)
  (inv-on-screen (tick-invaders loi)))

;; ListOfInvader -> ListOfInvader
;; interp. produce list of ticked invaders

(check-expect (tick-invaders empty) empty)

(check-expect (tick-invaders (cons (make-invader 150 100 12) empty))
              (cons (make-invader (+ 150 INVADER-X-SPEED)
                                  (+ 100 INVADER-Y-SPEED)
                                  12)
                    empty))

(check-expect (tick-invaders (cons (make-invader 150 100 -12) empty))
              (cons (make-invader (- 150 INVADER-X-SPEED)
                                  (- 100 INVADER-Y-SPEED)
                                  -12)
                    empty))


;(define (tick-invaders loi) empty) ;stub

; Used template from ListOfInvader
;
;(define (tick-invaders loi)
;  (cond [(empty? loi) empty]
;        [else
;         (cons (tick-invader (first loi))
;               (tick-invaders (rest loi)))]))


;; Invader -> Invader
;; interp. produce the next position of an invador on the screen (x , y coord)
;;         invader moves along x by dx pixels per tick

(check-expect (tick-invader (make-invader 0 0  10))    ; left edge -> right
              (make-invader (+ 0 INVADER-X-SPEED)
                            (+ 0 INVADER-Y-SPEED)
                            10))

(check-expect (tick-invader (make-invader 0 100 -10))    ; left edge, change dir
              (make-invader 0 100  10))

(check-expect (tick-invader (make-invader 50 100 -10))  ; moving left
              (make-invader (- 50  INVADER-X-SPEED)
                            (- 100 INVADER-Y-SPEED)
                            -10))  
(check-expect (tick-invader (make-invader 150 100  12))  ; moving right
              (make-invader (+ 150 INVADER-X-SPEED)
                            (+ 100 INVADER-Y-SPEED)
                            12))   
(check-expect (tick-invader (make-invader WIDTH 100 10))  ; right edge ,change dir
              (make-invader WIDTH
                            (- 100 INVADER-Y-SPEED)
                            -10))    


;(define (tick-invader invader) (make-invader 0 0 0)) ; stub

;; template from Invader

(define (tick-invader invader)
  (cond [ (and (= 0 (invader-x invader))
               (negative? (invader-dx invader)))
          (make-invader (invader-x invader)
                        (invader-y invader)
                        (- (invader-dx invader)))]
        [(= WIDTH (invader-x invader))
         (make-invader (invader-x invader)
                       (- (invader-y invader) INVADER-Y-SPEED)
                       (- (invader-dx invader)))]
        [else
         (if (negative? (invader-dx invader))
             (make-invader (- (invader-x invader) INVADER-X-SPEED)
                           (- (invader-y invader) INVADER-Y-SPEED)
                           (invader-dx invader))
             (make-invader (+ (invader-x invader) INVADER-X-SPEED)
                           (+ (invader-y invader) INVADER-Y-SPEED)
                           (invader-dx invader)))]))

;; ListOfInvaders -> ListOfInvaders
;; produce a list contaning only those invaders in loi that are onscreen
(check-expect (inv-on-screen empty) empty)
(check-expect (inv-on-screen (cons (make-invader 150 100 12)
                                   (cons (make-invader 90 HEIGHT -12)
                                         empty)))
              (cons (make-invader 150 100 12) empty))

;(define (inv-on-screen loi) empty) ;stub

; template from ListOfInvaders
(define (inv-on-screen loi)
  (cond [(empty? loi) empty]
        [else
         (if (i-onscreen? (first loi))
             (cons (first loi) (inv-on-screen (rest loi)))
             (inv-on-screen (rest loi)))]))

;; Invader -> Boolean
;; produce true if Invader dropt off the bottom of MTS
(check-expect (i-onscreen? I1) true)
(check-expect (i-onscreen? I2) true)
(check-expect (i-onscreen? I3) false)

;(define (i-onscreen? invader ) false) ;stub

;template from Invader
(define (i-onscreen? invader)
  (<= 0 (invader-y invader) HEIGHT))



;; Missile -> Missile
;; interp. produce the next missile location in x and y screen coord

(check-expect (next-missile (make-missile  50 20 ))
              (make-missile (+ 50 MISSILE-SPEED) (+ 20 MISSILE-SPEED)))

(check-expect (next-missile (make-missile  (invader-x I1) (invader-y I1)))
              (make-missile 0 0))

;(define (next-missile m) (make-missile 0 0)) ;stub

(define (next-missile m)
  (cond [ (and (= (missile-x m) (invader-x I1))
               (= (missile-y m) (invader-y I1)))
          (make-missile 0 0)]
        [else
         (make-missile (+ (missile-x m) MISSILE-SPEED)
                       (+ (missile-y m) MISSILE-SPEED))]))

;; ListOfMissile -> ListOfMissile
;; interp. produce ticked list of missiles
(check-expect (tick-missiles empty) empty)

(check-expect (tick-missiles (cons M1 empty))
              (cons (make-missile (+ 150 MISSILE-SPEED)
                                  (+ 300 MISSILE-SPEED))
                    empty
                    ))

(check-expect (tick-missiles (cons (make-missile  (invader-x I1) (invader-y I1))
                                   (cons M1 empty)))
              (cons (make-missile 0 0)
                    (cons (make-missile (+ 150 MISSILE-SPEED)
                                        (+ 300 MISSILE-SPEED))
                          empty)))


;(define (tick-missiles lom) empty) ;stub

; Used template from ListOfMissile

(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons(next-missile (first lom))
              (tick-missiles (rest lom)))]))


;; Tank -> Tank
;; produce next position of a tank in x screen coord
(check-expect (tick-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1)) ; middle -> left
(check-expect (tick-tank (make-tank 50 1)) (make-tank (+ 50 TANK-SPEED) 1))   ; middle -> right
(check-expect (tick-tank (make-tank 0 -1)) (make-tank 0 1))                   ; left edge , change dir
(check-expect (tick-tank (make-tank WIDTH 1)) (make-tank WIDTH  -1))           ; right edge , change dir

; (define (tick-tank t ) (make-tank 0 1)) ;stub
;; Template from Tank
(define (tick-tank t)
  (cond [(= 0 (tank-x t)) (make-tank 0 (- (tank-dir t)))]
        [(= WIDTH (tank-x t)) (make-tank WIDTH (- (tank-dir t)))]
        [else
         (if (= 1 (tank-dir t))
             (make-tank (+ (tank-x t) TANK-SPEED ) (tank-dir t))
             (make-tank (- (tank-x t) TANK-SPEED ) (tank-dir t)))]))







