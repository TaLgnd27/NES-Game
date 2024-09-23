  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
gamestate     .rs 1  ; .rs 1 means reserve one byte of space
ballx         .rs 1  ; ball horizontal position
bally         .rs 1  ; ball vertical position
ballup        .rs 1  ; 1 = ball moving up
balldown      .rs 1  ; 1 = ball moving down
ballleft      .rs 1  ; 1 = ball moving left
ballright     .rs 1  ; 1 = ball moving right
ballxspeed    .rs 1
ballyspeed    .rs 1
paddle1ytop   .rs 1  ; player 1 paddle top vertical position
paddle2ytop   .rs 1  ; player 2 paddle top vertical position
paddle1speed  .rs 1  ; player 1 paddle speed
paddle2speed  .rs 1  ; player 2 paddle speed
buttons1      .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2      .rs 1  ; player 2 gamepad buttons, one bit per button
score1        .rs 1  ; byte for each digit in the decimal score
score2        .rs 1
winner        .rs 1
pointerLo     .rs 1   ; pointer variables are declared in RAM
pointerHi     .rs 1   ; low byte first, high byte immediately after


;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
  
RIGHTWALL      = $FB  ; when ball reaches one of these, do something
TOPWALL        = $14
BOTTOMWALL     = $E0
LEFTWALL       = $04
  
PADDLE1X       = $08  ; horizontal position for paddles, doesnt move
PADDLE2X       = $EC



;;;;;;;;;;;;;;;;;;




  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

LoadTitleBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #$00
  STA pointerLo       ; put the low byte of the address of background into pointer
  LDA #HIGH(titleBackground)
  STA pointerHi       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoopTitle:
  
InsideLoopTitle:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoopTitle      ; run the inside loop 256 times before continuing down
  
  INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE OutsideLoopTitle     ; run the outside loop 4 times before continuing down



  


;;;Set some initial ball stats
  LDA #$01
  STA balldown
  STA ballright
  LDA #$00
  STA ballup
  STA ballleft
  
  LDA #$50
  STA bally
  
  LDA #$80
  STA ballx
  
  LDA #$02
  STA ballxspeed
  STA ballyspeed

;;;Set initial paddle stats
  LDA #$71
  STA paddle1ytop
  STA paddle2ytop

  LDA #$02
  STA paddle1speed
  STA paddle2speed


;;;Set initial score value
  LDA #$00
  STA score1
  STA score2


;;:Set starting game state
  LDA #STATETITLE
  STA gamestate


              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00001110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  LDA gamestate
  CMP #STATEPLAYING
  BNE SkipScore
  JSR DrawScore
SkipScore:

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA gamestate    ; check if we are on the game screen and skip enabling of sprites every frame if we are not on the game screen
  CMP #STATEPLAYING
  BNE SkipSprites
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
SkipSprites:
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

UnloadSprites:
  
  ;;;all graphics updates done by here, run game engine


  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2
  
GameEngine:  
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle    ;;game is displaying title screen
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver  ;;game is displaying ending screen
  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying   ;;game is playing
GameEngineDone:  
  
  JSR UpdateSprites  ;;set ball/paddle sprites from positions

  RTI             ; return from interrupt
 
 
 
 
;;;;;;;;
 
EngineTitle:
  LDA buttons1
  AND #%00010000
  BEQ GameEngineDone
  ;;  turn screen off
  LDA #%00000000   ; disable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  STA $2001
LoadGameBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #LOW(gameBackground)
  STA pointerLo       ; put the low byte of the address of background into pointer
  LDA #HIGH(gameBackground)
  STA pointerHi       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoopGame:
  
InsideLoopGame:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoopGame      ; run the inside loop 256 times before continuing down
  
  INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE OutsideLoopGame     ; run the outside loop 4 times before continuing down
  ;;  set starting paddle/ball position
  LDA #STATEPLAYING  ;;  go to Playing State
  STA gamestate  
  ;;  turn screen on
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  JMP GameEngineDone

;;;;;;;;; 
 
EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on 
  JMP GameEngineDone
 
;;;;;;;;;;;
 
EnginePlaying:

MoveBallRight:
  LDA ballright
  BEQ MoveBallRightDone   ;;if ballright=0, skip this section

  LDA ballx
  CLC
  ADC ballxspeed          ;;ballx position = ballx + ballspeed
  STA ballx

  CLC
  LDA ballx
  ADC #$04
  CMP #RIGHTWALL
  BCC MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
  ;;in real game, give point to player 1, reset ball
  jsr IncrementScore1
MoveBallRightDone:


MoveBallLeft:
  LDA ballleft
  BEQ MoveBallLeftDone   ;;if ballleft=0, skip this section

  LDA ballx
  SEC
  SBC ballxspeed          ;;ballx position = ballx - ballspeed
  STA ballx

  SEC
  LDA ballx
  SBC #$04
  CMP #LEFTWALL
  BCS MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
 
  ;;in real game, give point to player 2, reset ball
  jsr IncrementScore2
MoveBallLeftDone:


MoveBallUp:
  LDA ballup
  BEQ MoveBallUpDone   ;;if ballup=0, skip this section

  LDA bally
  SEC
  SBC ballyspeed         ;;bally position = bally - ballspeed
  STA bally

  SEC
  LDA bally
  SBC #$04
  CMP #TOPWALL
  BCS MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
  LDA #$01
  STA balldown
  LDA #$00
  STA ballup         ;;bounce, ball now moving down
MoveBallUpDone:


MoveBallDown:
  LDA balldown
  BEQ MoveBallDownDone   ;;if ballup=0, skip this section

  LDA bally
  CLC
  ADC ballyspeed         ;;bally position = bally + ballspeed 
  STA bally

  CLC
  LDA bally
  ADC #$08
  CMP #BOTTOMWALL
  BCC MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
  LDA #$00
  STA balldown
  LDA #$01
  STA ballup         ;;bounce, ball now moving down
MoveBallDownDone:

MovePaddle1Up:
  LDA buttons1
  AND #%00001000
  BEQ MovePaddle1UpDone
  LDA paddle1ytop
  SEC
  SBC paddle1speed
  STA paddle1ytop
  SEC
  SBC #$04
  CMP #TOPWALL
  BCS MovePaddle1UpDone
  CLC
  LDA #TOPWALL
  ADC #$04
  STA paddle1ytop
MovePaddle1UpDone:

MovePaddle1Down:
  LDA buttons1
  AND #%00000100
  BEQ MovePaddle1DownDone
  LDA paddle1ytop
  CLC
  ADC paddle1speed
  STA paddle1ytop
  CLC
  ADC #$18
  CMP #BOTTOMWALL
  BCC MovePaddle1DownDone
  SEC
  LDA #BOTTOMWALL
  SBC #$18
  STA paddle1ytop
MovePaddle1DownDone:

MovePaddle2Up:
  LDA buttons2
  AND #%00001000
  BEQ MovePaddle2UpDone
  LDA paddle2ytop
  SEC
  SBC paddle2speed
  STA paddle2ytop
  SEC
  SBC #$04
  CMP #TOPWALL
  BCS MovePaddle2UpDone
  CLC
  LDA #TOPWALL
  ADC #$04
  STA paddle2ytop
MovePaddle2UpDone:

MovePaddle2Down:
  LDA buttons2
  AND #%00000100
  BEQ MovePaddle2DownDone
  LDA paddle2ytop
  CLC
  ADC paddle2speed
  STA paddle2ytop
  CLC
  ADC #$18
  CMP #BOTTOMWALL
  BCC MovePaddle2DownDone
  SEC
  LDA #BOTTOMWALL
  SBC #$18
  STA paddle2ytop
MovePaddle2DownDone:
  
CheckPaddle1Collision:
  SEC
  LDA ballx
  SBC #$08
  CMP #PADDLE1X
  BNE CheckPaddle1CollisionDone
  SEC
  LDA paddle1ytop
  SBC #$08
  CMP bally
  BPL CheckPaddle1CollisionDone
  CLC
  LDA paddle1ytop
  ADC #$18
  CMP bally
  BMI CheckPaddle1CollisionDone
  LDA #$01
  STA ballright
  LDA #$00
  STA ballleft         ;;bounce, ball now moving right
  
CheckPaddle1CollisionDone:

CheckPaddle2Collision:
  CLC
  LDA ballx
  ADC #$04
  CMP #PADDLE2X
  BNE CheckPaddle2CollisionDone
  SEC
  LDA paddle2ytop
  SBC #$08
  CMP bally
  BPL CheckPaddle2CollisionDone
  CLC
  LDA paddle2ytop
  ADC #$18
  CMP bally
  BMI CheckPaddle2CollisionDone
  LDA #$01
  STA ballleft
  LDA #$00
  STA ballright         ;;bounce, ball now moving right
  
CheckPaddle2CollisionDone:

  ;;check for a winner
  LDA score1
  CMP #$05
  BMI GameOver1
  LDA #$01
  STA winner
  LDA #%00000000   ; disable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  STA $2001
LoadGameOverBackground1:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #LOW(gameOverBackground)
  STA pointerLo       ; put the low byte of the address of background into pointer
  LDA #HIGH(gameOverBackground)
  STA pointerHi       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoopGameOver1:
  
InsideLoopGameOver1:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoopGameOver1      ; run the inside loop 256 times before continuing down
  
  INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE OutsideLoopGameOver1     ; run the outside loop 4 times before continuing down
  ;;  set starting paddle/ball position
  LDA #STATEGAMEOVER  ;;  go to Playing State
  STA gamestate  
  ;;  turn screen on
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
GameOver1:
  LDA score2
  CMP #$05
  BMI GameOver2
  LDA #$02
  STA winner
  LDA #%00000000   ; disable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  STA $2001
LoadGameOverBackground2:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #LOW(gameOverBackground)
  STA pointerLo       ; put the low byte of the address of background into pointer
  LDA #HIGH(gameOverBackground)
  STA pointerHi       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoopGameOver2:
  
InsideLoopGameOver2:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoopGameOver2      ; run the inside loop 256 times before continuing down
  
  INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE OutsideLoopGameOver2     ; run the outside loop 4 times before continuing down
  ;;  set starting paddle/ball position
  LDA #STATEGAMEOVER  ;;  go to Playing State
  STA gamestate  
  ;;  turn screen on
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00000110   ; enable sprites, enable background, no clipping on left side
  STA $2001
GameOver2:
  JMP GameEngineDone
 
UpdateSprites:
  LDA bally  ;;update all ball sprite info
  STA $0200
  
  LDA #$00
  STA $0201
  
  LDA #$00
  STA $0202
  
  LDA ballx
  STA $0203
  
  ;;update paddle1 sprites
  LDA paddle1ytop
  STA $0204
  CLC 
  ADC #$07
  STA $0208
  CLC 
  ADC #$07
  STA $020C

  LDA #$01
  STA $0205

  LDA #$02
  STA $0209

  LDA #$03
  STA $020D

  LDA #%00000001
  STA $0206
  STA $020A
  STA $020E

  LDA #PADDLE1X
  STA $0207
  STA $020B
  STA $020F

  ;;update paddle2 sprites
  LDA paddle2ytop
  STA $0210
  CLC 
  ADC #$07
  STA $0214
  CLC 
  ADC #$07
  STA $0218

  LDA #$01
  STA $0211

  LDA #$02
  STA $0215

  LDA #$03
  STA $0219

  LDA #%00000010
  STA $0212
  STA $0216
  STA $021A

  LDA #PADDLE2X
  STA $0213
  STA $0217
  STA $021B
  RTS
 



 
DrawScore:
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$43
  STA $2006          ; start drawing the score at PPU $2043
  
  LDA score1      ; last digit
;  CLC
;  ADC #$30           ; add ascii offset
  STA $2007

  LDA $2002
  LDA #$20
  STA $2006
  LDA #$5D
  STA $2006          ; start drawing the score at PPU $205D
  
  LDA score2      ; last digit
;  CLC
;  ADC #$30           ; add ascii offset
  STA $2007
  RTS
 
 
IncrementScore1:
  LDA score1      ; load the lowest digit of the number
  CLC 
  ADC #$01           ; add one
  STA score1
  RTS

IncrementScore2:
  LDA score2      ; load the lowest digit of the number
  CLC 
  ADC #$01           ; add one
  STA score2
  RTS



 
ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
  
ReadController2:
  LDA #$01
  STA $4017
  LDA #$00
  STA $4017
  LDX #$08
ReadController2Loop:
  LDA $4017
  LSR A            ; bit0 -> Carry
  ROL buttons2     ; bit0 <- Carry
  DEX
  BNE ReadController2Loop
  RTS  
    
        
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000

titleBackground:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;Cut Off in NTSC

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;Sometimes cut off by TV

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$36,$34,$32,$24,$24,$36,$34,$32,$24  ;;row 13
  .db $24,$37,$38,$3D,$24,$24,$36,$3E,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$31,$34,$33,$24,$24,$3C,$24,$3C,$24  ;;row 14
  .db $24,$3C,$39,$3C,$24,$24,$3C,$3F,$32,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$30,$24,$24,$24,$24,$35,$34,$33,$24  ;;row 15
  .db $24,$30,$3A,$3B,$24,$24,$35,$34,$33,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36,$34  ;;row 17
  .db $34,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$30,$24  ;;row 18
  .db $41,$42,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41  ;;row 19
  .db $42,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$42  ;;row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$40,$34  ;;row 21
  .db $34,$3E,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$19,$1B,$0E,$1C,$1C,$24  ;;row 23
  .db $24,$1C,$1D,$0A,$1B,$1D,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 27
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 28
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 29
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 30
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

attributesTitle:  ;8 x 8 = 64 bytes
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101

gameBackground:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;Cut Off in NTSC

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;Sometimes cut off by TV

  .db $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25  ;;row 3
  .db $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 4
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 5
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 6
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 7
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 8
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 9
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 10
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 11
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 12
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 13
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 14
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 15
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 16
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 17
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 18
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 19
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 20
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 21
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 22
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 23
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 24
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 25
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 26
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 27
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$26  ;;row 28
  .db $27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28  ;;row 29
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 30
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;


attributesGame:  ;8 x 8 = 64 bytes
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101

gameOverBackground:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;Cut Off in NTSC

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;Sometimes cut off by TV

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 14
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$19,$24,$15,$24,$0A,$24,$22  ;;row 15
  .db $24,$0E,$24,$1B,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 17
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 18
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 19
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 21
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 23
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 27
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 28
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 29
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 30
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;

attributesGameOver:  ;8 x 8 = 64 bytes
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101
  .db %00000000, %00000000, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101

  .db $24,$24,$24,$24, $24,$24,$24,$24 
  .db $24,$24,$24,$24, $24,$24,$24,$24 
  .db $24,$24,$24,$24, $24,$24,$24,$24
  .db $24,$24,$24,$24, $24,$24,$24,$24  ;;brick bottoms
  .db $24,$24,$24,$24, $24,$24,$24,$24 
  .db $24,$24,$24,$24, $24,$24,$24,$24
  .db $24,$24,$24,$24, $24,$24,$24,$24 

palette:
  .db $0F,$30,$15,$02,  $0F,$30,$11,$06,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $0F,$30,$10,$00,  $0F,$30,$06,$15,  $22,$30,$02,$11,  $22,$02,$38,$3C   ;;sprite palette

sprites:
     ;vert tile attr      horiz
  .db $80, $00, %00000000, $80   ;ball sprite\

  .db $71, $01, %00000001, PADDLE1X ;p1 paddle top
  .db $80, $02, %00000001, PADDLE1X ;p1 paddle middle
  .db $8E, $03, %00000001, PADDLE1X ;p1 paddle bottom

  .db $71, $01, %00000010, PADDLE2X ;p2 paddle top
  .db $80, $02, %00000010, PADDLE2X ;p2 paddle middle
  .db $8E, $03, %00000010, PADDLE2X ;p2 paddle bottom



  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1