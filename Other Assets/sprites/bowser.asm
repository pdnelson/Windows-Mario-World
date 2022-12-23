;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bowser, by mikeyk
;;
;; Description: SMB1 style
;;
;; Uses first extra bit: YES
;; If the first extra bit is set, Bowser will throw hammers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    JUMP_TIMER = $163E
                    HAMMER_TIMER = $15AC
                    FIRE_TIMER = $1528
                    DIR_TIMER = $1540
                    SPRITE_DIR = $157C
                    SPRITE_IMG = $1602
                    SPRITE_STATUS = $14C8
                    SPRITE_STATE = $151C
                    
                    EXTRA_BITS = $7FAB10                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; init JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
                    JSR SUB_GET_DIR
                    TYA
                    STA SPRITE_DIR,x

                    STZ SPRITE_IMG,x

                    LDA EXTRA_BITS,x
                    AND #$04
                    BEQ NO_SET_TIME
                    LDA #$48
                    STA HAMMER_TIMER,x
NO_SET_TIME                    
                    TXA
                    AND #$03
                    ASL A
                    ASL A
                    ASL A
                    ASL A
                    PHA
                    ASL A
                    ADC #$4
                    STA FIRE_TIMER,x
                    
                    PLA
                    ADC #$00
                    STA JUMP_TIMER,x

                    RTL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main sprite JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "MAIN"
HAMMER_BRO_JSL      PHB                     ; \
                    PHK                     ;  | main sprite function, just calls local subroutine
                    PLB                     ;  |
	            JSR DecrementTimers
                    JSR START_HB_CODE       ;  |
                    PLB                     ;  |
                    RTL                     ; /

DecrementTimers:
	LDA $14C8,x
	CMP #$08
	BNE DoneDec
	LDA $9D
	BNE DoneDec
	LDA FIRE_TIMER,x
	BEQ DoneDec
	DEC FIRE_TIMER,x
DoneDec:
	RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main sprite routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    
                    TIME_TO_LOCK = $28
                    TIME_TO_JUMP = $88
                    TIME_TO_SHOW = $12      ;time to display the boomerang before it is thrown

X_SPEED             dcb $00,$FA,$00,$06     ;rest at bottom, moving up, rest at top, moving down
TIME_IN_POS         dcb $78,$14,$74,$14     ;moving up, rest at top, moving down, rest at bottom
TIME_TILL_THROW     dcb $78,$A8
TIME_TILL_HAMMER    dcb $0C,$0C,$0C,$0C,$0C,$43,$0C,$0C
                    dcb $0C,$0C,$0C,$0C,$73,$0C,$0C,$0C
                    dcb $0C,$0C,$63,$0C,$0C,$0C,$0C,$0C
                    dcb $0C,$83,$0C,$0C,$0C,$0C,$0C,$53

STAR                INC HAMMER_TIMER,x
RETURN              RTS                    
START_HB_CODE       JSR SUB_GFX             ; draw sprite gfx
                    LDA SPRITE_STATUS,x     ; \ if sprite status != 8...
                    CMP #$02                ;  }   ... not (killed with spin jump [4] or star[2])
                    BEQ STAR
                    CMP #$08
                    BNE RETURN              ; /    ... return
                    LDA $9D                 ; \ if sprites locked...
                    BNE RETURN              ; /    ... return

                    JSR SUB_GET_DIR         ; \ always face mario
                    TYA                     ;  | 
                    STA SPRITE_DIR,x        ; /
                    

                    JSR SUB_OFF_SCREEN_X3   ; only process sprite while on screen
                    INC $1570,x             ; increment number of frames sprite has been on screen
                    
                    LDA $1570,x             ; \ calculate which frame to show:
                    LSR A                   ;  | 
                    LSR A                   ;  | 
                    LSR A                   ;  | 
                    LSR A                   ;  | 
                    AND #$01                ;  | update every 16 cycles if normal
                    STA SPRITE_IMG,x        ; / write frame to show

                    LDA FIRE_TIMER,x        ; \ if time until spit >= $10
                    CMP #TIME_TO_SHOW       ;  |   just go to normal walking code
                    BCS TRY_HAMMER          ; /
                    INC SPRITE_IMG,x 
                    INC SPRITE_IMG,x 
                
                    LDA FIRE_TIMER,x             ; throw fire if it's time
                    BNE NO_RESET
                    LDY $C2,x
                    LDA TIME_TILL_THROW,y
                    STA FIRE_TIMER,x
NO_RESET            CMP #$01
                    BNE TRY_HAMMER
                    LDA $C2,x
                    EOR #$01
                    STA $C2,x
                    JSR SUB_FIRE_THROW
                    
TRY_HAMMER          LDA EXTRA_BITS,x       ; only throw hammers if first extra bit is set
                    AND #$04
                    BEQ JUMP_SPRITE
                    LDA HAMMER_TIMER,x     ; throw hammer if it's time
                    BNE NO_RESET2
                    LDA $1504,x
                    TAY
                    LDA TIME_TILL_HAMMER,y
                    STA HAMMER_TIMER,x
NO_RESET2           CMP #$01
                    BNE JUMP_SPRITE
                    LDA $1504,x
                    INC A
                    AND #$1F
                    STA $1504,x
                    JSR SUB_HAMMER_THROW 

JUMP_SPRITE         LDA JUMP_TIMER,x
                    CMP #$40                ;  |   just go to normal walking code
                    BCS WALK_SPRITE         ; /
                    LDA SPRITE_IMG,x             ; \ lock image while jumping
                    ORA #$01                ;  |
                    STA SPRITE_IMG,x             ; /
                    INC DIR_TIMER,x             ; we didn't move the sprite this frame, so we don't want a decrement
                    STZ $B6,x               ; stop sprite from moving
                    LDA JUMP_TIMER,x
                    CMP #$38
                    BNE NO_JUMP2
                    LDA #$B0                ; \  y speed
                    STA $AA,x               ; /
                    BRA APPLY_SPEED
NO_JUMP2            CMP #$00
                    BNE APPLY_SPEED
                    LDA #TIME_TO_JUMP
                    STA JUMP_TIMER,x

                    BRA APPLY_SPEED

WALK_SPRITE         LDA $151C,x             ;
                    TAY                     ;
                    LDA DIR_TIMER,x             ;
                    BEQ CHANGE_SPEED        ;
                    LDA X_SPEED,y           ; | set y speed
                    STA $B6,x               ; /
                    BRA APPLY_SPEED
                    
CHANGE_SPEED        LDA TIME_IN_POS,y       ;A:0001 X:0007 Y:0000 D:0000 DB:01 S:01F5 P:envMXdiZCHC:0654 VC:057 00 FL:24235
                    STA DIR_TIMER,x             ;A:0020 X:0007 Y:0000 D:0000 DB:01 S:01F5 P:envMXdizCHC:0686 VC:057 00 FL:24235
                    LDA $151C,x
                    INC A
                    AND #$03
                    STA $151C,x
                    
APPLY_SPEED         JSL $01802A             ; update position based on speed values

CHECK_TOUCH         LDA $1588,x             ; \ if sprite is touching the side of an object...
                    AND #$03                ;  |
                    BEQ DONT_CHANGE_DIR     ;  |
                    LDA $151C,x
                    INC A
                    AND #$03
                    STA $151C,x
                    
DONT_CHANGE_DIR     JSL $018032             ; interact with other sprites               
                    JSL $01A7DC             ; check for mario/sprite contact

NO_CONTACT          RTS                     ; return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fire routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

XF_OFFSET            dcb $13,$FC
XF_OFFSET2           dcb $00,$FF

RETURN68            RTS     
SUB_FIRE_THROW      ;LDA $15A0,x            ; no fire if off screen
                    LDA $186C,x             
                    ORA $15D0,x
                    BNE RETURN68
                    
                    JSL $02A9DE             ; \ get an index to an unused sprite slot, return if all slots full
                    BMI RETURN68            ; / after: Y has index of sprite being generated

                    LDA #$01                ; \ set sprite status for new sprite
                    STA SPRITE_STATUS,y             ; /

                    LDA #$B3
                    STA $9E,y

                    PHY                     ; set x position for new sprite
                    LDA SPRITE_DIR,x
                    TAY
                    LDA $E4,x               
                    CLC
                    ADC XF_OFFSET,y
                    PLY
                    STA $00E4,y
    
                    PHY                     ; set x position for new sprite
                    LDA SPRITE_DIR,x
                    TAY
                    LDA $14E0,x             
                    ADC XF_OFFSET2,y
                    PLY
                    STA $14E0,y      

                    LDA $D8,x               ; \ set y position for new sprite
                    SEC                     ;  | (y position of generator - 1)
                    SBC #$03                ;  |
                    STA $00D8,y             ;  |
                    LDA $14D4,x             ;  |
                    SBC #$00                ;  |
                    STA $14D4,y             ; /

                    PHX                     ; \ before: X must have index of sprite being generated
                    TYX                     ;  | routine clears *all* old sprite values...
                    JSL $07F7D2             ;  | ...and loads in new values for the 6 main sprite tables
                    PLX                     ; / 

                    LDA #$17
                    STA $1DFC

RETURN67            RTS                     ; return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hammer routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

X_OFFSET            dcb $02,$0C
X_OFFSET2           dcb $00,$00
X_SPEED_HAMMER      dcb $12,$EE

SUB_HAMMER_THROW    LDY #$E8                ;A:0218 X:0009 Y:0009 D:0000 DB:03 S:01E8 P:envMXdizcHC:0522 VC:104 00 FL:19452
                    LDA SPRITE_DIR,x             ;A:0218 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:0538 VC:104 00 FL:19452
                    BNE LABEL9              ;A:0201 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:envMXdizcHC:0570 VC:104 00 FL:19452
                    LDY #$18                ;A:0200 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:envMXdiZcHC:0210 VC:098 00 FL:21239
LABEL9              STY $00                 ;A:0201 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:envMXdizcHC:0592 VC:104 00 FL:19452
                    LDY #$07                ;A:0201 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:envMXdizcHC:0616 VC:104 00 FL:19452
LABEL8              LDA $170B,y             ;A:0201 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0632 VC:104 00 FL:19452
                    BEQ LABEL7              ;A:0200 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdiZcHC:0664 VC:104 00 FL:19452
                    DEY                     ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0088 VC:103 00 FL:19638
                    BPL LABEL8              ;A:0204 X:0009 Y:0006 D:0000 DB:03 S:01E8 P:envMXdizcHC:0102 VC:103 00 FL:19638
                    RTS                     ;
LABEL7              LDA #$04                ;A:0200 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdiZcHC:0686 VC:104 00 FL:19452
                    STA $170B,y             ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0702 VC:104 00 FL:19452

                    PHY
                    LDA SPRITE_DIR,x
                    TAY
                    LDA $E4,x               ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0734 VC:104 00 FL:19452
                    CLC
                    ADC X_OFFSET,y
                    PLY
                    STA $171F,y             ;A:02C9 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:0764 VC:104 00 FL:19452

                    PHY
                    LDA SPRITE_DIR,x
                    TAY
                    LDA $14E0,x             ;  |
                    ADC X_OFFSET2,y
                    PLY
                    STA $1733,y             ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0828 VC:104 00 FL:19452

                    LDA $D8,x               ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0860 VC:104 00 FL:19452
                    CLC
                    ADC #$F0
                    STA $1715,y             ;A:0270 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0890 VC:104 00 FL:19452
                    LDA $14D4,x             ;A:0270 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0922 VC:104 00 FL:19452
                    ADC #$FF
                    STA $1729,y             ;A:0201 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0954 VC:104 00 FL:19452

                    LDA #$D0                ;A:0201 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0986 VC:104 00 FL:19452
                    STA $173D,y             ;A:02C8 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:1002 VC:104 00 FL:19452
                    PHY
                    LDA SPRITE_DIR,x
                    TAY
                    LDA X_SPEED_HAMMER,y
                    ;LDA #$F0                 ;A:02C8 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:1034 VC:104 00 FL:19452
                    PLY
                    STA $1747,y             ;A:02E8 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:1058 VC:104 00 FL:19452
                    BRA LABEL10             ;A:02E8 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:1090 VC:104 00 FL:19452
LABEL6              RTS                     ;A:0101 X:0009 Y:0009 D:0000 DB:03 S:01E8 P:envMXdizcHC:0248 VC:086 00 FL:19396
LABEL10             LDA $1715,y             ;A:02E8 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:1112 VC:104 00 FL:19452
                    ADC #$F7                ;A:0270 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:1144 VC:104 00 FL:19452
                    STA $1715,y             ;A:0267 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizCHC:1160 VC:104 00 FL:19452
                    BCC LABEL11             ;A:0267 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizCHC:1192 VC:104 00 FL:19452
                    LDA $1729,y             ;A:0267 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizCHC:1208 VC:104 00 FL:19452
                    ADC #$FF                ;A:0201 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizCHC:1240 VC:104 00 FL:19452
                    STA $1729,y             ;A:0201 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizCHC:1256 VC:104 00 FL:19452
LABEL11             RTS                     ;A:0220 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizCHC:1336 VC:104 00 FL:19452



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TILEMAP             dcb $00,$02,$27,$29,$00,$06,$4C,$4E,$04,$02,$27,$29,$04,$06,$4C,$4E
                    dcb $00,$02,$27,$29,$00,$06,$4C,$4E,$04,$02,$27,$29,$04,$06,$4C,$4E
HORIZ_DISP          dcb $00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10
                    dcb $10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00
VERT_DISP           dcb $F0,$F0,$00,$00,$F0,$F0,$00,$00,$F0,$F0,$00,$00,$F0,$F0,$00,$00
                    dcb $F0,$F0,$00,$00,$F0,$F0,$00,$00,$F0,$F0,$00,$00,$F0,$F0,$00,$00
PROPERTIES          dcb $40,$00             ;xyppccct format

                    HAMMER_TILE = $6D


SUB_GFX             JSR GET_DRAW_INFO       ; after: Y = index to sprite tile map ($300)
                                            ;      $00 = sprite x position relative to screen boarder 
                                            ;      $01 = sprite y position relative to screen boarder  
                    LDA SPRITE_IMG,x             ; \
                    ASL A                   ;  | $03 = index to frame start (frame to show * 2 tile per frame)
                    ASL A
                    STA $03                 ; /
                    LDA SPRITE_DIR,x             ; \ $02 = sprite direction
                    STA $02                 ; /
                    BNE NO_ADD
                    LDA $03
                    CLC
                    ADC #$10
                    STA $03
NO_ADD              PHX                     ; push sprite index

                    LDX #$03                ; loop counter = (number of tiles per frame) - 1
LOOP_START          PHX                     ; push current tile number
                    TXA                     ; \ X = index to horizontal displacement
                    ORA $03                 ; / get index of tile (index to first tile of frame + current tile number)
FACING_LEFT         TAX                     ; \ 
                    
                    LDA $00                 ; \ tile x position = sprite x location ($00)
                    CLC                     ;  |
                    ADC HORIZ_DISP,x         ;  |
                    STA $0300,y             ; /
                    
                    LDA $01                 ; \ tile y position = sprite y location ($01) + tile displacement
                    CLC                     ;  |
                    ADC VERT_DISP,x         ;  |
                    STA $0301,y             ; /
                    
                    LDA TILEMAP,x           ; \ store tile
                    STA $0302,y             ; / 
        
                    LDX $02                 ; \
                    LDA PROPERTIES,x        ;  | get tile properties using sprite direction
                    LDX $15E9               ;  |
                    ORA $15F6,x             ;  | get palette info
                    ORA $64                 ;  | put in level properties
                    STA $0303,y             ; / store tile properties
                    
                    PLX                     ; \ pull, X = current tile of the frame we're drawing
                    INY                     ;  | increase index to sprite tile map ($300)...
                    INY                     ;  |    ...we wrote 1 16x16 tile...
                    INY                     ;  |    ...sprite OAM is 8x8...
                    INY                     ;  |    ...so increment 4 times
                    DEX                     ;  | go to next tile of frame and loop
                    BPL LOOP_START          ; / 

                    PLX                     ; pull, X = sprite index
                    
                    LDA HAMMER_TIMER,x
                    CMP #$02
                    BCC NO_SHOW_HAMMER
                    CMP #30
                    BCS NO_SHOW_HAMMER
                    LDA SPRITE_IMG,x
                    CMP #$00
                    BCS SHOW_HAMMER_TOO
                    
NO_SHOW_HAMMER      LDY #$02                ; \ 02, because we didn't write to 460 yet
                    LDA #$03                ;  | A = number of tiles drawn - 1
                    JSL $01B7B3             ; / don't draw if offscreen
                    RTS                     ; return

SHOW_HAMMER_TOO     PHX
                    
                    LDA $00
                    LDX $02
                    CLC
                    ADC X_OFFSET,x
                    STA $0300,y
                    
                    LDA $01                 ; \ tile y position = sprite y location ($01) + tile displacement
                    CLC                     ;  |
                    ADC #$E8
                    STA $0301,y             ; /
                    
                    LDA #HAMMER_TILE                ; \ store tile
                    STA $0302,y             ; / 

                    LDA #$07 
                    CPX #$00
                    BNE NO_FLIP_HAMMER     
                    ORA #$40
NO_FLIP_HAMMER      ORA $64                 ;  | put in level properties
                    STA $0303,y             ; / store tile properties
                    
                    PLX
                    INY                     ;  | increase index to sprite tile map ($300)...
                    INY                     ;  |    ...we wrote 1 16x16 tile...
                    INY                     ;  |    ...sprite OAM is 8x8...
                    INY                     ;  |    ...so increment 4 times

                    LDY #$02                ; \ 02, because we didn't write to 460 yet
                    LDA #$04                ;  | A = number of tiles drawn - 1
                    JSL $01B7B3             ; / don't draw if offscreen
                    RTS                     ; return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; routines below can be shared by all sprites.  they are ripped from original
; SMW and poorly documented
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $B760 - graphics routine helper - shared
; sets off screen flags and sets index to OAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B75C

TABLE1              dcb $0C,$1C
TABLE2              dcb $01,$02

GET_DRAW_INFO       STZ $186C,x             ; reset sprite offscreen flag, vertical
                    STZ $15A0,x             ; reset sprite offscreen flag, horizontal
                    LDA $E4,x               ; \
                    CMP $1A                 ;  | set horizontal offscreen if necessary
                    LDA $14E0,x             ;  |
                    SBC $1B                 ;  |
                    BEQ ON_SCREEN_X         ;  |
                    INC $15A0,x             ; /

ON_SCREEN_X         LDA $14E0,x             ; \
                    XBA                     ;  |
                    LDA $E4,x               ;  |
                    REP #$20                ;  |
                    SEC                     ;  |
                    SBC $1A                 ;  | mark sprite invalid if far enough off screen
                    CLC                     ;  |
                    ADC.W #$0040            ;  |
                    CMP.W #$0180            ;  |
                    SEP #$20                ;  |
                    ROL A                   ;  |
                    AND #$01                ;  |
                    STA $15C4,x             ; / 
                    BNE INVALID             ; 
                    
                    LDY #$00                ; \ set up loop:
                    LDA $1662,x             ;  | 
                    AND #$20                ;  | if not smushed (1662 & 0x20), go through loop twice
                    BEQ ON_SCREEN_LOOP      ;  | else, go through loop once
                    INY                     ; / 
ON_SCREEN_LOOP      LDA $D8,x               ; \ 
                    CLC                     ;  | set vertical offscreen if necessary
                    ADC TABLE1,y            ;  |
                    PHP                     ;  |
                    CMP $1C                 ;  | (vert screen boundry)
                    ROL $00                 ;  |
                    PLP                     ;  |
                    LDA $14D4,x             ;  | 
                    ADC #$00                ;  |
                    LSR $00                 ;  |
                    SBC $1D                 ;  |
                    BEQ ON_SCREEN_Y         ;  |
                    LDA $186C,x             ;  | (vert offscreen)
                    ORA TABLE2,y            ;  |
                    STA $186C,x             ;  |
ON_SCREEN_Y         DEY                     ;  |
                    BPL ON_SCREEN_LOOP      ; /

                    LDY $15EA,x             ; get offset to sprite OAM
                    LDA $E4,x               ; \ 
                    SEC                     ;  | 
                    SBC $1A                 ;  | $00 = sprite x position relative to screen boarder
                    STA $00                 ; / 
                    LDA $D8,x               ; \ 
                    SEC                     ;  | 
                    SBC $1C                 ;  | $01 = sprite y position relative to screen boarder
                    STA $01                 ; / 
                    RTS                     ; return

INVALID             PLA                     ; \ return from *main gfx routine* subroutine...
                    PLA                     ;  |    ...(not just this subroutine)
                    RTS                     ; /

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $B817 - horizontal mario/sprite check - shared
; Y = 1 if mario left of sprite??
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B817                ; Y = 1 if contact

SUB_GET_DIR         LDY #$00                ;A:25D0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1020 VC:097 00 FL:31642
                    LDA $94                 ;A:25D0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:envMXdiZCHC:1036 VC:097 00 FL:31642
                    SEC                     ;A:25F0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1060 VC:097 00 FL:31642
                    SBC $E4,x               ;A:25F0 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1074 VC:097 00 FL:31642
                    STA $0F                 ;A:25F4 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1104 VC:097 00 FL:31642
                    LDA $95                 ;A:25F4 X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1128 VC:097 00 FL:31642
                    SBC $14E0,x             ;A:2500 X:0006 Y:0000 D:0000 DB:03 S:01ED P:envMXdiZcHC:1152 VC:097 00 FL:31642
                    BPL LABEL16             ;A:25FF X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1184 VC:097 00 FL:31642
                    INY                     ;A:25FF X:0006 Y:0000 D:0000 DB:03 S:01ED P:eNvMXdizcHC:1200 VC:097 00 FL:31642
LABEL16             RTS                     ;A:25FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:1214 VC:097 00 FL:31642


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $B85D - off screen processing code - shared
; sprites enter at different points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B83B             

TABLE3              dcb $40,$B0
TABLE6              dcb $01,$FF 
TABLE4              dcb $30,$C0,$A0,$80,$A0,$40,$60,$B0 
TABLE5              dcb $01,$FF,$01,$FF,$01,$00,$01,$FF

SUB_OFF_SCREEN_X0   LDA #$06                ; \ entry point of routine determines value of $03
                    BRA STORE_03            ;  | 
SUB_OFF_SCREEN_X1   LDA #$04                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X2   LDA #$02                ;  |
STORE_03            STA $03                 ;  |
                    BRA START_SUB           ;  |
SUB_OFF_SCREEN_X3   STZ $03                 ; /

START_SUB           JSR SUB_IS_OFF_SCREEN   ; \ if sprite is not off screen, return
                    BEQ RETURN_2            ; /    
                    LDA $5B                 ; \  goto VERTICAL_LEVEL if vertical level
                    AND #$01                ;  |
                    BNE VERTICAL_LEVEL      ; /     
                    LDA $D8,x               ; \
                    CLC                     ;  | 
                    ADC #$50                ;  | if the sprite has gone off the bottom of the level...
                    LDA $14D4,x             ;  | (if adding 0x50 to the sprite y position would make the high byte >= 2)
                    ADC #$00                ;  | 
                    CMP #$02                ;  | 
                    BPL ERASE_SPRITE        ; /    ...erase the sprite
                    LDA $167A,x             ; \ if "process offscreen" flag is set, return
                    AND #$04                ;  |
                    BNE RETURN_2            ; /
                    LDA $13                 ; \ 
                    AND #$01                ;  | 
                    ORA $03                 ;  | 
                    STA $01                 ;  |
                    TAY                     ; /
                    LDA $1A                 ;x boundry ;A:0101 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0256 VC:090 00 FL:16953
                    CLC                     ;A:0100 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0280 VC:090 00 FL:16953
                    ADC TABLE4,y            ;A:0100 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0294 VC:090 00 FL:16953
                    ROL $00                 ;A:01C0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizcHC:0326 VC:090 00 FL:16953
                    CMP $E4,x               ;x pos ;A:01C0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizcHC:0364 VC:090 00 FL:16953
                    PHP                     ;A:01C0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:0394 VC:090 00 FL:16953
                    LDA $1B                 ;x boundry hi ;A:01C0 X:0006 Y:0001 D:0000 DB:03 S:01EC P:eNvMXdizCHC:0416 VC:090 00 FL:16953
                    LSR $00                 ;A:0100 X:0006 Y:0001 D:0000 DB:03 S:01EC P:envMXdiZCHC:0440 VC:090 00 FL:16953
                    ADC TABLE5,y            ;A:0100 X:0006 Y:0001 D:0000 DB:03 S:01EC P:envMXdizcHC:0478 VC:090 00 FL:16953
                    PLP                     ;A:01FF X:0006 Y:0001 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0510 VC:090 00 FL:16953
                    SBC $14E0,x             ;x pos high ;A:01FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:0538 VC:090 00 FL:16953
                    STA $00                 ;A:01FE X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:0570 VC:090 00 FL:16953
                    LSR $01                 ;A:01FE X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:0594 VC:090 00 FL:16953
                    BCC LABEL20             ;A:01FE X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZCHC:0632 VC:090 00 FL:16953
                    EOR #$80                ;A:01FE X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZCHC:0648 VC:090 00 FL:16953
                    STA $00                 ;A:017E X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0664 VC:090 00 FL:16953
LABEL20             LDA $00                 ;A:017E X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0688 VC:090 00 FL:16953
                    BPL RETURN_2            ;A:017E X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0712 VC:090 00 FL:16953
ERASE_SPRITE        LDA $14C8,x             ; \ if sprite status < 8, permanently erase sprite
                    CMP #$08                ;  |
                    BCC KILL_SPRITE         ; /
                    LDY $161A,x             ;A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZCHC:0140 VC:071 00 FL:21152
                    CPY #$FF                ;A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0172 VC:071 00 FL:21152
                    BEQ KILL_SPRITE         ;A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0188 VC:071 00 FL:21152
                    LDA #$00                ; \ mark sprite to come back    A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0204 VC:071 00 FL:21152
                    STA $1938,y             ; /                             A:FF00 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0220 VC:071 00 FL:21152
KILL_SPRITE         STZ $14C8,x             ; erase sprite
RETURN_2            RTS                     ; return

VERTICAL_LEVEL      LDA $167A,x             ; \ if "process offscreen" flag is set, return
                    AND #$04                ;  |
                    BNE RETURN_2            ; /
                    LDA $13                 ; \ only handle every other frame??
                    LSR A                   ;  | 
                    BCS RETURN_2            ; /
                    AND #$01                ;A:0227 X:0006 Y:00EC D:0000 DB:03 S:01ED P:envMXdizcHC:0228 VC:112 00 FL:1142
                    STA $01                 ;A:0201 X:0006 Y:00EC D:0000 DB:03 S:01ED P:envMXdizcHC:0244 VC:112 00 FL:1142
                    TAY                     ;A:0201 X:0006 Y:00EC D:0000 DB:03 S:01ED P:envMXdizcHC:0268 VC:112 00 FL:1142
                    LDA $1C                 ;A:0201 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0282 VC:112 00 FL:1142
                    CLC                     ;A:02BD X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizcHC:0306 VC:112 00 FL:1142
                    ADC TABLE3,y            ;A:02BD X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizcHC:0320 VC:112 00 FL:1142
                    ROL $00                 ;A:026D X:0006 Y:0001 D:0000 DB:03 S:01ED P:enVMXdizCHC:0352 VC:112 00 FL:1142
                    CMP $D8,x               ;A:026D X:0006 Y:0001 D:0000 DB:03 S:01ED P:enVMXdizCHC:0390 VC:112 00 FL:1142
                    PHP                     ;A:026D X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNVMXdizcHC:0420 VC:112 00 FL:1142
                    LDA.W $001D             ;A:026D X:0006 Y:0001 D:0000 DB:03 S:01EC P:eNVMXdizcHC:0442 VC:112 00 FL:1142
                    LSR $00                 ;A:0200 X:0006 Y:0001 D:0000 DB:03 S:01EC P:enVMXdiZcHC:0474 VC:112 00 FL:1142
                    ADC TABLE6,y            ;A:0200 X:0006 Y:0001 D:0000 DB:03 S:01EC P:enVMXdizCHC:0512 VC:112 00 FL:1142
                    PLP                     ;A:0200 X:0006 Y:0001 D:0000 DB:03 S:01EC P:envMXdiZCHC:0544 VC:112 00 FL:1142
                    SBC $14D4,x             ;A:0200 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNVMXdizcHC:0572 VC:112 00 FL:1142
                    STA $00                 ;A:02FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizcHC:0604 VC:112 00 FL:1142
                    LDY $01                 ;A:02FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizcHC:0628 VC:112 00 FL:1142
                    BEQ LABEL22             ;A:02FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0652 VC:112 00 FL:1142
                    EOR #$80                ;A:02FF X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0668 VC:112 00 FL:1142
                    STA $00                 ;A:027F X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0684 VC:112 00 FL:1142
LABEL22             LDA $00                 ;A:027F X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0708 VC:112 00 FL:1142
                    BPL RETURN_2            ;A:027F X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0732 VC:112 00 FL:1142
                    BMI ERASE_SPRITE        ;A:0280 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:0170 VC:064 00 FL:1195

SUB_IS_OFF_SCREEN   LDA $15A0,x             ; \ if sprite is on screen, accumulator = 0 
                    ORA $186C,x             ;  |  
                    RTS                     ; / return

