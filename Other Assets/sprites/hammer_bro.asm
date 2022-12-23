;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hammer Brother, by mikeyk
;;
;; Description: This guy walks back and forth, throwing hammers at Mario.
;;
;; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	RAM_ThrowTimer = $1504
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    
                    HAMMER_TILE = $44
                    
TILEMAP             dcb $20,$40,$40,$00
                    dcb $20,$24,$24,$00
                    dcb $22,$42,$42,$00
                    dcb $22,$42,$42,$00

HORZ_DISP           dcb $00,$00,$00
                    dcb $00,$00,$00
VERT_DISP           dcb $F0,$00,$00
TILE_SIZE           dcb $02,$02,$02

PROPERTIES          dcb $40,$00             ;xyppccct format

SPEED_TABLE         dcb $08,$F8             ; speed of hammer bro, right, left

                    TIME_TO_SHOW = $18      ;time to display the boomerang before it is thrown
                    TIME_TILL_THROW = $28

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; init JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
                    PHY
                    JSR SUB_GET_DIR
                    TYA
                    STA $157C,x
                    PLY
                    
                    TXA
                    AND #$01
                    ASL A 
                    ASL A 
                    ASL A 
                    ASL A 
                    CLC
                    ADC #$20
                    STA RAM_ThrowTimer,x
                    ;STA $1570,x
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
	LDA RAM_ThrowTimer,x
	BEQ DoneDec
	DEC RAM_ThrowTimer,x
DoneDec:
	RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main sprite routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

STAR                INC RAM_ThrowTimer,x
RETURN              RTS                    
START_HB_CODE       JSR SUB_GFX             ; draw hammer bro gfx
                    LDA $14C8,x             ; \ if hammer bro status != 8...
                    CMP #$02                ;  }   ... not (killed with spin jump [4] or star[2])
                    BEQ STAR
                    CMP #$08
                    BNE STAR              ; /    ... return
                    LDA $9D                 ; \ if sprites locked...
                    BNE RETURN              ; /    ... return

                    JSR SUB_OFF_SCREEN_HB   ; only process hammer bro while on screen
                    INC $1570,x             ; increment number of frames hammer bro has been on screen
                    LDA $1570,x             ; \ calculate which frame to show:
                    LSR A                   ;  | 
                    LSR A                   ;  | 
                    LSR A                   ;  | 
                    AND #$01                ;  | update every 16 cycles if normal
LABEL3              STA $1602,x             ; / write frame to show

                    LDA $1540,x
                    BEQ DISABLE_NOT_SET
                    CMP #$01
                    BNE DISABLE_NOT_SET
                    LDA $1686,x
                    AND #$7F
                    STA $1686,x

DISABLE_NOT_SET

                    LDA RAM_ThrowTimer,x             ; \ if time until throw < TIME_TO_SHOW
                    CMP #TIME_TO_SHOW       ;  |
                    BCS NO_THROW            ;  | 
                    INC $1602,x             ;  | change image (hammer will be displayed)
                    INC $1602,x             ; /

                    LDA RAM_ThrowTimer,x             ; \ if time until throw = 0
                    BNE NO_TIME_SET         ;  |
                    LDA #TIME_TILL_THROW    ;  | set the timer
                    STA RAM_ThrowTimer,x             ; /
NO_TIME_SET         CMP #$01                ; \ call the hammer routine if the timer is 
                    BNE NO_THROW            ;  | about to tun out
                    JSR SUB_HAMMER_THROW    ; /
NO_THROW             

                    LDA $1588,x             ; \  if sprite is not on ground...
                    AND #$04                ;  }    ...(4 = on ground) ...
                    BEQ NO_JUMP             ; /     ...goto NO_JUMP
                    LDA #$10                ; \  y speed = 10
                    STA $AA,x               ; /

                    JSR SUB_GET_DIR         ; \ always face mario
                    TYA                     ;  | 
                    STA $157C,x             ; /
                    
                    LDA $1570,x             ; \ makes hammer bro jump
                    CLC                     ;  |
                    ADC #$77                ;  |
                    AND #$E7                ;  |
                    BNE NO_JUMP             ;  |
                    
                    JSR SUB_GET_SPEED

                    LDA $1686,x
                    ORA #$80
                    STA $1686,x
                    
        

NO_JUMP             LDA $14                 ; \ set x speed
                    AND #$3F                ;  |A:014C X:0009 Y:0001 D:0000 DB:03 S:01E9 P:envMXdizcHC:1328 VC:089 00 FL:19405
                    BNE LABEL4              ;  |A:0140 X:0009 Y:0001 D:0000 DB:03 S:01E9 P:envMXdizcHC:1344 VC:089 00 FL:19405
                    LDA $151C,x             ;  |
                    EOR #$01                ;  |
                    STA $151C,x             ;  |
LABEL4              LDA $151C,x             ;  |
                    AND #$01                ;  |
                    TAY                     ;  |
                    LDA SPEED_TABLE,y       ;  |
                    STA $B6,x               ; / A:01F9 X:0009 Y:0001 D:0000 DB:03 S:01E9 P:eNvMXdizcHC:0014 VC:090 00 FL:19405
                    
                    JSL $01802A             ; update position based on speed values
                    
                    LDA $1588,x             ; \ if hammer bro is touching the side of an object...
                    AND #$03                ;  |
                    BEQ DONT_CHANGE_DIR     ;  |
                    LDA $151C,x             ;  |
                    EOR #$01                ;  |    ... change hammer bro direction
                    STA $151C,x             ; /

DONT_CHANGE_DIR     JSL $018032             ; interact with other sprites               
                    JSL $01A7DC             ; check for mario/hammer bro contact

NO_CONTACT          RTS                     ; return



JUMP_HEIGHT         dcb $B0,$D0
DISABLE_TIME        dcb $24,$28

SUB_GET_SPEED       LDA $14D4,x
                    XBA
                    LDA $D8,x
                    PHP
                    REP #$30                    
                    CMP.W #$0138
                    BCS JUMP_UP
                    CMP.W #$00F8
                    BCC JUMP_DOWN
                    PLP
                    JSL $01ACF9
                    AND #$01
                    
                    PHY
                    TAY
SET_HEIGHT          LDA DISABLE_TIME,y 
                    STA $1540,x
                    LDA JUMP_HEIGHT,y 
                    STA $AA,x                                   
                    PLY
                    RTS
                    
JUMP_UP             PLP
                    PHY
                    LDY #$00
                    BRA SET_HEIGHT

JUMP_DOWN           PLP
                    PHY
                    LDY #$01
                    BRA SET_HEIGHT                  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hammer routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

X_OFFSET            dcb $F6,$0A
X_OFFSET2           dcb $FF,$00

                    Y_SPEED = $C8

SUB_HAMMER_THROW    LDA $15A0,x             ;A:0100 X:0009 Y:0009 D:0000 DB:03 S:01E8 P:envMXdiZcHC:0130 VC:086 00 FL:19396
                    ORA $186C,x             ;A:0101 X:0009 Y:0009 D:0000 DB:03 S:01E8 P:envMXdizcHC:0162 VC:086 00 FL:19396
                    ORA $15D0,x
                    BNE LABEL6

                    LDY #$EE                ;A:0218 X:0009 Y:0009 D:0000 DB:03 S:01E8 P:envMXdizcHC:0522 VC:104 00 FL:19452
                    LDA $157C,x             ;A:0218 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:0538 VC:104 00 FL:19452
                    BNE LABEL9              ;A:0201 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:envMXdizcHC:0570 VC:104 00 FL:19452
                    LDY #$12                ;A:0200 X:0009 Y:00E8 D:0000 DB:03 S:01E8 P:envMXdiZcHC:0210 VC:098 00 FL:21239
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
                    LDA $157C,x
                    TAY
                    LDA $E4,x               ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0734 VC:104 00 FL:19452
                    CLC
                    ADC X_OFFSET,y
                    PLY
                    STA $171F,y             ;A:02C9 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:0764 VC:104 00 FL:19452
                    PHY
                    LDA $157C,x
                    TAY
                    LDA $14E0,x             ;  |
                    ADC X_OFFSET2,y
                    PLY
                    STA $1733,y             ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0828 VC:104 00 FL:19452
                    
                    LDA $D8,x               ;A:0204 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0860 VC:104 00 FL:19452
                    STA $1715,y             ;A:0270 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0890 VC:104 00 FL:19452
                    LDA $14D4,x             ;A:0270 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0922 VC:104 00 FL:19452
                    STA $1729,y             ;A:0201 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0954 VC:104 00 FL:19452
                    LDA #Y_SPEED            ;A:0201 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:envMXdizcHC:0986 VC:104 00 FL:19452
                    STA $173D,y             ;A:02C8 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:1002 VC:104 00 FL:19452
                    LDA $00                 ;A:02C8 X:0009 Y:0007 D:0000 DB:03 S:01E8 P:eNvMXdizcHC:1034 VC:104 00 FL:19452
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

SUB_GFX             JSR GET_DRAW_INFO       ; after: Y = index to sprite tile map ($300)
                                            ;      $00 = sprite x position relative to screen boarder 
                                            ;      $01 = sprite y position relative to screen boarder  
                    LDA $1602,x             ; \
                    ASL A
                    ASL A                   ;  | $03 = index to frame start (frame to show * 2 tile per frame)
                    STA $03                 ; /
                    LDA $157C,x             ; \ $02 = sprite direction
                    STA $02                 ; /
                    PHX                     ; push sprite index

                    LDX #$02                ; loop counter = (number of tiles per frame) - 1
LOOP_START          PHX                     ; push current tile number
                   
                    PHX
                    TXA
                    LDX $02
                    BNE NO_ADJ
                    CLC
                    ADC #$03
NO_ADJ              TAX                    
                    LDA $00                 ; \ tile x position = sprite x location ($00)
                    CLC
                    ADC HORZ_DISP,x
                    STA $0300,y             ; /                    
                    PLX
                                        
                    LDA $01                 ; \ tile y position = sprite y location ($01) + tile displacement
                    CLC                     ;  |
                    ADC VERT_DISP,x         ;  |
                    STA $0301,y             ; /
                    
                    LDA TILE_SIZE,x
                    PHX
                    PHA
                    TYA                     ; \ get index to sprite property map ($460)...
                    LSR A                   ; |    ...we use the sprite OAM index...
                    LSR A                   ; |    ...and divide by 4 because a 16x16 tile is 4 8x8 tiles
                    TAX                     ; | 
                    PLA
                    STA $0460,x             ; /  
                    PLX                  

                    TXA                     ; \ X = index to horizontal displacement
                    ORA $03                 ; / get index of tile (index to first tile of frame + current tile number)
                    TAX                     ; \ 
                                 
                    LDA TILEMAP,x           ; \ store tile
                    STA $0302,y             ; / 
        
                    LDX $02                 ; \
                    LDA PROPERTIES,x        ;  | get tile properties using sprite direction
                    LDX $15E9               ;  |
                    ORA $15F6,x             ;  | get palette info
                    ORA $64                 ;  | ?? what is in 64, level properties... disable layer priority??
                    STA $0303,y             ; / store tile properties
                    
                    PLX                     ; \ pull, X = current tile of the frame we're drawing
                    INY                     ;  | increase index to sprite tile map ($300)...
                    INY                     ;  |    ...we wrote 1 16x16 tile...
                    INY                     ;  |    ...sprite OAM is 8x8...
                    INY                     ;  |    ...so increment 4 times
                    DEX                     ;  | go to next tile of frame and loop
                    BPL LOOP_START          ; / 

                    PLX                     ; pull, X = sprite index

                    LDA RAM_ThrowTimer,x
                    CMP #$02
                    BCC NO_SHOW_HAMMER
                    CMP #30
                    BCS NO_SHOW_HAMMER
                    LDA $1602,x
                    CMP #$02
                    BCS SHOW_HAMMER_TOO
                    
NO_SHOW_HAMMER      LDY #$FF                ; \ 02, because we didn't write to 460 yet
                    LDA #$02                ;  | A = number of tiles drawn - 1
                    JSL $01B7B3             ; / don't draw if offscreen
                    RTS                     ; return

HAMMER_OFFSET       dcb $F6,$0A

SHOW_HAMMER_TOO     PHX
                    
                    LDA $00
                    LDX $02
                    CLC
                    ADC HAMMER_OFFSET,x
                    STA $0300,y
                    
                    LDA $01                 ; \ tile y position = sprite y location ($01) + tile displacement
                    CLC                     ;  |
                    ADC #$F2
                    STA $0301,y             ; /
                    
                    LDA #HAMMER_TILE                ; \ store tile
                    STA $0302,y             ; / 

                    PHX
                    TYA                     ; \ get index to sprite property map ($460)...
                    LSR A                   ; |    ...we use the sprite OAM index...
                    LSR A                   ; |    ...and divide by 4 because a 16x16 tile is 4 8x8 tiles
                    TAX                     ; | 
                    LDA #$02
                    STA $0460,x             ; /  
                    PLX     

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

                    LDY #$FF                ; \ 02, because we didn't write to 460 yet
                    LDA #$03                ;  | A = number of tiles drawn - 1
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

SUB_OFF_SCREEN_MOLE LDA #$06                ; \ entry point of routine determines value of $03
                    BRA STORE_03            ;  | 
SUB_OFF_SCREEN_X1   LDA #$04                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X2   LDA #$02                ;  |
STORE_03            STA $03                 ;  |
                    BRA START_SUB           ;  |
SUB_OFF_SCREEN_HB   STZ $03                 ; /

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

