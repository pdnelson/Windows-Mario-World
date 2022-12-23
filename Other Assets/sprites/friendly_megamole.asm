;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Friendly Mega Mole, ripped by mikeyk
;
; Description: Like the original Mega Mole, except he cannot hurt Mario
;
; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite initialization JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
                    PHY
                    JSR SUB_HORZ_POS
                    TYA
                    STA $157C,x
                    PLY
                    RTL
                    
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite main JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
                    dcb "MAIN"                        
                    PHB                     ; \
                    PHK                     ;  | main sprite function, just calls local subroutine
                    PLB                     ;  |
                    JSR START_SPRITE_CODE   ;  |
                    PLB                     ;  |
                    RTL                     ; /


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main sprite sprite code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

X_SPEED             dcb $10,$F0
RETURN              RTS

START_SPRITE_CODE   JSR MOLE_GRAPHICS       ; graphics routine
                    LDA $14C8,x             ; \ 
                    CMP #$08                ;  | if status != 8, return
                    BNE RETURN              ; /
                    JSR SUB_OFF_SCREEN_MOLE ; handle off screen situation
                    LDY $157C,x             ; \ set x speed based on direction
                    LDA X_SPEED,y           ;  |
                    STA $B6,x               ; /
                    LDA $9D                 ; \ if sprites locked, return
                    BNE RETURN              ; /
                    LDA $1588,x             ;A:0100 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZCHC:0276 VC:077 00 FL:623
                    AND #$04                ;A:0100 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZCHC:0308 VC:077 00 FL:623
                    PHA                     ;A:0100 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZCHC:0324 VC:077 00 FL:623
                    JSL $01802A             ; update position based on speed values
                    JSL $018032             ; interact with other sprites
                    LDA $1588,x             ;A:254B X:0007 Y:0007 D:0000 DB:03 S:01EE P:envMXdizcHC:0684 VC:085 00 FL:623
                    AND #$04                ;A:2504 X:0007 Y:0007 D:0000 DB:03 S:01EE P:envMXdizcHC:0716 VC:085 00 FL:623
                    BEQ IN_AIR              ;A:2504 X:0007 Y:0007 D:0000 DB:03 S:01EE P:envMXdizcHC:0732 VC:085 00 FL:623
                    STZ $AA,x               ;A:2504 X:0007 Y:0007 D:0000 DB:03 S:01EE P:envMXdizcHC:0748 VC:085 00 FL:623
                    PLA                     ;A:2504 X:0007 Y:0007 D:0000 DB:03 S:01EE P:envMXdizcHC:0778 VC:085 00 FL:623
                    BRA ON_GROUND           ;A:2500 X:0007 Y:0007 D:0000 DB:03 S:01EF P:envMXdiZcHC:0806 VC:085 00 FL:623
IN_AIR              PLA                     ;A:2500 X:0007 Y:0006 D:0000 DB:03 S:01EB P:envMXdiZcHC:0316 VC:085 00 FL:4955
                    BEQ WAS_IN_AIR          ;A:2504 X:0007 Y:0006 D:0000 DB:03 S:01EC P:envMXdizcHC:0344 VC:085 00 FL:4955
                    LDA #$0A                ;A:2504 X:0007 Y:0006 D:0000 DB:03 S:01EC P:envMXdizcHC:0360 VC:085 00 FL:4955
                    STA $1540,x             ;A:25FF X:0007 Y:0006 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0376 VC:085 00 FL:4955
WAS_IN_AIR          LDA $1540,x             ;A:25FF X:0007 Y:0006 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0408 VC:085 00 FL:4955
                    BEQ ON_GROUND           ;A:25FF X:0007 Y:0006 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0440 VC:085 00 FL:4955
                    STZ $AA,x               ;A:25FF X:0007 Y:0006 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0456 VC:085 00 FL:4955
ON_GROUND           LDY $15AC,x             ; \
                    LDA $1588,x             ; | if mega mole is in contact with an object...
                    AND #$03                ; |
                    BEQ NO_CONTACT          ; |
                    CPY #$00                ; |    ... and timer hasn't been set (time until flip == 0)...
                    BNE DONT_SET_TIMER      ; |
                    LDA #$10                ; |    ... set time until flip
                    STA $15AC,x             ; /
DONT_SET_TIMER      LDA $157C,x             ; \ flip the temp direction status
                    EOR #$01                ; |
                    STA $157C,x             ; /
NO_CONTACT          CPY #$00                ; \ if time until flip == 0...
                    BNE DONT_UPDATE         ; |
                    LDA $157C,x             ; |    ...update the direction status used by the gfx routine
                    STA $151C,x             ; /

DONT_UPDATE         JSL $01A7DC             ; check for mario/mega mole contact
                    BCC RETURN_24           ; (carry set = contact)
                    JSR SUB1                ;A:25A1 X:0007 Y:0001 D:0000 DB:03 S:01EC P:envMXdizCHC:0320 VC:087 00 FL:811
                    LDA $0E                 ;A:25FF X:0007 Y:0001 D:0000 DB:03 S:01EC P:envMXdizcHC:0602 VC:087 00 FL:811
                    CMP #$D8                ;A:25F0 X:0007 Y:0001 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0626 VC:087 00 FL:811
                    BPL MOLE_WINS           ;A:25F0 X:0007 Y:0001 D:0000 DB:03 S:01EC P:envMXdizCHC:0642 VC:087 00 FL:811
                    LDA $7D                 ;A:25D0 X:0007 Y:0001 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0306 VC:087 00 FL:2713
                    BMI RETURN_24           ;A:2546 X:0007 Y:0001 D:0000 DB:03 S:01EC P:envMXdizcHC:0330 VC:087 00 FL:2713
                    LDA #$01                ; \ set "on sprite" flag
                    STA $1471               ; /
                    LDA #$06                ; \ set riding mega mole
                    STA $154C,x             ; / 
                    STZ $7D                 ; y speed = 0
                    LDA #$D6                ; \
                    LDY $187A               ; | mario's y position += C6 or D6 depending if on yoshi
                    BEQ NO_YOSHI            ; |
                    LDA #$C6                ; |
NO_YOSHI            CLC                     ; |
                    ADC $D8,x               ; |
                    STA $96                 ; |
                    LDA $14D4,x             ; |
                    ADC #$FF                ; |
                    STA $97                 ; /
                    LDY #$00                ; \ 
                    LDA $1491               ; | $1491 == 01 or FF, depending on direction
                    BPL LABEL9              ; | set mario's new x position
                    DEY                     ; |
LABEL9              CLC                     ; |
                    ADC $94                 ; |
                    STA $94                 ; |
                    TYA                     ; |
                    ADC $95                 ; |
                    STA $95                 ;/
                    RTS                     ;

MOLE_WINS           LDA $154C,x             ; \ if riding mega mole...
                    ORA $15D0,x             ; |   ...or mega mole being eaten...
                    BNE RETURN_24           ; /   ...return
                    ;JSL $00F5B7             ; hurt mario
RETURN_24           RTS                     ; final return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics routine - specific to mega mole
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03882B

MOLE_HORIZ_DISP     dcb $00,$10,$00,$10,$10,$00,$10,$00 
MOLE_VERT_DISP      dcb $F0,$F0,$00,$00 
MOLE_TILEMAP        dcb $C6,$C8,$E6,$E8,$CA,$CC,$EA,$EC

MOLE_GRAPHICS       JSR GET_DRAW_INFO       ;A:87BF X:0007 Y:0000 D:0000 DB:03 S:01ED P:envMXdiZCHC:1102 VC:066 00 FL:665
                    LDA $151C,x             ; \ $02 = direction
                    STA $02                 ; / 
                    LDA $14                 ;\ 
                    LSR A                   ; |
                    LSR A                   ; |
                    NOP                     ; |
                    CLC                     ; |
                    ADC $15E9               ; |
                    AND #$01                ; |
                    ASL A                   ; |
                    ASL A                   ; |
                    STA $03                 ; | $03 = index to frame start (0 or 4)
                    PHX                     ; /

                    LDX #$03                ; run loop 4 times, cuz 4 tiles per frame
LOOP_START_2        PHX                     ; push, current tile
                    LDA $02                 ; \
                    BNE FACING_LEFT_2       ; | if facing right, index to frame end += 4
                    INX                     ; |
                    INX                     ; |
                    INX                     ; |
                    INX                     ; /
FACING_LEFT_2       LDA $00                 ; \ tile x position = sprite x location ($00) + tile displacement
                    CLC                     ; |
                    ADC MOLE_HORIZ_DISP,x   ; |
                    STA $0300,y             ; /
                    PLX                     ; \ pull, X = index to frame end
                    LDA $01                 ; |
                    CLC                     ; | tile y position = sprite y location ($01) + tile displacement
                    ADC MOLE_VERT_DISP,x    ; |
                    STA $0301,y             ; /
                    PHX                     ; \ set current tile
                    TXA                     ; | X = index of frame start + current tile
                    CLC                     ; |
                    ADC $03                 ; |
                    TAX                     ; |
                    LDA MOLE_TILEMAP,x      ; |
                    STA $0302,y             ; /
                    LDA #$01                ; tile properties xyppccct, format
                    LDX $02                 ; \ if direction == 0...
                    BNE NO_FLIP             ; |
                    ORA #$40                ; /    ...flip tile
NO_FLIP             ORA $64                 ; add in tile priority of level
                    STA $0303,y             ; store tile properties
                    PLX                     ; \ pull, current tile
                    INY                     ; | increase index to sprite tile map ($300)...
                    INY                     ; |
                    INY                     ; |    ...we wrote 4 bytes
                    INY                     ; |    ...so increment 4 times
                    DEX                     ; | go to next tile of frame and loop
                    BPL LOOP_START_2        ; /

                    PLX                     ; pull, X = sprite index
                    LDY #$02                ; \ why 02? (460 = 2) all 16x16 tiles
                    LDA #$03                ; | A = number of tiles drawn - 1
                    JSL $01B7B3             ; / don't draw if offscreen
                    RTS                     ; return
                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics routine helper - shared
; sets off screen flags and sets index to OAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B75C

TABLE1              dcb $0C,$1C
TABLE2              dcb $01,$02

GET_DRAW_INFO       STZ $186C,x             ; reset sprite offscreen flag, vertical
                    STZ $15A0,x             ; reset sprite offscreen flag, horizontal
                    LDA $E4,x               ; \
                    CMP $1A                 ; | set horizontal offscreen if necessary
                    LDA $14E0,x             ; |
                    SBC $1B                 ; |
                    BEQ ON_SCREEN_X         ; |
                    INC $15A0,x             ; /

ON_SCREEN_X         LDA $14E0,x             ; \
                    XBA                     ; |
                    LDA $E4,x               ; |
                    REP #$20                ; |
                    SEC                     ; |
                    SBC $1A                 ; | mark sprite invalid if far enough off screen
                    CLC                     ; |
                    ADC.W #$0040            ; |
                    CMP.W #$0180            ; |
                    SEP #$20                ; |
                    ROL A                   ; |
                    AND #$01                ; |
                    STA $15C4,x             ; / 
                    BNE INVALID             ; 
                    
                    LDY #$00                ; \ set up loop:
                    LDA $1662,x             ; | 
                    AND #$20                ; | if not smushed (1662 & 0x20), go through loop twice
                    BEQ ON_SCREEN_LOOP      ; | else, go through loop once
                    INY                     ; / 
ON_SCREEN_LOOP      LDA $D8,x               ; \ 
                    CLC                     ; | set vertical offscreen if necessary
                    ADC TABLE1,y            ; |
                    PHP                     ; |
                    CMP $1C                 ; | (vert screen boundry)
                    ROL $00                 ; |
                    PLP                     ; |
                    LDA $14D4,x             ; | 
                    ADC #$00                ; |
                    LSR $00                 ; |
                    SBC $1D                 ; |
                    BEQ ON_SCREEN_Y         ; |
                    LDA $186C,x             ; | (vert offscreen)
                    ORA TABLE2,y            ; |
                    STA $186C,x             ; |
ON_SCREEN_Y         DEY                     ; |
                    BPL ON_SCREEN_LOOP      ; /

                    LDY $15EA,x             ; get offset to sprite OAM
                    LDA $E4,x               ; \ 
                    SEC                     ; | 
                    SBC $1A                 ; | $00 = sprite x position relative to screen boarder
                    STA $00                 ; / 
                    LDA $D8,x               ; \ 
                    SEC                     ; | 
                    SBC $1C                 ; | $01 = sprite y position relative to screen boarder
                    STA $01                 ; / 
                    RTS                     ; return

INVALID             PLA                     ; \ return from *main gfx routine* subroutine...
                    PLA                     ; |    ...(not just this subroutine)
                    RTS                     ; /


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vertical mario/sprite contact - shared
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B829 

SUB1                LDY #$00               ;A:25A1 X:0007 Y:0001 D:0000 DB:03 S:01EA P:envMXdizCHC:0130 VC:085 00 FL:924
                    LDA $96                ;A:25A1 X:0007 Y:0000 D:0000 DB:03 S:01EA P:envMXdiZCHC:0146 VC:085 00 FL:924
                    SEC                    ;A:2546 X:0007 Y:0000 D:0000 DB:03 S:01EA P:envMXdizCHC:0170 VC:085 00 FL:924
                    SBC $D8,x              ;A:2546 X:0007 Y:0000 D:0000 DB:03 S:01EA P:envMXdizCHC:0184 VC:085 00 FL:924
                    STA $0F                ;A:25D6 X:0007 Y:0000 D:0000 DB:03 S:01EA P:eNvMXdizcHC:0214 VC:085 00 FL:924
                    LDA $97                ;A:25D6 X:0007 Y:0000 D:0000 DB:03 S:01EA P:eNvMXdizcHC:0238 VC:085 00 FL:924
                    SBC $14D4,x            ;A:2501 X:0007 Y:0000 D:0000 DB:03 S:01EA P:envMXdizcHC:0262 VC:085 00 FL:924
                    BPL LABEL11            ;A:25FF X:0007 Y:0000 D:0000 DB:03 S:01EA P:eNvMXdizcHC:0294 VC:085 00 FL:924
                    INY                    ;A:25FF X:0007 Y:0000 D:0000 DB:03 S:01EA P:eNvMXdizcHC:0310 VC:085 00 FL:924
LABEL11             RTS                    ;A:25FF X:0007 Y:0001 D:0000 DB:03 S:01EA P:envMXdizcHC:0324 VC:085 00 FL:924


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; horizontal mario/sprite contact - shared
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B817            

SUB_HORZ_POS        LDY #$00                ;A:25D0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1020 VC:097 00 FL:31642
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
; off screen processing code - shared
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B83B             

TABLE3              dcb $40,$B0
TABLE6              dcb $01,$FF 
TABLE4              dcb $30,$C0,$A0,$80,$A0,$40,$60,$B0 
TABLE5              dcb $01,$FF,$01,$FF,$01,$00,$01,$FF

SUB_OFF_SCREEN_MOLE LDA #$06                ; \ entry point of routine determines value of $03
                    BRA STORE_03            ; | 
SUB_OFF_SCREEN_X1   LDA #$04                ; |
                    BRA STORE_03            ; |
SUB_OFF_SCREEN_X2   LDA #$02                ; |
STORE_03            STA $03                 ; |
                    BRA START_SUB           ; |
SUB_OFF_SCREEN_REX  STZ $03                 ; /

START_SUB           JSR SUB_IS_OFF_SCREEN   ; \ if sprite is not off screen, return
                    BEQ RETURN_2            ; /    
                    LDA $5B                 ; \  goto VERTICAL_LEVEL if vertical level
                    AND #$01                ; |
                    BNE VERTICAL_LEVEL      ; /     
                    LDA $D8,x               ; \
                    CLC                     ; | 
                    ADC #$50                ; | if the sprite has gone off the bottom of the level...
                    LDA $14D4,x             ; | (if adding 0x50 to the sprite y position would make the high byte >= 2)
                    ADC #$00                ; | 
                    CMP #$02                ; | 
                    BPL ERASE_SPRITE        ; /    ...erase the sprite
                    LDA $167A,x             ; \ if "process offscreen" flag is set, return
                    AND #$04                ; |
                    BNE RETURN_2            ; /
                    LDA $13                 ; \ 
                    AND #$01                ; | 
                    ORA $03                 ; | 
                    STA $01                 ; |
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
                    CMP #$08                ; |
                    BCC KILL_SPRITE         ; /
                    LDY $161A,x             ;A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZCHC:0140 VC:071 00 FL:21152
                    CPY #$FF                ;A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0172 VC:071 00 FL:21152
                    BEQ KILL_SPRITE         ;A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0188 VC:071 00 FL:21152
                    LDA #$00                ; \ mark sprite to come back    A:FF08 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0204 VC:071 00 FL:21152
                    STA $1938,y             ; /                             A:FF00 X:0006 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0220 VC:071 00 FL:21152
KILL_SPRITE         STZ $14C8,x             ; erase sprite
RETURN_2            RTS                     ; return

VERTICAL_LEVEL      LDA $167A,x             ; \ if "process offscreen" flag is set, return
                    AND #$04                ; |
                    BNE RETURN_2            ; /
                    LDA $13                 ; \ only handle every other frame??
                    LSR A                   ; | 
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
                    ORA $186C,x             ; |  
                    RTS                     ; / return
