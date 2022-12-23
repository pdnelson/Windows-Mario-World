print "INIT ",pc
JSR SUB_HORZ_POS
TYA
STA $157C,x
RTL
 
print "MAIN ",pc
PHB : PHK : PLB  
JSR SPR_MAIN  
PLB
RTL
 
SPR_MAIN:
JSR SPR_GFX
 
LDA $14C8,x
CMP #$08
BNE RETURN
LDA $9D
BNE RETURN

JSR SubOffscreenX0 ; HANDLE SPRITE GOING OFFSCREEN
JSL $018032 ; INTERACT WITH SPRITES
 
LDA $1588,x
AND #$03
BEQ NOTWALL
 
LDA $157C,x
EOR #$01
STA $157C,x
 
NOTWALL:
LDA $C2,x
CMP #$01
BNE NORMAL
STZ $B6,x
BRA CONTACT_STUFF
RTS
NORMAL:
LDA $1588,x
AND #$04
BEQ CONTACT_STUFF
LDY $157C,x
LDA XSPD,y
STA $B6,x
LDA #$10
STA $AA,x
 
CONTACT_STUFF:
JSL $01802A
JSL $01A7DC
BCC NOCONTACT
 
LDA $1490
BNE HasStar
JSL $00F5B7
RETURN: RTS
 
HasStar:
    LDA #$02        ;\ Set the sprite status to ..
    STA $14C8,x     ;/ Killed by a star.
    LDA #$D0        ;
    STA $AA,x       ;/ Set Y speed so sprite falls down.
    INC $18D2       ; Increase number of consective enemies stomped/ killed by star.
    LDA $18D2       ; IF consecutive # of enemies killed by star/stomped ..
    CMP #$08        ; IS 8 ..
    BCC NotEight
    LDA #$08        ;\ Keep it static at 8, because you get 1-ups all the time afterwards.
    STA $18D2       ;/
NotEight:
    JSL $02ACE5     ; This code calls the "give points" routine.
    LDY $18D2       ; Get consecutive # of enemies stomped in Y.
    CPY #$08        ; If it's less than 8 once again, return.
    BCC NoSound
    LDA StarSFX,y
    STA $1DF9
    RTS
NoSound:
LDA #$13 : STA $1DF9
RTS        
 
StarSFX: db $00,$13,$14,$15,$16,$17,$18,$19
 
NOCONTACT:
LDA $1534,x
CMP #$BE
BEQ SHOOTFIRE
INC $1534,x
 
LDA $1594,x
CMP #$50
BEQ ENDNOW
INC $1594,x
RTS
ENDNOW: 
STZ $1594,x
STZ $C2,x
RTS
 
XSPD: db $10,$F0
 
SHOOTFIRE:
PHY
JSR SUB_HORZ_POS
TYA
STA $157C,x
PLY
LDA #$01
STA $C2,x
STZ $1534,x
	
    LDY #$01    ; number of projectiles to spawn
-   TYA     ; 
    STA $00     ; / get number within loop
    PHY
    JSR .SPAWNFIREBALL  ; spawn fireball
    PLY
    DEY
    BPL -
    RTS
 
.SPAWNFIREBALL
    LDY #$07    ; number of slots to check for (#$07 = 8 slots; leaves the last 2 slots for Mario's fireballs)
-   LDA $170B,y
    BEQ Extra1
    DEY
    BPL -
    RTS
Extra1:
    LDA #$02
    STA $170B,y
   
    LDA $E4,x
    STA $171F,y
    LDA $14E0,x
    STA $1733,y
    LDA $D8,x
    STA $1715,y
    LDA $14D4,x
    STA $1729,y
   
    PHY
    LDY $157C,x
    LDA .FIREBALL_XSPD,y
    PLY
    STA $1747,y
    PHY
    LDY $00
    LDA .FIREBALL_YSPD,y
    PLY
    STA $173D,y
 
    LDA #$06
    STA $1DFC
    LDA #$FF
    STA $176F,y
    RTS
	
.FIREBALL_XSPD:
db $16,$EA
.FIREBALL_YSPD:
db $00,$FB
;;;;;;;;;;;;;; GFX ROUTINE ;;;;;;;;;;;;;;;;;;
 
TILEMAP:
db $82,$8C ; FRAME 1
db $84,$88 ; FRAME 2
 
PROP:
db $00,$40
 
YDISP:
db $10,$00 ; RIGHT
db $10,$00 ; LEFT  
 
SPR_GFX:
 
JSR GET_DRAW_INFO
  
PHX
LDA $157C,x
STA $02
TAX
LDA PROP,x
STA $0F
PLX
 
LDA $14
LSR #3
CLC : ADC $15E9
AND #$01 ; ANIMATE BETWEEN TWO FRAMES
ASL A
STA $03
 
LDA $C2,x
CMP #$01
BEQ SPR_GFX1
 
PHX
LDX #$01 ; 2 TILES DRAWN SO LOOP ONCE
 
LOOP:
LDA $00
STA $0300,y
 
LDA $01
SEC : SBC YDISP,x
STA $0301,y
 
PHX
LDX $15E9
LDA $15F6,x
ORA $0F
ORA $64
STA $0303,y
PLX
 
PHX
TXA
CLC : ADC $03
TAX
LDA TILEMAP,x
STA $0302,y
PLX
 
INY #4
DEX
 
BPL LOOP
 
PLX
 
LDY #$02 ; 16X16 TILES
LDA #$01 ; 2 TILES DRAWN
JSL $01B7B3
 
RTS

;;;;;;;;;;;;;;;
 
FIRE_TILEMAP:
db $82,$86 ; FRAME 1
db $84,$86 ; FRAME 2
 
SPR_GFX1:
PHX
LDX #$01 ; 2 TILES DRAWN SO LOOP ONCE
 
LOOP1:
LDA $00
STA $0300,y
 
LDA $01
SEC : SBC YDISP,x
STA $0301,y
 
PHX
LDX $15E9
LDA $15F6,x
ORA $0F
ORA $64
STA $0303,y
PLX
 
PHX
TXA
CLC : ADC $03
TAX
LDA FIRE_TILEMAP,x
STA $0302,y
PLX
 
INY #4
DEX 
BPL LOOP1
 
PLX
 
LDY #$02 ; 16X16 TILES
LDA #$01 ; 2 TILES DRAWN
JSL $01B7B3
 
RTS
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GET_DRAW_INFO
; This is a helper for the graphics routine.  It sets off screen flags, and sets up
; variables.  It will return with the following:
;
;       Y = index to sprite OAM ($300)
;       $00 = sprite x position relative to screen boarder
;       $01 = sprite y position relative to screen boarder  
;
; It is adapted from the subroutine at $03B760
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
SPR_T1:              db $0C,$1C
SPR_T2:              db $01,$02
 
GET_DRAW_INFO:
    STZ $186C,x             ; reset sprite offscreen flag, vertical
    STZ $15A0,x             ; reset sprite offscreen flag, horizontal
    LDA $E4,x               ; \
    CMP $1A                 ;  | set horizontal offscreen if necessary
    LDA $14E0,x             ;  |
    SBC $1B                 ;  |
    BEQ .ON_SCREEN_X        ;  |
    INC $15A0,x             ; /
 
.ON_SCREEN_X
    LDA $14E0,x             ; \
    XBA                     ;  |
    LDA $E4,x               ;  |
    REP #$20                ;  |
    SEC                     ;  |
    SBC $1A                 ;  | mark sprite invalid if far enough off screen
    CLC                     ;  |
    ADC.w #$0040            ;  |
    CMP.w #$0180            ;  |
    SEP #$20                ;  |
    ROL A                   ;  |
    AND #$01                ;  |
    STA $15C4,x             ; /
    BNE .INVALID           ;
   
    LDY #$00                ; \ set up loop:
    LDA $1662,x             ;  |
    AND #$20                ;  | if not smushed (1662 & 0x20), go through loop twice
    BEQ .ON_SCREEN_LOOP     ;  | else, go through loop once
    INY                     ; /
.ON_SCREEN_LOOP
    LDA $D8,x               ; \
    CLC                     ;  | set vertical offscreen if necessary
    ADC SPR_T1,y            ;  |
    PHP                     ;  |
    CMP $1C                 ;  | (vert screen boundry)
    ROL $00                 ;  |
    PLP                     ;  |
    LDA $14D4,x             ;  |
    ADC #$00                ;  |
    LSR $00                 ;  |
    SBC $1D                 ;  |
    BEQ .ON_SCREEN_Y        ;  |
    LDA $186C,x             ;  | (vert offscreen)
    ORA SPR_T2,y            ;  |
    STA $186C,x             ;  |
.ON_SCREEN_Y
    DEY                     ;  |
    BPL .ON_SCREEN_LOOP     ; /
 
    LDY $15EA,x             ; get offset to sprite OAM
    LDA $E4,x               ; \
    SEC                     ;  |
    SBC $1A                 ;  | $00 = sprite x position relative to screen boarder
    STA $00                 ; /
    LDA $D8,x               ; \
    SEC                     ;  |
    SBC $1C                 ;  | $01 = sprite y position relative to screen boarder
    STA $01                 ; /
    RTS  ; return
 
.INVALID
    PLA                     ; \ return from *main gfx routine* subroutine...
    PLA                     ;  |    ...(not just this subroutine)
    RTS                     ; /
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sub horz pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
SUB_HORZ_POS:       LDY #$00
                    LDA $94
                    SEC
                    SBC $E4,x
                    STA $0F
                    LDA $95
                    SBC $14E0,x
                    BPL SPR_L16
                    INY
SPR_L16:            RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_OFF_SCREEN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Table3:		db $40,$B0
Table4:		db $30,$C0,$A0,$C0,$A0,$F0,$60,$90,$30,$C0,$A0,$80,$A0,$40,$60,$B0
Table5:		db $01,$FF,$01,$FF,$01,$FF,$01,$FF,$01,$FF,$01,$FF,$01,$00,$01,$FF
Table6:		db $01,$FF

SubOffscreenX0:
LDA #$00
BRA SubOffscreen
SubOffscreenX1:
LDA #$02
BRA SubOffscreen
SubOffscreenX2:
LDA #$04
BRA SubOffscreen
SubOffscreenX3:
LDA #$06
;BRA SubOffscreen
;SubOffscreenX4:
;LDA #$08
;BRA SubOffscreen
;SubOffscreenX5:
;LDA #$0A
;BRA SubOffscreen
;SubOffscreenX6:
;LDA #$0C
;BRA SubOffscreen
;SubOffscreenX7:
;LDA #$0E
SubOffscreen:
STA $03
JSR SubIsOffscreen
BEQ Return2
LDA $5B
AND #$01
BNE VerticalLevel
LDA $D8,x
CLC
ADC #$50
LDA $14D4,x
ADC #$00
CMP #$02
BPL EraseSprite
LDA $167A,x
AND #$04
BNE Return2
LDA $13
AND #$01
ORA $03
STA $01
TAY
LDA $1A
CLC
ADC Table4,y
ROL $00
CMP $E4,x
PHP
LDA $1B
LSR $00
ADC Table5,y
PLP
SBC $14E0,x
STA $00
LSR $01
BCC Label20
EOR #$80
STA $00
Label20:
LDA $00
BPL Return2
EraseSprite:
LDA $14C8,x
CMP #$08
BCC KillSprite
LDY $161A,x
CPY #$FF
BEQ KillSprite
LDA #$00
STA $1938,y
KillSprite:
STZ $14C8,x
Return2:
RTS

VerticalLevel:
LDA $167A,x
AND #$04
BNE Return2
LDA $13
LSR
BCS Return2
AND #$01
STA $01
TAY
LDA $1C
CLC
ADC Table3,y
ROL $00
CMP $D8,x
PHP
LDA $1D
LSR $00
ADC Table6,y
PLP
SBC $14D4,x
STA $00
LDY $01
BEQ Label22
EOR #$80
STA $00
Label22:
LDA $00
BPL Return2
BMI EraseSprite
SubIsOffscreen:
LDA $15A0,x
ORA $186C,x
RTS