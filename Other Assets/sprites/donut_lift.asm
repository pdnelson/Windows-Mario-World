;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Donut Lift (sprite portion), by mikeyk
;;
;; Description: 
;;
;; NOTE: This sprite works in conjunction with a blocktool block.  The MAP16 number for
;; the block must be specified where it says DONUT_MAP16_NUM, and the sprite must
;; be inserted as sprite 85.
;; 
;; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        DONUT_MAP16_NUM = $0534             ; map16 value of the donut block in hex
        
        DONUT_SPRITE_TILE = $80             ; graphic tile to use for donut sprite
        DONUT_SPRITE_PAL = $02              ; xyppccct format
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; init JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
                    RTL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main sprite JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "MAIN"
                    PHB                     ; \ 
                    PHK                     ;  |
                    PLB                     ;  |
                    JSR DONUT_CODE_START    ;  |
                    PLB                     ;  |
                    RTL                     ; /


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; donut main code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DONUT_CODE_START    JSR DONUT_GRAPHICS      ;gfx routine
                    JSR SUB_OFF_SCREEN_X3   ; handle off screen situation                   

                    LDA $9D                 ; \ if sprites locked, return
                    BNE RETURN              ; /

NOTLOCKED           LDA $AA,x               ;if y speed = 0, jumps to timer code
                    BEQ NOSPEED
                    LDA $AA,x               ;caps y speed at 40
                    CMP #$38  
                    BPL SETSPEED 
                    CLC                     ;increments y speed by 2
                    ADC #$02   
                    STA $AA,x
SETSPEED            JSL $01801A             ;sets speed
                    LDA #$01
                    STA $1558,x

NOSPEED             JSL $01B44F             ;interact with sprite
                    BCC MAKE_BLOCK          ;if not on donut lift, change sprite to map16 block
                    
                    LDA $1558,x             ;if the timer hasn't been set, set timer
                    BNE DONTSETTIME
                    LDA #$28    
                    STA $1558,x 
DONTSETTIME         DEC A                   ;decrements timer
                    STA $1558,x     
                    CMP #$01                ;if the timer is down to 1, set y speed
                    BNE RETURN 
                    LDA #$0B                ;y speed=0B
                    STA $AA,x       
RETURN              RTS        

MAKE_BLOCK          LDA $AA,x
                    BNE RETURN
                    STZ $14C8,x             ; destroy the sprite
                    STZ $1558,x             ; reset timer
                    
                    LDA $E4,x               ; \ setup block properties
                    STA $9A                 ;  |
                    LDA $14E0,x             ;  |
                    STA $9B                 ;  |
                    LDA $D8,x               ;  |
                    STA $98                 ;  |
                    LDA $14D4,x             ;  |
                    STA $99                 ; /
                    
                    PHP
                    REP #$30                ; \ change sprite to block 
                    LDA.W #DONUT_MAP16_NUM  ;  |
                    STA $03                 ;  |
                    JSR SUBL_SET_MAP16      ;  |
                    PLP                     ; / 
                    
                    RTS     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; donut graphics routine - specific 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DONUT_GRAPHICS      JSR GET_DRAW_INFO       ; sets y = OAM offset
                    
                    
                    STZ $02                 
                    STZ $03                 
                    
                    LDA $AA,x
                    BNE LABEL40
                    LDA $14    
                    AND #$02   
                    BNE LABEL40
                    LDA $1558,x
                    BEQ LABEL40
                    LDA $00    
                    INC A
                    BRA LABEL41
LABEL40             LDA $00
LABEL41             STA $0300,y

                    LDA $01                 ; \ tile y position = sprite y location ($01)
                    STA $0301,y             ; /

                    LDA $15F6,x             ; tile properties xyppccct, format
                    ORA $64                 ; add in tile priority of level
                    STA $0303,y             ; store tile properties

                    LDA #DONUT_SPRITE_TILE  ; \ store tile
                    STA $0302,y             ; /

                    INY                     ; \ increase index to sprite tile map ($300)...
                    INY                     ;  |    ...we wrote 1 16x16 tile...
                    INY                     ;  |    ...sprite OAM is 8x8...
                    INY                     ; /    ...so increment 4 times

                    LDY #$02                ; \ 460 = 2 (all 16x16 tiles)
                    LDA #$00                ;  | A = (number of tiles drawn - 1)
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
            
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map16 subroutine
; doesn't work with mario allstars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUBL_SET_MAP16      PHP                     ;A:0266 X:0007 Y:0001 D:0000 DB:01 S:01EE P:envmxdizcHC:1306 VC:149 00 FL:1681
                    REP #$30                ;A:0266 X:0007 Y:0001 D:0000 DB:01 S:01ED P:envmxdizcHC:1328 VC:149 00 FL:1681
                    PHY                     ;A:0266 X:0007 Y:0001 D:0000 DB:01 S:01ED P:envmxdizcHC:1350 VC:149 00 FL:1681
                    PHX                     ;A:0266 X:0007 Y:0001 D:0000 DB:01 S:01EB P:envmxdizcHC:0012 VC:150 00 FL:1681
                    TAX                     ;A:0266 X:0007 Y:0001 D:0000 DB:01 S:01E9 P:envmxdizcHC:0042 VC:150 00 FL:1681
                    LDA $03                 ;A:0266 X:0266 Y:0001 D:0000 DB:01 S:01E9 P:envmxdizcHC:0056 VC:150 00 FL:1681
                    PHA                     ;A:0266 X:0266 Y:0001 D:0000 DB:01 S:01E9 P:envmxdizcHC:0088 VC:150 00 FL:1681
                    JSR SUB_8034            ;A:0266 X:0266 Y:0001 D:0000 DB:01 S:01E7 P:envmxdizcHC:0118 VC:150 00 FL:1681
                    PLA                     ;A:0010 X:0000 Y:0006 D:0000 DB:00 S:01DC P:envmxdizCHC:0726 VC:032 00 FL:11805
                    STA $03                 ;A:02A8 X:0000 Y:0006 D:0000 DB:00 S:01DE P:envmxdizCHC:0762 VC:032 00 FL:11805
                    PLX                     ;A:02A8 X:0000 Y:0006 D:0000 DB:00 S:01DE P:envmxdizCHC:0794 VC:032 00 FL:11805
                    PLY                     ;A:02A8 X:0020 Y:0006 D:0000 DB:00 S:01E0 P:envmxdizCHC:0830 VC:032 00 FL:11805
                    PLP                     ;A:02A8 X:0020 Y:0001 D:0000 DB:00 S:01E2 P:envmxdizCHC:0866 VC:032 00 FL:11805
                    RTS                     ;A:02A8 X:0020 Y:0001 D:0000 DB:00 S:01E3 P:envmxdizCHC:0894 VC:032 00 FL:11805

                    JMP $FEA301
RETURN18            PLX
                    PLB
                    PLP
                    RTS

SUB_8034            PHP                     ;A:0266 X:0266 Y:0001 D:0000 DB:01 S:01E5 P:envmxdizcHC:0164 VC:150 00 FL:1682
                    SEP #$20                ;A:0266 X:0266 Y:0001 D:0000 DB:01 S:01E4 P:envmxdizcHC:0186 VC:150 00 FL:1682
                    PHB                     ;A:0266 X:0266 Y:0001 D:0000 DB:01 S:01E4 P:envMxdizcHC:0208 VC:150 00 FL:1682
                    LDA #$00                ;A:0200 X:0266 Y:0001 D:0000 DB:01 S:01E3 P:envMxdiZcHC:0286 VC:150 00 FL:1682
                    PHA                     ;A:0200 X:0266 Y:0001 D:0000 DB:01 S:01E3 P:envMxdiZcHC:0324 VC:150 00 FL:1682
                    PLB                     ;A:0200 X:0266 Y:0001 D:0000 DB:01 S:01E2 P:envMxdiZcHC:0346 VC:150 00 FL:1682
                    REP #$30                ;A:0200 X:0266 Y:0001 D:0000 DB:00 S:01E3 P:envMxdiZcHC:0374 VC:150 00 FL:1682
                    PHX                     ;A:0200 X:0266 Y:0001 D:0000 DB:00 S:01E3 P:envmxdiZcHC:0396 VC:150 00 FL:1682
                    LDA $9A                 ;A:0200 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envmxdiZcHC:0426 VC:150 00 FL:1682
                    STA $0C                 ;A:0070 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envmxdizcHC:0458 VC:150 00 FL:1682
                    LDA $98                 ;A:0070 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envmxdizcHC:0490 VC:150 00 FL:1682
                    STA $0E                 ;A:0130 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envmxdizcHC:0522 VC:150 00 FL:1682
                    LDA.W #$0000            ;A:0130 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envmxdizcHC:0554 VC:150 00 FL:1682
                    SEP #$20                ;A:0000 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envmxdiZcHC:0578 VC:150 00 FL:1682
                    LDA $5B                 ;A:0000 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0600 VC:150 00 FL:1682
                    STA $09                 ;A:0000 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0624 VC:150 00 FL:1682
                    LDA $1933               ;A:0000 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0648 VC:150 00 FL:1682
                    BEQ NO_SHIFT            ;A:0000 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0680 VC:150 00 FL:1682
                    LSR $09
NO_SHIFT            LDY $0E                 ;A:0000 X:0266 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0702 VC:150 00 FL:1682
                    LDA $09                 ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:envMxdizcHC:0734 VC:150 00 FL:1682
                    AND #$01                ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0758 VC:150 00 FL:1682
                    BEQ HORIZ               ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0774 VC:150 00 FL:1682
                    LDA $9B
                    STA $00
                    LDA $99
                    STA $9B
                    LDA $00  
                    STA $99
                    LDY $0C
HORIZ               CPY.W #$0200            ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0796 VC:150 00 FL:1682
                    BCS RETURN18            ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0820 VC:150 00 FL:1682
                    LDA $1933               ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0836 VC:150 00 FL:1682
                    ASL A                   ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0868 VC:150 00 FL:1682
                    TAX                     ;A:0000 X:0266 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0882 VC:150 00 FL:1682
                    LDA $BEA8,x ;[$00:BEA8] ;A:0000 X:0000 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0896 VC:150 00 FL:1682
                    STA $65                 ;A:00A8 X:0000 Y:0130 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0928 VC:150 00 FL:1682
                    LDA $BEA9,x ;[$00:BEA9] ;A:00A8 X:0000 Y:0130 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0952 VC:150 00 FL:1682
                    STA $66                 ;A:00BD X:0000 Y:0130 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0984 VC:150 00 FL:1682
                    STZ $67                 ;A:00BD X:0000 Y:0130 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:1008 VC:150 00 FL:1682
                    LDA $1925               ;A:00BD X:0000 Y:0130 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:1032 VC:150 00 FL:1682
                    ASL A                   ;A:0000 X:0000 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1064 VC:150 00 FL:1682
                    TAY                     ;A:0000 X:0000 Y:0130 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1078 VC:150 00 FL:1682
                    LDA ($65),y ;[$00:BDA8] ;A:0000 X:0000 Y:0000 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1092 VC:150 00 FL:1682
                    STA $04                 ;A:00D8 X:0000 Y:0000 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:1132 VC:150 00 FL:1682
                    INY                     ;A:00D8 X:0000 Y:0000 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:1156 VC:150 00 FL:1682
                    LDA ($65),y ;[$00:BDA9] ;A:00D8 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:1170 VC:150 00 FL:1682
                    STA $05                 ;A:00BA X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:1210 VC:150 00 FL:1682
                    STZ $06                 ;A:00BA X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:1234 VC:150 00 FL:1682
                    LDA $9B                 ;A:00BA X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:1258 VC:150 00 FL:1682
                    STA $07                 ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1282 VC:150 00 FL:1682
                    ASL A                   ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1306 VC:150 00 FL:1682
                    CLC                     ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1320 VC:150 00 FL:1682
                    ADC $07                 ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1334 VC:150 00 FL:1682
                    TAY                     ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:1358 VC:150 00 FL:1682
                    LDA ($04),y ;[$00:BAD8] ;A:0000 X:0000 Y:0000 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0004 VC:151 00 FL:1682
                    STA $6B                 ;A:0000 X:0000 Y:0000 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0044 VC:151 00 FL:1682
                    STA $6E                 ;A:0000 X:0000 Y:0000 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0068 VC:151 00 FL:1682
                    INY                     ;A:0000 X:0000 Y:0000 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0092 VC:151 00 FL:1682
                    LDA ($04),y ;[$00:BAD9] ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0106 VC:151 00 FL:1682
                    STA $6C                 ;A:00C8 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0146 VC:151 00 FL:1682
                    STA $6F                 ;A:00C8 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0170 VC:151 00 FL:1682
                    LDA #$7E                ;A:00C8 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0194 VC:151 00 FL:1682
                    STA $6D                 ;A:007E X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0210 VC:151 00 FL:1682
                    INC A                   ;A:007E X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0234 VC:151 00 FL:1682
                    STA $70                 ;A:007F X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0248 VC:151 00 FL:1682
                    LDA $09                 ;A:007F X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0272 VC:151 00 FL:1682
                    AND #$01                ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0296 VC:151 00 FL:1682
                    BEQ NO_AND              ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0312 VC:151 00 FL:1682
                    LDA $99
                    LSR A
                    LDA $9B 
                    AND #$01
                    BRA LABEL52
NO_AND              LDA $9B                 ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0334 VC:151 00 FL:1682
                    LSR A                   ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0358 VC:151 00 FL:1682
                    LDA $99                 ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZcHC:0372 VC:151 00 FL:1682
LABEL52             ROL A                   ;A:0001 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0396 VC:151 00 FL:1682
                    ASL A                   ;A:0002 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0410 VC:151 00 FL:1682
                    ASL A                   ;A:0004 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0424 VC:151 00 FL:1682
                    ORA #$20                ;A:0008 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0438 VC:151 00 FL:1682
                    STA $04                 ;A:0028 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0454 VC:151 00 FL:1682
                    CPX.W #$0000            ;A:0028 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0478 VC:151 00 FL:1682
                    BEQ NO_ADD              ;A:0028 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZCHC:0502 VC:151 00 FL:1682
                    CLC
                    ADC #$10 
                    STA $04
NO_ADD              LDA $98                 ;A:0028 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZCHC:0524 VC:151 00 FL:1682
                    AND #$F0                ;A:0030 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizCHC:0548 VC:151 00 FL:1682
                    CLC                     ;A:0030 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizCHC:0564 VC:151 00 FL:1682
                    ASL A                   ;A:0030 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0578 VC:151 00 FL:1682
                    ROL A                   ;A:0060 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0592 VC:151 00 FL:1682
                    STA $05                 ;A:00C0 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0606 VC:151 00 FL:1682
                    ROL A                   ;A:00C0 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0630 VC:151 00 FL:1682
                    AND #$03                ;A:0080 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizCHC:0644 VC:151 00 FL:1682
                    ORA $04                 ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdiZCHC:0660 VC:151 00 FL:1682
                    STA $06                 ;A:0028 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizCHC:0684 VC:151 00 FL:1682
                    LDA $9A                 ;A:0028 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizCHC:0708 VC:151 00 FL:1682
                    AND #$F0                ;A:0070 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizCHC:0732 VC:151 00 FL:1682
                    LSR A                   ;A:0070 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizCHC:0748 VC:151 00 FL:1682
                    LSR A                   ;A:0038 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0762 VC:151 00 FL:1682
                    LSR A                   ;A:001C X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0776 VC:151 00 FL:1682
                    STA $04                 ;A:000E X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0790 VC:151 00 FL:1682
                    LDA $05                 ;A:000E X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envMxdizcHC:0814 VC:151 00 FL:1682
                    AND #$C0                ;A:00C0 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0838 VC:151 00 FL:1682
                    ORA $04                 ;A:00C0 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0854 VC:151 00 FL:1682
                    STA $07                 ;A:00CE X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0878 VC:151 00 FL:1682
                    REP #$20                ;A:00CE X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvMxdizcHC:0902 VC:151 00 FL:1682
                    LDA $09                 ;A:00CE X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvmxdizcHC:0924 VC:151 00 FL:1682
                    AND.W #$0001            ;A:0100 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envmxdizcHC:0956 VC:151 00 FL:1682
                    BNE LABEL51             ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envmxdiZcHC:0980 VC:151 00 FL:1682
                    LDA $1A                 ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envmxdiZcHC:0996 VC:151 00 FL:1682
                    SEC                     ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envmxdiZcHC:1028 VC:151 00 FL:1682
                    SBC.W #$0080            ;A:0000 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:envmxdiZCHC:1042 VC:151 00 FL:1682
                    TAX                     ;A:FF80 X:0000 Y:0001 D:0000 DB:00 S:01E1 P:eNvmxdizcHC:1066 VC:151 00 FL:1682
                    LDY $1C                 ;A:FF80 X:FF80 Y:0001 D:0000 DB:00 S:01E1 P:eNvmxdizcHC:1080 VC:151 00 FL:1682
                    LDA $1933               ;A:FF80 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:1112 VC:151 00 FL:1682
                    BEQ LABEL50             ;A:0000 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdiZcHC:1152 VC:151 00 FL:1682
                    LDX $1E
                    LDA $20
                    SEC
                    SBC.W #$0080
                    TAY
                    BRA LABEL50
LABEL51             LDX $1A
                    LDA $1C
                    SEC
                    SBC.W #$0080
                    TAY
                    LDA $1933
                    BEQ LABEL50
                    LDA $1E
                    SEC
                    SBC.W #$0080
                    TAX  
                    LDY $20
LABEL50             STX $08                 ;A:0000 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdiZcHC:1174 VC:151 00 FL:1682
                    STY $0A                 ;A:0000 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdiZcHC:1206 VC:151 00 FL:1682
                    LDA $98                 ;A:0000 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdiZcHC:1238 VC:151 00 FL:1682
                    AND.W #$01F0            ;A:0130 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:1270 VC:151 00 FL:1682
                    STA $04                 ;A:0130 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:1294 VC:151 00 FL:1682
                    LDA $9A                 ;A:0130 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:1326 VC:151 00 FL:1682
                    LSR A                   ;A:0070 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:1358 VC:151 00 FL:1682
                    LSR A                   ;A:0038 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:0004 VC:152 00 FL:1682
                    LSR A                   ;A:001C X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:0018 VC:152 00 FL:1682
                    LSR A                   ;A:000E X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:0032 VC:152 00 FL:1682
                    AND.W #$000F            ;A:0007 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:0046 VC:152 00 FL:1682
                    ORA $04                 ;A:0007 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:0070 VC:152 00 FL:1682
                    TAY                     ;A:0137 X:FF80 Y:00C0 D:0000 DB:00 S:01E1 P:envmxdizcHC:0102 VC:152 00 FL:1682
                    PLA                     ;A:0137 X:FF80 Y:0137 D:0000 DB:00 S:01E1 P:envmxdizcHC:0116 VC:152 00 FL:1682
                    SEP #$20                ;A:0266 X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envmxdizcHC:0152 VC:152 00 FL:1682
                    STA [$6B],y ;[$7E:C937] ;A:0266 X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envMxdizcHC:0174 VC:152 00 FL:1682
                    XBA                     ;A:0266 X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envMxdizcHC:0222 VC:152 00 FL:1682
                    STA [$6E],y ;[$7F:C937] ;A:6602 X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envMxdizcHC:0242 VC:152 00 FL:1682
                    XBA                     ;A:6602 X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envMxdizcHC:0290 VC:152 00 FL:1682
                    REP #$20                ;A:0266 X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envMxdizcHC:0310 VC:152 00 FL:1682
                    ASL A                   ;A:0266 X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envmxdizcHC:0332 VC:152 00 FL:1682
                    TAY                     ;A:04CC X:FF80 Y:0137 D:0000 DB:00 S:01E3 P:envmxdizcHC:0346 VC:152 00 FL:1682
                    PHK                     ;A:04CC X:FF80 Y:04CC D:0000 DB:00 S:01E3 P:envmxdizcHC:0360 VC:152 00 FL:1682
                    PER.W #$0006            ; NOTE: this relative counter must always point to MAP16_RETURN. 
                    PEA $804C
                    JMP.L $00C0FB
MAP16_RETURN        PLB
                    PLP
                    RTS

                    
                    NOP
                    