;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crab, by mikeyk
;;
;; Description: The crab walks towards Mario, and if jumped on he tries to flee.  He
;; dies if hit a second time
;;
;; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TILEMAP             dcb $AE,$AE,$8E,$8E,$8E
SPEED_TABLE         dcb $08,$F8             ; speed of sprite, right, left
                    dcb $10,$F0             ; speed of smushed sprite, right, left
KILLED_X_SPEED      dcb $F0,$10


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
                    
START_SPRITE_CODE   JSR SUB_GFX             ; draw sprite gfx
                    LDA $14C8,x             ; \ if sprite status != 8...
                    CMP #$08                ;  }   ... not (killed with spin jump [4] or star[2])
                    BNE RETURN              ; /    ... return
                    LDA $9D                 ; \ if sprites locked...
                    BEQ ALIVE               ; /    ... return
RETURN              RTS                     ; return

ALIVE               JSR SUB_OFF_SCREEN_X3   ; only process sprite while on screen
                    INC $1570,x             ; increment number of frames sprite has been on screen
                    LDA $1570,x             ; \ calculate which frame to show:
                    LSR A                   ; | 
                    LSR A                   ; | 
                    LDY $C2,x               ; | number of hits determines if smushed
                    BEQ NOT_HIT             ; |
                    AND #$01                ; | update every 8 cycles if smushed
                    CLC                     ; |
                    ADC #$03                ; | show smushed frame
                    BRA LABEL3              ; |
NOT_HIT             LSR A                   ; | 
                    AND #$01                ; | update every 16 cycles if normal
LABEL3              STA $1602,x             ; / write frame to show
                    LDA $1588,x             ; \  if sprite is not on ground...
                    AND #$04                ;  }    ...(4 = on ground) ...
                    BEQ IN_AIR              ; /     ...goto IN_AIR
                    LDA #$10                ; \  y speed = 10
                    STA $AA,x               ; /
                    LDY $157C,x             ; load, y = sprite direction, as index for speed
                    LDA $C2,x               ; \ if hits on sprite == 0...
                    BEQ NO_FAST_SPEED       ; /    ...goto DONT_ADJUST_SPEED
                    INY                     ; \ increment y twice...
                    INY                     ; /    ...in order to get speed for smushed sprite
NO_FAST_SPEED       LDA SPEED_TABLE,y       ; \ load x speed from ROM...
                    STA $B6,x               ; /    ...and store it
IN_AIR              LDA $1FE2,x             ; \ if time to show half-smushed sprite > 0...
                    BNE FREEZE_SPRITE          ; /    ...goto HALF_SMUSHED
                    JSL $01802A             ; update position based on speed values
FREEZE_SPRITE       LDA $1588,x             ; \ if sprite is touching the side of an object...
                    AND #$03                ; |
                    BEQ DONT_CHANGE_DIR     ; |
                    LDA $157C,x             ; |
                    EOR #$01                ; |    ... change sprite direction
                    STA $157C,x             ; /
DONT_CHANGE_DIR     JSL $018032             ; interact with other sprites
                    JSL $01A7DC             ; check for mario/sprite contact
                    BCC NO_CONTACT          ; (carry set = mario/sprite contact)
                    LDA $1490               ; \ if mario star timer > 0 ...
                    BNE HAS_STAR            ; /    ... goto HAS_STAR
                    LDA $154C,x             ; \ if sprite invincibility timer > 0 ...
                    BNE NO_CONTACT          ; /    ... goto NO_CONTACT
                    LDA #$08                ; \ sprite invincibility timer = $08
                    STA $154C,x             ; /
                    LDA $7D                 ; \  if mario's y speed < 10 ...
                    CMP #$10                ;  }   ... sprite will hurt mario
                    BMI SPRITE_WINS            ; /    

MARIO_WINS          JSR SUB_STOMP_PTS       ; give mario points
                    JSL $01AA33             ; set mario speed
                    JSL $01AB99             ; display contact graphic
                    LDA $140D               ; \  if mario is spin jumping...
                    ORA $187A               ;  }    ... or on yoshi ...
                    BNE SPIN_KILL           ; /     ... goto SPIN_KILL
                    INC $C2,x               ; increment sprite hit counter
                    LDA $C2,x               ; \  if sprite hit counter == 2
                    CMP #$02                ;  |   
                    BNE SMUSH_SPRITE        ;  |

                    LDA #$02            
                    STA $14C8,x
                    STZ $B6,x
                    LDA #$F0
                    STA $AA,x
                    RTS                     ; return 

SMUSH_SPRITE        LDA #$0C                ; \ time to show semi-squashed sprite = $0C
                    STA $1FE2,x             ; /
                    LDA $157C,x 
                    EOR #$01                    
                    STA $157C,x 
                    RTS                     ; return

SPRITE_WINS         LDA $1497               ; \ if mario is invincible...
                    ORA $187A               ;  }  ... or mario on yoshi...
                    BNE NO_CONTACT          ; /   ... return
                    JSR SUB_HORZ_POS         ; \  set new sprite direction
                    TYA                     ;  }  
                    STA $157C,x             ; /
                    JSL $00F5B7             ; hurt mario
NO_CONTACT          RTS                     ; return

SPIN_KILL           LDA #$04                ; \ sprite status = 4 (being killed by spin jump)
                    STA $14C8,x             ; /   
                    LDA #$1F                ; \ set spin jump animation timer
                    STA $1540,x             ; /
                    JSL $07FC3B             ; show star animation
                    LDA #$08                ; \ play sound effect
                    STA $1DF9               ; /
                    RTS                     ; return

HAS_STAR            LDA #$02                ; \ sprite status = 2 (being killed by star)
                    STA $14C8,x             ; /
                    LDA #$D0                ; \ set y speed
                    STA $AA,x               ; /
                    JSR SUB_HORZ_POS         ; get new sprite direction
                    LDA KILLED_X_SPEED,y    ; \ set x speed based on sprite direction
                    STA $B6,x               ; /
                    INC $18D2               ; increment number consecutive enemies killed
                    LDA $18D2               ; \
                    CMP #$08                ; | if consecutive enemies stomped >= 8, reset to 8
                    BCC NO_RESET2           ; |
                    LDA #$08                ; |
                    STA $18D2               ; /   
NO_RESET2           JSL $02ACE5             ; give mario points
                    LDY $18D2               ; \ 
                    CPY #$08                ; | if consecutive enemies stomped < 8 ...
                    BCS NO_SOUND2           ; |
                    LDA STAR_SOUNDS,y       ; |    ... play sound effect
                    STA $1DF9               ; /
NO_SOUND2           RTS                     ; final return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; points routine - unknown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

STAR_SOUNDS         dcb $00,$13,$14,$15,$16,$17,$18,$19

SUB_STOMP_PTS       PHY                     ; 
                    LDA $1697               ; \
                    CLC                     ;  } 
                    ADC $1626,x             ; / some enemies give higher pts/1ups quicker??
                    INC $1697               ; increase consecutive enemies stomped
                    TAY                     ;
                    INY                     ;
                    CPY #$08                ; \ if consecutive enemies stomped >= 8 ...
                    BCS NO_SOUND            ; /    ... don't play sound 
                    LDA STAR_SOUNDS,y       ; \ play sound effect
                    STA $1DF9               ; /   
NO_SOUND            TYA                     ; \
                    CMP #$08                ;  | if consecutive enemies stomped >= 8, reset to 8
                    BCC NO_RESET            ;  |
                    LDA #$08                ; /
NO_RESET            JSL $02ACE5             ; give mario points
                    PLY                     ;
                    RTS                     ; return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics routine - specific to sprite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUB_GFX             LDA $1FE2,x             ; \ if time to show half smushed sprite > 0...
                    BEQ NOT_HALF_SMUSHED    ; |
                    LDA #$02                ; |    ...set sprite frame = 2 (half smushed)
                    STA $1602,x             ; /
NOT_HALF_SMUSHED    JSR GET_DRAW_INFO       ; after: Y = index to sprite tile map ($300)
                                            ;      $00 = sprite x position relative to screen boarder 
                                            ;      $01 = sprite y position relative to screen boarder  

                    LDA $1602,x             ; \ $index to tables
                    STA $03                 ; /
                    
                    LDA $1602,x             ; \ $02 = sprite direction
                    AND #$01
                    STA $02                 ; /
                    PHX                     ; push sprite index
                    STX $15E9

                    LDA $14C8,x
                    CMP #$02
                    BNE LOOP_START_2
                    ;STZ $03
                    LDA $15F6,x
                    ORA #$80
                    STA $15F6,x
LOOP_START_2                    

                    LDA $03                 ; / get index of tile (index to first tile of frame + current tile number)
FACING_LEFT         TAX                     ; \ 

                    LDA $00                 ; | tile x position = sprite x location ($00) + tile displacement
                    STA $0300,y             ; /

                    LDA $01                 ; | tile y position = sprite y location ($01) + tile displacement
                    STA $0301,y             ; /

                    LDA TILEMAP,x           ; \ store tile
                    STA $0302,y             ; / 
                    
                    LDX $15E9
                    LDA $15F6,x             ; load tile pallette
                    LDX $02                 ; \ if direction == 0...
                    BNE NO_FLIP             ;  |
                    ORA #$40                ;  |   ...flip tile
NO_FLIP             ORA $64                 ;  | 
                    STA $0303,y             ; / store tile properties
                   
                    TYA                     ; \ get index to sprite property map ($460)...
                    LSR A                   ; |    ...we use the sprite OAM index...
                    LSR A                   ; |    ...and divide by 4 because a 16x16 tile is 4 8x8 tiles
                    TAX                     ; | 
                    LDA #$02                ; | else show a full 16 x 16 tile
                    STA $0460,x             ; /

                    INY                     ; | increase index to sprite tile map ($300)...
                    INY                     ; |    ...we wrote 1 16x16 tile...
                    INY                     ; |    ...sprite OAM is 8x8...
                    INY                     ; |    ...so increment 4 times

                    PLX                     ; pull, X = sprite index
                    LDY #$FF                ; \ FF because we already wrote to $0460
                    LDA #$00                ; | A = number of tiles drawn - 1
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
; horizontal mario/sprite contact - shared
; Y = 1 if contact
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B817             ; Y = 1 if contact

SUB_HORZ_POS         LDY #$00                ;A:25D0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1020 VC:097 00 FL:31642
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

SUB_OFF_SCREEN_X0   LDA #$06                ; \ entry point of routine determines value of $03
                    BRA STORE_03            ; | 
SUB_OFF_SCREEN_X1   LDA #$04                ; |
                    BRA STORE_03            ; |
SUB_OFF_SCREEN_X2   LDA #$02                ; |
STORE_03            STA $03                 ; |
                    BRA START_SUB           ; |
SUB_OFF_SCREEN_X3  STZ $03                 ; /

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

NOP