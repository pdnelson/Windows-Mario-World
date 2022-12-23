;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Roto Disc, by mikeyk
;;
;; Description: This sprite circles a block.  Mario cannot touch it, even with a spin
;; jump.  The direction of rotation is determined by the x position.  The radius is
;; specified in the .cfg file
;;
;; NOTE: Like the Ball and Chain, this enemy should not be used in levels that
;; allow Yoshi.
;;
;; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                    RADIUS = $28
                    CIRCLE_COORDS = $07F7DB
                    
                    SPRITE_STATE = $C2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite initialization JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
                    RTL
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite main JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
                    dcb "MAIN"                        
                    PHB                     ; \
                    PHK                     ;  | main sprite function, just calls local subroutine
                    PLB                     ;  |
                    LDA #$02
                    STA SPRITE_STATE,x
                    JSR START_SPRITE_CODE   ;  |
                    DEC SPRITE_STATE,x
                    JSR START_SPRITE_CODE   ;  |
                    DEC SPRITE_STATE,x
                    JSR START_SPRITE_CODE   ;  |
                    PLB                     ;  |
                    RTL                     ; /


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main sprite sprite code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RADIUS              dcb $28,$18,$08
CLOCK_SPEED         dcb $03,$FD

START_SPRITE_CODE   JSR SUB_OFF_SCREEN_X1
		    LDY SPRITE_STATE,x
                    LDA RADIUS,y
                    STA $187B,x
                    
                    LDA $9D
                    BNE LABEL92
                    
                    LDA SPRITE_STATE,x
                    CMP #$02
                    BNE LABEL92
		    
                    LDY #$00
                    LDA $E4,x
                    AND #$10
                    BNE LABEL90
                    INY
LABEL90             TYA
                    STA $157C,x
                    LDA CLOCK_SPEED,y
                    LDY #$00
                    CMP #$00
                    BPL LABEL91
                    DEY
LABEL91             CLC
                    ADC $1602,x
                    STA $1602,x
                    TYA
                    ADC $151C,x
                    AND #$01
                    STA $151C,x
LABEL92             LDA $151C,x
                    STA $01
                    LDA $1602,x
                    STA $00
                    REP #$30
                    LDA $00
                    CLC
                    ADC.W #$0080
                    AND.W #$01FF
                    STA $02
                    LDA $00
                    AND.W #$00FF
                    ASL A
                    TAX
                    LDA $07F7DB,x
                    STA $04
                    LDA $02
                    AND.W #$00FF
                    ASL A
                    TAX
                    LDA $07F7DB,x
                    STA $06
                    SEP #$30
                    LDX $15E9
                    LDA $04
                    STA $4202
                    LDA $187B,x
                    LDY $05
                    BNE LABEL93
                    STA $4203
                    ASL $4216
                    LDA $4217
                    ADC #$00
LABEL93             LSR $01
                    BCC LABEL94
                    EOR #$FF                ; \ reverse direction of rotation
                    INC A                   ; /
LABEL94             STA $04
                    LDA $06
                    STA $4202
                    LDA $187B,x
                    LDY $07
                    BNE LABEL95
                    STA $4203
                    ASL $4216
                    LDA $4217
                    ADC #$00
LABEL95             LSR $03
                    BCC LABEL96
                    EOR #$FF
                    INC A
LABEL96             STA $06
                    LDA $E4,x
                    PHA
                    LDA $14E0,x
                    PHA
                    LDA $D8,x
                    PHA
                    LDA $14D4,x
                    PHA
                    LDY $0F86,x
                    STZ $00
                    LDA $04
                    BPL LABEL97
                    DEC $00
LABEL97             CLC
                    ADC $E4,x
                    STA $E4,x
                    PHP
                    PHA
                    SEC
                    SBC $1534,x
                    STA $1528,x
                    PLA
                    STA $1534,x
                    PLP
                    LDA $14E0,x
                    ADC $00
                    STA $14E0,x
                    STZ $01
                    LDA $06
                    BPL LABEL98
                    DEC $01
LABEL98             CLC
                    ADC $D8,x
                    STA $D8,x
                    LDA $14D4,x
                    ADC $01
                    STA $14D4,x
            
                    JSL $01A7DC             ; check for mario/sprite contact
                    BCC RETURN_EXTRA          ; (carry set = mario/sprite contact)
                    LDA $1490               ; \ if mario star timer > 0 ...
                    BNE HAS_STAR            ; /    ... goto HAS_STAR

SPRITE_WINS         LDA $1497               ; \ if mario is invincible...
                    ORA $187A               ;  }  ... or mario on yoshi...
                    BNE RETURN_EXTRA          ; /   ... return
                    JSL $00F5B7             ; hurt mario

RETURN_EXTRA        JSR SUB_GFX
                    
                    LDA $14C8,x             ; \ if sprite status != 8...
                    CMP #$08
                    BEQ ALIVE
                    
                    PLA
                    PLA
                    PLA
                    PLA
                    BRA DONE
                    
ALIVE               PLA     
                    STA $14D4,x
                    PLA        
                    STA $D8,x  
                    PLA        
                    STA $14E0,x
                    PLA        
                    STA $E4,x                   
                    
DONE                LDA SPRITE_STATE,x
                    BNE RETURN78
                    LDY #$02                ; \ 02 because we haven't written to $0460
                    LDA #$02                ; | A = number of tiles drawn - 1
                    JSL $01B7B3             ; / don't draw if offscreen
               
RETURN78            RTS

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
NO_SOUND2           BRA RETURN_EXTRA                    ; final return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics routine - specific to sprite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    TILEMAP     dcb $E8,$EA,$E8,$EA
                    PROPERTIES  dcb $00,$00,$C0,$C0


SUB_GFX             JSR GET_DRAW_INFO       ; after: Y = index to sprite tile map ($300)
                                            ;      $00 = sprite x position relative to screen boarder 
                                            ;      $01 = sprite y position relative to screen boarder  
                    
                    LDA SPRITE_STATE,x
                    ASL A
                    ASL A
                    STA $02
                    TYA
                    CLC
                    ADC $02
                    TAY
                   
                    LDA $157C,x
                    STA $02
                    
                    LDA $00                 ; | tile x position = sprite x location ($00) + tile displacement
                    STA $0300,y             ; /

                    LDA $01                 ; | tile y position = sprite y location ($01) + tile displacement
                    STA $0301,y             ; /

                    PHX
                    LDA $14
                    LSR A
                    LSR A
                    AND #$03
                    TAX
                    LDA TILEMAP,x               ; \ store tile
                    STA $0302,y             ; / 
                    
                    LDA PROPERTIES,x
                    LDX $15E9
                    ORA $15F6,x             ; load tile pallette
                    LDX $02
                    BNE NO_XOR
                    EOR #$40
NO_XOR              ORA $64                 ;  | 
                    STA $0303,y             ; / store tile properties
                    PLX
                   
                    INY                     ; | increase index to sprite tile map ($300)...
                    INY                     ; |    ...we wrote 4 bytes of data...
                    INY                     ; |    
                    INY                     ; |    ...so increment 4 times

                    RTS                     ; return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; points routine - unknown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

KILLED_X_SPEED      dcb $F0,$10
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
; graphics routine helper - shared
; sets off screen flags and sets index to OAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $02D374

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

;OFF SCREEN ROUTINE
;1. Off screen tables
SPR_T12             dcb $40,$B0
SPR_T13             dcb $01,$FF
SPR_T14             dcb $30,$C0,$A0,$C0,$A0,$F0,$60,$90		;bank 1 sizes
		            dcb $30,$C0,$A0,$80,$A0,$40,$60,$B0		;bank 3 sizes
SPR_T15             dcb $01,$FF,$01,$FF,$01,$FF,$01,$FF		;bank 1 sizes
					dcb $01,$FF,$01,$FF,$01,$00,$01,$FF		;bank 3 sizes
;2. Code
SUB_OFF_SCREEN_X1   LDA #$02                ; \ entry point of routine determines value of $03
STORE_03            STA $03					;  |            

START_SUB           JSR SUB_IS_OFF_SCREEN   ; \ if sprite is not off screen, return
                    BEQ RETURN_35           ; /
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
                    BNE RETURN_35           ; /
                    LDA $13                 ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZcHC:0756 VC:176 00 FL:205
                    AND #$01                ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0780 VC:176 00 FL:205
                    ORA $03                 ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0796 VC:176 00 FL:205
                    STA $01                 ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0820 VC:176 00 FL:205
                    TAY                     ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0844 VC:176 00 FL:205
                    LDA $1A                 ;A:8A01 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizcHC:0858 VC:176 00 FL:205
                    CLC                     ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZcHC:0882 VC:176 00 FL:205
                    ADC SPR_T14,y           ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZcHC:0896 VC:176 00 FL:205
                    ROL $00                 ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizcHC:0928 VC:176 00 FL:205
                    CMP $E4,x               ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizCHC:0966 VC:176 00 FL:205
                    PHP                     ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:0996 VC:176 00 FL:205
                    LDA $1B                 ;A:8AC0 X:0009 Y:0001 D:0000 DB:01 S:01F0 P:envMXdizCHC:1018 VC:176 00 FL:205
                    LSR $00                 ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F0 P:envMXdiZCHC:1042 VC:176 00 FL:205
                    ADC SPR_T15,y           ;A:8A00 X:0009 Y:0001 D:0000 DB:01 S:01F0 P:envMXdizcHC:1080 VC:176 00 FL:205
                    PLP                     ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F0 P:eNvMXdizcHC:1112 VC:176 00 FL:205
                    SBC $14E0,x             ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1140 VC:176 00 FL:205
                    STA $00                 ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizCHC:1172 VC:176 00 FL:205
                    LSR $01                 ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:eNvMXdizCHC:1196 VC:176 00 FL:205
                    BCC SPR_L31             ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZCHC:1234 VC:176 00 FL:205
                    EOR #$80                ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdiZCHC:1250 VC:176 00 FL:205
                    STA $00                 ;A:8A7F X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1266 VC:176 00 FL:205
SPR_L31             LDA $00                 ;A:8A7F X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1290 VC:176 00 FL:205
                    BPL RETURN_35           ;A:8A7F X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1314 VC:176 00 FL:205
ERASE_SPRITE        LDA $14C8,x             ; \ if sprite status < 8, permanently erase sprite
                    CMP #$08                ; |
                    BCC KILL_SPRITE         ; /    
                    LDY $161A,x             ;A:FF08 X:0007 Y:0001 D:0000 DB:01 S:01F3 P:envMXdiZCHC:1108 VC:059 00 FL:2878
                    CPY #$FF                ;A:FF08 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdiZCHC:1140 VC:059 00 FL:2878
                    BEQ KILL_SPRITE         ;A:FF08 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdizcHC:1156 VC:059 00 FL:2878
                    LDA #$00                ;A:FF08 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdizcHC:1172 VC:059 00 FL:2878
                    STA $1938,y             ;A:FF00 X:0007 Y:0000 D:0000 DB:01 S:01F3 P:envMXdiZcHC:1188 VC:059 00 FL:2878
KILL_SPRITE         STZ $14C8,x             ; erase sprite
RETURN_35           RTS                     ; return

VERTICAL_LEVEL      LDA $167A,x             ; \ if "process offscreen" flag is set, return
                    AND #$04                ; |
                    BNE RETURN_35           ; /
                    LDA $13                 ; \
                    LSR A                   ; | 
                    BCS RETURN_35           ; /
                    LDA $E4,x               ; \ 
                    CMP #$00                ;  | if the sprite has gone off the side of the level...
                    LDA $14E0,x             ;  |
                    SBC #$00                ;  |
                    CMP #$02                ;  |
                    BCS ERASE_SPRITE        ; /  ...erase the sprite
                    LDA $13                 ;A:0000 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:1218 VC:250 00 FL:5379
                    LSR A                   ;A:0016 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1242 VC:250 00 FL:5379
                    AND #$01                ;A:000B X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1256 VC:250 00 FL:5379
                    STA $01                 ;A:0001 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1272 VC:250 00 FL:5379
                    TAY                     ;A:0001 X:0009 Y:00E4 D:0000 DB:01 S:01F3 P:envMXdizcHC:1296 VC:250 00 FL:5379
                    LDA $1C                 ;A:001A X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0052 VC:251 00 FL:5379
                    CLC                     ;A:00BD X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0076 VC:251 00 FL:5379
                    ADC SPR_T12,y           ;A:00BD X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0090 VC:251 00 FL:5379
                    ROL $00                 ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F3 P:enVMXdizCHC:0122 VC:251 00 FL:5379
                    CMP $D8,x               ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNVMXdizcHC:0160 VC:251 00 FL:5379
                    PHP                     ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNVMXdizcHC:0190 VC:251 00 FL:5379
                    LDA.W $001D             ;A:006D X:0009 Y:0001 D:0000 DB:01 S:01F2 P:eNVMXdizcHC:0212 VC:251 00 FL:5379
                    LSR $00                 ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F2 P:enVMXdiZcHC:0244 VC:251 00 FL:5379
                    ADC SPR_T13,y           ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F2 P:enVMXdizCHC:0282 VC:251 00 FL:5379
                    PLP                     ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F2 P:envMXdiZCHC:0314 VC:251 00 FL:5379
                    SBC $14D4,x             ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNVMXdizcHC:0342 VC:251 00 FL:5379
                    STA $00                 ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0374 VC:251 00 FL:5379
                    LDY $01                 ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0398 VC:251 00 FL:5379
                    BEQ SPR_L38             ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0422 VC:251 00 FL:5379
                    EOR #$80                ;A:00FF X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0438 VC:251 00 FL:5379
                    STA $00                 ;A:007F X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0454 VC:251 00 FL:5379
SPR_L38             LDA $00                 ;A:007F X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0478 VC:251 00 FL:5379
                    BPL RETURN_35           ;A:007F X:0009 Y:0001 D:0000 DB:01 S:01F3 P:envMXdizcHC:0502 VC:251 00 FL:5379
                    BMI ERASE_SPRITE        ;A:8AFF X:0002 Y:0000 D:0000 DB:01 S:01F3 P:eNvMXdizcHC:0704 VC:184 00 FL:5490

SUB_IS_OFF_SCREEN   LDA $15A0,x             ; \ if sprite is on screen, accumulator = 0 
                    ORA $186C,x             ; |  
                    RTS                     ; / return

            