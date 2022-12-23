;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn Block Bridge, adapted by mikeyk
;;
;; Description: 
;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    
                    ;symbolic names for ram addresses
                    MARIO_STATUS    = $19 
                    MARIO_X_SPEED   = $7B
                    MARIO_Y_SPEED   = $7D
                    MARIO_X_POS     = $94
                    MARIO_X_POS_HI  = $95
                    MARIO_Y_POS     = $96 
                    MARIO_Y_POS_HI  = $97
                    SPRITE_Y_SPEED  = $AA
                    SPRITE_X_SPEED  = $B6
                    SPRITE_STATE    = $C2       
                    SPRITE_Y_POS    = $D8
                    SPRITE_X_POS    = $E4
                    SPRITE_Y_POS_HI = $14D4
                    SPRITE_X_POS_HI = $14E0
                    SPR_OBJ_STATUS  = $1588
                    IS_ON_YOSHI     = $187A 
                    SPRITE_OAM      = $15EA
                    
                    EXTRA_BITS = $7FAB10
                    
                    ON_THIS         = $1528
                    ON_LAST         = $151C
                    
                    ;data
                    YOSHI_OFFSET    = $10
                    SIZE            = $08
                    
Y_SPEED             dcb $00,$F8,$00,$F8,$00
X_SPEED             dcb $00,$00,$F8,$00,$08
MOVEMENT            dcb $00,$00,$FF,$00,$01
                    
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite init JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
                    LDA SPRITE_X_POS,x
                    ORA #$08
                    STA SPRITE_X_POS,x
                    STZ ON_THIS,x
                    STZ SPRITE_STATE,x
                    RTL
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite code JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "MAIN"                                    
                    PHB                     
                    PHK                     
                    PLB                     
                    JSR SPRITE_CODE_START   
                    PLB                     
                    RTL      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    
SPRITE_CODE_START   JSR SUB_OFF_SCREEN_X0
                    JSR SUB_GFX 
                    LDA $9D
                    BNE BAIL
                    JSR SUB_B852
                    JSR SUB_MOVEMENT
BAIL                RTS                     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUB_MOVEMENT        LDA SPRITE_STATE,x
                    TAY
                    
                    LDA X_SPEED,y
                    STA SPRITE_X_SPEED,x
                    LDA Y_SPEED,y
                    STA SPRITE_Y_SPEED,x
                    
                    LDA ON_THIS,x
                    BEQ DONT_MOVE_MARIO

                    LDA $77                 ;if mario is touching the ceiling
                    AND #$08
                    BEQ MOVE_MARIO
                    ;LDA SPRITE_STATE,x
                    ;AND #$01
                    ;BEQ MOVE_MARIO                    
                    ;LDA #$18
                    ;STA $154C,x                    
                    
                    STZ $14C8,x

MOVE_MARIO                             
                    LDA $13
                    AND #$01
                    BNE DONT_MOVE_MARIO
                    
           
                    PHX
                    LDX #$00
                    LDA MOVEMENT,y
                    BPL GET_HI
                    DEX
GET_HI              CLC                     
                    ADC MARIO_X_POS       
                    STA MARIO_X_POS       
                    TXA           
                    ADC MARIO_X_POS_HI       
                    STA MARIO_X_POS_HI                        
                    PLX
DONT_MOVE_MARIO                    
                    
                    JSL $018022             ;update x position, no gravity
                    JSL $01801A             ;update y position, no gravity 
                    RTS
                    
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TILEMAP             dcb $EA,$EA,$A6,$A6,$C2,$C4,$A6,$A6,$C4,$C2
PROPERTIES          dcb $00,$40,$00,$40,$00,$00,$00,$40,$40,$40
X_OFFSET            dcb $F8,$08

SUB_GFX 			JSR GET_DRAW_INFO
                    PHX                     ; push sprite index

                    LDX #$01                ; loop counter = (number of tiles per frame) - 1
LOOP_START          PHX                     ; push current tile number
                    STX $03
                    
                    LDA $00                 ; \ tile x position = sprite x location ($00)
                    CLC
                    ADC X_OFFSET,x
                    STA $0300,y             ; /
                    
                    LDA $01                 ; \ tile y position = sprite y location ($01) + tile displacement
                    STA $0301,y             ; /
                    
                    LDX $15E9 
                    LDA SPRITE_STATE,x
                    ASL A
                    CLC
                    ADC $03
                    TAX
                    LDA TILEMAP,x           ; \ store tile
                    STA $0302,y             ; / 
        
                    LDA PROPERTIES,x
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

                    STZ $00       
                    STZ $01       
                    STZ $02       
                    STZ $03       
                    LDY #$00                 
                    LDA #SIZE   
                    STA $0000,y   
                    LSR A         
                    STA $0001,y   
                                        
                    LDA $00       
                    PHA           
                    LDA $02       
                    PHA       
                    
                    LDY #$02                ; \ 02, because we didn't write to 460 yet
                    LDA #$01                ;  | A = number of tiles drawn - 1
                    JSL $01B7B3             ; / don't draw if offscreen

                    PLA           
                    STA $02       
                    PLA           
                    STA $00       
                    RTS                    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; platform code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUB_B852            LDA ON_THIS,x
                    STA ON_LAST,x
                    STZ ON_THIS,x
                    LDA $15C4,x 
                    ORA $154C,x   
                    BNE RETURN05
                          
                    LDA $71       
                    CMP #$01                
                    BCS RETURN05     
                    
                    JSR SUB_B8FF
                    BCC RETURN05      
                    
                    LDA SPRITE_Y_POS,x    
                    SEC          
                    SBC $1C      
                    STA $02      
                    SEC          
                    SBC $0D      
                    STA $09      
                    LDA $80      
                    CLC          
                    ADC #$18     
                    CMP $09      
                    BCS LBL06      
                    LDA MARIO_Y_SPEED    
                    BMI RETURN05     
                    STZ MARIO_Y_SPEED  
                        
                    LDA #$01     
                    STA $1471    
                    LDA $0D      
                    CLC          
                    ADC #$1F     
                    
                    LDY IS_ON_YOSHI         ; adjust for yoshi  
                    BEQ NO_YOSHI      
                    CLC
                    ADC #YOSHI_OFFSET
NO_YOSHI
                    STA $00      
                    
                    LDA #$01
                    STA ON_THIS,x
                    
                    LDA EXTRA_BITS,x
                    AND #$04
                    BNE NO_NEW_STATE

                    LDA ON_LAST,x
                    BNE NO_NEW_STATE
                    LDA SPRITE_STATE,x
                    AND #$03
                    INC A
                    STA SPRITE_STATE,x
                    
                    LDA #$0B
                    STA $1DF9
                    
NO_NEW_STATE                    
                   
                    LDA SPRITE_Y_POS,x    
                    SEC          
                    SBC $00      
                    STA MARIO_Y_POS      
                    LDA SPRITE_Y_POS_HI,x  
                    SBC #$00     
                    STA MARIO_Y_POS_HI      
                    LDY #$00     
                    LDA $1491               ;amount to move mario 
                    BPL LBL04      
                    DEY
LBL04
                    CLC                     
                    ADC MARIO_X_POS       
                    STA MARIO_X_POS       
                    TYA           
                    ADC MARIO_X_POS_HI       
                    STA MARIO_X_POS_HI       
RETURN05            RTS           

LBL06               LDA $02       
                    CLC           
                    ADC $0D       
                    STA $02       
                    LDA #$FF      
                    LDY $73       
                    BNE LBL07      
                    LDY MARIO_STATUS       
                    BNE SMALL_MARIO
LBL07
                    LDA #$08   
SMALL_MARIO
                    CLC                     
                    ADC $80      
                    CMP $02      
                    BCC LBL10      
                    LDA MARIO_Y_SPEED       
                    BPL LBL09    
                    LDA #$10                
                    ;STA MARIO_Y_SPEED    
LBL09
                    RTS       
LBL10
                    LDA $0E       
                    CLC           
                    ADC #$10      
                    STA $00       
                    LDY #$00      
                    LDA SPRITE_X_POS,x     
                    SEC           
                    SBC $1A       
                    CMP $7E       
                    BCC LBL11     
                    LDA $00       
                    EOR #$FF      
                    INC A         
                    STA $00       
                    DEY     
LBL11
                    LDA SPRITE_X_POS,x     
                    CLC           
                    ADC $00       
                    ;STA MARIO_X_POS       
                    TYA           
                    ADC SPRITE_X_POS_HI,x   
                    ;STA MARIO_X_POS_HI       
                    ;STZ MARIO_X_SPEED       
                    RTS                     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make standable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUB_B8FF
                    LDA $00      
                    STA $0E      
                    LDA $02      
                    STA $0D      
                    LDA SPRITE_X_POS,x    
                    SEC          
                    SBC $00      
                    STA $04      
                    LDA SPRITE_X_POS_HI,x  
                    SBC #$00     
                    STA $0A      
                    LDA $00      
                    ASL A        
                    CLC          
                    ADC #$10     
                    STA $06      
                    LDA SPRITE_Y_POS,x    
                    SEC          
                    SBC $02      
                    STA $05      
                    LDA SPRITE_Y_POS_HI,x  
                    SBC #$00     
                    STA $0B      
                    LDA $02      
                    ASL A        
                    CLC          
                    ADC #$10     
                    STA $07      
                    JSL $03B664  
                    JSL $03B72B  
                    RTS 

                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GET_DRAW_INFO
; This is a helper for the graphics routine.  It sets off screen flags, and sets up
; variables.  It will return with the following:
;
;		Y = index to sprite OAM ($300)
;		$00 = sprite x position relative to screen boarder
;		$01 = sprite y position relative to screen boarder  
;
; It is adapted from the subroutine at $03B760
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SPR_T1              dcb $0C,$1C
SPR_T2              dcb $01,$02

GET_DRAW_INFO       STZ $186C,x             ; reset sprite offscreen flag, vertical
                    STZ $15A0,x             ; reset sprite offscreen flag, horizontal
                    LDA $E4,x               ; \
                    CMP $1A                 ;  | set horizontal offscreen if necessary
                    LDA SPRITE_X_POS_HI,x             ;  |
                    SBC $1B                 ;  |
                    BEQ ON_SCREEN_X         ;  |
                    INC $15A0,x             ; /

ON_SCREEN_X         LDA SPRITE_X_POS_HI,x             ; \
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
                    ADC SPR_T1,y            ;  |
                    PHP                     ;  |
                    CMP $1C                 ;  | (vert screen boundry)
                    ROL $00                 ;  |
                    PLP                     ;  |
                    LDA SPRITE_Y_POS_HI,x             ;  | 
                    ADC #$00                ;  |
                    LSR $00                 ;  |
                    SBC $1D                 ;  |
                    BEQ ON_SCREEN_Y         ;  |
                    LDA $186C,x             ;  | (vert offscreen)
                    ORA SPR_T2,y            ;  |
                    STA $186C,x             ;  |
ON_SCREEN_Y         DEY                     ;  |
                    BPL ON_SCREEN_LOOP      ; /

                    LDY SPRITE_OAM,x        ; get offset to sprite OAM
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
; SUB_OFF_SCREEN
; This subroutine deals with sprites that have moved off screen
; It is adapted from the subroutine at $01AC0D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    
SPR_T12             dcb $40,$B0
SPR_T13             dcb $01,$FF
SPR_T14             dcb $30,$C0,$A0,$C0,$A0,$F0,$60,$90		;bank 1 sizes
		            dcb $30,$C0,$A0,$80,$A0,$40,$60,$B0		;bank 3 sizes
SPR_T15             dcb $01,$FF,$01,$FF,$01,$FF,$01,$FF		;bank 1 sizes
					dcb $01,$FF,$01,$FF,$01,$00,$01,$FF		;bank 3 sizes

SUB_OFF_SCREEN_X1   LDA #$02                ; \ entry point of routine determines value of $03
                    BRA STORE_03            ;  | (table entry to use on horizontal levels)
SUB_OFF_SCREEN_X2   LDA #$04                ;  | 
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X3   LDA #$06                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X4   LDA #$08                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X5   LDA #$0A                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X6   LDA #$0C                ;  |
                    BRA STORE_03            ;  |
SUB_OFF_SCREEN_X7   LDA #$0E                ;  |
STORE_03			STA $03					;  |            
					BRA START_SUB			;  |
SUB_OFF_SCREEN_X0   STZ $03					; /

START_SUB           JSR SUB_IS_OFF_SCREEN   ; \ if sprite is not off screen, return
                    BEQ RETURN_35           ; /
                    LDA $5B                 ; \  goto VERTICAL_LEVEL if vertical level
                    AND #$01                ; |
                    BNE VERTICAL_LEVEL      ; /     
                    LDA $D8,x               ; \
                    CLC                     ; | 
                    ADC #$50                ; | if the sprite has gone off the bottom of the level...
                    LDA SPRITE_Y_POS_HI,x             ; | (if adding 0x50 to the sprite y position would make the high byte >= 2)
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
                    SBC SPRITE_X_POS_HI,x             ;A:8AFF X:0009 Y:0001 D:0000 DB:01 S:01F1 P:envMXdizCHC:1140 VC:176 00 FL:205
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
                    LDA SPRITE_X_POS_HI,x             ;  |
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
                    SBC SPRITE_Y_POS_HI,x             ;A:0000 X:0009 Y:0001 D:0000 DB:01 S:01F3 P:eNVMXdizcHC:0342 VC:251 00 FL:5379
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

