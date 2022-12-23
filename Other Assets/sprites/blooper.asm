;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Blooper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; symbolic names for RAM addresses
  IS_MOVING_HORZTLY = $151C
  ATTACK_TIMER = $1540
  DIRECTION = $C2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
; INIT and MAIN routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	            DCB "INIT"
                    LDA #$18                
                    STA ATTACK_TIMER,X             
                    RTL                       

                    DCB "MAIN"
                    PHB                       
                    PHK                       
                    PLB                       
                    JSR BlooperMain
                    PLB                       
                    RTL
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BlooperMain:	    JSR SubGfx	              ; Draw sprite

                    LDA $14C8,X               ; \ Return if status != Normal
                    CMP #$08                  ;  |
                    BNE Return                ; /
                    LDA $9D                   ; \ Return if sprites are locked
                    BNE Return		      ; /

	            JSR SubOffScreen          ; Only process while on screen
	
                    JSR MoveSprite
	            JSL $018022               ; Update x position, no gravity
                    JSL $01801A               ; Update y position, no gravity
	
                    LDA #$00                  ; \ Make stompable if Mario isn't Swimming
                    LDY $75	              ;  |
                    BNE NotSwimming           ;  |
                    LDA #$10                  ;  |
NotSwimming:        STA $1656,X               ; /
	
                    JSL $01A7DC               ; Handle Mario/sprite contact
	
Return:             RTS                       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Handle movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SpeedY:             dcb $08,$EB
InitialSpeedX:      dcb $06,$FA
AccelerationX:      dcb $02,$FE
MaxSpeedX:          dcb $20,$E0
	
MoveSprite:         STZ $1602,X	              ; Initialize animation frame

                    LDA IS_MOVING_HORZTLY,X   ; \ If we are in the middle of an attack...
                    BNE SetAttackSpeed        ; / ...just update the speed

		    LDY #$00 	              ; \ Set descending speed
                    LDA SpeedY,Y	      ;  |
                    STA $AA,X                 ; /
	
                    LDA ATTACK_TIMER,X        ; \ Change animation frame if time to attack == #$0B
	            CMP #$0B                  ;  |
		    BCS NoChangeImage         ;  |
                    LDA #$01                  ;  |
	            STA $1602,X		      ; /
NoChangeImage:	    LDA ATTACK_TIMER,X        ; \ Return if not time to attack
        	    BNE Return2               ; /
                  
                    JSR SubVertPos            ; \ Return if not close enough to Mario
	            TYA                       ;  |
                    BEQ Return2               ; /
	
StartAttack:        STZ $00                   ; \ Set Y = direction Blooper should attack
	            JSR GetRand               ;  |
		    CMP #$01                  ;  |
	            BNE MoveTowardMario       ;  |
	            INC $00                   ;  | 
MoveTowardMario:    JSR SubHorzPos            ;  | He will tend to go towards Mario...
                    TYA                       ;  |  ...but there is a semi-random factor
                    EOR $00                   ;  |
                    TAY                       ; /
                    STA DIRECTION,X           ; Set acceleration direction
                    INC IS_MOVING_HORZTLY,X   ; Set movement flag
                    LDA InitialSpeedX,Y       ; \ Set inital X speed
                    STA $B6,X		      ; / 
	
SetAttackSpeed:     LDY #$01 	              ; \ Set Attacking Y Speed
                    LDA SpeedY,Y	      ;  |
                    STA $AA,X                 ; /
	
                    LDY DIRECTION,X	      ; \
                    LDA $B6,X                 ;  | Update X speed
                    CLC                       ;  |
                    ADC AccelerationX,Y       ;  | 
                    STA $B6,X                 ;  | If the speed is 0...
                    BEQ DoneAttacking         ; / ...we are done attacking

                    CMP MaxSpeedX,Y           ; \ If we have hit the max speed...
                    BNE Return2               ;  | ...we flip the direction...
                    LDA DIRECTION,X           ;  | ...to begin deccelerating
                    EOR #$01                  ;  | 
                    STA DIRECTION,X           ; /
                    BRA Return2             
	
DoneAttacking:      STZ IS_MOVING_HORZTLY,X   ; Reset movement flag
	            LDA #$36	              ; \ Set timer until next attack
	            STA ATTACK_TIMER,X        ; /

Return2:            RTS			      

GetRand:            JSL $01ACF9
                    AND #$07
                    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Graphics Routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SpriteTiles:        dcb $E6,$E6,$E5,$E5	      ; Normal
	            dcb $E6,$E6,$F5,$F5	      ; Compressed
	            dcb $E6,$E6,$F5,$F5	      ; Dead
SpriteTileSize:     dcb $02,$02,$00,$00
	            dcb $00,$00,$00,$00
	            dcb $00,$00,$00,$00	
SpriteTileDispY:    dcb $F8,$F8,$08,$08
     	            dcb $00,$00,$08,$08
   	            dcb $08,$08,$00,$00
SpriteTileDispX:    dcb $00,$00,$00,$08
     	            dcb $00,$08,$00,$08
   	            dcb $00,$08,$00,$08	
SpriteGfxFlip:      dcb $00,$40	
	
SubGfx:             JSR GetDrawInfo
		    LDA $15F6,X	              ; \ Store palette information
                    STA $02		      ; /
        
                    LDA $14C8,X               ; \ If sprite has been killed...
                    CMP #$02                  ;  | 
                    BNE NoStarKill  	      ;  |
                    LDA #$02                  ;  | ...set death frame...
                    STA $1602,X               ;  |
        	    LDA $02                   ;  |
                    ORA #$80                  ;  | ...flip tile
                    STA $02                   ; /
	
NoStarKill:         LDA $1602,X	              ; \ Store frame offset
	            ASL                       ;  |
	            ASL                       ;  |
	            STA $03                   ; /
	
                    PHX                       
                    LDX #$03
GfxLoopStart:       PHX
        
                    TXA                       
                    CLC                       
                    ADC $03
                    TAX
	            PHX
                    LDA $00                   
                    CLC                       
                    ADC SpriteTileDispX,X       
                    STA $0300,Y             
                    LDA $01                   
                    CLC                       
                    ADC SpriteTileDispY,X       
                    STA $0301,Y
                    LDA SpriteTiles,X       
                    STA $0302,Y
		    TXA
	            AND #$01
	            TAX	
                    LDA SpriteGfxFlip,X       
                    ORA $02                   
                    ORA $64                   
                    STA $0303,Y

                    PLX
	            LDA SpriteTileSize,X
	            PHA
	            TYA
	            LSR
	            LSR
	            TAX
	            PLA
                    STA $0460,X	
	
                    PLX                       
                    INY                       ; \ increase index to sprite tile map ($300)...
                    INY                       ;  |    ...we wrote 4 bytes...
                    INY                       ;  |
                    INY                       ; /    ...so increment 4 times
                    DEX                       
                    BPL GfxLoopStart
                    PLX                       

                    LDY #$FF		      ; We already wrote to $0460
                    LDA #$03                  ; We wrote 4 tiles
                    JSL $01B7B3             
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
 	
DATA_03B75C:        dcb $0C,$1C
DATA_03B75E:        dcb $01,$02

GetDrawInfo:        STZ $186C,X               ; Reset sprite offscreen flag, vertical 
                    STZ $15A0,X               ; Reset sprite offscreen flag, horizontal 
                    LDA $E4,X                 ; \ 
                    CMP $1A                   ;  | Set horizontal offscreen if necessary 
                    LDA $14E0,X               ;  | 
                    SBC $1B                   ;  | 
                    BEQ ADDR_03B774           ;  | 
                    INC $15A0,X               ; / 
ADDR_03B774:        LDA $14E0,X               ; \ 
                    XBA                       ;  | Mark sprite invalid if far enough off screen 
                    LDA $E4,X                 ;  | 
                    REP #$20                  ; Accum (16 bit) 
                    SEC                       ;  | 
                    SBC $1A                   ;  | 
                    CLC                       ;  | 
                    ADC.W #$0040              ;  | 
                    CMP.W #$0180              ;  | 
                    SEP #$20                  ; Accum (8 bit) 
                    ROL                       ;  | 
                    AND.B #$01                ;  | 
                    STA $15C4,X               ;  | 
                    BNE ADDR_03B7CF           ; /  
                    LDY.B #$00                ; \ set up loop: 
                    LDA $1662,X               ;  |  
                    AND.B #$20                ;  | if not smushed (1662 & 0x20), go through loop twice 
                    BEQ ADDR_03B79A           ;  | else, go through loop once 
                    INY                       ; /                        
ADDR_03B79A:        LDA $D8,X                 ; \                        
                    CLC                       ;  | set vertical offscree 
                    ADC DATA_03B75C,Y         ;  |                       
                    PHP                       ;  |                       
                    CMP $1C                   ;  | (vert screen boundry) 
                    ROL $00                   ;  |                       
                    PLP                       ;  |                       
                    LDA $14D4,X               ;  |                       
                    ADC.B #$00                ;  |                       
                    LSR $00                   ;  |                       
                    SBC $1D                   ;  |                       
                    BEQ ADDR_03B7BA           ;  |                       
                    LDA $186C,X               ;  | (vert offscreen)      
                    ORA DATA_03B75E,Y         ;  |                       
                    STA $186C,X               ;  |                       
ADDR_03B7BA:        DEY                       ;  |                       
                    BPL ADDR_03B79A           ; /                        
                    LDY $15EA,X               ; get offset to sprite OAM                           
                    LDA $E4,X                 ; \ 
                    SEC                       ;  |                                                     
                    SBC $1A                   ;  |                                                    
                    STA $00                   ; / $00 = sprite x position relative to screen boarder 
                    LDA $D8,X                 ; \                                                     
                    SEC                       ;  |                                                     
                    SBC $1C                   ;  |                                                    
                    STA $01                   ; / $01 = sprite y position relative to screen boarder 
                    RTS                       ; Return 

ADDR_03B7CF:        PLA                       ; \ Return from *main gfx routine* subroutine... 
                    PLA                       ;  |    ...(not just this subroutine) 
                    RTS                       ; / 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_HORZ_POS
; This routine determines which side of the sprite Mario is on.  It sets the Y register
; to the direction such that the sprite would face Mario
; It is ripped from $03B817
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubHorzPos:         LDY #$00
		    LDA $94	
		    SEC 
		    SBC $E4,x
		    STA $0F	
		    LDA $95	
		    SBC $14E0,x
		    BPL SPR_L16 
		    INY 
SPR_L16		    RTS 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_VERT_POS
; This routine determines if Mario is above or below the sprite.  It sets the Y register
; to the direction such that the sprite would face Mario
; It is ripped from $03B829
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubVertPos:	    LDY #$00             
                    LDA $96              
                    SEC                  
                    SBC $D8,x            
                    LDA $97              
                    SBC $14D4,x          
                    BPL LABEL11          
                    INY                  
LABEL11             RTS                  
        	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUB_OFF_SCREEN
; This subroutine deals with sprites that have moved off screen
; It is adapted from the subroutine at $01AC0D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
DATA_01AC0D:        dcb $40,$B0
DATA_01AC0F:        dcb $01,$FF
DATA_01AC11:        dcb $30,$C0
DATA_01AC19:        dcb $01,$FF

SubOffScreen:	    STZ $03                   
                    JSR IsSprOnScreen         ; \ if sprite is not off screen, return                                       
                    BEQ Return01ACA4          ; /                                                                           
                    LDA $5B                   ; \  vertical level                                    
                    AND #$01                  ; |                                                                           
                    BNE VerticalLevel         ; /                                                                           
                    LDA $D8,X                 ; \                                                                           
                    CLC                       ; |                                                                           
                    ADC #$50                  ; | if the sprite has gone off the bottom of the level...                     
                    LDA $14D4,X               ; | (if adding 0x50 to the sprite y position would make the high byte >= 2)   
                    ADC #$00                  ; |                                                                           
                    CMP #$02                  ; |                                                                           
                    BPL OffScrEraseSprite     ; /    ...erase the sprite                                                    
                    LDA $167A,X               ; \ if "process offscreen" flag is set, return                                
                    AND #$04                  ; |                                                                           
                    BNE Return01ACA4          ; /                                                                           
                    LDA $13                   
                    AND #$01                
                    STA $01                   
                    TAY                       
                    LDA $1A                   
                    CLC                       
                    ADC DATA_01AC11,Y       
                    ROL $00                   
                    CMP $E4,X                 
                    PHP                       
                    LDA $1B                   
                    LSR $00                   
                    ADC DATA_01AC19,Y       
                    PLP                       
                    SBC $14E0,X             
                    STA $00                   
                    LSR $01                   
                    BCC ADDR_01AC7C           
                    EOR #$80                
                    STA $00                   
ADDR_01AC7C:        LDA $00                   
                    BPL Return01ACA4          
OffScrEraseSprite:  LDA $14C8,X               ; \ If sprite status < 8, permanently erase sprite 
                    CMP #$08                  ;  | 
                    BCC OffScrKillSprite      ; / 
                    LDY $161A,X             
                    CPY #$FF                
                    BEQ OffScrKillSprite      
                    LDA #$00                
                    STA $1938,Y             
OffScrKillSprite:   STZ $14C8,X               ; Erase sprite 
Return01ACA4:       RTS                       

VerticalLevel:      LDA $167A,X               ; \ If "process offscreen" flag is set, return                
                    AND #$04                  ; |                                                           
                    BNE Return01ACA4          ; /                                                           
                    LDA $13                   ; \                                                           
                    LSR                       ; |                                                           
                    BCS Return01ACA4          ; /                                                           
                    LDA $E4,X                 ; \                                                           
                    CMP #$00                  ;  | If the sprite has gone off the side of the level...      
                    LDA $14E0,X               ;  |                                                          
                    SBC #$00                  ;  |                                                          
                    CMP #$02                  ;  |                                                          
                    BCS OffScrEraseSprite     ; /  ...erase the sprite      
                    LDA $13                   
                    LSR                       
                    AND #$01                
                    STA $01                   
                    TAY                       
		    LDA $1C                   
                    CLC                       
                    ADC DATA_01AC0D,Y       
                    ROL $00                   
                    CMP $D8,X                 
                    PHP                       
                    LDA $001D               
                    LSR $00                   
                    ADC DATA_01AC0F,Y       
                    PLP                       
                    SBC $14D4,X             
                    STA $00                   
                    LDY $01                   
                    BEQ ADDR_01ACF3           
                    EOR #$80                
                    STA $00                   
ADDR_01ACF3:        LDA $00                   
                    BPL Return01ACA4          
                    BMI OffScrEraseSprite  

IsSprOnScreen:      LDA $15A0,X               ; \ A = Current sprite is offscreen 
                    ORA $186C,X               ; /  
                    RTS                       ; Return 
