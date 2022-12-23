;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boss Bass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

				; symbolic names for RAM addresses
	RAM_SpriteDir   = $157C
	RAM_NoJumpTimer = $1504
	RAM_StateTimer  = $1540
	RAM_SpriteState = $C2

	ExecutePtr      = $0086DF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
; INIT and MAIN routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	DCB "INIT"
	LDA $D8,x
	ORA #$08
	STA $D8,x
	
	STZ RAM_SpriteState,x
	JSR SubHorzPos
	TYA
	STA RAM_SpriteDir,x
	LDA TimeInState
	STA RAM_StateTimer,x
        RTL

        DCB "MAIN"
        PHB
        PHK
        PLB
	JSR DecrementTimers
        JSR BossBassMain
        PLB
        RTL
	
DecrementTimers:
	LDA $14C8,x
	CMP #$08
	BNE DoneDec
	LDA $9D
	BNE DoneDec
	LDA RAM_NoJumpTimer,x
	BEQ DoneDec
	DEC RAM_NoJumpTimer,x
DoneDec:
	RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BossBassMain:	
	JSR SubGfx		; Draw sprite

	LDA $1528,x		; If the eaten flag is set:
	BEQ NotEaten
	LDA #$03		; Make Mario invisible
	STA $78
	STA $9D			; Lock sprites
NotEaten:
	
        LDA $14C8,X		; \ Return if status != Normal
        CMP #$08              	;  |
        BNE Return              ; /
        LDA $9D                 ; \ Return if sprites are locked
        BNE Return		; /

	;; 	JSR SubOffScreen        ; Only process while on screen

        JSR SetUpMovement
	JSL $018022             ; Update x position, no gravity
        JSL $01801A             ; Update y position, no gravity

        JSL $01A7DC             ; Handle Mario/sprite contact
	BCC Return		; Return if no contact

	JSR HandleContact	
Return:
        RTS                       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Handle movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TimeInState:
	dcb $11,$28
	
SwimSpeedX:
	DCB $2C,$D4
JumpProximity:
	DCB $20,$E0

SetUpMovement:
	LDA RAM_SpriteState,x
	BNE Jumping		
Swimming:
	LDA RAM_SpriteDir,x
	LDY RAM_StateTimer,x
 	BNE SkipFlipDireciton
	EOR #$01
	STA RAM_SpriteDir,x
	LDA #$04
	STA RAM_NoJumpTimer,x
	LDA #$1C
	STA RAM_StateTimer,x	
SkipFlipDireciton:
	LDY RAM_SpriteDir,x
	LDA SwimSpeedX,y
	STA $B6,x
	
	JSR SubHorzPos		;0 if traveling towards mario
	TYA
	EOR RAM_SpriteDir,x
	BNE SkipSetTimerMax

	LDA RAM_NoJumpTimer,x	; Don't jump if timer is set
	BNE NoIncState
	
	JSR SubVertPos
	CPY #$01		; Doesn't jump if above Mario
	BNE NoIncState
	LDA $0F
	CMP #$B0
	BCC NoIncState
	
	JSR SubHorzPos
	CPY #$00
	BEQ Less20
GreatE0:
	LDA $0F
	CMP #$F0
	BCS NoIncState
	CMP #$D0
	BCC NoIncState
	BRA IncState
Less20:	
	LDA $0F	
	CMP #$10
	BCC NoIncState
	CMP #$30
	BCS NoIncState

IncState:
	INC RAM_SpriteState,x
	JSR SetStateTimer
	LDA #$D9
	STA $AA,x
	RTS

NoIncState:
	LDA #$20
	STA RAM_StateTimer,x
	
SkipSetTimerMax:	
	STZ $AA,x
	RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
JumpAccelX:	
	dcb $01,$FF

Jumping:
	LDA RAM_StateTimer,x	; Go back to swimming state if time
	BEQ GotoSwim
	
	LDA $AA,x		; Adjust Y speed
	CLC
	ADC #$02
	STA $AA,x

	LDA RAM_SpriteDir,x	; Get index for X acceleration
	LDY $00AA,x
	BPL NoAdjustIndex
	EOR #$01
NoAdjustIndex:	
	TAY

	LDA $B6,x          	; Adjust X speed
	CLC
	ADC JumpAccelX,Y
	STA $B6,x
Return2:	
	RTS

GotoSwim:
	STZ RAM_SpriteState,x
	JSR SetStateTimer
	STZ $AA,x
	LDA #$10
	STA RAM_NoJumpTimer,x
	RTS

HandleContact:
	LDA RAM_SpriteState,x	; Only hurt Mario when jumping
	BEQ Return2
	LDA.B #$09		; Set death animation
	STA $71
	STA $1DFB		; Set death music
	LDA.B #$30		; Set time until we return to the OW
	STA $1496
	STA $1528,x		; Set eaten flag	
Return2:	
	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetStateTimer:
	LDY RAM_SpriteState,x
	LDA TimeInState,Y
	STA RAM_StateTimer,x
	RTS
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Graphics Routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SpriteTiles:        dcb $46,$47,$2B,$49,$59,$FF,$FF,$FF
                    dcb $46,$47,$2B,$4A,$5A,$FF,$FF,$FF
	            dcb $46,$47,$2B,$4D,$7F,$FF,$FF,$FF
	            dcb $46,$47,$04,$4D,$7F
SpriteTileSize:     dcb $02,$02,$02,$00,$00
	;; SpriteTileDispY:    dcb $E8,$E8,$F8,$F8,$00
SpriteTileDispY:    dcb $F0,$F0,$00,$00,$08
SpriteTileDispX:    dcb $00,$08,$00,$10,$10
     	            dcb $00,$F8,$00,$F8,$F8
SpriteGfxFlip:      dcb $00,$40	
	
SubGfx:
	JSR GetDrawInfo       	

	PHY
	LDA $15F6,X		; $02 = Palette info
	LDY RAM_SpriteDir,X
	BNE NoFlip
	ORA #$40
NoFlip:	
        STA $02		    

	LDA #$03		; $03 = Tilemap offset
        LDY $C2,x
        BNE StoreFrame
	LDA $14
	LSR
	AND #$02	
StoreFrame:
	ASL
	ASL
	ASL
	STA $03
	PLY

        PHX			; Push sprite index
        LDX #$04		; Draw 5 tiles
GfxLoopStart:
	PHX			; Push tile number

	PHX			; Push tile number
	LDA $02
	AND #$40
	BEQ NoAdjust
	TXA
	CLC
	ADC #$05
	TAX
NoAdjust:	
        LDA $00
        CLC
	ADC SpriteTileDispX,X
        STA $0300,Y
	PLX			; Pull tile number
	
	LDA $01
        CLC
        ADC SpriteTileDispY,X
        STA $0301,Y

	PHX			; Push tile number
	TXA
	CLC
	ADC $03
	TAX
        LDA SpriteTiles,X
        STA $0302,Y
	PLX			; Pull tile number
	
        LDA $02
        ORA $64
        STA $0303,Y
	
	LDA SpriteTileSize,X
	PHA
	TYA
	LSR
	LSR
	TAX
	PLA
        STA $0460,X
	PLX			; Pull tile number

        INY                     ; Adjust index into the OAM
        INY                     
        INY                     
        INY                     
        DEX
        BPL GfxLoopStart

	PLX			; Pull sprite index

        LDY #$FF		; We already wrote to $0460,Y
        LDA #$04                ; We wrote 5 tiles
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
; This routine determines which side of the sprite Mario is on.  It sets the Y register
; to the direction such that the sprite would face Mario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubHorzPos:
	LDY #$00
	LDA $94
	SEC
	SBC $E4,x
	STA $0F
	LDA $95
	SBC $14E0,x
	BPL HorzIncY
	INY 
HorzIncY:
	RTS 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This routine determines if Mario is above or below the sprite.  It sets the Y register
; to the direction such that the sprite would face Mario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubVertPos:
	LDY #$00
        LDA $96
        SEC
        SBC $D8,x
        STA $0F
        LDA $97
        SBC $14D4,x
        BPL VertIncY
        INY
VertIncY:
        RTS                  
        	
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
