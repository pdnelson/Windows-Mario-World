;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mushroom Block (Sprite Portion)
;;
;; Description: When used in conjunction with the Block Tool block, this kind of acts
;; like the Mushroom Block from SMB2
;;
;; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SprGfxRt2x2 = $018042
	SprGfxRt1x1	= $0190B2
	UpdateSpritePos = $01802A
	GetMarioClipping = $03B664 
	GetSpriteClippingA = $03B69F
	GetSpriteClippingB	= $03B6E5	
	CheckForContact = $03B72B
	SprContactGfxSfx = $01AB6F
	GivePoints = $02ACE5

	;; RAM addresses
	RAM_SprOAMIndex = $15EA
	OAM_Tile	= $0302
	OAM_DispX	= $0300
	OAM_DispY	= $0301
	OAM_Prop	= $0303
	RAM_SpriteStatus = $14C8
	BlockCount = $7F8821

	;; Variables
	Map16Num = $0535
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	dcb "INIT"
	RTL

	dcb "MAIN"
        PHB              
        PHK              
        PLB              
	CMP #$08
	BNE SkipMain
	JSR SpriteMainSub
	BRA Gfx
SkipMain:
	JSR ThrownSub
Gfx:	
	JSR SubGfx
	PLB
        RTL
	

ThrownSub:
	LDY.B #$0B		
LoopStart:
	LDA RAM_SpriteStatus,Y	; Skip dead sprites
	CMP #$08
	BCC NextSprite
	LDA $1686,Y		; Skip sprites that don't interact with others
	AND #$08
	BNE NextSprite
	JSR SprSprInteract
NextSprite:	
        DEY
	BPL LoopStart
	RTS

SprSprInteract:
	JSL GetSpriteClippingA
	PHX
	TYX
	JSL GetSpriteClippingB
	PLX
	JSL CheckForContact
	BCC Return1
	LDA #$08
	STA $14C8,x
	PHX
	TYX
        LDA #$02                ; Sprite status = disappear in cloud of smoke
        STA $14C8,x
	LDA #$D0
	STA $AA,x
	JSL SprContactGfxSfx   
	LDA #$04                
	JSL GivePoints  
	PLX
Return1:		
	RTS	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATA_01E611:
	dcb $00,$01,$02,$02,$02,$01,$01,$00
        dcb $00
DATA_01E61A:
        dcb $1E,$1B,$18,$18,$18,$1A,$1C,$1D
        dcb $1E
	
SpriteMainSub:
	JSL SprGfxRt1x1
	LDY RAM_SprOAMIndex
	LDA #$AB
	STA OAM_Tile,Y
	LDA $9D			; \ If sprites locked, 
        BEQ NotLocked		;  |
        JMP ADDR_01E6F0		; / jump to end of routine to draw sprite
NotLocked:
	LDA BlockCount
	CMP #$02
	BCS CrazyShit
	LDA $1528,x
	BEQ NoCrazyShit
CrazyShit:	
	LDA #$00
	STA $1528,x
	STA BlockCount
NoCrazyShit:		
	JSR SubOffscreen	; Erase when offscreen
        JSL UpdateSpritePos	; Move sprite
	LDA $1588,x
	AND #$04
        BEQ NotOnGround
        JSR ADDR_0197D5		; Runs if on ground
	BRA CheckSide
NotOnGround:
	LDA #$08
	STA $163E,x
	JSR ThrownSub
CheckSide:	
	LDA $1588,X
        AND #$03
        BEQ NotTouchingSide
	LDA #$10
	STA $163E,x
        JSR SetSpriteTurning	; Runs if touching object side
        LDA $B6,X
        ASL
        PHP
        ROR $B6,X
        PLP
        ROR $B6,X
NotTouchingSide:	
	LDA $1588,X          
        AND #$08
        BEQ NotTouchingCeiling
        STZ $AA,X	      	; If touching celing, sprite Y Speed = 0
NotTouchingCeiling:	

	LDA $AA,x
	ORA $B6,x
	BNE NoChange

	JSL GetMarioClipping    ; Return if Mario is in contact
	JSL GetSpriteClippingA  ; (otherwise will turn into a block and kill him)
	JSL CheckForContact
	BCS NoChange

	LDA $163E,x
	BNE NoChange
	
	JSR TurnToBlock
	RTS
SetBounce:
	LDA #$D0
	STA $AA,x
NoChange:	
	LDA $1540,X		; Load time to boost Mario
        BEQ ADDR_01E6B0	

        STZ $72
        LDA #$02
        STA $1471
        LDA $1540,X
        CMP #$07
        STZ $1471
        LDY #$B0
        LDA $17
        BPL ADDR_01E69A
        LDA #$01
        STA $140D
        BRA ADDR_01E69E           

ADDR_01E69A:
        LDA $15
        BPL ADDR_01E6A7           
ADDR_01E69E:
        LDA #$0B
        STA $72
        LDY #$80
        STY $1406               
ADDR_01E6A7:
ADDR_01E6AE:
        BRA ADDR_01E6F0           

ADDR_01E6B0:
	JSL $01A7DC             ; Handle Mario/sprite contact
        BCC ADDR_01E6F0
        STZ $154C,X
        LDA $D8,X
        SEC
        SBC $96
        CLC
        ADC #$04
        CMP #$1C
        BCC ADDR_01E6CE
        BRA ADDR_01E6F0           
ADDR_01E6CE:
ADDR_01E6E2:
	JSR ADDR_01AB31
ADDR_01E6E7:
ADDR_01E6F0:
Return01E6F:
	RTS			; Return 

DATA_01E6FD:
	dcb $00,$02,$00
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TurnToBlock:
	STZ $14C8,x             ; destroy the sprite
        
        LDA $E4,x               ; \ setup block position
	CLC
	ADC #$08
	AND #$F0
        STA $9A                 ;  |
        LDA $14E0,x             ;  |
	ADC #$00
        STA $9B                 ;  |
	
        LDA $D8,x               ;  |
	CLC
	ADC #$08
	AND #$F0
        STA $98                 ;  |
        LDA $14D4,x             ;  |
	ADC #$00
        STA $99                 ; /
        
        PHP
        REP #$30                ; \ change sprite to block 
        LDA.W #Map16Num  	;  |
        STA $03                 ;  |
        JSR SetMap16Block       ; /
        PLP

	RTS
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map16 subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetMap16Block:
	PHP                     
        REP #$30                
        PHY                     
        PHX                     
        TAX                     
        LDA $03                 
        PHA                     
        JSR SUB_8034            
        PLA                     
        STA $03                 
        PLX                     
        PLY                     
        PLP                     
        RTS                     
	
        JMP $FEA301
RETURN18:
	PLX
        PLB
        PLP
        RTS

SUB_8034:
	PHP                     
        SEP #$20                
        PHB                     
        LDA #$00                
        PHA                     
        PLB                     
        REP #$30                
        PHX                     
        LDA $9A                 
        STA $0C                 
        LDA $98                 
        STA $0E                 
        LDA.W #$0000            
        SEP #$20                
        LDA $5B                 
        STA $09                 
        LDA $1933               
        BEQ NO_SHIFT            
        LSR $09
NO_SHIFT:
	LDY $0E                 
        LDA $09                 
        AND #$01                
        BEQ HORIZ               
        LDA $9B
        STA $00
        LDA $99
        STA $9B
        LDA $00  
        STA $99
        LDY $0C
HORIZ:
	CPY.W #$0200            
        BCS RETURN18            
        LDA $1933               
        ASL A                   
        TAX                     
        LDA $BEA8,x ;[$00:BEA8] 
        STA $65                 
        LDA $BEA9,x ;[$00:BEA9] 
        STA $66                 
        STZ $67                 
        LDA $1925               
        ASL A                   
        TAY                     
        LDA ($65),y ;[$00:BDA8] 
        STA $04                 
        INY                     
        LDA ($65),y ;[$00:BDA9] 
        STA $05                 
        STZ $06                 
        LDA $9B                 
        STA $07                 
        ASL A                   
        CLC                     
        ADC $07                 
        TAY                     
        LDA ($04),y ;[$00:BAD8] 
        STA $6B                 
        STA $6E                 
        INY                     
        LDA ($04),y ;[$00:BAD9] 
        STA $6C                 
        STA $6F                 
        LDA #$7E                
        STA $6D                 
        INC A                   
        STA $70                 
        LDA $09                 
        AND #$01                
        BEQ NO_AND              
        LDA $99
        LSR A
        LDA $9B 
        AND #$01
        BRA LABEL52
NO_AND:
	LDA $9B                 
        LSR A                   
        LDA $99                 
LABEL52:
	ROL A                   
        ASL A                   
        ASL A                   
        ORA #$20                
        STA $04                 
        CPX.W #$0000            
        BEQ NO_ADD              
        CLC
        ADC #$10 
        STA $04
NO_ADD:
	LDA $98                 
        AND #$F0                
        CLC                     
        ASL A                   
        ROL A                   
        STA $05                 
        ROL A                   
        AND #$03                
        ORA $04                 
        STA $06                 
        LDA $9A                 
        AND #$F0                
        LSR A                   
        LSR A                   
        LSR A                   
        STA $04                 
        LDA $05                 
        AND #$C0                
        ORA $04                 
        STA $07                 
        REP #$20                
        LDA $09                 
        AND.W #$0001            
        BNE LABEL51             
        LDA $1A                 
        SEC                     
        SBC.W #$0080            
        TAX                     
        LDY $1C                 
        LDA $1933               
        BEQ LABEL50             
        LDX $1E
        LDA $20
        SEC
        SBC.W #$0080
        TAY
        BRA LABEL50
LABEL51:
	LDX $1A
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
LABEL50:
	STX $08 	                
        STY 	$0A                 
        LDA $98     	            
        AND.W #$01F0        	    
        STA $04                 	
        LDA $9A                 	
        LSR A                   	
        LSR A                   	
        LSR A                   
        LSR A                   
        AND.W #$000F            
        ORA $04                 
        TAY                     
        PLA                     
        SEP #$20                
        STA [$6B],y ;[$7E:C937] 
        XBA                     
        STA [$6E],y ;[$7F:C937] 
        XBA                     
        REP #$20                
        ASL A                   
        TAY                     
        PHK                     
        PER.W #$0006            ; NOTE: this relative counter must always point to MAP16_RETURN. 
        PEA $804C
        JMP.L $00C0FB
MAP16_RETURN:
	PLB
        PLP
        RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATA_01AB2D:
        dcb $01,$00,$FF,$FF

ADDR_01AB31:
	LDA BlockCount
	BNE NoPush
	STZ $7B
        JSR SubHorizPos
        TYA
        ASL
        TAY
        REP #$20                  ; Accum (16 bit)
        LDA $94
        CLC
        ADC DATA_01AB2D,Y
        STA $94
        SEP #$20                  ; Accum (8 bit)
	LDA #$01
	STA BlockCount
	STA $1528,x
NoPush:	
	RTS                       ; Return 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetSpriteTurning:
	LDA $15AC,X             ; \ Return if turning timer is set
        BNE Return0190B1        ; /
        LDA #$08                ; \ Set turning timer
        STA $15AC,X             ; / 
FlipSpriteDir:
	LDA $B6,X		; \ Invert speed
        EOR #$FF                ;  |
        INC A			;  |
        STA $B6,X		; /
        LDA $157C,X             ; \ Flip sprite direction
        EOR #$01                ;  |
        STA $157C,X             ; / 
Return0190B1:
	RTS                       ; Return 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubHorizPos:
        LDY #$00
        LDA $D1
        SEC
        SBC $E4,X
        STA $0F
        LDA $D2
        SBC $14E0,X
        BPL Return01AD41
        INY                       
Return01AD41:
	RTS                       ; Return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATA_0197AF:
        dcb $00,$00,$00,$F8,$F8,$F8,$F8,$F8
        dcb $F8,$F7,$F6,$F5,$F4,$F3,$F2,$E8
        dcb $E8,$E8,$E8,$00,$00,$00,$00,$FE
        dcb $FC,$F8,$EC,$EC,$EC,$E8,$E4,$E0
        dcb $DC,$D8,$D4,$D0,$CC,$C8

ADDR_0197D5:
        LDA $B6,X
        PHP
        BPL ADDR_0197DD
        EOR #$FF		  ; \ Set A to -A
        INC A                     ; /  
ADDR_0197DD:
        LSR
        PLP
        BPL ADDR_0197E4
        EOR #$FF		  ; \ Set A to -A
        INC A                     ; /  
ADDR_0197E4:
        STA $B6,X
        LDA $AA,X
        PHA
        JSR SetSomeYSpeed
        PLA
        LSR
        LSR
        TAY
        LDA $9E,X                 ; \ If Goomba, Y += #$13
        CMP #$0F		  ;  |
        BNE ADDR_0197FB           ;  |
        TYA                       ;  |
        CLC                       ;  |
        ADC #$13		  ;  |
        TAY                       ; / 
ADDR_0197FB:
        LDA DATA_0197AF,Y
        LDY $1588,X
        BMI Return019805
        STA $AA,X                 
Return019805:
	RTS       

SetSomeYSpeed:
	LDA $1588,X
        BMI ADDR_019A10
        LDA #$00                ; \ Sprite Y speed = #$00 or #$18
        LDY $15B8,X             ;  | Depending on 15B8,x ???
        BEQ ADDR_019A12		;  | 
ADDR_019A10:
        LDA #$18                ;  | 
ADDR_019A12:
        STA $AA,X		; / 
Return019A14:
	RTS			; Return 
	
	
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Graphics Routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubGfx:
	JSR GetDrawInfo       	

	LDA $15F6,X		; $02 = Palette info
	ORA $64
	ORA #$40
	STA OAM_Prop,Y
	STA OAM_Prop+4,Y
	STA OAM_Prop+8,Y
	STA OAM_Prop+12,Y
	
        LDA $00
	STA OAM_DispX,Y
	STA OAM_DispX+4,Y
	STA OAM_DispX+8,Y
	STA OAM_DispX+12,Y

	LDA $01
	STA OAM_DispY,Y
	STA OAM_DispY+4,Y
	STA OAM_DispY+8,Y
	STA OAM_DispY+12,Y

	LDA #$AB
	STA OAM_Tile,Y
	STA OAM_Tile+4,Y	
	STA OAM_Tile+8,Y
	STA OAM_Tile+12,Y	

        LDY #$02		; We already wrote to $0460,Y
        LDA #$03                ; We wrote 5 tiles
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

	