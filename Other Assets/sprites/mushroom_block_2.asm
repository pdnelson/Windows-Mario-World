;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mushroom Block (Sprite Portion)
;;
;; Description: When used in conjunction with the Block Tool block, this kind of acts
;; like the Mushroom Block from SMB2
;;
;; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	UpdateSpritePos = $01802A
	InvisBlkMainRt  = $01B44F
	SprGfxRt1x1	= $0190B2

	GetMarioClipping = $03B664
	GetSpriteClippingA = $03B69F
	CheckForContact	 = $03B72B
	
	;; RAM addresses
	RAM_SprOAMIndex = $15EA
	OAM_Tile	= $0302
	OAM_Prop	= $0303
	RAM_ExtraProp2  = $7FAB34

	;; Variables
	Map16Num = $0535

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	dcb "INIT"
	RTL

	dcb "MAIN"
        PHB              
        PHK              
        PLB

	PHA
	LDA $1686,x		; set to interact with sprites (may get overridden later)
	AND #$F7
	STA $1686,x
	PLA
	
	CMP #$09
	BNE NotStunned
	JSR Stunned
	BRA ReturnLong
NotStunned:

	CMP #$0B
	BNE NotCarried
	JSR Carried
	BRA ReturnLong
NotCarried:
	
	CMP #$08
	BNE ReturnLong
	JSR TurnToBlock		;SpriteMainSub2
	
ReturnLong:
	LDY RAM_SprOAMIndex,X
	LDA #$AB
	STA OAM_Tile,Y

	LDA OAM_Prop,y
	ORA #$40
	STA OAM_Prop,y
	
        PLB
        RTL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Stunned:
	LDA $AA,x
	ORA $B6,x
	BNE StunnedReturn
	LDA #$08
	STA $14c8,x
StunnedReturn:	
	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Carried:
	LDA RAM_ExtraProp2,x
	AND #$01
	BEQ CarriedReturn
	LDA $1686,x	; don't interact with sprites when carried
	ORA #$08
	STA $1686,x
CarriedReturn:	
	RTS	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetTimer:
	LDA #$05
	STA $1540,x
Return:
	RTS
TryCarry:
	LDA $1588,x
	AND #$04
	BNE Return
	BIT $15
        BVC Return
        LDA $1470               ; \ Branch if carrying an enemy...
        ORA $187A               ;  | ...or if on Yoshi
        BNE Return		; /
	STZ $1540,x
        LDA #$0B                ; \ Sprite status = carried
        STA $14C8,X             ; /
	JSR Carried
	RTS	
	
TurnToBlock:
	JSL SprGfxRt1x1
	LDA $9D			; Return if sprites locked
	BNE Return

	JSL UpdateSpritePos

	JSL GetMarioClipping    ; Return if Mario is in contact
	JSL GetSpriteClippingA  ; (otherwise will turn into a block and kill him)
	JSL CheckForContact
	BCS TryCarry
	
	LDA $B6,x		; Return if has X speed
	BNE Return

	LDA $1588,x		; Return if not on ground
	AND #$04
	BEQ SetTimer
	
	LDA $1540,x
	BNE Return
MakeBlock:	
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
                    	