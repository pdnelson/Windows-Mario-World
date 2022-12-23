;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Classic Firebar    by 33953YoShI
;
; IMPORTANT NOTE:
; - This sprite is require No More Sprite Tile Limit for use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Uses first extra bit: YES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extra Bit			Rotation direction
; Extra Property Byte 1		bit0-6	: Number of fire -2
;				bit7	: Default angle, highbyte
; Extra Property Byte 2		Default angle, lowbyte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		!ClockSpeed =	$02
		!CounterClock =	$FE

		!DispX =	$01
		!DispY =	$01
		!Width =	$06
		!Height =	$06

		!ExtraBits =	$7FAB10
		!ExtraProp1 =	$7FAB28
		!ExtraProp2 =	$7FAB34
		!NewSpriteNum =	$7FAB9E
		
		!GetSin =	$87F7DB
		!GetCos =	$87F7DB
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		print "INIT ",pc
		LDA $D8,x
		AND #$F0
		ORA #$04
		STA $D8,x
		LDA $E4,x
		AND #$F0
		ORA #$04
		STA $E4,x
		
		LDA !ExtraProp1,x
		ASL A
		ROL $151C,x
		LSR A
		STA $187B,x
		LDA !ExtraProp2,x
		STA $1602,x
		LDY #!ClockSpeed
		LDA !ExtraBits,x
		AND #$04
		BEQ +
		LDY #!CounterClock
+		TYA
		STA $1528,x
		LDA #$00
		STA !ExtraProp1,x
		STA !ExtraProp2,x
		RTL
		
		print "MAIN ",pc
		PHB
		PHK
		PLB
		JSR MainCode
		PLB
		RTL
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MainCode:	JSR GetDrawInfo
		LDA $9D
		BNE .Lock
		LDY #$08
		JSR SubOffScreen
		LDY #$00
		LDA $1528,x
		BPL +
		DEY
+		CLC
		ADC $1602,x
		STA $1602,x
		TYA
		ADC $151C,x
		AND #$01
		STA $151C,x
		
.Lock		LDA $14E0,x
		STA $01
		PHA
		LDA $E4,x
		STA $00
		PHA
		LDA $14D4,x
		STA $03
		PHA
		LDA $D8,x
		STA $02
		PHA
		LDY $187B,x
		LDA $151C,x
		XBA
		LDA $1602,x
		REP #$30
		PHY
		STA $0E
		
		TAY
		AND.w #$00FF
		ASL A
		TAX
		LDA !GetSin,x
		LDX.w #$0000
		ASL A
		BEQ +
		ASL #2
		CPY.w #$0100
		BCC +
		EOR.w #$FFFF
		INC A
		DEX
		CLC
+		STA $04
		STA $0C
		STX $06
		LDA $0E
		ADC.w #$0080
		AND.w #$01FF
		TAY
		AND.w #$00FF
		ASL A
		TAX
		LDA !GetCos,x
		LDX.w #$0000
		ASL A
		BEQ +
		ASL #2
		CPY.w #$0100
		BCC +
		EOR.w #$FFFF
		INC A
		DEX
+		STA $08
		STA $0E
		STX $0A
		PLY
		BRA .Init
		
.PositionLoop	LDA $04
		CLC
		ADC $0C
		STA $04
		LDA $08
		CLC
		ADC $0E
		STA $08
		
.Init		LDA $00
		CLC
		ADC $09
		PHA
		LDA $02
		CLC
		ADC $05
		PHA
		
		DEY
		BPL .PositionLoop
		SEP #$30
		
		LDX $15E9
		LDA $187B,x
		INC A
		STA $0D
		LDY $15C4,x
		BNE +
		STA $0F
		JSR SubGFX
		
+		LDA $71
		BNE .PullAll
		LDA $13F9
		EOR $1632,x
		BEQ .GetCollision
.PullAll	REP #$20
		LDA $0D
		INC A
		AND.w #$00FF
		ASL #2
		STA $0D
		TSC
		ADC $0D
		TCS
		SEP #$20
		RTS
		
.GetCollision	LDA #!Width
		STA $06
		LDA #!Height
		STA $07
		JSL $83B664
		
.CollisionLoop	LDA $0D
		EOR $14
		LSR A
		BCC .PullNext
		LDA $94
		SBC $03,s
		CLC
		ADC #$20
		CMP #$40
		BCS .PullNext
		LDA $96
		ADC #$18
		SEC
		SBC $01,s
		CMP #$60
		BCS .PullNext
		
		PLA
		PLY
		ADC #!DispY
		STA $05
		BCC +
		INY
		CLC
+		STY $0B
		PLA
		PLY
		ADC #!DispX
		STA $04
		BCC +
		INY
+		STY $0A
		JSL $83B72B
		BCC .Next
		LDA $1497
		ORA $1490
		BNE .Next
		LDA $187A
		BEQ +
		JSR LoseYoshi
		BRA .Next
+		JSL $80F5B7
		BRA .Next
		
.PullNext	PLA
		PLA
		PLA
		PLA
.Next		DEC $0D
		BPL .CollisionLoop
		RTS
		
LoseYoshi:	LDX $18E2
		DEX
		BMI +
		LDA #$10
		STA $163E,x
		LDA #$03
		STA $1DFA
		LDA #$13
		STA $1DFC
		LDA #$02
		STA $C2,x
		STZ $187A
		STZ $0DC1
		LDA #$C0
		STA $7D
		STZ $7B
		LDY $157C,x
		LDA .SetXspeed,y
		STA $B6,x
		STZ $1594,x
		STZ $151C,x
		STZ $18AE
		LDA #$30
		STA $1497
+		LDX $15E9
		RTS

.SetXspeed	db $20,$E0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubGFX:		TXA
		EOR $14
		LSR #2
		AND #$03
		TAY
		LDA .TileMap,y
		STA $02
		LDA $15F6,x
		EOR .TileProp,y
		ORA $64
		STA $03
		STZ $2183
		LDA #$03
		STA $2182
		LDA $15EA,x
		STA $2181
		LSR #2
		TAY
		TSX
		STZ $01
		
.Loop		REP #$21
		LDA $0103,x
		ADC.w #$0007
		SEC
		SBC $1C
		CMP.w #$00E7
		BCS .Next
		STA $00
		LDA $0105,x
		ADC.w #$0007
		SEC
		SBC $1A
		CMP.w #$0107
		BCS .Next
		SBC.w #$0006
		SEP #$21
		STA $2180
		LDA $00
		SBC #$07
		STA $2180
		LDA $02
		STA $2180
		LDA $03
		STA $2180
		XBA
		AND #$01
		STA $0460,y
		INY
.Next		SEP #$21
		TXA
		ADC #$03
		TAX
		DEC $0F
		BPL .Loop
		LDX $15E9
		RTS
		
.TileMap	db $2C,$2D,$2C,$2D
.TileProp	db $00,$00,$C0,$C0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GetDrawInfo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DON'T REPLACE TO SHARED ROUTINE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetDrawInfo:	LDA $E4,x
		CMP $1A
		LDA $14E0,x
		SBC $1B
		BEQ .OnScreenX
		LDA #$01
.OnScreenX	STA $15A0,x

		LDA $14E0,x
		PHA
		LDA $E4,x
		PHA
		LDY $187B,x
		INY #2
		REP #$20
		TYA
		ASL #3
		DEC A
		STA $00
		ASL A
		ADC.w #$0108
		STA $02
		PLA
		ADC $00
		CLC
		ADC.w #$0008
		SEC
		SBC $1A
		CMP $02
		SEP #$20
		TDC
		ROL A
		STA $15C4,x
		BNE .Invalid
		
		LDA $14D4,x
		XBA
		LDA $D8,x
		REP #$21
		ADC.w #$000C
		SEC
		SBC $1C
		SEP #$20
		XBA
		BEQ .OnScreenY
		LDA #$01
.OnScreenY	STA $186C,x
.Invalid	RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SubOffScreen:	LDA $15A0,x
		ORA $186C,x
		BEQ .Return2
		LDA $167A,x
		AND #$04
		BNE .Return2
		LDA $5B
		LSR A
		BCS .VerticalLevel
		LDA $D8,x
		ADC #$50
		LDA $14D4,x
		ADC #$00
		CMP #$02
		BCS .EraseSprite
		LDA $14E0,x
		XBA
		LDA $E4,x
		REP #$21
		ADC .AddTable,y
		SEC
		SBC $1A
		CMP .CmpTable,y
.Common		SEP #$20
		BCC .Return2
.EraseSprite	LDA $14C8,x
		CMP #$08
		BCC .KillSprite
		LDY $161A,x
		CPY #$FF
		BEQ .KillSprite
		LDA $1938,y
		AND #$FE
		STA $1938,y
.KillSprite	STZ $14C8,x
.Return2	RTS

.VerticalLevel	LDA $13
		LSR A
		BCS .CheckY
		LDA $14E0,x
		XBA
		LDA $E4,x
		REP #$21
		ADC.w #$0040
		CMP.w #$0280
		BRA .Common
.CheckY		LDA $14D4,x
		XBA
		LDA $D8,x
		REP #$20
		SBC $1C
		CLC
		ADC.w #$0070
		CMP.w #$01D0
		BRA .Common

.AddTable	dw $0040,$0040,$0010,$0070
		dw $0090,$0050,$0080,$FFC0
.CmpTable	dw $0170,$01E0,$01B0,$01D0
		dw $0230,$01B0,$0220,$0160
