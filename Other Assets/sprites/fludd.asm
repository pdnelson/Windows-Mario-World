;	######################################
;	         F.L.U.D.D. (kind of)
;	           by WhiteYoshiEgg
;	######################################


;######################################
;		F.L.U.D.D.F.A.Q.
;######################################


;	* Is this really the FLUDD from Super Mario Sunshine?
;		It's intended to be, but it's far from being a perfect replica.
;		I've never played SMS, so I don't really know how exactly it works.
;		The only thing I knew when I first made that sprite was that it could
;		shoot you upwards with water, and while I know now that it has other functions too,
;		I'm not planning on adding these now. I guess you just have to be satisfied
;		with what you have now. Its customizability should make up for that
;		a little, though.

;	* If this isn't an exact copy, how does it work?
;		Just place this sprite at the starting screen of the level.
;		To "fly" upwards, just press the UP button (or whatever button you want - see below).
;		While flying, the water counter in the status bar will slowly count down. When
;		you've run out of water, you can't use the FLUDD anymore. To refill your water, just jump
;		into a water pool and wait for it to be recharged. (As far as I know, you had to do that
;		manually in SMS, but it works automatically here.) Again, the speeds at which you
;		gain/lose water, and even the sound effect played during that, are easily customizable.

;	* How can I get it to look right?
;		Insert the ExGFX file (ExGFXFAD.bin) into the SP3 slot. Also, import the included palette
;		through "File --> Palettes --> Import Level palette from file". It uses palette E,
;		so it shouldn't conflict with too many other things.

;	* What's the other sprite for?
;		It's a mushroom that instantly refills your water. Don't know if something like that
;		existed in SMS too, but I thought it'd be nice to have.
;		Make sure that if you change the first two defines here, you change them to the same value
;		in the mushroom's ASM file too!

;	* Any bugs?
;		For starters, I don't recommend using this together with midway points, since the FLUDD
;		won't appear when you start the level from the midway entrance. Placing another FLUDD
;		at the midway entrance won't fix it either - as soon as you get to the midway point,
;		the water counter instantly will get max'd again, and it will probably count up/down
;		twice as fast, since there are now two FLUDDs. Same with secondary entrances, I'm afraid.
;		Apart from that, the FLUDD looks a bit weird when spin-jumping, dying and entering pipes,
;		but that's nothing game-breaking.

;	* Do I have to give you credit?
;		Nah. Sure, it would be nice, but if you don't want to, you don't need to.
;		HOWEVER, you HAVE TO give credit to Xander (from mfgg.net) for the FLUDD graphics,
;		as he apparently requires it.



;			Have fun!



;######################################
;		Defines
;######################################

		!Water		= $0670	;   Free RAM address, used for water counter
		!MaxWater	= 25	;   Max. amount of water (yes, it's decimal)
		!MoreRAM	= $0671 ;   Another free RAM address
		!EvenMoreRAM	= $0672 ;   Yet another free RAM address
		!Speed		= $E0	;   Speed at which you fly upwards (must be between 80 and FF)
		!DownFrequency	= $16	;   How fast the water counter decreases (lower value = faster)
		!UpFrequency	= $0B	;   How fast the water counter increases (lower value = faster)

		!SFX_WtrDecr	= $21	;   What sound to play when you lose water
		!SFX_WtrRunOut	= $2A	;   What sound to play when there's less than 5 water units left
		!SFX_WtrRefill	= $15	;   What sound to play when the water is refilled

		!CounterPos	= $0F19 ;   Where to display the water counter (status bar)

		!Button		= $08	;   What button to press to fly up
					;   (possible values: 01=Right, 02=Left, 04=Down, 08=Up, 10=Start,
					;   20=Select, 40=Y and X, 80=B and A)
					;   I don't recommend changing that though.


;######################################
;		INIT Routine
;######################################

print "INIT ",pc

		STZ !MoreRAM		; \  Reset RAM addresses
		STZ !EvenMoreRAM	; /  for the heck of it

		LDA #!MaxWater		; \  Fill the water counter
		STA !Water		; /  at the start of the level

	RelativeToMario:		;    This routine attaches the FLUDD to Mario's back
		LDA $D3			; \
		STA $D8,x		;  | Y Position
		LDA $D4			;  |
		STA $14D4,x		; /
		
	LDA $76				; \  Show sprite left or right of Mario
	BEQ Right			; /  depending on his direction
		LDA $D1
		SEC : SBC #$08
		STA $E4,x
		LDA $D2
		SBC #$00   
		STA $14E0,x
		BRA INITReturn
	Right:
		LDA $D1
		CLC : ADC #$08
		STA $E4,x
		LDA $D2
		ADC #$00   
		STA $14E0,x
	INITReturn:
		RTL			;    End subroutine (and with that, the INIT routine too)

		

;######################################
;		MAIN Routine
;######################################

print "MAIN ",pc
		
		PHB : PHK : PLB : JSR SpriteCode : PLB : RTL


	SpriteCode:
		
		JSL RelativeToMario	;    Same as in the INIT, but every frame this time
		JSR SubGFX		;    Graphics
		JSR StatusBarStuff	;    Status Bar stuff

		LDA $71			; \
		CMP #$09		;  | Don't do anything
		BEQ Return		;  | if Mario's dying
		LDA $9D			;  | or sprites are locked
		BNE Return		; /

	


	; Refill water counter if Mario's in water

		LDA $75			; \  If not in water,
		BEQ Continue		; /  skip this part
		
		LDA !Water		; \
		CMP #!MaxWater		;  | Don't increase water if it's already at max
		BEQ Continue		; /

		LDA !MoreRAM		; \
		CMP #!UpFrequency	;  | This code makes it so that
		BCS $05			;  | everything between the
		INC !MoreRAM		;  | STZ !MoreRAM and the ++ label
		BRA ++			;  | will only be executed every few frames
		STZ !MoreRAM		; /  (depending on !UpFrequency)

		INC !Water		; \
		LDA #!SFX_WtrRefill	;  | Increase water amount
		STA $1DFC		;  | and play sound effect
		++			; /
	
	Continue:


	

	; Fly when UP is pressed
 
		LDA !Water		; \  If there's no water left,
		BEQ Return		; /  skip this part

		LDA $15			; \  If UP is not pressed,
		AND #!Button		;  | skip this part too
		BEQ Return		; /
		
		LDA #!Speed		; \  Give Mario the
		STA $7D			; /  specified Y speed

		LDA !EvenMoreRAM	; \
		CMP #!DownFrequency	;  | This code makes it so that
		BCS +			;  | everything between the
		INC !EvenMoreRAM	;  | STZ !MoreRAM and the ++ label
		BRA Return		;  | will only be executed every few frames
		+			;  | (depending on !DownFrequency)
		STZ !EvenMoreRAM	; /

		DEC !Water		; \
		LDA !Water		;  |
		CMP #$06		;  | Decrease water amount
		BCC DifferentSound	;  | and play sound effect
		LDA #!SFX_WtrDecr	;  |
		BRA Store		;  | 
		DifferentSound:		;  | 
		LDA #!SFX_WtrRunOut	;  | (Play a different sound when there's 
		Store:			;  | 5 or fewer water units left)
		STA $1DFC		; /

	Return:
		RTS




	StatusBarStuff:
		LDA #$FC		; \  Clear the tiles in question
		STA !CounterPos		;  | before adding new ones
		STA !CounterPos+1	; /

		PHX			;    Preserve X
		LDA !Water		; \  Convert water amount
		JSL $00974C		; /  to decimal
		CPX #$00		; \
		BEQ OneDigit		;  |
		STA !CounterPos+1	;  | If there are less than 10
		STX !CounterPos		;  | water units left,
		BRA StatusBarReturn	;  | move the counter
	OneDigit:			;  | one tile to the right
		STA !CounterPos		;  | to save some space
	StatusBarReturn:		;  | 	
		PLX			; /
		RTS			;    Return from subroutine









	SubGFX:
		JSR GetDrawInfo		; after: Y = index to sprite OAM ($300)
					;      $00 = sprite x position relative to screen border 
					;      $01 = sprite y position relative to screen border  
                    
		LDA $157C,x		; $02 = sprite direction
		STA $02

		; if you wish to draw more than one tile, each step between the lines must be repeated
		;*************************************************************************************  

		LDA $00			; set x position of the tile
		STA $0300,y

		LDA $01			; set y position of the tile
		STA $0301,y

		LDA $71			; \
		CMP #$09		;  |
		BEQ OtherTile1		;  | 
		LDA $15			;  | 
		AND #!Button		;  | Display a different tile
		BEQ OtherTile1		;  | when you're pressing UP
		LDA #$2B		;  | and not dying
		BRA Store1		;  |
	OtherTile1:			;  |
		LDA #$04		;  |
	Store1:				;  |
		STA $0302,y		; /

		LDA $15F6,x		; get sprite palette info
		PHX
		LDX $76			; flip the tile if Mario's direction is 0
		BNE +
		ORA #$40
	+           
		PLX
		ORA $64			; add in the priority bits from the level settings
		STA $0303,y		; set properties

		INY #4			; get the index to the next slot of the OAM

                    
		;*************************************************************************************


		LDA $00			; set x position of the tile
		STA $0300,y

		LDA $01			; set y position of the tile
		CLC : ADC #$10
		STA $0301,y

		LDA !Water		; \
		BEQ OtherTile2		;  | 
		LDA #$00		;  | 
		BRA Store2		;  | Display a different tile
	OtherTile2:			;  | when there's no water left
		LDA #$02		;  |
	Store2:				;  |
		STA $0302,y		; /

		LDA $15F6,x		; get sprite palette info
		PHX
		LDX $76			; flip the tile if Mario's direction is 0
		BNE +
		ORA #$40
	+           
		PLX
		ORA $64			; add in the priority bits from the level settings
		STA $0303,y		; set properties

		INY #4			; get the index to the next slot of the OAM

                    
		;*************************************************************************************

		LDA $00			; set x position of the tile
		STA $0300,y

		LDA $01			; set y position of the tile
		CLC : ADC #$10
		STA $0301,y

		LDA !Water		; \
		BEQ OtherTile3		;  |
		LDA $15			;  |
		AND #!Button		;  | 
		BEQ OtherTile3		;  | 
		LDA $71			;  | Only display this tile
		CMP #$09		;  | if you're pressing UP, not dying
		BEQ OtherTile3		;  | and there's some water left
		LDA #$46		;  | 
		BRA Store3		;  |
	OtherTile3:			;  | (this is the water stream, top tile)
		LDA #$6D		;  |
	Store3:				;  |
		STA $0302,y		; /

		LDA $15F6,x		; get sprite palette info
		PHX
		LDX $76			; flip the tile if Mario's direction is 0
		BNE +
		ORA #$40
	+           
		PLX
		ORA #$20		; give the sprite second-highest priority
		ORA $64			; add in the priority bits from the level settings
		STA $0303,y		; set properties

		INY #4			; get the index to the next slot of the OAM

                    
		;*************************************************************************************



		LDA $00			; set x position of the tile
		STA $0300,y

		LDA $01			; set y position of the tile
		CLC : ADC #$20
		STA $0301,y

		LDA !Water		; \
		BEQ OtherTile4		;  |
		LDA $15			;  |
		AND #!Button		;  | 
		BEQ OtherTile4		;  | 
		LDA $71			;  | Only display this tile
		CMP #$09		;  | if you're pressing UP, not dying
		BEQ OtherTile4		;  | and there's some water left
		LDA #$48		;  | 
		BRA Store4		;  |
	OtherTile4:			;  | (this is the water stream, bottom tile)
		LDA #$6D		;  |
	Store4:				;  |
		STA $0302,y		; /

		LDA $15F6,x		; get sprite palette info
		PHX
		LDX $76			; flip the tile if Mario's direction is 0
		BNE +
		ORA #$40
	+           
		PLX
		ORA #$20		; give the sprite second-highest priority
		ORA $64			; add in the priority bits from the level settings
		STA $0303,y		; set properties

		INY #4			; get the index to the next slot of the OAM

                    
		;*************************************************************************************

                

		LDY #$02		; #$02 means the tiles are 16x16
		LDA #$03		; This means we drew 4 tiles
		JSL $01B7B3
		RTS



	SprT1:	db $0C,$1C
	SprT2:	db $01,$02

	GetDrawInfo:
		STZ $186C,x		; reset sprite offscreen flag, vertical
		STZ $15A0,x		; reset sprite offscreen flag, horizontal
		LDA $E4,x		; \
		CMP $1A			;  | set horizontal offscreen if necessary
		LDA $14E0,x		;  |
		SBC $1B			;  |
		BEQ OnScreenX:		;  |
		INC $15A0,x		; /
	OnScreenX:
		LDA $14E0,x		; \
                XBA			;  |
                LDA $E4,x		;  |
                REP #$20		;  |
		SEC			;  |
                SBC $1A			;  | mark sprite invalid if far enough off screen
                CLC			;  |
                ADC.w #$004		;  |
                CMP.w #$0180		;  |
                SEP #$20		;  |
                ROL A			;  |
                AND #$01		;  |
                STA $15C4,x		;  | 
                BNE Invalid 		; /
                
                LDY #$00		; \ set up loop:
                LDA $1662,x		;  | 
                AND #$20		;  | if not smushed (1662 & 0x20), go through loop twice
                BEQ OnScreenLoop	;  | else, go through loop once
                INY			; / 
	OnScreenLoop:
		LDA $D8,x		; \ 
		CLC			;  | set vertical offscreen if necessary
		ADC SprT1,y		;  |
		PHP			;  |
		CMP $1C			;  | (vert screen boundary)
		ROL $00			;  |
		PLP			;  |
		LDA $14D4,x		;  | 
		ADC #$00		;  |
		LSR $00			;  |
		SBC $1D			;  |
		BEQ OnScreenY		;  |
		LDA $186C,x		;  | (vert offscreen)
		ORA SprT2,y		;  |
		STA $186C,x		;  |
	OnScreenY:			;  |
		DEY			;  |
		BPL OnScreenLoop	; /

		LDY $15EA,x		;    get offset to sprite OAM
		LDA $E4,x		; \ 
		SEC			;  | 
		SBC $1A			;  | $00 = sprite x position relative to screen boarder
		STA $00			; / 
		LDA $D8,x		; \ 
		SEC			;  | 
		SBC $1C			;  | $01 = sprite y position relative to screen boarder
		STA $01			; / 
		RTS			;    return

	Invalid:
		PLA			; \  return from *main gfx routine* subroutine...
		PLA			;  | ...(not just this subroutine)
		RTS			; /		