;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fire Breathing Dino Rhino, by mikeyk
;;
;; Description: This Dino Rhino is similar to the original, but he spits fire like the
;; Dino Torch. 
;;
;; Uses first extra bit: NO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    EXTRA_BITS = $7FAB10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite initialization JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
                    ;PHY
                    ;JSR SUB_HORZ_POS
                    ;TYA
                    ;STA $157C,x
                    ;PLY
                    LDA #$02                ;A:9D00 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdiZcHC:0890 VC:068 00 FL:5641
                    STA $C2,x
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

                    TORCH_SPRITE_NUM = $6F
                       
START_SPRITE_CODE   JSR SUB_RHINO_GFX       ;A:876E X:0007 Y:0000 D:0000 DB:03 S:01EF P:envMXdizCHC:1104 VC:086 00 FL:2316
                    LDA $9E,x
                    CMP #TORCH_SPRITE_NUM
                    BNE NO_TORCH
                    LDA EXTRA_BITS,x
                    AND #$F7           
                    STA EXTRA_BITS,x                    
NO_TORCH            LDA $9D                 ;A:00F0 X:0007 Y:00FC D:0000 DB:03 S:01EF P:envMXdizcHC:0220 VC:096 00 FL:2316
                    BNE RETURN_8            ;A:0000 X:0007 Y:00FC D:0000 DB:03 S:01EF P:envMXdiZcHC:0244 VC:096 00 FL:2316
                    LDA $14C8,x             ;A:0000 X:0007 Y:00FC D:0000 DB:03 S:01EF P:envMXdiZcHC:0260 VC:096 00 FL:2316
                    CMP #$08                ;A:0008 X:0007 Y:00FC D:0000 DB:03 S:01EF P:envMXdizcHC:0292 VC:096 00 FL:2316
                    BNE RETURN_8            ;A:0008 X:0007 Y:00FC D:0000 DB:03 S:01EF P:envMXdiZCHC:0308 VC:096 00 FL:2316
                    JSR SUB_OFF_SCREEN_X3   ; decide whether to process offscreen
                    JSL $01A7DC             ; contact with mario?
                    JSL $01802A             ; update position based on speed values?
                    LDA $C2,x               ;A:3000 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0606 VC:101 00 FL:2316
                    BEQ SUB9CA8
                    CMP #$03
                    BEQ SUB9C74
                    JMP SUB9D41

X_OFFSET_LOW        dcb $00,$FE,$02
X_OFFSET_HIGH       dcb $00,$FF,$00
                    
SUB9C74             LDA $AA,x
                    BMI LABEL29
                    STZ $C2,x
                    LDA $1588,x
                    AND #$03
                    BEQ LABEL29
                    LDA $157C,x
                    EOR #$01
                    STA $157C,x

LABEL29             STZ $1602,x             ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZcHC:0012 VC:075 00 FL:2544
                    LDA $1588,x             ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZcHC:0044 VC:075 00 FL:2544
                    AND #$03                ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZcHC:0076 VC:075 00 FL:2544
                    TAY                     ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZcHC:0092 VC:075 00 FL:2544
                    LDA $E4,x               ;A:9C00 X:0007 Y:0000 D:0000 DB:03 S:01EF P:envMXdiZcHC:0106 VC:075 00 FL:2544
                    CLC                     ;A:9C97 X:0007 Y:0000 D:0000 DB:03 S:01EF P:eNvMXdizcHC:0136 VC:075 00 FL:2544
                    ADC X_OFFSET_LOW,y      ;A:9C97 X:0007 Y:0000 D:0000 DB:03 S:01EF P:eNvMXdizcHC:0150 VC:075 00 FL:2544
                    STA $E4,x               ;sprite x low ;A:9C97 X:0007 Y:0000 D:0000 DB:03 S:01EF P:eNvMXdizcHC:0182 VC:075 00 FL:2544
                    LDA $14E0,x             ;A:9C97 X:0007 Y:0000 D:0000 DB:03 S:01EF P:eNvMXdizcHC:0212 VC:075 00 FL:2544
                    ADC X_OFFSET_HIGH,y     ;A:9C00 X:0007 Y:0000 D:0000 DB:03 S:01EF P:envMXdiZcHC:0244 VC:075 00 FL:2544
                    STA $14E0,x             ;sprite x high ;A:9C00 X:0007 Y:0000 D:0000 DB:03 S:01EF P:envMXdiZcHC:0276 VC:075 00 FL:2544
RETURN_8            RTS                     ;A:9C00 X:0007 Y:0000 D:0000 DB:03 S:01EF P:envMXdiZcHC:0308 VC:075 00 FL:2544

RHINO_SPEED         dcb $08,$F8,$10,$F0

SUB9CA8             LDA $1588,x             ;A:9CA8 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:1238 VC:101 00 FL:2317
                    AND #$04                ;A:9C04 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:1270 VC:101 00 FL:2317
                    BEQ LABEL29             ;A:9C04 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:1286 VC:101 00 FL:2317
                    LDA $1540,x             ;A:9C04 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:1302 VC:101 00 FL:2317
                    BNE NO_FLAME            ;A:9CFE X:0007 Y:0002 D:0000 DB:03 S:01EF P:eNvMXdizcHC:1334 VC:101 00 FL:2317
                    LDA #$FF                ; \ set fire breathing timer
                    STA $1540,x             ; / 
                    JSL $01ACF9             ;A;:9CFF X:0007 Y:0001 D:0000 DB:03 S:01EF P:eNvMXdizCHC:0092 VC:082 00 FL:1597
                    AND #$01                ;A:9C05 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:0056 VC:083 00 FL:1597
                    INC A                   ;A:9C01 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:0072 VC:083 00 FL:1597
                    STA $C2,x               ;A:9C02 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:0086 VC:083 00 FL:1597
NO_FLAME            TXA                     ;A:9CFE X:0007 Y:0002 D:0000 DB:03 S:01EF P:eNvMXdizcHC:1356 VC:101 00 FL:2317
                    ASL A                   ;A:9C07 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0002 VC:102 00 FL:2317
                    ASL A                   ;A:9C0E X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0016 VC:102 00 FL:2317
                    ASL A                   ;A:9C1C X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0030 VC:102 00 FL:2317
                    ASL A                   ;A:9C38 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0044 VC:102 00 FL:2317
                    ADC $14                 ;A:9C70 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0058 VC:102 00 FL:2317
                    AND #$3F                ;A:9C78 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0082 VC:102 00 FL:2317
                    BNE LABEL30             ;A:9C38 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0098 VC:102 00 FL:2317
                    JSR SUB_HORZ_POS     ; \ if not facing mario, change directions
                    TYA                     ; |
                    STA $157C,x             ; /
LABEL30             LDA #$10                ;A:9C38 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0120 VC:102 00 FL:2317
                    STA $AA,x               ;A:9C10 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0136 VC:102 00 FL:2317
                    LDY $157C,x             ; \ set x speed for rhino based on direction and sprite number
                    LDA RHINO_SPEED,y       ; | 
                    STA $B6,x               ; / 
                    JSR SUB_SET_FRAME       ;A:9CF8 X:0007 Y:0001 D:0000 DB:03 S:01EF P:eNvMXdizCHC:0328 VC:102 00 FL:2317
                    LDA $1588,x             ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZcHC:0584 VC:102 00 FL:2317
                    AND #$03                ;A:9C04 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:0616 VC:102 00 FL:2317
                    BEQ LABEL32             ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZcHC:0632 VC:102 00 FL:2317
                    LDA #$C0                ;A:9C02 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:1240 VC:078 00 FL:2864
                    STA $AA,x               ;A:9CC0 X:0007 Y:0001 D:0000 DB:03 S:01EF P:eNvMXdizcHC:1256 VC:078 00 FL:2864
                    LDA #$03                ;A:9CC0 X:0007 Y:0001 D:0000 DB:03 S:01EF P:eNvMXdizcHC:1286 VC:078 00 FL:2864
                    STA $C2,x               ;A:9C03 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:1302 VC:078 00 FL:2864
LABEL32             RTS                     ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdiZcHC:0654 VC:102 00 FL:2317


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fire breathing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    
FLAME_TABLE         dcb $41,$42,$42,$32,$22,$12,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
                    dcb $02,$02,$02,$02,$02,$02,$02,$12,$22,$32,$42,$42,$42,$42,$41,$41
                    
                    dcb $41,$43,$43,$33,$23,$13,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
                    dcb $03,$03,$03,$03,$03,$03,$03,$13,$23,$33,$43,$43,$43,$43,$41,$41                    
                    
SUB9D41             STZ $B6,x               ; no x speed while breating fire
                    LDA $1540,x             ;A:9D41 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:1082 VC:085 00 FL:4601
                    BNE TIMER_SET           ;A:9DFF X:0007 Y:0001 D:0000 DB:03 S:01EF P:eNvMXdizcHC:1114 VC:085 00 FL:4601
                    STZ $C2,x               ;A:9D00 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdiZcHC:0860 VC:068 00 FL:5641
                    LDA #$40                ;A:9D00 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdiZcHC:0890 VC:068 00 FL:5641
                    STA $1540,x             ;A:9D40 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0906 VC:068 00 FL:5641
                    LDA #$00                ;A:9D40 X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:0938 VC:068 00 FL:5641
TIMER_SET           CMP #$C0                ;A:9DFF X:0007 Y:0001 D:0000 DB:03 S:01EF P:eNvMXdizcHC:1136 VC:085 00 FL:4601
                    BNE LABEL46             ;A:9DFF X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizCHC:1152 VC:085 00 FL:4601
                    LDY #$17                ; \ play fire breathing sound
                    STY $1DFC               ; /
LABEL46             LSR A                   ;A:9DFF X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizCHC:1174 VC:085 00 FL:4601
                    LSR A                   ;A:9D7F X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizCHC:1188 VC:085 00 FL:4601
                    LSR A                   ;A:9D3F X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizCHC:1202 VC:085 00 FL:4601
                    LDY $C2,x               ;A:9D1F X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizCHC:1216 VC:085 00 FL:4601
                    CPY #$02                ;A:9D1F X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizCHC:1246 VC:085 00 FL:4601
                    BNE LABEL47             ;A:9D1F X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdiZCHC:1262 VC:085 00 FL:4601
                    CLC                     ;A:9D1F X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdiZCHC:1278 VC:085 00 FL:4601
                    ADC #$20                ;A:9D1F X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdiZcHC:1292 VC:085 00 FL:4601
LABEL47             TAY                     ;A:9D3F X:0007 Y:0002 D:0000 DB:03 S:01EF P:envMXdizcHC:1308 VC:085 00 FL:4601
                    LDA FLAME_TABLE,y       ;A:9D3F X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:1322 VC:085 00 FL:4601
                    PHA                     ;A:9D41 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:1354 VC:085 00 FL:4601
                    AND #$0F                ;A:9D41 X:0007 Y:003F D:0000 DB:03 S:01EE P:envMXdizcHC:0008 VC:086 00 FL:4601
                    STA $1602,x             ;A:9D01 X:0007 Y:003F D:0000 DB:03 S:01EE P:envMXdizcHC:0024 VC:086 00 FL:4601
                    PLA                     ;A:9D01 X:0007 Y:003F D:0000 DB:03 S:01EE P:envMXdizcHC:0056 VC:086 00 FL:4601
                    LSR A                   ;A:9D41 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:0084 VC:086 00 FL:4601
                    LSR A                   ;A:9D20 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizCHC:0098 VC:086 00 FL:4601
                    LSR A                   ;A:9D10 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:0112 VC:086 00 FL:4601
                    LSR A                   ;A:9D08 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:0126 VC:086 00 FL:4601
                    STA $151C,x             ;A:9D04 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:0140 VC:086 00 FL:4601
                    BNE RETURN_10           ;A:9D04 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:0172 VC:086 00 FL:4601
                    TXA                     ;A:9D6F X:0007 Y:0036 D:0000 DB:03 S:01EF P:envMXdizCHC:0406 VC:076 00 FL:4909
                    EOR $13                 ;A:9D07 X:0007 Y:0036 D:0000 DB:03 S:01EF P:envMXdizCHC:0420 VC:076 00 FL:4909
                    AND #$03                ;A:9DEE X:0007 Y:0036 D:0000 DB:03 S:01EF P:eNvMXdizCHC:0444 VC:076 00 FL:4909
                    BNE RETURN_10           ;A:9D02 X:0007 Y:0036 D:0000 DB:03 S:01EF P:envMXdizCHC:0460 VC:076 00 FL:4909
                    JSR SUB_FLAME_CLIP      ;A:9D00 X:0007 Y:0036 D:0000 DB:03 S:01EF P:envMXdiZCHC:0398 VC:077 00 FL:4917
                    JSL $03B664             ;A:9D24 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizCHC:1104 VC:077 00 FL:4917
                    JSL $03B72B             ;A:9D01 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:0694 VC:078 00 FL:4917
                    BCC RETURN_10           ;A:9D30 X:0007 Y:0001 D:0000 DB:03 S:01EF P:envMXdizcHC:0272 VC:079 00 FL:4917
                    LDA $1490               ; \ flame doesn't hurt mario if has star
                    BNE RETURN_10           ; /
                    JSL $00F5B7             ; hurt mario
RETURN_10           RTS                     ;A:9D04 X:0007 Y:003F D:0000 DB:03 S:01EF P:envMXdizcHC:0194 VC:086 00 FL:4601


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $9DB6 - make flames deadly!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;facing left sideways, facing left up, facing right sideways, facing right up
FLAME1              dcb $DC,$FB,$10,$FB     ;x displacement, low
FLAME2              dcb $FF,$FF,$00,$FF     ;x displacement, high
FLAME3              dcb $24,$0C,$24,$0C
FLAME4              dcb $02,$D2,$02,$D2     ;y displacement, low
FLAME5              dcb $00,$FF,$00,$FF     ;y displacement, high
FLAME6              dcb $0C,$24,$0C,$24

SUB_FLAME_CLIP      LDA $1602,x             ;A:9D00 X:0007 Y:0036 D:0000 DB:03 S:01ED P:envMXdiZCHC:0444 VC:077 00 FL:4918
                    SEC                     ;A:9D03 X:0007 Y:0036 D:0000 DB:03 S:01ED P:envMXdizCHC:0476 VC:077 00 FL:4918
                    SBC #$02                ;A:9D03 X:0007 Y:0036 D:0000 DB:03 S:01ED P:envMXdizCHC:0490 VC:077 00 FL:4918
                    TAY                     ;A:9D01 X:0007 Y:0036 D:0000 DB:03 S:01ED P:envMXdizCHC:0506 VC:077 00 FL:4918
                    LDA $157C,x             ;A:9D01 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0520 VC:077 00 FL:4918
                    BNE LABEL49             ;A:9D01 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0552 VC:077 00 FL:4918
                    INY                     ;
                    INY                     ;
LABEL49             LDA $E4,x               ;A:9D01 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0574 VC:077 00 FL:4918
                    CLC                     ;A:9D71 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0604 VC:077 00 FL:4918
                    ADC FLAME1,y            ;A:9D71 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0618 VC:077 00 FL:4918
                    STA $04                 ;A:9D73 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0650 VC:077 00 FL:4918
                    LDA $14E0,x             ;A:9D73 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0674 VC:077 00 FL:4918
                    ADC FLAME2,y            ;A:9D00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0706 VC:077 00 FL:4918
                    STA $0A                 ;A:9D00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0738 VC:077 00 FL:4918
                    LDA FLAME3,y            ;A:9D00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0762 VC:077 00 FL:4918
                    STA $06                 ;A:9D0C X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0794 VC:077 00 FL:4918
                    LDA $D8,x               ;A:9D0C X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0818 VC:077 00 FL:4918
                    CLC                     ;A:9D70 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0848 VC:077 00 FL:4918
                    ADC FLAME4,y            ;A:9D70 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizcHC:0862 VC:077 00 FL:4918
                    STA $05                 ;A:9D4C X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0894 VC:077 00 FL:4918
                    LDA $14D4,x             ;A:9D4C X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0918 VC:077 00 FL:4918
                    ADC FLAME5,y            ;A:9D01 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0950 VC:077 00 FL:4918
                    STA $0B                 ;A:9D01 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0982 VC:077 00 FL:4918
                    LDA FLAME6,y            ;A:9D01 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:1006 VC:077 00 FL:4918
                    STA $07                 ;A:9D24 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:1038 VC:077 00 FL:4918
                    RTS                     ;A:9D24 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:1062 VC:077 00 FL:4918


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; unknown routine - specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;;org $039DEF

SUB_SET_FRAME       INC $1570,x             ;A:9CF8 X:0007 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:0374 VC:102 00 FL:2318
                    LDA $1570,x             ;A:9CF8 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0420 VC:102 00 FL:2318
                    AND #$08                ;A:9C01 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdizCHC:0452 VC:102 00 FL:2318
                    LSR A                   ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZCHC:0468 VC:102 00 FL:2318
                    LSR A                   ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0482 VC:102 00 FL:2318
                    LSR A                   ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0496 VC:102 00 FL:2318
                    STA $1602,x             ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0510 VC:102 00 FL:2318
                    RTS                     ;A:9C00 X:0007 Y:0001 D:0000 DB:03 S:01ED P:envMXdiZcHC:0542 VC:102 00 FL:2318


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dino rhino and dino torch graphics - specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DT_X_OFFSET         dcb $D6,$DE,$EA,$F6,$00,$FB,$FB,$FB,$FB,$00 
DT_Y_OFFSET         dcb $FC,$FC,$FC,$FC,$00,$CC,$D4,$E0,$EC,$00
DT_FIRE_TILEMAP     dcb $80,$82,$84,$86,$00,$88,$8A,$8C,$8E,$00 
DT_PROP             dcb $09,$05,$05,$05,$0F 

DR_X_OFFSET         dcb $F8,$08,$F8,$08,$08,$F8,$08,$F8 
DR_PROP             dcb $0F,$0F,$0F,$0F,$4F,$4F,$4F,$4F             ;dino rhino properties, xyppccct
DR_Y_OFFSET         dcb $F0,$F0,$00,$00
DR_TILEMAP          dcb $C0,$C2,$E4,$E6,$C0,$C2,$E0,$E2,$C8,$CA,$E8,$E2,$CC,$CE,$EC,$EE

FRAMES_WRITTEN      dcb $07,$06,$05,$04,$03
             
SUB_RHINO_GFX       JSR SUB_GET_DRAW_INFO   ;A:876E X:0007 Y:0000 D:0000 DB:03 S:01ED P:envMXdizCHC:0806 VC:079 00 FL:524
                    LDA $157C,x             ;A:0040 X:0007 Y:00EC D:0000 DB:03 S:01ED P:envMXdizcHC:0686 VC:080 00 FL:524
                    STA $02                 ;A:0001 X:0007 Y:00EC D:0000 DB:03 S:01ED P:envMXdizcHC:0718 VC:080 00 FL:524
                    LDA $1602,x             ;A:0001 X:0007 Y:00EC D:0000 DB:03 S:01ED P:envMXdizcHC:0742 VC:080 00 FL:524
                    STA $04                 ;A:0000 X:0007 Y:00EC D:0000 DB:03 S:01ED P:envMXdiZcHC:0774 VC:080 00 FL:524
                    PHX                     ;A:006E X:0007 Y:00EC D:0000 DB:03 S:01ED P:eNvMXdizcHC:0860 VC:080 00 FL:524
                    
                    LDX #$03                ;draw dino rhino first
LABEL33             STX $0F                 ;A:006E X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0898 VC:080 00 FL:524
                    LDA $02                 ;A:006E X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0922 VC:080 00 FL:524
                    CMP #$01                ;A:0001 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0946 VC:080 00 FL:524
                    BCS LABEL34             ;A:0001 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZCHC:0962 VC:080 00 FL:524
                    TXA                     ;A:0002 X:0004 Y:0000 D:0000 DB:00 S:01F5 P:envMXdizcHC:0626 VC:049 00 FL:6024
                    CLC                     ;A:0004 X:0004 Y:0000 D:0000 DB:00 S:01F5 P:envMXdizcHC:0640 VC:049 00 FL:6024
                    ADC #$04                ;A:0004 X:0004 Y:0000 D:0000 DB:00 S:01F5 P:envMXdizcHC:0654 VC:049 00 FL:6024
                    TAX                     ;A:0094 X:0004 Y:0000 D:0000 DB:00 S:01F5 P:eNvMXdizcHC:0670 VC:049 00 FL:6024
LABEL34             LDA DR_PROP,x           ;A:0001 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZCHC:0984 VC:080 00 FL:524
                    ORA $64 
                    STA $0303,y             ;A:002F X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:1016 VC:080 00 FL:524
                    LDA DR_X_OFFSET,x       ;A:002F X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:1048 VC:080 00 FL:524
                    CLC                     ;A:0008 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:1080 VC:080 00 FL:524
                    ADC $00                 ;A:0008 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:1094 VC:080 00 FL:524
                    STA $0300,y             ;A:00B8 X:0003 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizcHC:1118 VC:080 00 FL:524
                    LDA $04                 ;A:00B8 X:0003 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizcHC:1150 VC:080 00 FL:524
                    CMP #$01                ;A:0000 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZcHC:1174 VC:080 00 FL:524
                    LDX $0F                 ;A:0000 X:0003 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizcHC:1190 VC:080 00 FL:524
                    LDA DR_Y_OFFSET,x       ;A:0000 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:1214 VC:080 00 FL:524
                    ADC $01                 ;A:0000 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZcHC:1246 VC:080 00 FL:524
                    STA $0301,y             ;A:0040 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:1270 VC:080 00 FL:524
                    LDA $04                 ;A:0040 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:1302 VC:080 00 FL:524
                    ASL A                   ;A:0000 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZcHC:1326 VC:080 00 FL:524
                    ASL A                   ;A:0000 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZcHC:1340 VC:080 00 FL:524
                    ADC $0F                 ;A:0000 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZcHC:1354 VC:080 00 FL:524
                    TAX                     ;A:0003 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0010 VC:081 00 FL:524
                    LDA DR_TILEMAP,x        ;A:0003 X:0003 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0024 VC:081 00 FL:524
                    STA $0302,y             ;A:00E6 X:0003 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizcHC:0056 VC:081 00 FL:524
                    INY                     ;A:00E6 X:0003 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizcHC:0088 VC:081 00 FL:524
                    INY                     ;A:00E6 X:0003 Y:00ED D:0000 DB:03 S:01EC P:eNvMXdizcHC:0102 VC:081 00 FL:524
                    INY                     ;A:00E6 X:0003 Y:00EE D:0000 DB:03 S:01EC P:eNvMXdizcHC:0116 VC:081 00 FL:524
                    INY                     ;A:00E6 X:0003 Y:00EF D:0000 DB:03 S:01EC P:eNvMXdizcHC:0130 VC:081 00 FL:524
                    LDX $0F                 ;A:00E6 X:0003 Y:00F0 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0144 VC:081 00 FL:524
                    DEX                     ;A:00E6 X:0003 Y:00F0 D:0000 DB:03 S:01EC P:envMXdizcHC:0168 VC:081 00 FL:524
                    BPL LABEL33             ;A:00E6 X:0002 Y:00F0 D:0000 DB:03 S:01EC P:envMXdizcHC:0182 VC:081 00 FL:524
                    PLX                     ;A:00C0 X:00FF Y:00FC D:0000 DB:03 S:01EC P:eNvMXdizcHC:0204 VC:083 00 FL:524



                    LDA $151C,x             ; now draw the flame
                    STA $03                 ;A:0004 X:0007 Y:00EC D:0000 DB:03 S:01ED P:enVMXdizCHC:0842 VC:058 00 FL:4606
                    LDA $1602,x             ;A:0004 X:0007 Y:00EC D:0000 DB:03 S:01ED P:enVMXdizCHC:0866 VC:058 00 FL:4606
                    STA $04                 ;A:0001 X:0007 Y:00EC D:0000 DB:03 S:01ED P:enVMXdizCHC:0898 VC:058 00 FL:4606
                    PHX                     ;A:0001 X:0007 Y:00EC D:0000 DB:03 S:01ED P:enVMXdizCHC:0922 VC:058 00 FL:4606
                    LDA $14                 ;A:0001 X:0007 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizCHC:0944 VC:058 00 FL:4606
                    AND #$02                ;A:0097 X:0007 Y:00EC D:0000 DB:03 S:01EC P:eNVMXdizCHC:0968 VC:058 00 FL:4606
                    ASL A                   ;A:0002 X:0007 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizCHC:0984 VC:058 00 FL:4606
                    ASL A                   ;A:0004 X:0007 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:0998 VC:058 00 FL:4606
                    ASL A                   ;A:0008 X:0007 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1012 VC:058 00 FL:4606
                    ASL A                   ;A:0010 X:0007 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1026 VC:058 00 FL:4606
                    ASL A                   ;A:0020 X:0007 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1040 VC:058 00 FL:4606
                    LDX $04                 ;A:0040 X:0007 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1054 VC:058 00 FL:4606
                    CPX #$03                ;A:0040 X:0001 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1078 VC:058 00 FL:4606
                    BEQ LABEL35             ;A:0040 X:0001 Y:00EC D:0000 DB:03 S:01EC P:eNVMXdizcHC:1094 VC:058 00 FL:4606
                    ASL A                   ;A:0040 X:0001 Y:00EC D:0000 DB:03 S:01EC P:eNVMXdizcHC:1110 VC:058 00 FL:4606
LABEL35             STA $05                 ;A:0080 X:0001 Y:00EC D:0000 DB:03 S:01EC P:eNVMXdizcHC:1124 VC:058 00 FL:4606

                    LDX #$03                ;A:0080 X:0001 Y:00EC D:0000 DB:03 S:01EC P:eNVMXdizcHC:1148 VC:058 00 FL:4606
LABEL40             CPX $03                 ;A:002F X:0003 Y:00F0 D:0000 DB:03 S:01EC P:envMXdizCHC:0630 VC:059 00 FL:4606
                    BMI DONE                ;A:002F X:0003 Y:00F0 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0654 VC:059 00 FL:4606
                    STX $06                 ;A:0080 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1164 VC:058 00 FL:4606
                    LDA $04                 ;A:0080 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1188 VC:058 00 FL:4606
                    CMP #$03                ;A:0001 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:1212 VC:058 00 FL:4606
                    BNE LABEL36             ;A:0001 X:0004 Y:00EC D:0000 DB:03 S:01EC P:eNVMXdizcHC:1228 VC:058 00 FL:4606
                    TXA                     ;A:0003 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdiZCHC:0814 VC:059 00 FL:4680
                    CLC                     ;A:0004 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizCHC:0828 VC:059 00 FL:4680
                    ADC #$05                ;A:0004 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:0842 VC:059 00 FL:4680
                    TAX                     ;A:0009 X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0858 VC:059 00 FL:4680
LABEL36             PHX                     ;A:0001 X:0004 Y:00EC D:0000 DB:03 S:01EC P:eNVMXdizcHC:1250 VC:058 00 FL:4606
                    LDA DT_X_OFFSET,x       ;A:0001 X:0004 Y:00EC D:0000 DB:03 S:01EB P:eNVMXdizcHC:1272 VC:058 00 FL:4606
                    LDX $02                 ;A:0000 X:0004 Y:00EC D:0000 DB:03 S:01EB P:enVMXdiZcHC:1304 VC:058 00 FL:4606
                    BNE LABEL37             ;A:0000 X:0001 Y:00EC D:0000 DB:03 S:01EB P:enVMXdizcHC:1328 VC:058 00 FL:4606
                    EOR #$FF                ;
                    INC A                   ;
LABEL37             PLX                     ;A:0000 X:0001 Y:00EC D:0000 DB:03 S:01EB P:enVMXdizcHC:1350 VC:058 00 FL:4606
                    CLC                     ;A:0000 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:0010 VC:059 00 FL:4606
                    ADC $00                 ;A:0000 X:0004 Y:00EC D:0000 DB:03 S:01EC P:enVMXdizcHC:0024 VC:059 00 FL:4606
                    STA $0300,y             ;A:0071 X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0048 VC:059 00 FL:4606
                    LDA DT_Y_OFFSET,x       ;A:0071 X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdizcHC:0080 VC:059 00 FL:4606
                    CLC                     ;A:0000 X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZcHC:0112 VC:059 00 FL:4606
                    ADC $01                 ;A:0000 X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZcHC:0126 VC:059 00 FL:4606
                    STA $0301,y             ;A:00B0 X:0004 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizcHC:0150 VC:059 00 FL:4606
LABEL38             LDA DT_FIRE_TILEMAP,x   ;A:0003 X:0008 Y:00F0 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0972 VC:059 00 FL:4817
                    STA $0302,y             ;A:00AA X:0001 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizCHC:0316 VC:059 00 FL:4606
                    LDA #$00                ;A:00AA X:0001 Y:00EC D:0000 DB:03 S:01EC P:eNvMXdizCHC:0348 VC:059 00 FL:4606
                    LDX $02                 ;A:0000 X:0001 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZCHC:0364 VC:059 00 FL:4606
                    BNE LABEL41             ;A:0000 X:0001 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:0388 VC:059 00 FL:4606
                    ORA #$40                ;
LABEL41             LDX $06                 ;A:0000 X:0001 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:0410 VC:059 00 FL:4606
                    EOR $05                 ;A:0000 X:0003 Y:00F0 D:0000 DB:03 S:01EC P:eNvMXdizcHC:1210 VC:059 00 FL:4817
                    ORA DT_PROP,x           ;A:0000 X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdiZCHC:0472 VC:059 00 FL:4606
                    ORA $64                 ;A:000F X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:0504 VC:059 00 FL:4606
                    STA $0303,y             ;A:002F X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:0528 VC:059 00 FL:4606
                    INY                     ;A:002F X:0004 Y:00EC D:0000 DB:03 S:01EC P:envMXdizCHC:0560 VC:059 00 FL:4606
                    INY                     ;A:002F X:0004 Y:00ED D:0000 DB:03 S:01EC P:eNvMXdizCHC:0574 VC:059 00 FL:4606
                    INY                     ;A:002F X:0004 Y:00EE D:0000 DB:03 S:01EC P:eNvMXdizCHC:0588 VC:059 00 FL:4606
                    INY                     ;A:002F X:0004 Y:00EF D:0000 DB:03 S:01EC P:eNvMXdizCHC:0602 VC:059 00 FL:4606
                    DEX                     ;A:002F X:0004 Y:00F0 D:0000 DB:03 S:01EC P:eNvMXdizCHC:0616 VC:059 00 FL:4606
                    CPX $03                 ;A:002F X:0003 Y:00F0 D:0000 DB:03 S:01EC P:envMXdizCHC:0630 VC:059 00 FL:4606
                    BPL LABEL40             ;A:002F X:0003 Y:00F0 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0654 VC:059 00 FL:4606
DONE

                    PLX                     ;A:002F X:0003 Y:00F0 D:0000 DB:03 S:01EC P:eNvMXdizcHC:0670 VC:059 00 FL:4606
                    LDY $151C,x             ;A:002F X:0007 Y:00F0 D:0000 DB:03 S:01ED P:envMXdizcHC:0698 VC:059 00 FL:4606
                    LDA FRAMES_WRITTEN,y    ;A:002F X:0007 Y:0004 D:0000 DB:03 S:01ED P:envMXdizcHC:0730 VC:059 00 FL:4606
                    LDY #$02                ;A:0000 X:0007 Y:0004 D:0000 DB:03 S:01ED P:envMXdiZcHC:0762 VC:059 00 FL:4606
                    JSL $01B7B3             ;A:0000 X:0007 Y:0002 D:0000 DB:03 S:01ED P:envMXdizcHC:0778 VC:059 00 FL:4606
                    RTS                     ;A:0170 X:0007 Y:00F0 D:0000 DB:03 S:01ED P:envMXdizcHC:0846 VC:061 00 FL:4606
                                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SHARED ROUTINES BELOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
             
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $B760 - graphics routine helper - shared
; sets off screen flags and sets index to OAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B75C

TABLE1              dcb $0C,$1C
TABLE2              dcb $01,$02

SUB_GET_DRAW_INFO   STZ $186C,x             ; reset sprite offscreen flag, vertical
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
; $B817 - horizontal mario/sprite check - shared
; Y = 1 if mario left of sprite??
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B817             ; Y = 1 if contact

SUB_HORZ_POS        LDY #$00                ;A:25D0 X:0006 Y:0001 D:0000 DB:03 S:01ED P:eNvMXdizCHC:1020 VC:097 00 FL:31642
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
; $B85D - off screen processing code - shared
; sprites enter at different points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    ;org $03B83B             

TABLE3              dcb $40,$B0
TABLE6              dcb $01,$FF 
TABLE4              dcb $30,$C0,$A0,$80,$A0,$40,$60,$B0 
TABLE5              dcb $01,$FF,$01,$FF,$01,$00,$01,$FF

SUB_OFF_SCREEN_X0   LDA #$06                ; \ megamole
                    BRA STORE_03            ; | 
SUB_OFF_SCREEN_X1   LDA #$04                ; |
                    BRA STORE_03            ; |
SUB_OFF_SCREEN_X2   LDA #$02                ; |
STORE_03            STA $03                 ; |
                    BRA START_SUB           ; |
SUB_OFF_SCREEN_X3   STZ $03                 ; / rex, dino rhino/torch

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

                