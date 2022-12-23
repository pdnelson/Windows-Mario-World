;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This sprite will allow mario to double jump for the entire level. Insert as sprite. Requested by darkguitar4life
; INIT and MAIN JSL targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    dcb "INIT"
LDA #$01   ;\
STA $7F9F4D;/ $7F9F4D = My Jump flag <33333
                    RTL         

                    dcb "MAIN"
                    JSL aabbcF
                    RTL

aabbcF: ; medicre bits get triva!
LDA $77  ;\
AND #$04 ;| If Mario is touching the ground, reset the jump flag
BNE reset;/
LDA $7F9F4D ;\ If jump flag is working, then disable everything else
BEQ disab   ;/
LDA $16   ;\
AND #$80  ;| If pressing B then do jump code
BNE jump  ;/ 
disab:   ; This sprite was actually based off a cutscene tool I made a while back.
RTL     ; I worked forever on it; and it worked really nicely. Too bad Romi beat me too it. Then again mine just did the transitions!
reset:  ; Oh well.
LDA #$01   ;\
STA $7F9F4D;/ Resetting jump flag
RTL
jump:
LDA #$A9 ;\I had some sense A9 was the correct speed. And it was >_>
STA $7D  ;/
LDA #$00   ;\
STA $7F9F4D;/ has jumped!
RTL    ; Glad you're looking at code! ^_^