;Disables jumping and spin jumping
;By: Chdata/Fakescaper

print "INIT ",pc
print "MAIN ",pc
PHB : PHK : PLB
JSR AntiJump
PLB : RTL

AntiJump:
REP #$20
LDA #$8080	;Disable any form of jumping
TSB $0DAA
TSB $0DAC
SEP #$20
RTS