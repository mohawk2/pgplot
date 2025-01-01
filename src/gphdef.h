
                     /* Graphics processor definitions */


#define GPHrFIFOentry         0x00     /* Non-incrementing registers */
#define GPHrCmdCtl            0x02
#define GPHrOpMode            0x04
#define GPHrDispCtl           0x06

#define GPHrRasterCount       0x80     /* Auto-incrementing registers */
#define GPHrHorizSync         0x82
#define GPHrHorizDisp         0x84
#define GPHrVertSync          0x86
#define GPHrVertDisp          0x88
#define GPHrSplitScreen       0x8a
#define GPHrBlinkCtl          0x90
#define GPHrHorizWindow       0x92
#define GPHrVertWindow        0x94
#define GPHrGraphicCursor     0x98

#define GPHrRasterAddr0       0xC0
#define GPHrMemWidth0         0xC2
#define GPHrStartAddr0        0xC4
#define GPHrRasterAddr1       0xC8
#define GPHrMemWidth1         0xCA
#define GPHrStartAddr1        0xCC
#define GPHrRasterAddr2       0xD0
#define GPHrMemWidth2         0xD2
#define GPHrStartAddr2        0xD4
#define GPHrRasterAddr3       0xD8
#define GPHrMemWidth3         0xDA
#define GPHrStartAddr3        0xDC
#define GPHrBlockCur1         0xE0
#define GPHrBlockCur2         0xE4
#define GPHrCursorDef         0xE8
#define GPHrZoomFactor        0xEA
#define GPHrLightPenAddr      0xEC

#define GPHsWriteEmpty        0x01     /* Status bits */
#define GPHsWriteReady        0x02
#define GPHsReadReady         0x04
#define GPHsReadFull          0x08
#define GPHsPenStrobe         0x10
#define GPHsCommandEnd        0x20
#define GPHsAreaDetect        0x40
#define GPHsCommandError      0x80

#define GPHarNoChk            0x00     /* Area modes */
#define GPHarExAbort          0x20
#define GPHarExSupp           0x40
#define GPHarExDetect         0x60
#define GPHarEnAbort          0xa0
#define GPHarEnSupp           0xc0
#define GPHarEnDetect         0xe0

#define GPHclBoth             0x00     /* Color mode */
#define GPHclOne              0x08
#define GPHclZero             0x10
#define GPHclRAM              0x18

#define GPHopRep              0x00     /* Operation mode */
#define GPHopOR               0x01
#define GPHopAND              0x02
#define GPHopEOR              0x03
#define GPHopEQL              0x04
#define GPHopNEQ              0x05
#define GPHopLT               0x06
#define GPHopGT               0x07

#define GPHabs              0x0000     /* Relative absolute mode */
#define GPHrel              0x0400
