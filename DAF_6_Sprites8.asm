*****************************************************
** Used Labels                                      *
*****************************************************

TEXT_SCREEN_START       EQU $0400
TEXT_SCREEN_END         EQU $05FF

FIRQENR                 EQU $FF93
FIRQ_Jump_position      EQU $FEF4    * Store $7E which is the JMP opcode
FIRQ_Start_Address      EQU $FEF5    * Store the address you want the IRQ to jumpt to at this address
FIRQ_POINT              EQU $FEF4    * $FEF4 is the usual CoCo3 SuperBasic FIRQ value

IRQENR                  EQU $FF92
IRQ_Jump_position       EQU $FEF7       * Store  $7E which is the JMP opcode
IRQ_Start_Address       EQU $FEF8       * Store the address you want the IRQ to jumpt to at this address
IRQ_POINT               EQU $FEF7       * $FEF7 is the usual CoCo3 SuperBasic IRQ value

PIA0_Byte_0_KeyRw_Joy   EQU $FF00
PIA0_Byte_1_HSYNC       EQU $FF01
PIA0_Byte_2             EQU $FF02
PIA0_Byte_3_VSYNC       EQU $FF03

PIA1_Byte_0_IRQ         EQU $FF20
PIA1_Byte_1_IRQ         EQU $FF21
PIA1_Byte_2             EQU $FF22
PIA1_Byte_3_IRQ_Ct_Snd  EQU $FF23

INIT0_Register0         EQU $FF90

INIT1_Register1         EQU $FF91

Timer_register_LSB      EQU $FF95
Timer_register_MSB      EQU $FF94

Video_Mode_Register     EQU $FF98
Vid_Res_Reg             EQU $FF99
Border_Register         EQU $FF9A
VidStart                EQU $FF9D           *
Hor_Offset_Reg          EQU $FF9F

* MMU Related
MMU_Reg_Bank0_0         EQU $FFA0   * Page $0000-$1FFF  Block #0
MMU_Reg_Bank0_1			    EQU $FFA1   * Page $2000-$3FFF  Block #1
MMU_Reg_Bank0_2			    EQU $FFA2   * Page $4000-$5FFF  Block #2
MMU_Reg_Bank0_3			    EQU $FFA3   * Page $6000-$7FFF  Block #3
MMU_Reg_Bank0_4			    EQU $FFA4   * Page $8000-$9FFF  Block #4
MMU_Reg_Bank0_5			    EQU $FFA5   * Page $A000-$BFFF  Block #5
MMU_Reg_Bank0_6			    EQU $FFA6   * Page $C000-$DFFF  Block #6
MMU_Reg_Bank0_7			    EQU $FFA7   * Page $E000-$FFFF  Block #7
MMU_Reg_Bank1_0         EQU $FFA8   * Page $0000-$1FFF  Block #0
MMU_Reg_Bank1_1			    EQU $FFA9   * Page $2000-$3FFF  Block #1
MMU_Reg_Bank1_2			    EQU $FFAA   * Page $4000-$5FFF  Block #2
MMU_Reg_Bank1_3			    EQU $FFAB   * Page $6000-$7FFF  Block #3
MMU_Reg_Bank1_4			    EQU $FFAC   * Page $8000-$9FFF  Block #4
MMU_Reg_Bank1_5			    EQU $FFAD   * Page $A000-$BFFF  Block #5
MMU_Reg_Bank1_6			    EQU $FFAE   * Page $C000-$DFFF  Block #6
MMU_Reg_Bank1_7			    EQU $FFAF   * Page $E000-$FFFF  Block #7

*Screen Colour Settings xxRGBrgb
Palette_Start           EQU $FFB0
Palette_0               EQU $FFB0
Palette_1               EQU $FFB1
Palette_2               EQU $FFB2
Palette_3               EQU $FFB3
Palette_4               EQU $FFB4
Palette_5               EQU $FFB5
Palette_6               EQU $FFB6
Palette_7               EQU $FFB7
Palette_8               EQU $FFB8
Palette_9               EQU $FFB9
Palette_10              EQU $FFBA
Palette_11              EQU $FFBB
Palette_12              EQU $FFBC
Palette_13              EQU $FFBD
Palette_14              EQU $FFBE
Palette_15              EQU $FFBF

Regular_Speed           EQU $FFD8
High_Speed_Mode         EQU $FFD9
ROM_Mode_Enable         EQU $FFDE
RAM_Mode_Enable         EQU $FFDF

Screen_Start            EQU $0F80
Screen_Location         EQU $0F80
Border_Colour           EQU $00     * 00 - Black

*                        PAGE BLK       GIME Address      Used for
* StarsData/VideoRam0     EQU $38     * 70000-71FFF   0   0000-1FFF       Graphics RAM
* VideoRam1               EQU $39     * 72000-73FFF   1   2000-3FFF       Graphics RAM
* VideoRam2               EQU $3A     * 74000-75FFF   2   4000-5FFF       Graphics RAM
* VideoRam3               EQU $3B     * 76000-77FFF   3   6000-7FFF       Graphics RAM
* Unused                  EQU $3C     * 78000-79FFF   4   8000-9FFF       6809 Code
* Unused                  EQU $3D     * 7A000-7BFFF   5   A000-BFFF       6809 Code
* Unused                  EQU $3E     * 7C000-7DFFF   6   C000-DFFF       6809 Code
* IRQsAndCode             EQU $3F     * 7E000-7FDFF   7   E000-FDFF       6809 Code

* Sample is 27686 bytes we will load it in RAM at $53DA to $BFFF using mem Blocks
* We will use memblocks:
* $30 = 0000-1FFF
* $31 = 2000-3FFF
* $32 = 4000-5FFF
* $33 = 6000-7FFF

* Stored in Pages: $30,$31,$32,$33
        ORG     MMU_Reg_Bank0_0 * Set Page 0 - $0000 - $1FFF
        FCB     $30             * GIME Page $30
        ORG     MMU_Reg_Bank0_1 * Set Page 1 - $2000 - $3FFF
        FCB     $31             * GIME Page $31
        ORG     MMU_Reg_Bank0_2 * Set Page 2 - $4000 - $5FFF
        FCB     $32             * GIME Page $32
        ORG     MMU_Reg_Bank0_3 * Set Page 3 - $6000 - $7FFF
        FCB     $33             * GIME Page $33

SampleStart  EQU   $8000-27686  * $8000 minus the real length of the sample file
        ORG   SampleStart
    INCLUDEBIN ./Samples/Music.raw
SampleEnd:
RealSampleStart EQU   SampleStart
SampleStartMSB  EQU   (RealSampleStart/256)
SampleStartLSB  EQU   RealSampleStart-(SampleStartMSB*256)
****
* Load in the sprites
        ORG     MMU_Reg_Bank0_4 * EQU $FFA4   * Set Page 3 - $8000 - $9FFF
        FCB     $34             * GIME Page $34
        ORG   $8000
    INCLUDE ./Sprites/000061.bmp.asm

    ORG     $D000
* Sprite movement (Position) Co-ordinates
SpritePos:
    INCLUDE ./SpriteCoOrdinates.asm

SpritePosEnd:


    SETDP   $F0
      ORG   $F000
RND     RMB     2         * Random number seed

* Tested with MAME, 100 times the FIRQ was triggered between each VSYNC IRQ
* MAME scanline delay count before the scanline is really at 00 (I think??  From CoCo 3 Mame info...)
* Triggered 100 times between each IRQ, which delays it 18 times

FIRQ_Between_IRQs   EQU 99   	* Tested with MAME, 99 times the FIRQ was triggered between each VSYNC IRQ
IRQ_Delay_Count     EQU 59   	* MAME scanline delay count before the scanline should be in the middle of the screen (From CoCo 3 Mame info...)
FIRQ_Countdown      EQU $0254  	* $254=6005.950455 samples per second
FIRQCountDown       FCB 61     	* FIRQ counts this value down to keep track of where the scanline is being drawn
TestPointer         FDB $4000
TestPointerA        FCB 0
VideoBeam           FCB 0
FIRQCount           FCB 0
IRQ_Flag            FCB 0     * 0 if IRQ is not in progress, 1 is processing the IRQ

StarsDelay          EQU $0100+3    * Delay # of VSYNC IRQs until the stars move down the screen
StarsCounter        FDB StarsDelay

ToggleStars         FCB 0
ToggleTop           FCB 0
ToggleBot           FCB 0

Stars       EQU $0000      * Stars start address
StarsEnd    EQU $03FF      * Stars end address
*****************************************
VSyncIRQ:
        LDA     IRQENR         	  * Re enable the VSYNC IRQ
        LDA     #50               * allows the scanline to move to the top of the screen and get to row 95 (we can draw the lower halfish of the screen now)
        STA     FIRQCount         * 100 FIRQs per frame so each time we count down 25 we will be 1/4 further down the screen
        LDA     #0                * Make the video beam 0 + add the height of the sprite to draw
        STA     VideoBeam         * Video beam just finished drawing the screen and is in VBLANC on it's way to start again at the top so we are now at the top of the screen

        DEC     StarsCounter+1    * Decrement the delay counter for when we should move the starfield
        BNE     >
        DEC     StarsCounter
        BNE     >
        CLR     ToggleStars
!
        TST     ToggleBot         * If flag <> 0 then draw sprites on the bottom of the screen
        BEQ     >                 * If flag = 0 then skip drawing sprites on the bottom of the screen
        JSR     DrawBottomSprites * Draw the sprites that are from row 112 to 224
!
        RTI

*****************************************
* Audio sample player
        opt     cd
        opt     cc                *
FIRQ_Sound:
        STA     FIRQ0Restore+1    * Save exit value of A
        LDA     FIRQENR           * Re enable the FIRQ
        LDA     #$21              * Set the MMU bank registers to
        STA     INIT1_Register1   * Bank task 1 - alternate 64k bank is now the current one
SamplePointer:
        LDA     >$0000
        STA     $FF20             * Send byte to DAC
        INC     SamplePointer+2   * move the pointer forward to the next byte
        BNE     >                 * Skip if we haven't reached $00
        INC     SamplePointer+1   * Increment the MSB of the pointer to the sample
        BPL     >                 * If sample pointer has not reach $8000 then skip forward
        LDA     #SampleStartMSB   * Restore pointer to the start of the sample (loop forever)
        STA     SamplePointer+1
        LDA     #SampleStartLSB
        STA     SamplePointer+2
!
        LDA     #$20              * Set the MMU bank registers to
        STA     INIT1_Register1   * Bank task 0 - Back to the normal 64k bank
        DEC     FIRQCount
        BEQ     Do_VideoBeam_IRQ  * We have counted down FIRQs so the Video beam should be down another 1/4 of the screen so go act like an VideoBeam IRQ was triggered for Joust Hardware

FIRQ0Restore:
        LDA     #$00
        RTI                       * Return from the FIRQ
* Act like a Hardware VideoBeam IRQ has been triggered
Do_VideoBeam_IRQ:
        LDA     #25               * The VSYNC IRQ will trigger before this counts down again, which is what we want
        STA     FIRQCount         * 100 FIRQs per frame so each time we count down 25 we will be 1/4 further down the screen
        LDA     VideoBeam         * Get the current Videobeam address (either 0,56,112)
        ADDA    #56
        STA     VideoBeam         * Update it
        CMPA    #112
        BNE     FIRQ0Restore      * IF A is <>112 then Don't draw sprites on the top of the screen, just exit
        TST     ToggleTop         * If flag <> 0 then draw sprites on the top of the screen
        BEQ     FIRQ0Restore      * If flag = 0 then skip drawing sprites on the top of the screen so exit FIRQ

        LDA     FIRQ0Restore+1    * Restore entry value of A
        PSHS    CC,DP,A,B,X,Y,U   * Save the registers as the normal IRQ would have done (A was already saved - self modified code)
        ANDCC   #%10111111        * Clear the FIRQ flag so the FIRQ can happen while doing the Top sprites
        JSR     DrawTopSprites    * Draw the sprites that are on the screen from rows 0 to 95 (Top half of the screen)
;        CLR     IRQ_Flag          * We are done the IRQ so flag it as done
        PULS    CC,A,B,DP,X,Y,U   * Restore all the registers just like an regular RTI would do
        PULS    CC,PC             * Act like an RTI would if we exited the FIRQ
!
        BRA     FIRQ0Restore      * If we get here then exit the FIRQ like normal (couldn't do the Joust IRQ)

***********************************************************
Stack_Space         RMB $80        * Leave room for a our stack
Stack_Pointer       EQU START-1
START   LDS         #Stack_Pointer  * Start of program
*****************************************************
* Set the direct page to $F0
    LDA   #$F0
    TFR   A,DP
***********************************************************
System_EnableGraphics:
        ORCC    #$50                    * disable interrupts

        CLR     High_Speed_Mode         * Put CoCo 3 into highspeed mode

        LDD     #RealSampleStart
        STD     SamplePointer+1         * Setup the starting address for the audio sample

        LDA     #%00110100              * Setup the CoCo 3's hardware
        STA     PIA0_Byte_1_HSYNC       * HSYNC IRQ Disabled, IRQ Polarity Flag falling Edge, Data Direction Normal, Select Line LSB = 0, HSYNC Flag = 0
        STA     PIA0_Byte_3_VSYNC       * VSYNC IRQ Disabled, IRQ Polarity Flag falling Edge, Data Direction Normal, Select Line MSB = 0, VSYNC Flag = 0
        STA     PIA1_Byte_1_IRQ         * CONTROL OF CD FIRQ* TO CPU DISABLED, IRQ Polarity Falling Edge of CD, CD Flag off
        STA     PIA1_Byte_3_IRQ_Ct_Snd  * CONTROL OF Cart FIRQ* TO CPU DISABLED, IRQ Polarity Falling Edge of Cart, Cart Flag off

        LDA     #%01001100              * Diasble the Interrupts for now
        STA     INIT0_Register0         * CoCo 3 Mode, MMU Enabled, GIME IRQ Disabled, GIME FIRQ Disabled, Vector RAM at FEXX enabled, Standard SCS Normal, ROM Map 16k Int, 16k Ext
        LDA     #%00100000              *
        STA     INIT1_Register1         * Mem Type 64k chips, 279.365 nsec timer, MMU Task 0 - $FFA0-$FFA7
        LDA     #%10000000              *
        STA     Video_Mode_Register     * Graphics mode, Colour output, 60 hz, max vertical res

* Setup and enable the FIRQ
        LDA     #$7E                    * Write the JMP instruction if it's possbile to use Direct Page for the sample playback then use $0E = direct page JMP location, and 1 byte for address
        LDX     #FIRQ_Sound             * Enable FIRQ0 - do nothing
        STA     FIRQ_Jump_position
        STX     FIRQ_Start_Address      * FIRQ now set to playback no sound, this will be changed when an sound is played
* Setup and enable the IRQ
        LDA     #$7E                    * Write the JMP instruction
        LDX     #VSyncIRQ               *
        STA     IRQ_Jump_position       *
        STX     IRQ_Start_Address       * Point the IRQ to the VSyncIRQ address

        LDA     #%01111100              *
        STA     INIT0_Register0         * CoCo 3 Mode, MMU Enabled, GIME IRQ Enabled, GIME FIRQ Enabled, Vector RAM at FEXX enabled, Standard SCS Normal, ROM Map 16k Int, 16k Ext
        LDA     #%00001000              * $08
        STA     IRQENR                  * Enable only the Vertical Border Sync (VBORD) Interrupt
        LDD     #FIRQ_Countdown         * This is the speed the audio samples will playback at
        STD     $FF94                   * Set countdown Timer to $0254 - good for 6005.95 Hz samples
                                        * This frequency is tied to the FIRQ and scanline counter, it must be $0254 and samples must be 6005.95 Hz
        LDA     #%00100000              * $20
        STA     FIRQENR                 * Enable TIMER FIRQ Interrupt

* Configure Audio settings
*************************************
* Configure Audio settings
        LDA     PIA0_Byte_1_HSYNC       * SELECT SOUND OUT
        ANDA    #%11110111              * = $F7 - RESET LSB OF MUX BIT
        STA     PIA0_Byte_1_HSYNC       * STORE

        LDA     PIA0_Byte_3_VSYNC       * SELECT SOUND OUT
        ANDA    #%11110111              * = $F7 - RESET MSB OF MUX BIT
        STA     PIA0_Byte_3_VSYNC       * STORE

        LDA     PIA1_Byte_3_IRQ_Ct_Snd  * GET PIA
        ORA     #%00001000              * = $08 - SET 6-BIT SOUND ENABLE
        STA     PIA1_Byte_3_IRQ_Ct_Snd  * STORE

* This code masks off the two low bits written to $FF20
* So you can send the PCM Unsigned 8 Bit sample as is, no masking needed
* This will allow us to use the same PCM data for the Orchestra 90 and Coco Flash audio output cards and keep a little more fidelity
        LDA     PIA1_Byte_1_IRQ
        PSHS    A
        ANDA    #%00110011              * FORCE BIT2 LOW
        STA     PIA1_Byte_1_IRQ         * $FF20 NOW DATA DIRECTION REGISTER
        LDA     #%11111100              * OUTPUT ON DAC, INPUT ON RS-232 & CDI
        STA     PIA1_Byte_0_IRQ
        PULS    A
        STA     PIA1_Byte_1_IRQ
*************************************

***********************************************************
* Set Hires Screen Resolution and the number of Colours
*
* Bit Pattern   Rows Displayed
*    x00xxxxx   192
*    x01xxxxx   200
*    x10xxxxx   *zero/infinite lines on screen (undefined)
*    x11xxxxx   225
* Bit Pattern   Bytes/Row (Graphics)
*    xxx000xx   16
*    xxx001xx   20
*    xxx010xx   32
*    xxx011xx   40
*    xxx100xx   64
*    xxx101xx   80   320 4 Colours 01
*    xxx110xx   128
*    xxx111xx   160
* Bit Pattern   Colours     Pixels/Byte
*    xxxxxx00   2           8
*    xxxxxx01   4           4
*    xxxxxxl0   16          2
*    xxxxxx11   Undefined   Undefined
***********************************************************
* Most Common used settings (Uncomment the one you want to use)
*       LDA     #%00001000            * 256 x 192 x 2 Colours  requires 6,144  bytes = $1800 RAM
*       LDA     #%00101000            * 256 x 200 x 2 Colours  requires 6,400  bytes = $1900 RAM
*       LDA     #%01101000            * 256 x 225 x 2 Colours  requires 7,200  bytes = $1C20 RAM
*       LDA     #%00010001            * 256 x 192 x 4 Colours  requires 12,288 bytes = $3000 RAM
*       LDA     #%00110001            * 256 x 200 x 4 Colours  requires 12,800 bytes = $3200 RAM
*       LDA     #%01110001            * 256 x 225 x 4 Colours  requires 14,400 bytes = $3840 RAM
*       LDA     #%00011010            * 256 x 192 x 16 Colours requires 24,576 bytes = $6000 RAM
*       LDA     #%00111010            * 256 x 200 x 16 Colours requires 25,600 bytes = $6400 RAM
       LDA     #%01111010            * 256 x 225 x 16 Colours requires 28,800 bytes = $7080 RAM
*       LDA     #%00001100            * 320 x 192 x 2 Colours  requires 7,680  bytes = $1E00 RAM
*       LDA     #%00101100            * 320 x 200 x 2 Colours  requires 8,000  bytes = $1F40 RAM
*       LDA     #%01101100            * 320 x 225 x 2 Colours  requires 10,240 bytes = $2800 RAM
*       LDA     #%00010101            * 320 x 192 x 4 Colours  requires 15,360 bytes = $3C00 RAM
*       LDA     #%00110101            * 320 x 200 x 4 Colours  requires 16,000 bytes = $3E80 RAM
*       LDA     #%01110101            * 320 x 225 x 4 Colours  requires 18,000 bytes = $4650 RAM
*       LDA     #%00011110            * 320 x 192 x 16 Colours requires 30,720 bytes = $7800 RAM
*       LDA     #%00111110            * 320 x 200 x 16 Colours requires 32,00  bytes = $7D00 RAM
*       LDA     #%01111110            * 320 x 225 x 16 Colours requires 36,000 bytes = $8CA0 RAM
*       LDA     #%00010100            * 640 x 192 x 2 Colours  requires 15,360 bytes = $3C00 RAM
*       LDA     #%00110100            * 640 x 200 x 2 Colours  requires 16,000 bytes = $3E80 RAM
*       LDA     #%01110100            * 640 x 225 x 2 Colours  requires 18,000 bytes = $4650 RAM
*       LDA     #%00011101            * 640 x 192 x 4 Colours  requires 30,720 bytes = $7800 RAM
*       LDA     #%00111101            * 640 x 200 x 4 Colours  requires 32,000 bytes = $7D00 RAM
*       LDA     #%01111101            * 640 x 225 x 4 Colours  requires 36,000 bytes = $8CA0 RAM

        STA     Vid_Res_Reg

***********************************************************
* Border settings
        LDA     #Border_Colour          *
        STA     Border_Register         *

        LDD     #$E1F0                  * Point video screen veiwer at $70F80 (Hi-Res page start)
        STD     VidStart                * which is at $70F80 / 8 = $E1F0

* Load GIME pages $30-34 into CPU RAM $0000-$7FFF  (some HIRES screens are huge > $8000)
SetGraphicsMem:
        LDA     #$38                    * Start at Hi-Res page #0 (default page)
        LDX     #MMU_Reg_Bank0_0        * mapped to $0000-$7FFF, then more
        LDB     #4                      * 4 pages to load
!       STA     ,X+
        INCA                            * next page
        DECB
        BNE     <

SetAudioSampleBlocks:
        LDA     #$30                    * Start of audio sample pages
        LDX     #MMU_Reg_Bank1_0        * mapped to $0000-$7FFF, then more in bank 1
        LDB     #4                      * 4 pages to load
!       STA     ,X+
        INCA                            * next page
        DECB
        BNE     <

* Set the Palette registers
        LDX   #CoCoPal1
        LDU   #Palette_Start
!       LDA   ,X+
        STA   ,U+
        CMPX    #CoCoPal1+16
        BNE     <

***********************************************************
* Our regular 64k RAM now looks like this
* 0000-01FF - Star Field locations (2 bytes for each star)
* 0200-0F7F - Code and Variables
* 0F80-7FFF - Graphics RAM
***********************************************************

* Show some stuff on the screen (for a test)
        LDX     #Screen_Location        * screen start
        LDA     #$00                     * Colour 00
horiz    LDY     #1920                   * bytes per 1/16 of screen
!       STA     ,X+
        LEAY    -1,Y                    * decrement counter
        BNE     <
        ADDA    #$11                    * increment both pixel Colours
        CMPX    #$8000                  * end of graphics
        BNE     horiz

        LBSR     Inkey           * wait for space key press

        JSR     CLS

*****************************
* Get random numbers for the star locations
* need locations from $0F80 to $7FFF
* Array of star locations is at
* 0000-03FF - Star Field locations (2 bytes each)
**********************************************************

        LDX     #Stars
!
        JSR     RAND            * Go get Random # between 0 and 255, return it in A
        TFR     A,B             * Move RND # to B
        JSR     RAND            * Go get Random # between 0 and 255, return it in A
        CMPD    #$7080
        BHS     <               * If star location is outside of the range of the screen then get another random position
        ADDD    #$0F80          * make it from #$0F80 to $7FFF
        STD     ,X++            * store random star in array and move to the next position
        JSR     RAND            * Go get Random # between 0 and 255, return it in A
        ANDA    #%00000011      * Random star movement speed 0 to 3
        INCA                    * Make it 1 to 4
        TFR     A,B             * Move value to B
        JSR     RAND            * Go get Random # between 0 and 255, return it in A
        INCA                    * Make it non zero
        BITA    #%01000000      * if this bit is set then let's use the left nibble
        BNE     LeftNibble      * Let's use the left nibble
        ANDA    #%00001111      * Random star color 0 to 15
        BRA     Skipahead
LeftNibble:
        LSLA
        LSLA
        LSLA
        LSLA                    * move right nibble to the left
Skipahead:
        STD     ,X++            * Add the info to the array
        CMPX    #StarsEnd+1     * Done yet?
        BNE     <               * If not do it again

* You can edit the value of the two lines below
NumberOfSprites   EQU     10    * Number of sprites (max 12)
MovementPostions  EQU     12     * Number of movement positions between sprites
;--------------------------------------------------------------------------------
CheckedAllSprites EQU     NumberOfSprites*2   * Have we checked all the sprites?


* Setup Sprite location reading
        LDX     #SpritePos      * Start of Sprite x & Y co-ordinates table
        LDB     #MovementPostions * Number of movement positions between sprites         ***
        LSLB                    * Two bytes per entry
        STX     SpritePointer1
        ABX
        STX     SpritePointer2
        ABX
        STX     SpritePointer3
        ABX
        STX     SpritePointer4
        ABX
        STX     SpritePointer5
        ABX
        STX     SpritePointer6
        ABX
        STX     SpritePointer7
        ABX
        STX     SpritePointer8
        ABX
        STX     SpritePointer9
        ABX
        STX     SpritePointer10
        ABX
        STX     SpritePointer11
        ABX
        STX     SpritePointer12

* Enable both the FIRQ and IRQ Interupts
                                        *                    EFHI NZVC
        ANDCC    #$AF                   * Enable interrupts %1010 1111

Mainloop:
        LDY     #$0600
Cloop:  LEAY    -1,Y
        BNE     Cloop                 * 8 cycles * $800 = 8 * 2048 = 16,384 cycles for game code  (Average 5 cycles per instruction = 3,376 instructions)
        JSR     MoveSprites
        LDA     TopNumSprites
        ADDA    BotNumSprites
        BNE     Mainloop              * If there are still sprites to draw then ignore new sprite locations until (F)IRQs have moved all the old sprites so go move new sprites
        CLR     ToggleTop             * Flag to not draw sprites
        CLR     ToggleBot             * Flag to not draw sprites
        CLR     TopNumSpritesArrange
        CLR     BotNumSpritesArrange
        LDY     #Sprite1InRAM1
ArrangeSprites:
        LDA     ,Y                      * Get the Row down the screen
        CMPA    #$47                    * See if we are in the middle of the screen
        BHS     >                       * If we are in the bottom half then jump ahead
        INC     TopNumSpritesArrange           * Increment Top elements counter
        LDB     TopNumSpritesArrange           * B=# of sprites in the Top
        LSLB                            * B=B*2 so it points to the correct 16 bit offset
        LSLB                            * B=B*4 so it points to the correct 16 bit offset
        LDU     ,Y++                    * U = address of the sprite in the top of the screen, move Y pointer 2 bytes
        LDX     #TopSpriteAddresses-4   * X=table start -4 because the first one will = 4
ArrangeMore:
        ABX
        STU     ,X++                    * Save address of sprite in the table
        LEAU    22,Y     * Get the old sprite location so it can be erased   ***
        STU     ,X                      * Save the erase sprite extended address info at the proper location
        CMPY    #Sprite1InRAM1+CheckedAllSprites      * Have we checked all the sprites?                  ***
        BNE     ArrangeSprites          * Loop again if not
        LDA     TopNumSpritesArrange
        STA     TopNumSprites
        LDA     BotNumSpritesArrange
        STA     BotNumSprites
        COM     ToggleTop               * Flag <> 0 we can draw more sprites
        COM     ToggleBot               * Flag <> 0 we can draw more sprites
        BRA     Mainloop                * Jump back to the main loop
!
        INC     BotNumSpritesArrange           * Increment Bottom elements counter
        LDB     BotNumSpritesArrange           * B=# of sprites in the Bottom
        LSLB                            * B=B*2 so it points to the correct 16 bit offset
        LSLB                            * B=B*4 so it points to the correct 16 bit offset
        LDU     ,Y++                    * U = address of the sprite in the top of the screen, move Y pointer 2 bytes
        LDX     #BotSpriteAddresses-4   * X=table start -4 because the first one will = 4
        BRA     ArrangeMore

FakeZero  FCB (112*128+$F80)/256
TablePos  FCB 0
SpritePointer1   RMB   2        * Table of pointers for each sprite in the location table
SpritePointer2   RMB   2
SpritePointer3   RMB   2
SpritePointer4   RMB   2
SpritePointer5   RMB   2
SpritePointer6   RMB   2
SpritePointer7   RMB   2
SpritePointer8   RMB   2
SpritePointer9   RMB   2
SpritePointer10   RMB   2
SpritePointer11   RMB   2
SpritePointer12   RMB   2

Sprite1InRAM1    FDB   $1000
Sprite1InRAM2    FDB   $1000
Sprite1InRAM3    FDB   $1000
Sprite1InRAM4    FDB   $1000
Sprite1InRAM5    FDB   $1000
Sprite1InRAM6    FDB   $1000
Sprite1InRAM7    FDB   $1000
Sprite1InRAM8    FDB   $1000
Sprite1InRAM9    FDB   $1000
Sprite1InRAM10    FDB   $1000
Sprite1InRAM11    FDB   $1000
Sprite1InRAM12    FDB   $1000

Old_SpriteLocation1   FDB   $1000
Old_SpriteLocation2   FDB   $1000
Old_SpriteLocation3   FDB   $1000
Old_SpriteLocation4   FDB   $1000
Old_SpriteLocation5   FDB   $1000
Old_SpriteLocation6   FDB   $1000
Old_SpriteLocation7   FDB   $1000
Old_SpriteLocation8   FDB   $1000
Old_SpriteLocation9   FDB   $1000
Old_SpriteLocation10   FDB   $1000
Old_SpriteLocation11   FDB   $1000
Old_SpriteLocation12   FDB   $1000

MoveSprites:
        LDU   #SpritePointer1
MoveSpritesLoop:
        LDX   ,U
        LDD   ,X++            * A = x co-ordinate, B = y co-ordinate
        STX   ,U
        CMPX  #SpritePosEnd   * Test if we are at the end of the Table
        BNE   >
        LDX   #SpritePos      * Start of Sprite x & Y co-ordinates
        STX   ,U
!
        LSRA                  * two pixels per byte on screen
        STA   SelfModAdd+2    * Self modify add below
        LDA   #128            * 128 bytes per row
        MUL                   * D now has the correct row location
        ADDD  #$0F80          * Add the screen start location
SelfModAdd:
        ADDD  #$0000          * Add self mod value for the column of the sprite
        STD   24,U * We now have the new address of the sprite to be drawn in RAM   ***
        LEAU  2,U
        CMPU  #SpritePointer1+NumberOfSprites*2                                     ***
        BNE   MoveSpritesLoop
        RTS

* Screen RAM is from $0FB0 to $7FFF  = 256 pixel wide screen = 128 bytes across
* So $7F-$0F = $7000 bytes for screen (approx) so divide $7000 by each row is 128 bytes = 224 or $E0 rows
* take that and divide it by 4 and we get 56 which is the number of rows we need to keep track of for each quarter of the screen
* We need to add $0f or 15 rows as the starting address so the video beam number for easy/fast comparing need to be:
* $0F = 15 (top), $47 = 71 (1/4 of screen), $7F = 127 (middle of the screen), $B7 = 183 (3/4 down the screen)
Old_SpriteLocation   FDB   64+112*128       * Location where last sprite was drawn in RAM - use this to erase old sprite
Zero                 FCB   $00          * Always a zero to be added to for the X location
New_SpriteLocationX  FDB   64*256       * Start near the center of the screen
New_SpriteLocationY  FDB   0*256      * Start near the center of the screen
New_SpriteInRAM      FDB   64+112*128   * New RAM location to be drawn on screen
MoveX                FDB   $0070
MoveY                FDB   $0070
SpriteDrawnFlag      FCB   $00          * Flag if sprite has been drawn this frame
* We are in the IRQ at this point
* VideoBeam is either 0,56,112 or 168
*********************************************
* Arrange sprite tables that has the # of sprites in the top half of the screen and their address
* Also do the same for the sprites in the bottom half of the screen
*
TopNumSpritesArrange  FCB   0     * Number of sprites on the top half of the screen
TopNumSprites         FCB   0     * Number of sprites on the top half of the screen
TopSpriteAddresses    FDB   $1000 * Six sprites * 2 byte address each for current address * 2 byte adress for current Old Sprite Location
                      FDB   $1000 * 1
                      FDB   $1000
                      FDB   $1000 * 2
                      FDB   $1000
                      FDB   $1000 * 3
                      FDB   $1000
                      FDB   $1000 * 4
                      FDB   $1000
                      FDB   $1000 * 5
                      FDB   $1000
                      FDB   $1000 * 6
                      FDB   $1000
                      FDB   $1000 * 7
                      FDB   $1000
                      FDB   $1000 * 8
                      FDB   $1000
                      FDB   $1000 * 9
                      FDB   $1000
                      FDB   $1000 * 10
                      FDB   $1000
                      FDB   $1000 * 11
                      FDB   $1000
                      FDB   $1000 * 12
BotNumSpritesArrange  FCB   0     * Number of sprites on the bottom half of the screen
BotNumSprites         FCB   0     * Number of sprites on the bottom half of the screen
BotSpriteAddresses    FDB   $1000 * Six sprites * 2 byte address each for current address * 2 byte adress for current Old Sprite Location
                      FDB   $1000 * 1   Extended address of sprite location to be erased
                      FDB   $1000
                      FDB   $1000 * 2
                      FDB   $1000
                      FDB   $1000 * 3
                      FDB   $1000
                      FDB   $1000 * 4
                      FDB   $1000
                      FDB   $1000 * 5
                      FDB   $1000
                      FDB   $1000 * 6
                      FDB   $1000
                      FDB   $1000 * 7
                      FDB   $1000
                      FDB   $1000 * 8
                      FDB   $1000
                      FDB   $1000 * 9
                      FDB   $1000
                      FDB   $1000 * 10
                      FDB   $1000
                      FDB   $1000 * 11
                      FDB   $1000
                      FDB   $1000 * 12

DrawBottomSprites:
* Draw the stars first
        TST   ToggleStars         * Check flag of VSYNC IRQ, if zero then it just triggered and we can update the stars
        BNE   >                   * Otherwise skip drawing the stars
        JSR   MoveStarsDown       * Move the stars
        LDD   #StarsDelay
        STD   StarsCounter
        COM   ToggleStars
!
        TST   BotNumSprites       * Get the number of sprites to draw on the bottom half of the screen
        BEQ   SkipDraw            * Skip drawing if none of the sprites are on the bottom of the screen
        LDX   #BotSpriteAddresses
!
        LDU   [2,X]               * The location in RAM where the sprite was previously drawn
        BSR   EraseOldSprite      * Go erase old sprite
* Draw new sprite
        LDU   ,X                  * Get address of sprite
        STU   [2,X]               * save address of new sprite so it can be erased
        LEAX  4,X                 * move forward
        PSHS  X
        JSR   _000061.bmp         * Go Draw a sprite @ U
        PULS  X
        DEC   BotNumSprites       * Decrement the # of sprites to draw
        BNE   <                   * Go draw another if not zero
SkipDraw:
        RTS
DrawTopSprites:
* Draw the stars first
        TST   TopNumSprites       * Get the number of sprites to draw on the bottom half of the screen
        BEQ   SkipDraw            * Skip drawing if none of the sprites are on the bottom of the screen
        LDX   #TopSpriteAddresses
!
        LDU   [2,X]               * The location in RAM where the sprite was previously drawn
        BSR   EraseOldSprite      * Go erase old sprite
* Draw new sprite
        LDU   ,X                  * Get address of sprite
        STU   [2,X]               * save address of new sprite so it can be erased
        LEAX  4,X                 * move forward
        PSHS  X
        JSR   _000061.bmp         * Go Draw a sprite @ U
        PULS  X
        DEC   TopNumSprites       * Decrement the # of sprites to draw
        BNE   <                   * Go draw another if not zero
        RTS

EraseOldSprite:
* Erase the old sprite
**************************************************
* 00 00 00 FF FF FF F0 00 00 00 - 1
* 00 00 FF FF FF FF FF F0 00 00 - 2
* 00 0F EE FF FF FF FF FF 00 00 - 3
* 00 FE 99 EF FF FF FF FF F0 00 - 4
* 00 FE AA EF FF FF FF FF F0 00 - 5
* 0F FE 99 EF FF FF FF FF FF 00 - 6
* 0F FF FF FF FF FF FF FF EF 00 - 7
* FF FF FF FF FF FF FF FE EF F0 - 8
* FF FF FF FF FF FF FF FE EF F0 - 9
* FF FF FF FF FF FF FF EE EF F0 - 10
* FF FF FF FF FF FF FE EE EF F0 - 11
* 0F FF FF FF FF FF EE DE EF 00 - 12
* 0F FF FF FF FF FE ED EE FF 00 - 13
* 00 FF FF FF FF EE DE EE F0 00 - 14
* 00 FF FF FF EE EE EE EF F0 00 - 15
* 00 0F FF EE EE EE EE FF 00 00 - 16
* 00 00 FF FE EE EF FF F0 00 00 - 17
* 00 00 00 0F FF FF 00 00 00 00 - 18
**************************************************

        PSHS  X                   * Save X
        LEAU  128*17+6,U         * Move the U pointer to the bottom right of the location (width is 20 pixels, height is 18 pixels)
        LDX   #$0000
        LDD   #$0000
        LEAY  ,X
        PSHU  A,X     * Row 18
        LEAU  -128+5,U
        PSHU  D,X,Y   * Row 17
        LEAU  -128+6,U
        PSHU  D,X,Y
        STA   -1,U    * Row 16
        LEAU  -128+7,U
        PSHU  D,X
        PSHU  X,Y     * Row 15
        LEAU  -128+8,U
        PSHU  D,X
        PSHU  X,Y     * Row 14
        LEAU  -128+8,U
        PSHU  D,X,Y
        PSHU  A,X     * Row 13
        LEAU  -128+9,U
        PSHU  D,X,Y
        PSHU  A,X     * Row 12
        LEAU  -128+10,U
        PSHU  D,X,Y
        PSHU  D,X     * Row 11
        LEAU  -128+10,U
        PSHU  D,X,Y
        PSHU  D,X     * Row 10
        LEAU  -128+10,U
        PSHU  D,X,Y
        PSHU  D,X     * Row 9
        LEAU  -128+10,U
        PSHU  D,X,Y
        PSHU  D,X     * Row 8
        LEAU  -128+9,U
        PSHU  D,X,Y
        PSHU  A,X     *Row 7
        LEAU  -128+9,U
        PSHU  D,X,Y
        PSHU  A,X     * Row 6
        LEAU  -128+9,U
        PSHU  D,X
        PSHU  X,Y     * Row 5
        LEAU  -128+8,U
        PSHU  D,X
        PSHU  X,Y     * Row 4
        LEAU  -128+7,U
        PSHU  D,X
        PSHU  A,Y     * Row 3
        LEAU  -128+7,U
        PSHU  D,X,Y   * Row 2
        LEAU  -128+5,U
        PSHU  D,X     * Row 1
        PULS  X,PC

CoCoPal1:
        FCB     $00         ; Black
        FCB     %00000111   ; Dark Grey
        FCB     %00000111   ; Dark Grey
        FCB     %00000111   ; Dark Grey

        FCB     %00111000   ; Light Grey
        FCB     %00111000   ; Light Grey
        FCB     %00111000   ; Light Grey
        FCB     %00000111   ; Dark Grey

        FCB     %00111000   ; Light Grey
        FCB     %00110100   ; Orange
        FCB     %00110110   ; Yellow
        FCB     %00001000   ; Medium Blue

        FCB     %00001001   ; Blue
        FCB     %00100100   ; Red
        FCB     %00100000   ; Medium Red
        FCB     %00000100   ; Dark Red

        opt     c,ct,cc       * show cycle count, add the counts, clear the current count

MoveStarsDown:
* Scroll the stars down the screen method 2
;        LDA     #$F0            * A is the star value
Again:
        LDX     #$0000          * X points to the start of our starfield address array
StarsLoop:
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X++]            * Erase old star on screen
        LDD     ,X++             * A=Colour of star B = the amount to move
        ADDB    -4,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     -4,X              * Save new MSB of address
        STA     [-4,X]
        CLR     [,X]            * Erase old star on screen
        LDD     2,X             * A=Colour of star B = the amount to move
        ADDB    ,X              * B=B+MSB of stars current address (move it down the screen)
        BPL     >               * If MSB <= $7F skip ahead
        SUBB     #$70           * Otherwise make B = MSB of star is $00 (top of the screen)
!       STB     ,X              * Save new MSB of address
        STA     [,X]

        RTS

***********************************************************
* Keyboard Reading is done by sending a masked byte based on the on the top row below on $FF02 and tested
* against the bits on the left marked Row.  Match up the grid.
* For example to test if the letter 'T' is being pressed
* you would store the byte %11101111 (bit 4 from the data row) in $FF02
* and load the byte from $FF00 and test it against %00000100 (bit 2 from the Row) if it's equal then the key was pressed
*
*Row                Data
*bits   7    6    5    4    3    2    1    0
*0         G    F    E    D    C    B    A    @
*1         O    N    M    L    K    J    I    H
*2         W    V    U    T    S    R    Q    P
*3         SP    RGT    LFT    DN    UP    Z    Y    X
*4         '    &    %    $    #    "    !    0
*4      7    6    5    4    3    2    1
*5         ?    >    =    <    +    *    )    (
*5      /    .    _    ,    ;    :    9    8
*6     shifts                      BRK CLR ENT
***********************************************************
Inkey:
        PSHS    A,X
        LDX     #$FF00                  *
        LDB     #$00                    *
Z5084
        INC     RND+1                   * change the Random seed LSB
        BNE     >
        INC     RND                     * change the Random seed MSB
!
        LDA     #%01111111              * bit 7
        STA     $02,X                   *
        LDA     ,X                      *
        BITA    #%00001000              * and bit 3 = spacebar
        BEQ     Z509A                   *
        LDA     #%11110111              * bit 3
        STA     $02,X                   *
        LDA     ,X                      *
        BITA    #%00000001              * and bit 0 = 'C'
        BNE     Z5084                   *
Z509A
        LDX     RND
        BNE     >                   * Make sure the seed is not zero
        LDX     #$3487              * otherwise make it #$3487
        STX     RND
!
        PULS    A,X,PC

***********************************************************
* Clear the screen palette 00
***********************************************************
CLS:
        PSHS    D,X,Y,U
        STS     CLS_Restore+2
        LDS     #$8000                  * End of screen RAM+1
        LDX     #$0000
        LEAY    ,X
        LDU     #$0000
        LDD     #$0000
!       PSHS    D,X,Y,U
        CMPS    #$0F80
        BNE     <
CLS_Restore:
        LDS     #CLS_Restore
        PULS    D,X,Y,U,PC

* Get a random number in A from 0 to 255
RAND
        LDA   RND
        LSRA
        ROL   RND+1
        BCC   NoEOR
        EORA  #$B4
NoEOR:
        STA   RND
        EORA  RND+1
        RTS


CosineTable:
        FCB    -127,-127,-127,-127,-127,-126,-126,-125
        FCB    -125,-124,-123,-122,-122,-121,-119,-118
        FCB    -117,-116,-115,-113,-112,-110,-109,-107
        FCB    -105,-103,-101,-99,-97,-95,-93,-91
        FCB    -89,-87,-84,-82,-80,-77,-75,-72
        FCB    -69,-67,-64,-61,-59,-56,-53,-50
        FCB    -47,-44,-41,-38,-35,-32,-29,-26
        FCB    -23,-20,-17,-14,-11,-8,-5,-1

SineTable:
        FCB     0,3,6,9,12,15,18,21
        FCB     24,27,30,33,36,39,42,45
        FCB     48,51,54,57,60,62,65,68
        FCB     70,73,75,78,80,83,85,87
        FCB     89,92,94,96,98,100,102,104
        FCB     105,107,109,110,112,113,114,116
        FCB     117,118,119,120,121,122,123,124
        FCB     124,125,125,126,126,126,126,126

        FCB     126,126,126,126,126,125,125,125
        FCB     124,123,123,122,121,120,119,118
        FCB     117,115,114,113,111,110,108,106
        FCB     105,103,101,99,97,95,93,91
        FCB     89,86,84,82,79,77,74,72
        FCB     69,67,64,61,59,56,53,50
        FCB     47,44,41,38,35,32,29,26
        FCB     23,20,17,14,11,8,5,2

        FCB    -2,-5,-8,-11,-14,-17,-20,-23
        FCB    -26,-29,-32,-35,-38,-41,-44,-47
        FCB    -50,-53,-56,-59,-61,-64,-67,-70
        FCB    -72,-75,-77,-80,-82,-84,-87,-89
        FCB    -91,-93,-96,-98,-100,-101,-103,-105
        FCB    -107,-109,-110,-112,-113,-115,-116,-117
        FCB    -118,-120,-121,-122,-122,-123,-124,-125
        FCB    -125,-126,-126,-127,-127,-127,-127,-127
* Cosine Table ends here...

        FCB    -127,-127,-127,-127,-127,-126,-126,-125
        FCB    -125,-124,-123,-122,-122,-121,-119,-118
        FCB    -117,-116,-115,-113,-112,-110,-109,-107
        FCB    -105,-103,-101,-99,-97,-95,-93,-91
        FCB    -89,-87,-84,-82,-80,-77,-75,-72
        FCB    -69,-67,-64,-61,-59,-56,-53,-50
        FCB    -47,-44,-41,-38,-35,-32,-29,-26
        FCB    -23,-20,-17,-14,-11,-8,-5,-1
* Sine Table ends here...



Final    END     START                   * set exec address

* end of file -
