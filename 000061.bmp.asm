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
_000061.bmp:
        opt     cd
        opt     cc
* Row 18
        LEAU    128*17+6,U    * A=, B=, X=, Y=
* Row 18 800
* 00 00 00 0F FF FF 00 00 00 00
        LDB     -3,U            * A=, B=, X=, Y=
        ORB     #$0F            * A=, B=, X=, Y=
        STB     -3,U            * A=, B=XX, X=, Y=
        LDD     #$FFFF          * A=, B=XX, X=, Y=
        STD     -2,U            * A=FF, B=FF, X=, Y=
* Row 17 800
* 00 00 FF FE EE EF FF F0 00 00
        LEAU    -128*1+1,U      * A=FF, B=FF, X=, Y=
        LDA     ,U              * A=FF, B=FF, X=, Y=
        ORA     #$F0            * A=FF, B=FF, X=, Y=
        STA     ,U              * A=XX, B=FF, X=, Y=
        LDD     #$FFFE          * A=XX, B=FF, X=, Y=
        STD     -5,U            * A=FF, B=FE, X=, Y=
        STA     -1,U            * A=FF, B=FE, X=, Y=
        LDD     #$EEEF          * A=FF, B=FE, X=, Y=
        STD     -3,U            * A=EE, B=EF, X=, Y=
* Row 16 800
* 00 0F FF EE EE EE EE FF 00 00
        LEAU    -128*1+1,U      * A=EE, B=EF, X=, Y=
        LDB     -7,U            * A=EE, B=EF, X=, Y=
        ORB     #$0F            * A=EE, B=EF, X=, Y=
        STB     -7,U            * A=EE, B=XX, X=, Y=
        LDD     #$FFEE          * A=FF, B=EE, X=, Y=
        LDX     #$EEEE          * A=FF, B=EE, X=EEEE, Y=
        LDY     #$EEFF          * A=FF, B=EE, X=EEEE, Y=EEFF
        PSHU    D,X,Y           * A=FF, B=EE, X=EEEE, Y=EEFF
* Row 15 800
* 00 FF FF FF EE EE EE EF F0 00
        LEAU    -128*1+6,U      * A=FF, B=EE, X=EEEE, Y=EEFF
        LDA     ,U              * A=FF, B=EE, X=EEEE, Y=EEFF
        ORA     #$F0            * A=FF, B=EE, X=EEEE, Y=EEFF
        STA     ,U              * A=XX, B=EE, X=EEEE, Y=EEFF
        STX     -4,U            * A=XX, B=EE, X=EEEE, Y=EEFF
        LDD     #$FFFF          * A=XX, B=EE, X=EEEE, Y=EEFF
        STD     -7,U            * A=FF, B=FF, X=EEEE, Y=EEFF
        STA     -5,U            * A=FF, B=FF, X=EEEE, Y=EEFF
        LDD     #$EEEF          * A=FF, B=FF, X=EEEE, Y=EEFF
        STD     -2,U            * A=EE, B=EF, X=EEEE, Y=EEFF
* Row 14 800
* 00 FF FF FF FF EE DE EE F0 00
        LEAU    -128*1,U        * A=EE, B=EF, X=EEEE, Y=EEFF
        LDA     ,U              * A=EE, B=EF, X=EEEE, Y=EEFF
        ORA     #$F0            * A=EE, B=EF, X=EEEE, Y=EEFF
        STA     ,U              * A=XX, B=EF, X=EEEE, Y=EEFF
        LDD     #$FFFF          * A=FF, B=FF, X=EEEE, Y=EEFF
        LDX     #$FFEE          * A=FF, B=FF, X=FFEE, Y=EEFF
        LDY     #$DEEE          * A=FF, B=FF, X=FFEE, Y=DEEE
        PSHU    D,X,Y           * A=FF, B=FF, X=FFEE, Y=DEEE
        LDB     #$FF            * A=FF, B=FF, X=FFEE, Y=DEEE
        STB     -1,U            * A=FF, B=FF, X=FFEE, Y=DEEE
* Row 13 800
* 0F FF FF FF FF FE ED EE FF 00
        LEAU    -128*1+7,U      * A=FF, B=FF, X=FFEE, Y=DEEE
        LDB     -9,U            * A=FF, B=FF, X=FFEE, Y=DEEE
        ORB     #$0F            * A=FF, B=FF, X=FFEE, Y=DEEE
        STB     -9,U            * A=FF, B=XX, X=FFEE, Y=DEEE
        LDB     #$FF            * A=FF, B=FF, X=FFEE, Y=DEEE
        LDX     #$FEED          * A=FF, B=FF, X=FEED, Y=DEEE
        LDY     #$EEFF          * A=FF, B=FF, X=FEED, Y=EEFF
        PSHU    D,X,Y           * A=FF, B=FF, X=FEED, Y=EEFF
        LDB     #$FF            * A=FF, B=FF, X=FEED, Y=EEFF
        LDA     #$FF            * A=FF, B=FF, X=FEED, Y=EEFF
        STD     -2,U            * A=FF, B=FF, X=FEED, Y=EEFF
* Row 12 800
* 0F FF FF FF FF FF EE DE EF 00
        LEAU    -128*1+6,U      * A=FF, B=FF, X=FEED, Y=EEFF
        LDB     -9,U            * A=FF, B=FF, X=FEED, Y=EEFF
        ORB     #$0F            * A=FF, B=FF, X=FEED, Y=EEFF
        STB     -9,U            * A=FF, B=XX, X=FEED, Y=EEFF
        LDB     #$FF            * A=FF, B=FF, X=FEED, Y=EEFF
        LDX     #$FFEE          * A=FF, B=FF, X=FFEE, Y=EEFF
        LDY     #$DEEF          * A=FF, B=FF, X=FFEE, Y=DEEF
        PSHU    D,X,Y           * A=FF, B=FF, X=FFEE, Y=DEEF
        LDB     #$FF            * A=FF, B=FF, X=FFEE, Y=DEEF
        LDA     #$FF            * A=FF, B=FF, X=FFEE, Y=DEEF
        STD     -2,U            * A=FF, B=FF, X=FFEE, Y=DEEF
* Row 11 800
* FF FF FF FF FF FF FE EE EF F0
        LEAU    -128*1+6,U      * A=FF, B=FF, X=FFEE, Y=DEEF
        LDA     ,U              * A=FF, B=FF, X=FFEE, Y=DEEF
        ORA     #$F0            * A=FF, B=FF, X=FFEE, Y=DEEF
        STA     ,U              * A=XX, B=FF, X=FFEE, Y=DEEF
        LDA     #$FF            * A=FF, B=FF, X=FFEE, Y=DEEF
        LDX     #$FFFE          * A=FF, B=FF, X=FFFE, Y=DEEF
        LDY     #$EEEF          * A=FF, B=FF, X=FFFE, Y=EEEF
        PSHU    D,X,Y           * A=FF, B=FF, X=FFFE, Y=EEEF
        LDB     #$FF            * A=FF, B=FF, X=FFFE, Y=EEEF
        LDA     #$FF            * A=FF, B=FF, X=FFFE, Y=EEEF
        STD     -3,U            * A=FF, B=FF, X=FFFE, Y=EEEF
        STA     -1,U            * A=FF, B=FF, X=FFFE, Y=EEEF
* Row 10 800
* FF FF FF FF FF FF FF EE EF F0
        LEAU    -128*1+6,U      * A=FF, B=FF, X=FFFE, Y=EEEF
        LDA     ,U              * A=FF, B=FF, X=FFFE, Y=EEEF
        ORA     #$F0            * A=FF, B=FF, X=FFFE, Y=EEEF
        STA     ,U              * A=XX, B=FF, X=FFFE, Y=EEEF
        PSHU    B,Y             * A=XX, B=FF, X=FFFE, Y=EEEF
        LDA     #$FF            * A=FF, B=FF, X=FFFE, Y=EEEF
        LDX     #$FFFF          * A=FF, B=FF, X=FFFF, Y=EEEF
        LEAY    ,X              * A=FF, B=FF, X=FFFF, Y=FFFF
        PSHU    D,X,Y           * A=FF, B=FF, X=FFFF, Y=FFFF
* Row 9 800
* FF FF FF FF FF FF FF FE EF F0
        LEAU    -128*1+9,U      * A=FF, B=FF, X=FFFF, Y=FFFF
        LDA     ,U              * A=FF, B=FF, X=FFFF, Y=FFFF
        ORA     #$F0            * A=FF, B=FF, X=FFFF, Y=FFFF
        STA     ,U              * A=XX, B=FF, X=FFFF, Y=FFFF
        STX     -9,U            * A=XX, B=FF, X=FFFF, Y=FFFF
        STX     -7,U            * A=XX, B=FF, X=FFFF, Y=FFFF
        STX     -5,U            * A=XX, B=FF, X=FFFF, Y=FFFF
        LDD     #$FFFE          * A=XX, B=FF, X=FFFF, Y=FFFF
        STD     -3,U            * A=FF, B=FE, X=FFFF, Y=FFFF
        LDB     #$EF            * A=FF, B=FE, X=FFFF, Y=FFFF
        STB     -1,U            * A=FF, B=EF, X=FFFF, Y=FFFF
* Row 8 800
* FF FF FF FF FF FF FF FE EF F0
        LEAU    -128*1,U        * A=FF, B=EF, X=FFFF, Y=FFFF
        LDA     ,U              * A=FF, B=EF, X=FFFF, Y=FFFF
        ORA     #$F0            * A=FF, B=EF, X=FFFF, Y=FFFF
        STA     ,U              * A=XX, B=EF, X=FFFF, Y=FFFF
        STX     -9,U            * A=XX, B=EF, X=FFFF, Y=FFFF
        STX     -7,U            * A=XX, B=EF, X=FFFF, Y=FFFF
        STX     -5,U            * A=XX, B=EF, X=FFFF, Y=FFFF
        LDD     #$FFFE          * A=XX, B=EF, X=FFFF, Y=FFFF
        STD     -3,U            * A=FF, B=FE, X=FFFF, Y=FFFF
        LDB     #$EF            * A=FF, B=FE, X=FFFF, Y=FFFF
        STB     -1,U            * A=FF, B=EF, X=FFFF, Y=FFFF
* Row 7 800
* 0F FF FF FF FF FF FF FF EF 00
        LEAU    -128*1,U        * A=FF, B=EF, X=FFFF, Y=FFFF
        LDB     -9,U            * A=FF, B=EF, X=FFFF, Y=FFFF
        ORB     #$0F            * A=FF, B=EF, X=FFFF, Y=FFFF
        STB     -9,U            * A=FF, B=XX, X=FFFF, Y=FFFF
        STX     -8,U            * A=FF, B=XX, X=FFFF, Y=FFFF
        STX     -6,U            * A=FF, B=XX, X=FFFF, Y=FFFF
        STX     -4,U            * A=FF, B=XX, X=FFFF, Y=FFFF
        LDB     #$EF            * A=FF, B=XX, X=FFFF, Y=FFFF
        LDA     #$FF            * A=FF, B=EF, X=FFFF, Y=FFFF
        STD     -2,U            * A=FF, B=EF, X=FFFF, Y=FFFF
* Row 6 800
* 0F FE 99 EF FF FF FF FF FF 00
        LEAU    -128*1,U        * A=FF, B=EF, X=FFFF, Y=FFFF
        PSHU    X,Y             * A=FF, B=EF, X=FFFF, Y=FFFF
        LDB     -5,U            * A=FF, B=EF, X=FFFF, Y=FFFF
        ORB     #$0F            * A=FF, B=EF, X=FFFF, Y=FFFF
        STB     -5,U            * A=FF, B=XX, X=FFFF, Y=FFFF
        LDD     #$FE99          * A=FF, B=XX, X=FFFF, Y=FFFF
        STD     -4,U            * A=FE, B=99, X=FFFF, Y=FFFF
        LDD     #$EFFF          * A=FE, B=99, X=FFFF, Y=FFFF
        STD     -2,U            * A=EF, B=FF, X=FFFF, Y=FFFF
* Row 5 800
* 00 FE AA EF FF FF FF FF F0 00
        LEAU    -128*1+3,U      * A=EF, B=FF, X=FFFF, Y=FFFF
        LDA     ,U              * A=EF, B=FF, X=FFFF, Y=FFFF
        ORA     #$F0            * A=EF, B=FF, X=FFFF, Y=FFFF
        STA     ,U              * A=XX, B=FF, X=FFFF, Y=FFFF
        PSHU    X,Y             * A=XX, B=FF, X=FFFF, Y=FFFF
        LDB     #$FE            * A=XX, B=FE, X=FFFF, Y=FFFF
        LDX     #$AAEF          * A=XX, B=FE, X=AAEF, Y=FFFF
        PSHU    B,X             * A=XX, B=FE, X=AAEF, Y=FFFF
* Row 4 800
* 00 FE 99 EF FF FF FF FF F0 00
        LEAU    -128*1+7,U      * A=XX, B=FE, X=AAEF, Y=FFFF
        LDA     ,U              * A=XX, B=FE, X=AAEF, Y=FFFF
        ORA     #$F0            * A=XX, B=FE, X=AAEF, Y=FFFF
        STA     ,U              * A=XX, B=FE, X=AAEF, Y=FFFF
        LDD     #$99EF          * A=99, B=EF, X=AAEF, Y=FFFF
        LDX     #$FFFF          * A=99, B=EF, X=FFFF, Y=FFFF
        PSHU    D,X,Y           * A=99, B=EF, X=FFFF, Y=FFFF
        LDB     #$FE            * A=99, B=EF, X=FFFF, Y=FFFF
        STB     -1,U            * A=99, B=FE, X=FFFF, Y=FFFF
* Row 3 800
* 00 0F EE FF FF FF FF FF 00 00
        LEAU    -128*1+6,U      * A=99, B=FE, X=FFFF, Y=FFFF
        PSHU    X,Y             * A=99, B=FE, X=FFFF, Y=FFFF
        LDB     -3,U            * A=99, B=FE, X=FFFF, Y=FFFF
        ORB     #$0F            * A=99, B=FE, X=FFFF, Y=FFFF
        STB     -3,U            * A=99, B=XX, X=FFFF, Y=FFFF
        LDD     #$EEFF          * A=99, B=XX, X=FFFF, Y=FFFF
        STD     -2,U            * A=EE, B=FF, X=FFFF, Y=FFFF
* Row 2 800
* 00 00 FF FF FF FF FF F0 00 00
        LEAU    -128*1+3,U      * A=EE, B=FF, X=FFFF, Y=FFFF
        LDA     ,U              * A=EE, B=FF, X=FFFF, Y=FFFF
        ORA     #$F0            * A=EE, B=FF, X=FFFF, Y=FFFF
        STA     ,U              * A=XX, B=FF, X=FFFF, Y=FFFF
        PSHU    X,Y             * A=XX, B=FF, X=FFFF, Y=FFFF
        LDB     #$FF            * A=XX, B=FF, X=FFFF, Y=FFFF
        STB     -1,U            * A=XX, B=FF, X=FFFF, Y=FFFF
* Row 1 800
* 00 00 00 FF FF FF F0 00 00 00
        LEAU    -128*1+3,U      * A=XX, B=FF, X=FFFF, Y=FFFF
        LDA     ,U              * A=XX, B=FF, X=FFFF, Y=FFFF
        ORA     #$F0            * A=XX, B=FF, X=FFFF, Y=FFFF
        STA     ,U              * A=XX, B=FF, X=FFFF, Y=FFFF
        PSHU    B,X             * A=XX, B=FF, X=FFFF, Y=FFFF
        RTS                     * A=XX, B=FF, X=FFFF, Y=FFFF
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
_Restore_000061.bmp:
        opt     cd
        opt     cc
* Row 18
        LEAU    128*17+6-3079,U    * A=, B=, X=, Y=
        LEAY    $6500,U         * A=, B=, X=, Y=
* Row 18 1800
* 00 00 00 0F FF FF 00 00 00 00
        LDX     -2,Y            * A=, B=, X=, Y=
        LDA     -3,Y            * A=, B=, X=, Y=
        PSHU    A,X             * A=, B=, X=, Y=
* Row 17 1800
* 00 00 FF FE EE EF FF F0 00 00
        LEAU    -128*1+5,U      * A=, B=, X=, Y=
        LEAY    -126,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        STX     -2,U            * A=, B=, X=, Y=
* Row 16 1800
* 00 0F FF EE EE EE EE FF 00 00
        LEAU    -128*1+4,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDA     -7,Y            * A=, B=, X=, Y=
        PSHU    A,X             * A=, B=, X=, Y=
* Row 15 1800
* 00 FF FF FF EE EE EE EF F0 00
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -127,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
* Row 14 1800
* 00 FF FF FF FF EE DE EE F0 00
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
* Row 13 1800
* 0F FF FF FF FF FE ED EE FF 00
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDA     -9,Y            * A=, B=, X=, Y=
        STA     -1,U            * A=, B=, X=, Y=
* Row 12 1800
* 0F FF FF FF FF FF EE DE EF 00
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDA     -9,Y            * A=, B=, X=, Y=
        STA     -1,U            * A=, B=, X=, Y=
* Row 11 1800
* FF FF FF FF FF FF FE EE EF F0
        LEAU    -128*1+9,U      * A=, B=, X=, Y=
        LEAY    -127,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -10,Y           * A=, B=, X=, Y=
        STX     -2,U            * A=, B=, X=, Y=
* Row 10 1800
* FF FF FF FF FF FF FF EE EF F0
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -10,Y           * A=, B=, X=, Y=
        STX     -2,U            * A=, B=, X=, Y=
* Row 9 1800
* FF FF FF FF FF FF FF FE EF F0
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -10,Y           * A=, B=, X=, Y=
        STX     -2,U            * A=, B=, X=, Y=
* Row 8 1800
* FF FF FF FF FF FF FF FE EF F0
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -10,Y           * A=, B=, X=, Y=
        STX     -2,U            * A=, B=, X=, Y=
* Row 7 1800
* 0F FF FF FF FF FF FF FF EF 00
        LEAU    -128*1+7,U      * A=, B=, X=, Y=
        LEAY    -129,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDA     -9,Y            * A=, B=, X=, Y=
        STA     -1,U            * A=, B=, X=, Y=
* Row 6 1800
* 0F FE 99 EF FF FF FF FF FF 00
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDA     -9,Y            * A=, B=, X=, Y=
        STA     -1,U            * A=, B=, X=, Y=
* Row 5 1800
* 00 FE AA EF FF FF FF FF F0 00
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
* Row 4 1800
* 00 FE 99 EF FF FF FF FF F0 00
        LEAU    -128*1+8,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDD     -8,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
* Row 3 1800
* 00 0F EE FF FF FF FF FF 00 00
        LEAU    -128*1+7,U      * A=, B=, X=, Y=
        LEAY    -129,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        LDA     -7,Y            * A=, B=, X=, Y=
        PSHU    A,X             * A=, B=, X=, Y=
* Row 2 1800
* 00 00 FF FF FF FF FF F0 00 00
        LEAU    -128*1+7,U      * A=, B=, X=, Y=
        LEAY    -128,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        LDX     -6,Y            * A=, B=, X=, Y=
        STX     -2,U            * A=, B=, X=, Y=
* Row 1 1800
* 00 00 00 FF FF FF F0 00 00 00
        LEAU    -128*1+3,U      * A=, B=, X=, Y=
        LEAY    -129,Y          * A=, B=, X=, Y=
        LDX     -2,Y            * A=, B=, X=, Y=
        LDD     -4,Y            * A=, B=, X=, Y=
        PSHU    D,X             * A=, B=, X=, Y=
        RTS                     * A=, B=, X=, Y=
