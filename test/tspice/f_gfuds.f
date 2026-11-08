C$Procedure F_GFUDS ( GFUDS family tests )

      SUBROUTINE F_GFUDS ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFUDS
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the higher-level SPICELIB geometry routine
C     GFUDS.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E. D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.2.0, 28-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.1.0, 27-NOV-2012 (EDW)
C
C        Added test on GFSTOL use.
C
C-    TSPICE Version 1.0.0, 16-FEB-2010 (EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      DVNORM
      INTEGER               WNCARD

      EXTERNAL              GFQ
      EXTERNAL              GFDECR
      EXTERNAL              GFRRDC

C
C     Local parameters
C
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfuds.bsp' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               NLOOPS
      PARAMETER           ( NLOOPS = 7 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-10 )

C
C     Local variables
C

      CHARACTER*(BDNMLN)    RELATE
      CHARACTER*(BDNMLN)    RELATS (NLOOPS)
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    ABCORR
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      STRTR
      DOUBLE PRECISION      FNISHR
      DOUBLE PRECISION      STRTU
      DOUBLE PRECISION      FNISHU
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DRDTU
      DOUBLE PRECISION      DRDTR
      DOUBLE PRECISION      POSU   ( 6 )
      DOUBLE PRECISION      POSR   ( 6 )
      DOUBLE PRECISION      RESLUD ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RESLRR ( LBCELL : MAXWIN )
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWUDS )

      INTEGER               COUNT
      INTEGER               I
      INTEGER               J
      INTEGER               MW
      INTEGER               NW
      INTEGER               HANDL1
      INTEGER               HANDL2


C
C     Saved everything.
C
      SAVE

      DATA                  RELATS / '=',
     .                               '<',
     .                               '>',
     .                               'LOCMIN',
     .                               'ABSMIN',
     .                               'LOCMAX',
     .                               'ABSMAX'  /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFUDS' )


      CALL TCASE ( 'Setup: create and load SPK and LSK files.' )

C
C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load an SPK file as well.
C
      CALL TSTSPK ( SPK, .FALSE., HANDL1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( SPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Create the PCK for Nat's Solar System.
C
      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create the SPK for Nat's Solar System.
C
      CALL NATSPK ( SPK1, .FALSE., HANDL2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, RESLUD )
      CALL SSIZED ( MAXWIN, RESLRR )
      CALL SSIZED ( 2,      CNFINE )
      CALL SCARDD ( 0,      CNFINE )

C
C     Create a confinement window.
C
      CALL STR2ET ( '2007 JAN 1', LEFT )
      CALL STR2ET ( '2007 MAY 1', RIGHT)
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



C
C     Case 1
C

      CALL TCASE ( 'Invalid result window size' )

      CALL SSIZED ( 1, RESLUD )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


      CALL SSIZED ( 0, RESLUD )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )



C
C     Case 2
C

      CALL TCASE ( 'Non-positive step size' )

      CALL SSIZED ( MAXWIN, RESLUD )

      STEP   = -1.D0
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )

      STEP   = 0.D0

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


C
C     Case 3
C

      CALL TCASE ( 'Invalid relations operator' )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '!='

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )


C
C     Case 4
C

      CALL TCASE ( 'Negative adjustment value' )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = -1.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )


C
C     Case 5
C

      CALL TCASE ( 'Invalid value for MW, NW' )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='
      MW = 0
      NW = NWUDS

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MW,     NW,      WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

C
C     Usable size of WORK windows is positive but below limit.
C
      MW = 1
      NW = NWUDS

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MW,     NW,      WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


C
C     Usable size of WORK windows is positive but is too small
C     to hold all intervals found across CNFINE. CNFINE spans
C     120 days, the constraint occurs approximately every
C     15 days.
C
C
      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='
      MW     = 6
      NW     = NWUDS

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MW,     NW,      WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)',   OK )

C
C     Work window count below limit - NW = 4
C

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='
      MW     = MAXWIN
      NW     = 4

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MW,     NW,      WORK,   RESLUD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


C
C     Case 6
C
      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      TARGET = '301'
      ABCORR = 'NONE'
      OBSRVR = '10'

      DO I = 1, NLOOPS

         RELATE = RELATS(I)

         CALL REPMC ( 'GFRR vs GFUDS numerical: #', '#', RELATE, TITLE)
         CALL TCASE ( TITLE )


         CALL SCARDD ( 0,      CNFINE )
         CALL WNINSD ( LEFT,   RIGHT, CNFINE )

         CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .               REFVAL, ADJUST, STEP,   CNFINE,
     .               MAXWIN, NWRR,   WORK,   RESLRR )

         CALL CHCKXC ( .FALSE., ' ',   OK )


         CALL SCARDD ( 0,      CNFINE )
         CALL WNINSD ( LEFT,   RIGHT, CNFINE )

C
C        GFDECR - wholly numerical derivative
C
         CALL GFUDS ( GFQ,    GFDECR,
     .                RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .                MAXWIN, NWUDS,   WORK,   RESLUD )

         CALL CHCKXC ( .FALSE., ' ',   OK )


C
C        The cardinality of RESULD and RESLRR better match...
C
         CALL CHCKSL ( 'COUNT != 0', WNCARD( RESLUD ) .NE. 0.D0,
     .                               .TRUE., OK )

         CALL CHCKSI ( 'COUNT', WNCARD( RESLUD ), '=',
     .                          WNCARD( RESLRR ), 0,  OK )

         IF ( OK ) THEN

            DO J=1, WNCARD( RESLUD )

C
C              Fetch the endpoints of the Jth interval
C              of the result windows.
C
               CALL WNFETD ( RESLRR, J, STRTR, FNISHR )
               CALL WNFETD ( RESLUD, J, STRTU, FNISHU )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL SPKEZR ( '301', STRTU, 'J2000', 'NONE',
     .                       '10',  POSU,   LT            )
               CALL SPKEZR ( '301', STRTR, 'J2000', 'NONE',
     .                       '10',  POSR,   LT            )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DRDTU = DVNORM(POSU)
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Start',  DRDTU,  '~', DRDTR,  VTIGHT, OK )


               CALL SPKEZR ( '301', FNISHU, 'J2000', 'NONE',
     .                       '10',  POSU,   LT            )
               CALL SPKEZR ( '301', FNISHR, 'J2000', 'NONE',
     .                       '10',  POSR,   LT            )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DRDTU = DVNORM(POSU)
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Finish', DRDTU, '~', DRDTR, VTIGHT, OK )

            END DO

         END IF


C
C        GFRRDC - analytic/numerical derivative
C
         CALL REPMC ( 'GFRR vs GFUDS analytic: #', '#', RELATE, TITLE)
         CALL TCASE ( TITLE )

         CALL GFUDS ( GFQ,    GFRRDC,
     .                RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .                MAXWIN, NWUDS,   WORK,   RESLUD )

         CALL CHCKXC ( .FALSE., ' ',   OK )


C
C        The cardinality of RESULD and RESLRR better match...
C
         CALL CHCKSL ( 'COUNT != 0', WNCARD( RESLUD ) .NE. 0.D0,
     .                               .TRUE., OK )

         CALL CHCKSI ( 'COUNT', WNCARD( RESLUD ), '=',
     .                          WNCARD( RESLRR ), 0,  OK )

         IF ( OK ) THEN

            DO J=1, WNCARD( RESLUD )

C
C              Fetch the endpoints of the Jth interval
C              of the result windows. The values of the range rate
C              should equal to within VTIGHT.
C
               CALL WNFETD ( RESLRR, J, STRTR, FNISHR )
               CALL WNFETD ( RESLUD, J, STRTU, FNISHU )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL SPKEZR ( '301', STRTU, 'J2000', 'NONE',
     .                       '10',  POSU,   LT            )
               DRDTU = DVNORM(POSU)
               CALL CHCKXC ( .FALSE., ' ',   OK )


               CALL SPKEZR ( '301', STRTR, 'J2000', 'NONE',
     .                       '10',  POSR,   LT            )
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Start',  DRDTU,  '~', DRDTR,  VTIGHT, OK )


               CALL SPKEZR ( '301', FNISHU, 'J2000', 'NONE',
     .                       '10',  POSU,   LT            )
               DRDTU = DVNORM(POSU)
               CALL CHCKXC ( .FALSE., ' ',   OK )


               CALL SPKEZR ( '301', FNISHR, 'J2000', 'NONE',
     .                       '10',  POSR,   LT            )
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Finish', DRDTU, '~', DRDTR, VTIGHT, OK )

            END DO

         END IF


      END DO



C
C     Case 7
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C
      STEP   = SPD()
      REFVAL = 0.D0
      ADJUST = 0.D0
      TARGET = '301'
      ABCORR = 'NONE'
      OBSRVR = '10'
      RELATE = 'ABSMIN'

      CALL SCARDD ( 0, RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESLUD)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESLUD, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFUDS ( GFQ,    GFDECR,
     .             RELATE, REFVAL,  ADJUST, STEP, CNFINE,
     .             MAXWIN, NWUDS,   WORK,   RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESLUD)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESLUD, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     The values in the time window should not match
C     as the search used different tolerances. Check
C     the first value in the first interval.
C
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )



C
C     Reset the convergence tolerance.
C
      CALL GFSTOL ( CNVTOL )


C
C     Case n
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END


C
C---    The user defined functions required by GFUDS.
C
C      gfq    for udfunc
C      gfdecr for udqdec
C      gfrrdc for udqdec
C


C$Procedure GFQ ( Scalar function - range rate )

      SUBROUTINE GFQ ( ET, VALUE )

C$ Abstract
C
C     User defined geometric quantity function. In this case,
C     the range from the sun to the Moon at TDB time ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C   None.
C
C$ Keywords
C
C   None.
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      VALUE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C$ Detailed_Input
C
C   None.
C
C$ Detailed_Output
C
C   None.
C
C$ Parameters
C
C   None.
C
C$ Exceptions
C
C   None.
C
C$ Files
C
C   None.
C
C$ Particulars
C
C   None.
C
C$ Examples
C
C   None.
C
C$ Restrictions
C
C   None.
C
C$ Literature_References
C
C   None.
C
C$ Author_and_Institution
C
C     E. D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 04-OCT-2009 (EDW)
C
C-&


C
C     Local variables.
C
      INTEGER               TARG
      INTEGER               OBS

      CHARACTER*(12)        REF
      CHARACTER*(12)        ABCORR

      DOUBLE PRECISION      STATE ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DVNORM

C
C     Initialization. Retrieve the vector from the Sun to
C     the Moon in the J2000 frame, without aberration
C     correction.
C
      TARG   = 301
      REF    = 'J2000'
      ABCORR = 'NONE'
      OBS    = 10

      CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )

C
C     Calculate the scalar range rate corresponding the
C     STATE vector.
C
      VALUE = DVNORM( STATE )

      END




C$Procedure GFDECR ( Derivative numeric )

      SUBROUTINE GFDECR ( UDFUNC, ET, BOOL )

C$ Abstract
C
C     User defined function to detect if the function derivative
C     is negative (the function is decreasing) at TDB time ET.
C     Numeric.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C   None.
C
C$ Keywords
C
C   None.
C
C$ Declarations

      IMPLICIT NONE

      EXTERNAL              UDFUNC

      DOUBLE PRECISION      ET
      LOGICAL               BOOL

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C$ Detailed_Input
C
C   None.
C
C$ Detailed_Output
C
C   None.
C
C$ Parameters
C
C   None.
C
C$ Exceptions
C
C   None.
C
C$ Files
C
C   None.
C
C$ Particulars
C
C   None.
C
C$ Examples
C
C   None.
C
C$ Restrictions
C
C   None.
C
C$ Literature_References
C
C   None.
C
C$ Author_and_Institution
C
C     E. D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 04-OCT-2009 (EDW)
C
C-&


      DOUBLE PRECISION      DT

      DT = 1.D0

C
C     Determine if GFQ is increasing or decreasing at ET.
C
C     UDDC - the SPICE function to determine if
C            the derivative of the user defined
C            function is negative at ET.
C
C     UDFUNC - the user defined scalar quantity function.
C
      CALL UDDC ( UDFUNC, ET, DT, BOOL )

      END




C$Procedure GFRRDC ( Derivative analytic/numeric )

      SUBROUTINE GFRRDC ( UDFUNC, ET, DECRES )

C$ Abstract
C
C     User defined function to detect if the function derivative
C     is negative (the function is decreasing) at TDB time ET.
C     Analytic/numeric.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C   None.
C
C$ Keywords
C
C   None.
C
C$ Declarations

      IMPLICIT NONE

      EXTERNAL              UDFUNC
      DOUBLE PRECISION      ET
      LOGICAL               DECRES

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C$ Detailed_Input
C
C   None.
C
C$ Detailed_Output
C
C   None.
C
C$ Parameters
C
C   None.
C
C$ Exceptions
C
C   None.
C
C$ Files
C
C   None.
C
C$ Particulars
C
C   None.
C
C$ Examples
C
C   None.
C
C$ Restrictions
C
C   None.
C
C$ Literature_References
C
C   None.
C
C$ Author_and_Institution
C
C     E. D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 04-OCT-2009 (EDW)
C
C-&


C
C     SPICELIB functions.
C
      DOUBLE PRECISION      VDOT


      INTEGER               N
      INTEGER               TARG
      INTEGER               OBS

      DOUBLE PRECISION      DT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DRVEL
      DOUBLE PRECISION      SRHAT  ( 6 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      DFDT   ( 6 )
      DOUBLE PRECISION      STATE1 ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )

      CHARACTER*(12)        REF
      CHARACTER*(12)        ABCORR


      DT     = 1.D0
      N      = 6
      TARG   = 301
      REF    = 'J2000'
      ABCORR = 'NONE'
      OBS    = 10

      CALL SPKEZ ( TARG, ET-DT, REF, ABCORR, OBS, STATE1, LT )
      CALL SPKEZ ( TARG, ET+DT, REF, ABCORR, OBS, STATE2, LT )


C
C     Approximate the derivative of the position and velocity by
C     finding the derivative of a quadratic approximating function.
C
C        DFDT(1) = Vx
C        DFDT(2) = Vy
C        DFDT(3) = Vz
C        DFDT(4) = Ax
C        DFDT(5) = Ay
C        DFDT(6) = Az
C
      CALL QDERIV ( N, STATE1, STATE2, DT, DFDT )

      CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )


C
C        d ||r||     ^
C        ------- = < r, v >
C        dt
C
C         2            ^          ^
C        d ||r||   < d r, v > + < r, d v >
C        ------- =   ---             ---
C          2
C        dt          dt              dt
C
      CALL DVHAT ( STATE, SRHAT )

      DRVEL  =  VDOT( SRHAT, DFDT(4) ) + VDOT( SRHAT(4), STATE(4) )
      DECRES =  DRVEL .LT. 0.D0

      END


C
C23456789012345678901234567890123456789012345678901234567890123456789012
C

