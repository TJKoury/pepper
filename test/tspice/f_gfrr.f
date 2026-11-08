C$Procedure F_GFRR ( GFRR family tests )

      SUBROUTINE F_GFRR ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFRR
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
      INCLUDE 'zzabcorr.inc'

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
C     This routine tests the higher-level SPICELIB
C     geometry routine GFRR.
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
C-    TSPICE Version 1.1.0, 20-NOV-2012 (EDW)
C
C        Added test on GFSTOL use.
C
C-    TSPICE Version 1.0.0, 25-AUG-2009 (EDW)
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD

      INTEGER               WNCARD


C
C     Local parameters
C
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfrr.bsp' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-4 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 3.D-6 )

      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 30.D0 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 100 )

C
C     Local variables
C
      CHARACTER*(BDNMLN)    RELATE
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    CORR   ( NCORR )


      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWRR )


      INTEGER               I
      INTEGER               NDAYS
      INTEGER               COUNT
      INTEGER               MW
      INTEGER               NW
      INTEGER               HANDL1
      INTEGER               HANDL2

C
C     Saved everything.
C
      SAVE

C
C     Initial values
C
      DATA                  CORR / 'NONE',
     .                             'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                             'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFRR' )

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


      LEFT  = 0.D0
      RIGHT = 360 * SPD()

      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( 2,      CNFINE )
      CALL SCARDD ( 0,      RESULT )
      CALL SCARDD ( 0,      CNFINE )

      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Case 1
C
      CALL TCASE ( 'Invalid result window size' )

      CALL SSIZED ( 1, RESULT )

      STEP   = 10.D0
      ADJUST = 0.D0
      REFVAL = 0.D0
      TARGET = 'MOON'
      OBSRVR = 'EARTH'
      RELATE = '>'
      ABCORR = 'NONE'

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MAXWIN, NWRR,  WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


      CALL SSIZED ( 0, RESULT )

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MAXWIN, NWRR,  WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )



C
C     Case 2
C
      CALL TCASE ( 'Non-positive step size' )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( 2,      CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      STEP   = 0.D0
      ADJUST = 0.D0
      REFVAL = 0.D0
      TARGET = 'MOON'
      OBSRVR = 'EARTH'
      RELATE = '>'
      ABCORR = 'NONE'

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MAXWIN, NWRR,  WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


      STEP = -1.D0
      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MAXWIN, NWRR,  WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )



C
C     Case 3
C

      CALL TCASE ( 'Invalid aberration correction specifier' )

      STEP   = 7*SPD()
      ADJUST = 0.D0
      RELATE = 'LOCMAX'
      REFVAL = 0.D0
      TARGET = 'MOON'
      OBSRVR = 'SUN'
      MW     = MAXWIN
      NW     = NWRR

      CALL SCARDD ( 0,             CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ABCORR = 'X'

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



C
C     Case 4
C

      CALL TCASE ( 'Invalid relations operator' )

      STEP   = 7*SPD()
      ADJUST = 0.D0
      REFVAL = 0.D0
      TARGET = 'MOON'
      OBSRVR = 'SUN'
      RELATE = 'LOCMAX'
      ABCORR = 'NONE'
      MW     = MAXWIN
      NW     = NWRR
      ABCORR = 'NONE'

      RELATE = '!='

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )



C
C     Case 5
C

      CALL TCASE ( 'Invalid body names' )

      MW     = MAXWIN
      NW     = NWRR
      RELATE = 'LOCMAX'

      TARGET = 'X'
      OBSRVR = 'SUN'

      CALL SCARDD ( 0,             CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'EARTH'
      OBSRVR = 'X'

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



C
C     Case 6
C

      CALL TCASE ( 'Negative adjustment value' )

      TARGET = 'MOON'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      RELATE = 'LOCMAX'
      REFVAL = 0.D0
      STEP   = 7*SPD()
      MW     = MAXWIN
      NW     = NWRR

      ADJUST = -1.D0

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )



C
C     Case 7
C

      CALL TCASE ( 'Ephemeris data unavailable' )

      TARGET = 'DAWN'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      RELATE = 'LOCMAX'
      REFVAL = 0.D0
      ADJUST = 0.D0
      STEP   = 7*SPD()
      MW     = MAXWIN
      NW     = NWRR


      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )



C
C     Case 8
C
      CALL TCASE ( 'Invalid value for MW, NW' )

      TARGET = 'MOON'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      RELATE = 'LOCMAX'
      REFVAL = 0.D0
      ADJUST = 0.D0
      STEP   = 7*SPD()


C
C     Usable size of WORK windows is zero.
C
      MW = 0
      NW = NWRR

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,  WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

C
C     Usable size of WORK windows is positive but below limit.
C
      MW = 1
      NW = NWRR

      CALL SCARDD ( 0,             CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

C
C     Usable size of WORK windows is positive but is too small
C     to hold all intervals found across CNFINE. CNFINE spans
C     360 days, the local maximums occur approximately every
C     28 days.
C
C

      MW  = 6
      NW = NWRR

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)',   OK )


C
C      Work window count below limit - NW = 4
C

      MW = MAXWIN
      NW = 4
      CALL SCARDD ( 0,             CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MW,     NW,     WORK,   RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )


C
C     Case 9
C
C     Loops over aberration corrections for a configuration with
C     a known geometry and behavior.
C
      TARGET = 'ALPHA'
      OBSRVR = 'BETA'
      REFVAL = 0.D0
      ADJUST = 0.D0
      STEP   = 0.4D0*SPD()
      MW     = MAXWIN
      NW     = NWRR

C
C     Confine for ninety days.
C
      NDAYS= 90
      LEFT = 0.D0
      RIGHT= NDAYS * SPD()

      CALL SCARDD ( 0,             CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, NCORR

         ABCORR = CORR(I)

         RELATE = 'LOCMAX'
         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .               REFVAL, ADJUST, STEP,   CNFINE,
     .               MW,     NW,     WORK,   RESULT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C        We expect one event per day.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', NDAYS, 0, OK )



         RELATE = 'LOCMIN'
         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .               REFVAL, ADJUST, STEP,   CNFINE,
     .               MW,     NW,     WORK,   RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C        We expect one event per day.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', NDAYS, 0, OK )


C
C        Each orbit of BETA includes a local minimum and local
C        maximum range rate event with respect to ALPHA. As the
C        orbit is periodic and closed, two events per orbit must
C        exist where the sign of the range rate changes., i.e.
C        the range rate equals zero.
C
         RELATE = '='
         REFVAL = 0.D0

         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .               REFVAL, ADJUST, STEP,   CNFINE,
     .               MW,     NW,     WORK,   RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C        We expect two events per day.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2*NDAYS, 0, OK )


      END DO




C
C     Case 10
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C

      TARGET = 'ALPHA'
      OBSRVR = 'BETA'
      REFVAL = 0.D0
      ADJUST = 0.D0
      STEP   = 0.4D0*SPD()
      RELATE = 'LOCMIN'
      ABCORR = 'NONE'

      LEFT = 0.D0
      RIGHT= 2.D0 * SPD()

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MAXWIN, NWRR,  WORK,   RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .            REFVAL, ADJUST, STEP,   CNFINE,
     .            MAXWIN, NWRR,  WORK,   RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
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
C23456789012345678901234567890123456789012345678901234567890123456789012
C