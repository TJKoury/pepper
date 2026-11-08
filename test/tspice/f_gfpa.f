C$Procedure F_GFPA ( GFPA family tests )

      SUBROUTINE F_GFPA ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFPA
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
C     This routine tests the higher-level SPICELIB geometry
C     routine GFPA.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 28-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.0.0, 18-JUN-2013 (EDW)(BVS)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      INTEGER               WNCARD


C
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfpa.pck' )

      CHARACTER*(*)         LSK
      PARAMETER           ( LSK    = 'gfpa.tls' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-4 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 5000 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 5 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(LNSIZE)    TARGET
      CHARACTER*(LNSIZE)    LUMIN
      CHARACTER*(LNSIZE)    OBSRVR
      CHARACTER*(LNSIZE)    RELATE
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWPA )
      DOUBLE PRECISION      POSA   ( 3 )
      DOUBLE PRECISION      POSB   ( 3 )
      DOUBLE PRECISION      VALUE


      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               COUNT
      INTEGER               NW
      INTEGER               NDAYS

C
C     Save everything
C
      SAVE

C
C     Initial values
C
      DATA                 CORR / 'NONE',
     .                             'lt',
     .                             ' lt+s',
     .                             ' cn',
     .                             ' cn + s'  /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFPA' )


      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

C
C     Create an LSK, load using FURNSH.
C
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a PCK, load using FURNSH.
C
      CALL ZZTSTPCK( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

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
      CALL NATSPK ( SPK1, .TRUE., HAN1 )
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
C    Error cases
C

C
C     Case 1
C
      CALL TCASE ( 'Non-positive step size' )

      STEP   = 0.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )



C
C     Case 2
C
      CALL TCASE ( 'Non unique body IDs.' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'MOON'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARGET = 'SUN'
      LUMIN  = 'SUN'
      OBSRVR = 'EARTH'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARGET = 'MOON'
      LUMIN  = 'SUN'
      OBSRVR = 'SUN'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C
C     Case 3
C
      CALL TCASE ( 'Invalid aberration correction specifier' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'X'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



C
C     Case 4
C
      CALL TCASE ( 'Invalid relations operator' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '=='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )


C
C     Case 5
C
      CALL TCASE ( 'Invalid body names' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'X'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'MOON'
      LUMIN  = 'X'
      OBSRVR = 'EARTH'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'MOON'
      LUMIN  = 'SUN'
      OBSRVR = 'X'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



C
C     Case 6
C
      CALL TCASE ( 'Negative adjustment value' )

      STEP   = 1.D0
      ADJUST =-1.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )



C
C     Case 7
C
      CALL TCASE ( 'Ephemeris data unavailable' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'DAWN'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )



C
C     Case 8
C
      CALL TCASE ( 'Invalid value for MW and NW' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, 0,      NWPA,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )


C
C     Work window count below limit - NW = 4
C

      NW = 4

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'ALPHA'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'BETA'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .            RELATE, REFVAL, ADJUST, STEP,
     .            CNFINE, MAXWIN, NW,   WORK,
     .            RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )


C
C     Case 9
C
C     Loops over aberration corrections for a configuration with
C     a known geometry and behavior.
C
      STEP   = 0.2D0 * SPD()
      ADJUST = 0.D0
      REFVAL = 0.D0
      TARGET = 'ALPHA'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'BETA'

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

C
C        Observe the phase angle of ALPHA from BETA. Two
C        minimum values per day will occur.
C
         RELATE = 'LOCMIN'

         ABCORR = CORR(I)

         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .               RELATE, REFVAL, ADJUST, STEP,
     .               CNFINE, MAXWIN, NWPA,   WORK,
     .               RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )


C
C        Check the number of intervals in the result window.
C        We expect two events per day over CNFINE.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2*NDAYS, 0, OK )


C
C        Two maximum values per day will occur.
C
         RELATE = 'LOCMAX'

         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .               RELATE, REFVAL, ADJUST, STEP,
     .               CNFINE, MAXWIN, NWPA,   WORK,
     .               RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )


C
C        Check the number of intervals in the result window.
C        We expect two events per day over CNFINE.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2*NDAYS, 0, OK )

      END DO


C
C     Check for a specific reference value.
C
      RELATE = '='
      REFVAL = 0.1D0

      DO I = 1, NCORR

         ABCORR = CORR(I)

         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR,
     .               RELATE, REFVAL, ADJUST, STEP,
     .               CNFINE, MAXWIN, NWPA,   WORK,
     .               RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         COUNT = 0
         COUNT = WNCARD(RESULT)

         DO J = 1, COUNT

            CALL WNFETD ( RESULT, J, BEG, END  )

            CALL ZZGFPAQ ( BEG, 1000, 10, 2000, ABCORR, VALUE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( TITLE, REFVAL, '~', VALUE, MEDIUM, OK )

         END DO

      END DO



C
C     Case 10
C
      TITLE = 'Check phase angle against known value'
      CALL TCASE( TITLE )

C
C     We can compute the value of the maximum phase angle based
C     on the ALPHA-BETA geometry, the half-angle of the BETA
C     orbit as seen from ALPHA.
C
C        sin(alpha) = beta_orbit_radius     5.246368076245e+05
C                     ------------------ ~  ------------------
C                     alpha_orbit_radius    2.098547206045e+06
C

      CALL SPKEZP ( 1000, LEFT, 'J2000', CORR(1), 10, POSA, LT )
      CALL SPKEZP ( 2000, LEFT, 'J2000', CORR(1), 10, POSB, LT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ALPHA = DASIN( VNORM( POSB )/VNORM( POSA ) )

      STEP   = 0.2D0 * SPD()
      ADJUST = 0.D0
      TARGET = 'ALPHA'
      LUMIN  = 'SUN'
      OBSRVR = 'BETA'
      RELATE = 'LOCMAX'

      CALL GFPA ( TARGET, LUMIN,  CORR(1), OBSRVR,
     .            RELATE, REFVAL, ADJUST,  STEP,
     .            CNFINE, MAXWIN, NWPA,    WORK,
     .            RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      COUNT = 0
      COUNT = WNCARD(RESULT)

      DO J = 1, COUNT

         CALL WNFETD ( RESULT, J, BEG, END  )

         CALL ZZGFPAQ ( BEG, 1000, 10, 2000, CORR(1), VALUE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSD ( TITLE, ALPHA, '~', VALUE, MEDIUM, OK )

      END DO


C
C     Minimum phase angle should have value approximately 0.
C
      RELATE = 'LOCMIN'

      CALL GFPA ( TARGET, LUMIN,  CORR(1), OBSRVR,
     .            RELATE, REFVAL, ADJUST,  STEP,
     .            CNFINE, MAXWIN, NWPA,    WORK,
     .            RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      COUNT = 0
      COUNT = WNCARD(RESULT)

      DO J = 1, COUNT

         CALL WNFETD ( RESULT, J, BEG, END  )

         CALL ZZGFPAQ ( BEG, 1000, 10, 2000, CORR(1), VALUE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSD ( TITLE, 0.D0, '~', VALUE, MEDIUM, OK )

      END DO



C
C     Case 11
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )

      CALL WNFETD ( RESULT, 1, BEG, END  )


C
C     Re-run a valid search after using GFSTOL.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0,  RESULT )

      CALL GFPA ( TARGET, LUMIN,  CORR(1), OBSRVR,
     .            RELATE, REFVAL, ADJUST,  STEP,
     .            CNFINE, MAXWIN, NWPA,    WORK,
     .            RESULT )
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
C Case n
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RETURN
      END

C
C23456789012345678901234567890123456789012345678901234567890123456789012
C
