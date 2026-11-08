!$Procedure F_GFPA ( GFPA family tests )

      SUBROUTINE F_GFPA ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFPA
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the higher-level SPICELIB geometry
!     routine GFPA.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 28-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.0.0, 18-JUN-2013 (EDW)(BVS)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      INTEGER               WNCARD


!
!     Local parameters
!
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

!
!     Local variables
!
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

!
!     Save everything
!
      SAVE

!
!     Initial values
!
      DATA                 CORR / 'NONE', &
                                   'lt', &
                                   ' lt+s', &
                                   ' cn', &
                                   ' cn + s'  /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFPA' )


      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

!
!     Create an LSK, load using FURNSH.
!
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a PCK, load using FURNSH.
!
      CALL ZZTSTPCK( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create the PCK for Nat's Solar System.
!
      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create the SPK for Nat's Solar System.
!
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


!
!    Error cases
!

!
!     Case 1
!
      CALL TCASE ( 'Non-positive step size' )

      STEP   = 0.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )



!
!     Case 2
!
      CALL TCASE ( 'Non unique body IDs.' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'MOON'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARGET = 'SUN'
      LUMIN  = 'SUN'
      OBSRVR = 'EARTH'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARGET = 'MOON'
      LUMIN  = 'SUN'
      OBSRVR = 'SUN'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



!
!     Case 3
!
      CALL TCASE ( 'Invalid aberration correction specifier' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'X'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



!
!     Case 4
!
      CALL TCASE ( 'Invalid relations operator' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '=='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )


!
!     Case 5
!
      CALL TCASE ( 'Invalid body names' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'X'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'MOON'
      LUMIN  = 'X'
      OBSRVR = 'EARTH'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'MOON'
      LUMIN  = 'SUN'
      OBSRVR = 'X'

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



!
!     Case 6
!
      CALL TCASE ( 'Negative adjustment value' )

      STEP   = 1.D0
      ADJUST =-1.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )



!
!     Case 7
!
      CALL TCASE ( 'Ephemeris data unavailable' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'DAWN'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )



!
!     Case 8
!
      CALL TCASE ( 'Invalid value for MW and NW' )

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'MOON'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'EARTH'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, 0,      NWPA,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )


!
!     Work window count below limit - NW = 4
!

      NW = 4

      STEP   = 1.D0
      ADJUST = 0.D0
      TARGET = 'ALPHA'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'BETA'
      RELATE = '='
      REFVAL = 0.D0

      CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                  RELATE, REFVAL, ADJUST, STEP, &
                  CNFINE, MAXWIN, NW,   WORK, &
                  RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )


!
!     Case 9
!
!     Loops over aberration corrections for a configuration with
!     a known geometry and behavior.
!
      STEP   = 0.2D0 * SPD()
      ADJUST = 0.D0
      REFVAL = 0.D0
      TARGET = 'ALPHA'
      LUMIN  = 'SUN'
      ABCORR = 'NONE'
      OBSRVR = 'BETA'

!
!     Confine for ninety days.
!
      NDAYS= 90
      LEFT = 0.D0
      RIGHT= NDAYS * SPD()

      CALL SCARDD ( 0,             CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      DO I = 1, NCORR

!
!        Observe the phase angle of ALPHA from BETA. Two
!        minimum values per day will occur.
!
         RELATE = 'LOCMIN'

         ABCORR = CORR(I)

         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                     RELATE, REFVAL, ADJUST, STEP, &
                     CNFINE, MAXWIN, NWPA,   WORK, &
                     RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )


!
!        Check the number of intervals in the result window.
!        We expect two events per day over CNFINE.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2*NDAYS, 0, OK )


!
!        Two maximum values per day will occur.
!
         RELATE = 'LOCMAX'

         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                     RELATE, REFVAL, ADJUST, STEP, &
                     CNFINE, MAXWIN, NWPA,   WORK, &
                     RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )


!
!        Check the number of intervals in the result window.
!        We expect two events per day over CNFINE.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2*NDAYS, 0, OK )

      END DO


!
!     Check for a specific reference value.
!
      RELATE = '='
      REFVAL = 0.1D0

      DO I = 1, NCORR

         ABCORR = CORR(I)

         CALL REPMC ( '# #', '#', RELATE, TITLE)
         CALL REPMC ( TITLE, '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         CALL GFPA ( TARGET, LUMIN,  ABCORR, OBSRVR, &
                     RELATE, REFVAL, ADJUST, STEP, &
                     CNFINE, MAXWIN, NWPA,   WORK, &
                     RESULT )
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



!
!     Case 10
!
      TITLE = 'Check phase angle against known value'
      CALL TCASE( TITLE )

!
!     We can compute the value of the maximum phase angle based
!     on the ALPHA-BETA geometry, the half-angle of the BETA
!     orbit as seen from ALPHA.
!
!        sin(alpha) = beta_orbit_radius     5.246368076245e+05
!                     ------------------ ~  ------------------
!                     alpha_orbit_radius    2.098547206045e+06
!

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

      CALL GFPA ( TARGET, LUMIN,  CORR(1), OBSRVR, &
                  RELATE, REFVAL, ADJUST,  STEP, &
                  CNFINE, MAXWIN, NWPA,    WORK, &
                  RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      COUNT = 0
      COUNT = WNCARD(RESULT)

      DO J = 1, COUNT

         CALL WNFETD ( RESULT, J, BEG, END  )

         CALL ZZGFPAQ ( BEG, 1000, 10, 2000, CORR(1), VALUE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSD ( TITLE, ALPHA, '~', VALUE, MEDIUM, OK )

      END DO


!
!     Minimum phase angle should have value approximately 0.
!
      RELATE = 'LOCMIN'

      CALL GFPA ( TARGET, LUMIN,  CORR(1), OBSRVR, &
                  RELATE, REFVAL, ADJUST,  STEP, &
                  CNFINE, MAXWIN, NWPA,    WORK, &
                  RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      COUNT = 0
      COUNT = WNCARD(RESULT)

      DO J = 1, COUNT

         CALL WNFETD ( RESULT, J, BEG, END  )

         CALL ZZGFPAQ ( BEG, 1000, 10, 2000, CORR(1), VALUE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSD ( TITLE, 0.D0, '~', VALUE, MEDIUM, OK )

      END DO



!
!     Case 11
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )

      CALL WNFETD ( RESULT, 1, BEG, END  )


!
!     Re-run a valid search after using GFSTOL.
!

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0,  RESULT )

      CALL GFPA ( TARGET, LUMIN,  CORR(1), OBSRVR, &
                  RELATE, REFVAL, ADJUST,  STEP, &
                  CNFINE, MAXWIN, NWPA,    WORK, &
                  RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     The values in the time window should not match
!     as the search used different tolerances. Check
!     the first value in the first interval.
!
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )


!
!     Reset the convergence tolerance.
!
      CALL GFSTOL ( CNVTOL )



!
! Case n
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RETURN
      END

!
!23456789012345678901234567890123456789012345678901234567890123456789012
!
