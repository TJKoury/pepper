!$Procedure F_PHASEQ ( PHASEQ tests )

      SUBROUTINE F_PHASEQ ( OK )
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        PHASEQ
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
!     None.
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
!-    TSPICE Version 1.0.0, 09-AUG-2012 (EDW)
!
!-&

!
!     SPICELIB functions
!

      DOUBLE PRECISION      PHASEQ
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      T_RANDD
      INTEGER               WNCARD

!
!     Local parameters
!

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'nat.pck' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'nat.spk' )

      CHARACTER*(*)         LSK
      PARAMETER           ( LSK    = 'phaseq.tls' )

      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL  = 1.D-9 )

      INTEGER               NRCORR
      PARAMETER           ( NRCORR = 5 )

      INTEGER               NXCORR
      PARAMETER           ( NXCORR = 4 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 25 )

      INTEGER               STRLEN
      PARAMETER           ( STRLEN = 124 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )


      CHARACTER*(ABCLEN)    ABCORR

      CHARACTER*(2*CORLEN)  RCORR  (NRCORR)
      CHARACTER*(2*CORLEN)  XCORR  (NXCORR)
      CHARACTER*(ABCLEN)    TARGET ( 6 )
      CHARACTER*(ABCLEN)    ILLUMN ( 6 )
      CHARACTER*(ABCLEN)    OBSRVR ( 6 )
      CHARACTER*(ABCLEN)    TARG
      CHARACTER*(ABCLEN)    ILLUM
      CHARACTER*(ABCLEN)    OBS
      CHARACTER*(STRLEN)    TXT


      DOUBLE PRECISION      V_PHAS (NRCORR)
      DOUBLE PRECISION      PHAS
      DOUBLE PRECISION      XPHAS
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      POSA   (3)
      DOUBLE PRECISION      POSB   (3)
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      TBEG
      DOUBLE PRECISION      TEND
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWSEP )
      DOUBLE PRECISION      RLOG
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      TOL


      INTEGER               COUNT
      INTEGER               I
      INTEGER               J
      INTEGER               HAN
      INTEGER               SEED1

!
!     Saved variables
!
      SAVE                  RCORR
      SAVE                  XCORR
      SAVE                  TARGET
      SAVE                  ILLUMN
      SAVE                  OBSRVR

      DATA                  RCORR / '  nOne ', &
                                     ' lT', &
                                     '  Cn', &
                                     ' Lt + s', &
                                     'cN + S' /

      DATA                  XCORR / ' xlT', &
                                     '  xCn', &
                                     ' XLt + s', &
                                     'XcN + S'   /

!
!     Indices 1:3 for Invalid body name test, 4:6 for not distinct
!     body names test.
!
      DATA     TARGET /'ALPHA', 'ALPHA', 'X', 'ALPHA', 'SUN', 'ALPHA' /

      DATA     ILLUMN / 'SUN', 'X',  'SUN', 'ALPHA', 'SUN', 'SUN'     /

      DATA     OBSRVR / 'X', 'BETA', 'BETA', 'BETA', 'ALPHA', 'SUN'  /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_PHASEQ' )

      CALL KCLEAR()
      SEED1  = -54290018



!
!     Case 1: Create test kernels.
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

!
!     Create the PCK for Nat's Solar System.
!
      CALL NATPCK ( PCK,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create the SPK for Nat's Solar System.
!
      CALL NATSPK ( SPK, .TRUE., HAN )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Create an LSK, load using FURNSH.
!
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )




!
!     Case 2: Invalid body names.
!

!
!     Set appropriate values for ET, ABCORR.
!
      ET     = 0.D0
      ABCORR = RCORR(1)

      DO I=1, 3

         TARG  = TARGET(I)
         ILLUM = ILLUMN(I)
         OBS   = OBSRVR(I)

         TXT = 'Invalid body name test. TARG = #, ILLUM = #, OBS = #'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', ILLUM, TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         CALL TCASE (TXT)

         PHAS = PHASEQ ( ET, TARG, ILLUM, OBS, ABCORR )
         CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      END DO




!
!     Case 3: Invalid aberration corrections.
!

!
!     Set appropriate values for TARGET, ILLUMN, OBSRVR.
!
      TARG  = TARGET(1)
      ILLUM = ILLUMN(1)
      OBS   = OBSRVR(2)

      DO I=1, NXCORR

         ABCORR = XCORR(I)

         TXT = 'Invalid aberration correction. ABCOR = #'
         CALL REPMC ( TXT, '#', ABCORR,  TXT )

         CALL TCASE (TXT)

         PHAS = PHASEQ ( ET, TARG, ILLUM, OBS, ABCORR )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      END DO




!
!     Case 4: Not distinct body names.
!

!
!     Set appropriate values for ABCORR.
!
      ABCORR = RCORR(1)

      DO I=4, 6

         TARG  = TARGET(I)
         ILLUM = ILLUMN(I)
         OBS   = OBSRVR(I)

         TXT = 'Not distinct body name test. ' &
            // 'TARG = #, ILLUM = #, OBS = #'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', ILLUM, TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         CALL TCASE (TXT)

         PHAS = PHASEQ ( ET, TARG, ILLUM, OBS, ABCORR )
         CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      END DO




!
!     Case 5:
!
      CALL TCASE ( 'Separation angle vs phase angle - 1' )

!
!     Calculate the phase angle for geometry with BETA as
!     the observer with ALPHA as the target. We perform
!     an angular separtion search for a geometry with an
!     identical result. This test requires use of NONE
!     correction.
!

!
!     Perform a simple search using ALPHA and BETA for times
!     of maximum angular separation as seen from the sun.
!     Recall ALPHA - BETA occultation occurs every day
!     at 12:00 PM TDB with correction NONE.
!

      STEP   = 60.D0
      ADJUST = 0.D0
      REFVAL = 0.0D0

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Store the time bounds of our search interval in
!     the CNFINE confinement window.
!
      CALL STR2ET ( '1999 DEC 31 21:00:00 TDB', ET0 )
      CALL STR2ET ( '2000 JAN 02 03:00:00 TDB', ET1 )

      CALL SCARDD  ( 0, CNFINE)
      CALL WNINSD ( ET0, ET1, CNFINE )

      CALL GFSEP ( 'SUN',   'POINT', 'IAU_SUN', &
                   'BETA',  'POINT', 'BETAFIXED', &
                   ABCORR,  'ALPHA', 'ABSMAX', &
                   REFVAL,  ADJUST, STEP, &
                   CNFINE,  MAXWIN, NWSEP, WORK, &
                   RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Check the number of intervals in the result window.
!
      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )


      CALL WNFETD ( RESULT, 1, TBEG, TEND  )

      CALL SPKPOS ( 'SUN',  TBEG, 'J2000', ABCORR, 'ALPHA', &
                                                       POSA, LT)
      CALL SPKPOS ( 'BETA', TBEG, 'J2000', ABCORR, 'ALPHA', &
                                                       POSB, LT)

      TARG  = 'ALPHA'
      ILLUM = 'SUN'
      OBS   = 'BETA'

      PHAS = PHASEQ ( TBEG, TARG, ILLUM, OBS, ABCORR )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The angular separation should be within the ANGTOL
!     tolerance of the phase angle.
!
      CALL CHCKSD ( 'SEP vs PHASE', VSEP(POSA, POSB), '~', &
                                         PHAS, ANGTOL, OK )




!
!     Case 6:
!
      CALL TCASE ( 'Separation angle vs phase angle - 2' )

!
!     Use ALPHA as the observer with BETA as the target.
!     The maximum value for this geometry phase angle is PI.
!     As before, perform an angular separtion search for a
!     geometry with an identical result. This test requires
!     use of NONE correction.
!

      CALL SCARDD  ( 0, CNFINE)
      CALL WNINSD ( ET0, ET1, CNFINE )

      CALL GFSEP ( 'SUN',   'POINT', 'IAU_SUN', &
                   'ALPHA', 'POINT', 'ALPHAFIXED', &
                   ABCORR,  'BETA', 'ABSMAX', &
                   REFVAL,  ADJUST, STEP, &
                   CNFINE,  MAXWIN, NWSEP, WORK, &
                   RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Check the number of intervals in the result window.
!
      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )


      CALL WNFETD ( RESULT, 1, TBEG, TEND  )

      CALL SPKPOS ( 'BETA',  TBEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)
      CALL SPKPOS ( 'BETA', TBEG, 'J2000', ABCORR, 'ALPHA', &
                                                       POSB, LT)

      TARG  = 'BETA'
      ILLUM = 'SUN'
      OBS   = 'ALPHA'

      PHAS = PHASEQ ( TBEG, TARG, ILLUM, OBS, ABCORR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The angular separation should be within the ANGTOL
!     tolerance of the phase angle, in this case, PI.
!
      CALL CHCKSD ( 'SEP vs PHASE', VSEP(POSA, POSB), '~', &
                                         PHAS, ANGTOL, OK )




!
!     Case 7:
!
!     Specifically confirm the PHASEQ calculation returns numerically
!     different results for different valid aberration corrections with
!     all other arguments held constant.
!
      DO I = 1, NRCORR

         V_PHAS(I) = PHASEQ ( TBEG, TARG, ILLUM, OBS, RCORR(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NRCORR -1

         DO J= I-1, NRCORR

            IF ( I .LT. J ) THEN

               TXT = 'Diff phase on diff abcorr, I = #, J = #'
               CALL REPMI ( TXT, '#', I, TXT )
               CALL REPMI ( TXT, '#', J, TXT )

               CALL TCASE (TXT)

               CALL CHCKSD ( TXT, &
                             V_PHAS(I), '!=', V_PHAS(J), 0.D0, OK )
            END IF

         END DO

      END DO




!
!     Case 8:
!
!     Confirm the expected property PHASE(t) = PHASE(t + tau*n)
!     for the orbit of BETA relative to ALPHA for all allowed
!     aberration corrections.
!
!     Recall ALPHA and BETA are in circular orbits.
!
!     In this case with BETA orbit period of 21 hours, and
!     ALPHA orbit period of 7 days, we can derive the period of
!     BETA with respect to ALPHA.
!
!        omega = 2pi/21hours.tosecs - 2pi/7days.tosecs
!        tau   = 2pi/omega
!
!     This gives tau = 86400secs.
!
      DO I = 1, NRCORR

!
!        The time coverage for nat.spk is 1899 DEC 31 12:00:00.000 TDB
!        to 2100 JAN 01 12:00:00.000 TDB. This corresponds to
!        approximately -3.1*10^9 TDB seconds from J2000 to 3.1*10^9
!        TDB seconds from J2000. Randomly select a positive time within
!        this interval as the test time.
!
!        10^9.477121254719663 ~ 3*10^9
!
         RLOG   = T_RANDD( 0.D0, 9.D0, SEED1 )
         T1     = 10.D0**RLOG

!
!        At T1 near 10^8, we loose of accuracy due to the use of
!        double precision evaluations, so reduce the tolerance by
!        an order of magnitude. T1 will never exceed 10^9 in this test.
!
         IF ( T1 .GT. 10.D0**8 ) THEN
            TOL = ANGTOL *10.D0
         ELSE
            TOL = ANGTOL
         END IF


         XPHAS = PHASEQ ( T1, TARG, ILLUM, OBS, RCORR(I) )

         DO J = 1, 5

            TXT = 'PHASE(t) = PHASE(t + # * tau), CORR = #'
            CALL REPMI ( TXT, '#', J*10,     TXT )
            CALL REPMC ( TXT, '#', RCORR(I), TXT )

            CALL TCASE (TXT)

            PHAS = PHASEQ ( T1 + J*SPD()*10, TARG, &
                                             ILLUM, OBS, RCORR(I) )
            CALL CHCKSD ( TXT, PHAS, '~', XPHAS, TOL, OK )

         END DO

      END DO




!
!     Case N:
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR()

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END

