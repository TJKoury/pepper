C$Procedure F_PHASEQ ( PHASEQ tests )

      SUBROUTINE F_PHASEQ ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routines
C
C        PHASEQ
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
C     None.
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
C-    TSPICE Version 1.0.0, 09-AUG-2012 (EDW)
C
C-&

C
C     SPICELIB functions
C

      DOUBLE PRECISION      PHASEQ
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      T_RANDD
      INTEGER               WNCARD

C
C     Local parameters
C

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

C
C     Saved variables
C
      SAVE                  RCORR
      SAVE                  XCORR
      SAVE                  TARGET
      SAVE                  ILLUMN
      SAVE                  OBSRVR

      DATA                  RCORR / '  nOne ',
     .                               ' lT',
     .                               '  Cn',
     .                               ' Lt + s',
     .                               'cN + S' /

      DATA                  XCORR / ' xlT',
     .                               '  xCn',
     .                               ' XLt + s',
     .                               'XcN + S'   /

C
C     Indices 1:3 for Invalid body name test, 4:6 for not distinct
C     body names test.
C
      DATA     TARGET /'ALPHA', 'ALPHA', 'X', 'ALPHA', 'SUN', 'ALPHA' /

      DATA     ILLUMN / 'SUN', 'X',  'SUN', 'ALPHA', 'SUN', 'SUN'     /

      DATA     OBSRVR / 'X', 'BETA', 'BETA', 'BETA', 'ALPHA', 'SUN'  /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_PHASEQ' )

      CALL KCLEAR()
      SEED1  = -54290018



C
C     Case 1: Create test kernels.
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

C
C     Create the PCK for Nat's Solar System.
C
      CALL NATPCK ( PCK,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create the SPK for Nat's Solar System.
C
      CALL NATSPK ( SPK, .TRUE., HAN )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Create an LSK, load using FURNSH.
C
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )




C
C     Case 2: Invalid body names.
C

C
C     Set appropriate values for ET, ABCORR.
C
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




C
C     Case 3: Invalid aberration corrections.
C

C
C     Set appropriate values for TARGET, ILLUMN, OBSRVR.
C
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




C
C     Case 4: Not distinct body names.
C

C
C     Set appropriate values for ABCORR.
C
      ABCORR = RCORR(1)

      DO I=4, 6

         TARG  = TARGET(I)
         ILLUM = ILLUMN(I)
         OBS   = OBSRVR(I)

         TXT = 'Not distinct body name test. '
     .      // 'TARG = #, ILLUM = #, OBS = #'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', ILLUM, TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         CALL TCASE (TXT)

         PHAS = PHASEQ ( ET, TARG, ILLUM, OBS, ABCORR )
         CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      END DO




C
C     Case 5:
C
      CALL TCASE ( 'Separation angle vs phase angle - 1' )

C
C     Calculate the phase angle for geometry with BETA as
C     the observer with ALPHA as the target. We perform
C     an angular separtion search for a geometry with an
C     identical result. This test requires use of NONE
C     correction.
C

C
C     Perform a simple search using ALPHA and BETA for times
C     of maximum angular separation as seen from the sun.
C     Recall ALPHA - BETA occultation occurs every day
C     at 12:00 PM TDB with correction NONE.
C

      STEP   = 60.D0
      ADJUST = 0.D0
      REFVAL = 0.0D0

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Store the time bounds of our search interval in
C     the CNFINE confinement window.
C
      CALL STR2ET ( '1999 DEC 31 21:00:00 TDB', ET0 )
      CALL STR2ET ( '2000 JAN 02 03:00:00 TDB', ET1 )

      CALL SCARDD  ( 0, CNFINE)
      CALL WNINSD ( ET0, ET1, CNFINE )

      CALL GFSEP ( 'SUN',   'POINT', 'IAU_SUN',
     .             'BETA',  'POINT', 'BETAFIXED',
     .             ABCORR,  'ALPHA', 'ABSMAX',
     .             REFVAL,  ADJUST, STEP,
     .             CNFINE,  MAXWIN, NWSEP, WORK,
     .             RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Check the number of intervals in the result window.
C
      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )


      CALL WNFETD ( RESULT, 1, TBEG, TEND  )

      CALL SPKPOS ( 'SUN',  TBEG, 'J2000', ABCORR, 'ALPHA',
     .                                                 POSA, LT)
      CALL SPKPOS ( 'BETA', TBEG, 'J2000', ABCORR, 'ALPHA',
     .                                                 POSB, LT)

      TARG  = 'ALPHA'
      ILLUM = 'SUN'
      OBS   = 'BETA'

      PHAS = PHASEQ ( TBEG, TARG, ILLUM, OBS, ABCORR )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The angular separation should be within the ANGTOL
C     tolerance of the phase angle.
C
      CALL CHCKSD ( 'SEP vs PHASE', VSEP(POSA, POSB), '~',
     .                                   PHAS, ANGTOL, OK )




C
C     Case 6:
C
      CALL TCASE ( 'Separation angle vs phase angle - 2' )

C
C     Use ALPHA as the observer with BETA as the target.
C     The maximum value for this geometry phase angle is PI.
C     As before, perform an angular separtion search for a
C     geometry with an identical result. This test requires
C     use of NONE correction.
C

      CALL SCARDD  ( 0, CNFINE)
      CALL WNINSD ( ET0, ET1, CNFINE )

      CALL GFSEP ( 'SUN',   'POINT', 'IAU_SUN',
     .             'ALPHA', 'POINT', 'ALPHAFIXED',
     .             ABCORR,  'BETA', 'ABSMAX',
     .             REFVAL,  ADJUST, STEP,
     .             CNFINE,  MAXWIN, NWSEP, WORK,
     .             RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Check the number of intervals in the result window.
C
      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )


      CALL WNFETD ( RESULT, 1, TBEG, TEND  )

      CALL SPKPOS ( 'BETA',  TBEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)
      CALL SPKPOS ( 'BETA', TBEG, 'J2000', ABCORR, 'ALPHA',
     .                                                 POSB, LT)

      TARG  = 'BETA'
      ILLUM = 'SUN'
      OBS   = 'ALPHA'

      PHAS = PHASEQ ( TBEG, TARG, ILLUM, OBS, ABCORR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The angular separation should be within the ANGTOL
C     tolerance of the phase angle, in this case, PI.
C
      CALL CHCKSD ( 'SEP vs PHASE', VSEP(POSA, POSB), '~',
     .                                   PHAS, ANGTOL, OK )




C
C     Case 7:
C
C     Specifically confirm the PHASEQ calculation returns numerically
C     different results for different valid aberration corrections with
C     all other arguments held constant.
C
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

               CALL CHCKSD ( TXT,
     .                       V_PHAS(I), '!=', V_PHAS(J), 0.D0, OK )
            END IF

         END DO

      END DO




C
C     Case 8:
C
C     Confirm the expected property PHASE(t) = PHASE(t + tau*n)
C     for the orbit of BETA relative to ALPHA for all allowed
C     aberration corrections.
C
C     Recall ALPHA and BETA are in circular orbits.
C
C     In this case with BETA orbit period of 21 hours, and
C     ALPHA orbit period of 7 days, we can derive the period of
C     BETA with respect to ALPHA.
C
C        omega = 2pi/21hours.tosecs - 2pi/7days.tosecs
C        tau   = 2pi/omega
C
C     This gives tau = 86400secs.
C
      DO I = 1, NRCORR

C
C        The time coverage for nat.spk is 1899 DEC 31 12:00:00.000 TDB
C        to 2100 JAN 01 12:00:00.000 TDB. This corresponds to
C        approximately -3.1*10^9 TDB seconds from J2000 to 3.1*10^9
C        TDB seconds from J2000. Randomly select a positive time within
C        this interval as the test time.
C
C        10^9.477121254719663 ~ 3*10^9
C
         RLOG   = T_RANDD( 0.D0, 9.D0, SEED1 )
         T1     = 10.D0**RLOG

C
C        At T1 near 10^8, we loose of accuracy due to the use of
C        double precision evaluations, so reduce the tolerance by
C        an order of magnitude. T1 will never exceed 10^9 in this test.
C
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

            PHAS = PHASEQ ( T1 + J*SPD()*10, TARG,
     .                                       ILLUM, OBS, RCORR(I) )
            CALL CHCKSD ( TXT, PHAS, '~', XPHAS, TOL, OK )

         END DO

      END DO




C
C     Case N:
C
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

