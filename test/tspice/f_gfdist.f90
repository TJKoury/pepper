!$Procedure F_GFDIST ( GFDIST family tests )

      SUBROUTINE F_GFDIST ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFDIST
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
!     This routine tests the higher-level SPICELIB
!     geometry routine GFDIST.
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
!     N.J. Bachman     (JPL)
!     B.V. Semenov     (JPL)
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.2.0, 28-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.1.0, 20-NOV-2012 (EDW)
!
!        Added test on GFSTOL use.
!
!-    TSPICE Version 1.0.0, 01-MAR-2009 (NJB) (BVS) (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      INTEGER               WNCARD


!
!     Local parameters
!
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfdist.bsp' )

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

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )

      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIME0
      CHARACTER*(TIMLEN)    TIME1

      DOUBLE PRECISION      ABSMAX
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      ABSMIN
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      SPAN
      DOUBLE PRECISION      CNFIN1 ( LBCELL : MAXWIN )
      DOUBLE PRECISION      CNFIN2 ( LBCELL : MAXWIN )
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      COMP   ( LBCELL : MAXWIN )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAXDST
      DOUBLE PRECISION      MINDST
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RES2   ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RESGT  ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RESLT  ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      STEPSZ
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      T
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
      DOUBLE PRECISION      XLEFT
      DOUBLE PRECISION      XRIGHT
      DOUBLE PRECISION      XTIME

      INTEGER               HANDLE
      INTEGER               COUNT
      INTEGER               I
      INTEGER               IXMAX
      INTEGER               IXMIN
      INTEGER               J
      INTEGER               K


!
!     Saved everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CORR / 'NONE', &
                                   'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                   'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFDIST' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!      CALL FURNSH ( 'de421.bsp' )
!      CALL CHCKXC ( .FALSE., ' ', OK )



!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Invalid result window size' )

      CALL SSIZED ( 1, RESULT )

      STEP   = 10.D0
      ADJUST = 0.D0

      REFVAL = 0.D0

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


      CALL SSIZED ( 0, RESULT )

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Non-positive step size' )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      STEP   = 0.D0
      ADJUST = 0.D0
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


      STEP = -1.D0
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Workspace too small' )

      STEP  = 7*SPD()

      LEFT  = 0.D0
      RIGHT = 60 * SPD()

      CALL SSIZED ( MAXWIN,        RESULT )
      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Size is zero.
!
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    0,      NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

!
!     Size is positive but below limit.
!
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    1,      NWDIST,  WORK,     RESULT   )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

!
!     Size is positive but is too small (error discovery at
!     search time).
!
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    2,      NWDIST,  WORK,     RESULT   )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)',   OK )

!
!     Window count is positive but below limit.
!
      CALL GFDIST ( 'MOON',  'NONE',    'EARTH',  'LOCMAX', &
                    REFVAL,  ADJUST,    STEP,     CNFINE, &
                    MAXWIN,  NWDIST-1,  WORK,     RESULT   )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Result window too small' )

      STEP  = 7*SPD()

      LEFT  = 0.D0
      RIGHT = 60 * SPD()

      CALL SSIZED ( 2,             RESULT )
      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(OUTOFROOM)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Invalid operator' )

      STEP = 10.D0
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '!=', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Negative adjustment value' )

      STEP   = 10.D0
      ADJUST = -1.0D0

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'ABSMAX', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Target and observer are identical' )


      CALL GFDIST ( 'MOON',  'NONE',  'MOON',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized target or observer' )

      STEP = 1.D0

      CALL GFDIST ( 'MOOON', 'NONE',  'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL GFDIST ( 'MOON',  'NONE',  'ERTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Ephemeris data unavailable' )


      LEFT   = 0.D0
      RIGHT  = 60 * SPD()

      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEP   = 7*SPD()
      ADJUST = 0.D0


      CALL GFDIST ( 'GASPRA', 'NONE',  'EARTH',  '>', &
                    REFVAL,   ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction specifiers' )


      LEFT   = 0.D0
      RIGHT  = 60 * SPD()

      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEP   = 7*SPD()
      ADJUST = 0.D0


      CALL GFDIST ( 'MOON',  'S',     'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'XS',    'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'RLT',   'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'XRLT',  'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'z',     'EARTH',  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )






!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************



!
!---- Case -------------------------------------------------------------
!

!
!     To speed things up, we're first going to create a couple of
!     "smarter" confinement windows. The first is the set of times when
!     the uncorrected distance is at least 404000 km. We'll call this
!     CNFIN1.
!
      ABCORR = 'NONE'
      TARGET = 'MOON'
      OBSRVR = 'EARTH'

      TITLE  = 'Find times when #-# distance ' &
      //       '> # km. ABCORR = #.'
      CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
      CALL REPMC ( TITLE, '#', TARGET,    TITLE )
      CALL REPMD ( TITLE, '#', REFVAL, 6, TITLE )
      CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

      CALL TCASE ( TITLE )


      TIME0 = '2000 JAN 1  00:00:00 TDB'
      TIME1 = '2000 APR 1  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFIN1 )
      CALL SSIZED ( 2*MAXWIN, CNFIN2 )
      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Create a confinement window from ET0 and ET1.
!
      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      STEP   = SPD()
      REFVAL = 404000.D0
      ADJUST = 0.D0

      CALL GFDIST ( TARGET,  'NONE',  OBSRVR,  '>', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     CNFIN1 )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'CNFIN1 cardinality', WNCARD(CNFIN1), '=', 4, 0, &
                    OK                                               )

      DO J = 1, WNCARD(CNFIN1)

         CALL WNFETD ( CNFIN1, J, LEFT, RIGHT )

      END DO

!
!     The second confinement window is used for searches for minima.
!     This will be the window over which the uncorrected distance is at
!     less than 375000 km. We'll call this CNFIN2.
!
      REFVAL = 375000.D0
      ADJUST = 0.D0

      CALL GFDIST ( TARGET,  'NONE',  OBSRVR,  '<', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     CNFIN2 )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'CNFIN2 cardinality', WNCARD(CNFIN2), '=', 3, 0, &
                    OK                                               )

      DO J = 1, WNCARD(CNFIN2)

         CALL WNFETD ( CNFIN2, J, LEFT, RIGHT )

!         WRITE (*,*) RIGHT-LEFT

      END DO


!
!     Check equality constraint for each aberration correction.
!
      DO I = 1, NCORR
!
!---- Case -------------------------------------------------------------
!
         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 404100.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance ' &
         //           '= # km. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', REFVAL, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

         CALL GFDIST ( 'MOON',  ABCORR,  'EARTH',  '=', &
                       REFVAL,  ADJUST,  STEP,     CNFIN1, &
                       MAXWIN,  NWDIST,  WORK,     RESULT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        We need to have at least two intervals for
!        a reasonable test case.
!
         CALL CHCKSI ( 'RESULT cardinality', &
                       WNCARD(RESULT), '>', 1, 0,  OK )

!
!        Check distance at the endpoint of the
!        intervals of the result window.
!
         DO J = 1, WNCARD(RESULT)

            CALL WNFETD ( RESULT, J, LEFT, RIGHT )

!
!           Check the distance at the interval's endpoints.
!
            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR, &
                          OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST = VNORM(POS)

!            CALL SPKEZR ( TARGET, LEFT,  'J2000', ABCORR,
!     .                    OBSRVR, STATE, LT               )
!            CALL CHCKXC ( .FALSE., ' ',   OK )
!            CALL VHAT ( POS, UHAT )
!            RR = VDOT ( STATE(4), UHAT )
!            WRITE (*,*) 'Range rate = ', RR

            TITLE = 'Interval # start: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )


            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR, &
                          OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST = VNORM(POS)

            TITLE = 'Interval # stop: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )

         END DO


      END DO


!
!     Check > and < constraints for each aberration correction.
!
      CALL SSIZED ( MAXWIN, RESLT )
      CALL SSIZED ( MAXWIN, RESGT )
      CALL SSIZED ( MAXWIN, COMP  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, NCORR
!
!---- Case -------------------------------------------------------------
!
         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 404100.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance ' &
         //           '> # km. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', REFVAL, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

!
!        Search using the > operator.
!
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  '>', &
                       REFVAL,  ADJUST,  STEP,     CNFIN1, &
                       MAXWIN,  NWDIST,  WORK,     RESGT  )

         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        We need to have at least two intervals for
!        a reasonable test case.
!
         CALL CHCKSI ( 'RESGT cardinality', &
                       WNCARD(RESGT), '>', 2, 0,  OK )


!
!        Check distance at the endpoints of the
!        intervals of the RESGT result window.
!
!        At the first start and the last stop, simply
!        check that the inequality is satisfied, since
!        these bounds may be imposed by the confinement
!        window.
!
!        Also check distance at NSAMP interior points of the
!        each interval of the RESGT result window.
!
         DO J = 1, WNCARD(RESGT)
!
!           Check the distance at the interval's endpoints.
!
            CALL WNFETD ( RESGT, J, LEFT, RIGHT )

!            WRITE (*,*) 'RESGT interval: ', J,
!     .                  LEFT, RIGHT, RIGHT-LEFT

            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR, &
                          OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            TITLE = 'RESGT Interval # start: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )

            IF ( J .EQ. 1 ) THEN
               CALL CHCKSD ( TITLE, DIST, '>', &
                             REFVAL-MEDIUM, MEDIUM, OK )
            ELSE
               CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )
            END IF

            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR, &
                          OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST = VNORM(POS)

            TITLE = 'RESGT Interval # stop: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )

            IF ( J .EQ. WNCARD(RESGT) ) THEN
               CALL CHCKSD ( TITLE, DIST, '>', &
                             REFVAL-MEDIUM, MEDIUM, OK )
            ELSE
               CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )
            END IF


!
!           Sample interior points in the current interval.
!
            SPAN   = RIGHT - LEFT
            STEPSZ = SPAN  / (NSAMP+1)

            DO K = 1, NSAMP

               T = LEFT + K*STEPSZ

               CALL SPKPOS ( TARGET, T,    'J2000', ABCORR, &
                             OBSRVR, POS,  LT               )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DIST  = VNORM(POS)

               TITLE = 'RESGT Interval # sample # distance'
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL CHCKSD ( TITLE, DIST, '>', &
                             REFVAL-MEDIUM, MEDIUM, OK )

            END DO
!
!           We've completed tests for the current interval
!           of RESGT.
!
         END DO

!
!---- Case -------------------------------------------------------------
!
!
!        Search using the < constraint.
!
!        Use the settings of
!
!           OSERVR
!           TARGET
!           REFVAL
!           ABCORR
!           STEP
!
!        from the previous > test case.
!

         TITLE = 'Find times when #-# distance ' &
         //           '< # km. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', REFVAL, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )
!
!        Find the complement of the result window relative
!        to the confinement window.
!
         CALL WNDIFD ( CNFIN1, RESGT, COMP )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Search using the < operator.
!
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  '<', &
                       REFVAL,  ADJUST,  STEP,     CNFIN1, &
                       MAXWIN,  NWDIST,  WORK,     RESLT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        We need to have at least two intervals for
!        a reasonable test case.
!
         CALL CHCKSI ( 'RESLT cardinality', &
                       WNCARD(RESLT), '>', 1, 0,  OK )

!
!        Test RESLT: we expect this window to be very close
!        to the same window as COMP.
!
         DO J = 1, WNCARD(RESLT)

!            WRITE (*,*) 'RESLT Interval: ', J, LEFT, RIGHT,
!     .                  RIGHT-LEFT

            CALL WNFETD ( RESLT, J, LEFT,  RIGHT  )
            CALL WNFETD ( COMP,  J, XLEFT, XRIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )


            TITLE = 'RESLT Interval # start time'
            CALL REPMI ( TITLE, '#', J, TITLE )

            CALL CHCKSD ( TITLE, LEFT, '~', XLEFT, 3.D-6, OK )


            TITLE = 'RESLT Interval # stop time'
            CALL REPMI ( TITLE, '#', J, TITLE )

            CALL CHCKSD ( TITLE, RIGHT, '~', XRIGHT, 3.D-6, OK )

         END DO

!
!        We've completed tests for the current aberration
!        correction.
!
      END DO
!
!     We've completed tests of the > and < operators for
!     each aberration correction.
!


!
!     Check LOCMAX and ABSMAX, and ABSMAX with adjustment
!     constraints for each aberration correction.
!

      DO I = 1, NCORR
!
!---- Case -------------------------------------------------------------
!

!
!        Switch observer and target.
!
         TARGET = 'EARTH'
         OBSRVR = 'MOON'

         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 404000.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance ' &
         //      'is a local maximum. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

!
!        Search using the LOCMAX operator.
!
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'LOCMAX', &
                       REFVAL,  ADJUST,  STEP,     CNFIN1, &
                       MAXWIN,  NWDIST,  WORK,     RESULT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'RESULT cardinality', &
                       WNCARD(RESULT), '>', 2, 0,  OK )


         ABSMAX = 0.D0
         IXMAX  = 0

         DO J = 1, WNCARD(RESULT)
!
!           Fetch the endpoints of the current
!           interval. We expect them to match within 2*CNVTOL.
!
            CALL WNFETD ( RESULT, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            TITLE = 'Interval # left/right endpoint'
            CALL REPMI ( TITLE, '#', J, TITLE )

            CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

!
!           Check the distance at TDELTA before and after
!           the interval's left endpoint.
!

            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR, &
                          OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            MAXDST  = VNORM(POS)

            IF ( MAXDST .GT. ABSMAX ) THEN
               ABSMAX = MAXDST
               IXMAX  = J
            END IF

            DO K = 1, 2

               IF ( K .EQ. 1 ) THEN
                  T     = LEFT - TDELTA
                  TITLE = 'Interval # start-# sec: distance'
               ELSE
                  T     = LEFT + TDELTA
                  TITLE = 'Interval # start+# sec: distance'
               END IF

               CALL SPKPOS ( TARGET, T, 'J2000', ABCORR, &
                             OBSRVR, POS,  LT               )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DIST  = VNORM(POS)

               CALL REPMI ( TITLE, '#', J,         TITLE )
               CALL REPMD ( TITLE, '#', TDELTA, 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( TITLE, DIST, '<', MAXDST, 0.D0, OK )

            END DO
!
!           We've completed tests for the current interval
!           of RESULT.
!
         END DO


!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Find times when #-# distance ' &
         //      'is an absolute maximum. ABCORR = #.'

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

!
!        Now search for the absolute maximum distance.
!
         CALL SSIZED ( MAXWIN,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMAX', &
                       REFVAL,  ADJUST,  STEP,     CNFIN1, &
                       MAXWIN,  NWDIST,  WORK,     RES2    )

         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        We expect a result window containing just 1 interval.
!
         CALL CHCKSI ( 'RES2 cardinality (0)', &
                       WNCARD(RES2), '=', 1, 0,  OK )

!
!        Fetch the endpoints of the result window's single
!        interval. We expect them to match within 2*CNVTOL.
!
         CALL WNFETD ( RESULT, 1, LEFT, RIGHT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         TITLE = 'Interval 1 left/right endpoint'

         CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

!
!        Check the distance and time of the maximum against those
!        obtained from the local maximum search.
!
         T = RES2(1)

         CALL SPKPOS ( TARGET, T, 'J2000', ABCORR, &
                       OBSRVR, POS,  LT               )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         DIST  = VNORM(POS)

         CALL CHCKSD ( 'Abs max distance', DIST,   '~/', &
                                           ABSMAX, VTIGHT, OK )

         XTIME = RESULT( 2*(IXMAX-1)+1 )

         CALL CHCKSD ( 'Abs max epoch', T,     '~', &
                                        XTIME, TIMTOL, OK )



!
!        Now search for the absolute maximum distance, using a
!        non-zero adjustment value.
!
!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Find times when #-# distance ' &
         //      'is within # of an absolute maximum. ' &
         //      'ABCORR = #.'


         ADJUST = 100.D0

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', ADJUST, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )


         CALL SCARDD ( 0,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMAX', &
                       REFVAL,  ADJUST,  STEP,     CNFIN1, &
                       MAXWIN,  NWDIST,  WORK,     RES2    )

         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        We expect a result window containing at least 1 interval.
!
         CALL CHCKSI ( 'RES2 cardinality (1)', &
                       WNCARD(RES2), '>', 0, 0,  OK )

!
!        Check the distance the result window's endpoints
!        against that obtained from the absolute maximum search.
!
         DO J = 1, WNCARD(RES2)

            CALL WNFETD ( RES2, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )


            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR, &
                          OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMAX distance, left', &
                          DIST,   '~',  ABSMAX-ADJUST, MEDIUM, OK )

            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR, &
                          OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMAX distance, right', &
                          DIST,   '~',  ABSMAX-ADJUST, MEDIUM, OK )
         END DO



!
!        We've completed tests for the current aberration
!        correction.
!
      END DO




!
!     Check LOCMIN and ABSMIN, and ABSMIN with adjustment
!     constraints for each aberration correction.
!
      DO I = 1, NCORR
!
!---- Case -------------------------------------------------------------
!

         TARGET = 'EARTH'
         OBSRVR = 'MOON'

         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 374000.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance ' &
         //      'is a local minimum. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

!
!        Search using the LOCMIN operator.
!
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'LOCMIN', &
                       REFVAL,  ADJUST,  STEP,     CNFIN2, &
                       MAXWIN,  NWDIST,  WORK,     RESULT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'RESULT cardinality', &
                       WNCARD(RESULT), '>', 2, 0,  OK )


         ABSMIN = DPMAX()
         IXMIN  = 0

         DO J = 1, WNCARD(RESULT)
!
!           Fetch the endpoints of the current
!           interval. We expect them to match within 2*CNVTOL.
!
            CALL WNFETD ( RESULT, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            TITLE = 'Interval # left/right endpoint'
            CALL REPMI ( TITLE, '#', J, TITLE )

            CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

!
!           Check the distance at TDELTA before and after
!           the interval's left endpoint.
!

            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR, &
                          OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            MINDST  = VNORM(POS)

            IF ( MINDST .LT. ABSMIN ) THEN
               ABSMIN = MINDST
               IXMIN  = J
            END IF

            DO K = 1, 2

               IF ( K .EQ. 1 ) THEN
                  T     = LEFT - TDELTA
                  TITLE = 'Interval # start-# sec: distance'
               ELSE
                  T     = LEFT + TDELTA
                  TITLE = 'Interval # start+# sec: distance'
               END IF

               CALL SPKPOS ( TARGET, T, 'J2000', ABCORR, &
                             OBSRVR, POS,  LT               )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DIST  = VNORM(POS)

               CALL REPMI ( TITLE, '#', J,         TITLE )
               CALL REPMD ( TITLE, '#', TDELTA, 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( TITLE, DIST, '>', MINDST, 0.D0, OK )

            END DO
!
!           We've completed tests for the current interval
!           of RESULT.
!
         END DO


!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Find times when #-# distance ' &
         //      'is an absolute minimum. ABCORR = #.'

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

!
!        Now search for the absolute minimum distance.
!
         CALL SSIZED ( MAXWIN,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN', &
                       REFVAL,  ADJUST,  STEP,     CNFIN2, &
                       MAXWIN,  NWDIST,  WORK,     RES2   )

         CALL CHCKXC ( .FALSE., ' ',   OK )


!
!        We expect a result window containing just 1 interval.
!
         CALL CHCKSI ( 'RES2 cardinality (2)', &
                       WNCARD(RES2), '=', 1, 0,  OK )

!
!        Fetch the endpoints of the result window's single
!        interval. We expect them to match within 2*CNVTOL.
!
         CALL WNFETD ( RESULT, 1, LEFT, RIGHT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         TITLE = 'Interval 1 left/right endpoint'

         CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

!
!        Check the distance and time of the minimum against those
!        obtained from the local minimum search.
!
         T = RES2(1)

         CALL SPKPOS ( TARGET, T, 'J2000', ABCORR, &
                       OBSRVR, POS,  LT               )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         DIST  = VNORM(POS)

         CALL CHCKSD ( 'Abs min distance', DIST,   '~/', &
                                           ABSMIN, VTIGHT, OK )

         XTIME = RESULT( 2*(IXMIN-1)+1 )

         CALL CHCKSD ( 'Abs min epoch', T,     '~', &
                                        XTIME, TIMTOL, OK )



!
!        Now search for the absolute minimum distance, using a
!        non-zero adjustment value.
!
!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Find times when #-# distance ' &
         //      'is within # of an absolute minimum. ' &
         //      'ABCORR = #.'


         ADJUST = 100.D0

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', ADJUST, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )


         CALL SCARDD ( 0,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN', &
                       REFVAL,  ADJUST,  STEP,     CNFIN2, &
                       MAXWIN,  NWDIST,  WORK,     RES2    )

         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        We expect a result window containing at least 1 interval.
!
         CALL CHCKSI ( 'RES2 cardinality (3)', &
                       WNCARD(RES2), '>', 0, 0,  OK )

!
!        Check the distance the result window's endpoints
!        against that obtained from the absolute minimum search.
!
         DO J = 1, WNCARD(RES2)

            CALL WNFETD ( RES2, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )


            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR, &
                          OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMIN distance, left', &
                          DIST,   '~',  ABSMIN+ADJUST, MEDIUM, OK )

            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR, &
                          OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMIN distance, right', &
                          DIST,   '~',  ABSMIN+ADJUST, MEDIUM, OK )
         END DO



!
!        We've completed tests for the current aberration
!        correction.
!
      END DO




!
!     Case
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'NONE'
      STEP   = SPD()
      REFVAL = 374000.D0
      ADJUST = 0.D0

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Reset tol.
!

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN', &
                    REFVAL,  ADJUST,  STEP,     CNFINE, &
                    MAXWIN,  NWDIST,  WORK,     RESULT )
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
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END
