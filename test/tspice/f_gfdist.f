C$Procedure F_GFDIST ( GFDIST family tests )

      SUBROUTINE F_GFDIST ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFDIST
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
C     geometry routine GFDIST.
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
C     N.J. Bachman     (JPL)
C     B.V. Semenov     (JPL)
C     E.D. Wright      (JPL)
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
C-    TSPICE Version 1.0.0, 01-MAR-2009 (NJB) (BVS) (EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      INTEGER               WNCARD


C
C     Local parameters
C
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

C
C     Local variables
C
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
      CALL TOPEN ( 'F_GFDIST' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load an SPK file as well.
C
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C      CALL FURNSH ( 'de421.bsp' )
C      CALL CHCKXC ( .FALSE., ' ', OK )



C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Invalid result window size' )

      CALL SSIZED ( 1, RESULT )

      STEP   = 10.D0
      ADJUST = 0.D0

      REFVAL = 0.D0

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


      CALL SSIZED ( 0, RESULT )

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Non-positive step size' )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      STEP   = 0.D0
      ADJUST = 0.D0
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


      STEP = -1.D0
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Workspace too small' )

      STEP  = 7*SPD()

      LEFT  = 0.D0
      RIGHT = 60 * SPD()

      CALL SSIZED ( MAXWIN,        RESULT )
      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Size is zero.
C
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              0,      NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

C
C     Size is positive but below limit.
C
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              1,      NWDIST,  WORK,     RESULT   )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

C
C     Size is positive but is too small (error discovery at
C     search time).
C
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              2,      NWDIST,  WORK,     RESULT   )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)',   OK )

C
C     Window count is positive but below limit.
C
      CALL GFDIST ( 'MOON',  'NONE',    'EARTH',  'LOCMAX',
     .              REFVAL,  ADJUST,    STEP,     CNFINE,
     .              MAXWIN,  NWDIST-1,  WORK,     RESULT   )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Result window too small' )

      STEP  = 7*SPD()

      LEFT  = 0.D0
      RIGHT = 60 * SPD()

      CALL SSIZED ( 2,             RESULT )
      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'LOCMAX',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(OUTOFROOM)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Invalid operator' )

      STEP = 10.D0
      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  '!=',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Negative adjustment value' )

      STEP   = 10.D0
      ADJUST = -1.0D0

      CALL GFDIST ( 'MOON',  'NONE',  'EARTH',  'ABSMAX',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Target and observer are identical' )


      CALL GFDIST ( 'MOON',  'NONE',  'MOON',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized target or observer' )

      STEP = 1.D0

      CALL GFDIST ( 'MOOON', 'NONE',  'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL GFDIST ( 'MOON',  'NONE',  'ERTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Ephemeris data unavailable' )


      LEFT   = 0.D0
      RIGHT  = 60 * SPD()

      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEP   = 7*SPD()
      ADJUST = 0.D0


      CALL GFDIST ( 'GASPRA', 'NONE',  'EARTH',  '>',
     .              REFVAL,   ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction specifiers' )


      LEFT   = 0.D0
      RIGHT  = 60 * SPD()

      CALL SSIZED ( MAXWIN,        CNFINE )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEP   = 7*SPD()
      ADJUST = 0.D0


      CALL GFDIST ( 'MOON',  'S',     'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'XS',    'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'RLT',   'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'XRLT',  'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFDIST ( 'MOON',  'z',     'EARTH',  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )






C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************



C
C---- Case -------------------------------------------------------------
C

C
C     To speed things up, we're first going to create a couple of
C     "smarter" confinement windows. The first is the set of times when
C     the uncorrected distance is at least 404000 km. We'll call this
C     CNFIN1.
C
      ABCORR = 'NONE'
      TARGET = 'MOON'
      OBSRVR = 'EARTH'

      TITLE  = 'Find times when #-# distance '
     .//       '> # km. ABCORR = #.'
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

C
C     Create a confinement window from ET0 and ET1.
C
      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      STEP   = SPD()
      REFVAL = 404000.D0
      ADJUST = 0.D0

      CALL GFDIST ( TARGET,  'NONE',  OBSRVR,  '>',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     CNFIN1 )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'CNFIN1 cardinality', WNCARD(CNFIN1), '=', 4, 0,
     .              OK                                               )

      DO J = 1, WNCARD(CNFIN1)

         CALL WNFETD ( CNFIN1, J, LEFT, RIGHT )

      END DO

C
C     The second confinement window is used for searches for minima.
C     This will be the window over which the uncorrected distance is at
C     less than 375000 km. We'll call this CNFIN2.
C
      REFVAL = 375000.D0
      ADJUST = 0.D0

      CALL GFDIST ( TARGET,  'NONE',  OBSRVR,  '<',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     CNFIN2 )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'CNFIN2 cardinality', WNCARD(CNFIN2), '=', 3, 0,
     .              OK                                               )

      DO J = 1, WNCARD(CNFIN2)

         CALL WNFETD ( CNFIN2, J, LEFT, RIGHT )

C         WRITE (*,*) RIGHT-LEFT

      END DO


C
C     Check equality constraint for each aberration correction.
C
      DO I = 1, NCORR
C
C---- Case -------------------------------------------------------------
C
         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 404100.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance '
     .   //           '= # km. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', REFVAL, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

         CALL GFDIST ( 'MOON',  ABCORR,  'EARTH',  '=',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN1,
     .                 MAXWIN,  NWDIST,  WORK,     RESULT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        We need to have at least two intervals for
C        a reasonable test case.
C
         CALL CHCKSI ( 'RESULT cardinality',
     .                 WNCARD(RESULT), '>', 1, 0,  OK )

C
C        Check distance at the endpoint of the
C        intervals of the result window.
C
         DO J = 1, WNCARD(RESULT)

            CALL WNFETD ( RESULT, J, LEFT, RIGHT )

C
C           Check the distance at the interval's endpoints.
C
            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR,
     .                    OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST = VNORM(POS)

C            CALL SPKEZR ( TARGET, LEFT,  'J2000', ABCORR,
C     .                    OBSRVR, STATE, LT               )
C            CALL CHCKXC ( .FALSE., ' ',   OK )
C            CALL VHAT ( POS, UHAT )
C            RR = VDOT ( STATE(4), UHAT )
C            WRITE (*,*) 'Range rate = ', RR

            TITLE = 'Interval # start: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )


            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR,
     .                    OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST = VNORM(POS)

            TITLE = 'Interval # stop: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )

         END DO


      END DO


C
C     Check > and < constraints for each aberration correction.
C
      CALL SSIZED ( MAXWIN, RESLT )
      CALL SSIZED ( MAXWIN, RESGT )
      CALL SSIZED ( MAXWIN, COMP  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, NCORR
C
C---- Case -------------------------------------------------------------
C
         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 404100.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance '
     .   //           '> # km. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', REFVAL, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

C
C        Search using the > operator.
C
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  '>',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN1,
     .                 MAXWIN,  NWDIST,  WORK,     RESGT  )

         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        We need to have at least two intervals for
C        a reasonable test case.
C
         CALL CHCKSI ( 'RESGT cardinality',
     .                 WNCARD(RESGT), '>', 2, 0,  OK )


C
C        Check distance at the endpoints of the
C        intervals of the RESGT result window.
C
C        At the first start and the last stop, simply
C        check that the inequality is satisfied, since
C        these bounds may be imposed by the confinement
C        window.
C
C        Also check distance at NSAMP interior points of the
C        each interval of the RESGT result window.
C
         DO J = 1, WNCARD(RESGT)
C
C           Check the distance at the interval's endpoints.
C
            CALL WNFETD ( RESGT, J, LEFT, RIGHT )

C            WRITE (*,*) 'RESGT interval: ', J,
C     .                  LEFT, RIGHT, RIGHT-LEFT

            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR,
     .                    OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            TITLE = 'RESGT Interval # start: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )

            IF ( J .EQ. 1 ) THEN
               CALL CHCKSD ( TITLE, DIST, '>',
     .                       REFVAL-MEDIUM, MEDIUM, OK )
            ELSE
               CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )
            END IF

            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR,
     .                    OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST = VNORM(POS)

            TITLE = 'RESGT Interval # stop: distance'
            CALL REPMI ( TITLE, '#', J, TITLE )

            IF ( J .EQ. WNCARD(RESGT) ) THEN
               CALL CHCKSD ( TITLE, DIST, '>',
     .                       REFVAL-MEDIUM, MEDIUM, OK )
            ELSE
               CALL CHCKSD ( TITLE, DIST, '~', REFVAL, MEDIUM, OK )
            END IF


C
C           Sample interior points in the current interval.
C
            SPAN   = RIGHT - LEFT
            STEPSZ = SPAN  / (NSAMP+1)

            DO K = 1, NSAMP

               T = LEFT + K*STEPSZ

               CALL SPKPOS ( TARGET, T,    'J2000', ABCORR,
     .                       OBSRVR, POS,  LT               )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DIST  = VNORM(POS)

               TITLE = 'RESGT Interval # sample # distance'
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL CHCKSD ( TITLE, DIST, '>',
     .                       REFVAL-MEDIUM, MEDIUM, OK )

            END DO
C
C           We've completed tests for the current interval
C           of RESGT.
C
         END DO

C
C---- Case -------------------------------------------------------------
C
C
C        Search using the < constraint.
C
C        Use the settings of
C
C           OSERVR
C           TARGET
C           REFVAL
C           ABCORR
C           STEP
C
C        from the previous > test case.
C

         TITLE = 'Find times when #-# distance '
     .   //           '< # km. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', REFVAL, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )
C
C        Find the complement of the result window relative
C        to the confinement window.
C
         CALL WNDIFD ( CNFIN1, RESGT, COMP )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Search using the < operator.
C
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  '<',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN1,
     .                 MAXWIN,  NWDIST,  WORK,     RESLT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        We need to have at least two intervals for
C        a reasonable test case.
C
         CALL CHCKSI ( 'RESLT cardinality',
     .                 WNCARD(RESLT), '>', 1, 0,  OK )

C
C        Test RESLT: we expect this window to be very close
C        to the same window as COMP.
C
         DO J = 1, WNCARD(RESLT)

C            WRITE (*,*) 'RESLT Interval: ', J, LEFT, RIGHT,
C     .                  RIGHT-LEFT

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

C
C        We've completed tests for the current aberration
C        correction.
C
      END DO
C
C     We've completed tests of the > and < operators for
C     each aberration correction.
C


C
C     Check LOCMAX and ABSMAX, and ABSMAX with adjustment
C     constraints for each aberration correction.
C

      DO I = 1, NCORR
C
C---- Case -------------------------------------------------------------
C

C
C        Switch observer and target.
C
         TARGET = 'EARTH'
         OBSRVR = 'MOON'

         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 404000.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance '
     .   //      'is a local maximum. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

C
C        Search using the LOCMAX operator.
C
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'LOCMAX',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN1,
     .                 MAXWIN,  NWDIST,  WORK,     RESULT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'RESULT cardinality',
     .                 WNCARD(RESULT), '>', 2, 0,  OK )


         ABSMAX = 0.D0
         IXMAX  = 0

         DO J = 1, WNCARD(RESULT)
C
C           Fetch the endpoints of the current
C           interval. We expect them to match within 2*CNVTOL.
C
            CALL WNFETD ( RESULT, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            TITLE = 'Interval # left/right endpoint'
            CALL REPMI ( TITLE, '#', J, TITLE )

            CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

C
C           Check the distance at TDELTA before and after
C           the interval's left endpoint.
C

            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR,
     .                    OBSRVR, POS,  LT               )
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

               CALL SPKPOS ( TARGET, T, 'J2000', ABCORR,
     .                       OBSRVR, POS,  LT               )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DIST  = VNORM(POS)

               CALL REPMI ( TITLE, '#', J,         TITLE )
               CALL REPMD ( TITLE, '#', TDELTA, 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( TITLE, DIST, '<', MAXDST, 0.D0, OK )

            END DO
C
C           We've completed tests for the current interval
C           of RESULT.
C
         END DO


C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Find times when #-# distance '
     .   //      'is an absolute maximum. ABCORR = #.'

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

C
C        Now search for the absolute maximum distance.
C
         CALL SSIZED ( MAXWIN,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMAX',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN1,
     .                 MAXWIN,  NWDIST,  WORK,     RES2    )

         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        We expect a result window containing just 1 interval.
C
         CALL CHCKSI ( 'RES2 cardinality (0)',
     .                 WNCARD(RES2), '=', 1, 0,  OK )

C
C        Fetch the endpoints of the result window's single
C        interval. We expect them to match within 2*CNVTOL.
C
         CALL WNFETD ( RESULT, 1, LEFT, RIGHT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         TITLE = 'Interval 1 left/right endpoint'

         CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

C
C        Check the distance and time of the maximum against those
C        obtained from the local maximum search.
C
         T = RES2(1)

         CALL SPKPOS ( TARGET, T, 'J2000', ABCORR,
     .                 OBSRVR, POS,  LT               )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         DIST  = VNORM(POS)

         CALL CHCKSD ( 'Abs max distance', DIST,   '~/',
     .                                     ABSMAX, VTIGHT, OK )

         XTIME = RESULT( 2*(IXMAX-1)+1 )

         CALL CHCKSD ( 'Abs max epoch', T,     '~',
     .                                  XTIME, TIMTOL, OK )



C
C        Now search for the absolute maximum distance, using a
C        non-zero adjustment value.
C
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Find times when #-# distance '
     .   //      'is within # of an absolute maximum. '
     .   //      'ABCORR = #.'


         ADJUST = 100.D0

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', ADJUST, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )


         CALL SCARDD ( 0,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMAX',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN1,
     .                 MAXWIN,  NWDIST,  WORK,     RES2    )

         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        We expect a result window containing at least 1 interval.
C
         CALL CHCKSI ( 'RES2 cardinality (1)',
     .                 WNCARD(RES2), '>', 0, 0,  OK )

C
C        Check the distance the result window's endpoints
C        against that obtained from the absolute maximum search.
C
         DO J = 1, WNCARD(RES2)

            CALL WNFETD ( RES2, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )


            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR,
     .                    OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMAX distance, left',
     .                    DIST,   '~',  ABSMAX-ADJUST, MEDIUM, OK )

            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR,
     .                    OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMAX distance, right',
     .                    DIST,   '~',  ABSMAX-ADJUST, MEDIUM, OK )
         END DO



C
C        We've completed tests for the current aberration
C        correction.
C
      END DO




C
C     Check LOCMIN and ABSMIN, and ABSMIN with adjustment
C     constraints for each aberration correction.
C
      DO I = 1, NCORR
C
C---- Case -------------------------------------------------------------
C

         TARGET = 'EARTH'
         OBSRVR = 'MOON'

         ABCORR = CORR(I)

         STEP   = SPD()
         REFVAL = 374000.D0
         ADJUST = 0.D0

         TITLE = 'Find times when #-# distance '
     .   //      'is a local minimum. ABCORR = #.'
         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

C
C        Search using the LOCMIN operator.
C
         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'LOCMIN',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN2,
     .                 MAXWIN,  NWDIST,  WORK,     RESULT )

         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'RESULT cardinality',
     .                 WNCARD(RESULT), '>', 2, 0,  OK )


         ABSMIN = DPMAX()
         IXMIN  = 0

         DO J = 1, WNCARD(RESULT)
C
C           Fetch the endpoints of the current
C           interval. We expect them to match within 2*CNVTOL.
C
            CALL WNFETD ( RESULT, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            TITLE = 'Interval # left/right endpoint'
            CALL REPMI ( TITLE, '#', J, TITLE )

            CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

C
C           Check the distance at TDELTA before and after
C           the interval's left endpoint.
C

            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR,
     .                    OBSRVR, POS,  LT               )
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

               CALL SPKPOS ( TARGET, T, 'J2000', ABCORR,
     .                       OBSRVR, POS,  LT               )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DIST  = VNORM(POS)

               CALL REPMI ( TITLE, '#', J,         TITLE )
               CALL REPMD ( TITLE, '#', TDELTA, 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( TITLE, DIST, '>', MINDST, 0.D0, OK )

            END DO
C
C           We've completed tests for the current interval
C           of RESULT.
C
         END DO


C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Find times when #-# distance '
     .   //      'is an absolute minimum. ABCORR = #.'

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )

C
C        Now search for the absolute minimum distance.
C
         CALL SSIZED ( MAXWIN,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN2,
     .                 MAXWIN,  NWDIST,  WORK,     RES2   )

         CALL CHCKXC ( .FALSE., ' ',   OK )


C
C        We expect a result window containing just 1 interval.
C
         CALL CHCKSI ( 'RES2 cardinality (2)',
     .                 WNCARD(RES2), '=', 1, 0,  OK )

C
C        Fetch the endpoints of the result window's single
C        interval. We expect them to match within 2*CNVTOL.
C
         CALL WNFETD ( RESULT, 1, LEFT, RIGHT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         TITLE = 'Interval 1 left/right endpoint'

         CALL CHCKSD ( TITLE, LEFT, '~', RIGHT, 2*CNVTOL, OK )

C
C        Check the distance and time of the minimum against those
C        obtained from the local minimum search.
C
         T = RES2(1)

         CALL SPKPOS ( TARGET, T, 'J2000', ABCORR,
     .                 OBSRVR, POS,  LT               )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         DIST  = VNORM(POS)

         CALL CHCKSD ( 'Abs min distance', DIST,   '~/',
     .                                     ABSMIN, VTIGHT, OK )

         XTIME = RESULT( 2*(IXMIN-1)+1 )

         CALL CHCKSD ( 'Abs min epoch', T,     '~',
     .                                  XTIME, TIMTOL, OK )



C
C        Now search for the absolute minimum distance, using a
C        non-zero adjustment value.
C
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Find times when #-# distance '
     .   //      'is within # of an absolute minimum. '
     .   //      'ABCORR = #.'


         ADJUST = 100.D0

         CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
         CALL REPMC ( TITLE, '#', TARGET,    TITLE )
         CALL REPMD ( TITLE, '#', ADJUST, 6, TITLE )
         CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

         CALL TCASE ( TITLE )


         CALL SCARDD ( 0,  RES2 )

         CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN',
     .                 REFVAL,  ADJUST,  STEP,     CNFIN2,
     .                 MAXWIN,  NWDIST,  WORK,     RES2    )

         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        We expect a result window containing at least 1 interval.
C
         CALL CHCKSI ( 'RES2 cardinality (3)',
     .                 WNCARD(RES2), '>', 0, 0,  OK )

C
C        Check the distance the result window's endpoints
C        against that obtained from the absolute minimum search.
C
         DO J = 1, WNCARD(RES2)

            CALL WNFETD ( RES2, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )


            CALL SPKPOS ( TARGET, LEFT, 'J2000', ABCORR,
     .                    OBSRVR, POS,  LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMIN distance, left',
     .                    DIST,   '~',  ABSMIN+ADJUST, MEDIUM, OK )

            CALL SPKPOS ( TARGET, RIGHT, 'J2000', ABCORR,
     .                    OBSRVR, POS,   LT               )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DIST  = VNORM(POS)

            CALL CHCKSD ( 'Adjusted ABSMIN distance, right',
     .                    DIST,   '~',  ABSMIN+ADJUST, MEDIUM, OK )
         END DO



C
C        We've completed tests for the current aberration
C        correction.
C
      END DO




C
C     Case
C
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

      CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )
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

      CALL GFDIST ( TARGET,  ABCORR,  OBSRVR,  'ABSMIN',
     .              REFVAL,  ADJUST,  STEP,     CNFINE,
     .              MAXWIN,  NWDIST,  WORK,     RESULT )
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
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END
