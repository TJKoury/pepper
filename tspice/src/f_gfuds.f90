!$Procedure F_GFUDS ( GFUDS family tests )

      SUBROUTINE F_GFUDS ( OK )
      use pepper_inc_gf

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFUDS
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
!     This routine tests the higher-level SPICELIB geometry routine
!     GFUDS.
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
!     E. D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.2.0, 28-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.1.0, 27-NOV-2012 (EDW)
!
!        Added test on GFSTOL use.
!
!-    TSPICE Version 1.0.0, 16-FEB-2010 (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      DVNORM
      INTEGER               WNCARD

      EXTERNAL              GFQ
      EXTERNAL              GFDECR
      EXTERNAL              GFRRDC

!
!     Local parameters
!
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

!
!     Local variables
!

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


!
!     Saved everything.
!
      SAVE

      DATA                  RELATS / '=', &
                                     '<', &
                                     '>', &
                                     'LOCMIN', &
                                     'ABSMIN', &
                                     'LOCMAX', &
                                     'ABSMAX'  /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFUDS' )


      CALL TCASE ( 'Setup: create and load SPK and LSK files.' )

!
!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .FALSE., HANDL1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( SPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

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
      CALL NATSPK ( SPK1, .FALSE., HANDL2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, RESLUD )
      CALL SSIZED ( MAXWIN, RESLRR )
      CALL SSIZED ( 2,      CNFINE )
      CALL SCARDD ( 0,      CNFINE )

!
!     Create a confinement window.
!
      CALL STR2ET ( '2007 JAN 1', LEFT )
      CALL STR2ET ( '2007 MAY 1', RIGHT)
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



!
!     Case 1
!

      CALL TCASE ( 'Invalid result window size' )

      CALL SSIZED ( 1, RESLUD )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


      CALL SSIZED ( 0, RESLUD )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )



!
!     Case 2
!

      CALL TCASE ( 'Non-positive step size' )

      CALL SSIZED ( MAXWIN, RESLUD )

      STEP   = -1.D0
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )

      STEP   = 0.D0

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


!
!     Case 3
!

      CALL TCASE ( 'Invalid relations operator' )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '!='

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )


!
!     Case 4
!

      CALL TCASE ( 'Negative adjustment value' )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = -1.D0
      RELATE = '='

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )


!
!     Case 5
!

      CALL TCASE ( 'Invalid value for MW, NW' )

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='
      MW = 0
      NW = NWUDS

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MW,     NW,      WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

!
!     Usable size of WORK windows is positive but below limit.
!
      MW = 1
      NW = NWUDS

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MW,     NW,      WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


!
!     Usable size of WORK windows is positive but is too small
!     to hold all intervals found across CNFINE. CNFINE spans
!     120 days, the constraint occurs approximately every
!     15 days.
!
!
      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='
      MW     = 6
      NW     = NWUDS

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MW,     NW,      WORK,   RESLUD )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)',   OK )

!
!     Work window count below limit - NW = 4
!

      STEP   = SPD()
      REFVAL = .3365D0
      ADJUST = 0.D0
      RELATE = '='
      MW     = MAXWIN
      NW     = 4

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MW,     NW,      WORK,   RESLUD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )


!
!     Case 6
!
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

         CALL GFRR ( TARGET, ABCORR, OBSRVR, RELATE, &
                     REFVAL, ADJUST, STEP,   CNFINE, &
                     MAXWIN, NWRR,   WORK,   RESLRR )

         CALL CHCKXC ( .FALSE., ' ',   OK )


         CALL SCARDD ( 0,      CNFINE )
         CALL WNINSD ( LEFT,   RIGHT, CNFINE )

!
!        GFDECR - wholly numerical derivative
!
         CALL GFUDS ( GFQ,    GFDECR, &
                      RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                      MAXWIN, NWUDS,   WORK,   RESLUD )

         CALL CHCKXC ( .FALSE., ' ',   OK )


!
!        The cardinality of RESULD and RESLRR better match...
!
         CALL CHCKSL ( 'COUNT != 0', WNCARD( RESLUD ) .NE. 0.D0, &
                                     .TRUE., OK )

         CALL CHCKSI ( 'COUNT', WNCARD( RESLUD ), '=', &
                                WNCARD( RESLRR ), 0,  OK )

         IF ( OK ) THEN

            DO J=1, WNCARD( RESLUD )

!
!              Fetch the endpoints of the Jth interval
!              of the result windows.
!
               CALL WNFETD ( RESLRR, J, STRTR, FNISHR )
               CALL WNFETD ( RESLUD, J, STRTU, FNISHU )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL SPKEZR ( '301', STRTU, 'J2000', 'NONE', &
                             '10',  POSU,   LT            )
               CALL SPKEZR ( '301', STRTR, 'J2000', 'NONE', &
                             '10',  POSR,   LT            )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DRDTU = DVNORM(POSU)
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Start',  DRDTU,  '~', DRDTR,  VTIGHT, OK )


               CALL SPKEZR ( '301', FNISHU, 'J2000', 'NONE', &
                             '10',  POSU,   LT            )
               CALL SPKEZR ( '301', FNISHR, 'J2000', 'NONE', &
                             '10',  POSR,   LT            )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               DRDTU = DVNORM(POSU)
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Finish', DRDTU, '~', DRDTR, VTIGHT, OK )

            END DO

         END IF


!
!        GFRRDC - analytic/numerical derivative
!
         CALL REPMC ( 'GFRR vs GFUDS analytic: #', '#', RELATE, TITLE)
         CALL TCASE ( TITLE )

         CALL GFUDS ( GFQ,    GFRRDC, &
                      RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                      MAXWIN, NWUDS,   WORK,   RESLUD )

         CALL CHCKXC ( .FALSE., ' ',   OK )


!
!        The cardinality of RESULD and RESLRR better match...
!
         CALL CHCKSL ( 'COUNT != 0', WNCARD( RESLUD ) .NE. 0.D0, &
                                     .TRUE., OK )

         CALL CHCKSI ( 'COUNT', WNCARD( RESLUD ), '=', &
                                WNCARD( RESLRR ), 0,  OK )

         IF ( OK ) THEN

            DO J=1, WNCARD( RESLUD )

!
!              Fetch the endpoints of the Jth interval
!              of the result windows. The values of the range rate
!              should equal to within VTIGHT.
!
               CALL WNFETD ( RESLRR, J, STRTR, FNISHR )
               CALL WNFETD ( RESLUD, J, STRTU, FNISHU )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL SPKEZR ( '301', STRTU, 'J2000', 'NONE', &
                             '10',  POSU,   LT            )
               DRDTU = DVNORM(POSU)
               CALL CHCKXC ( .FALSE., ' ',   OK )


               CALL SPKEZR ( '301', STRTR, 'J2000', 'NONE', &
                             '10',  POSR,   LT            )
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Start',  DRDTU,  '~', DRDTR,  VTIGHT, OK )


               CALL SPKEZR ( '301', FNISHU, 'J2000', 'NONE', &
                             '10',  POSU,   LT            )
               DRDTU = DVNORM(POSU)
               CALL CHCKXC ( .FALSE., ' ',   OK )


               CALL SPKEZR ( '301', FNISHR, 'J2000', 'NONE', &
                             '10',  POSR,   LT            )
               DRDTR = DVNORM(POSR)
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL CHCKSD ( 'Finish', DRDTU, '~', DRDTR, VTIGHT, OK )

            END DO

         END IF


      END DO



!
!     Case 7
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


!
!     Re-run a valid search after using GFSTOL to set the convergence
!     tolerance to a value that should cause a numerical error signal.
!
      STEP   = SPD()
      REFVAL = 0.D0
      ADJUST = 0.D0
      TARGET = '301'
      ABCORR = 'NONE'
      OBSRVR = '10'
      RELATE = 'ABSMIN'

      CALL SCARDD ( 0, RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESLUD)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESLUD, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Reset tol.
!

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFUDS ( GFQ,    GFDECR, &
                   RELATE, REFVAL,  ADJUST, STEP, CNFINE, &
                   MAXWIN, NWUDS,   WORK,   RESLUD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESLUD)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESLUD, 1, LEFT, RIGHT  )
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
!     Case n
!
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


!
!---    The user defined functions required by GFUDS.
!
!      gfq    for udfunc
!      gfdecr for udqdec
!      gfrrdc for udqdec
!


!$Procedure GFQ ( Scalar function - range rate )

      SUBROUTINE GFQ ( ET, VALUE )

! $ Abstract
!
!     User defined geometric quantity function. In this case,
!     the range from the sun to the Moon at TDB time ET.
!
! $ Required_Reading
!
!   None.
!
! $ Keywords
!
!   None.
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      VALUE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
! $ Detailed_Input
!
!   None.
!
! $ Detailed_Output
!
!   None.
!
! $ Parameters
!
!   None.
!
! $ Exceptions
!
!   None.
!
! $ Files
!
!   None.
!
! $ Particulars
!
!   None.
!
! $ Examples
!
!   None.
!
! $ Restrictions
!
!   None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!     E. D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 04-OCT-2009 (EDW)
!
!-&


!
!     Local variables.
!
      INTEGER               TARG
      INTEGER               OBS

      CHARACTER*(12)        REF
      CHARACTER*(12)        ABCORR

      DOUBLE PRECISION      STATE ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DVNORM

!
!     Initialization. Retrieve the vector from the Sun to
!     the Moon in the J2000 frame, without aberration
!     correction.
!
      TARG   = 301
      REF    = 'J2000'
      ABCORR = 'NONE'
      OBS    = 10

      CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )

!
!     Calculate the scalar range rate corresponding the
!     STATE vector.
!
      VALUE = DVNORM( STATE )

      END




!$Procedure GFDECR ( Derivative numeric )

      SUBROUTINE GFDECR ( UDFUNC, ET, BOOL )

! $ Abstract
!
!     User defined function to detect if the function derivative
!     is negative (the function is decreasing) at TDB time ET.
!     Numeric.
!
! $ Required_Reading
!
!   None.
!
! $ Keywords
!
!   None.
!
! $ Declarations

      IMPLICIT NONE

      EXTERNAL              UDFUNC

      DOUBLE PRECISION      ET
      LOGICAL               BOOL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
! $ Detailed_Input
!
!   None.
!
! $ Detailed_Output
!
!   None.
!
! $ Parameters
!
!   None.
!
! $ Exceptions
!
!   None.
!
! $ Files
!
!   None.
!
! $ Particulars
!
!   None.
!
! $ Examples
!
!   None.
!
! $ Restrictions
!
!   None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!     E. D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 04-OCT-2009 (EDW)
!
!-&


      DOUBLE PRECISION      DT

      DT = 1.D0

!
!     Determine if GFQ is increasing or decreasing at ET.
!
!     UDDC - the SPICE function to determine if
!            the derivative of the user defined
!            function is negative at ET.
!
!     UDFUNC - the user defined scalar quantity function.
!
      CALL UDDC ( UDFUNC, ET, DT, BOOL )

      END




!$Procedure GFRRDC ( Derivative analytic/numeric )

      SUBROUTINE GFRRDC ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     User defined function to detect if the function derivative
!     is negative (the function is decreasing) at TDB time ET.
!     Analytic/numeric.
!
! $ Required_Reading
!
!   None.
!
! $ Keywords
!
!   None.
!
! $ Declarations

      IMPLICIT NONE

      EXTERNAL              UDFUNC
      DOUBLE PRECISION      ET
      LOGICAL               DECRES

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
! $ Detailed_Input
!
!   None.
!
! $ Detailed_Output
!
!   None.
!
! $ Parameters
!
!   None.
!
! $ Exceptions
!
!   None.
!
! $ Files
!
!   None.
!
! $ Particulars
!
!   None.
!
! $ Examples
!
!   None.
!
! $ Restrictions
!
!   None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!     E. D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 04-OCT-2009 (EDW)
!
!-&


!
!     SPICELIB functions.
!
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


!
!     Approximate the derivative of the position and velocity by
!     finding the derivative of a quadratic approximating function.
!
!        DFDT(1) = Vx
!        DFDT(2) = Vy
!        DFDT(3) = Vz
!        DFDT(4) = Ax
!        DFDT(5) = Ay
!        DFDT(6) = Az
!
      CALL QDERIV ( N, STATE1, STATE2, DT, DFDT )

      CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )


!
!        d ||r||     ^
!        ------- = < r, v >
!        dt
!
!         2            ^          ^
!        d ||r||   < d r, v > + < r, d v >
!        ------- =   ---             ---
!          2
!        dt          dt              dt
!
      CALL DVHAT ( STATE, SRHAT )

      DRVEL  =  VDOT( SRHAT, DFDT(4) ) + VDOT( SRHAT(4), STATE(4) )
      DECRES =  DRVEL .LT. 0.D0

      END


!
!23456789012345678901234567890123456789012345678901234567890123456789012
!

