!$Procedure      F_GFFOVE ( Test GFFOVE )

      SUBROUTINE F_GFFOVE ( OK )

! $ Abstract
!
!     Test the GF FOV search API GFFOVE.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
!     This routine does not generate any exceptions. Routines in its
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
!     This family partially tests the GF API routine GFFOVE. Complete
!     testing of GFFOVE requires execution of the test families
!
!        F_GFTFOV
!        F_GFRFOV
!        F_ZZGFFVU
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 15-APR-2014 (EDW)
!
!        Added explicit declaration of GFBAIL type.
!
!-    TSPICE Version 1.0.0, 17-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD

      INTEGER               WNCARD

      LOGICAL               GFBAIL


!
!     EXTERNAL declarations
!
!
!     SPICELIB default functions for
!
!        - Interrupt handling (no-op function):   GFBAIL
!        - Search refinement:                     GFREFN
!        - Progress report termination:           GFREPF
!        - Progress report initialization:        GFREPI
!        - Progress report update:                GFREPU
!        - Search step size "get" function:       GFSTEP
!
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPF
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP


!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgffvu.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         NIK
      PARAMETER           ( NIK    = 'nat.ti' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgffvu.bsp' )


      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 2*CNVTOL )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LBLSIZ)    QNAME
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TFRAME
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(SHPLEN)    TSHAPE

      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      ENDPT  ( 2 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XTIME

      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N

      LOGICAL               BAIL
      LOGICAL               RPT

!
!     Saved Variables
!
      SAVE

!
!     Initial values
!

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFFOVE' )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load a PCK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load Nat's solar system SPK, PCK/FK, and IK
!     files.
!
      CALL NATPCK ( NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATIK  ( NIK,  NSPK,   NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Actual DE-based ephemerides yield better comparisons,
!     since these ephemerides have less noise than do those
!     produced by TSTSPK.
!
!      CALL FURNSH ( 'de421.bsp' )
!      CALL FURNSH ( 'jup230.bsp' )
!


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!     We exercise only those error checks that are actually
!     performed in GFOCCE.
!



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Result window too small' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( 1,      RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      TSHAPE = 'ELLIPSOID'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL

      RPT    = .FALSE.
      BAIL   = .FALSE.

      CALL GFSSTP ( STEP )

      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR, &
                    TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL, &
                    GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU, &
                    GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

!
!     Restore window sizes.
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad tolerance value' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 0.D0
      TOL    = 0.D0

      RPT    = .FALSE.
      BAIL   = .FALSE.

      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR, &
                    TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL, &
                    GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU, &
                    GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )






!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!     We'll start out with cases using an ellipsoidal target and
!     geometric states. We'll use the instrument defined
!     in nat.ti:
!
!        ALPHA_ELLIPSE_NONE
!
!     This instrument track's body Alpha so body Beta's FOV entry and
!     exit times match the start and stop times of Beta's transit
!     across Alpha.
!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

!
!     Use a 3-day span that covers 3 events.
!
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TARGET = 'BETA'
      TSHAPE = 'ELLIPSOID'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL
      ABCORR = 'NONE'

      RPT    = .FALSE.
      BAIL   = .FALSE.


!
! --- Case: ------------------------------------------------------
!
!

!
!     Set up the TCASE call.
!
      TITLE = 'Target #; target shape #; Inst. #; #; ' &
      //      'target frame #'

      CALL REPMC ( TITLE, '#', TARGET, TITLE )
      CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
      CALL REPMC ( TITLE, '#', INST,   TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
      CALL REPMC ( TITLE, '#', TFRAME, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE ( TITLE )

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR, &
                    TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL, &
                    GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU, &
                    GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = WNCARD( RESULT )

!
!     We're expecting 3 events.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      IF ( N .GT. 0 ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the interval endpoints. The event should
!           start at noon and stop 10 minutes later.

            QNAME = 'Start time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD()

            CALL CHCKSD ( QNAME, ENDPT(1), '~', &
                          XTIME, TIMTOL,    OK )


            QNAME = 'Stop time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD() + 600.D0

            CALL CHCKSD ( QNAME, ENDPT(2), '~', &
                         XTIME, TIMTOL,    OK )

         END DO
!
!        End of result interval check loop.
!
      END IF
!
!     End of non-empty result window block.
!



!
!     The second case uses a ray as the target. The ray will
!     point along the +X axis of the BETA_VIEW_XY frame, which
!     tracks body BETA.
!
!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

!
!     Use a 3-day span that covers 3 events.
!
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      INST   = 'ALPHA_RECTANGLE_NONE'
      TARGET = '<none>'
      TSHAPE = 'RAY'
      TFRAME = 'BETA_VIEW_XY'

      CALL VPACK ( 1.D0, 0.0D0, 0.D0, RAYDIR )

      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL
      ABCORR = 'NONE'

      RPT    = .FALSE.
      BAIL   = .FALSE.


!
! --- Case: ------------------------------------------------------
!
!

!
!     Set up the TCASE call.
!
      TITLE = 'Target #; target shape #; Inst. #; #; ' &
      //      'target frame #'

      CALL REPMC ( TITLE, '#', TARGET, TITLE )
      CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
      CALL REPMC ( TITLE, '#', INST,   TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
      CALL REPMC ( TITLE, '#', TFRAME, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      TITLE = 'Target #; target shape #; Inst. #; #; '

      CALL TCASE ( TITLE )

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR, &
                    TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL, &
                    GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU, &
                    GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = WNCARD( RESULT )

!
!     We're expecting 3 events.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      IF ( N .GT. 0 ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the interval endpoints. The event should start at one
!           minute past noon and stop 8 minutes later.
!
            QNAME = 'Start time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD() + 60.D0

            CALL CHCKSD ( QNAME, ENDPT(1), '~', &
                          XTIME, TIMTOL,    OK )


            QNAME = 'Stop time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD() + 540.D0

            CALL CHCKSD ( QNAME, ENDPT(2), '~', &
                         XTIME, TIMTOL,    OK )

         END DO
!
!        End of result interval check loop.
!
      END IF
!
!     End of non-empty result window block.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END



