!$Procedure      F_GFRFOV ( Test GFRFOV )

      SUBROUTINE F_GFRFOV ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF FOV search API GFRFOV.
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
!     This family tests the GF AP routine GFRFOV. The underlying
!     GF FOV search utility routines
!
!        ZZGFFVIN
!        ZZGFFVST
!
!     are implicitly tested by this family together with
!
!        F_GFTFOV.
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
!-    TSPICE Version 1.2.0, 31-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLV.
!
!-    TSPICE Version 1.1.0, 20-NOV-2012 (EDW)
!
!        Added test on GFSTOL use.
!
!-    TSPICE Version 1.0.0, 17-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!


!
!     EXTERNAL declarations
!


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

      INTEGER               KVARLN
      PARAMETER           ( KVARLN = 32 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NINST
      PARAMETER           ( NINST = 4 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

      INTEGER               NCORS
      PARAMETER           ( NCORS  = 3 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(CORLEN)    ABCORS ( NCORS )
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(BDNMLN)    INSTRS ( NINST )
      CHARACTER*(KVARLN)    KVNAME
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LBLSIZ)    QNAME
      CHARACTER*(FRNMLN)    RFRAME
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      BADBND ( 3, MAXVRT )
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      END
      DOUBLE PRECISION      ENDPT  ( 2 )
      DOUBLE PRECISION      FOVBND ( 3, MAXVRT )
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      XTIME

      INTEGER               COUNT
      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INS
      INTEGER               INSTID
      INTEGER               N
      INTEGER               NC

      LOGICAL               FOUND

!
!     Saved Variables
!
      SAVE

!
!     Initial values
!
      DATA                  ABCORS / 'NONE', 'S', 'XS' /

      DATA                  INSTRS / 'ALPHA_CIRCLE_NONE', &
                                     'ALPHA_ELLIPSE_NONE', &
                                     'ALPHA_RECTANGLE_NONE', &
                                     'ALPHA_DIAMOND_NONE'    /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFRFOV' )

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
!     The following error cases involve invalid initialization
!     values or missing data discovered at search time.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad step size' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 0.D0

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Result window too small' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( 0,      RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 300.D0

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

!
!     Restore window sizes.
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad observer name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'BET'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction for ray ' &
      //           'target.'                            )

      ABCORR = 'LT+S'

      OBSRVR = 'SUN'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad reference frame RFRAME' )


      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      RFRAME = 'EME2000'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-existent reference frame.' )


      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      RFRAME = 'EME2000'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ID code for instrument.' )

      INST   = 'ALPHA_ELLIPSE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Instrument parameters missing from kernel pool.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      KVNAME = 'INS#_FOV_SHAPE'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
!
!     Delete the instrument shape from the kernel pool.
!
      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SHAPEMISSING)', OK )

!
!     Restore the instrument shape value.
!
      CALL PCPOOL ( KVNAME, 1, 'ELLIPSE' )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Degenerate FOV ellipse.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Fetch the instrument boresight vector from the kernel pool.
!
      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3, N, BSITE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Fetch the instrument boundary vectors from the kernel pool.
!
      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3*MAXVRT, N, FOVBND, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the first boundary vector equal to the boresight vector.
!
      CALL MOVED ( FOVBND, N, BADBND      )
      CALL MOVED ( BSITE,  3, BADBND(1,1) )

      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
!     Restore the first boundary vector and set the second
!     equal to the boresight vector.
!
      CALL MOVED ( FOVBND(1,1), 3, BADBND(1,1) )
      CALL MOVED ( BSITE,       3, BADBND(1,2) )

      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!     Restore the instrument FOV boundary vectors.
!
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'FOV boundary vector has excessive angular ' &
      //           'separation from boresight.'                 )

      INST   = 'ALPHA_RECTANGLE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Fetch the instrument boresight vector from the kernel pool.
!
      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3, N, BSITE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Fetch the instrument boundary vectors from the kernel pool.
!
      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3*MAXVRT, N, FOVBND, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the third boundary vector equal to the *negative*
!     of the boresight vector.
!
      CALL MOVED  ( FOVBND, N, BADBND      )
      CALL VMINUS ( BSITE,     BADBND(1,3) )

      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(FACENOTFOUND)', OK )

!
!     Restore the instrument FOV boundary vectors.
!
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )
!
!     This error is detected post-initialization.
!
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RFRAME = 'IAU_SUN'
      OBSRVR = 'GASPRA'
      ABCORR = 'S'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No RFRAME orientation data available' )
!
!     This error is detected post-initialization.
!
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      OBSRVR = 'MOON'
      RFRAME = 'ITRF93'

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )


      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!*********************************************************************
!*
!*    Non-error exceptional cases
!*
!*********************************************************************


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!     We'll use the four instruments defined in nat.ti:
!
!        ALPHA_CIRCLE_NONE
!        ALPHA_ELLIPSE_NONE
!        ALPHA_RECTANGLE_NONE
!        ALPHA_DIAMOND_NONE
!
!     These have the FOV shapes
!
!        ELLIPSE
!        CIRCLE
!        RECTANGLE
!        POLYGON
!
!     and track body Alpha so body Beta's FOV entry and
!     exit times match the start and stop times of Beta's
!     transit across Alpha.
!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

!
!     Use a 3-day span that covers 3 events.
!
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

!
!     For this set of cases, we treat the target as a ray: the target
!     is represented by the X-axis of the BETA_VIEW_XY frame.
!
!     This should cause each FOV entry to start 1 minute late and end
!     1 minute early.
!
      RFRAME = 'BETA_VIEW_XY'
      OBSRVR = 'SUN'
      STEP   = 300.D0


      DO INS = 1, NINST

         INST = INSTRS(INS)

         DO NC = 1, NCORS

!
! --- Case: ------------------------------------------------------
!
!
            ABCORR = ABCORS(NC)


!
!           Set up the TCASE call.
!
            TITLE = 'Inst. #; #; inst frame #'

            CALL REPMC ( TITLE, '#', INST,   TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', RFRAME, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

!
!           Set the ray's direction vector: BETA_VIEW_XY +X.
!
            CALL VPACK ( 1.D0, 0.D0, 0.D0, RAYDIR )

!
!           Perform the search.
!
            CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                          OBSRVR, STEP,   CNFINE, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )

!
!           We're expecting 3 events.
!
            CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

            IF ( N .GT. 0 ) THEN

               DO I = 1, N

                  CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
!
!                 Stellar aberration should have *no* effect on
!                 event times, since the observer is the Sun
!                 and the Sun is placed at the solar system
!                 barycenter in Nat's solar system. We still
!                 exercise the target code's aberration correction
!                 logic paths.
!
!                 The target is a ray, so the event times will be
!                 offset from those of the ellipsoidal target BETA.
!                 Events will start 1 minute late and end 1 minute
!                 early.
!
                  QNAME = 'Start time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + 60.D0

                  CALL CHCKSD ( QNAME, ENDPT(1), '~', &
                                XTIME, TIMTOL,    OK )


                  QNAME = 'Stop time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + 600.D0 - 60.D0

                  CALL CHCKSD ( QNAME, ENDPT(2), '~', &
                                XTIME, TIMTOL,    OK )

               END DO
!
!              End of result interval check loop.
!
            END IF
!
!           End of non-empty result window block.
!


         END DO
!
!        End of aberration correction loop.
!
      END DO
!
!     End of instrument loop.
!




!
!     Case
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


!
!     Re-run a valid search after using GFSTOL to set the convergence
!     tolerance to a value that should cause a numerical error signal.
!

      INS    = 1
      NC     = 1
      INST   = INSTRS(INS)
      ABCORR = ABCORS(NC)
      RFRAME = 'BETA_VIEW_XY'
      OBSRVR = 'SUN'
      STEP   = 300.D0

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Use a 3-day span that covers 3 events.
!
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Set the ray's direction vector: BETA_VIEW_XY +X.
!
      CALL VPACK ( 1.D0, 0.D0, 0.D0, RAYDIR )

!
!     Perform the search.
!
      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

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

      CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, STEP,   CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

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

      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END



