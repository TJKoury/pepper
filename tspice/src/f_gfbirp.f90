!$Procedure      F_GFBIRP ( Test GF binary quantity progress reporting )

      SUBROUTINE F_GFBIRP ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF binary state quantity progress reporting and
!     interrupt handling functionality.
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
!     This family tests the GF subsystem's use of the default
!     progress reporting and interrupt handling functions.
!     The point is to test the callers of the functions, not
!     the functions themselves.
!
!     The routines exercised by this test family are
!
!        GFFOVE
!        GFOCCE
!        ZZGFREL
!        ZZGFSOLV
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
!-    SPICELIB Version 1.0.0, 08-MAR-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!

      EXTERNAL T_GFREPI
      EXTERNAL T_GFREPU
      EXTERNAL T_GFREPF

!
!     Local parameters
!
      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         NIK
      PARAMETER           ( NIK    = 'nat.ti' )



      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )

      INTEGER               MXPASS
      PARAMETER           ( MXPASS = 20 )

      INTEGER               MXCALL
      PARAMETER           ( MXCALL = 20000 )

!
!     Maximum progress report prefix or suffix length.
!     MXMSG is declared in zzgf.inc.
!
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = MXMSG )


!
!     Local Variables
!
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(MAXMSG)    MSGLOG ( 2, MXPASS )
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TFRAME
      CHARACTER*(SHPLEN)    TSHAPE

      DOUBLE PRECISION      CNFINE ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      CNFLOG ( LBCELL : 2*MAXWIN, MXPASS )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      MEASUR
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      REPLOG ( 3, MXCALL )
      DOUBLE PRECISION      RESULT ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XMESUR

      INTEGER               HAN2
      INTEGER               I
      INTEGER               MW
      INTEGER               NCALLS
      INTEGER               NIVL
      INTEGER               NUPDAT
      INTEGER               SEQLOG ( MXCALL )
      INTEGER               TRMLOG ( MXCALL )
      INTEGER               XSQLOG ( MXCALL )

      LOGICAL               BAIL
      LOGICAL               RPT

!
!     Saved variables
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFBIRP' )





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
!     Create and load Nat's solar system SPK, PCK/FK, and IK
!     files.
!
      CALL NATPCK ( NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATIK  ( NIK,  NSPK,   NPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************




!*********************************************************************
!*
!*    Binary state search tests
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run simple occultation search using GFOCCE.' )

!
!     Search for any occultation of ALPHA by BETA, abcorr = NONE.
!


!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( 2*MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


!
!     Note: the stellar aberration correction spec should be ignored.
!
      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

!
!     Create a confinement window with 5 intervals.
!
      NIVL   = 5
      XMESUR = 0.D0

      DO I = 1, NIVL

         ET0 = -900.D0 + (I-3)*3600.D0
         ET1 =  ET0    + 1800.D0

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         XMESUR = XMESUR + ET1 - ET0

      END DO

      BAIL = .FALSE.
      RPT  = .TRUE.


      STEP = 300.D0
      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL  = CNVTOL

      CALL GFOCCE ( 'ANY', &
                    'beta',    'ellipsoid',  'betafixed', &
                    'alpha',   'ellipsoid',  'alphafixed', &
                    'NONE',    'sun',        TOL, &
                    GFSTEP,    GFREFN,       RPT, &
                    T_GFREPI,  T_GFREPU,     T_GFREPF, &
                    BAIL,      GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now that the search is done, the interesting work starts. We're
!     going to do a post-mortem on the progress reporting calls that
!     were made during the search.
!
!     Start out by fetching the information that was passed to T_GFREPI
!     during the search.
!
      MW = 2*MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NCALLS, SEQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Occultation searches require one pass, so we expect that
!     just one call to T_GFREPI was made.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NCALLS, '=', 1, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', SEQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefix and suffix.
!
      CALL CHCKSC ( 'Prefix', MSGLOG(1,1), '=', &
                    'Occultation/transit search', OK )

      CALL CHCKSC ( 'Suffix', MSGLOG(2,1), '=', &
                    'done.', OK )



!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the sequence numbers of the calls. They should
!     range from 2 to NUPDAT+1 and should be in increasing
!     order.
!
      DO I = 1, NUPDAT
         XSQLOG(I) = I+1
      END DO

      CALL CHCKAI ( 'Update SEQLOG', SEQLOG, '=', &
                                     XSQLOG, NUPDAT, OK )
!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Occultation searches require one pass, so we expect that
!     just one call to T_GFRTRM was made.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

!
!     Check the sequence number of the first termination call. Take
!     into account that the first call was to the init routine.
!
      CALL CHCKSI ( 'T_GFREPF seq', TRMLOG(1), '=', NUPDAT+2, 0, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run simple FOV search using GFFOVE.' )


!
!     We'll use the instrument defined in nat.ti:
!
!        ALPHA_ELLIPSE_NONE
!
!     This instrument tracks body Alpha so body Beta's FOV entry and
!     exit times match the start and stop times of Beta's transit
!     across Alpha.
!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( 2*MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

!
!     Create a confinement window with 3 intervals.
!
      NIVL   = 3
      XMESUR = 0.D0

      DO I = 1, NIVL

         ET0 = -800.D0 + (I-3)*3600.D0
         ET1 =  ET0    + 1600.D0

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         XMESUR = XMESUR + ET1 - ET0

      END DO



      INST   = 'ALPHA_ELLIPSE_NONE'
      TARGET = 'BETA'
      TSHAPE = 'ELLIPSOID'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL
      ABCORR = 'NONE'

      CALL CLEARD ( 3, RAYDIR )

      RPT    = .TRUE.
      BAIL   = .FALSE.


      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL GFFOVE ( INST,      TSHAPE,  RAYDIR, &
                    TARGET,    TFRAME,  ABCORR,  OBSRVR,    TOL, &
                    GFSTEP,    GFREFN,  RPT,     T_GFREPI,  T_GFREPU, &
                    T_GFREPF,  BAIL,    GFBAIL,  CNFINE,    RESULT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now that the search is done, the interesting work starts. We're
!     going to do a post-mortem on the progress reporting calls that
!     were made during the search.
!
!     Start out by fetching the information that was passed to T_GFREPI
!     during the search.
!
      MW = 2*MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NCALLS, SEQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     FOV searches require one pass.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NCALLS, '=', 1, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', SEQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefix and suffix.
!
      CALL CHCKSC ( 'Prefix', MSGLOG(1,1), '=', &
                    'Target visibility search', OK )

      CALL CHCKSC ( 'Suffix', MSGLOG(2,1), '=', &
                    'done.', OK )


!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the sequence numbers of the calls. They should
!     range from 2 to NUPDAT+1 and should be in increasing
!     order.
!
      DO I = 1, NUPDAT
         XSQLOG(I) = I+1
      END DO

      CALL CHCKAI ( 'Update SEQLOG', SEQLOG, '=', &
                                     XSQLOG, NUPDAT, OK )
!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     FOV searches require one pass, so we expect that
!     just one call to T_GFRTRM was made.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

!
!     Check the sequence number of the first termination call. Take
!     into account that the first call was to the init routine.
!
      CALL CHCKSI ( 'T_GFREPF seq', TRMLOG(1), '=', NUPDAT+2, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )


      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )




!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!

      CALL T_SUCCESS ( OK )
      RETURN
      END

