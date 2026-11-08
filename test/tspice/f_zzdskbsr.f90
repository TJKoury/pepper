!$Procedure F_ZZDSKBSR ( ZZDSKBSR tests )

      SUBROUTINE F_ZZDSKBSR ( OK )

! $ Abstract
!
!     Exercise entry points of the SPICELIB routine ZZDSKSBR.
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
      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'zzctr.inc'

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
!     This routine tests entry points of the SPICELIB routine KEEPER,
!     using DSK files. Entry points exercised by this test family are:
!
!        ZZDSKBSR (umbrella)
!        ZZDSKLSF
!        ZZDSKUSF
!        ZZDSKBSS
!        ZZDSKSNS
!        ZZDSKCHK
!
!     The routine
!
!        ZZDSKBBL
!
!     is also exercised by this test family.
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
!
! $ Version
!
!-    TSPICE Version 1.0.0, 08-JUL-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EVEN
      LOGICAL               EXISTS
      LOGICAL               ODD
      LOGICAL               ZZDSKBDC
      LOGICAL               ZZDSKSBD

!
!     EXTERNAL functions
!
      EXTERNAL              ZZDSKBDC
      EXTERNAL              ZZDSKNOT
      EXTERNAL              ZZDSKSBD

!
!     ZZDSKBSR parameters
!
      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE = 100 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 10000 )

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )



!
!     Local Parameters
!
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = FTSIZE + 4 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KERSIZ
      PARAMETER           ( KERSIZ = 12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

!
!     Local Variables
!
      CHARACTER*(FILSIZ)    BTDSK
      CHARACTER*(FILSIZ)    DSKS   ( MAXN )
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LBLSIZ)    LABEL
      CHARACTER*(FILSIZ)    OVRDSK

      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      XDSKDS ( DSKDSZ )

      INTEGER               BODYID
      INTEGER               BTHAN
      INTEGER               CENTRS ( BTSIZE+1 )
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               DSKCTR ( CTRSIZ )
      INTEGER               FILENO
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               HANDLS  ( MAXN )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NBOD
      INTEGER               NEARTH
      INTEGER               NSAT
      INTEGER               NSEG
      INTEGER               NXTDSC ( DLADSZ )
      INTEGER               OVRHAN
      INTEGER               PRVDSC ( DLADSZ )
      INTEGER               SURFID
      INTEGER               XDLADS ( DLADSZ )

      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               RETVAL
      LOGICAL               UPDATE

!
!     Saved values
!
!
!     Save everything in order to avoid stack problems on
!     some platforms.
!
      SAVE


!     Initial values
!


!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZDSKBSR' )


!***********************************************************************
!
!     ZZDSKBSR entry point tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKBSR error: direct call to ZZDSKBSR.' )


      CALL ZZDSKBSR ( ' ',    1,      1,      ZZDSKBDC, DSKCTR, &
                      UPDATE, DLADSC, DSKDSC, FOUND             )

      CALL CHCKXC ( .TRUE., 'SPICE(DSKBOGUSENTRY)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create two DSK files, each containing ' &
      //           'data for bodies 499 and 599.'                 )


      DSKS(1) = 'zzdskbsr_test_1.bds'
      DSKS(2) = 'zzdskbsr_test_2.bds'

      DO I = 1, 2

         IF ( EXISTS(DSKS(I)) ) THEN

            CALL DELFIL ( DSKS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         BODYID  = 499
         FRAME   = 'IAU_MARS'
         SURFID  = I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

!
!        Append segment.
!
         BODYID  = 599
         FRAME   = 'IAU_JUPITER'
         SURFID  = I

         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO



!***********************************************************************
!
!     ZZDSKLSF/ZZDSKUSF/ZZDSKBSS/ZZDSKSNS basic tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSNS Error case: search for segments for ' &
      //           'body 499 without first calling ZZDSKBSS.'      )

      BODYID = 499

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(CALLZZDSKBSSFIRST)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKBSS Error case: search for segments for ' &
      //           'body 499. No DSKs have been loaded yet.'      )

      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Load first DSK; search for segments for ' &
      //           'body 499.'                                        )


!
!     Initialize the local DSK state counter.
!
      CALL ZZCTRUIN ( DSKCTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Load the first DSK file.
!
      CALL ZZDSKLSF ( DSKS(1), HANDLS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     We should find one segment.
!
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN
!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1), 0, OK )

         CALL DLABFS ( HANDLS(1), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(1), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

!
!     Continue the search. We should not find another segment.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Search for segments for body 599.' )

      BODYID = 599


      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     We should find one segment.
!
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN
!
!        Check the other outputs. We need to locate the second
!        segment in the file first.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1), 0, OK )

         CALL DLABFS ( HANDLS(1), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL DLAFNS ( HANDLS(1), XDLADS, NXTDSC, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL MOVEI ( NXTDSC, DLADSZ, XDLADS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLAFNS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(1), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

!
!     Continue the search. We should not find another segment.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )

!
!     Check the state change status. We expect that no update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Search for segments for body 699. None should ' &
      //           'be found. No error should be signaled.'        )


      BODYID = 699


      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     We should find...nothing.
!
      CALL CHCKSL ( 'FOUND (1)', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Load second DSK; search for segments for ' &
      //           'body 499.'                                         )

      CALL ZZDSKLSF ( DSKS(2), HANDLS(2) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find two segments. The first one found should
!        be in the file loaded last.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
!
!           Check the other outputs.
!
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
!
!           Search backwards in the file for the expected segment.
!           In each case we're looking for the first segment in
!           the file.
!
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL DLAFPS ( HANDLS(J), XDLADS, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLAFPS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', &
                                    XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', &
                                    XDSKDS, DSKDSZ, 0.D0, OK )
         END IF

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Keep both files loaded; search for ' &
      //           'segments for body 599.'                       )

      BODYID = 599

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find two segments. The first one found should
!        be in the file loaded last.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
!
!           Check the other outputs.
!
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
!
!           Search backwards in the file for the expected segment.
!           In each case we're looking for the second segment in
!           the file.
!
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLABBS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', &
                                    XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', &
                                    XDSKDS, DSKDSZ, 0.D0, OK )
         END IF

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Load first DSK a second time; ' &
      //           'verify that the handle is unchanged.'    )

      CALL ZZDSKLSF ( DSKS(1), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1), 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Load second DSK a second time; ' &
      //           'verify that the handle is unchanged.'    )

      CALL ZZDSKLSF ( DSKS(2), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2), 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSF: Unload the first file; keep the ' &
      //           'second file loaded; search for ' &
      //           'segments for body 499.'                  )


      CALL ZZDSKUSF ( HANDLS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     We should find one segment.
!
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN
!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2), 0, OK )

         CALL DLABFS ( HANDLS(2), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(2), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

!
!     Continue the search. We should not find another segment.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSF: Continue the previous case; ' &
      //           'search for segments for body 599.'     )

      BODYID = 599

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     We should find one segment.
!
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN
!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2), 0, OK )

!
!        Use a backward search here.
!
         CALL DLABBS ( HANDLS(2), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(2), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

!
!     Continue the search. We should not find another segment.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSF: unload the second file. Call ' &
      //           'ZZDSKBSS to try to start a search for ' &
      //           'segments for body 499.'                 )


      CALL ZZDSKUSF ( HANDLS(2) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )



      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Load first DSK a third time; ' &
      //           'verify that the handle is new.'    )

      CALL ZZDSKLSF ( DSKS(1), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )



      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2)+1, 0, OK )

      HANDLS(1) = HANDLE

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: Load second DSK a third time; ' &
      //           'verify that the handle is unchanged.'    )

      CALL ZZDSKLSF ( DSKS(2), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1)+1, 0, OK )

      HANDLS(2) = HANDLE




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: After reload: search for segments for ' &
      //           'body 499.'                                         )


!
!     We must first build a segment list for body 499, so the segments
!     will be found in the expected order.
!
      BODYID = 499

      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find two segments. The first one found should
!        be in the file loaded last.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
!
!           Check the other outputs.
!
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
!
!           Search backwards in the file for the expected segment.
!           In each case we're looking for the first segment in
!           the file.
!
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL DLAFPS ( HANDLS(J), XDLADS, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLAFPS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', &
                                    XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', &
                                    XDSKDS, DSKDSZ, 0.D0, OK )

         END IF

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: After reload: search for ' &
      //           'segments for body 599.'              )

      BODYID = 599
!
!     Rebuild the segment list for 599.
!
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Start a search.
!
      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find two segments. The first one found should
!        be in the file loaded last.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
!
!           Check the other outputs.
!
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
!
!           Search backwards in the file for the expected segment.
!           In each case we're looking for the second segment in
!           the file.
!
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLABBS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', &
                                    XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', &
                                    XDSKDS, DSKDSZ, 0.D0, OK )
         END IF

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO




!***********************************************************************
!
!
!     ZZDSKLSF/ZZDSKBSS/ZZDSKSNS full capability tests
!
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create DSK containing data for BTSIZE+1' &
      //           'bodies. There are two segments per body.' )


      BTDSK = 'zzdskbsr_body_table.bds'

      IF ( EXISTS(BTDSK) ) THEN
         CALL DELFIL ( BTDSK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

!
!     We want to take advantage of the SPICE built-in non-inertial
!     frames. We need BTSIZE+1 of them, so we must include the nonsense
!     frames such as those associated with planetary barycenters.
!     This doesn't matter from the perspective of this test.
!
!
      NBOD = BTSIZE + 1

      N    = 0
      I    = 1


      DO WHILE ( ( N .LT. NBOD ) .AND. ( I .LT. BTSIZE+10 ) )

         FRAMID = 10000 + I

         CALL FRINFO ( FRAMID, BODYID, CLASS, CLSSID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Note that there's a gap in the sequence for class ID 3000;
!        there may be other gaps.
!
!        Also, we want to avoid using the earth, since IAU_EARTH
!        and EARTH_FIXED are both built-in frames centered on
!        the earth. This is an unnecessary complication.
!
         IF (  FOUND .AND. ( BODYID .NE. 399 )  ) THEN

            N = N + 1

            CALL FRMNAM ( FRAMID, FRAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( FRAME .EQ. ' ' ) THEN
               CALL SETMSG ( 'Frame name for ID # is blank.' )
               CALL ERRINT ( '#', FRAMID                     )
               CALL SIGERR ( 'SPICE(NOTRANSLATION)'          )
               CALL CHCKXC ( .FALSE., ' ', OK )
            END IF

            SURFID = N

            CALL T_SMLDSK ( BODYID, SURFID, FRAME, BTDSK )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            SURFID = -N

            CALL T_SMLDSK ( BODYID, SURFID, FRAME, BTDSK )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CENTRS(N) = BODYID

         END IF

         I = I + 1

      END DO

      CALL CHCKSI ( 'N', N, '=', NBOD, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Load the DSK that contains BTSIZE+1 bodies. ' &
      //           'Look up data for each one.'                )


!
!     This test will force one body out of the body list.
!
      CALL ZZDSKLSF ( BTDSK, BTHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = NBOD, 1, -1

         BODYID = CENTRS(I)

         CALL ZZDSKBSS ( BODYID )
         CALL CHCKXC   ( .FALSE., ' ', OK )

!
!        Prepare segment selection test function for use in BSR search.
!
         RETVAL = ZZDSKSBD ( BODYID )
         CALL CHCKXC   ( .FALSE., ' ', OK )

!
!        Perform the search. We expect to find two segments per body.
!
         DO J = 1, 2

            CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'ZZDSKSNS FOUND', FOUND, .TRUE., OK )

            IF ( FOUND ) THEN
!
!              Check the other outputs.
!
               LABEL = 'HANDLE I=@; J=@'
               CALL REPMI ( LABEL, '@', I, LABEL )
               CALL REPMI ( LABEL, '@', J, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, HANDLE, '=', BTHAN, 0, OK )

!
!              Search backwards in the file for the expected segment.
!
               IF ( ( I .EQ. NBOD ) .AND. ( J .EQ. 1 ) ) THEN

                  CALL DLABBS ( BTHAN, XDLADS, FND )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'DLABBS FOUND', FND, .TRUE., OK )

               ELSE

                  CALL DLAFPS ( BTHAN, XDLADS, PRVDSC, FND )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'DLAFPS FOUND', FND, .TRUE., OK )

                  CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

               END IF

               CALL DSKGD ( BTHAN, XDLADS, XDSKDS )
               CALL CHCKXC   ( .FALSE., ' ', OK )


               LABEL = 'DLADSC I=@; J=@'
               CALL REPMI ( LABEL, '@', I, LABEL )
               CALL REPMI ( LABEL, '@', J, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKAI ( LABEL, DLADSC, '=', &
                                    XDLADS, DLADSZ, OK )

               LABEL = 'DSKDSC I=@; J=@'
               CALL REPMI ( LABEL, '@', I, LABEL )
               CALL REPMI ( LABEL, '@', J, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKAD ( LABEL, DSKDSC, '=', &
                                    XDSKDS, DSKDSZ, 0.D0, OK )

            END IF

         END DO

      END DO


!
!     Unload this file to simplify the later tests.
!
      CALL ZZDSKUSF ( BTHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create FTSIZE+1 DSKs containing data ' &
      //           'for the earth. Each file but the last ' &
      //           'contains two segments. The last one contains ' &
      //           'five segments.'                               )


      NEARTH = FTSIZE + 1

      DO I = 3, 2 + NEARTH

         DSKS(I) =  'zzdskbsr_test_#.bds'
         CALL REPMI ( DSKS(I), '#', I, DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( EXISTS(DSKS(I)) ) THEN

            CALL DELFIL ( DSKS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         BODYID  = 399
         FRAME   = 'IAU_EARTH'
         SURFID  = I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         SURFID  = -I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

      I = 2 + NEARTH

      DO J = 1, 3

         SURFID = J
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: load FTSIZE-2 earth DSKs.' )

!
!     Note that DSKs 1 and 2 are still loaded.
!
      DO I = 3, FTSIZE

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

!
!        Check the state change status. We expect that an update is
!        indicated.
!
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF error case: try to load one more ' &
      //           'earth DSK.' )

      I = FTSIZE + 1

      CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
      CALL CHCKXC   ( .TRUE., 'SPICE(FTFULL)', OK )
!
!     The error message above brought to you by ZZDDHOPN.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKLSF: unload first two DSKs and load ' &
      //           'remaining earth DSKs, except for the last one.' )


      DO I = 1, 2

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
!
!        Check the state change status. We expect that an update is
!        indicated.
!
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      END DO

      DO I = 1, 2

         J = FTSIZE + I

         CALL ZZDSKLSF ( DSKS(J), HANDLS(J) )

         CALL CHCKXC   ( .FALSE., ' ', OK )
!
!        Check the state change status. We expect that an update is
!        indicated.
!
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: prepare to look up all earth ' &
      //           'segments but those in the last earth file. ' &
      //           'Force ZZDSKBSR to build a segment list ' &
      //           'for the earth via a call to ZZDSKBBL.'            )

      BODYID = 399



      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that no update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: look up all loaded ' &
      //           'earth segments ' )

!
!     Note that the last earth file is not loaded.
!

      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that no update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = STSIZE
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find STSIZE segments.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         I      = I    + 1
         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1

!
!        Check the state change status. We expect that no update is
!        indicated.
!
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )

!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( ODD(I) ) THEN
!
!           Start a backward search for the expected
!           segment.
!
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
!
!           Continue the backward search for the
!           expected segment.
!
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', &
                                     XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', &
                                     XDSKDS, DSKDSZ, 0.D0, OK )

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     We should have found STSIZE segments.
!
      CALL CHCKSI ( 'num. segments found', I, '=', STSIZE, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: repeat look up.' )


      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that no update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = STSIZE
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find STSIZE segments.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         I      = I    + 1
         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1

!
!        Check the state change status. We expect that no update is
!        indicated.
!
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )

!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( ODD(I) ) THEN
!
!           Start a backward search for the expected
!           segment.
!
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
!
!           Continue the backward search for the
!           expected segment.
!
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', &
                                     XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', &
                                     XDSKDS, DSKDSZ, 0.D0, OK )

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     We should have found STSIZE segments.
!
      CALL CHCKSI ( 'num. segments found', I, '=', STSIZE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: unload even-indexed ' &
      //           'earth DSKs. Look up remaining segments. ' )


      DO I = 4, 2+FTSIZE, 2

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

!
!     Start a new search.
!
      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )
!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = STSIZE

!
!     I is the segment offset from the head of the list. I is 1-based.
!     K is the iteration count.

      I = 0
      K = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find STSIZE/2 segments.
!
         K = K + 1

         IF ( EVEN(I) ) THEN

            I = I + 3
         ELSE
            I = I + 1
         END IF

         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1

!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( EVEN(J) ) THEN
!
!           Start a backward search in the file at index FILENO for the
!           expected segment.
!
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
!
!           Continue the backward search for the
!           expected segment.
!
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', &
                                     XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', &
                                     XDSKDS, DSKDSZ, 0.D0, OK )

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     We should have found STSIZE/2 segments.
!
      CALL CHCKSI ( 'num. segments found', K, '=', STSIZE/2, 0, OK )



!
!     Reload the earth kernels indexed 1 through FTSIZE to restore
!     the original file priorities.
!
      DO I = 3, FTSIZE+2

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     Force construction of a new segment list.
!
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: unload odd-indexed ' &
      //           'earth DSKs. Look up remaining segments. ' )


      DO I = 3, 2+FTSIZE, 2

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

!
!     Start a new search.
!
      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )
!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = STSIZE

!
!     I is the segment offset from the head of the list. I is 1-based.
!     K is the iteration count.

      I = -2
      K =  0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
!
!        We should find STSIZE/2 segments.
!
         K = K + 1


         IF ( EVEN(I) ) THEN

            I = I + 3
         ELSE
            I = I + 1
         END IF

         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1


!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( EVEN(J) ) THEN
!
!           Start a backward search in the file at index FILENO for the
!           expected segment.
!
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
!
!           Continue the backward search for the
!           expected segment.
!
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', &
                                     XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', &
                                     XDSKDS, DSKDSZ, 0.D0, OK )

!
!        Fetch information for the next segment.
!
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     We should have found STSIZE/2 segments.
!
      CALL CHCKSI ( 'num. segments found', K, '=', STSIZE/2, 0, OK )


!
!     Reload the earth kernels indexed 1 through FTSIZE to restore
!     the original file priorities.
!
      DO I = 3, FTSIZE+2

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


!
!     Force construction of a new segment list.
!
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unload the first earth DSK. ' &
      //           'Load the final earth DSK. Attempt a search.' )
!
!     The segment list should run out of room while the front of the
!     list is built. This should force the reconstruction of the list,
!     with eventual failure when the tail of the list cannot be
!     accommodated.
!
      CALL ZZDSKUSF ( HANDLS(3) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      K = 2 + NEARTH

      CALL ZZDSKLSF ( DSKS(K), HANDLS(K) )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
!     Check the state change status. We expect that no update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search over all segments. We expect a failure.
!
!     Use ZZDSKNOT to force a complete search.
!

      CALL ZZDSKSNS ( ZZDSKNOT, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(BUFFEROVERFLOW)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Search for one earth segment. It should be ' &
      //           'located in the last file loaded.'            )



      K = 2 + NEARTH

      CALL ZZDSKLSF ( DSKS(K), HANDLS(K) )
      CALL CHCKXC   ( .FALSE., ' ', OK )
!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that no update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      NSEG = STSIZE

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Fetch data to compare.
!
      CALL DLABBS ( HANDLS(K), XDLADS, FND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

!
!     Check the other outputs.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(K), 0, OK )


      CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

      CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Look up earth segments until we encounter a ' &
      //           'failure. We don''t have enough room in the ' &
      //           'segment table for all earth segments.'       )


      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKSNS ( ZZDSKNOT, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(BUFFEROVERFLOW)', OK )

!
!     Unload the last earth DSK.
!
      CALL ZZDSKUSF ( HANDLS(2+NEARTH) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Restore the first FTSIZE earth DSKs.
!
      DO I = 3, FTSIZE+2

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     Force construction of a new segment list.
!
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create a DSK file containing 10 ' &
      //           'segments for body 699.'                    )


!
!     To even create a new DSK, we need to unload one of them,
!     since we'll open a new DSK. We normally would need to
!     unload two files to accommodate the scratch DAS used for
!     DAS segregation, but T_SMLDSK doesn't segregate its output
!     file.
!
      CALL ZZDSKUSF ( HANDLS(FTSIZE+2) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      NSAT = 10


      I = FTSIZE + 3

      DSKS(I) = 'zzdskbsr_test_#.bds'
      CALL REPMI ( DSKS(I), '#', I, DSKS(I) )

      IF ( EXISTS(DSKS(I)) ) THEN

         CALL DELFIL ( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      BODYID  = 699
      FRAME   = 'IAU_SATURN'

      DO J = 1, NSAT

         SURFID  = J
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: load ' &
      //           'the Saturn DSK. Look up the Saturn segments. ' &
      //           'This look-up should cause the 399, 499, ' &
      //           'and 599 segment lists to be dumped.'          )

!
!     Load the first DSK and find data for 499 and 599, so we have
!     segment lists that need to be removed. We have too many files
!     loaded, so unload an earth file first.
!
      CALL ZZDSKUSF ( HANDLS(3) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKLSF ( DSKS(1), HANDLS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKBBL ( 499 )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKBBL ( 599 )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      I = FTSIZE + 3

      CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Check the state change status. We expect that an update is
!     indicated.
!
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 699

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Prepare segment selection test function for use in BSR search.
!
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Perform the search.
!
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     We should find NSAT segments from DSK #(FTSIZE+3).
!
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )
      CALL DLABBS ( HANDLS(I), XDLADS, FND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

      J = 0

      DO WHILE ( FOUND .AND. FND .AND. ( J .LT. NSAT) )

         J = J + 1
!
!        Check the other outputs.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(I), 0, OK )


         CALL DSKGD ( HANDLS(I), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )


!
!        Continue to search backwards.
!
         CALL DLAFPS ( HANDLS(I), XDLADS, PRVDSC, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSF test: cause 699 body table entry ' &
      //           'to be deleted, when 699 is not at index NBT' )

!
!     To set this up, we'll put the earth body table entry at
!     the end of the table. We need to do an earth lookup to
!     get the earth back into the body table, while avoiding
!     removal of the Saturn list. Make the set of earth segments
!     smaller by unloading some earth files.
!
      DO I = 3, 12
         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
      END DO
!
!     Now do an earth search.
!
      BODYID = 399
      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     All of the preceding was to get the earth into the last
!     position of the body table. Now unload the Saturn file.
!
      K = FTSIZE + 3

      CALL ZZDSKUSF ( HANDLS(K) )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSNS test: search a file that has more ' &
      //           'than STSIZE segments.'                      )

!
!     Create a monster DSK.
!
      OVRDSK = 'zzdskbsr_toobig.bds'


      IF ( EXISTS(OVRDSK) ) THEN

         CALL DELFIL ( OVRDSK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      BODYID  = 999

      DO I = 1, STSIZE + 1

         FRAME   = 'IAU_PLUTO'
         SURFID  = I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, OVRDSK )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     Try to build a segment list for 999.
!
      CALL ZZDSKLSF ( OVRDSK, OVRHAN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .TRUE., 'SPICE(BUFFEROVERFLOW)', OK )


!
!     Clean up.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )



      DO I = 1, FTSIZE + 3

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZDSKUSF ( BTHAN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL DELFIL   ( BTDSK )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKUSF ( OVRHAN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL DELFIL   ( OVRDSK )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


