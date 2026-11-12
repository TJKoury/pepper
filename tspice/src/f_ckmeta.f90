!$Procedure F_CKMETA ( Family of CKMETA tests )

      SUBROUTINE F_CKMETA ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the routine CKMETA.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.1, 29-MAY-2021 (JDR)
!
!        Updated header and comments to comply with NAIF standard.
!
!-    TSPICE Version 2.0.0, 09-SEP-2013 (BVS)
!
!        Added more tests.
!
!-    TSPICE Version 1.0.0, 09-SEP-2013 (WLT)
!
!-&


! $ Index_Entries
!
!     test CKMETA
!
!-&


!
!     Test Utility Functions
!
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT

!
!     SPICELIB Functions
!

!
!     Local parameters.
!
!     NCK is the size of the buffer in CKMETA
!
      INTEGER               NCK
      PARAMETER           ( NCK = 30 )

!
!     NIDS is the number of IDs we will test.
!
      INTEGER               NIDS
      PARAMETER           ( NIDS = NCK * 2 + 13 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

!
!     Local Variables
!
      CHARACTER*(WDSIZE)    SCLKWS ( NIDS )
      CHARACTER*(WDSIZE)    SCLKWB ( NIDS )
      CHARACTER*(WDSIZE)    SPKKWS ( NIDS )
      CHARACTER*(WDSIZE)    SPKKWB ( NIDS )
      CHARACTER*(WDSIZE*2)  KEYWRD

      INTEGER               CKSM   ( NIDS )
      INTEGER               CKBG   ( NIDS )
      INTEGER               SCLKSM ( NIDS )
      INTEGER               SCLKBG ( NIDS )
      INTEGER               SCLKPS ( NIDS )
      INTEGER               SCLKPB ( NIDS )
      INTEGER               SPKSM  ( NIDS )
      INTEGER               SPKBG  ( NIDS )
      INTEGER               SPKPS  ( NIDS )
      INTEGER               SPKPB  ( NIDS )

      INTEGER               IDCODE
      INTEGER               SCLKID
      INTEGER               SPKID

      INTEGER               SUNIT
      INTEGER               I
      INTEGER               J

!
!     Save everything.
!
      SAVE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CKMETA' )

!
!     Populate test arrays and make test text kernel.
!
      CALL KILFIL ( 'myconns.ker' )
      CALL TXTOPN ( 'myconns.ker', SUNIT )
      CALL WRITLN ( BEGDAT(), SUNIT )

      DO I = 1, NIDS

!
!        CK IDs greater than -1000 and CKs IDs equal to or less than
!        -1000.
!
         CKSM   ( I ) = -1000 + I
         CKBG   ( I ) = -1000 * I - MOD( I, 1000 )

!
!        Default SCLK and SPK IDs for CK IDs greater than -1000.
!
         SCLKSM ( I ) = 0
         SPKSM  ( I ) = 0

!
!        Default SCLK and SPK IDs for CKs IDs equal to or less than
!        -1000.
!
         SCLKBG ( I ) = -I
         SPKBG  ( I ) = -I

!
!        POOL IDs for SCLK and SPK IDs for CK IDs greater than -1000.
!
         SCLKPS ( I ) = CKBG ( I )
         SPKPS  ( I ) = CKBG ( I ) - 2

!
!        POOL IDs SCLK and SPK IDs for CKs IDs equal to or less than
!        -1000.
!
         SCLKPB ( I ) = CKBG ( I ) - 1
         SPKPB  ( I ) = CKBG ( I ) - 3

!
!        POOL variables and agents.
!
         KEYWRD = 'CK_#_SCLK = #'
         CALL REPMI  ( KEYWRD, '#', CKSM ( I ),   KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SCLKPS ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SCLKWS ( I ), J )

         KEYWRD = 'CK_#_SPK = #'
         CALL REPMI  ( KEYWRD, '#', CKSM ( I ),  KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SPKPS ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SPKKWS ( I ), J )

         KEYWRD = 'CK_#_SCLK = #'
         CALL REPMI  ( KEYWRD, '#', CKBG ( I ),   KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SCLKPB ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SCLKWB ( I ), J )

         KEYWRD = 'CK_#_SPK = #'
         CALL REPMI  ( KEYWRD, '#', CKBG ( I ),  KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SPKPB ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SPKKWB ( I ), J )

      END DO

      CALL WRITLN ( BEGTXT(), SUNIT )
      CLOSE ( SUNIT  )

      CALL CLPOOL ( )

!
!     *****************************************************************
!
!        Error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Test exception for unrecognized meta item.
!
      CALL TCASE ( 'Unrecognized CKMETA item' )

      IDCODE = 1

      CALL CKMETA ( -10000, 'IK', IDCODE )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNCKMETA)',  OK )
      CALL CHCKSI ( 'IDCODE', IDCODE, '=', 0, 0,     OK )


!
!     *****************************************************************
!
!        Normal cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Test default IDs.
!
      CALL TCASE ( 'Default ID codes both SCLK and SPK.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKSM( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKSM( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKBG( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKBG( I ), 0, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
!     Test POOL IDs.
!
      CALL LDPOOL( 'myconns.ker' )

      CALL TCASE ( 'POOL ID codes for both SCLK and SPK. ' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPS( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPS( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPB( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPB( I ), 0, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
!     Test default SCLK IDs and POOL SPK IDs
!
      DO I = 1, NIDS
         CALL DVPOOL ( SCLKWS ( I ) )
         CALL DVPOOL ( SCLKWB ( I ) )
      END DO

      CALL TCASE ( 'Default ID codes for SCLK, POOL ID codes for SPK.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKSM( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPS( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKBG( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPB( I ), 0, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
!     Test POOL IDs again to make sure that all watchers are set.
!
      CALL CLPOOL ( )

      CALL LDPOOL( 'myconns.ker' )

      CALL TCASE ( 'POOL ID codes for both SCLK and SPK, pass 2. ' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPS( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPS( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPB( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPB( I ), 0, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
!     Test default SPK IDs and POOL SCLK IDs
!
      DO I = 1, NIDS
         CALL DVPOOL ( SPKKWS ( I ) )
         CALL DVPOOL ( SPKKWB ( I ) )
      END DO

      CALL TCASE ( 'Default ID codes for SPK, POOL ID codes for SCLK.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPS( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKSM( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPB( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKBG( I ), 0, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
!     Clear POOL and test default IDs again.
!
      CALL CLPOOL ( )

      CALL TCASE ( 'Default ID codes both SCLK and SPK, pass 2.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKSM( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKSM( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKBG( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKBG( I ), 0, OK )

      END DO

!
!     Clean up.
!
      CALL KILFIL ( 'myconns.ker' )

      CALL T_SUCCESS ( OK )
      RETURN
      END
