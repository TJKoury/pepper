!$Procedure F_CKFROT ( Test Family for CKFROT )

      SUBROUTINE F_CKFROT ( OK )
      use pepper_legacy, pepper_legacy_frame => frame

! $ Abstract
!
!     Test the routine CKFROT.
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
!     There are several test cases. Several to make sure that nothing
!     is found if an instrument is not loaded. Another to make sure
!     the correct frame is located when the instrument is loaded.
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.1, 29-MAY-2021 (JDR)
!
!        Updated header and comments to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 14-MAR-2000 (WLT)
!
!        Added three test cases to make sure that the case when no
!        C-kernel is loaded is dealt with correctly as well as
!        the case when no SCLK kernel is loaded.
!
!-    SPICELIB Version 1.0.0, 06-OCT-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     test CKFROT routine
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Variables
!

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    ERROR

      DOUBLE PRECISION      AV    ( 3    )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROT   ( 3, 3 )
      DOUBLE PRECISION      XFORM ( 3, 3 )
      DOUBLE PRECISION      XFORM2( 3, 3 )


      INTEGER               EFRAME
      INTEGER               FRAME
      INTEGER               HANDLE

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CKFROT' )

      CALL KILFIL ( 'TEST.CK'   )
      CALL KILFIL ( 'TEST.SCLK' )


!
!     *****************************************************************
!
!     Normal cases
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure that we do not signal an error ' &
      //           'when no C or SCLK kernels have been loaded. ' )


      CALL TPARSE ( '1-JAN-1990',     ET,    ERROR )
      CALL CKFROT ( -9998, ET, XFORM, FRAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure nothing is found if we don''t have ' &
      //           'a C-kernel. ' )

!
!     Create a test (but don't load) C-kernel; load the
!     associated SCLK.
!
      CALL TSTCKN ( 'TEST.CK', 'TEST.SCLK', .FALSE., .TRUE., &
                                            .TRUE.,   HANDLE  )

      CALL TPARSE ( '1-JAN-1990',     ET,    ERROR )
      CALL CKFROT ( -9998, ET, XFORM, FRAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure nothing is found if we don''t have ' &
      //           'an instrument in the C-kernel. ' )

!
!     Load a test C-kernel.
!
      CALL CKLPF  ( 'TEST.CK',  HANDLE )

      CALL TPARSE ( '1-JAN-1990',     ET,    ERROR )
      CALL CKFROT ( -9998, ET, XFORM, FRAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure that when data is available, that ' &
      //           'a frame transformation if found and that it ' &
      //           'has the correct value. ' )


      CALL CKFROT ( -9999, ET, XFORM, FRAME, FOUND  )
      CALL TSTATD (        ET, ROT,   AV            )
      CALL XPOSE  ( ROT,   XFORM2 )

      CALL NAMFRM ( 'GALACTIC', EFRAME              )

      CALL CHCKXC ( .FALSE.,               ' ',                OK )
      CALL CHCKSL ( 'FOUND', FOUND,        .TRUE.,             OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=',   EFRAME,    0,       OK )
      CALL CHCKAD ( 'XFORM', XFORM, '||',  XFORM2, 9, 1.0D-14, OK )
      CALL CHCKAD ( 'XFORM', XFORM, '~~/', XFORM2, 9, 1.0D-14, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check to make sure CKMETA is properly used ' &
      //           'and that we get the expected frame ' &
      //           'transformation. (Object -10000)' )



      CALL CKFROT ( -10000, ET, XFORM, FRAME, FOUND  )
      CALL TSTATD (         ET, ROT,   AV            )
      CALL XPOSE  ( ROT,    XFORM2              )

      CALL NAMFRM ( 'FK4', EFRAME              )

      CALL CHCKXC ( .FALSE.,               ' ',                OK )
      CALL CHCKSL ( 'FOUND', FOUND,        .TRUE.,             OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=',   EFRAME,    0,       OK )
      CALL CHCKAD ( 'XFORM', XFORM, '||',  XFORM2, 9, 1.0D-14, OK )
      CALL CHCKAD ( 'XFORM', XFORM, '~~/', XFORM2, 9, 1.0D-14, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Same test for object -10001' )


      CALL CKFROT ( -10001, ET, XFORM, FRAME, FOUND  )
      CALL TSTATD (         ET, ROT,   AV            )

      CALL XPOSE  ( ROT,    XFORM2              )

      CALL NAMFRM ( 'J2000', EFRAME              )

      CALL CHCKXC ( .FALSE.,               ' ',                OK )
      CALL CHCKSL ( 'FOUND', FOUND,        .TRUE.,             OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=',   EFRAME,    0,       OK )
      CALL CHCKAD ( 'XFORM', XFORM, '||',  XFORM2, 9, 1.0D-14, OK )
      CALL CHCKAD ( 'XFORM', XFORM, '~~/', XFORM2, 9, 1.0D-14, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Perform the same test as before, but clear ' &
      //           'the kernel pool first. We should not find a ' &
      //           'transformation now, because there is no SCLK ' &
      //           'data available. ' )


      CALL CLPOOL ( )

      CALL TPARSE ( '1-JAN-1990',     ET,    ERROR )
      CALL CKFROT ( -10001, ET, XFORM, FRAME, FOUND  )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



      CALL CKUPF  (  HANDLE     )
      CALL KILFIL ( 'TEST.CK'   )
      CALL KILFIL ( 'TEST.SCLK' )

      CALL T_SUCCESS ( OK )
      RETURN
      END
