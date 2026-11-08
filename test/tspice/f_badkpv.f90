!$Procedure F_BADKPV (Family of tests for BADKPV )

      SUBROUTINE F_BADKPV ( OK )

! $ Abstract
!
!     This routine checks all of the various tests possible
!     in the routine BADKPV.
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
!     This routine tests the SPICELIB routine BADKPV.
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
!-    TSPICE Version 1.1.0, 07-DEC-2020 (JDR)
!
!        Added test case for unknown expected kernel pool variable type.
!
!        Edited the header and comments to comply with NAIF standard.
!
!        Added $Version history.
!
!-    TSPICE Version 1.0.0, 09-APR-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     Test family for BADKPV routine
!
!-&


!
!     Test Utility Functions
!
      CHARACTER*(32)        BEGDAT
      CHARACTER*(32)        BEGTXT

!
!     SPICELIB Functions
!
      LOGICAL               BADKPV

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 6 )

      CHARACTER*(LNSIZE)    LINES ( NLINES )

      LOGICAL               LNUM
      LOGICAL               LSTR

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_BADKPV' )

      LINES(  1 ) = BEGTXT()
      LINES(  2 ) = 'This text kernel is for test purposes on' &
      //            'ly.  It serves no'
      LINES(  3 ) = 'other purpose'
      LINES(  4 ) = BEGDAT()
      LINES(  5 ) = 'NUM = ( 1, 2, 3, 4, 5 )'
      LINES(  6 ) = 'STR = ( ''This'', ''is'', ''a'', ''strin' &
      //            'g'', ''of'', ''words.'' )'

      CALL KILFIL ( 'sample.txt' )
      CALL TSTTXT ( 'sample.txt', LINES, 6, .TRUE., .TRUE. )

      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     *****************************************************************
!
!     Error cases: BADKPV
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled when a ' &
      //           'character variable has integer values. ' )


      LNUM = BADKPV ( 'TEST', 'NUM', '=', 5, 1, 'C' )


      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled when an ' &
      //           'numeric variable has character values ' )

      LSTR = BADKPV ( 'TEST', 'STR', '=', 6, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled when a ' &
      //           'variable has a dimension that is not the ' &
      //           'exact expected value ')

      LNUM = BADKPV ( 'TEST', 'NUM', '=', 6, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled when a ' &
      //           'variable has a dimension that is not more ' &
      //           'than an expected value. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '>', 5, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LNUM', LNUM,          .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled ' &
      //           'when a variable has a dimension that is not ' &
      //           'at least an expected value. ' )

      LSTR = BADKPV ( 'TEST', 'STR', '=>', 7, 1, 'C' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled when a ' &
      //           'variable has a dimension that is not at most ' &
      //           'an expected value. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '<=', 4, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled ' &
      //           'when a variable has a dimension that is not ' &
      //           'less than some expected value. ' )

      LSTR = BADKPV ( 'TEST', 'STR', '<', 6, 1, 'C' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled ' &
      //           'when the dimension of a variable is not ' &
      //           'divisible by the prescribed value. ' )

      LSTR = BADKPV ( 'TEST', 'STR', '=', 6, 4, 'C' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR,         .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled ' &
      //           'when a variable is not found in the kernel ' &
      //           'pool. ' )

      LNUM = BADKPV ( 'TEST', 'SPK', '=', 1, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(VARIABLENOTFOUND)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,     OK )

      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled ' &
      //           'when the comparison operator is not ' &
      //           'recognized. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '?', 5, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNCOMPARE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,    OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure an error is signaled ' &
      //           'when the expected kernel variable type is not ' &
      //           'recognized. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '=', 5, 1, '?' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTYPE)',    OK )
      CALL CHCKSL ( 'LNUM',  LNUM,        .TRUE.,    OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


!
!     *****************************************************************
!
!     Normal cases: BADKPV
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure the ''='' operator behaves as ' &
      //           'expected ' )



      LSTR = BADKPV ( 'TEST', 'STR', '=', 6, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '=', 5, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure the ''>'' operator behaves as ' &
      //           'expected ' )


      LSTR = BADKPV ( 'TEST', 'STR', '>', 4, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '>', 4, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure the ''=>'' operator behaves as ' &
      //           'expected ' )



      LSTR = BADKPV ( 'TEST', 'STR', '=>', 6, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '=>', 5, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure the ''<'' operator behaves as ' &
      //           'expected ' )


      LSTR = BADKPV ( 'TEST', 'STR', '<', 7, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '<', 6, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure the ''<='' operator behaves as ' &
      //           'expected ' )


      LSTR = BADKPV ( 'TEST', 'STR', '<=', 6, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '<=', 5, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.


      CALL KILFIL ( 'sample.txt' )
      CALL T_SUCCESS ( OK )
      RETURN
      END
