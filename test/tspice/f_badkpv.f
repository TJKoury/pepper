C$Procedure F_BADKPV (Family of tests for BADKPV )

      SUBROUTINE F_BADKPV ( OK )

C$ Abstract
C
C     This routine checks all of the various tests possible
C     in the routine BADKPV.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routine BADKPV.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 07-DEC-2020 (JDR)
C
C        Added test case for unknown expected kernel pool variable type.
C
C        Edited the header and comments to comply with NAIF standard.
C
C        Added $Version history.
C
C-    TSPICE Version 1.0.0, 09-APR-1997 (WLT)
C
C-&


C$ Index_Entries
C
C     Test family for BADKPV routine
C
C-&


C
C     Test Utility Functions
C
      CHARACTER*(32)        BEGDAT
      CHARACTER*(32)        BEGTXT

C
C     SPICELIB Functions
C
      LOGICAL               BADKPV

C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 6 )

      CHARACTER*(LNSIZE)    LINES ( NLINES )

      LOGICAL               LNUM
      LOGICAL               LSTR

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_BADKPV' )

      LINES(  1 ) = BEGTXT()
      LINES(  2 ) = 'This text kernel is for test purposes on'
     .//            'ly.  It serves no'
      LINES(  3 ) = 'other purpose'
      LINES(  4 ) = BEGDAT()
      LINES(  5 ) = 'NUM = ( 1, 2, 3, 4, 5 )'
      LINES(  6 ) = 'STR = ( ''This'', ''is'', ''a'', ''strin'
     .//            'g'', ''of'', ''words.'' )'

      CALL KILFIL ( 'sample.txt' )
      CALL TSTTXT ( 'sample.txt', LINES, 6, .TRUE., .TRUE. )

      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     *****************************************************************
C
C     Error cases: BADKPV
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled when a '
     .//           'character variable has integer values. ' )


      LNUM = BADKPV ( 'TEST', 'NUM', '=', 5, 1, 'C' )


      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled when an '
     .//           'numeric variable has character values ' )

      LSTR = BADKPV ( 'TEST', 'STR', '=', 6, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled when a '
     .//           'variable has a dimension that is not the '
     .//           'exact expected value ')

      LNUM = BADKPV ( 'TEST', 'NUM', '=', 6, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled when a '
     .//           'variable has a dimension that is not more '
     .//           'than an expected value. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '>', 5, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LNUM', LNUM,          .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled '
     .//           'when a variable has a dimension that is not '
     .//           'at least an expected value. ' )

      LSTR = BADKPV ( 'TEST', 'STR', '=>', 7, 1, 'C' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled when a '
     .//           'variable has a dimension that is not at most '
     .//           'an expected value. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '<=', 4, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled '
     .//           'when a variable has a dimension that is not '
     .//           'less than some expected value. ' )

      LSTR = BADKPV ( 'TEST', 'STR', '<', 6, 1, 'C' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR, .TRUE.,             OK )
      LNUM = .FALSE.
      LSTR = .FALSE.

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled '
     .//           'when the dimension of a variable is not '
     .//           'divisible by the prescribed value. ' )

      LSTR = BADKPV ( 'TEST', 'STR', '=', 6, 4, 'C' )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)',  OK )
      CALL CHCKSL ( 'LSTR',  LSTR,         .TRUE.,     OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled '
     .//           'when a variable is not found in the kernel '
     .//           'pool. ' )

      LNUM = BADKPV ( 'TEST', 'SPK', '=', 1, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(VARIABLENOTFOUND)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,     OK )

      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled '
     .//           'when the comparison operator is not '
     .//           'recognized. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '?', 5, 1, 'N' )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNCOMPARE)',  OK )
      CALL CHCKSL ( 'LNUM',  LNUM,         .TRUE.,    OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure an error is signaled '
     .//           'when the expected kernel variable type is not '
     .//           'recognized. ' )

      LNUM = BADKPV ( 'TEST', 'NUM', '=', 5, 1, '?' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTYPE)',    OK )
      CALL CHCKSL ( 'LNUM',  LNUM,        .TRUE.,    OK )
      LNUM = .FALSE.
      LSTR = .FALSE.


C
C     *****************************************************************
C
C     Normal cases: BADKPV
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure the ''='' operator behaves as '
     .//           'expected ' )



      LSTR = BADKPV ( 'TEST', 'STR', '=', 6, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '=', 5, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure the ''>'' operator behaves as '
     .//           'expected ' )


      LSTR = BADKPV ( 'TEST', 'STR', '>', 4, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '>', 4, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure the ''=>'' operator behaves as '
     .//           'expected ' )



      LSTR = BADKPV ( 'TEST', 'STR', '=>', 6, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '=>', 5, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure the ''<'' operator behaves as '
     .//           'expected ' )


      LSTR = BADKPV ( 'TEST', 'STR', '<', 7, 2, 'C' )
      LNUM = BADKPV ( 'TEST', 'NUM', '<', 6, 5, 'N' )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'LNUM', LNUM, .FALSE.,  OK )
      CALL CHCKSL ( 'LSTR', LSTR, .FALSE.,  OK )
      LNUM = .TRUE.
      LSTR = .TRUE.


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure the ''<='' operator behaves as '
     .//           'expected ' )


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
