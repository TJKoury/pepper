C$Procedure F_CKFROT ( Test Family for CKFROT )

      SUBROUTINE F_CKFROT ( OK )

C$ Abstract
C
C     Test the routine CKFROT.
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
C     There are several test cases. Several to make sure that nothing
C     is found if an instrument is not loaded. Another to make sure
C     the correct frame is located when the instrument is loaded.
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
C-    TSPICE Version 2.0.1, 29-MAY-2021 (JDR)
C
C        Updated header and comments to comply with NAIF standard.
C
C-    SPICELIB Version 2.0.0, 14-MAR-2000 (WLT)
C
C        Added three test cases to make sure that the case when no
C        C-kernel is loaded is dealt with correctly as well as
C        the case when no SCLK kernel is loaded.
C
C-    SPICELIB Version 1.0.0, 06-OCT-1999 (WLT)
C
C-&


C$ Index_Entries
C
C     test CKFROT routine
C
C-&


C
C     Test Utility Functions
C


C
C     SPICELIB Functions
C

C
C     Local Variables
C

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

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CKFROT' )

      CALL KILFIL ( 'TEST.CK'   )
      CALL KILFIL ( 'TEST.SCLK' )


C
C     *****************************************************************
C
C     Normal cases
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that we do not signal an error '
     .//           'when no C or SCLK kernels have been loaded. ' )


      CALL TPARSE ( '1-JAN-1990',     ET,    ERROR )
      CALL CKFROT ( -9998, ET, XFORM, FRAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure nothing is found if we don''t have '
     .//           'a C-kernel. ' )

C
C     Create a test (but don't load) C-kernel; load the
C     associated SCLK.
C
      CALL TSTCKN ( 'TEST.CK', 'TEST.SCLK', .FALSE., .TRUE.,
     .                                      .TRUE.,   HANDLE  )

      CALL TPARSE ( '1-JAN-1990',     ET,    ERROR )
      CALL CKFROT ( -9998, ET, XFORM, FRAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure nothing is found if we don''t have '
     .//           'an instrument in the C-kernel. ' )

C
C     Load a test C-kernel.
C
      CALL CKLPF  ( 'TEST.CK',  HANDLE )

      CALL TPARSE ( '1-JAN-1990',     ET,    ERROR )
      CALL CKFROT ( -9998, ET, XFORM, FRAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that when data is available, that '
     .//           'a frame transformation if found and that it '
     .//           'has the correct value. ' )


      CALL CKFROT ( -9999, ET, XFORM, FRAME, FOUND  )
      CALL TSTATD (        ET, ROT,   AV            )
      CALL XPOSE  ( ROT,   XFORM2 )

      CALL NAMFRM ( 'GALACTIC', EFRAME              )

      CALL CHCKXC ( .FALSE.,               ' ',                OK )
      CALL CHCKSL ( 'FOUND', FOUND,        .TRUE.,             OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=',   EFRAME,    0,       OK )
      CALL CHCKAD ( 'XFORM', XFORM, '||',  XFORM2, 9, 1.0D-14, OK )
      CALL CHCKAD ( 'XFORM', XFORM, '~~/', XFORM2, 9, 1.0D-14, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check to make sure CKMETA is properly used '
     .//           'and that we get the expected frame '
     .//           'transformation. (Object -10000)' )



      CALL CKFROT ( -10000, ET, XFORM, FRAME, FOUND  )
      CALL TSTATD (         ET, ROT,   AV            )
      CALL XPOSE  ( ROT,    XFORM2              )

      CALL NAMFRM ( 'FK4', EFRAME              )

      CALL CHCKXC ( .FALSE.,               ' ',                OK )
      CALL CHCKSL ( 'FOUND', FOUND,        .TRUE.,             OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=',   EFRAME,    0,       OK )
      CALL CHCKAD ( 'XFORM', XFORM, '||',  XFORM2, 9, 1.0D-14, OK )
      CALL CHCKAD ( 'XFORM', XFORM, '~~/', XFORM2, 9, 1.0D-14, OK )

C
C     --- Case: -------------------------------------------------------
C
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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Perform the same test as before, but clear '
     .//           'the kernel pool first. We should not find a '
     .//           'transformation now, because there is no SCLK '
     .//           'data available. ' )


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
