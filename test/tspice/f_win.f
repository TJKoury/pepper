C$Procedure F_WIN ( Window family tests )

      SUBROUTINE F_WIN ( OK )

C$ Abstract
C
C     This routine tests SPICELIB window routines.
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
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.1, 05-OCT-2021 (EDW)
C
C        Altered string length constant, 116 to 115, to 
C        reflect typo corrections in ZZWNINSD.
C
C-    TSPICE Version 1.0.0, 03-MAR-2009 (EDW)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               POS

C     
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 20000 )


C
C     Local variables
C
      CHARACTER*(320)       MSG

      DOUBLE PRECISION      RESULT ( LBCELL : MAXVAL )

      INTEGER               M
      DOUBLE PRECISION      MEAS
      DOUBLE PRECISION      AVG
      DOUBLE PRECISION      STDDEV
      INTEGER               SHORT
      INTEGER               LONG 

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_WIN' )


C
C   Case 1
C
C
      CALL TCASE ( 'Check ZZWNINSD error cases' )

      CALL SSIZED ( MAXVAL, RESULT )
      CALL SCARDD ( 0,      RESULT )

C
C     Try to add an invalid interval to the window.
C
      CALL ZZWNINSD ( 0.D0, -1.D0, 'TEST_STRING_1', RESULT  )

      CALL GETMSG ( 'LONG' , MSG )
      CALL CHCKXC ( .TRUE., 'SPICE(BADENDPOINTS)', OK )

      M = POS ( MSG, 'TEST_STRING_1', 1 )
      CALL CHCKSI ( 'TEST_STRING_1', M, '=', 115, 0, OK )

C
C     Empty the window, then reduce size to zero.
C
      CALL SSIZED ( 0, RESULT )
      CALL SCARDD ( 0, RESULT )

C
C     Try to add an interval to the window.
C
      CALL ZZWNINSD ( 0.D0,  1.D0, 'TEST_STRING_2', RESULT )

      CALL GETMSG ( 'LONG' , MSG )
      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)', OK )

      M = POS ( MSG, 'TEST_STRING_2', 1 )
      CALL CHCKSI ( 'TEST_STRING_2', M, '=', 90, 0, OK )




C
C   Case 2
C
C
      CALL TCASE ( 'Check WNSUMD error case' )

C
C     An odd cardinality for a WNSUMD input window should
C     signal an error.
C
      CALL SSIZED ( MAXVAL, RESULT )
      CALL SCARDD ( 13,      RESULT )
      
      CALL WNSUMD ( RESULT, MEAS,  AVG, STDDEV,
     .                      SHORT, LONG )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCARDINALITY)', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END



C
C23456789012345678901234567890123456789012345678901234567890123456789012
C
