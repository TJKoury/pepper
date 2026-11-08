C$Procedure      SHOERR ( Show Errors )
 
      SUBROUTINE SHOERR
 
C$ Abstract
C
C     This routine sets the error handling within a test case so
C     that error messages are sent to the screen.  When the
C     test case ends, messages are once again sent to 'NULL'
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
C     TESTING
C
C$ Declarations
 
      IMPLICIT NONE
 
 
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine allows you to cause error messages issued through
C     the SPICE exception handling system to be sent to the screen
C     instead of being blocked from output by the Test Utilities
C     library.
C
C     However, this effect lasts only for the duration of a single
C     test case.  Once a new test case or family is specified
C     by a call to TCASE or TOPEN, error messages will once again
C     be prohibited from being displayed on the terminal window.
C
C     This routine is primarily useful in the debugging stages of
C     a test case or test family.
C
C$ Examples
C
C     Suppose that you are creating a test case within a family of
C     test cases and wish to have all text associated with an
C     exception that may arise within the scope of a test case
C     to be sent to standard output.  Simply set up your
C     test case in this way.
C
C        CALL TCASE ( 'Name of the test case.' )
C        CALL SHOERR
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    Test Utilities Version 1.0.0, 27-JUL-1999 (WLT)
C
C
C-&
 
      CALL ERRDEV ( 'SET', 'SCREEN' )
      RETURN
 
      END
