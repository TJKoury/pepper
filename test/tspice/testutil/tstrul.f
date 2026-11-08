C$Procedure      TSTRUL (Test Rules)
C
      SUBROUTINE TSTRUL
C
C$ Abstract
C
C     This routine creates the rules that are used to delimit test
C     cases when testing in verbose mode.
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
C      None.
C
C$ Keywords
C
C       Testing
C
C$ Declarations
C
      IMPLICIT NONE
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      None.
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
C      None.
C
C$ Files
C
C      This routine calls TSTLOG to write to write to the screen
C      and test log file.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is a utility routine for writing the rules that are used
C     to mark the boundaries of test cases when a test program is
C     running in verbose mode.
C
C     The code is isolated here so that any part of the test
C     utiltities that may need to create the rules can do so.
C
C     Also this allows NAIF to easily change the appearence of these
C     rules if it is deemed necessary without having to search the
C     entire set of test utilities to find out where they might be
C     present.
C
C$ Examples
C
C     See TOPEN or TCLOSE.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    Testing Utilities 1.0.0, 5-DEC-1994 (WLT)
C
C
C-&
C
C$ Index_Entries
C
C     Draw the rules that delimit test cases in verbose mode.
C
C-&
 
      LOGICAL               VERBOS
 
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    MESSGE
 
 
      IF ( VERBOS() ) THEN
 
         MESSGE = '-------------------------------------------'
     .   //       '---------------------------- '
 
         CALL TSTLOG   ( MESSGE,            .FALSE. )
 
 
      END IF
 
      RETURN
      END
 
 
 
 
 
 
