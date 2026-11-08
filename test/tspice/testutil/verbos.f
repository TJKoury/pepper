C$Procedure      VERBOS ( Display detailed information to the screen)
C
      LOGICAL FUNCTION VERBOS ()
C
C$ Abstract
C
C     This routine controls whether or not detailed information is
C     written to the screen and log file.
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
C       TESTING
C
C$ Declarations
C
      IMPLICIT NONE
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       The function returns TRUE if details are to be displayed.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     The function returns TRUE if detailed information should be
C     written to the screen and log file.  This behavior is controlled
C     through the two entry points VERBON and VERBOFF.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routines controls the writing of detailed messages to
C     the screen and log file.  The default behavior is to return
C     the value of FALSE.  You may turn on verbose output by calling
C     the entry point VERBON
C
C        ONOFF = VERBON
C
C     To turn verbose messages off call the entry point VERBOFF
C
C        ONOFF = VERBOFF
C
C$ Examples
C
C     Later.
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
C-    Testing Utilities Version 1.0.0, 21-NOV-1994 (WLT)
C
C
C-&
C
C$ Index_Entries
C
C     Determine the status of verbos output.
C
C-&
C     Entry points
C
      LOGICAL               VERBON
      LOGICAL               VERBOFF
C
C     Local Variables.
C
      LOGICAL               DISPLY
      SAVE
 
      DATA                  DISPLY / .FALSE. /
 
 
      VERBOS = DISPLY
      RETURN
 
      ENTRY VERBON
 
      DISPLY = .TRUE.
      VERBON = .TRUE.
      RETURN
 
      ENTRY VERBOFF
 
      DISPLY  = .FALSE.
      VERBOFF = .FALSE.
      RETURN
 
      END
 
 
