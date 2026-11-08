C$Procedure   ISON ( Determine whether a system is on or off )
 
      LOGICAL FUNCTION ISON ( SYSTEM )
 
C$ Abstract
C
C     Test whether or not a specified system is "on" (enabled).
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
C     KEYWORD
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         SYSTEM
      LOGICAL               SETON
      LOGICAL               SETOFF
      LOGICAL               ISOFF
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SYSTEM     I   The name of some system that may be on or off
C     ISON       O   .TRUE. if the specified system is "ON"
C     ISOFF      O   .TRUE  if the specified system if "OFF"
C     SETON      O   .TRUE.
C     SETOFF     O   .TRUE.
C
C     The function returns the state of the specified system.
C
C$ Detailed_Input
C
C     SYSTEM    is the name of some system.  The routine is case
C               sensitive for SYSTEM.
C
C$ Detailed_Output
C
C     The function returns the value TRUE if the specified system
C     is on.  By default all systems are regarded as being in the
C     "ON" state (value .TRUE. returned) unless initialized to
C     "OFF" by a call to SETOFF.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is a utility routine that allows different
C     parts of a program to communicate the status and set the
C     state of some "system" of the program.
C
C$ Examples
C
C     The following illustrates how this routine and the entry points
C     SETON, SETOFF are used within the SPICE Test Utilities Package.
C
C
C     The routines TSETUP sets the status of the exception handling
C     checks to OFF.  This means that no calls have been made to
C     the checker function CHCKXC.
C
C     TSETUP
C     ======
C
C
C        DUMMY = SETOFF ( 'CHCKXC' )
C
C
C     In the routine CHCKXC,  the following call is placed to
C     indicate that a call to CHCKXC has been made and that
C     the current status of the SPICE exception handling system
C     has been checked.
C
C     CHCKSC
C     ======
C
C        DUMMY = SETON ( 'CHCKXC' )
C
C
C     Because the status of exception checking is maintained in this
C     way, the routines TCASE, TOPEN and TCLOSE can determine whether
C     the status of the SPICE exception handling system has been
C     checked recently.  If it has NOT been checked, these routines
C     perform such a check (under the assumption that NO exception is
C     expected) and then reset the "checking status" to unchecked
C     so that the next test case exercised  can test the exception
C     handling status if needed.
C
C
C     TCASE
C     ======
C
C     IF ( ISOFF ( 'CHCKXC' ) ) THEN
C        CALL CHCKXC ( .FALSE., ' ',  OK )
C        DUMMY = SETOFF( 'CHCKXC' )
C     END IF
C
C
C     TOPEN
C     ======
C
C     IF ( ISOFF ( 'CHCKXC' ) ) THEN
C        CALL CHCKXC ( .FALSE., ' ',  OK )
C        DUMMY = SETOFF( 'CHCKXC' )
C     END IF
C
C
C     TCLOSE
C     ======
C
C     IF ( ISOFF ( 'CHCKXC' ) ) THEN
C        CALL CHCKXC ( .FALSE., ' ',  OK )
C        DUMMY = SETOFF( 'CHCKXC' )
C     END IF
C
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.1.1, 05-MAY-2020 (JDR)
C
C        Removed non-printing characters from Keywords section. Sorted
C        the header sections to comply with NAIF standard.
C
C-    Testing Utilities Version 1.1.0, 28-DEC-2001 (NJB)
C
C        Dummy return values are now set for entry points SETON and
C        SETOFF.  Updated Procedure line and Abstract.
C
C-    Testing Utilities Version 1.0.0, 27-JUL-1999 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Test the status of a system
C
C-&


C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
 
 
      INTEGER               MAXSYS
      PARAMETER           ( MAXSYS = 200 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    SYSTMS ( MAXSYS )
      SAVE                  SYSTMS
      
      INTEGER               I
      INTEGER               OVFLOW
      SAVE                  OVFLOW
      
      INTEGER               NSYS
      SAVE                  NSYS
      
      LOGICAL               STATE  ( MAXSYS )
      SAVE                  STATE
 
      LOGICAL               ON
      PARAMETER           ( ON = .TRUE. )
      LOGICAL               OFF
      PARAMETER           ( OFF = .FALSE. )
 
      DATA                  NSYS   / 0      /
      DATA                  OVFLOW / MAXSYS /
 
 
      I = ISRCHC ( SYSTEM, NSYS, SYSTMS )
 
      IF ( I .GT. 0 ) THEN
         ISON = STATE(I)
      ELSE
         ISON = ON
      END IF
 
      RETURN
 
 
      ENTRY ISOFF ( SYSTEM )
 
 
         I = ISRCHC ( SYSTEM, NSYS, SYSTMS )
 
         IF ( I .GT. 0 ) THEN
            ISOFF = .NOT. STATE(I)
         ELSE
            ISOFF = OFF
         END IF
 
         RETURN
 
 
      ENTRY SETON ( SYSTEM )

C
C        Provide a dummy return value to satisfy various compiler
C        checks.
C     
         SETON = .TRUE.

C
C        See if we recognize this system.
C
         I = ISRCHC ( SYSTEM, NSYS, SYSTMS )
 
         IF ( I .EQ. 0 ) THEN
C
C           Nope, add it into the list.
C
            IF ( NSYS .LT. MAXSYS ) THEN
 
               NSYS = NSYS + 1
               SYSTMS( NSYS ) = SYSTEM
               STATE ( NSYS ) = ON
 
            ELSE
 
               OVFLOW = OVFLOW + 1
               IF ( OVFLOW .GT. MAXSYS ) THEN
                  OVFLOW = 1
               END IF
 
               SYSTMS( NSYS ) = SYSTEM
               STATE ( NSYS ) = ON
 
            END IF
 
         ELSE
 
            STATE( I ) = ON
 
         END IF
 
         RETURN
 


      ENTRY SETOFF ( SYSTEM )
 
C
C        Provide a dummy return value to satisfy various compiler
C        checks.
C     
         SETOFF = .TRUE.

C
C        See if we recognize this system.
C
         I = ISRCHC ( SYSTEM, NSYS, SYSTMS )
 
         IF ( I .EQ. 0 ) THEN
C
C           Nope, add it into the list.
C
            IF ( NSYS .LT. MAXSYS ) THEN
 
               NSYS = NSYS + 1
               SYSTMS( NSYS ) = SYSTEM
               STATE ( NSYS ) = OFF
 
            ELSE
 
               OVFLOW = OVFLOW + 1
               IF ( OVFLOW .GT. MAXSYS ) THEN
                  OVFLOW = 1
               END IF
 
               SYSTMS( NSYS ) = SYSTEM
               STATE ( NSYS ) = OFF
 
            END IF
 
         ELSE
 
            STATE( I ) = OFF
 
         END IF
 
         RETURN
 
 
      END
