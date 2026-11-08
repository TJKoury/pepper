!$Procedure   ISON ( Determine whether a system is on or off )

      LOGICAL FUNCTION ISON ( SYSTEM )

! $ Abstract
!
!     Test whether or not a specified system is "on" (enabled).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     KEYWORD
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         SYSTEM
      LOGICAL               SETON
      LOGICAL               SETOFF
      LOGICAL               ISOFF


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SYSTEM     I   The name of some system that may be on or off
!     ISON       O   .TRUE. if the specified system is "ON"
!     ISOFF      O   .TRUE  if the specified system if "OFF"
!     SETON      O   .TRUE.
!     SETOFF     O   .TRUE.
!
!     The function returns the state of the specified system.
!
! $ Detailed_Input
!
!     SYSTEM    is the name of some system.  The routine is case
!               sensitive for SYSTEM.
!
! $ Detailed_Output
!
!     The function returns the value TRUE if the specified system
!     is on.  By default all systems are regarded as being in the
!     "ON" state (value .TRUE. returned) unless initialized to
!     "OFF" by a call to SETOFF.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a utility routine that allows different
!     parts of a program to communicate the status and set the
!     state of some "system" of the program.
!
! $ Examples
!
!     The following illustrates how this routine and the entry points
!     SETON, SETOFF are used within the SPICE Test Utilities Package.
!
!
!     The routines TSETUP sets the status of the exception handling
!     checks to OFF.  This means that no calls have been made to
!     the checker function CHCKXC.
!
!     TSETUP
!     ======
!
!
!        DUMMY = SETOFF ( 'CHCKXC' )
!
!
!     In the routine CHCKXC,  the following call is placed to
!     indicate that a call to CHCKXC has been made and that
!     the current status of the SPICE exception handling system
!     has been checked.
!
!     CHCKSC
!     ======
!
!        DUMMY = SETON ( 'CHCKXC' )
!
!
!     Because the status of exception checking is maintained in this
!     way, the routines TCASE, TOPEN and TCLOSE can determine whether
!     the status of the SPICE exception handling system has been
!     checked recently.  If it has NOT been checked, these routines
!     perform such a check (under the assumption that NO exception is
!     expected) and then reset the "checking status" to unchecked
!     so that the next test case exercised  can test the exception
!     handling status if needed.
!
!
!     TCASE
!     ======
!
!     IF ( ISOFF ( 'CHCKXC' ) ) THEN
!        CALL CHCKXC ( .FALSE., ' ',  OK )
!        DUMMY = SETOFF( 'CHCKXC' )
!     END IF
!
!
!     TOPEN
!     ======
!
!     IF ( ISOFF ( 'CHCKXC' ) ) THEN
!        CALL CHCKXC ( .FALSE., ' ',  OK )
!        DUMMY = SETOFF( 'CHCKXC' )
!     END IF
!
!
!     TCLOSE
!     ======
!
!     IF ( ISOFF ( 'CHCKXC' ) ) THEN
!        CALL CHCKXC ( .FALSE., ' ',  OK )
!        DUMMY = SETOFF( 'CHCKXC' )
!     END IF
!
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.1.1, 05-MAY-2020 (JDR)
!
!        Removed non-printing characters from Keywords section. Sorted
!        the header sections to comply with NAIF standard.
!
!-    Testing Utilities Version 1.1.0, 28-DEC-2001 (NJB)
!
!        Dummy return values are now set for entry points SETON and
!        SETOFF.  Updated Procedure line and Abstract.
!
!-    Testing Utilities Version 1.0.0, 27-JUL-1999 (WLT)
!
!-&

! $ Index_Entries
!
!     Test the status of a system
!
!-&


!
!     SPICELIB Functions
!
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

!
!        Provide a dummy return value to satisfy various compiler
!        checks.
!
         SETON = .TRUE.

!
!        See if we recognize this system.
!
         I = ISRCHC ( SYSTEM, NSYS, SYSTMS )

         IF ( I .EQ. 0 ) THEN
!
!           Nope, add it into the list.
!
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

!
!        Provide a dummy return value to satisfy various compiler
!        checks.
!
         SETOFF = .TRUE.

!
!        See if we recognize this system.
!
         I = ISRCHC ( SYSTEM, NSYS, SYSTMS )

         IF ( I .EQ. 0 ) THEN
!
!           Nope, add it into the list.
!
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
