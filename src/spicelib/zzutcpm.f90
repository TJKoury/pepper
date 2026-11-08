!$Procedure      ZZUTCPM ( UTC Plus or Minus Parse )

      SUBROUTINE ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Parse a substring of the form ::UTC[+/-]1-12:0-59
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      Time --- PRIVATE
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         STRING
      INTEGER               START
      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      MOFF
      INTEGER               LAST
      LOGICAL               SUCCES

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   is a string containing a substring ::UTC+HR:MN
!     START      I   is the location in the string to start parsing
!     HOFF       O   is the d.p. value associated with HR.
!     MOFF       O   is the d.p. value associated with MN
!     LAST       O   is the end of the time zone substring.
!     SUCCES     O   indicates that a time zone was parsed.
!
! $ Detailed_Input
!
!     STRING     is a string that has an embedded substring of the
!                form ::UTC+HR[:MN] ( or ::UTC-HR[:MN] starting at
!                character start.
!
!     START      is the location in STRING where a time zone
!                specification is believed to begin.
!
! $ Detailed_Output
!
!     HOFF       is the double precision value associated with
!                HR in the picture above.  This value will be
!                between -12 and 12 inclusive.
!
!     MOFF       is the double precision value associated with MN
!                in the picture above.  This value will be between
!                0 and 59 inclusive (or -59 and 0 inclusive) depending
!                on the sign present in the UTC+/- substring.  The
!                sign of MOFF is the same as the sign present in the
!                string.
!
!     LAST       is the last character of the time zone specification.
!                If the string doesn't have a correct format and
!                range of values, LAST is returns as START - 1.
!
!     SUCCES     is a logical which if true, indicates that a time
!                zone was successfully parsed.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) There are no exceptions.  Either the string matches
!        the template or it doesn't.  No case is regarded
!        as an error.
!
! $ Particulars
!
!     This is a private routine for parsing time zones specified
!     as UTC+/-HR:MN  where HR is an unsigned integer between 0 and
!     11.  HR must have no more than 2 digits.  MN is expected
!     to be an unsigned integer between 0 and 59 inclusive.  It must
!     have no more than 2 digits.
!
! $ Examples
!
!     See TIMOUT.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 27-SEP-1996 (WLT)
!
!
!-&
!
!     Spicelib functions
!
      LOGICAL               SAMCH
!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    ERROR

      INTEGER               NEED
      INTEGER               LENGTH
      INTEGER               SIGNAT
      INTEGER               UNSAT
      INTEGER               UNSTO
      INTEGER               NCHAR
      INTEGER               PTR

      DOUBLE PRECISION      SIGN
      DOUBLE PRECISION      X

!
!     This is a special purpose routine.  The input string must have
!     exactly the right format to be a time zone substring.  If anything
!     goes wrong, we just bail out and leave HOFF and MOFF right at
!     zero.
!
      HOFF   =  0.0D0
      MOFF   =  0.0D0
      LAST   =  START - 1
      SUCCES = .FALSE.

!
!     Note that NEED   = START + LEN('::UTC+x') - 1
!               SIGNAT = START + LEN('::UTC+' ) - 1
!
      LENGTH = LEN(STRING)
      NEED   = START + 6
      SIGNAT = START + 5
      UNSAT  = NEED

      IF ( LENGTH .LT. NEED ) THEN
         RETURN
      END IF

      IF       ( ICHAR(STRING(SIGNAT:SIGNAT)) .EQ. ICHAR('+') ) THEN
         SIGN =  1.0D0
      ELSE IF  ( ICHAR(STRING(SIGNAT:SIGNAT)) .EQ. ICHAR('-') ) THEN
         SIGN = -1.0D0
      ELSE
         RETURN
      END IF
!
!     So far everything looks fine, "lex" the string starting at
!     SIGNAT + 1 for an unsigned integer.
!
      CALL LX4UNS ( STRING, UNSAT, UNSTO, NCHAR )

      IF ( NCHAR .GT. 0 .AND. NCHAR .LT. 3 ) THEN
         CALL NPARSD ( STRING(UNSAT:UNSTO), X, ERROR, PTR )

         IF ( X .GE. 13.0D0 ) THEN
            RETURN
         END IF

         LAST = UNSTO
         HOFF = SIGN * X

      ELSE
         RETURN
      END IF
!
!     If we're still in the game at this point, we have at least
!     an hour offset, see if there is a minutes portion to the
!     time zone.
!
      SUCCES = .TRUE.

      IF ( SAMCH( STRING, UNSTO+1, ':', 1 ) ) THEN
         UNSAT = UNSTO + 2
      ELSE
         RETURN
      END IF

      CALL LX4UNS ( STRING, UNSAT, UNSTO, NCHAR )

      IF ( NCHAR .GT. 0 .AND. NCHAR .LT. 3 ) THEN
         CALL NPARSD ( STRING(UNSAT:UNSTO), X, ERROR, PTR )

         IF ( X .GT. 59.0D0 ) THEN
            RETURN
         END IF

         LAST = UNSTO
         MOFF = SIGN*X

      END IF

      RETURN
      END
