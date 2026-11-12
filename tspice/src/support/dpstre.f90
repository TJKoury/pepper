!$Procedure      DPSTRE ( Double Precision Number to Character )

      SUBROUTINE DPSTRE ( X, SIGDIG, STRING )
      use pepper_legacy

! $ Abstract
!
!     Take a double precision number and convert it to an equivalent
!     character string representation (base 10).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     CONVERSION
!     PARSING
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION X
      INTEGER          SIGDIG
      CHARACTER*(*)    STRING

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   A double precision number
!     SIGDIG     I   The number of significant digits placed in output
!     STRING     O   A character string representation of X
!
! $ Detailed_Input
!
!     X          is a double precision number.
!
!     SIGDIG     is the number of significant digits that are desired
!                for the output string.
!
! $ Detailed_Output
!
!
!     STRING     is a character representation of X to the number of
!                significant digits specified by SIGDIG.  The number of
!                spaces required to return the requested character
!                string is SIGDIG + 6.  If STRING is not declared to
!                have adequate length, the number returned will be
!                truncated on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     If SIGDIG is less than one, this routine returns one significant
!     digit in the output string.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes an approximate character representation of
!     the input DP number X. The minimum number of significant digits
!     returned is 1. The maximum number of significant digits returned
!     is 33.
!
!     For the numbers of significant digits less or equal to 14 this
!     routines calls DPSTR.
!
!     For the numbers of significant digits between 15 and 33 this
!     routines uses the following FORTRAN write statement
!
!         WRITE ( STRING, FMT=(1PEXX.YY) )
!
!     where XX = (SIGDIG + 6) and YY = (SIGDIG - 1).
!
!     For the numbers of significant digits greater than 33 this
!     routines uses the following FORTRAN write statement
!
!         WRITE ( STRING, FMT=(1PE39.32) )
!
!     as if the number of significant digits was 33.
!
! $ Examples
!
!     This example program prints PI with 3, 14, 17 and 22 significant
!     digits:
!
!           DOUBLE PRECISION      PI
!           CHARACTER*(80)     TEXT
!
!           CALL DPSTRE( PI(),  3, TEXT )
!           CALL TOSTDO( TEXT )
!           CALL DPSTRE( PI(), 14, TEXT )
!           CALL TOSTDO( TEXT )
!           CALL DPSTRE( PI(), 17, TEXT )
!           CALL TOSTDO( TEXT )
!           CALL DPSTRE( PI(), 22, TEXT )
!           CALL TOSTDO( TEXT )
!
!           END
!
!     When compiled with 32bit GFORTRAN on a Linux box it produces
!     the following output:
!
!            3.14E+00
!            3.1415926535898E+00
!            3.14159265358979312E+00
!            3.141592653589793115998E+00
!
! $ Restrictions
!
!     The maximum number of significant digits returned is 33.
!
!     If the output string is not declared to be adequately large
!     (at least SIGDIG + 6), the numeric string will be truncated
!     on the right.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SUPPORT Version 1.0.0, 27-JAN-2012 (BVS)
!
!-&

! $ Index_Entries
!
!     d.p. number to character
!
!-&

!
!     Local parameters.
!
!     The maximum number of allowed significant digits is set to 33
!     (=16*2+1). This is an arbitrarily picked value because FORTRAN
!     doesn't seen to have a limit. But we do need a limit to make sure
!     that the formatted WRITE does not overflow the local buffer
!     string.
!
!
      INTEGER               MOSTDG
      PARAMETER           ( MOSTDG = 33 )

!
!     Format template.
!
      CHARACTER*(*)         FMTPIC
      PARAMETER           ( FMTPIC = '(1PE#.#)' )

!
!     Local variables
!
      INTEGER               IOSTAT
      INTEGER               MAXSIG
      INTEGER               MAXSAV

      CHARACTER*(10)        FMTSTR
      CHARACTER*(MOSTDG+7)  NUMSTR

      LOGICAL               FIRST

!
!     Saved variables.
!
      SAVE                  MAXSAV
      SAVE                  FIRST
      SAVE                  FMTSTR

!
!     Initial values.
!
      DATA                  FIRST   / .TRUE. /
      DATA                  MAXSAV  / 14 /
      DATA                  FMTSTR  / '(1PE20.13)' /

!
!     Reset the input number of significant digits if it is outside of
!     the allowed range (1 to 33).
!
      MAXSIG = MIN ( MOSTDG, MAX( 1, SIGDIG ) )

!
!     If the number of significant digits is less then or equal to 14,
!     outsource conversion to DPSTR.
!
      IF ( MAXSIG .LE. 14 ) THEN

         CALL DPSTR( X, MAXSIG, STRING )

      ELSE

!
!        The number of significant digits is greater than 14. Make
!        output format. Do it only for the first call or if the
!        previous call had a different number of significant digits.
!        Otherwise, use the SAVEd format string from the previous
!        call.
!
         IF ( FIRST .OR. MAXSIG .NE. MAXSAV ) THEN

            FMTSTR = FMTPIC
            CALL REPMI( FMTSTR, '#', MAXSIG+6, FMTSTR )
            CALL REPMI( FMTSTR, '#', MAXSIG-1, FMTSTR )

            MAXSAV = MAXSIG
            FIRST  = .FALSE.

         END IF

!
!        Use WRITE to create a temporary output string. This string is
!        declared to have enough room for any allowed numbers of
!        significant digits. We should not get any errors.
!
         WRITE ( NUMSTR, &
                 FMTSTR, &
                 IOSTAT = IOSTAT ) X

!
!        This is fail safe check. Since we made the format string
!        ourselves and declared the output string with enough room we
!        should never hit it. But we do this check anyway, just in
!        case.
!
         IF ( IOSTAT .NE. 0 ) THEN

            CALL CHKIN  ( 'DPSTRE'                             )
            CALL SETMSG ( 'Bug. FORTRAN WRITE failed; number ' // &
                          '= #; format = #; IOSTAT = #'        )
            CALL ERRDP  ( '#', X                               )
            CALL ERRCH  ( '#', FMTSTR                          )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(BUGWRITEFAILED)'              )
            CALL CHKOUT ( 'DPSTRE'                             )
            RETURN

         END IF

!
!        NOTE 1: should we also check for '*'s?
!
!        NOTE 2: should we check for 'E' in the string for cases of
!                output FORTRAN WRITE for numbers greater than 1D100?
!                (In this case GFORTRAN leaves E out and prints
!                pi*1D101 like this -3.14+101.)
!

!
!        Assign output string.
!
         STRING = NUMSTR

      END IF

      RETURN

      END
