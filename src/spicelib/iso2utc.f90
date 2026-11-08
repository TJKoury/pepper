!$Procedure ISO2UTC ( Convert ISO time strings to UTC strings. )

      SUBROUTINE ISO2UTC ( TSTRNG, UTCSTR, ERROR )

! $ Abstract
!
!     Deprecated: This routine is deprecated because all high-level
!     time conversion routines (STR2ET, UTC2ET, TPARSE) were updated
!     to accept ISO formatted times on input.  This routine is
!     supported for purposes of backward compatibility only.
!
!     Convert date-time strings represented in the format adopted by the
!     International Standards Organization (ISO) to equivalent UTC time
!     strings recognized by the SPICELIB routine TPARSE.
!
! $ Required_Reading
!
!     TIME
!
! $ Keywords
!
!     TIME
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         TSTRNG
      CHARACTER*(*)         UTCSTR
      CHARACTER*(*)         ERROR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TSTRNG     I   String representing a calendar or julian date epoch
!     UTCSTR     O   SPICELIB UTC string corresponding to TSTRNG
!     ERROR      O   Error message if something went wrong.
!
! $ Detailed_Input
!
!     TSTRNG   is an input time string, containing a time string
!              in ISO format. This routine is not sensitive to
!              the case of the characters that make up TSTRNG.
!              Thus 1992-192t12:29:28 and 1992-192T12:29:28
!              are equivalent.
!
!              The ISO standard time formats are:
!
!                 Year Month Day    yyyy-mm-ddThh:mm:ss[.sss...]
!                                   yyyy-mm-dd
!
!                 Day of Year       yyyy-dddThh:mm:ss[.sss...]
!                                   yyyy-ddd
!
!              The letters y,m,d,h,m,s can stand for any digit.
!              All digits are required in these formats. Moreover
!              the year portion of these strings must be between
!              1000 and 2999 inclusive.
!
!              The length of TSTRNG should not exceed 80 characters.
!
!              We point out that the format yyyy-ddd may be
!              interpreted very differently by routine UTC2ET.
!              1992-003 is interpreted by UTC2ET as March 1, 1992
!              whereas it is interpret as January 3, 1992 by ISO2ET.
!
!              User's should be aware of these differences in
!              interpretation and exercise adequate care in their
!              programs to avoid this possible confusion.
!
! $ Detailed_Output
!
!     UTCSTR   is the equivalent of TSTRNG, expressed in a UTC
!              time string that can be parsed by the SPICELIB
!              routine TPARSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the string is interpreted as an ISO format string and
!         the year portion is not within the range [1000, 2999], the
!         error SPICE(YEAROUTOFBOUNDS) is signaled. UTCSTR is
!         not changed.
!
!     2)  If the string does not clearly match the ISO format,
!         the error SPICE(NOTISOFORMAT) is signaled. UTCSTR is not
!         changed.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The input string is converted to a UTC time string as defined
!     by the SPICELIB routine TPARSE.
!
! $ Examples
!
!     To convert the time string 1992-04-03T14:12:28 to the
!     corresponding ephemeris time, execute the following instructions:
!
!        TSTRNG = '1992-04-03T14:12:28'
!
!        CALL ISO2UTC ( TSTRNG, UTCSTR, ERROR )
!
!        CALL TPARSE ( UTCSTR, UTCSEC, ERROR )
!
!        CALL DELTET ( UTCSEC, 'UTC', DELTA )
!
!        ET = DELTA + UTCSEC
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
!          Clocks, Understanding Time and Frequency," Dover
!          Publications, Inc. New York, 1877.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 23-DEC-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Remove obsolete
!        reference to NAIF document 268 from $Literature_References.
!
!-    SPICELIB Version 1.1.2, 28-FEB-2008 (BVS)
!
!        Corrected the contents of the $Required_Reading section.
!
!-    SPICELIB Version 1.1.1, 19-SEP-2006 (EDW)
!
!        Added text to previously empty $Restrictions section.
!
!-    SPICELIB Version 1.0.0, 11-JUL-1995 (KRG) (JML)
!
!        Based on
!
!        EKLIB Version 1.1.0, 11-JUL-1995 (KRG)
!
!           Fixed a typo in the $Detailed_Output section of the header.
!           The output variable was listed as ET when it should have
!           been UTCSTR.
!
!           Changed the length of ASCII to be 100 rather than 128. This
!           removes possible compiler warning messages for truncating
!           character variables on assignments. The maximum nonblank
!           length for an input time ISO string is 80 characters, so
!           placing it into a temporary array of 100 characters should
!           pose no difficulties.
!
!        EKLIB Version 1.0.0, 25-FEB-1993 (JML)
!
!-&


! $ Index_Entries
!
!     DEPRECATED Transform ISO time strings to UTC strings
!
!-&


!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      INTEGER               RTRIM

      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               NMNTHS
      PARAMETER           ( NMNTHS = 12 )

!
!     In-line functions.
!
      LOGICAL               DIGIT
      LOGICAL               COLON
      LOGICAL               HYPHEN
      LOGICAL               T

!
!     Local Variables
!
      CHARACTER*(100)       ASCII
      CHARACTER*(128)       MYSTR

      CHARACTER*(3)         MONTHS ( 0 : NMNTHS )
      SAVE                  MONTHS

      CHARACTER*(2)         IMONTH (     NMNTHS )
      SAVE                  IMONTH

      INTEGER               I
      INTEGER               L
      INTEGER               M

      LOGICAL               CHANGE
!
!     Initial Values
!

      DATA                  MONTHS  / '???', 'JAN', 'FEB', 'MAR', &
                                             'APR', 'MAY', 'JUN', &
                                             'JUL', 'AUG', 'SEP', &
                                             'OCT', 'NOV', 'DEC' /

      DATA                  IMONTH  /        '01',  '02',  '03', &
                                             '04',  '05',  '06', &
                                             '07',  '08',  '09', &
                                             '10',  '11',  '12'  /



!
!     In-line Function Definitions
!
      DIGIT(I)  =      ICHAR(ASCII(I:I)) .GE. ICHAR('0') &
                 .AND. ICHAR(ASCII(I:I)) .LE. ICHAR('9')

      HYPHEN(I) = ASCII(I:I) .EQ. '-'
      COLON(I)  = ASCII(I:I) .EQ. ':'
      T(I)      = ASCII(I:I) .EQ. 'T' .OR. ASCII(I:I) .EQ. 't'


!
!     Standard SPICELIB exception handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ISO2UTC' )
      END IF

!
!     Left justify the input time string, and determine the location of
!     it's last non-blank character.  Finally make some local copies.
!
      CALL      LJUST ( TSTRNG, ASCII )
      L      =  RTRIM (         ASCII )
      MYSTR  =  ASCII
      CHANGE = .FALSE.

!
!     Next check for one of the ISO allowed formats.
!
      IF      ( L .EQ. 8 ) THEN

!
!        The possible format is: yyyy-ddd.  If we get a
!        match construct the corresponding SPICE day of
!        year format using JAN  (e.g. 1991-JAN-261).
!
         IF (       DIGIT  (1) &
              .AND. DIGIT  (2) &
              .AND. DIGIT  (3) &
              .AND. DIGIT  (4) &
              .AND. HYPHEN (5) &
              .AND. DIGIT  (6) &
              .AND. DIGIT  (7) &
              .AND. DIGIT  (8) ) THEN

            MYSTR  =  ASCII(1:5) // 'JAN' // ASCII(5:)
            CHANGE = .TRUE.

         END IF

      ELSE IF ( L .EQ. 10 ) THEN

!
!        The possible format is: yyyy-mm-dd. If we get a match
!        construct the corresponding SPICE yyyy-mm-dd format.
!
         IF (       DIGIT  (1) &
              .AND. DIGIT  (2) &
              .AND. DIGIT  (3) &
              .AND. DIGIT  (4) &
              .AND. HYPHEN (5) &
              .AND. DIGIT  (6) &
              .AND. DIGIT  (7) &
              .AND. HYPHEN (8) &
              .AND. DIGIT  (9) &
              .AND. DIGIT  (10)  ) THEN

            M      =  BSRCHC ( ASCII(6:7), NMNTHS, IMONTH )
            MYSTR  =  ASCII(1:5) // MONTHS(M) // ASCII(8:)
            CHANGE = .TRUE.

         END IF

      ELSE IF ( L .GE. 17 ) THEN

!
!        There are two possible formats yyyy-dddThh:mm:ss.ssssss
!                                       yyyy-mm-ddThh:mm:ss.ssssss
!        As above, if we get a match up to the first character following
!        a 'T', convert this to a standard SPICE time string.
!
         IF (       DIGIT  (1)  .AND. DIGIT  (2) &
              .AND. DIGIT  (3)  .AND. DIGIT  (4) &
              .AND. HYPHEN (5) &
              .AND. DIGIT  (6)  .AND. DIGIT  (7)  .AND. DIGIT  (8) &
              .AND. T      (9) &
              .AND. DIGIT  (10) .AND. DIGIT  (11) &
              .AND. COLON  (12) &
              .AND. DIGIT  (13) .AND. DIGIT  (14) &
              .AND. COLON  (15) &
              .AND. DIGIT  (16) .AND. DIGIT  (17) ) THEN


            MYSTR  =  ASCII(1:5) // &
                     'JAN'       // &
                      ASCII(5:8) // &
                     ' '         // &
                      ASCII(10:)

            CHANGE = .TRUE.

         ELSE IF (       DIGIT  (1)  .AND. DIGIT  (2) &
                   .AND. DIGIT  (3)  .AND. DIGIT  (4) &
                   .AND. HYPHEN (5) &
                   .AND. DIGIT  (6)  .AND. DIGIT  (7) &
                   .AND. HYPHEN (8) &
                   .AND. DIGIT  (9)  .AND. DIGIT  (10) &
                   .AND. T      (11) &
                   .AND. DIGIT  (12) .AND. DIGIT  (13) &
                   .AND. COLON  (14) &
                   .AND. DIGIT  (15) .AND. DIGIT  (16) &
                   .AND. COLON  (17) &
                   .AND. DIGIT  (18) .AND. DIGIT  (19) ) THEN

            M      =  BSRCHC ( ASCII(6:7), NMNTHS, IMONTH )
            MYSTR  =  ASCII(1:5)  // &
                      MONTHS(M)   // &
                      ASCII(8:10) // &
                      ' '         // &
                      ASCII(12:)

            CHANGE = .TRUE.
         END IF

      END IF


!
!     If we didn't make some change to the input string, it's NOT
!     an ISO format string. Say so in an error message and return.
!
      IF ( .NOT. CHANGE ) THEN

         ERROR = 'The input string does not match the format '  // &
                 'expected of ISO time strings. The acceptable '// &
                 'formats are: yyyy-ddd, yyyy-mm-dd, '          // &
                 'yyyy-dddThh:mm:ss[.ss...], and '              // &
                 'yyyy-mm-ddThh:mm:ss[.ss...].  The input '     // &
                 'string was #. '

         CALL REPMC  ( ERROR, '#', MYSTR(1:L), ERROR )
         CALL CHKOUT ( 'ISO2UTC' )
         RETURN

      END IF

!
!     Check for a year out of the range from 1000 to 2999
!
      IF ( CHANGE &
           .AND. (      LLT ( ASCII(1:4), '1000' ) &
                   .OR. LGT ( ASCII(1:4), '2999' ) ) ) THEN

         ERROR = 'Years outside the range from 1000 '           // &
                 'to 2999 are not supported in SPICE-ISO '      // &
                 'format. You''ve '                             // &
                 'supplied a time string of the form '          // &
                 '# ... '
         CALL REPMC ( ERROR, '#', ASCII(1:7), ERROR )
         CALL CHKOUT ( 'ISO2UTC' )
         RETURN

      END IF


!
!     That's it.
!
      ERROR = ' '

      UTCSTR = MYSTR

      CALL CHKOUT ( 'ISO2UTC'  )

      RETURN

      END
