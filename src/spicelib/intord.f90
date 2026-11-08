!$Procedure INTORD ( Convert an integer to ordinal text )

      SUBROUTINE INTORD ( N, STRING )

! $ Abstract
!
!     Convert an integer to an equivalent written ordinal phrase.
!     For example, convert 121 to 'ONE HUNDRED TWENTY-FIRST'.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONVERSION
!     PARSING
!     STRING
!     UNITS
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               MAXORD
      PARAMETER           ( MAXORD = 148 )

      INTEGER               N
      CHARACTER*(*)         STRING

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   An integer (less than 10**12 in absolute value).
!     STRING     O   An English string representing the ordinal of N.
!
! $ Detailed_Input
!
!     N        is an integer (less than 10**12 in absolute value).
!              Moreover, if N is less than zero, -N must be a
!              a legitimate number on the host machine.
!
!              In the context of this routine N represents the
!              ranking of some item within a group.
!
! $ Detailed_Output
!
!     STRING   is the English ordinal equivalent of N. STRING will
!              contain only upper case letters.
!
! $ Parameters
!
!     MAXORD   is one more than the length of the longest ordinal
!              string that can be produced by a call to this routine:
!              One string of maximum length is:
!
!                 'NEGATIVE '                                  //
!                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
!                 'SEVEN HUNDRED SEVENTY-SEVENTH'
!
!              It has 147 characters.
!
!              The parameter MAXORD is used to declare a local string
!              of sufficient length to allow the construction of
!              any ordinal string.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the resulting ordinal is longer than the output string,
!         it will be truncated on the right, leaving only the most
!         significant portion of the ordinal.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is used primarily for generating error messages. For example,
!     if the third letter or token in a string is in error, it might
!     be desirable to supply a message like the following:
!
!        'The third token of 31-JAN-198$ is not a valid year.'
!
! $ Examples
!
!     N           STRING
!     ------      -------------------------------------------
!     -6          NEGATIVE SIXTH
!      1          FIRST
!      2          SECOND
!      3          THIRD
!      4          FOURTH
!      20         TWENTIETH
!      21         TWENTY-FIRST
!      99         NINETY-NINTH
!      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-FIRST
!
! $ Restrictions
!
!     1)  Whatever restrictions apply to INTTXT apply to this routine
!         as well.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     convert an integer to ordinal text
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB

!
!     Local variables
!
      INTEGER               I
      INTEGER               LAST
      CHARACTER*(MAXORD)    MYSTR

!
!     First get the English equivalent of the cardinal N.
!
      MYSTR = ' '

      CALL INTTXT ( N, MYSTR )

      LAST  = LASTNB(MYSTR)
      I     = LAST

!
!     Find the beginning of the last number of MYSTR.
!
      DO WHILE (       ( MYSTR(I:I) .NE. '-' ) &
                 .AND. ( MYSTR(I:I) .NE. ' ' ) &
                 .AND. ( I          .GT.  1  )  )

         I = I - 1

      END DO

      IF (      ( MYSTR(I:I) .EQ. ' ' ) &
           .OR. ( MYSTR(I:I) .EQ. '-' )  ) THEN

         I = I + 1

      END IF

!
!     Now convert the last cardinal to an ordinal.
!
      IF ( MYSTR(I:LAST) .EQ. 'ONE' ) THEN

         MYSTR(I:) = 'FIRST'

      ELSE IF ( MYSTR(I:LAST) .EQ. 'TWO' ) THEN

         MYSTR(I:) = 'SECOND'

      ELSE IF ( MYSTR(I:LAST) .EQ. 'THREE' ) THEN

         MYSTR(I:) = 'THIRD'

      ELSE IF ( MYSTR(I:LAST) .EQ. 'FIVE' ) THEN

         MYSTR(I:) = 'FIFTH'

      ELSE IF ( MYSTR(I:LAST) .EQ. 'EIGHT' ) THEN

         MYSTR(I:) = 'EIGHTH'

      ELSE IF ( MYSTR(I:LAST) .EQ. 'NINE' ) THEN

         MYSTR(I:) = 'NINTH'

      ELSE IF ( MYSTR (I:LAST) .EQ. 'TWELVE' ) THEN

         MYSTR(I:) = 'TWELFTH'

      ELSE IF ( MYSTR(LAST:LAST) .EQ. 'Y' ) THEN

         MYSTR(LAST:) = 'IETH'

      ELSE

         CALL SUFFIX ( 'TH', 0, MYSTR )

      END IF

!
!     Now simply put MYSTR into STRING and return.
!
      STRING = MYSTR

      RETURN
      END
