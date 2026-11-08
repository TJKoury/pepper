!$Procedure LJUCRS ( Left-justify, Uppercase, Compress )

      SUBROUTINE LJUCRS ( N, INPUT, OUTPUT )

! $ Abstract
!
!     Left-justify, uppercase, and space-compress a character string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!
! $ Declarations

      IMPLICIT NONE

      INTEGER             N
      CHARACTER*(*)       INPUT
      CHARACTER*(*)       OUTPUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I      Maximum consecutive occurrences of space.
!     INPUT      I      Input string.
!     OUTPUT     O      Output string.
!
! $ Detailed_Input
!
!     N        is the maximum number of consecutive occurrences
!              of space that will be allowed to remain in the
!              output string.
!
!     INPUT    is the input string.
!
! $ Detailed_Output
!
!     OUTPUT   is the output string. This is the input string that
!              left-justified and with all occurrences of more than
!              N consecutive spaces removed.
!
!              If OUTPUT is not large enough to hold the
!              compressed string, it is truncated on the right.
!
!              OUTPUT may overwrite INPUT.
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
!     The string is left-justified and uppercased. Occurrences of more
!     than N consecutive spaces are removed from the input string as it
!     is copied to the output string. If the output string is not large
!     enough to hold the compressed string, it is truncated on the
!     right.
!
! $ Examples
!
!     Let N = 1. Then
!
!         ' Abc  DE F  ',           becomes    'ABC DE F',
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&


! $ Index_Entries
!
!     compress uppercase left-justify a character_string
!
!-&


!
!
!     Local Variables
!
      INTEGER               I
      INTEGER               J
      INTEGER               COUNT

      INTEGER               INLEN
      INTEGER               OUTLEN

      INTEGER               ICH

!
!     Saved variables
!
      INTEGER               SHIFT
      INTEGER               LOWA
      INTEGER               LOWZ

      LOGICAL               FIRST

      SAVE                  FIRST
      SAVE                  SHIFT
      SAVE                  LOWA
      SAVE                  LOWZ

!
!     Initial Data
!
      DATA                  FIRST / .TRUE. /

!
!     Do some set up stuff the first time through so that we do not
!     need to reinitialize the boundary values used for comparisons
!     and the shift on each call.
!
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         LOWA  = ICHAR ('a')
         LOWZ  = ICHAR ('z')
         SHIFT = ICHAR ('A') - LOWA
      END IF

!
!     Find out how much space there is in the INPUT and OUTPUT strings
!     and initialize the space counter and output place holder.
!
      INLEN  = LEN(INPUT)
      OUTLEN = LEN(OUTPUT)

      COUNT  = 0
      J      = 0

      DO I = 1, INLEN

!
!        Skip leading spaces.
!
         IF ( J .EQ. 0 .AND. INPUT(I:I) .EQ. ' ' ) THEN

!
!           Another leading space. Skip it.
!

         ELSE

!
!           Check this character to see if it is a space or not.
!
            IF ( INPUT(I:I) .EQ. ' ' ) THEN

               COUNT = COUNT + 1

!
!              Copy spaces until enough consecutive spaces
!              have been accumulated. When enough consecutive spaces
!              have accumulated, we no longer copy them.
!
               IF ( COUNT .LE. N ) THEN
                  J           = J + 1
                  OUTPUT(J:J) = INPUT(I:I)
               END IF

            ELSE

!
!              We don't have a space here. Set the space counter to
!              zero.
!
               COUNT       = 0

!
!              Copy this character while swapping lowercase with upper
!              case along the way.
!
               J           = J + 1

               ICH = ICHAR( INPUT(I:I) )

               IF ( ( ICH .GE. LOWA ) .AND. ( ICH .LE. LOWZ ) ) THEN
                  OUTPUT(J:J) = CHAR ( ICH + SHIFT )
               ELSE
                  OUTPUT(J:J) = INPUT(I:I)
               END IF

            END IF

            IF ( J .EQ. OUTLEN ) THEN
               RETURN
            END IF

         END IF

      END DO

!
!     Pad any left over space in the output string with blanks. Note
!     that if the input string was blank, J will be zero at this
!     point and the case below will set the whole output string to
!     blank.
!
      IF ( J .LT. OUTLEN ) THEN
         OUTPUT(J+1:) = ' '
      END IF

      RETURN
      END
