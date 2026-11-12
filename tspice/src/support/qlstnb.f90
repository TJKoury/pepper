!$Procedure      QLSTNB (Quick LAST non-blank character)

      INTEGER FUNCTION QLSTNB ( STRING )
      use pepper_legacy

! $ Abstract
!
!     This is a "faster" version of the SPICELIB routine LASTNB.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      ASCII,  CHARACTER,  SEARCH
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         STRING

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      STRING     I   Input character string.
!      QLSTNB     O   Index of the last non-blank character in STRING.
!
! $ Detailed_Input
!
!      STRING      is the input character string.
!
! $ Detailed_Output
!
!      QLSTNB      is the index of the last non-blank character
!                  in the input string. If there are no non-blank
!                  characters in the string, QLSTNB is zero.
!
! $ Parameters
!
!     None.
!
! $ Particulars
!
!      If the string is blank, return zero. Otherwise, step through
!      the string one character at a time until something other than
!      a blank is found. Return the index of that something within
!      the string.
!
!      This routine has the same function as the SPICE routine
!      LASTNB however, it turns out to be substantially faster
!      when applied to longer strings.  This is somewhat surprising
!      but happens due to a combination of machine instructions
!      available for comparing strings and the ineffective optimizations
!      performed by all compilers we've examined.  See the code
!      for more details regarding how this routine takes advantage
!      of native instructions and ineffective optimizations.
!
! $ Examples
!
!      The following examples illustrate the use of QLSTNB.
!
!            QLSTNB ( 'ABCDE'              )   = 5
!            QLSTNB ( 'AN EXAMPLE'         )   = 10
!            QLSTNB ( 'AN EXAMPLE        ' )   = 10
!            QLSTNB ( '                  ' )   = 0
!
! $ Restrictions
!
!      None.
!
! $ Exceptions
!
!      Error free.
!
! $ Files
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!-    SPICELIB Version 1.0.0, 22-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Get the index of the last non-blank character of a string.
!
!-&



      INTEGER               B
      INTEGER               I
      INTEGER               L
      INTEGER               M
      INTEGER               NL
      INTEGER               BLANK
      INTEGER               TUNE
      PARAMETER           ( TUNE = 16 )


      BLANK = ICHAR(' ')
      L     =  LEN(STRING)
!
!     If this is a short string there is no particular advantage
!     to be gained by making use of the binary search idea.
!     The speed up just doesn't buy much when compared with
!     the loop overhead.
!
      IF ( L .LE. 32 ) THEN

         DO I = L, 1, -1
            IF ( ICHAR(STRING(I:I)) .NE. BLANK ) THEN
               QLSTNB = I
               RETURN
            END IF
         END DO

         QLSTNB = 0
         RETURN

      END IF


      B     =  1
      NL    =  L - 1

!
!     We want M to be ( B + NL ) / 2   but right now that's L/2
!
      M     =  L / 2


      DO WHILE ( L - B .GT. TUNE )
!
!         What is true right now?  The string from L+1 on out
!         is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2;
!         and M >= B,  B is at least one and if greater than 1
!         there must be a non-blank character between B and the
!         end of the string.
!
         IF ( ICHAR(STRING(L:L)) .NE. BLANK ) THEN

            QLSTNB = L
            RETURN

         ELSE IF ( ICHAR(STRING(M:M)) .NE. BLANK ) THEN

            L = NL
            B = M

         ELSE IF ( STRING(M+1:NL) .EQ. ' ' ) THEN

!
!            If you got here, the STRING(L:L) is a blank.
!            The string from L+1 on out is blank.
!            The string from M to NL (=L-1) is blank.  Thus the
!            string from M out is blank.
!
!            M is greater than or equal to B
!            If M  is less than B + 2, then L will become
!            B or less and there will not be a
!            next pass through the loop.  That means that
!            we will never get to this point again and don't
!            have to worry about the reference STRING(M:NL)
!            giving us an access violation.
!
            L  = M - 1
!
!            With the new value of L, we now know that STRING(L+1:)
!            is blank.
!
         ELSE
!
!            If you get to this point all of the string from
!            L out is blank and L is greater than M.
!            There is a non-blank character between M+1 and NL.
!            If L should become equal to B below, then the loop
!            will not be executed again.  That means again that
!            we don't have to worry about STRING(M:NL) being
!            an ill formed string.
!
            L  =  NL
            B  =  M + 1
!
!            With the new value of L, we now know that STRING(L+1:)
!            is blank.
!
         END IF

         NL =  L - 1
         M  = (B + NL) / 2

!
!         What's true now?  The string from L+1 on out is blank.
!         Somewhere between B and L is a non-blank character.
!
      END DO
!
!      Either B never changed from 1 or B was set to a value such that
!      there was a non-blank character between B and the end of
!      the string,  And the string from L+1 out to the end is
!      blank.  Since we want this to mimick RTRIM, we are done.
!
      DO I = L, 1, -1

         IF ( ICHAR(STRING(I:I)) .NE. BLANK  ) THEN
            QLSTNB = I
            RETURN
         END IF
      END DO

      QLSTNB = 0
      RETURN
      END
