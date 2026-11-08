!$Procedure      M2NTEM ( Parse the numeric template of a @number )

      SUBROUTINE M2NTEM ( STRING, BASE, BEG, END, A, B )
      IMPLICIT NONE

! $ Abstract
!
!     Parse the numeric template of a META/2 @numeric META-KEY.
!
! $ Required_Reading
!
!     META/2 a language specification language.
!
! $ Keywords
!
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations

      CHARACTER*(*)         STRING
      CHARACTER*(*)         BASE
      INTEGER               BEG
      INTEGER               END
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A META/2 language statement specification.
!     BASE       I   Type of META-KEY associated with this template.
!     BEG       I/0  The beginning of the substring on input and output
!     END       I/0  The end of the substring on input and output
!     A          O   Lower value of the numeric restriction template
!     B          O   Upper value of the numeric restriction template
!
! $ Detailed_Input
!
!     STRING(BEG:END)  is a word in the META/2 language.  Is a META/2
!                      range restriction template.  It has the form
!                      (A:B) where A and B are both chracter strings
!                      representing numbers.
!
!     BASE             is a character string and should be '@int' or
!                      '@number'.
!
! $ Detailed_Output
!
!     BEG        On ouput BEG points to the first character following
!                the input value of END.
!
!     END        is returned unchanged.
!
!     A          is the value represented by the first numeric string
!                of the restriction template.  If a numeric string
!                is not present, A is not assigned the minimum possible
!                value associated with the data type given in BASE.
!
!     B          is the value represented by the second numeric string
!                of the restriction template (if there is a second
!                numeric string)  If no numeric string is present B is
!                assigned the maximum possible value associated with
!                the data type given in BASE.
!
!
! $ Error_Handling
!
!     None.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!      The range restriction template is part of the META/2 language
!      and is  described in the required reading section.  Briefly it
!      is a string at the beginning of a word that has the form
!
!      (A:B)
!
!      where A is a string representing a positive integer, and
!      B the null string or a string representing a positive integer
!      greater than A.
!
!      This routine determines if a range template is present and if so
!      what the values of A and B are.  If A (or B )is the null string
!      it is assumed to represent the smallest possible (largest
!      possible ) number of the type indicated by BASE.
!
! $ Examples
!
!      None.
!
! $ Restrictions
!
!      None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
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
!     Version B1.0.0, 23-MAR-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               INTMAX
      INTEGER               INTMIN

      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

!
!     Local variables
!
      INTEGER               BEGIN
      INTEGER               LAST
      INTEGER               J
      INTEGER               K

      CHARACTER*80          ERROR

      DOUBLE PRECISION      MINVAL
      DOUBLE PRECISION      MAXVAL

      SAVE


      LAST  = END - 1
      BEGIN = BEG + 1

!
!     First get the min and max's for this kind of word.
!
      IF ( BASE .EQ. '@int' ) THEN

         MINVAL = DBLE ( INTMIN() )
         MAXVAL = DBLE ( INTMAX() )

      ELSE

         MINVAL = DPMIN()
         MAXVAL = DPMAX()

      END IF

!
!      parse the restriction template
!
      IF ( STRING(BEGIN:BEGIN) .EQ. ':' ) THEN

         A = MINVAL
         CALL NPARSD ( STRING(BEGIN+1:LAST), B, ERROR, J )

         IF ( ERROR .NE. ' ' ) THEN
            B = MAXVAL
         END IF

      ELSE IF ( STRING(LAST:LAST) .EQ. ':' ) THEN

         CALL NPARSD ( STRING(BEGIN:LAST-1), A, ERROR, J )

         IF ( ERROR .NE. ' ' ) THEN
            A = MINVAL
         END IF

         B = MAXVAL

      ELSE

         J = INDEX ( STRING(BEGIN:LAST) , ':' ) + BEG

         CALL NPARSD ( STRING(BEGIN:J-1), A, ERROR, K )

         IF ( ERROR .NE. ' ' ) THEN
            A = MINVAL
         END IF

         CALL NPARSD ( STRING(J+1  :LAST), B, ERROR, K )

         IF ( ERROR .NE. ' ' ) THEN
            B = MAXVAL
         END IF

      END IF

      BEG = END + 1

      RETURN
      END
