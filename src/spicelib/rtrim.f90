!$Procedure RTRIM ( Right trim )

      INTEGER FUNCTION RTRIM ( STRING )

! $ Abstract
!
!     Return the maximum of 1 and the location of the last non-blank
!     character in the string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     STRING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         STRING

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!
!     STRING     I   String to be trimmed.
!
!     The function returns the maximum of 1 and the location of the
!     last non-blank character in STRING.
!
! $ Detailed_Input
!
!     STRING   is a string to be trimmed: the location of the
!              last non-blank character is desired.
!
! $ Detailed_Output
!
!     The function returns the maximum of 1 and the location of the
!     last non-blank character in STRING.
!
!     In particular, when STRING is blank, the function returns the
!     value 1.
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
!     When writing a character string to a file, we usually are content
!     to omit the trailing blanks. We'd like to use LASTNB as an upper
!     substring bound, but we have to handle the case where LASTNB
!     returns 0, so we write:
!
!
!        WRITE ( UNIT, '(A)' ),  STRING ( : MAX (1, LASTNB (STRING)) )
!
!
!     This can be simplified using RTRIM:
!
!
!        WRITE ( UNIT, '(A)' ),  STRING ( : RTRIM (STRING) )  )
!
!
!     This routine has a counterpart, LTRIM, which finds the maximum of
!     1 and the position of the first non-blank character of a string.
!
! $ Examples
!
!     1)  Write the non-blank portion of each element of a character
!         cell to file SPUD.DAT:
!
!            DO I = 1,  CARDC (CELL)
!
!               CALL WRLINE ( 'SPUD.DAT',
!           .                  CELL(I) ( LTRIM (CELL) : RTRIM (CELL) ) )
!
!            END DO
!
!         When CELL(I) is blank, the string ' ' will be written.
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
!-    SPICELIB Version 1.0.0, 02-MAY-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     right trim
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB

!
!     `Just do it'.
!
      RTRIM = MAX ( 1, LASTNB (STRING) )

      RETURN
      END
