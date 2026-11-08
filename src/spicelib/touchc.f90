!$Procedure TOUCHC ( Touch a variable )

      CHARACTER FUNCTION TOUCHC ( STRING )

! $ Abstract
!
!     Return the first character of a string
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         STRING

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   any character string
!
!      The function returns the first character of the string.
!
! $ Detailed_Input
!
!     STRING   is any character string
!
! $ Detailed_Output
!
!     The function returns the first character of the string
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
!     This is a utility routine so that formal arguments to a routine
!     that are never used can be given the appearance of being used
!     to a compiler. In this way it is possible to leave "hooks" in
!     a calling sequence even if those variables are for the moment
!     unused. Similarly, variables declared for future use can be left
!     in place so that they don't need to be commented out
!
! $ Examples
!
!     Suppose that a routine takes as an argument a
!     fortran structure implemented as a set of parallel arrays.
!     But that one of the arrays is not needed for the purposes of
!     the routine. This routine allows you to touch that array
!     without changing it.
!
!
!        SUBROUTINE INCPTR ( N, PTR, VALUES )
!
!        This routine increments the current pointer into a circular
!        array of character strings.
!
!        INTEGER               N
!        INTEGER               PTR
!        CHARACTER*(*)         VALUES ( * )
!
!        Even though we don't need to do anything with the values
!        array, it's passed for the sake of uniformity in calling
!        sequences. Touch the VALUES array so that the compiler
!        will think it's been used.
!
!        VALUES(1)(1:1) = TOUCHC ( VALUES(1)(1:1) )
!
!        PTR = PTR + 1
!
!        IF ( PTR .GT. N ) THEN
!           PTR = 1
!        END IF
!        RETURN
!
! $ Restrictions
!
!     1)  If you use this routine, it would be a very good idea to
!         write down why you are using it in the calling routine.
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
!-    SPICELIB Version 1.0.1, 16-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 06-MAY-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     touch a character string
!
!-&


      TOUCHC = STRING(1:1)
      RETURN
      END
