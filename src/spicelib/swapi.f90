!$Procedure SWAPI ( Swap integer values )

      subroutine swapi ( a, b )

! $ Abstract
!
!     Swap the contents of two integer variables.
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

      implicit none

      integer          a
      integer          b

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A         I-O  First variable.
!     B         I-O  Second variable.
!
! $ Detailed_Input
!
!     A,
!     B        are two variables, the contents of which are to
!              be swapped (exchanged).
!
! $ Detailed_Output
!
!     A,
!     B        are the same two variables, after their contents
!              have been exchanged.
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
!     This is just shorthand notation for the code fragment
!
!           TEMP = A
!           A    = B
!           B    = TEMP
!
! $ Examples
!
!     Let
!           A = 11
!           B = 22
!
!     Then after calling SWAPI (A,B),
!
!           A = 22
!           B = 11
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     swap integer values
!
!-&


!
!     Local variables
!
      integer          temp



!
!     What is there to say?
!
      temp = a
      a    = b
      b    = temp

      return
      end
