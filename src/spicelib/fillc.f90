!$Procedure FILLC ( Fill a character array )

      SUBROUTINE FILLC ( VALUE, NDIM, ARRAY )

! $ Abstract
!
!     Fill a character string array with a specified string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     ASSIGNMENT
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    VALUE
      INTEGER          NDIM
      CHARACTER*(*)    ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     VALUE      I   Character string value to be placed in ARRAY.
!     NDIM       I   The number of elements in ARRAY.
!     ARRAY      O   Character string array which is to be filled.
!
! $ Detailed_Input
!
!     VALUE    is the value to be assigned to the array elements
!              1 through NDIM.
!
!     NDIM     is the number of elements in the array.
!
! $ Detailed_Output
!
!     ARRAY    is a character string array whose elements are to be
!              set to VALUE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If NDIM < 1, the array is not modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     Let  VALUE = '*'
!          NDIM  =  4
!
!     then the contents of ARRAY are:
!
!          ARRAY (1) = '*'
!          ARRAY (2) = '*'
!          ARRAY (3) = '*'
!          ARRAY (4) = '*'
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
!     W.M. Owen          (JPL)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     fill a character array
!
!-&


!
!     Local variables
!
      INTEGER I



      DO I = 1, NDIM
        ARRAY(I) = VALUE
      END DO


      RETURN
      END
