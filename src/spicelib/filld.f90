!$Procedure FILLD ( Fill a double precision array )

      SUBROUTINE FILLD ( VALUE, NDIM, ARRAY )

! $ Abstract
!
!     Fill a double precision array with a specified value.
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

      DOUBLE PRECISION      VALUE
      INTEGER               NDIM
      DOUBLE PRECISION      ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Double precision value to be placed in all the
!                    elements of ARRAY.
!     NDIM       I   The number of elements in ARRAY.
!     ARRAY      O   Double precision array which is to be filled.
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
!     ARRAY    is a double precision array whose elements are to be
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
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Initialize all members of a double precision array to the same
!        value.
!
!
!        Example code begins here.
!
!
!              PROGRAM FILLD_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NDIM
!              PARAMETER           ( NDIM = 4 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      ARRAY ( NDIM )
!
!              INTEGER               I
!
!        C
!        C     Initialize all members of the array ARRAY to 1, and
!        C     print out its contents.
!        C
!              CALL FILLD ( 1.D0, NDIM, ARRAY )
!
!              WRITE(*,'(A)') 'Contents of ARRAY:'
!              DO I=1, NDIM
!
!                 WRITE(*,'(A,I2,A,F4.1)') '   Index:', I,
!             .                            '; value:',  ARRAY(I)
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Contents of ARRAY:
!           Index: 1; value: 1.0
!           Index: 2; value: 1.0
!           Index: 3; value: 1.0
!           Index: 4; value: 1.0
!
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
!-    SPICELIB Version 1.1.0, 19-FEB-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Updated the header to comply with NAIF standard. Added
!        full code example.
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
!     fill a d.p. array
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
