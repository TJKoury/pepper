!$Procedure CLEARD ( Clear a double precision array )

      SUBROUTINE CLEARD ( NDIM, ARRAY )

! $ Abstract
!
!     Fill a double precision array with zeros.
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

      INTEGER            NDIM
      DOUBLE PRECISION   ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NDIM       I   The number of elements of ARRAY which are to be
!                    set to zero.
!     ARRAY      O   Double precision array to be filled.
!
! $ Detailed_Input
!
!     NDIM     is the number of elements in ARRAY which are to be
!              set to zero.
!
! $ Detailed_Output
!
!     ARRAY    is the double precision array which is to be filled
!              with zeros.
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
!     1) Initialize all members of a double precision array to the
!        same value and clear it afterwards.
!
!
!        Example code begins here.
!
!
!              PROGRAM CLEARD_EX1
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
!        C     Initialize all member of the array ARRAY to 11.5, and
!        C     print out its contents.
!        C
!              CALL FILLD ( 11.5D0, NDIM, ARRAY )
!
!              WRITE(*,'(A)') 'Contents of ARRAY before CLEARD:'
!              WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM )
!
!        C
!        C     Clear the contents of ARRAY and print it.
!        C
!              CALL CLEARD ( NDIM, ARRAY )
!
!              WRITE(*,*)
!              WRITE(*,'(A)') 'Contents of ARRAY after CLEARD:'
!              WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Contents of ARRAY before CLEARD:
!          11.5  11.5  11.5  11.5
!
!        Contents of ARRAY after CLEARD:
!           0.0   0.0   0.0   0.0
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 29-MAY-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Updated the header to comply with NAIF standard. Added
!        full code example.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     clear a d.p. array
!
!-&


!
!     Local variables
!
      INTEGER I


      DO I = 1, NDIM
        ARRAY(I) = 0.D0
      END DO


      RETURN
      END
