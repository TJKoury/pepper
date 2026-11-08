!$Procedure SAMEAI ( Are two integer arrays the same? )

      LOGICAL FUNCTION SAMEAI ( A1, A2, NDIM )

! $ Abstract
!
!     Indicate whether two integer arrays are equal.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               A1 ( * )
      INTEGER               A2 ( * )
      INTEGER               NDIM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A1         I    First array to be compared.
!     A2         I    Second array to be compared.
!     NDIM       I    Dimension of A1 and A2.
!
!     The function returns the value .TRUE. if and only if A1 = A2.
!
! $ Detailed_Input
!
!     A1,
!     A2       are two integer arrays to be compared.  A1 and
!              A2 must have the same dimension.
!
!     NDIM     is the common dimension of A1 and A2.
!
! $ Detailed_Output
!
!     The function takes the value .TRUE. if and only if A1 equals A2.
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
!     This function can be thought of as a macro. It replaces the
!     loop
!
!        SAME  = .TRUE.
!        I     =  1
!
!        DO WHILE (  ( I .LE. NDIM )  .AND.  SAME  )
!
!           IF ( A1(I) .NE.  A2(I)  )
!              SAME  = .FALSE.
!           ELSE
!              I     =  I + 1
!           END IF
!
!        END DO
!
! $ Examples
!
!     1)  Test two integer arrays A1 and A2 for equality, where both
!         arrays have declared length 10:
!
!            SAME  =  SAMEAI ( A1, A2, 10 )
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     test two integer arrays for equality
!
!-&


!
!     Local variables
!
      INTEGER               I

!
!     Executable code
!
      SAMEAI = .TRUE.

      DO I = 1, NDIM

         IF (  A1(I)  .NE.  A2(I)  ) THEN
            SAMEAI = .FALSE.
            RETURN
         END IF

      END DO

      RETURN
      END
