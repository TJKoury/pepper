!$Procedure ZZCK4D2I ( Unpack a set of integers from DP number )

      SUBROUTINE ZZCK4D2I ( DPCOEF, NSETS, PARCOD, I )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the routine that unpacks a set integer numbers stored in
!     a single double precision number.
!
!     Its current specific use is to "uncompress" seven integer numbers
!     representing numbers of polynomial coefficients in a logical
!     type 4 CK record from a single DP number stored in a physical
!     type 4 CK record in a file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION        DPCOEF
      INTEGER                 NSETS
      DOUBLE PRECISION        PARCOD
      INTEGER                 I      ( * )


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DPCOEF     I   DP number containing packed integer numbers.
!     NSETS      I   Number of integer components packed in DPCOEF.
!     PARCOD     I   Packing base.
!     I          O   Array of NSETS integer components.
!
! $ Detailed_Input
!
!     DPCOEF     is a DP number containing NSETS integers packed
!                together.
!
!     NSETS      is the number of integers packed in the DPCOEF.
!
!     PARCOD     is the packing base.
!
! $ Detailed_Output
!
!     I          is an array containing unpacked integers.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine unpacks NSETS integers packed in a single double
!     precision number using base specified by PARCOD and stored them
!     in the array I. The integers are packed in the DP using the
!     following algorithm:
!
!        [DPCOEF]= PARCOD ** ( NSETS - 1 ) * I( 1 )        +
!                  PARCOD ** ( NSETS - 2 ) * I( 2 )        +
!                  ...
!                  PARCOD ** 1             * I( NSETS - 1 )+
!                  PARCOD ** 0             * I( NSETS )
!     where:
!
!        I(1:NSETS) - is an array of integer numbers with values
!                     in the range [0:PARCOD-1].
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     Output integer array I must have enough space to hold NSETS
!     numbers.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     Y.K. Zaiko     (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
!
!-&

!
!     Local variables.
!
      DOUBLE PRECISION        X
      INTEGER                 K

!
!     Let's unpack it!
!
      X = PARCOD ** ( NSETS - 1 )

      DO K = 0, NSETS - 1

         I( NSETS - K ) = DPCOEF / X
         DPCOEF = DPCOEF - I( NSETS - K ) * X
         X = X / PARCOD

      END DO

!
!     All done.
!
      RETURN

      END
