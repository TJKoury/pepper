!$Procedure      ZZCK4I2D ( Pack set of integers into a single DP )

      subroutine zzck4i2d ( i, nsets, parcod, dpcoef )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the routine that packs a set integer numbers into a
!     single double precision number.
!
!     Its current specific use is to "compress" seven integer numbers
!     representing numbers of polynomial coefficients in a logical
!     type 4 CK record into a single DP number stored in a physical
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

      implicit none

      integer                 i      ( * )
      integer                 nsets
      double precision        parcod
      double precision        dpcoef

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     I          I   Array of NSETS integer components.
!     NSETS      I   Number of integer components in input array I.
!     PARCOD     I   Packing base.
!     DPCOEF     O   DP number containing NSETS packed integer numbers.
!
! $ Detailed_Input
!
!     I          is an array containing integers to be packed.
!
!     NSETS      is the number of elements in the array I.
!
!     PARCOD     is the packing base.
!
! $ Detailed_Output
!
!     DPCOEF     is a DP number containing elements of the input
!                array packed together.
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
!     No checking is done to determine whether elements of the array
!     I are within range [0:PARCOD-1] and whether PARCOD**NSETS will
!     cause DPCOEF mantissa overflow.
!
! $ Particulars
!
!     This routine packs NSETS elements of the array I into a single
!     double precision variable using base specified by PARCOD. When
!     packed the double precision number DPCOEF represents NSETS of
!     integer elements of the array I as follows:
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
!     1) No checking is done to determine whether elements of the
!        array I are within range [0:PARCOD-1] to prevent "overflow"
!        of particular
!
!     2) No checking is done to determine whether PARCOD**NSETS
!        will cause DPCOEF mantissa overflow.
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
!     Local variables
!
      double precision        x
      integer                 k

!
!     Let's pack it!
!
      dpcoef = 0.d0

      x = 1

      do k = 1, nsets, 1

         dpcoef = dpcoef + i( k ) * x
         x = x * parcod

      end do

!
!     All done.
!
      return
      end
