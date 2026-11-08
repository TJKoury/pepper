!$Procedure ORDERD ( Order of a double precision array )

      subroutine orderd ( array, ndim, iorder )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine the order of elements in a double precision array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     SORT
!
! $ Declarations

      implicit none

      real(wp)           array  ( * )
      integer(ip)        ndim
      integer(ip)        iorder ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARRAY      I    Input array.
!     NDIM       I    Dimension of ARRAY.
!     IORDER     O    Order vector for ARRAY.
!
! $ Detailed_Input
!
!     ARRAY    is the input array.
!
!     NDIM     is the number of elements in the input array.
!
! $ Detailed_Output
!
!     IORDER   is the order vector for the input array.
!              IORDER(1) is the index of the smallest element
!              of ARRAY; IORDER(2) is the index of the next
!              smallest; and so on.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  A negative input dimension causes this routine to leave the
!         output order vector unchanged.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     ORDERD finds the index of the smallest element of the input
!     array. This becomes the first element of the order vector.
!     The process is repeated for the rest of the elements.
!
!     The order vector returned by ORDERD may be used by any of
!     the REORD routines to sort sets of related arrays, as shown
!     in the example below.
!
! $ Examples
!
!     In the following example, the ORDER and REORD routines are
!     used to sort four related arrays (containing the names,
!     masses, integer ID codes, and visual magnitudes for a group
!     of satellites). This is representative of the typical use of
!     these routines.
!
!            C
!            C     Sort the object arrays by visual magnitude.
!            C
!                  CALL ORDERD ( VMAGS,  N, IORDER )
!
!                  CALL REORDC ( IORDER, N, NAMES )
!                  CALL REORDD ( IORDER, N, MASSES )
!                  CALL REORDI ( IORDER, N, CODES )
!                  CALL REORDR ( IORDER, N, VMAGS )
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 23-MAR-2010 (NJB)
!
!        Header example was updated to show use of this routine.
!        $Exceptions section was updated. Header sections were
!        re-ordered.
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
!     order of a d.p. array
!
!-&


!
!     Local variables
!
      integer(ip)      gap
      integer(ip)      i
      integer(ip)      j
      integer(ip)      jg


!
!     Begin with the initial ordering.
!
      do i = 1, ndim
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = ndim / 2

      do while ( gap .gt. 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j .gt. 0 )
               jg = j + gap

               if ( array(iorder(j)) .le. array(iorder(jg)) ) then
                  j = 0
               else
                  call swapi ( iorder(j), iorder(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end
