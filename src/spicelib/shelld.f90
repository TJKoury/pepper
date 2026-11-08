!$Procedure SHELLD ( Shell sort a double precision array )

      subroutine shelld ( ndim, array )

! $ Abstract
!
!     Sort a double precision array using the Shell Sort algorithm.
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

      integer            ndim
      double precision   array ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NDIM       I   Dimension of the array.
!     ARRAY     I-O  The array.
!
! $ Detailed_Input
!
!     NDIM     is the number of elements in the array to be sorted.
!
!     ARRAY    on input, is the array to be sorted.
!
! $ Detailed_Output
!
!     ARRAY    on output, contains the same elements, sorted
!              in increasing order. The actual sorting is done
!              in place in ARRAY.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If NDIM < 2, this routine does not modify the array.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The Shell Sort Algorithm is well known.
!
! $ Examples
!
!     Let ARRAY contain the following elements:
!
!           99.D0
!           33.D0
!           55.D0
!           44.D0
!          -77.D0
!           66.D0
!
!     Then after a call to SHELLD, the array would be ordered as
!     follows:
!
!          -77.D0
!           33.D0
!           44.D0
!           55.D0
!           66.D0
!           99.D0
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
!        Added entry #1 in $Exceptions section.
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
!     shell sort a d.p. array
!
!-&


!
!     Local variables
!
      integer          gap
      integer          i
      integer          j
      integer          jg



!
!     This is a straightforward implementation of the Shell Sort
!     algorithm.
!
      gap = ndim / 2

      do while ( gap .gt. 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j .gt. 0 )
               jg = j + gap

               if ( array(j) .le. array(jg) ) then
                  j = 0
               else
                  call swapd ( array(j), array(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end
