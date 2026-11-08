!$Procedure RMDUPD ( Remove duplicates from a double precision array )

      subroutine rmdupd ( nelt, array )

! $ Abstract
!
!     Remove duplicate elements from a double precision array.
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

      implicit none

      integer          nelt
      double precision array   (*)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NELT      I-O  Number of elements in the array.
!     ARRAY     I-O  Input/output array.
!
! $ Detailed_Input
!
!     NELT     on input is the number of elements in the input
!              array.
!
!     ARRAY    on input contains zero or more elements, from which
!              all duplicate elements are to be removed.
!
! $ Detailed_Output
!
!     NELT     on output is the number of elements in the output
!              array.
!
!     ARRAY    on output contains the distinct elements of the
!              input array, sorted in increasing order. (Character
!              arrays are sorted according to the ASCII collating
!              sequence).
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
!     None.
!
! $ Examples
!
!     Let the arrays C and I contain the following elements.
!
!           NC   = 7                NI   =   5
!           C(1) = 'Miranda'        I(1) =  13
!           C(2) = 'Ariel'          I(2) = -13
!           C(3) = 'Umbriel'        I(3) =   0
!           C(4) = 'Titania'        I(4) =   1
!           C(5) = 'Miranda'        I(5) =   0
!           C(6) = 'Oberon'
!           C(7) = 'Umbriel'
!
!     Then following the calls
!
!           CALL RMDUPC ( NC, C )
!           CALL RMDUPI ( NI, I )
!
!     C and I contain the following.
!
!           NC   = 5                NI   =   4
!           C(1) = 'Ariel'          I(1) = -13
!           C(2) = 'Miranda'        I(2) =   0
!           C(3) = 'Oberon'         I(3) =   1
!           C(4) = 'Titania'        I(4) =  13
!           C(5) = 'Umbriel'
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
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
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
!     remove duplicates from a d.p. array
!
!-&


!
!     Local variables
!
      integer          i
      integer          j



!
!     Proceed only if the array actually contains more than one element.
!
      if ( nelt .gt. 1 ) then

!
!        Sort the array in place.
!
         call shelld ( nelt, array )

!
!        Drop duplicate entries. Compare adjacent entries, and move
!        duplicates forward. (Duplicates are now adjacent, because of
!        sorting.)
!
         j = 1

         do i = 2, nelt

            if ( array(i) .ne. array(i-1) ) then
               j        = j + 1
               array(j) = array(i)
            end if

        end do

        nelt = j

      end if

      return
      end
