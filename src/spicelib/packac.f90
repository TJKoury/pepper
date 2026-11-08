!$Procedure PACKAC ( Pack a character array )

      subroutine packac ( in, &
                          pack,   npack, &
                          maxout, nout,   out )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Pack the contents of a CHARACTER array. That is, take
!     a set of arbitrarily spaced elements from an input array,
!     and make them adjacent elements in an output array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     ASSIGNMENT
!     UTILITY
!
! $ Declarations

      implicit none

      character*(*)    in      ( * )
      integer(ip)      pack    ( * )
      integer(ip)      npack
      integer(ip)      maxout

      integer(ip)      nout
      character*(*)    out     ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input array.
!     PACK       I   Indices of elements to be packed.
!     NPACK      I   Number of indices.
!     MAXOUT     I   Maximum number of elements in the output array.
!     NOUT       O   Number of elements in the output array.
!     OUT        O   Output array.
!
! $ Detailed_Input
!
!     IN       is the input array.
!
!     PACK     is the set of elements to be packed into the output
!              array. PACK(i) is the index of the element in the
!              input array that is to become the i'th element of
!              the output array.
!
!     NPACK    is the number of elements to be packed into the
!              output array.
!
!     MAXOUT   is the maximum number of elements to be packed
!              into the output array. If NPACK is larger than
!              MAXOUT, the extra items are ignored.
!
! $ Detailed_Output
!
!     NOUT     is the number of elements in the output array.
!
!     OUT      is the output array. This array contains up to
!              MAXOUT elements from the input array, located
!              in the first NOUT elements of the array.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an element in the PACK array is less than 1, the error
!         SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The indicated elements are moved from their current locations
!     in the input array to consecutive positions in the output array.
!
!        OUT(   1) = IN(PACK(   1))
!        OUT(   2) = IN(PACK(   2))
!             .
!             .
!        OUT(NOUT) = IN(PACK(NOUT))
!
!     NOUT is either NPACK or MAXOUT, whichever is smaller.
!
! $ Examples
!
!     The most common use for this routine is to remove unwanted items
!     from an array or set of arrays. For example, suppose that the
!     arrays NAME, CODE, RADIUS and MASS contain the names, NAIF
!     integer ID codes, radii, and masses of a set of NSAT satellites.
!     Suppose further that the user selects a subset of the original
!     set of satellites from a menu of some sort. Let the indices of
!     these satellites be the NSEL elements of the array SEL. The
!     following sequence would remove the names, codes, etc., of the
!     unselected satellites from the arrays.
!
!        CALL PACKAC ( NAME,   SEL, NSEL, NSAT, NOUT, NAME2   )
!        CALL PACKAI ( CODE,   SEL, NSEL, NSAT, NOUT, CODE2   )
!        CALL PACKAD ( RADIUS, SEL, NSEL, NSAT, NOUT, RADIUS2 )
!        CALL PACKAD ( MASS,   SEL, NSEL, NSAT, NOUT, MASS2   )
!
!     In the example above, suppose that NAME and PACK contain
!     the following:
!
!        NAME = 'MIMAS'          PACK = 2, 4, 6, 7
!               'ENCELADUS'
!               'TETHYS'
!               'DIONE'
!               'RHEA'
!               'TITAN'
!               'HYPERION'
!               'IAPETUS'
!               'PHOEBE'
!
!     Then, following the call to PACKAC, NOUT and NAME2 contain
!     the following:
!
!        NOUT = 4                 NAME2 = 'ENCELADUS'
!                                         'DIONE'
!                                         'TITAN'
!                                         'HYPERION'
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
!
!        Header correction: assertions that the output
!        can overwrite the input have been removed.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     pack a character array
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 4-JAN-1989 (HAN)
!
!         Error handling was added to detect array indices that are
!         out of bound. If any element contained in the PACK array is
!         less than one, an error is signaled, and the output array is
!         not packed.
!
!-&


!
!     Spicelib functions
!
      logical               return

!
!
!     Local variables
!
      integer(ip)      i




!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'PACKAC' )
      end if


!
!     First, determine how many items to transfer.
!
      nout = min ( npack, maxout )


!
!     Check to see if PACK contains valid array indices.
!
      do i = 1, nout

         if ( pack (i) .lt. 1 ) then
            call setmsg ( 'Element number * contains index *.' )
            call errint ( '*', i                )
            call errint ( '*', pack(i)          )
            call sigerr ( 'SPICE(INVALIDINDEX)' )
            call chkout ( 'PACKAC' )
            return
         end if

      end do


!
!     Transfer them. Just like it says in the header.
!
      do i = 1, nout
         out(i) = in(pack(i))
      end do


      call chkout ( 'PACKAC' )
      return
      end
