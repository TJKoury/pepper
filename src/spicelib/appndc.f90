!$Procedure APPNDC ( Append an item to a character cell )

      subroutine appndc ( item, cell )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Append an item to a character cell.
!
! $ Required_Reading
!
!     CELLS
!
! $ Keywords
!
!     CELLS
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character*(*)         item
      character*(*)         cell ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEM       I   The item to append.
!     CELL      I-O  The cell to which ITEM will be appended.
!
! $ Detailed_Input
!
!     ITEM     is a character string which is to be appended to CELL.
!
!     CELL     is a character SPICE cell to which ITEM will be
!              appended.
!
! $ Detailed_Output
!
!     CELL     is the input cell with ITEM appended. ITEM is the last
!              member of CELL.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input cell has invalid cardinality, an error is
!         signaled by a routine in the call tree of this routine.
!
!     2)  If the input cell has invalid size, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If the cell is not large enough to accommodate the addition
!         of a new element, the error SPICE(CELLTOOSMALL) is signaled.
!
!     4)  If the length of the item is longer than the length of the
!         cell, ITEM is truncated on the right.
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
!     In the following example, the item 'PLUTO' is appended to
!     the character cell PLANETS.
!
!     Before appending 'PLUTO', the cell contains:
!
!     PLANETS (1) = 'MERCURY'
!     PLANETS (2) = 'VENUS'
!     PLANETS (3) = 'EARTH'
!     PLANTES (4) = 'MARS'
!     PLANETS (5) = 'JUPITER'
!     PLANETS (6) = 'SATURN'
!     PLANETS (7) = 'URANUS'
!     PLANETS (8) = 'NEPTUNE'
!
!     The call
!
!        CALL APPNDC ( 'PLUTO', PLANETS )
!
!     appends the element 'PLUTO' at the location PLANETS (9), and the
!     cardinality is updated.
!
!     If the cell is not big enough to accommodate the addition of
!     the item, an error is signaled. In this case, the cell is not
!     altered.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Improved the documentation of CELL in $Detailed_Input and
!        $Detailed_Output. Added entries #1 and #2 to $Exceptions.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
!
!-&


! $ Index_Entries
!
!     append an item to a character cell
!
!-&


!
!     SPICELIB functions
!
      logical               return
      integer(ip)           cardc
      integer(ip)           sizec

!
!     Local variables
!
      integer(ip)           nwcard





!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'APPNDC' )
      end if

!
!     Check to see if the cell can accommodate the addition of a
!     new item. If there is room, append the item to the cell and
!     reset the cardinality. If the cell cannot accommodate the
!     addition of a new item, signal an error.
!

      nwcard = cardc (cell) + 1

      if (  ( nwcard )  .le.  ( sizec (cell) )  ) then

         cell (nwcard) = item
         call scardc ( nwcard, cell )

      else

        call setmsg ( 'The cell cannot accommodate the '     // &
                      'addition of the item *.'                 )

        call errch  ( '*', item )
        call sigerr ( 'SPICE(CELLTOOSMALL)' )

      end if


      call chkout ( 'APPNDC' )
      return
      end
