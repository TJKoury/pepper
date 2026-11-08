!$Procedure SSIZEC ( Set the size of a character cell )

      subroutine ssizec ( size, cell )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the size (maximum cardinality) of a character cell.
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

      integer(ip)        size
      character*(*)      cell   ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SIZE       I   Size (maximum cardinality) of the cell.
!     CELL       O   The cell.
!
! $ Detailed_Input
!
!     SIZE     is the size (maximum number of elements) of the cell.
!
! $ Detailed_Output
!
!     CELL     is a cell.
!
!
!                On output, the size of the cell is SIZE. The
!                cardinality of the cell is 0. The rest of the
!                control area is zeroed out.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
!     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
!     initialize cells for subsequent use. Since all cell routines
!     expect to find the size and cardinality of a cell in place,
!     no cell can be used until both have been set.
!
! $ Examples
!
!     In the example below, the size and cardinality of the character
!     cell FRED are set in the main module of the program FLNSTN.
!     Both are subsequently retrieved, and the cardinality changed,
!     in one of its subroutines, WILMA.
!
!           PROGRAM FLNSTN
!
!           CHARACTER*30     FRED ( LBCELL:100 )
!            .
!            .
!           CALL SSIZEC ( 100, FRED )
!            .
!            .
!           CALL WILMA ( FRED )
!            .
!            .
!           STOP
!           END
!
!
!           SUBROUTINE WILMA ( FRED )
!
!           CHARACTER*(*)      FRED  ( LBCELL:* )
!           INTEGER            SIZE
!           INTEGER            CARD
!
!           INTEGER            CARDC
!           INTEGER            SIZEC
!            .
!            .
!           SIZE = SIZEC ( FRED )
!           CARD = CARDC ( FRED )
!            .
!            .
!           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
!           CALL EXCESS ( CARD-SIZE, 'cell' )
!            .
!            .
!           RETURN
!           END
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
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
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
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     set the size of a character cell
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
!
!        Check for invalid size value added. An error
!        is signaled if the value is out of range. The cardinality
!        is now automatically reset to 0. The rest of the control
!        area is now zeroed out.
!
!        The examples have been updated to illustrate set initialization
!        without the use of the EMPTYx routines, which have been
!        removed from the library. Errors in the examples have been
!        removed, also.
!
!-&


!
!     SPICELIB functions
!
      logical               return


!
!     Local variables
!
      integer(ip)           i



      if ( return() ) then
         return
      else
         call chkin ( 'SSIZEC' )
      end if

!
!     The size must be non-negative.  Other values will be snubbed.
!
      if (  size .lt. 0  )  then
         call setmsg ( 'Attempt to set size of cell to invalid '      // &
                       'value.  The value was #.'  )
         call errint ( '#', size )
         call sigerr ( 'SPICE(INVALIDSIZE)' )
         call chkout ( 'SSIZEC' )
         return
      end if


!
!     Not much to this.
!
      call enchar ( size, cell(-1) )
      call enchar ( 0,    cell( 0) )

      do i = lbcell, -2
         call enchar ( 0, cell(i) )
      end do

      call  chkout ( 'SSIZEC' )
      return
      end
