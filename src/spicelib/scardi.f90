!$Procedure SCARDI ( Set the cardinality of an integer cell )

      subroutine scardi ( card, cell )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the cardinality of an integer cell.
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

      integer(ip)           card
      integer(ip)           cell   ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CARD       I   Cardinality of (number of elements in) the cell.
!     CELL       O   The cell.
!
! $ Detailed_Input
!
!     CARD     is the cardinality of (number of elements in) the
!              cell.
!
! $ Detailed_Output
!
!     CELL     is a cell.
!
!
!                On output, the cardinality of the cell is CARD.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the cardinality value supplied is less than 0 or greater
!         than the cell size, the error SPICE(INVALIDCARDINALITY) is
!         signaled.
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
!     The set cardinality routines are also used by library routines
!     which manipulate cells (including set and window routines) to
!     reset the cardinalities of cells as they gain or lose elements.
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
!     set the cardinality of an integer cell
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
!
!        Check for invalid cardinality value added. An error
!        is signaled if the value is out of range. Examples
!        updated so as not to refer to the EMPTYx routines, and
!        to show the correct calling protocol for EXCESS.
!
!-&


!
!     SPICELIB functions
!
      logical               return


      if ( return() ) return

!
!     The cardinality may range from 0 to the size of the cell,
!     inclusive.  Other values will be snubbed.
!
      if (  ( card < 0 )  .or.  ( card > cell(-1) )  )  then
         call setmsg ( 'Attempt to set cardinality of cell '          // &
                       'to invalid value.  The value was #.'  )
         call errint ( '#', card )
         call sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return
      end if

!
!     Not much to this.
!
      cell(0) = card

      return
      end
