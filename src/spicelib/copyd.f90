!$Procedure COPYD ( Copy a double precision cell )

      subroutine copyd ( cell, copy )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Copy the contents of a double precision cell to another cell.
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

      real(wp)           cell     ( lbcell:* )
      real(wp)           copy     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CELL       I   Cell to be copied.
!     COPY       O   New cell.
!
! $ Detailed_Input
!
!     CELL     is a cell.
!
! $ Detailed_Output
!
!     COPY     is a cell which contains the same elements as the
!              input cell, in the same order. If the size (maximum
!              cardinality) of the output cell is smaller than
!              the cardinality of the input cell, then only as many
!              items as will fit in the output cell are copied,
!              and an error is signaled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the output cell in not large enough to hold the elements
!         of the input cell, the error SPICE(CELLTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The copy routines (COPYC, COPYD, and COPYI) are used primarily
!     to manipulate working cells, since many routines that use cells
!     (binary set routines, for instance) do not allow cells to be
!     combined or manipulated in place.
!
! $ Examples
!
!     In the following example, COPYC is used to copy the result
!     of the union of two sets (ordered cells) from a temporary
!     working set back into the one of the original set.
!
!           CALL UNIONC ( BODIES, PLANETS, TEMP )
!           CALL COPYC  ( TEMP,   BODIES        )
!
!     If the size of the temporary cell is greater than the size
!     of the original set, the function FAILED should be checked to be
!     sure that no overflow occurred. If BODIES is at least as
!     large as TEMP, no such check is necessary.
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     copy a d.p. cell
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 09-JAN-1989 (NJB)
!
!        Calling protocol for EXCESS has been changed. Call to SETMSG
!        has been removed.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)      sized
      integer(ip)      cardd
      logical          return

!
!     Local variables
!
      integer(ip)      size
      integer(ip)      card
      integer(ip)      moved
      integer(ip)      i

!
!     Set up the error processing.
!
      if ( return() ) return
      call chkin ( 'COPYD' )


!
!     We need the cardinality of the input cell, and the size of
!     the output cell.
!
      card = cardd ( cell )
      size = sized ( copy )

!
!     Start moving the elements, one by one. Stop if the output
!     cell fills up.
!
      moved = min ( card, size )

      do i = 1, moved
         copy(i) = cell(i)
      end do

!
!     Set the cardinality of the output cell. Report any excess.
!
      call scardd ( moved, copy )


      if ( card .gt. size ) then

         call excess ( card - size, 'cell' )
         call sigerr ( 'SPICE(CELLTOOSMALL)' )
         call chkout ( 'COPYD' )
         return

      end if


      call chkout ( 'COPYD' )
      return
      end
