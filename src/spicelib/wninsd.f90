!$Procedure WNINSD ( Insert an interval into a DP window )

      subroutine wninsd ( left, right, window )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Insert an interval into a double precision window.
!
! $ Required_Reading
!
!     WINDOWS
!
! $ Keywords
!
!     WINDOWS
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      real(wp)              left
      real(wp)              right
      real(wp)              window    ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LEFT,
!     RIGHT      I   Left, right endpoints of new interval.
!     WINDOW    I-O  Input, output window.
!
! $ Detailed_Input
!
!     LEFT,
!     RIGHT    are the left and right endpoints of the interval to be
!              inserted.
!
!     WINDOW   on input, is a SPICE window containing zero or more
!              intervals.
!
! $ Detailed_Output
!
!     WINDOW   on output, is the original window following the insertion
!              of the interval from LEFT to RIGHT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS)
!         is signaled.
!
!     2)  If the insertion of the interval causes an excess of elements,
!         the error SPICE(WINDOWEXCESS) is signaled.
!
!     3)  The cardinality of the input WINDOW must be even. Left
!         endpoints of stored intervals must be strictly greater than
!         preceding right endpoints. Right endpoints must be greater
!         than or equal to corresponding left endpoints. Invalid window
!         data are not diagnosed by this routine and may lead to
!         unpredictable results.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine inserts the interval from LEFT to RIGHT into the
!     input window. If the new interval overlaps any of the intervals
!     in the window, the intervals are merged. Thus, the cardinality
!     of the input window can actually decrease as the result of an
!     insertion. However, because inserting an interval that is
!     disjoint from the other intervals in the window can increase the
!     cardinality of the window, the routine signals an error.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!     1) The following code example demonstrates how to insert an
!        interval into an existing double precision SPICE window, and
!        how to loop over all its intervals to extract their left and
!        right points.
!
!
!        Example code begins here.
!
!
!              PROGRAM WNINSD_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER               WNCARD
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               WNSIZE
!              PARAMETER           ( WNSIZE = 10 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE )
!              DOUBLE PRECISION      LEFT
!              DOUBLE PRECISION      RIGHT
!
!              INTEGER               I
!
!        C
!        C     Validate the window with size WNSIZE and zero elements.
!        C
!              CALL WNVALD( WNSIZE, 0, WINDOW )
!
!        C
!        C     Insert the intervals
!        C
!        C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!        C
!        C     into WINDOW.
!        C
!              CALL WNINSD(  1.D0,  3.D0, WINDOW )
!              CALL WNINSD(  7.D0, 11.D0, WINDOW )
!              CALL WNINSD( 23.D0, 27.D0, WINDOW )
!
!        C
!        C     Loop over the number of intervals in WINDOW, output
!        C     the LEFT and RIGHT endpoints for each interval.
!        C
!              DO I=1, WNCARD(WINDOW)
!
!                 CALL WNFETD( WINDOW, I, LEFT, RIGHT )
!
!                 WRITE(*,'(A,I2,2(A,F8.3),A)') 'Interval', I,
!             .                  ' [', LEFT,',',  RIGHT, ']'
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Interval 1 [   1.000,   3.000]
!        Interval 2 [   7.000,  11.000]
!        Interval 3 [  23.000,  27.000]
!
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 25-AUG-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Updated to remove unnecessary lines of code in the
!        Standard SPICE error handling CHKIN statements.
!
!        Edited the header to comply to NAIF standard. Added complete
!        code example, problem statement and solution. Added entry #3 in
!        $Exceptions section.
!
!        Removed irrelevant information related to other unary window
!        routines from $Particulars section.
!
!-    SPICELIB Version 1.3.0, 04-MAR-1993 (KRG)
!
!        There was a bug when moving the intervals in the cell
!        to the right when inserting a new interval to the left
!        of the left most interval. The incrementing in the DO
!        loop was incorrect.
!
!        The loop used to read:
!
!           DO J = I-1, CARD
!              WINDOW(J+2) = WINDOW(J)
!           END DO
!
!        which squashed everything to the right of the first interval
!        with the values of the first interval.
!
!        The loop now reads:
!
!           DO J = CARD, I-1, -1
!              WINDOW(J+2) = WINDOW(J)
!           END DO
!
!        which correctly scoots the elements in reverse order,
!        preserving their values.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     insert an interval into a d.p. window
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.3.0, 04-MAR-1993 (KRG)
!
!        There was a bug when moving the intervals in the cell
!        to the right when inserting a new interval to the left
!        of the left most interval. the incrementing in the DO
!        loop was incorrect.
!
!        The loop used to read:
!
!           DO J = I-1, CARD
!              WINDOW(J+2) = WINDOW(J)
!           END DO
!
!        which squashed everything to the right of the first interval
!        with the values of the first interval.
!
!        The loop now reads:
!
!           DO J = CARD, I-1, -1
!              WINDOW(J+2) = WINDOW(J)
!           END DO
!
!        which correctly scoots the elements in reverse order,
!        preserving their values.
!
!-    Beta Version 1.2.0, 27-FEB-1989 (HAN)
!
!        Due to the calling sequence and functionality changes
!        in the routine EXCESS, the method of signaling an
!        excess of elements needed to be changed.
!
!-    Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB)
!
!        Contents of the $Required_Reading section was
!        changed from "None." to "WINDOWS".  Also, the
!        declaration of the unused variable K was removed.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardd
      integer(ip)           sized
      logical               return

!
!     Local Variables
!
      integer(ip)           size
      integer(ip)           card

      integer(ip)           i
      integer(ip)           j



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'WNINSD' )

!
!     Get the size and cardinality of the window.
!
      size = sized ( window )
      card = cardd ( window )

!
!     Let's try the easy cases first. No input interval? No change.
!     Signal that an error has occurred and set the error message.
!
      if ( left .gt. right ) then
         call setmsg ( 'Left endpoint was *. Right endpoint was *.' )
         call errdp  ( '*', left  )
         call errdp  ( '*', right )
         call sigerr ( 'SPICE(BADENDPOINTS)' )
         call chkout ( 'WNINSD' )
         return

!
!     Empty window? Input interval later than the end of the window?
!     Just insert the interval, if there's room.
!
      else if ( card .eq. 0  .or.  left .gt. window(card) ) then

         if ( size .ge. card+2 ) then

            call scardd ( card+2, window )
            window(card+1) = left
            window(card+2) = right

         else

            call excess ( 2, 'window' )
            call sigerr ( 'SPICE(WINDOWEXCESS)' )

         end if

         call chkout ( 'WNINSD' )
         return

      end if


!
!     Now on to the tougher cases.
!
!     Skip intervals which lie completely to the left of the input
!     interval. (The index I will always point to the right endpoint
!     of an interval).
!
      i = 2

      do while ( i .le. card  .and.  window(i) .lt. left )
         i = i + 2
      end do

!
!     There are three ways this can go. The new interval can:
!
!        1) lie entirely between the previous interval and the next.
!
!        2) overlap the next interval, but no others.
!
!        3) overlap more than one interval.
!
!     Only the first case can possibly cause an overflow, since the
!     other two cases require existing intervals to be merged.
!

!
!     Case (1). If there's room, move succeeding intervals back and
!     insert the new one. If there isn't room, signal an error.
!
      if ( right .lt. window(i-1) ) then

         if ( size .ge. card+2 ) then

            do j = card, i-1, -1
               window(j+2) = window(j)
            end do

            call scardd ( card+2, window )
            window(i-1) = left
            window(i  ) = right

         else

            call excess ( 2, 'window' )
            call sigerr ( 'SPICE(WINDOWEXCESS)' )
            call chkout ( 'WNINSD' )
            return

         end if

!
!     Cases (2) and (3).
!
      else

!
!        The left and right endpoints of the new interval may or
!        may not replace the left and right endpoints of the existing
!        interval.
!
         window(i-1) = min ( left,  window(i-1) )
         window(i  ) = max ( right, window(i  ) )

!
!        Skip any intervals contained in the one we modified.
!        (Like I, J always points to the right endpoint of an
!        interval.)
!
         j = i + 2

         do while ( j .le. card  .and.  window(j) .le. window(i) )
            j = j + 2
         end do

!
!        If the modified interval extends into the next interval,
!        merge the two. (The modified interval grows to the right.)
!
         if ( j .le. card  .and.  window(i) .ge. window(j-1) ) then
            window(i) = window(j)
            j         = j + 2
         end if

!
!        Move the rest of the intervals forward to take up the
!        spaces left by the absorbed intervals.
!
         do while ( j .le. card )
            i           = i + 2
            window(i-1) = window(j-1)
            window(i  ) = window(j  )
            j           = j + 2
         end do

         call scardd ( i, window )

      end if

      call chkout ( 'WNINSD' )
      return
      end
