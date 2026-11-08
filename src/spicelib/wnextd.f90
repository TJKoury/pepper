!$Procedure WNEXTD ( Extract the endpoints from a DP window )

      subroutine wnextd ( side, window )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract the left or right endpoints from a double precision
!     window.
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

      character(1)          side
      real(wp)              window  ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SIDE       I   Extract left ('L') or right ('R') endpoints.
!     WINDOW    I-O  Window to be extracted.
!
! $ Detailed_Input
!
!     SIDE     indicates whether the left or right endpoints of the
!              intervals in the window are to be extracted.
!
!                 'L', 'l'       Left endpoints.
!                 'R', 'r'       Right endpoints.
!
!              If SIDE is not recognized, the input window is not
!              changed.
!
!     WINDOW   on input, is a window containing zero or more intervals.
!
! $ Detailed_Output
!
!     WINDOW   on output, is the collection of singleton intervals
!              containing either the left or the right endpoints of the
!              intervals in the original window.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the endpoint specification, SIDE, is not recognized, the
!         error SPICE(INVALIDENDPNTSPEC) is signaled.
!
!     2)  The cardinality of the input WINDOW must be even. Left
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
!     This routine replaces every interval in the input window with
!     the singleton interval containing one of the endpoints of the
!     interval.
!
! $ Examples
!
!     Let WINDOW contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
!
!     Then the call
!
!           CALL WNEXTD (  'L', WINDOW )
!
!     produces the window
!
!           [ 1, 1 ]  [ 7, 7 ]  [ 23, 23 ]  [ 29, 29 ]
!
!     And the call
!
!           CALL WNEXTD ( 'R', WINDOW )
!
!     produces the window
!
!           [ 3, 3 ]  [ 11, 11 ]  [ 27, 27 ]  [ 29, 29 ]
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
!-    SPICELIB Version 1.1.0, 14-MAR-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added entry #2
!        in $Exceptions section.
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
!     extract the endpoints from a d.p. window
!
!-&


! $ Revisions
!
!-    Beta Version 1.2.0, 24-FEB-1989  (HAN)
!
!        Added calls to CHKIN and CHKOUT. Error handling was added to
!        detect invalid endpoint specification. The previous version
!        did not signal an error if SIDE was not 'R', 'r', 'L', or 'l'.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardd
      logical               return

!
!     Local variables
!
      integer(ip)           card
      integer(ip)           i



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'WNEXTD' )
      end if

!
!     Get the cardinality of the window. (The size is not important;
!     this routine can't create any new intervals.)
!
      card = cardd ( window )

!
!     Step through the window, keeping one endpoint from each interval.
!     For the sake of efficiency, we have separate loops for the two
!     possible values of SIDE.
!
      if ( side == 'L'  .or.  side == 'l' ) then

         do i = 1, card, 2
            window(i+1) = window(i)
         end do

      else if ( side == 'R'  .or.  side == 'r' ) then

         do i = 1, card, 2
            window(i) = window(i+1)
         end do

      else

         call setmsg ( 'SIDE was *.' )
         call errch  ( '*', side     )
         call sigerr ( 'SPICE(INVALIDENDPNTSPEC)' )

      end if

      call chkout ( 'WNEXTD' )
      return
      end
