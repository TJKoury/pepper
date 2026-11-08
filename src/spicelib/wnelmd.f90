!$Procedure WNELMD ( Element of a DP window )

      logical function wnelmd ( point, window )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine whether a point is an element of a double precision
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

      real(wp)              point
      real(wp)              window      ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POINT      I   Input point.
!     WINDOW     I   Input window.
!
!     The function returns .TRUE. if POINT is an element of WINDOW.
!
! $ Detailed_Input
!
!     POINT    is a point, which may or may not be contained in one of
!              the intervals in WINDOW.
!
!     WINDOW   is a SPICE window containing zero or more intervals.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if the input point is an element of
!     the input window --- that is, if
!
!        a(i)  <  POINT  <  b(i)
!              -         -
!
!     for some interval [ a(i), b(i) ] in WINDOW --- and returns .FALSE.
!     otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The cardinality of the input WINDOW must be even. Left
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
!     None.
!
! $ Examples
!
!     Let A contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!     Then the following expressions are true
!
!           WNELMD ( 1, WINDOW )
!           WNELMD ( 9, WINDOW )
!
!     and the following expressions are false.
!
!           WNELMD (  0, WINDOW )
!           WNELMD ( 13, WINDOW )
!           WNELMD ( 29, WINDOW )
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 25-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added entry #1
!        in $Exceptions section.
!
!        Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of
!        the function.
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
!     element of a d.p. window
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
         wnelmd = .false.
         return
      end if


!
!     How many endpoints in the window?
!
      card = cardd ( window )

!
!     Check the point against every interval in the window. Quit if
!     we find an interval that contains it. Inefficient, but it works.
!
      do i = 1, card, 2

         if ( point >= window(i)  .and. point <= window(i+1) ) then
            wnelmd = .true.
            return
         end if

      end do

      wnelmd = .false.


      return
      end
