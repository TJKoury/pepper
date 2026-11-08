!$Procedure DASINE (arc sine of bracketed argument)

      real(wp) function dasine ( arg, tol )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Compute arc sine of a bracketed argument.
!
!     This routine produces a SPICE error if the |argument| exceeds
!     1.D0 by more than TOL. If ARG exceeds 1.D0, the argument is
!     evaluated as if it equaled 1.D0, if ARG is less than -1.,
!     the argument is evaluated as if it equaled -1.D0.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INTERVALS
!     INVERSE TRIGONOMETRIC FUNCTION
!     NUMBERS
!     UTILITY
!
! $ Declarations

      implicit none

      real(wp)           arg
      real(wp)           tol

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARG        I   Argument to be evaluated.
!     TOL        I   Tolerance.
!
!     The function returns the arc sine of ARG.
!
! $ Detailed_Input
!
!     ARG      is the arc sine argument that is to be evaluated such
!              that if it is less than -1.D0 by more than TOL or greater
!              than 1.D0 by more than TOL, an error results.
!
!     TOL      is a tolerance such that |ARG| is considered to be equal
!              to 1.D0 if |ARG| <= 1.D0 + TOL. TOL must be non-negative.
!
! $ Detailed_Output
!
!     The function returns the arc sine of ARG. If
!
!        |ARG| >= 1.D0,
!
!     it returns DASIN (1.D0) or DASIN (-1.D0) as appropriate. Values
!     range from -PI/2 to PI/2.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If TOL is less than zero, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
!     2)  If |ARG| > 1.D0 + TOL, the error SPICE(INPUTOUTOFBOUNDS) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines whether |ARG| > 1.D0 + TOL. If it is, an
!     error will be flagged. In addition, the values of ARG are
!     constrained to [-1.D0, 1.D0].
!
! $ Examples
!
!     The following illustrate the operation of DASINE.
!
!           DASINE (  -1.D0,        1.D-7 )  =  -PI/2
!           DASINE (  -1.00001D0,   1.D-3 )  =  -PI/2
!           DASINE (  -1.00001D0,   1.D-7 )  =  -PI/2 (error flagged)
!           DASINE (   0.D0,        1.D-7 )  =   0.D0
!           DASINE (   1.00001D0,   1.D-3 )  =   PI/2
!           DASINE (   1.00001D0,   1.D-7 )  =   PI/2 (error flagged)
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
!     L.S. Elson         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 28-FEB-2006 (LSE)
!
!-&


! $ Index_Entries
!
!     check a d.p. argument for ASIN before evaluation
!
!-&


!
!     Bracket ARG.
!

      dasine      =  asin ( max ( - 1.0_wp, min ( 1.0_wp, arg ) ) )

!
!     Check that tolerance is non negative.
!

      if ( tol < 0.0_wp ) then

         call setmsg ( 'TOL was #; must be non-negative.' )
         call errdp  ( '#',  tol                          )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'           )
         return

      end if

!
!     Check to see if |ARG| is within TOL of 1.D0. Signal error if
!     appropriate.
!

      if  ( ( abs ( arg ) - tol ) > 1.0_wp) then

         call setmsg ( 'The |argument| specified was greater '// &
                       'than 1.D0 by more than #. The value '// &
                       'of the argument is #. '           )
         call errdp  ( '#',  tol                          )
         call errdp  ( '#',  arg                          )
         call sigerr ( 'SPICE(INPUTOUTOFBOUNDS)'          )
         return

      end if

      return
      end
