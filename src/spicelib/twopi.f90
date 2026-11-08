!$Procedure TWOPI ( Twice the value of pi )

      real(wp) function twopi ( )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Return twice the value of pi (the ratio of the circumference of
!     a circle to its diameter).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     The function returns twice the value of pi.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns twice the value of pi (the ratio of
!     a circle's circumference to its diameter), determined by
!     the ACOS function. That is,
!
!           TWOPI = ACOS ( -1.D0 ) * 2.D0
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
!     The first time the function is referenced, the value is computed
!     as shown above. The value is saved, and returned directly upon
!     subsequent reference.
!
! $ Examples
!
!     The code fragment below illustrates the use of TWOPI.
!
!        C
!        C      The longitude of the ascending node is the angle
!        C      between the x-axis and the node vector, n.
!        C                                              -
!        C
!               NODE = ACOS ( N(1) / VNORM(N) )
!
!               IF ( NODE .LT. 0.D0 ) THEN
!                  NODE = NODE + TWOPI()
!               END IF
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
!-    SPICELIB Version 1.0.3, 07-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 28-AUG-1997 (WLT)
!
!        Fixed the description in the detailed output section
!        of the header.
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
!     twice the value of pi
!
!-&


!
!     Local variables
!
      real(wp)              value
      save                  value

!
!     Initial values
!
      data                  value      / 0.0_wp /


!
!     What is there to say?
!
      if ( value .eq. 0.0_wp ) then
         value = acos ( -1.0_wp ) * 2.0_wp
      end if

      twopi = value

      return
      end
