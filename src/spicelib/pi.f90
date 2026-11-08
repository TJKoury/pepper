!$Procedure PI ( Value of pi )

      double precision function pi ( )

! $ Abstract
!
!     Return the value of pi (the ratio of the circumference of
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
!     The function returns the value of pi.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the value of pi (the ratio of a circle's
!     circumference to its diameter), determined by the ACOS function.
!     That is,
!
!           PI = ACOS ( -1.D0 )
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
!     The code fragment below illustrates the use of PI.
!
!        C
!        C     Compute the polar radius,
!        C
!        C                 p
!        C          ----------------
!        C          1 + e cos(theta)
!        C
!        C     at evenly spaced values of the polar angle, theta.
!        C
!              DELTA = PI() / N
!
!              DO I = 0, N
!                 R(I) = P / (1.D0 + ECC * COS(I*DELTA))
!              END DO
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
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
!     value of pi
!
!-&


!
!     Local variables
!
      double precision      value
      save                  value

!
!     Initial values
!
      data                  value      / 0.d0 /


!
!     What is there to say?
!
      if ( value .eq. 0.d0 ) then
         value = acos ( -1.d0 )
      end if

      pi = value

      return
      end
