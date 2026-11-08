!$Procedure RPD ( Radians per degree )

      double precision function rpd ( )

! $ Abstract
!
!     Return the number of radians per degree.
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
!     The function returns the number of radians per degree.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of radians per degree: pi/180.
!     The value of pi is determined by the ACOS function. That is,
!
!           RPD = ACOS ( -1.D0 ) / 180.D0
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
!     The code fragment below illustrates the use of RPD.
!
!        C
!        C     Convert all input angles to radians.
!        C
!              CLOCK = CLOCK * RPD()
!              CONE  = CONE  * RPD()
!              TWIST = TWIST * RPD()
!
!     or equivalently,
!
!        C
!        C     Convert all input angles to radians.
!        C
!              CALL VPACK  ( CLOCK, CONE, CCTWIST, ALBTGAM )
!              CALL VSCL   ( RPD(), ALBTGAM, ALBTGAM )
!              CALL VUPACK ( ALBTGAM, CLOCK, CONE, CCTWIST )
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
!     radians per degree
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
         value = acos ( -1.d0 ) / 180.d0
      end if

      rpd = value

      return
      end
