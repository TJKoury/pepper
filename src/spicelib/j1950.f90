!$Procedure J1950 ( Julian Date of 1950.0 JAN 1.0 )

      real(wp) function j1950 ( )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Return the Julian Date of 1950 JAN 01 00:00:00 (1950 JAN 1.0).
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
!     The function returns the Julian Date of 1950 JAN 01 00:00:00
!     (1950 JAN 1.0).
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2433282.5, the Julian Date corresponding
!     to 1950 JAN 01 00:00:00 (1950 JAN 1.0).
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of J1950.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J1950).
!        C
!              SPREF = ( JD - J1950() ) * SPD()
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
!     julian date of 1950.0 jan 1.0
!
!-&


      j1950 = 2433282.5_wp

      return
      end
