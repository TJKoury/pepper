!$Procedure J2000 ( Julian Date of 2000 JAN 1.5 )

      DOUBLE PRECISION FUNCTION J2000 ( )

! $ Abstract
!
!     Return the Julian Date of 2000 JAN 01 12:00:00 (2000 JAN 1.5).
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
!     The function returns the Julian Date of 2000 JAN 01 12:00:00
!     (2000 JAN 1.5).
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2451545.0, the Julian Date corresponding
!     to 2000 JAN 01 12:00:00 (2000 JAN 1.5).
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
!     The following code fragment illustrates the use of J2000.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J2000).
!        C
!              SPREF = ( JD - J2000() ) * SPD()
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
!     julian date of 2000 jan 1.5
!
!-&


      J2000 = 2451545.0D0

      RETURN
      END
