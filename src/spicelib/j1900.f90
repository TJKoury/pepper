!$Procedure J1900 ( Julian Date of 1900.0 JAN 0.5 )

      double precision function j1900 ( )

! $ Abstract
!
!     Return the Julian Date of 1899 DEC 31 12:00:00 (1900 JAN 0.5).
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
!     The function returns the Julian Date of 1899 DEC 31 12:00:00
!     (1900 JAN 0.5).
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2415020.0, the Julian Date corresponding
!     to 1899 DEC 31 12:00:00 (1900 JAN 0.5).
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
!     The following code fragment illustrates the use of J1900.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J1900).
!        C
!              SPREF = ( JD - J1900() ) * SPD()
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
!     julian date of 1900.0 jan 0.5
!
!-&


      j1900 = 2415020.0d0

      return
      end
