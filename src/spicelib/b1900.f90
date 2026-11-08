!$Procedure B1900 ( Besselian Date 1900.0 )

      double precision function b1900 ( )

! $ Abstract
!
!     Return the Julian Date corresponding to Besselian Date 1900.0.
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
!     The function returns the Julian Date corresponding to Besselian
!     date 1900.0.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2415020.31352, the Julian Date corresponding
!     to Besselian Date 1900.0.
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
!     The following code fragment illustrates the use of B1900.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (B1900).
!        C
!              SPREF = ( JD - B1900() ) * SPD()
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
!          Astronomical Constants," Astron. Astrophys. 73, 282-284,
!          1979.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 13-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added entry [1]
!        to $Literature_References section.
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
!     besselian date 1900.0
!
!-&


      b1900 = 2415020.31352d0

      return
      end
