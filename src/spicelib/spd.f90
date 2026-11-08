!$Procedure SPD ( Seconds per day )

      double precision function spd ( )

! $ Abstract
!
!     Return the number of seconds in a day.
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
!     The function returns the number of seconds in a day.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of seconds in a day: 86400.
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
!     The following code fragment illustrates the use of SPD.
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
!-    SPICELIB Version 1.0.2, 07-JUL-2020 (JDR)
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
!     seconds per day
!
!-&


!
!     Just like it says.
!
      spd = 86400.d0

      return
      end
