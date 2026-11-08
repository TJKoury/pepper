!$Procedure CLIGHT ( C, Speed of light in a vacuum )

      real(wp) function clight ( )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Return the speed of light in a vacuum (IAU official
!     value, in km/sec).
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
!     The function returns the speed of light in vacuum (km/sec).
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the IAU official value for the speed of light
!     in vacuum: 299792.458 km/sec.
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
!     Find the light time corresponding to the length of a given
!     3-dimensional position vector. Length units are km.
!
!     To use CLIGHT, declare it as having double precision type:
!
!        DOUBLE PRECISION      CLIGHT
!
!     Let POS be a 3-vector of interest; let TAU be the light time.
!     VNORM is the SPICELIB function that returns the norm of a
!     3-vector.
!
!        DOUBLE PRECISION      VNORM
!        DOUBLE PRECISION      TAU
!        DOUBLE PRECISION      POS (3 )
!
!     Find the light time:
!
!        TAU = VNORM ( POS ) / CLIGHT ()
!
!     Note that the SPK readers
!
!        SPKEZR
!        SPKEZ
!        SPKPOS
!        SPKEZP
!
!     return the one-way light time between target and observer
!     as an output.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 25-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 08-JAN-2008 (NJB)
!
!        $Examples section was updated to remove references to SPKAPP
!        and BODMAT.
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
!     c speed of light in a vacuum
!
!-&


!
!     Just like it says.
!
      clight = 299792.458_wp

      return
      end
