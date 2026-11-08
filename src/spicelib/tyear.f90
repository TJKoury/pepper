!$Procedure TYEAR ( Seconds per tropical year )

      real(wp) function tyear ()
      use pepper_kinds, only: wp

! $ Abstract
!
!     Return the number of seconds in a tropical year.
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
!     The function returns the number of seconds per tropical year.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of seconds per tropical
!     year. This value is taken from the 1992 Explanatory Supplement
!     to the Astronomical Almanac (see [1]).
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
!     The tropical year is often used as a fundamental unit
!     of time when dealing with older ephemeris data. For this
!     reason its value in terms of ephemeris seconds is
!     recorded in this function.
!
! $ Examples
!
!     Suppose you wish to compute the number of tropical centuries
!     that have elapsed since the ephemeris epoch B1950 (beginning
!     of the Besselian year 1950) at a particular ET epoch. The
!     following line of code will do the trick.
!
!
!        CENTRY = ( ET - UNITIM ( B1950(), 'JED', 'ET' ) )
!       .       / ( 100.0D0 * TYEAR() )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
!          Astronomical Almanac," p 80, University Science Books, 1992.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 13-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT)
!
!-&


! $ Index_Entries
!
!     Number of seconds per tropical year
!
!-&


      tyear = 31556925.9747_wp
      return
      end
