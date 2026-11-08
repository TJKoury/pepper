!$Procedure JYEAR ( Seconds per julian year )

      DOUBLE PRECISION FUNCTION JYEAR ()

! $ Abstract
!
!     Return the number of seconds in a julian year.
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
!     The function returns the number of seconds per julian year.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of seconds per julian year.
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
!     The julian year is often used as a fundamental unit of time when
!     dealing with ephemeris data. For this reason its value in terms of
!     ephemeris seconds is recorded in this function.
!
! $ Examples
!
!     Suppose you wish to compute the number of julian centuries
!     that have elapsed since the ephemeris epoch J1950 (beginning
!     of the julian year 1950) at a particular ET epoch. The
!     following line of code will do the trick.
!
!
!        CENTRY = ( ET - UNITIM ( J1950(), 'JED', 'ET' ) )
!       .       / ( 100.0D0 * JYEAR() )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
!          Astronomical Almanac," Page 8, University Science Books,
!          1992.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 08-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT)
!
!-&


! $ Index_Entries
!
!     Number of seconds per julian year
!
!-&


      JYEAR = 31557600.0D0

      RETURN
      END
