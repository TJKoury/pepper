! $ Abstract
!
!     This file contains public, global parameter declarations
!     for the SPICELIB occultation routines.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ELLIPSOID
!     GEOMETRY
!     OCCULTATION
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     S.C. Krening      (JPL)
!     N.J. Bachman      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 24-JAN-2012 (SCK) (NJB)
!
!-&

!
!     The following integer codes indicate the geometric relationship
!     of the three bodies.
!
!     The meaning of the sign of each code is given below.
!
!                    Code sign          Meaning
!                    ---------          ------------------------------
!                       > 0             The second ellipsoid is
!                                       partially or fully occulted
!                                       by the first.
!
!                       < 0             The first ellipsoid is
!                                       partially of fully
!                                       occulted by the second.
!
!                       = 0             No occultation.
!
!     The meanings of the codes are given below. The variable names
!     indicate the type of occultation and which target is in the back.
!     For example, TOTAL1 represents a total occultation in which the
!     first target is in the back (or occulted by) the second target.
!
!                    Name      Code     Meaning
!                    ------    -----    ------------------------------
!                    TOTAL1     -3      Total occultation of first
!                                       target by second.
!
!                    ANNLR1     -2      Annular occultation of first
!                                       target by second.  The second
!                                       target does not block the limb
!                                       of the first.
!
!                    PARTL1     -1      Partial occultation of first
!                                       target by second target.
!
!                    NOOCC       0      No occultation or transit:  both
!                                       objects are completely visible
!                                       to the observer.
!
!                    PARTL2      1      Partial occultation of second
!                                       target by first target.
!
!                    ANNLR2      2      Annular occultation of second
!                                       target by first.
!
!                    TOTAL2      3      Total occultation of second
!                                       target by first.
!


      INTEGER               TOTAL1
      PARAMETER           ( TOTAL1 = -3 )

      INTEGER               ANNLR1
      PARAMETER           ( ANNLR1 = -2 )

      INTEGER               PARTL1
      PARAMETER           ( PARTL1 = -1 )

      INTEGER               NOOCC
      PARAMETER           ( NOOCC  =  0 )

      INTEGER               PARTL2
      PARAMETER           ( PARTL2 =  1 )

      INTEGER               ANNLR2
      PARAMETER           ( ANNLR2 =  2 )

      INTEGER               TOTAL2
      PARAMETER           ( TOTAL2 =  3 )

!
!     End include file occult.inc
!
