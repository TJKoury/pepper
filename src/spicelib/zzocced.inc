! $ Abstract
!
!     Declare ZZOCCED return code parameters, comparison strings
!     and other parameters.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     ELLIPSOID
!     GEOMETRY
!     GF
!     OCCULTATION
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB)
!
!-&


!     The function returns an integer code indicating the geometric
!     relationship of the three bodies.
!
!     Codes and meanings are:
!
!        -3                    Total occultation of first target by
!                              second.
!
      INTEGER               TOTAL1
      PARAMETER           ( TOTAL1 = -3 )

!
!        -2                    Annular occultation of first target by
!                              second.  The second target does not
!                              block the limb of the first.
!
      INTEGER               ANNLR1
      PARAMETER           ( ANNLR1 = -2 )

!
!        -1                    Partial occultation of first target by
!                              second target.
!
      INTEGER               PARTL1
      PARAMETER           ( PARTL1 = -1 )

!
!         0                    No occultation or transit:  both objects
!                              are completely visible to the observer.
!
      INTEGER               NOOCC
      PARAMETER           ( NOOCC  =  0 )

!
!         1                    Partial occultation of second target by
!                              first target.
!
      INTEGER               PARTL2
      PARAMETER           ( PARTL2 =  1 )

!
!         2                    Annular occultation of second target by
!                              first.
!
      INTEGER               ANNLR2
      PARAMETER           ( ANNLR2 =  2 )

!
!         3                    Total occultation of second target by
!                              first.
!
      INTEGER               TOTAL2
      PARAMETER           ( TOTAL2 =  3 )

!
!     End include file zzocced.inc
!

