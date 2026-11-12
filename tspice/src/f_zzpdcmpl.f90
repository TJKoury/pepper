!$Procedure F_ZZPDCMPL ( ZZPDCMPL tests )

      SUBROUTINE F_ZZPDCMPL ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the private SPICELIB geometry routine ZZPDCMPL.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB
!     geometry routine ZZPDCMPL.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 19-JAN-2017 (NJB)
!
!        Original version 09-MAR-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      INTEGER               LT
      PARAMETER           ( LT = -1 )

      INTEGER               EQ
      PARAMETER           ( EQ =  0 )

      INTEGER               GT
      PARAMETER           ( GT =  1 )

!
!     Local Variables
!
!

!      CHARACTER*(LNSIZE)    LABEL
!      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      P     ( 3 )
      DOUBLE PRECISION      PLAT
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP

!      INTEGER               I
!      INTEGER               J
      INTEGER               REL
      INTEGER               XREL

!
!     Saved values
!
!     Initial values
!


!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZPDCMPL' )





!***********************************************************************
!
!     Special cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Point is at origin, LAT > 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  = 0.D0
      P(2)  = 0.D0
      P(3)  = 0.D0

!
!     The latitude of P is considered to be zero.
!
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Point is at origin, LAT < 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4

      P(1)  = 0.D0
      P(2)  = 0.D0
      P(3)  = 0.D0

!
!     The latitude of P is considered to be zero.
!
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Point is at origin, LAT = 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  = 0.D0
      P(2)  = 0.D0
      P(3)  = 0.D0

!
!     The latitude of P is considered to be zero.
!
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = EQ

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Point is on -Z axis, LAT > 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  =  0.D0
      P(2)  =  0.D0
      P(3)  = -1.D0

!
!     The latitude of P is -pi/2.
!
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Point is on -Z axis, -pi/2 < LAT < 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4

      P(1)  =  0.D0
      P(2)  =  0.D0
      P(3)  = -1.D0

!
!     The latitude of P is -pi/2.
!
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Point is on -Z axis, LAT = 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  =  0.D0
      P(2)  =  0.D0
      P(3)  = -1.D0

!
!     The latitude of P is -pi/2.
!
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




!***********************************************************************
!
!     LAT = 0 tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = 0 deg; P(3) > 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = 0 deg; P(3) = 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 0.D0

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = 0 deg; P(3) < 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  = -1.D0

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = RE/2; LAT = 0 deg; P(3) > 0' )


      RE    = 2.D0
      RP    = 1.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = RE/2; LAT = 0 deg; P(3) = 0' )


      RE    = 2.D0
      RP    = 1.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 0.D0

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = RE/2; LAT = 0 deg; P(3) < 0' )


      RE    = 2.D0
      RP    = 1.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  = -1.D0

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




!***********************************************************************
!
!     LAT = pi/2, -pi/2 tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = 90 deg; P(3) > 0, P(1)=P(2)=0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = HALFPI()

      P(1)  = 0.D0
      P(2)  = 0.D0
      P(3)  = 1.D0

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = 90 deg; P(3) > 0, P(1) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = HALFPI()

      P(1)  = 1.D0
      P(2)  = 0.D0
      P(3)  = 1.D0

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = 90 deg; P(3) > 0, P(2) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = HALFPI()

      P(1)  =  0.D0
      P(2)  =  1.D0
      P(3)  =  1.D0

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = -90 deg; P(3) < 0, P(1)=P(2)=0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -HALFPI()

      P(1)  =  0.D0
      P(2)  =  0.D0
      P(3)  = -1.D0

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = -90 deg; P(3) < 0, P(1) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -HALFPI()

      P(1)  =  1.D0
      P(2)  =  0.D0
      P(3)  = -1.D0

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'RP = 2*RE; LAT = -90 deg; P(3) < 0, P(2) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -HALFPI()

      P(1)  =   0.D0
      P(2)  =   1.D0
      P(3)  =  -1.D0

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )





!***********************************************************************
!
!     Basic tests for prolate spheroids
!
!***********************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 70 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 50 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = -50 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 60+1e-13 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 60-1e-13 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = -70 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = -50 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = 50 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  =  50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = -60-1e-13 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = -60+1e-13 deg' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!***********************************************************************
!
!     Basic tests for oblate spheroids
!
!***********************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 70 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 50 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = -50 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 60+1e-13 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 60-1e-13 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = -70 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = -50 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = 50 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  =  50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = -60-1e-13 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = -60+1e-13 deg' )


      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )








!
! --- Case: ------------------------------------------------------
!


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


