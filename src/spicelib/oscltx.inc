!
!     Include file oscltx.inc
!

! $ Abstract
!
!     This file declares public parameters related to
!     the SPICELIB routine OSCLTX.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 25-JAN-2017 (NJB)
!
!-&


!
!     Size of the OSCLTX output argument array ELTS.
!
!     The output array ELTS is intended to contain unused space to hold
!     additional elements that may be added in a later version of this
!     routine. In order to maintain forward compatibility, user
!     applications should declare ELTS as follows:
!
!     DOUBLE PRECISION   ELTS( OSCXSZ )
!

      integer               oscxsz
      parameter           ( oscxsz = 20 )


!
!     End of include file oscltx.inc
!
