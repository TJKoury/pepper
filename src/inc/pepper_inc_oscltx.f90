!> This file declares public parameters related to the SPICELIB routine OSCLTX.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 25-JAN-2017 (NJB)
module pepper_inc_oscltx

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: oscxsz = 20  !! Size of the OSCLTX output argument array ELTS.
                                          !!
                                          !! The output array ELTS is intended to contain unused space to hold
                                          !! additional elements that may be added in a later version of this
                                          !! routine. In order to maintain forward compatibility, user
                                          !! applications should declare ELTS as follows:
                                          !!
                                          !! DOUBLE PRECISION   ELTS( OSCXSZ )

end module pepper_inc_oscltx
