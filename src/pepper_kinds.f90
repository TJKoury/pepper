!> Kind parameters used throughout PEPPER.
!>
!> `ip` is the integer kind and `wp` the real ("working precision") kind.
!> They are the same kinds as SPICELIB's default `integer` and
!> `double precision`.
module pepper_kinds

   use, intrinsic :: iso_fortran_env, only: int32, real64

   implicit none

   private

   integer, parameter, public :: ip = int32   !! integer kind
   integer, parameter, public :: wp = real64  !! real kind

end module pepper_kinds
