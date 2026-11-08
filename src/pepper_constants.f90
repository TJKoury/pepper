!> Constants. These replace SPICELIB's constant functions (``pi()``,
!> ``dpr()``, ``spd()``, ...): new code uses the named constants directly.
!> The functions remain, for existing callers, in pepper_legacy_constants.f90.
!> The values are the ones the SPICELIB functions return.
module pepper_constants

   use pepper_kinds, only: ip, wp

   implicit none

   private

   ! Mathematical constants (pi() computed acos(-1) on its first call).
   real(wp), parameter, public :: pi     = acos(-1.0_wp)            !! pi
   real(wp), parameter, public :: twopi  = acos(-1.0_wp) * 2.0_wp   !! 2 pi
   real(wp), parameter, public :: halfpi = acos(-1.0_wp) * 0.5_wp   !! pi / 2
   real(wp), parameter, public :: dpr    = 180.0_wp / acos(-1.0_wp) !! degrees per radian
   real(wp), parameter, public :: rpd    = acos(-1.0_wp) / 180.0_wp !! radians per degree

   ! Physical and time constants.
   real(wp), parameter, public :: clight = 299792.458_wp      !! speed of light in vacuum (km/s)
   real(wp), parameter, public :: spd    = 86400.0_wp         !! seconds per day
   real(wp), parameter, public :: jyear  = 31557600.0_wp      !! seconds per Julian year
   real(wp), parameter, public :: tyear  = 31556925.9747_wp   !! seconds per tropical year

   ! Julian dates of standard epochs.
   real(wp), parameter, public :: b1900 = 2415020.31352_wp    !! Besselian date 1900.0
   real(wp), parameter, public :: b1950 = 2433282.42345905_wp !! Besselian date 1950.0
   real(wp), parameter, public :: j1900 = 2415020.0_wp        !! Julian date 1899 DEC 31 12:00:00
   real(wp), parameter, public :: j1950 = 2433282.5_wp        !! Julian date 1950 JAN 01 00:00:00
   real(wp), parameter, public :: j2000 = 2451545.0_wp        !! Julian date 2000 JAN 01 12:00:00
   real(wp), parameter, public :: j2100 = 2488070.0_wp        !! Julian date 2100 JAN 01 12:00:00

   ! Limits (SPICELIB's dpmax/dpmin are slightly inside huge(1.0_wp)).
   real(wp), parameter, public :: dpmax =  1.79769313486231e308_wp     !! largest real
   real(wp), parameter, public :: dpmin = -1.79769313486231e308_wp     !! smallest (most negative) real
   integer(ip), parameter, public :: intmax = 2147483647_ip            !! largest integer
   integer(ip), parameter, public :: intmin = -2147483647_ip - 1_ip    !! smallest integer

end module pepper_constants
