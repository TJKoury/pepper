!> A SPICELIB-style program on PEPPER: SPICELIB's routines, names and
!> arguments, from the module pepper_legacy (no declarations of SPICELIB's
!> functions of its own). Run with `fpm run --example legacy_example`.
program legacy_example

   use pepper_legacy
   implicit none

   character(80) :: action
   character(25) :: short
   double precision :: et
   double precision :: rot(3, 3)

   ! errors: return to the caller (SPICELIB's default stops the program)
   action = 'RETURN'
   call erract('SET', action)

   ! a leapseconds kernel from lines (a program would FURNSH a file)
   call lmpool([character(80) :: &
      'DELTET/DELTA_T_A = 32.184', 'DELTET/K = 1.657D-3', 'DELTET/EB = 1.671D-2', &
      'DELTET/M = ( 6.239996D0 1.99096871D-7 )', &
      'DELTET/DELTA_AT = ( 10, @1972-JAN-1 37, @2017-JAN-1 )'], 5)
   call str2et('2026 NOV 8 12:00:00', et)
   print '(a, f0.3)', 'ET: ', et

   call pxform('J2000', 'ECLIPJ2000', et, rot)
   print '(a, f0.6)', 'obliquity (deg): ', acos(rot(3, 3)) * dpr()

   ! an error: a missing kernel
   call furnsh('no-such-kernel.bsp')
   if (failed()) then
      call getmsg('SHORT', short)
      print '(2a)', 'error: ', trim(short)
      call reset()
   end if

end program legacy_example
