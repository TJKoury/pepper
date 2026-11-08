!> Unit tests of the stateless core modules (pepper_constants, pepper_arrays,
!> pepper_math, pepper_strings), called through the module interfaces.
program test_core

   use pepper_kinds, only: ip, wp
   use pepper_constants, only: pi, twopi, halfpi, dpr, rpd, spd, intmax, intmin
   use pepper_arrays, only: orderd, reordd, shelli, bsrchi
   use pepper_math, only: vcrss, vdot, vnorm, vsep, mxv, ident, even, brcktd
   use pepper_strings, only: lastnb, ucase, lcase, repmc

   implicit none

   integer :: failures = 0

   call test_constants()
   call test_arrays()
   call test_math()
   call test_strings()
   call test_parallel_loop()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_core: all checks passed'

contains

   subroutine check(ok, what)
      logical, intent(in) :: ok
      character(*), intent(in) :: what
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
      end if
   end subroutine check

   subroutine test_constants()
      call check(pi == acos(-1.0_wp), 'pi')
      call check(twopi == 2*pi .and. halfpi == pi/2, 'twopi, halfpi')
      call check(abs(dpr*rpd - 1.0_wp) < 1e-15_wp, 'dpr * rpd')
      call check(spd == 86400.0_wp, 'spd')
      call check(intmax == huge(1_ip) .and. intmin == -huge(1_ip) - 1, 'intmax, intmin')
   end subroutine test_constants

   subroutine test_arrays()
      real(wp) :: a(5) = [3.0_wp, 1.0_wp, 4.0_wp, 1.5_wp, 2.0_wp]
      integer(ip) :: iorder(5), k(6) = [9, 2, 7, 4, 4, 1]
      call orderd(a, 5, iorder)
      call check(all(iorder == [2, 4, 5, 1, 3]), 'orderd')
      call reordd(iorder, 5, a)
      call check(all(a == [1.0_wp, 1.5_wp, 2.0_wp, 3.0_wp, 4.0_wp]), 'reordd')
      call shelli(6, k)
      call check(all(k == [1, 2, 4, 4, 7, 9]), 'shelli')
      call check(bsrchi(7, 6, k) == 5 .and. bsrchi(5, 6, k) == 0, 'bsrchi')
   end subroutine test_arrays

   subroutine test_math()
      real(wp) :: x(3) = [1, 0, 0], y(3) = [0, 1, 0], z(3), m(3, 3), v(3)
      call vcrss(x, y, z)
      call check(all(z == [0.0_wp, 0.0_wp, 1.0_wp]), 'vcrss')
      call check(vdot(x, y) == 0 .and. vnorm([3.0_wp, 4.0_wp, 0.0_wp]) == 5, 'vdot, vnorm')
      call check(abs(vsep(x, y) - halfpi) < 1e-15_wp, 'vsep')
      call ident(m)
      call mxv(m, [1.0_wp, 2.0_wp, 3.0_wp], v)
      call check(all(v == [1.0_wp, 2.0_wp, 3.0_wp]), 'ident, mxv')
      ! Elemental procedures work on arrays.
      call check(all(even([1_ip, 2_ip, 3_ip, 4_ip]) .eqv. [.false., .true., .false., .true.]), &
                 'even (elemental)')
      call check(all(brcktd([-2.0_wp, 0.5_wp, 3.0_wp], 0.0_wp, 1.0_wp) == [0.0_wp, 0.5_wp, 1.0_wp]), &
                 'brcktd (elemental)')
   end subroutine test_math

   subroutine test_strings()
      character(8) :: words(3) = [character(8) :: 'alpha', 'Beta', 'gamma ok']
      character(8) :: up(3), low(3)
      character(40) :: msg
      call ucase(words, up)
      call check(all(up == [character(8) :: 'ALPHA', 'BETA', 'GAMMA OK']), 'ucase (elemental)')
      call lcase(up, low)
      call check(all(low == [character(8) :: 'alpha', 'beta', 'gamma ok']), 'lcase (elemental)')
      call check(all(lastnb(words) == [5, 4, 8]), 'lastnb (elemental)')
      call repmc('Body # not found', '#', 'MOON', msg)
      call check(msg == 'Body MOON not found', 'repmc')
   end subroutine test_strings

   !> Pure procedures can be used in DO CONCURRENT (a first check that they
   !> do not depend on shared state).
   subroutine test_parallel_loop()
      integer, parameter :: n = 1000
      real(wp) :: v(3, n), w(3, n), s(n)
      integer :: i
      do concurrent (i = 1:n)
         v(:, i) = [cos(i*rpd), sin(i*rpd), 0.0_wp]
         call vcrss(v(:, i), [0.0_wp, 0.0_wp, 1.0_wp], w(:, i))
         s(i) = vsep(v(:, i), w(:, i))
      end do
      call check(all(abs(s - halfpi) < 1e-12_wp), 'vcrss/vsep in do concurrent')
   end subroutine test_parallel_loop

end program test_core
