!> The geometry finder through the module API.
!>
!> - GFUDS with callbacks written for the module API: they take the context
!>   as their first argument (the user function, and the "is decreasing"
!>   routine, which calls UDDC through the context).
!> - GFDIST from two contexts on an SPK written and loaded through one of
!>   them: both find the same window, the one expected from the geometry.
program test_gf

   use pepper_kinds, only: ip, wp
   use pepper_constants, only: twopi
   use pepper_instance, only: pepper_spice, pepper_context, gf_scalar

   implicit none

   character(*), parameter :: spk = 'test_gf.bsp'
   integer(ip), parameter :: lbcell = -5, mw = 200, nw = 15
   type(pepper_spice), target :: spice
   type(pepper_context), pointer :: a  !! the instance's context (it loads the kernels)
   type(pepper_context) :: b
   integer :: failures = 0
   real(wp) :: cnfine(lbcell:2), result(lbcell:mw), ra(lbcell:mw), rb(lbcell:mw)
   real(wp) :: work(lbcell:mw, nw), left, right
   integer(ip) :: k

   a => spice%context()
   b = spice%new_context()

   ! GFUDS: f(t) = sin(2 pi t / 1000) > 0.5 on [0, 3000] holds on
   ! [1000 k + 1000/12, 1000 k + 5000/12], k = 0, 1, 2.
   call a%ssized(2_ip, cnfine)
   call a%wninsd(0.0_wp, 3000.0_wp, cnfine)
   call a%ssized(mw, result)
   call a%gfuds(sine, decreasing, '>', 0.5_wp, 0.0_wp, 50.0_wp, cnfine, mw, nw, work, result)
   call check(.not. a%failed(), 'gfuds')
   call check(a%wncard(result) == 3, 'gfuds: three intervals')
   do k = 1, min(3_ip, a%wncard(result))
      call a%wnfetd(result, k, left, right)
      call check(abs(left - (1000.0_wp*(k - 1) + 1000.0_wp/12)) < 1.0e-5_wp .and. &
                 abs(right - (1000.0_wp*(k - 1) + 5000.0_wp/12)) < 1.0e-5_wp, 'gfuds: interval')
   end do

   ! GFDIST: body 1000 moves along X (x = -1e6 + 1000 t km, y = 3e5 km) from
   ! t = 0 to 2000 s; its distance from the barycenter is below 5e5 km where
   ! |x| < 4e5 km, for t in [600, 1400].
   call delete_file()
   call write_spk()
   call spice%furnsh(spk)
   call a%ssized(2_ip, cnfine)
   call a%wninsd(10.0_wp, 1990.0_wp, cnfine)
   call a%ssized(mw, ra)
   call b%ssized(mw, rb)
   call a%gfdist('1000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, 100.0_wp, cnfine, mw, nw, work, ra)
   call b%gfdist('1000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, 100.0_wp, cnfine, mw, nw, work, rb)
   call check(.not. a%failed() .and. .not. b%failed(), 'gfdist in a and b')
   call check(b%wncard(rb) == 1, 'gfdist: one interval')
   if (b%wncard(rb) == 1) then
      call b%wnfetd(rb, 1_ip, left, right)
      call check(abs(left - 600.0_wp) < 1.0e-5_wp .and. abs(right - 1400.0_wp) < 1.0e-5_wp, &
                 'gfdist: the interval [600, 1400]')
   end if
   call check(a%wncard(ra) == b%wncard(rb), 'gfdist: same windows in a and b')
   if (a%wncard(ra) == b%wncard(rb)) call check(all(ra(1:2*a%wncard(ra)) == rb(1:2*b%wncard(rb))), &
                                                'gfdist: same windows in a and b')
   call spice%kclear()
   call b%close_unloaded_files()  ! (Windows cannot delete an open file)
   call delete_file()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_gf: all checks passed'

contains

   !> The user function of the GFUDS search (a GF_SCALAR: the context first).
   subroutine sine(ctx, et, value)
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! the time
      real(wp), intent(out) :: value  !! sin(2 pi et / 1000)
      value = sin(twopi*et/1000.0_wp)
   end subroutine sine

   !> Whether the user function is decreasing (a GF_DECR_X), through UDDC.
   subroutine decreasing(ctx, udfunc, et, xbool)
      class(pepper_context), intent(inout) :: ctx  !! the context
      procedure(gf_scalar) :: udfunc  !! the function
      real(wp), intent(in) :: et  !! the time
      logical, intent(out) :: xbool  !! the function is decreasing at ET
      call ctx%uddc(udfunc, et, 1.0_wp, xbool)
   end subroutine decreasing

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: msg
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
         if (a%failed()) then
            call a%getmsg('LONG', msg)
            print '(2a)', '   a: ', trim(msg)
         end if
         if (b%failed()) then
            call b%getmsg('LONG', msg)
            print '(2a)', '   b: ', trim(msg)
         end if
      end if
   end subroutine check

   !> Remove the test file if it exists.
   subroutine delete_file()
      logical :: exists
      integer :: unit
      logical :: still_open  !! the file is connected to a unit (Windows can't delete it)
      inquire (file=spk, exist=exists)
      if (exists) then
         inquire (file=spk, opened=still_open)
         if (still_open) error stop 'a file to delete is still open (Windows cannot delete it): '//spk
         open (newunit=unit, file=spk, status='old')
         close (unit, status='delete')
      end if
   end subroutine delete_file

   !> Write an SPK with one type 9 segment (degree 1) for body 1000 relative
   !> to the barycenter in J2000: states every 100 s from 0 to 2000 s on the
   !> straight line x = -1e6 + 1000 t, y = 3e5 km.
   subroutine write_spk()
      integer, parameter :: n = 21
      integer(ip) :: h
      integer :: i
      real(wp) :: sum(5), epochs(n), states(6, n)
      epochs = [(100.0_wp*(i - 1), i = 1, n)]
      do i = 1, n
         states(:, i) = [-1.0e6_wp + 1000.0_wp*epochs(i), 3.0e5_wp, 0.0_wp, 1000.0_wp, 0.0_wp, 0.0_wp]
      end do
      call a%dafonw(spk, 'SPK', 2_ip, 6_ip, 'PEPPER GF test SPK', 0_ip, h)
      call a%dafps(2_ip, 6_ip, [epochs(1), epochs(n)], [1000_ip, 0_ip, 1_ip, 9_ip, 0_ip, 0_ip], sum)
      call a%dafbna(h, sum, 'LINE')
      call a%dafada(reshape(states, [6*n]), int(6*n, ip))
      call a%dafada(epochs, int(n, ip))
      call a%dafada([1.0_wp, real(n, wp)], 2_ip)
      call a%dafena()
      call a%dafcls(h)
      call check(.not. a%failed(), 'write ' // spk)
   end subroutine write_spk

end program test_gf
