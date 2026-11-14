!> The SPK API through the module API with two contexts: an SPK file (one
!> type 9 segment, written through the DAF routines) loaded through one
!> context is used by the other with SPKEZR and SPKPOS, with and without
!> aberration corrections and in another frame; the results are the same in
!> both contexts, and unloading the file is seen by both.
program test_spk

   use pepper_kinds, only: ip, wp
   use pepper_constants, only: clight
   use pepper_instance_module, only: pepper_instance, pepper_context

   implicit none

   character(*), parameter :: spk = 'test_spk.bsp'
   ! the object: body 1000, moving on a straight line relative to the
   ! solar system barycenter, in J2000 (km and km/s)
   integer(ip), parameter :: body = 1000
   real(wp), parameter :: p0(3) = [1.0e6_wp, 2.0e6_wp, 3.0e6_wp]
   real(wp), parameter :: v(3) = [10.0_wp, -20.0_wp, 5.0_wp]
   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: a  !! the instance's context (it loads the kernels)
   type(pepper_context) :: b
   integer :: failures = 0
   real(wp) :: sa(6), sb(6), pa(3), pb(3), lta, ltb, rot(3, 3), x(3)
   character(80) :: msg

   a => spice%context()
   b = spice%new_context()

   call delete_file()
   call write_spk()
   call spice%furnsh(spk)
   call check(.not. a%failed(), 'a loads the SPK')

   ! geometric state: exact for linear motion
   call b%spkezr('1000', 150.0_wp, 'J2000', 'NONE', 'SOLAR SYSTEM BARYCENTER', sb, ltb)
   call check(.not. b%failed() .and. all(abs(sb(1:3) - (p0 + v*150.0_wp)) < 1.0e-6_wp) .and. &
              all(abs(sb(4:6) - v) < 1.0e-9_wp), 'b: spkezr geometric state')
   call check(abs(ltb - norm2(sb(1:3))/clight) < 1.0e-12_wp, 'b: one-way light time')
   call a%spkezr('1000', 150.0_wp, 'J2000', 'NONE', 'SOLAR SYSTEM BARYCENTER', sa, lta)
   call check(all(sa == sb) .and. lta == ltb, 'a and b give the same state')

   ! converged light-time corrected position: the position at et - lt, with
   ! lt the light time from that position ('LT' makes a single iteration)
   call b%spkpos('1000', 150.0_wp, 'J2000', 'CN', '0', pb, ltb)
   call check(.not. b%failed() .and. all(abs(pb - (p0 + v*(150.0_wp - ltb))) < 1.0e-6_wp) .and. &
              abs(ltb - norm2(pb)/clight) < 1.0e-12_wp, 'b: spkpos with converged light time')
   call a%spkpos('1000', 150.0_wp, 'J2000', 'CN', '0', pa, lta)
   call check(all(pa == pb) .and. lta == ltb, 'a and b agree with light time')

   ! another frame: the same vector rotated
   call b%spkpos('1000', 150.0_wp, 'ECLIPJ2000', 'NONE', '0', pb, ltb)
   call b%pxform('J2000', 'ECLIPJ2000', 150.0_wp, rot)
   x = matmul(rot, p0 + v*150.0_wp)
   call check(.not. b%failed() .and. all(abs(pb - x) < 1.0e-6_wp), 'b: spkpos in ECLIPJ2000')

   ! outside the coverage: insufficient data
   call b%spkezr('1000', 1000.0_wp, 'J2000', 'NONE', '0', sb, ltb)
   call b%getmsg('SHORT', msg)
   call check(b%failed() .and. msg == 'SPICE(SPKINSUFFDATA)', 'b: no data outside the coverage')
   call b%reset()

   ! a unloads the file: b has no data
   call spice%unload(spk)
   call b%close_unloaded_files()  ! (Windows cannot delete an open file)
   call b%spkezr('1000', 150.0_wp, 'J2000', 'NONE', '0', sb, ltb)
   call check(b%failed(), 'b: no data after a unloads the SPK')
   call b%reset()

   call delete_file()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_spk: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: long
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
         if (a%failed()) then
            call a%getmsg('LONG', long)
            print '(2a)', '   a: ', trim(long)
         end if
         if (b%failed()) then
            call b%getmsg('LONG', long)
            print '(2a)', '   b: ', trim(long)
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

   !> Write an SPK with one type 9 segment for the body relative to the
   !> solar system barycenter in J2000: states at epochs 0, 100, 200, 300 s
   !> past J2000 on the straight line, interpolated with degree 1.
   subroutine write_spk()
      integer, parameter :: n = 4
      integer(ip) :: h, k
      real(wp) :: sum(5), epochs(n), states(6, n)
      epochs = [(100.0_wp*(k - 1), k = 1, n)]
      do k = 1, n
         states(1:3, k) = p0 + v*epochs(k)
         states(4:6, k) = v
      end do
      call a%dafonw(spk, 'SPK', 2_ip, 6_ip, 'PEPPER test SPK', 0_ip, h)
      call a%dafps(2_ip, 6_ip, [epochs(1), epochs(n)], [body, 0_ip, 1_ip, 9_ip, 0_ip, 0_ip], sum)
      call a%dafbna(h, sum, 'LINE')
      ! type 9: the states, the epochs, the epoch directory (empty for
      ! fewer than 101 epochs), the degree and the number of states
      call a%dafada(reshape(states, [6*n]), int(6*n, ip))
      call a%dafada(epochs, int(n, ip))
      call a%dafada([1.0_wp, real(n, wp)], 2_ip)
      call a%dafena()
      call a%dafcls(h)
      call check(.not. a%failed(), 'write ' // spk)
   end subroutine write_spk

end program test_spk
