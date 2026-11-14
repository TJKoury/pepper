!> Benchmark: SPK, frame and time queries from 1, 2, 4 and 8 threads, each
!> with its own context of one instance (the setting of test_parallel).
!>
!>     pixi run bench-parallel
!>
!> Prints the time per epoch (all queries) and the speedup over one thread.
!> The library has no OpenMP; only this program uses it.
program bench_parallel

   use pepper_module, only: ip, wp, twopi, pepper_instance, pepper_context
   !$ use omp_lib, only: omp_get_wtime

   implicit none

   character(*), parameter :: lsk = 'bench_parallel.tls'
   character(*), parameter :: spk = 'bench_parallel.bsp'
   character(*), parameter :: fk = 'bench_parallel.tf'
   integer, parameter :: nepoch = 200000
   integer, parameter :: threads(4) = [1, 2, 4, 8]
   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: setup  !! the instance's context (it loads the kernels)
   real(wp) :: et(nepoch), sums(8), t0, t1, base
   integer :: i, k

   setup => spice%context()
   call delete_files()
   call write_kernels()
   call spice%furnsh(lsk)
   call spice%furnsh(spk)
   call spice%furnsh(fk)
   if (setup%failed()) error stop 'loading the kernels failed'
   et = [(1.0e5_wp + (1.8e6_wp*(i - 1))/(nepoch - 1), i = 1, nepoch)]

   print '(a, i0, a)', 'bench_parallel: ', nepoch, ' epochs (spkezr CN+S, spkpos LT, 2 pxform, et2utc, str2et)'
   base = 0.0_wp
   do k = 1, size(threads)
      t0 = wtime()
      sums = 0.0_wp
      ! each thread's work in a procedure, whose context is private to the
      ! thread (a BLOCK's variables in the main program are shared by the
      ! threads with ifx, as the main program's own are)
      !$omp parallel num_threads(threads(k)) reduction(+:sums)
      call thread_queries(sums)
      !$omp end parallel
      t1 = wtime()
      if (k == 1) base = t1 - t0
      print '(i3, a, f9.3, a, f6.2)', threads(k), ' thread(s): ', (t1 - t0)*1.0e6_wp/nepoch, &
         ' us/epoch, speedup ', base/(t1 - t0)
   end do

   call delete_files()

contains

   !> Wall clock time (OpenMP's when available).
   real(wp) function wtime()
      integer(8) :: c, r
      call system_clock(c, r)
      wtime = real(c, wp)/real(r, wp)
      !$ wtime = omp_get_wtime()
   end function wtime

   !> One thread's share of the epochs, through a context of its own.
   subroutine thread_queries(sums)
      real(wp), intent(inout) :: sums(8)  !! checksums of the results (thread's share)
      type(pepper_context) :: ctx
      real(wp) :: res(30)
      character(40) :: utc
      integer :: j
      ctx = spice%new_context()
      !$omp do schedule(static)
      do j = 1, nepoch
         call queries(ctx, et(j), res, utc)
         sums(1) = sums(1) + res(1)
      end do
      !$omp end do
   end subroutine thread_queries

   !> The queries for one epoch through a context.
   subroutine queries(ctx, t, res, utc)
      type(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: t  !! the epoch
      real(wp), intent(out) :: res(30)  !! the results
      character(*), intent(out) :: utc  !! the epoch in UTC
      real(wp) :: lt, rot(3, 3)
      call ctx%spkezr('PEPPER_B', t, 'J2000', 'CN+S', 'PEPPER_A', res(1:6), res(7))
      call ctx%spkpos('PEPPER_A', t, 'PEPPER_TK', 'LT', 'PEPPER_B', res(8:10), lt)
      call ctx%pxform('J2000', 'ECLIPJ2000', t, rot)
      call ctx%pxform('PEPPER_TK', 'J2000', t, rot)
      call ctx%et2utc(t, 'ISOC', 3_ip, utc)
      call ctx%str2et(utc, res(30))
   end subroutine queries

   !> Remove the test files that exist.
   subroutine delete_files()
      character(32) :: files(3)
      logical :: exists
      integer :: unit, k
      files = [character(32) :: lsk, spk, fk]
      do k = 1, size(files)
         inquire (file=trim(files(k)), exist=exists)
         if (exists) then
            open (newunit=unit, file=trim(files(k)), status='old')
            close (unit, status='delete')
         end if
      end do
   end subroutine delete_files

   !> Write the kernels: the leapseconds, the frame kernel (a TK frame and
   !> the body names) and the SPK.
   subroutine write_kernels()
      integer :: unit
      open (newunit=unit, file=lsk, status='new', action='write')
      write (unit, '(a)') 'KPL/LSK'
      write (unit, '(a)') '\begindata'
      write (unit, '(a)') 'DELTET/DELTA_T_A = 32.184'
      write (unit, '(a)') 'DELTET/K = 1.657D-3'
      write (unit, '(a)') 'DELTET/EB = 1.671D-2'
      write (unit, '(a)') 'DELTET/M = ( 6.239996D0 1.99096871D-7 )'
      write (unit, '(a)') 'DELTET/DELTA_AT = ( 32, @1999-JAN-1 )'
      write (unit, '(a)') '\begintext'
      close (unit)
      open (newunit=unit, file=fk, status='new', action='write')
      write (unit, '(a)') 'KPL/FK'
      write (unit, '(a)') '\begindata'
      write (unit, '(a)') 'NAIF_BODY_NAME += ( ''PEPPER_A'', ''PEPPER_B'' )'
      write (unit, '(a)') 'NAIF_BODY_CODE += ( 1000, 1001 )'
      write (unit, '(a)') 'FRAME_PEPPER_TK = 1500001'
      write (unit, '(a)') 'FRAME_1500001_NAME = ''PEPPER_TK'''
      write (unit, '(a)') 'FRAME_1500001_CLASS = 4'
      write (unit, '(a)') 'FRAME_1500001_CLASS_ID = 1500001'
      write (unit, '(a)') 'FRAME_1500001_CENTER = 1000'
      write (unit, '(a)') 'TKFRAME_1500001_RELATIVE = ''ECLIPJ2000'''
      write (unit, '(a)') 'TKFRAME_1500001_SPEC = ''ANGLES'''
      write (unit, '(a)') 'TKFRAME_1500001_UNITS = ''DEGREES'''
      write (unit, '(a)') 'TKFRAME_1500001_AXES = ( 3, 1, 3 )'
      write (unit, '(a)') 'TKFRAME_1500001_ANGLES = ( 10.0, 20.0, 30.0 )'
      write (unit, '(a)') '\begintext'
      close (unit)
      call write_spk()
   end subroutine write_kernels

   !> Write the SPK: body 1000 on a circle around the solar system
   !> barycenter (1.5e8 km, one revolution in a year) and body 1001 on a
   !> circle around body 1000 (4e5 km, 27 days), both in J2000, as type 9
   !> segments (201 states, every 10000 s from 0 to 2e6 s, degree 7; the
   !> epoch directory has two entries).
   subroutine write_spk()
      integer(ip) :: h
      call setup%dafonw(spk, 'SPK', 2_ip, 6_ip, 'PEPPER parallel test SPK', 0_ip, h)
      call segment(h, 1000_ip, 0_ip, 1.5e8_wp, 365.25_wp*86400.0_wp)
      call segment(h, 1001_ip, 1000_ip, 4.0e5_wp, 27.0_wp*86400.0_wp)
      call setup%dafcls(h)
      if (setup%failed()) error stop 'writing the SPK failed'
   end subroutine write_spk

   !> One type 9 segment of the SPK: the body on a circle of radius r (km)
   !> and period p (s), slightly inclined to the X-Y plane of J2000,
   !> around the center.
   subroutine segment(h, body, center, r, p)
      integer(ip), intent(in) :: h  !! handle of the SPK being written
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! the center of motion
      real(wp), intent(in) :: r  !! the radius (km)
      real(wp), intent(in) :: p  !! the period (s)
      integer, parameter :: n = 201, degree = 7
      real(wp) :: epochs(n), states(6, n), w, sum(5)
      integer :: k
      w = twopi/p
      epochs = [(1.0e4_wp*(k - 1), k = 1, n)]
      do k = 1, n
         states(:, k) = [r*cos(w*epochs(k)), r*sin(w*epochs(k)), 0.1_wp*r*sin(w*epochs(k)), &
                         -r*w*sin(w*epochs(k)), r*w*cos(w*epochs(k)), 0.1_wp*r*w*cos(w*epochs(k))]
      end do
      call setup%dafps(2_ip, 6_ip, [epochs(1), epochs(n)], [body, center, 1_ip, 9_ip, 0_ip, 0_ip], sum)
      call setup%dafbna(h, sum, 'CIRCLE')
      ! type 9: the states, the epochs, the epoch directory (every 100th
      ! epoch), the degree and the number of states
      call setup%dafada(reshape(states, [6*n]), int(6*n, ip))
      call setup%dafada(epochs, int(n, ip))
      call setup%dafada([epochs(100), epochs(200)], 2_ip)
      call setup%dafada([real(degree, wp), real(n, wp)], 2_ip)
      call setup%dafena()
   end subroutine segment

end program bench_parallel
