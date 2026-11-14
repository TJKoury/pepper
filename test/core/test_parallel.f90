!> The phase 5 milestone: ephemeris, frame and time queries from several
!> threads at once, each with its own context of one instance.
!>
!> A setup context writes and loads small kernels (a leapseconds kernel, an
!> SPK with two bodies, the second relative to the first, a JSON frame kernel with
!> a TK frame, and body names). One context computes the reference results
!> serially; then OpenMP threads, each with a context of its own, compute the
!> same queries, interleaved in a different order, twice; then twice more
!> with a second instance that has the SPK in memory (FURNSH's in_memory:
!> the threads read one shared copy; a copy of the file, since a file can be
!> loaded by one instance at a time). Every result must be identical
!> (bitwise) to the serial one.
!>
!> The library has no OpenMP; only this test uses it (`pixi run
!> test-parallel` builds with -fopenmp). Built without OpenMP, the "parallel"
!> part runs on one thread and the test still checks the results.
program test_parallel

   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_constants, only: twopi
   use pepper_instance_module, only: pepper_instance, pepper_context, ephemeris_procedure, frame_procedure
   !$ use omp_lib, only: omp_get_thread_num, omp_get_max_threads

   implicit none

   character(*), parameter :: lsk = 'test_parallel.tls'
   character(*), parameter :: spk = 'test_parallel.bsp'
   character(*), parameter :: fk = 'test_parallel_fk.json'  !! a JSON kernel
   character(*), parameter :: spk_mem = 'test_parallel_mem.bsp'  !! the SPK's copy, loaded in memory
   integer, parameter :: nepoch = 2000  !! number of query epochs
   integer, parameter :: nres = 6 + 1 + 3 + 1 + 9 + 9 + 1 + 6 + 9 + 6  !! results per epoch
   integer, parameter :: nthreads = 4
   type(pepper_instance), target :: spice
   type(pepper_instance), target :: memspice  !! the same kernels, the SPK in memory
   type(pepper_context), pointer :: setup  !! the instance's context (it loads the kernels)
   type(pepper_context) :: serial
   ! allocatable: fixed-size arrays this large (2 MB) are on the stack with
   ! -frecursive, more than Windows' default stack of 1 to 2 MB
   real(wp), allocatable :: et(:)
   real(wp), allocatable :: ref(:, :), par(:, :)
   character(40), allocatable :: refutc(:), parutc(:)
   integer :: failures = 0, pass, i, nbad, errors
   type(ephemeris_procedure) :: eproc  !! a programmatic body (PEPPER_P, 9100)
   type(ephemeris_procedure) :: nproc  !! a programmatic body whose provider queries 9100 (9101)
   type(frame_procedure) :: fproc  !! a programmatic frame (PEPPER_SPIN)

   setup => spice%context()
   call delete_files()
   call write_kernels()
   call spice%furnsh(lsk)
   call spice%furnsh(spk)
   call spice%furnsh(fk)
   call check(.not. setup%failed(), 'setup loads the kernels')
   call copy_file(spk, spk_mem)
   call memspice%furnsh(lsk)
   call memspice%furnsh(spk_mem, in_memory=.true.)
   call memspice%furnsh(fk)
   call check(.not. memspice%failed() .and. memspice%kernel_memory() > 0, 'the second instance loads the SPK in memory')
   ! a programmatic body and frame in both instances: providers called from
   ! every thread
   eproc%f => programmatic_body
   fproc%f => programmatic_frame
   call spice%add_ephemeris(9100_ip, 1000_ip, 'J2000', 0.0_wp, 3.0e6_wp, eproc)
   call spice%add_frame('PEPPER_SPIN', 1400500_ip, 1000_ip, fproc)
   call memspice%add_ephemeris(9100_ip, 1000_ip, 'J2000', 0.0_wp, 3.0e6_wp, eproc)
   call memspice%add_frame('PEPPER_SPIN', 1400500_ip, 1000_ip, fproc)
   nproc%f => nested_body
   call spice%add_ephemeris(9101_ip, 1001_ip, 'J2000', 0.0_wp, 3.0e6_wp, nproc)
   call memspice%add_ephemeris(9101_ip, 1001_ip, 'J2000', 0.0_wp, 3.0e6_wp, nproc)
   call check(.not. spice%failed() .and. .not. memspice%failed(), 'a programmatic body and frame')

   allocate (et(nepoch), ref(nres, nepoch), par(nres, nepoch), refutc(nepoch), parutc(nepoch))
   do i = 1, nepoch
      et(i) = 1.0e5_wp + (1.8e6_wp*(i - 1))/(nepoch - 1)
   end do

   ! the reference: one context, in order
   serial = spice%new_context()
   do i = 1, nepoch
      call queries(serial, et(i), mod(i, 2) == 0, ref(:, i), refutc(i))
   end do
   call check(.not. serial%failed(), 'serial queries')

   !$ print '(a, i0, a)', 'test_parallel: ', nthreads, ' threads'
   do pass = 1, 4
      par = 0.0_wp
      parutc = ' '
      errors = 0
      ! the threads' work in a procedure: its context is a local variable,
      ! private to each thread (a variable of a BLOCK in the main program is
      ! shared: ifx gives it the main program's implicit SAVE)
      !$omp parallel num_threads(nthreads) reduction(+:errors)
      call thread_queries(pass, errors)
      !$omp end parallel
      call check(errors == 0, 'no errors in the parallel queries')
      nbad = count(any(par /= ref, dim=1) .or. parutc /= refutc)
      if (nbad > 0) print '(a, i0, a, i0)', '   pass ', pass, ': epochs with different results: ', nbad
      call check(nbad == 0, 'parallel results identical to the serial ones')
   end do

   call spice%kclear()
   call memspice%kclear()
   call serial%close_unloaded_files()  ! (Windows cannot delete an open file)
   call delete_files()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_parallel: all checks passed'

contains

   !> One thread's share of a pass of parallel queries: a context of its
   !> own (of the instance with the SPK on disk, passes 1 and 2, or in
   !> memory, passes 3 and 4) and the epochs the OpenMP loop gives it. Odd
   !> passes go forward, even passes backward; dynamic scheduling
   !> interleaves the threads' epochs (and so the contexts' caches see
   !> different sequences of queries than the serial run).
   subroutine thread_queries(pass, errors)
      integer, intent(in) :: pass  !! the pass (1 to 4)
      integer, intent(inout) :: errors  !! incremented if the context failed
      type(pepper_context) :: ctx
      character(1840) :: msg
      integer :: i, k
      if (pass <= 2) then
         ctx = spice%new_context()
      else
         ctx = memspice%new_context()  ! the SPK in memory
      end if
      !$omp do schedule(dynamic, 7)
      do k = 1, nepoch
         i = merge(k, nepoch + 1 - k, mod(pass, 2) == 1)
         call queries(ctx, et(i), mod(i, 2) == 0, par(:, i), parutc(i))
      end do
      !$omp end do
      if (ctx%failed()) then
         errors = errors + 1
         call ctx%getmsg('LONG', msg)
         !$omp critical (report)
         print '(2a)', '   error: ', trim(msg)
         !$omp end critical (report)
      end if
   end subroutine thread_queries

   !> The queries for one epoch, through one context: states and positions
   !> with aberration corrections, in J2000 and in the TK frame, rotations,
   !> and a round trip through UTC strings. obsalt chooses the observer of
   !> the first query (so that consecutive queries differ).
   subroutine queries(ctx, t, obsalt, res, utc)
      type(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: t  !! the epoch (TDB seconds past J2000)
      logical, intent(in) :: obsalt  !! use the barycenter as the first observer
      real(wp), intent(out) :: res(nres)  !! the results
      character(*), intent(out) :: utc  !! the epoch as a UTC string
      real(wp) :: lt, rot(3, 3)
      character(32) :: obs
      obs = merge('SOLAR SYSTEM BARYCENTER', 'PEPPER_A               ', obsalt)
      call ctx%spkezr('PEPPER_B', t, 'J2000', 'CN+S', obs, res(1:6), res(7))
      call ctx%spkpos('PEPPER_A', t, 'PEPPER_TK', 'LT', 'PEPPER_B', res(8:10), lt)
      res(11) = lt
      call ctx%pxform('J2000', 'ECLIPJ2000', t, rot)
      res(12:20) = reshape(rot, [9])
      call ctx%pxform('PEPPER_TK', 'J2000', t, rot)
      res(21:29) = reshape(rot, [9])
      call ctx%et2utc(t, 'ISOC', 3_ip, utc)
      call ctx%str2et(utc, res(30))
      call ctx%spkezr('9100', t, 'PEPPER_SPIN', 'LT+S', 'PEPPER_B', res(31:36), lt)
      call ctx%pxform('PEPPER_SPIN', 'PEPPER_TK', t, rot)
      res(37:45) = reshape(rot, [9])
      call ctx%spkezr('9101', t, 'PEPPER_SPIN', 'CN+S', 'PEPPER_A', res(46:51), lt)
   end subroutine queries

   !> A programmatic body whose provider queries programmatic body 9100:
   !> halfway from PEPPER_B to it (nested providers, from every thread).
   recursive subroutine nested_body(c, body, et, state, found)
      class(pepper_context), intent(inout) :: c  !! the context
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state relative to PEPPER_B
      logical, intent(out) :: found  !! a state is available
      real(wp) :: lt
      call c%spkgeo(9100_ip, et, 'J2000', 1001_ip, state, lt)
      state = 0.5_wp*state
      found = .not. c%failed()
   end subroutine nested_body

   !> A programmatic body circling PEPPER_A (an ephemeris_procedure).
   subroutine programmatic_body(c, body, et, state, found)
      class(pepper_context), intent(inout) :: c  !! the context
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state relative to PEPPER_A
      logical, intent(out) :: found  !! always
      real(wp), parameter :: r = 5000.0_wp, w = 1.0e-3_wp
      state = [r*cos(w*et), r*sin(w*et), 100.0_wp, -r*w*sin(w*et), r*w*cos(w*et), 0.0_wp]
      found = .true.
   end subroutine programmatic_body

   !> A programmatic frame turning about Z relative to J2000 (a
   !> frame_procedure).
   subroutine programmatic_frame(c, frame, et, xform, relative, found)
      class(pepper_context), intent(inout) :: c  !! the context
      integer(ip), intent(in) :: frame  !! the frame
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: xform(6, 6)  !! from this frame to J2000
      integer(ip), intent(out) :: relative  !! J2000
      logical, intent(out) :: found  !! always
      real(wp), parameter :: w = 2.0e-6_wp
      real(wp) :: cs, sn
      cs = cos(w*et)
      sn = sin(w*et)
      xform = 0
      xform(1:3, 1:3) = reshape([cs, sn, 0.0_wp, -sn, cs, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
      xform(4:6, 4:6) = xform(1:3, 1:3)
      xform(4:6, 1:3) = w*reshape([-sn, cs, 0.0_wp, -cs, -sn, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp], [3, 3])
      relative = 1
      found = .true.
   end subroutine programmatic_frame

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: msg
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
         if (setup%failed()) then
            call setup%getmsg('LONG', msg)
            print '(2a)', '   setup: ', trim(msg)
         end if
         if (serial%failed()) then
            call serial%getmsg('LONG', msg)
            print '(2a)', '   serial: ', trim(msg)
         end if
      end if
   end subroutine check

   !> Remove the test files that exist.
   subroutine delete_files()
      character(32) :: files(4)
      logical :: exists
      integer :: unit, k
      logical :: still_open  !! the file is connected to a unit (Windows can't delete it)
      files = [character(32) :: lsk, spk, fk, spk_mem]
      do k = 1, size(files)
         inquire (file=trim(files(k)), exist=exists)
         if (exists) then
            inquire (file=trim(files(k)), opened=still_open)
            if (still_open) error stop 'a file to delete is still open (Windows cannot delete it): '//trim(files(k))
            open (newunit=unit, file=trim(files(k)), status='old')
            close (unit, status='delete')
         end if
      end do
   end subroutine delete_files

   !> Copy the file `from` to `to`.
   subroutine copy_file(from, to)
      character(*), intent(in) :: from  !! the file
      character(*), intent(in) :: to  !! the copy
      character(1), allocatable :: bytes(:)
      integer :: unit
      integer(int64) :: n
      inquire (file=from, size=n)
      allocate (bytes(n))
      open (newunit=unit, file=from, access='stream', form='unformatted', status='old', action='read')
      read (unit) bytes
      close (unit)
      open (newunit=unit, file=to, access='stream', form='unformatted', status='replace', action='write')
      write (unit) bytes
      close (unit)
   end subroutine copy_file

   !> Write the kernels: the leapseconds, the frame kernel (a TK frame and
   !> the body names, as a JSON kernel) and the SPK.
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
      ! the frame kernel as a JSON kernel
      open (newunit=unit, file=fk, status='new', action='write')
      write (unit, '(a)') '// the frame kernel'
      write (unit, '(a)') '{'
      write (unit, '(a)') '  "+NAIF_BODY_NAME": ["PEPPER_A", "PEPPER_B"],'
      write (unit, '(a)') '  "+NAIF_BODY_CODE": [1000, 1001],'
      write (unit, '(a)') '  "FRAME_PEPPER_TK": 1500001,'
      write (unit, '(a)') '  "FRAME_1500001_NAME": "PEPPER_TK",'
      write (unit, '(a)') '  "FRAME_1500001_CLASS": 4,'
      write (unit, '(a)') '  "FRAME_1500001_CLASS_ID": 1500001,'
      write (unit, '(a)') '  "FRAME_1500001_CENTER": 1000,'
      write (unit, '(a)') '  "TKFRAME_1500001_RELATIVE": "ECLIPJ2000",'
      write (unit, '(a)') '  "TKFRAME_1500001_SPEC": "ANGLES",'
      write (unit, '(a)') '  "TKFRAME_1500001_UNITS": "DEGREES",'
      write (unit, '(a)') '  "TKFRAME_1500001_AXES": [3, 1, 3],'
      write (unit, '(a)') '  "TKFRAME_1500001_ANGLES": [10.0, 20.0, 30.0]'
      write (unit, '(a)') '}'
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
      call check(.not. setup%failed(), 'write ' // spk)
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

end program test_parallel
