!> Benchmark of the operations whose cost depends on the tables that
!> SPICELIB sizes with fixed limits (limits.md, phase 0): loading many
!> kernels, SPK queries across many files, pool lookups among many
!> variables, body names, TK frames, and new contexts. Every result is
!> checked, so it also runs as a test:
!>
!>     fpm test bench_limits                                  # small sizes (a test)
!>     pixi run bench-limits                                  # full sizes, timed
!>
!> The full sizes stay under SPICELIB's limits (5000 files, 26003 pool
!> variables (a TK frame takes 10), 14983 kernel body names, 5209 kernel
!> frames), so the same program measures the code before and after the
!> limits are removed.
!> Memory per context: run it under `/usr/bin/time -l` (macOS) or
!> `/usr/bin/time -v` (Linux).
program bench_limits

   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_constants, only: rpd
   use pepper_instance, only: pepper_spice, pepper_context
   use pepper_test_kernels, only: write_lsk, write_spk, write_pool_kernel, write_body_names, &
      write_tk_frames, kernel_name, delete_file, body_name, frame_name, circle_state

   implicit none

   character(*), parameter :: prefix = 'bench_limits_'
   character(*), parameter :: lsk = prefix // 'lsk.tls'
   character(*), parameter :: pool_file = prefix // 'pool.tpc'
   character(*), parameter :: names_file = prefix // 'names.tf'
   character(*), parameter :: frames_file = prefix // 'frames.tf'
   integer(ip), parameter :: first_body = 100001  !! code of the first SPK body
   integer(ip), parameter :: first_name = 200001  !! code of the first named body
   integer(ip), parameter :: first_frame = 1600001  !! ID of the first TK frame
   real(wp), parameter :: radius = 1.0e6_wp  !! radius of the SPK circles (km)

   type(pepper_spice), target :: spice
   type(pepper_context) :: ctx
   integer(ip) :: nspk, nvars, nnames, nframes, ncontexts, nqueries
   integer :: failures = 0
   logical :: full
   character(16) :: arg
   integer(ip) :: seed = 4242

   full = .false.
   if (command_argument_count() > 0) then
      call get_command_argument(1, arg)
      full = arg == 'full'
   end if
   if (full) then
      nspk = 2000; nvars = 20000; nnames = 10000; nframes = 2000; ncontexts = 100; nqueries = 200000
   else
      nspk = 60; nvars = 500; nnames = 500; nframes = 300; ncontexts = 5; nqueries = 2000
   end if

   ctx = spice%new_context()
   print '(a)', 'bench_limits (' // merge('full ', 'small', full) // ' sizes)'
   print '(a40, a10, a14)', 'operation', 'n', 'us per op'
   call bench_spk()
   call bench_pool()
   call bench_names()
   call bench_frames()
   call bench_time()
   call bench_contexts()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'bench_limits: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
      end if
   end subroutine check

   !> Stop if the context has an error (the rest would only fail too).
   subroutine check_ctx(c, what)
      type(pepper_context), intent(inout) :: c  !! the context
      character(*), intent(in) :: what  !! what was done
      character(1840) :: msg
      if (c%failed()) then
         call c%getmsg('LONG', msg)
         print '(4a)', 'FAILED: ', what, ': ', trim(msg)
         error stop
      end if
   end subroutine check_ctx

   !> Wall clock time (s).
   real(wp) function now()
      integer(int64) :: c, r
      call system_clock(c, r)
      now = real(c, wp)/real(r, wp)
   end function now

   !> Print one result: the time per operation.
   subroutine report(what, n, seconds)
      character(*), intent(in) :: what  !! the operation
      integer(ip), intent(in) :: n  !! the number of operations
      real(wp), intent(in) :: seconds  !! the total time
      print '(a40, i10, f14.3)', what, n, 1.0e6_wp*seconds/n
   end subroutine report

   !> A pseudo-random integer from 1 to n (a fixed sequence: the
   !> Park-Miller generator).
   integer(ip) function random(n)
      integer(ip), intent(in) :: n  !! the range
      seed = int(mod(int(seed, int64)*48271_int64, 2147483647_int64), ip)
      random = mod(seed, n) + 1
   end function random

   !> Write and load `nspk` SPKs (one body each), query the bodies (the
   !> same one repeatedly, then a random one each time), unload them.
   subroutine bench_spk()
      real(wp) :: t0, state(6), lt, et, err
      integer(ip) :: i, k
      call write_lsk(lsk)
      do i = 1, nspk
         call write_spk(ctx, kernel_name(prefix // 'spk_', i, '.bsp'), first_body + i - 1, 0_ip, &
                        radius, phase(i))
      end do
      call check_ctx(ctx, 'writing the SPKs')

      t0 = now()
      call ctx%furnsh(lsk)
      do i = 1, nspk
         call ctx%furnsh(kernel_name(prefix // 'spk_', i, '.bsp'))
      end do
      call report('furnsh (SPK)', nspk, now() - t0)
      call check_ctx(ctx, 'loading the SPKs')

      err = 0
      t0 = now()
      do k = 1, nqueries
         et = 1.0e3_wp*mod(k, 1000_ip)
         call ctx%spkgeo(first_body, et, 'J2000', 0_ip, state, lt)
         if (k <= 100) err = max(err, position_error(state, 1_ip, et))
      end do
      call report('spkgeo, one body', nqueries, now() - t0)
      call check_ctx(ctx, 'spkgeo, one body')

      t0 = now()
      do k = 1, nqueries
         i = random(nspk)
         et = 1.0e3_wp*mod(k, 1000_ip)
         call ctx%spkgeo(first_body + i - 1, et, 'J2000', 0_ip, state, lt)
         if (k <= 1000) err = max(err, position_error(state, i, et))
      end do
      call report('spkgeo, random body of all files', nqueries, now() - t0)
      call check_ctx(ctx, 'spkgeo, random body')

      ! A context of an instance that keeps every file open (PEPPER: the
      ! open files per context are a setting, 23 by default).
      block
         type(pepper_context) :: wide
         call spice%set_max_open_files(nspk + 16)
         wide = spice%new_context()
         do k = 1, nqueries
            i = random(nspk)
            et = 1.0e3_wp*mod(k, 1000_ip)
            call wide%spkgeo(first_body + i - 1, et, 'J2000', 0_ip, state, lt)
         end do
         t0 = now()
         do k = 1, nqueries
            i = random(nspk)
            et = 1.0e3_wp*mod(k, 1000_ip)
            call wide%spkgeo(first_body + i - 1, et, 'J2000', 0_ip, state, lt)
            if (k <= 1000) err = max(err, position_error(state, i, et))
         end do
         call report('spkgeo, random body, all files open', nqueries, now() - t0)
         call check_ctx(wide, 'spkgeo, random body, all files open')
         call spice%set_max_open_files(23_ip)
      end block
      call check(err < 1.0e-3_wp, 'spkgeo: the states are on the circles')

      t0 = now()
      do i = 1, nspk
         call ctx%unload(kernel_name(prefix // 'spk_', i, '.bsp'))
      end do
      call report('unload (SPK)', nspk, now() - t0)
      call check_ctx(ctx, 'unloading the SPKs')
      do i = 1, nspk
         call delete_file(kernel_name(prefix // 'spk_', i, '.bsp'))
      end do
   end subroutine bench_spk

   !> The phase of SPK body i (rad).
   pure real(wp) function phase(i)
      integer(ip), intent(in) :: i  !! the number of the body
      phase = 1.0e-3_wp*i
   end function phase

   !> The largest difference (km) between a position and the one of SPK
   !> body i at et.
   pure real(wp) function position_error(state, i, et)
      real(wp), intent(in) :: state(6)  !! the state (km, km/s)
      integer(ip), intent(in) :: i  !! the number of the body
      real(wp), intent(in) :: et  !! the epoch
      real(wp) :: expected(6)
      expected = circle_state(radius, phase(i), et)
      position_error = maxval(abs(state(1:3) - expected(1:3)))
   end function position_error

   !> Load a text kernel with `nvars` variables and look them up.
   subroutine bench_pool()
      real(wp) :: t0, values(3)
      integer(ip) :: i, k, n
      logical :: found, ok
      character(32) :: name
      call write_pool_kernel(pool_file, 'BENCH_VAR_', nvars, 3_ip)
      t0 = now()
      call ctx%furnsh(pool_file)
      call report('furnsh (text kernel, per variable)', nvars, now() - t0)
      call check_ctx(ctx, 'loading the pool kernel')
      ok = .true.
      t0 = now()
      do k = 1, nqueries
         i = random(nvars)
         write (name, '(a,i0)') 'BENCH_VAR_', i
         call ctx%gdpool(name, 1_ip, 3_ip, n, values, found)
         if (.not. found .or. n /= 3 .or. any(values /= [i, i + 1, i + 2])) ok = .false.
      end do
      call report('gdpool, random variable', nqueries, now() - t0)
      call check(ok, 'gdpool: the values of the variables')
      call ctx%unload(pool_file)
      call delete_file(pool_file)
      call check_ctx(ctx, 'the pool')
   end subroutine bench_pool

   !> Load `nnames` body names and translate names and codes.
   subroutine bench_names()
      real(wp) :: t0
      integer(ip) :: i, k, code
      logical :: found, ok
      character(36) :: name
      call write_body_names(names_file, nnames, first_name)
      t0 = now()
      call ctx%furnsh(names_file)
      call report('furnsh (body names, per name)', nnames, now() - t0)
      call check_ctx(ctx, 'loading the body names')
      ok = .true.
      t0 = now()
      do k = 1, nqueries
         i = random(nnames)
         call ctx%bodn2c(body_name(i), code, found)
         if (.not. found .or. code /= first_name + i - 1) ok = .false.
      end do
      call report('bodn2c, random kernel name', nqueries, now() - t0)
      t0 = now()
      do k = 1, nqueries
         i = random(nnames)
         call ctx%bodc2n(first_name + i - 1, name, found)
         if (.not. found .or. name /= body_name(i)) ok = .false.
      end do
      call report('bodc2n, random kernel code', nqueries, now() - t0)
      call check(ok, 'bodn2c/bodc2n: the kernel names')
      call ctx%unload(names_file)
      call delete_file(names_file)
      call check_ctx(ctx, 'the body names')
   end subroutine bench_names

   !> Load `nframes` TK frames and compute rotations to J2000.
   subroutine bench_frames()
      real(wp) :: t0, rot(3, 3), err
      integer(ip) :: i, k
      call write_tk_frames(frames_file, nframes, first_frame)
      call ctx%furnsh(frames_file)
      call check_ctx(ctx, 'loading the frames')
      err = 0
      t0 = now()
      do i = 1, nframes
         call ctx%pxform(frame_name(i), 'J2000', 0.0_wp, rot)
         err = max(err, abs(atan2(rot(1, 2), rot(1, 1)) - i/1000.0_wp*rpd))
      end do
      call report('pxform, each frame once', nframes, now() - t0)
      t0 = now()
      do k = 1, nqueries
         i = random(nframes)
         call ctx%pxform(frame_name(i), 'J2000', 0.0_wp, rot)
         if (k <= 1000) err = max(err, abs(atan2(rot(1, 2), rot(1, 1)) - i/1000.0_wp*rpd))
      end do
      call report('pxform, random frame', nqueries, now() - t0)
      call check_ctx(ctx, 'pxform')
      call check(err < 1.0e-12_wp, 'pxform: the rotation angles')
      call ctx%unload(frames_file)
      call delete_file(frames_file)
   end subroutine bench_frames

   !> Time conversions with 28 leapseconds (as many as the current LSK):
   !> DELTET for UTC and ET epochs, ET2UTC and STR2ET.
   subroutine bench_time()
      integer(ip), parameter :: nleap = 28
      real(wp) :: t0, dat(2*nleap), delta, sum, et, err
      character(40) :: utc
      integer(ip) :: i
      do i = 1, nleap
         dat(2*i - 1) = 9 + i
         dat(2*i) = (365.0_wp*i - 10000)*86400 - 43200
      end do
      call ctx%pdpool('DELTET/DELTA_T_A', 1_ip, [32.184_wp])
      call ctx%pdpool('DELTET/K', 1_ip, [1.657e-3_wp])
      call ctx%pdpool('DELTET/EB', 1_ip, [1.671e-2_wp])
      call ctx%pdpool('DELTET/M', 2_ip, [6.239996_wp, 1.99096871e-7_wp])
      call ctx%pdpool('DELTET/DELTA_AT', 2*nleap, dat)
      sum = 0
      t0 = now()
      do i = 1, nqueries
         call ctx%deltet(1.0e3_wp*i, 'UTC', delta)
         sum = sum + delta
      end do
      call report('deltet (UTC)', nqueries, now() - t0)
      t0 = now()
      do i = 1, nqueries
         call ctx%deltet(1.0e3_wp*i, 'ET', delta)
         sum = sum + delta
      end do
      call report('deltet (ET)', nqueries, now() - t0)
      t0 = now()
      do i = 1, nqueries
         call ctx%et2utc(1.0e3_wp*i, 'ISOC', 3_ip, utc)
      end do
      call report('et2utc', nqueries, now() - t0)
      err = 0
      t0 = now()
      do i = 1, nqueries
         call ctx%str2et(utc, et)
      end do
      call report('str2et', nqueries, now() - t0)
      call check_ctx(ctx, 'time conversions')
      call check(abs(et - 1.0e3_wp*nqueries) < 1.0e-3_wp, 'time: et2utc and str2et agree')
      print '(a, es24.16)', '   (checksum of the deltas: ', sum
   end subroutine bench_time

   !> Create contexts and make a first SPK query in each.
   subroutine bench_contexts()
      real(wp) :: t0, t1, state(6), lt
      type(pepper_context), allocatable :: contexts(:)
      integer(ip) :: i
      character(*), parameter :: spk = prefix // 'context.bsp'
      call write_spk(ctx, spk, first_body, 0_ip, radius, phase(1_ip))
      call ctx%furnsh(spk)
      call check_ctx(ctx, 'loading the SPK')
      allocate (contexts(ncontexts))
      t0 = now()
      do i = 1, ncontexts
         contexts(i) = spice%new_context()
      end do
      t1 = now()
      call report('new_context', ncontexts, t1 - t0)
      do i = 1, ncontexts
         call contexts(i)%spkgeo(first_body, 0.0_wp, 'J2000', 0_ip, state, lt)
         call check_ctx(contexts(i), 'first query in a new context')
      end do
      call report('first spkgeo in a new context', ncontexts, now() - t1)
      call check(position_error(state, 1_ip, 0.0_wp) < 1.0e-3_wp, &
                 'first query: the state')
      deallocate (contexts)
      call ctx%kclear()
      call delete_file(spk)
      call delete_file(lsk)
   end subroutine bench_contexts

end program bench_limits
