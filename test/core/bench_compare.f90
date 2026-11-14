!> PEPPER's side of the speed comparison with SPICELIB (run by
!> bench/compare_spicelib.py; bench/bench_spicelib.f90 is SPICELIB's side
!> and does the same operations in the same order, with the same
!> pseudo-random inputs).
!>
!> Arguments: the directory of the real kernels (naif0012.tls,
!> pck00011.tpc, de421.bsp, earth_latest_high_prec.bpc,
!> moon_pa_de440_200625.bpc, moon_de440_250416.tf) and a work directory,
!> where the synthetic kernels are written (SPICELIB's side reads them).
!> Each operation prints one line: `key|n|microseconds per operation|checksum`
!> (the checksum, a sum of results, shows that both sides compute the same
!> thing). Without arguments (as in `pixi run test`) it does nothing.
program bench_compare

   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_instance_module, only: pepper_instance, pepper_context
   use pepper_test_kernels, only: write_spk, write_pool_kernel, write_body_names, &
      write_tk_frames, kernel_name, body_name, frame_name

   implicit none

   integer(ip), parameter :: nsets = 20  !! loads of the real kernel set
   integer(ip), parameter :: nq = 100000  !! queries per operation
   integer(ip), parameter :: nspk = 1000  !! synthetic SPKs (one body each)
   integer(ip), parameter :: nvars = 20000  !! variables of the synthetic text kernel (4 values each)
   integer(ip), parameter :: nnames = 10000  !! synthetic body names
   integer(ip), parameter :: nframes = 1000  !! synthetic TK frames
   integer(ip), parameter :: first_body = 100001, first_name = 200001, first_frame = 1600001
   real(wp), parameter :: year = 365.25_wp*86400.0_wp
   character(*), parameter :: real_kernels(6) = [character(26) :: 'naif0012.tls', 'pck00011.tpc', &
      'de421.bsp', 'earth_latest_high_prec.bpc', 'moon_pa_de440_200625.bpc', 'moon_de440_250416.tf']

   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: ctx
   character(1024) :: kdir, wdir
   real(wp), allocatable :: ets(:)
   character(40), allocatable :: utcs(:)
   integer(ip) :: seed

   if (command_argument_count() < 2) then
      print '(a)', 'bench_compare: no kernel directory given; skipped (see bench/compare_spicelib.py)'
      stop
   end if
   call get_command_argument(1, kdir)
   call get_command_argument(2, wdir)
   ctx => spice%context()

   call bench_real_kernels()
   call bench_spks()
   call bench_pool()
   call bench_names()
   call bench_frames()

contains

   !> Wall clock time (s).
   real(wp) function now()
      integer(int64) :: c, r
      call system_clock(c, r)
      now = real(c, wp)/real(r, wp)
   end function now

   !> A pseudo-random integer from 1 to n (Park-Miller; the same sequence
   !> as in bench_spicelib.f90).
   integer(ip) function random(n)
      integer(ip), intent(in) :: n  !! the range
      seed = int(mod(int(seed, int64)*48271_int64, 2147483647_int64), ip)
      random = mod(seed, n) + 1
   end function random

   !> `n` pseudo-random epochs from 0 to `span` seconds past J2000.
   subroutine random_epochs(n, span)
      integer(ip), intent(in) :: n  !! the number of epochs
      real(wp), intent(in) :: span  !! the range (s)
      integer(ip) :: i
      seed = 4242
      if (allocated(ets)) deallocate (ets)
      allocate (ets(n))
      do i = 1, n
         ets(i) = span*(random(1000000_ip) - 1)/1.0e6_wp
      end do
   end subroutine random_epochs

   !> Print one result; stop on an error (the other side would not be
   !> comparable).
   subroutine report(key, n, seconds, checksum)
      character(*), intent(in) :: key  !! the operation
      integer(ip), intent(in) :: n  !! the number of operations
      real(wp), intent(in) :: seconds  !! the total time
      real(wp), intent(in) :: checksum  !! a sum of the results
      character(1840) :: msg
      if (ctx%failed()) then
         call ctx%getmsg('LONG', msg)
         print '(4a)', 'FAILED: ', key, ': ', trim(msg)
         error stop
      end if
      print '(a,"|",i0,"|",es12.5,"|",es22.15)', key, n, 1.0e6_wp*seconds/n, checksum
   end subroutine report

   !> The real kernels: loading them, and queries of states, frames, times,
   !> names and constants.
   subroutine bench_real_kernels()
      real(wp) :: t0, t, state(6), pos(3), lt, rot(3, 3), xform(6, 6), sum, radii(3)
      integer(ip) :: i, k, code, n
      logical :: found
      character(36) :: name
      character(*), parameter :: names(4) = [character(16) :: 'MOON', 'EARTH', 'MARS BARYCENTER', 'SUN']
      t = 0
      do k = 1, nsets
         t0 = now()
         do i = 1, size(real_kernels)
            call spice%furnsh(trim(kdir) // '/' // trim(real_kernels(i)))
         end do
         t = t + now() - t0
         if (k < nsets) call spice%kclear()
      end do
      call report('load_set', nsets, t, 0.0_wp)
      call random_epochs(nq, 20*year)

      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%spkezr('MOON', ets(i), 'J2000', 'NONE', 'EARTH', state, lt)
         sum = sum + state(1)
      end do
      call report('spkezr_moon', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%spkezr('MARS BARYCENTER', ets(i), 'ECLIPJ2000', 'LT+S', 'EARTH', state, lt)
         sum = sum + state(1)
      end do
      call report('spkezr_mars_lts', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%spkpos('SUN', ets(i), 'J2000', 'CN+S', 'EARTH', pos, lt)
         sum = sum + pos(1)
      end do
      call report('spkpos_sun_cns', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%spkgeo(301_ip, ets(i), 'J2000', 399_ip, state, lt)
         sum = sum + state(1)
      end do
      call report('spkgeo_moon', nq, now() - t0, sum)

      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%pxform('J2000', 'ITRF93', ets(i), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_itrf93', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%pxform('J2000', 'IAU_MARS', ets(i), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_iau_mars', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%pxform('MOON_ME', 'J2000', ets(i), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_moon_me', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%sxform('J2000', 'ITRF93', ets(i), xform)
         sum = sum + xform(4, 1)
      end do
      call report('sxform_itrf93', nq, now() - t0, sum)

      if (allocated(utcs)) deallocate (utcs)
      allocate (utcs(nq))
      do i = 1, nq
         call ctx%et2utc(ets(i), 'ISOC', 3_ip, utcs(i))
      end do
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%str2et(utcs(i), t)
         sum = sum + (t - ets(i))
      end do
      call report('str2et', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%et2utc(ets(i), 'C', 3_ip, name)
         sum = sum + ichar(name(20:20))
      end do
      call report('et2utc', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         sum = sum + (ctx%unitim(ets(i), 'TDB', 'TAI') - ets(i))
      end do
      call report('unitim_tdb_tai', nq, now() - t0, sum)

      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%bodn2c(trim(names(mod(i, 4_ip) + 1)), code, found)
         sum = sum + code
      end do
      call report('bodn2c', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%bodvrd('EARTH', 'RADII', 3_ip, n, radii)
         sum = sum + radii(1)
      end do
      call report('bodvrd_radii', nq, now() - t0, sum)
      call spice%kclear()
   end subroutine bench_real_kernels

   !> Many SPKs (one body each, from the work directory): loading, a random
   !> body, unloading.
   subroutine bench_spks()
      real(wp) :: t0, state(6), lt, sum
      integer(ip) :: i
      character(:), allocatable :: file
      do i = 1, nspk
         file = kernel_name(trim(wdir) // '/spk_', i, '.bsp')
         call write_spk(ctx, file, first_body + i - 1, 0_ip, 1.0e6_wp, real(i, wp))
      end do
      t0 = now()
      do i = 1, nspk
         call spice%furnsh(kernel_name(trim(wdir) // '/spk_', i, '.bsp'))
      end do
      call report('spk_load', nspk, now() - t0, 0.0_wp)
      call random_epochs(nq, 1.0e6_wp)
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%spkgeo(first_body + random(nspk) - 1, ets(i), 'J2000', 0_ip, state, lt)
         sum = sum + state(1)
      end do
      call report('spk_random_body', nq, now() - t0, sum)
      t0 = now()
      do i = 1, nspk
         call spice%unload(kernel_name(trim(wdir) // '/spk_', i, '.bsp'))
      end do
      call report('spk_unload', nspk, now() - t0, 0.0_wp)
   end subroutine bench_spks

   !> A text kernel with many variables: loading it, random lookups.
   subroutine bench_pool()
      real(wp) :: t0, values(4), sum
      integer(ip) :: i, n
      logical :: found
      character(16) :: var
      character(*), parameter :: file = 'pool.tpc'
      call write_pool_kernel(trim(wdir) // '/' // file, 'BENCH_VAR_', nvars, 4_ip)
      t0 = now()
      call spice%furnsh(trim(wdir) // '/' // file)
      call report('pool_load', nvars, now() - t0, 0.0_wp)
      seed = 4242
      sum = 0; t0 = now()
      do i = 1, nq
         write (var, '(a,i0)') 'BENCH_VAR_', random(nvars)
         call ctx%gdpool(var, 1_ip, 4_ip, n, values, found)
         sum = sum + values(4)
      end do
      call report('pool_gdpool', nq, now() - t0, sum)
      call spice%unload(trim(wdir) // '/' // file)
   end subroutine bench_pool

   !> Many body names: loading them, random lookups.
   subroutine bench_names()
      real(wp) :: t0, sum
      integer(ip) :: i, code
      logical :: found
      character(*), parameter :: file = 'names.tf'
      call write_body_names(trim(wdir) // '/' // file, nnames, first_name)
      t0 = now()
      call spice%furnsh(trim(wdir) // '/' // file)
      call ctx%bodn2c(body_name(1_ip), code, found)   ! the tables are built on the first lookup
      call report('names_load', nnames, now() - t0, 0.0_wp)
      seed = 4242
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%bodn2c(body_name(random(nnames)), code, found)
         sum = sum + code
      end do
      call report('names_bodn2c', nq, now() - t0, sum)
      call spice%unload(trim(wdir) // '/' // file)
   end subroutine bench_names

   !> Many TK frames: loading them, transformations to random frames.
   subroutine bench_frames()
      real(wp) :: t0, rot(3, 3), sum
      integer(ip) :: i
      character(*), parameter :: file = 'frames.tf'
      call write_tk_frames(trim(wdir) // '/' // file, nframes, first_frame)
      t0 = now()
      call spice%furnsh(trim(wdir) // '/' // file)
      call report('frames_load', nframes, now() - t0, 0.0_wp)
      seed = 4242
      sum = 0; t0 = now()
      do i = 1, nq
         call ctx%pxform(frame_name(random(nframes)), 'J2000', 0.0_wp, rot)
         sum = sum + rot(1, 2)
      end do
      call report('frames_pxform', nq, now() - t0, sum)
      call spice%unload(trim(wdir) // '/' // file)
   end subroutine bench_frames

end program bench_compare
