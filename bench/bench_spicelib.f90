!> SPICELIB's side of the speed comparison with PEPPER (run by
!> bench/compare_spicelib.py, linked with SPICELIB N0067 built from NAIF's
!> sources). The same operations, in the same order and with the same
!> pseudo-random inputs, as test/core/bench_compare.f90, which writes the
!> synthetic kernels this program reads (run it first).
!>
!> Arguments: the directory of the real kernels and the work directory.
!> Each operation prints one line: `key|n|microseconds per operation|checksum`.
program bench_spicelib

   use, intrinsic :: iso_fortran_env, only: int64, int32, real64

   implicit none

   integer, parameter :: ip = int32, wp = real64
   integer(ip), parameter :: nsets = 20, nq = 100000, nspk = 1000, nvars = 20000
   integer(ip), parameter :: nnames = 10000, nframes = 1000
   integer(ip), parameter :: first_body = 100001
   real(wp), parameter :: year = 365.25_wp*86400.0_wp
   character(*), parameter :: real_kernels(6) = [character(26) :: 'naif0012.tls', 'pck00011.tpc', &
      'de421.bsp', 'earth_latest_high_prec.bpc', 'moon_pa_de440_200625.bpc', 'moon_de440_250416.tf']

   logical, external :: failed
   double precision, external :: unitim

   character(1024) :: kdir, wdir
   real(wp), allocatable :: ets(:)
   character(40), allocatable :: utcs(:)
   integer(ip) :: seed

   call get_command_argument(1, kdir)
   call get_command_argument(2, wdir)
   call erract('SET', 'RETURN')

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

   !> A pseudo-random integer from 1 to n (Park-Miller).
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

   !> The name of synthetic kernel `i` (as kernel_name in PEPPER's tests).
   function kernel_name(prefix, i, ext) result(name)
      character(*), intent(in) :: prefix  !! the start of the name
      integer(ip), intent(in) :: i  !! the number of the file
      character(*), intent(in) :: ext  !! the extension
      character(:), allocatable :: name
      character(12) :: buf
      write (buf, '(i0)') i
      name = prefix // trim(buf) // ext
   end function kernel_name

   !> Synthetic body name `i`.
   function body_name(i) result(name)
      integer(ip), intent(in) :: i  !! the number of the body
      character(:), allocatable :: name
      character(12) :: buf
      write (buf, '(i0)') i
      name = 'PEPPER_BODY_' // trim(buf)
   end function body_name

   !> Synthetic frame name `i`.
   function frame_name(i) result(name)
      integer(ip), intent(in) :: i  !! the number of the frame
      character(:), allocatable :: name
      character(12) :: buf
      write (buf, '(i0)') i
      name = 'PEPPER_TK_' // trim(buf)
   end function frame_name

   !> Print one result; stop on an error.
   subroutine report(key, n, seconds, checksum)
      character(*), intent(in) :: key  !! the operation
      integer(ip), intent(in) :: n  !! the number of operations
      real(wp), intent(in) :: seconds  !! the total time
      real(wp), intent(in) :: checksum  !! a sum of the results
      character(1840) :: msg
      if (failed()) then
         call getmsg('LONG', msg)
         print '(4a)', 'FAILED: ', key, ': ', trim(msg)
         error stop
      end if
      print '(a,"|",i0,"|",es12.5,"|",es22.15)', key, n, 1.0e6_wp*seconds/n, checksum
   end subroutine report

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
            call furnsh(trim(kdir) // '/' // trim(real_kernels(i)))
         end do
         t = t + now() - t0
         if (k < nsets) call kclear()
      end do
      call report('load_set', nsets, t, 0.0_wp)
      call random_epochs(nq, 20*year)

      sum = 0; t0 = now()
      do i = 1, nq
         call spkezr('MOON', ets(i), 'J2000', 'NONE', 'EARTH', state, lt)
         sum = sum + state(1)
      end do
      call report('spkezr_moon', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call spkezr('MARS BARYCENTER', ets(i), 'ECLIPJ2000', 'LT+S', 'EARTH', state, lt)
         sum = sum + state(1)
      end do
      call report('spkezr_mars_lts', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call spkpos('SUN', ets(i), 'J2000', 'CN+S', 'EARTH', pos, lt)
         sum = sum + pos(1)
      end do
      call report('spkpos_sun_cns', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call spkgeo(301, ets(i), 'J2000', 399, state, lt)
         sum = sum + state(1)
      end do
      call report('spkgeo_moon', nq, now() - t0, sum)

      sum = 0; t0 = now()
      do i = 1, nq
         call pxform('J2000', 'ITRF93', ets(i), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_itrf93', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call pxform('J2000', 'IAU_MARS', ets(i), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_iau_mars', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call pxform('MOON_ME', 'J2000', ets(i), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_moon_me', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call sxform('J2000', 'ITRF93', ets(i), xform)
         sum = sum + xform(4, 1)
      end do
      call report('sxform_itrf93', nq, now() - t0, sum)

      if (allocated(utcs)) deallocate (utcs)
      allocate (utcs(nq))
      do i = 1, nq
         call et2utc(ets(i), 'ISOC', 3, utcs(i))
      end do
      sum = 0; t0 = now()
      do i = 1, nq
         call str2et(utcs(i), t)
         sum = sum + (t - ets(i))
      end do
      call report('str2et', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call et2utc(ets(i), 'C', 3, name)
         sum = sum + ichar(name(20:20))
      end do
      call report('et2utc', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         sum = sum + (unitim(ets(i), 'TDB', 'TAI') - ets(i))
      end do
      call report('unitim_tdb_tai', nq, now() - t0, sum)

      sum = 0; t0 = now()
      do i = 1, nq
         call bodn2c(trim(names(mod(i, 4_ip) + 1)), code, found)
         sum = sum + code
      end do
      call report('bodn2c', nq, now() - t0, sum)
      sum = 0; t0 = now()
      do i = 1, nq
         call bodvrd('EARTH', 'RADII', 3, n, radii)
         sum = sum + radii(1)
      end do
      call report('bodvrd_radii', nq, now() - t0, sum)
      call kclear()
   end subroutine bench_real_kernels

   subroutine bench_spks()
      real(wp) :: t0, state(6), lt, sum
      integer(ip) :: i
      t0 = now()
      do i = 1, nspk
         call furnsh(kernel_name(trim(wdir) // '/spk_', i, '.bsp'))
      end do
      call report('spk_load', nspk, now() - t0, 0.0_wp)
      call random_epochs(nq, 1.0e6_wp)
      sum = 0; t0 = now()
      do i = 1, nq
         call spkgeo(first_body + random(nspk) - 1, ets(i), 'J2000', 0, state, lt)
         sum = sum + state(1)
      end do
      call report('spk_random_body', nq, now() - t0, sum)
      t0 = now()
      do i = 1, nspk
         call unload(kernel_name(trim(wdir) // '/spk_', i, '.bsp'))
      end do
      call report('spk_unload', nspk, now() - t0, 0.0_wp)
   end subroutine bench_spks

   subroutine bench_pool()
      real(wp) :: t0, values(4), sum
      integer(ip) :: i, n
      logical :: found
      character(16) :: var
      t0 = now()
      call furnsh(trim(wdir) // '/pool.tpc')
      call report('pool_load', nvars, now() - t0, 0.0_wp)
      seed = 4242
      sum = 0; t0 = now()
      do i = 1, nq
         write (var, '(a,i0)') 'BENCH_VAR_', random(nvars)
         call gdpool(var, 1, 4, n, values, found)
         sum = sum + values(4)
      end do
      call report('pool_gdpool', nq, now() - t0, sum)
      call unload(trim(wdir) // '/pool.tpc')
   end subroutine bench_pool

   subroutine bench_names()
      real(wp) :: t0, sum
      integer(ip) :: i, code
      logical :: found
      t0 = now()
      call furnsh(trim(wdir) // '/names.tf')
      call bodn2c(body_name(1), code, found)
      call report('names_load', nnames, now() - t0, 0.0_wp)
      seed = 4242
      sum = 0; t0 = now()
      do i = 1, nq
         call bodn2c(body_name(random(nnames)), code, found)
         sum = sum + code
      end do
      call report('names_bodn2c', nq, now() - t0, sum)
      call unload(trim(wdir) // '/names.tf')
   end subroutine bench_names

   subroutine bench_frames()
      real(wp) :: t0, rot(3, 3), sum
      integer(ip) :: i
      t0 = now()
      call furnsh(trim(wdir) // '/frames.tf')
      call report('frames_load', nframes, now() - t0, 0.0_wp)
      seed = 4242
      sum = 0; t0 = now()
      do i = 1, nq
         call pxform(frame_name(random(nframes)), 'J2000', 0.0d0, rot)
         sum = sum + rot(1, 2)
      end do
      call report('frames_pxform', nq, now() - t0, sum)
      call unload(trim(wdir) // '/frames.tf')
   end subroutine bench_frames

end program bench_spicelib
