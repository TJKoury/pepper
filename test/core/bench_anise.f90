!> PEPPER's side of the speed comparison with ANISE (run by
!> bench/compare_anise.py; bench/anise/src/main.rs is ANISE's side and does
!> the same queries, with the same pseudo-random epochs).
!>
!> Argument: the directory of the real kernels (naif0012.tls, pck00011.tpc,
!> de421.bsp, earth_latest_high_prec.bpc, moon_pa_de440_200625.bpc,
!> moon_de440_250416.tf). Each operation prints one line:
!> `key|n|microseconds per operation|checksum` (the checksum, a sum of
!> results, shows that both sides compute the same thing). Without an
!> argument (as in `pixi run test`) it does nothing.
program bench_anise

   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   integer(ip), parameter :: nsets = 10  !! loads of the kernel set
   integer(ip), parameter :: nq = 100000  !! queries per operation
   real(wp), parameter :: year = 365.25_wp*86400.0_wp
   real(wp), parameter :: step = 60.0_wp  !! spacing of the sequential epochs (s)
   character(*), parameter :: kernels(6) = [character(26) :: 'naif0012.tls', 'pck00011.tpc', &
      'de421.bsp', 'earth_latest_high_prec.bpc', 'moon_pa_de440_200625.bpc', 'moon_de440_250416.tf']

   type(pepper_spice), target :: spice
   type(pepper_context), pointer :: ctx
   character(1024) :: kdir
   real(wp), allocatable :: ets(:), seq(:)
   integer(ip) :: seed, i

   if (command_argument_count() < 1) then
      print '(a)', 'bench_anise: no kernel directory given; skipped (see bench/compare_anise.py)'
      stop
   end if
   call get_command_argument(1, kdir)
   ctx => spice%context()

   call random_epochs(nq, 20*year)
   seq = [(i*step, i=0, nq - 1)]
   ! the kernels read from their files as needed, then loaded in memory
   call bench_load(.false., '')
   call bench_states(ets, '')
   call bench_states(seq, '_seq')
   call bench_frames(ets, '')
   call bench_frames(seq, '_seq')
   call bench_load(.true., '_mem')
   call bench_states(ets, '_mem')
   call bench_states(seq, '_mem_seq')
   call bench_frames(ets, '_mem')
   call bench_frames(seq, '_mem_seq')

contains

   !> Wall clock time (s).
   real(wp) function now()
      integer(int64) :: c, r
      call system_clock(c, r)
      now = real(c, wp)/real(r, wp)
   end function now

   !> A pseudo-random integer from 1 to n (Park-Miller; the same sequence
   !> as in bench/anise/src/main.rs and bench_compare.f90).
   integer(ip) function random(n)
      integer(ip), intent(in) :: n  !! the range
      seed = int(mod(int(seed, int64)*48271_int64, 2147483647_int64), ip)
      random = mod(seed, n) + 1
   end function random

   !> `n` pseudo-random epochs from 0 to `span` seconds past J2000.
   subroutine random_epochs(n, span)
      integer(ip), intent(in) :: n  !! the number of epochs
      real(wp), intent(in) :: span  !! the range (s)
      integer(ip) :: k
      seed = 4242
      allocate (ets(n))
      do k = 1, n
         ets(k) = span*(random(1000000_ip) - 1)/1.0e6_wp
      end do
   end subroutine random_epochs

   !> Print one result; stop on an error.
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

   !> Loading the kernel set (FURNSH of the six kernels), `nsets` times;
   !> with `in_memory`, the binary kernels are read whole into memory.
   subroutine bench_load(in_memory, suffix)
      logical, intent(in) :: in_memory  !! FURNSH's in_memory
      character(*), intent(in) :: suffix  !! for the key
      real(wp) :: t, t0
      integer(ip) :: k, j
      t = 0
      do k = 1, nsets
         call spice%kclear()
         t0 = now()
         do j = 1, size(kernels)
            call spice%furnsh(trim(kdir) // '/' // trim(kernels(j)), in_memory=in_memory)
         end do
         t = t + now() - t0
      end do
      call report('load_set' // suffix, nsets, t, 0.0_wp)
   end subroutine bench_load

   !> States at the epochs `et`; `suffix` is appended to the keys.
   subroutine bench_states(et, suffix)
      real(wp), intent(in) :: et(:)  !! the epochs (TDB seconds past J2000)
      character(*), intent(in) :: suffix  !! for the keys
      real(wp) :: t0, state(6), pos(3), lt, sum
      integer(ip) :: k
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%spkezr('MOON', et(k), 'J2000', 'NONE', 'EARTH', state, lt)
         sum = sum + state(1)
      end do
      call report('spkezr_moon' // suffix, nq, now() - t0, sum)
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%spkgeo(301_ip, et(k), 'J2000', 399_ip, state, lt)
         sum = sum + state(1)
      end do
      call report('spkgeo_moon' // suffix, nq, now() - t0, sum)
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%spkezr('MARS BARYCENTER', et(k), 'ECLIPJ2000', 'LT+S', 'EARTH', state, lt)
         sum = sum + state(1)
      end do
      call report('spkezr_mars_lts' // suffix, nq, now() - t0, sum)
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%spkpos('SUN', et(k), 'J2000', 'CN+S', 'EARTH', pos, lt)
         sum = sum + pos(1)
      end do
      call report('spkpos_sun_cns' // suffix, nq, now() - t0, sum)
   end subroutine bench_states

   !> Frame transformations at the epochs `et`; `suffix` is appended to the keys.
   subroutine bench_frames(et, suffix)
      real(wp), intent(in) :: et(:)  !! the epochs (TDB seconds past J2000)
      character(*), intent(in) :: suffix  !! for the keys
      real(wp) :: t0, rot(3, 3), xform(6, 6), sum
      integer(ip) :: k
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%pxform('J2000', 'ITRF93', et(k), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_itrf93' // suffix, nq, now() - t0, sum)
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%sxform('J2000', 'ITRF93', et(k), xform)
         sum = sum + xform(4, 1)
      end do
      call report('sxform_itrf93' // suffix, nq, now() - t0, sum)
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%pxform('J2000', 'IAU_MARS', et(k), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_iau_mars' // suffix, nq, now() - t0, sum)
      sum = 0; t0 = now()
      do k = 1, nq
         call ctx%pxform('MOON_ME', 'J2000', et(k), rot)
         sum = sum + rot(1, 1)
      end do
      call report('pxform_moon_me' // suffix, nq, now() - t0, sum)
   end subroutine bench_frames

end program bench_anise
