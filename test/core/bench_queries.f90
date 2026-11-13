!> Speed of prepared queries (pepper_queries) against SPKEZR, PXFORM and
!> SXFORM, on the real kernels of the ANISE comparison: each operation
!> evaluated by the SPICELIB routine, by a prepared query one epoch at a
!> time, and by a prepared query on the whole batch of epochs; at random and
!> at sequential epochs; with the binary kernels read from their files as
!> needed, and loaded in memory. The three must give the same sums.
!>
!> Argument: the directory of the kernels (naif0012.tls, pck00011.tpc,
!> de421.bsp, earth_latest_high_prec.bpc, moon_pa_de440_200625.bpc,
!> moon_de440_250416.tf; `pixi run bench-queries` uses build/compare/kernels).
!> Without an argument (as in `pixi run test`) it does nothing.
program bench_queries
   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context, state_query, rotation_query
   implicit none
   integer(ip), parameter :: nq = 200000  !! epochs per operation
   real(wp), parameter :: year = 365.25_wp*86400.0_wp
   character(*), parameter :: kernels(6) = [character(26) :: 'naif0012.tls', 'pck00011.tpc', &
      'de421.bsp', 'earth_latest_high_prec.bpc', 'moon_pa_de440_200625.bpc', 'moon_de440_250416.tf']
   type(pepper_spice), target :: spice
   type(pepper_context), pointer :: ctx
   character(1024) :: kdir
   real(wp), allocatable :: rnd(:), seq(:)
   integer(ip) :: seed, i
   integer :: mem
   if (command_argument_count() < 1) then
      print '(a)', 'bench_queries: no kernel directory given; skipped (pixi run bench-queries)'
      stop
   end if
   call get_command_argument(1, kdir)
   ctx => spice%context()
   seed = 4242
   allocate (rnd(nq))
   do i = 1, nq
      rnd(i) = 20*year*(random(1000000_ip) - 1)/1.0e6_wp
   end do
   ! (a loop, not an array constructor: its temporary of NQ elements would be
   ! on the stack, more than Windows' default of 1 to 2 MB)
   allocate (seq(nq))
   do i = 1, nq
      seq(i) = (i - 1)*60.0_wp
   end do
   print '(a)', '| Operation | Epochs | Kernels | SPICELIB routine (µs) | Prepared query (µs) | Batch (µs) | Speedup (query) | Speedup (batch) |'
   print '(a)', '|---|---|---|--:|--:|--:|--:|--:|'
   do mem = 0, 1
      call spice%kclear()
      do i = 1, size(kernels)
         call spice%furnsh(trim(kdir) // '/' // trim(kernels(i)), in_memory=mem == 1)
      end do
      if (ctx%failed()) error stop 'bench_queries: could not load the kernels'
      call bench_state('SPKEZR Moon from Earth, J2000, NONE', 'MOON', 'EARTH', 'J2000', 'NONE', mem == 1)
      call bench_state('SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S', 'MARS BARYCENTER', 'EARTH', &
                       'ECLIPJ2000', 'LT+S', mem == 1)
      call bench_state('SPKEZR Mars barycenter from Earth, J2000, LT', 'MARS BARYCENTER', 'EARTH', &
                       'J2000', 'LT', mem == 1)
      call bench_state('SPKEZR Sun from Earth, J2000, CN+S', 'SUN', 'EARTH', 'J2000', 'CN+S', mem == 1)
      call bench_state('SPKEZR Moon from Earth, ITRF93, LT+S', 'MOON', 'EARTH', 'ITRF93', 'LT+S', mem == 1)
      call bench_rotation('PXFORM J2000 to ITRF93', 'J2000', 'ITRF93', mem == 1, .false.)
      call bench_rotation('PXFORM J2000 to IAU_MARS', 'J2000', 'IAU_MARS', mem == 1, .false.)
      call bench_rotation('PXFORM MOON_ME to J2000', 'MOON_ME', 'J2000', mem == 1, .false.)
      call bench_rotation('PXFORM J2000 to ECLIPJ2000', 'J2000', 'ECLIPJ2000', mem == 1, .false.)
      call bench_rotation('SXFORM J2000 to ITRF93', 'J2000', 'ITRF93', mem == 1, .true.)
   end do
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

   !> Print a table row.
   subroutine row(what, epochs, mem, t1, t2, t3)
      character(*), intent(in) :: what, epochs  !! the operation and the epochs
      logical, intent(in) :: mem  !! the kernels are in memory
      real(wp), intent(in) :: t1, t2, t3  !! the times per epoch (µs)
      character(20) :: k
      k = 'read as needed'
      if (mem) k = 'in memory'
      print '(7a, 3(f0.3, a), 2(f0.1, a))', '| ', what, ' | ', trim(epochs), ' | ', trim(k), ' | ', t1, ' | ', &
         t2, ' | ', t3, ' | ', t1/t2, '× | ', t1/t3, '× |'
   end subroutine row

   !> SPKEZR, query_state one epoch at a time, and query_state on the batch.
   subroutine bench_state(what, targ, obs, frame, abcorr, mem)
      character(*), intent(in) :: what, targ, obs, frame, abcorr  !! the operation
      logical, intent(in) :: mem  !! the kernels are in memory
      type(state_query) :: q
      real(wp), allocatable :: states(:,:), lts(:)
      real(wp) :: st(6), lt, s1, s2, s3, t, t1, t2, t3
      integer :: pass, k
      allocate (states(6, nq), lts(nq))
      do pass = 1, 2
         associate (ets => merge(rnd, seq, pass == 1))
            s1 = 0
            t = now()
            do k = 1, nq
               call ctx%spkezr(targ, ets(k), frame, abcorr, obs, st, lt)
               s1 = s1 + st(1) + st(4) + lt
            end do
            t1 = (now() - t)/nq*1.0e6_wp
            call ctx%prepare_state(q, targ, obs, frame, abcorr)
            s2 = 0
            t = now()
            do k = 1, nq
               call ctx%query_state(q, ets(k), st, lt)
               s2 = s2 + st(1) + st(4) + lt
            end do
            t2 = (now() - t)/nq*1.0e6_wp
            call ctx%prepare_state(q, targ, obs, frame, abcorr)
            t = now()
            call ctx%query_state(q, ets, states, lts)
            t3 = (now() - t)/nq*1.0e6_wp
            s3 = 0
            do k = 1, nq
               s3 = s3 + states(1, k) + states(4, k) + lts(k)
            end do
         end associate
         if (ctx%failed()) error stop 'bench_queries: a state query failed'
         if (s1 /= s2 .or. s1 /= s3) print '(a, 3es24.16)', 'DIFFERENT SUMS: ' // what, s1, s2, s3
         call row(what, merge('random    ', 'sequential', pass == 1), mem, t1, t2, t3)
      end do
   end subroutine bench_state

   !> PXFORM (or SXFORM), query_rotation (query_transform) one epoch at a
   !> time, and on the batch.
   subroutine bench_rotation(what, from, to, mem, xform)
      character(*), intent(in) :: what, from, to  !! the operation
      logical, intent(in) :: mem  !! the kernels are in memory
      logical, intent(in) :: xform  !! state transformations (SXFORM) instead of rotations
      type(rotation_query) :: q
      real(wp), allocatable :: rots(:,:,:), xfs(:,:,:)
      real(wp) :: rot(3, 3), xf(6, 6), s1, s2, s3, t, t1, t2, t3
      integer :: pass, k
      if (xform) then
         allocate (xfs(6, 6, nq))
      else
         allocate (rots(3, 3, nq))
      end if
      do pass = 1, 2
         associate (ets => merge(rnd, seq, pass == 1))
            call ctx%prepare_rotation(q, from, to)
            s1 = 0
            s2 = 0
            s3 = 0
            if (xform) then
               t = now()
               do k = 1, nq
                  call ctx%sxform(from, to, ets(k), xf)
                  s1 = s1 + xf(1, 1) + xf(4, 1)
               end do
               t1 = (now() - t)/nq*1.0e6_wp
               t = now()
               do k = 1, nq
                  call ctx%query_transform(q, ets(k), xf)
                  s2 = s2 + xf(1, 1) + xf(4, 1)
               end do
               t2 = (now() - t)/nq*1.0e6_wp
               t = now()
               call ctx%query_transform(q, ets, xfs)
               t3 = (now() - t)/nq*1.0e6_wp
               do k = 1, nq
                  s3 = s3 + xfs(1, 1, k) + xfs(4, 1, k)
               end do
            else
               t = now()
               do k = 1, nq
                  call ctx%pxform(from, to, ets(k), rot)
                  s1 = s1 + rot(1, 1) + rot(2, 3)
               end do
               t1 = (now() - t)/nq*1.0e6_wp
               t = now()
               do k = 1, nq
                  call ctx%query_rotation(q, ets(k), rot)
                  s2 = s2 + rot(1, 1) + rot(2, 3)
               end do
               t2 = (now() - t)/nq*1.0e6_wp
               t = now()
               call ctx%query_rotation(q, ets, rots)
               t3 = (now() - t)/nq*1.0e6_wp
               do k = 1, nq
                  s3 = s3 + rots(1, 1, k) + rots(2, 3, k)
               end do
            end if
         end associate
         if (ctx%failed()) error stop 'bench_queries: a frame query failed'
         if (s1 /= s2 .or. s1 /= s3) print '(a, 3es24.16)', 'DIFFERENT SUMS: ' // what, s1, s2, s3
         call row(what, merge('random    ', 'sequential', pass == 1), mem, t1, t2, t3)
      end do
   end subroutine bench_rotation
end program bench_queries
