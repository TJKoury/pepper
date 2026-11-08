!> Benchmark: the cost of a kernel pool lookup (gdpool) through the legacy
!> interface, a native context, and the legacy context.
program bench_pool
   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context
   use pepper_legacy_context, only: legacy_context
   implicit none
   type(pepper_spice), target :: spice
   type(pepper_context) :: ctx
   type(pepper_context), pointer :: lctx
   real(wp) :: v(3), t0, t1
   integer(ip) :: n, i
   logical :: found
   integer, parameter :: m = 1000000
   external :: pdpool, gdpool, swpool, cvpool
   ctx = spice%new_context()
   call ctx%pdpool('BENCH_X', 3_ip, [1.0_wp, 2.0_wp, 3.0_wp])
   call pdpool('BENCH_X', 3_ip, [1.0_wp, 2.0_wp, 3.0_wp])
   lctx => legacy_context()
   call cpu_time(t0)
   do i = 1, m
      call ctx%gdpool('BENCH_X', 1_ip, 3_ip, n, v, found)
   end do
   call cpu_time(t1); print '(a,f8.3,a)', 'native context      ', (t1-t0)*1e6/m, ' us/call'
   call cpu_time(t0)
   do i = 1, m
      call lctx%gdpool('BENCH_X', 1_ip, 3_ip, n, v, found)
   end do
   call cpu_time(t1); print '(a,f8.3,a)', 'legacy context, bound', (t1-t0)*1e6/m, ' us/call'
   call cpu_time(t0)
   do i = 1, m
      call gdpool('BENCH_X', 1_ip, 3_ip, n, v, found)
   end do
   call cpu_time(t1); print '(a,f8.3,a)', 'legacy wrapper      ', (t1-t0)*1e6/m, ' us/call'
   ! Watchers: set a watch and check it, as the SCLK and frame code does
   ! when it switches between data sets.
   call cpu_time(t0)
   do i = 1, m / 10
      call swpool('BENCH_AGENT', 1_ip, ['BENCH_X'])
      call cvpool('BENCH_AGENT', found)
   end do
   call cpu_time(t1); print '(a,f8.3,a)', 'legacy swpool+cvpool', (t1-t0)*1e6/(m/10), ' us/call'
end program bench_pool
