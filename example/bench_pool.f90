!> Benchmark: the cost of a kernel pool lookup (gdpool) and of a watch
!> (swpool + cvpool) in a context.
program bench_pool
   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context
   implicit none
   type(pepper_spice), target :: spice
   type(pepper_context), pointer :: ctx  !! the instance's context (it loads the kernels)
   ctx => spice%context()
   call spice%pdpool('BENCH_X', 3_ip, [1.0_wp, 2.0_wp, 3.0_wp])
   call run(ctx, 'context')
contains
   !> Time the lookups and watches in a context.
   subroutine run(c, label)
      type(pepper_context), intent(inout) :: c  !! the context
      character(*), intent(in) :: label  !! its name in the output
      integer, parameter :: m = 1000000
      real(wp) :: v(3), t0, t1
      integer(ip) :: n
      integer :: i
      logical :: found
      call cpu_time(t0)
      do i = 1, m
         call c%gdpool('BENCH_X', 1_ip, 3_ip, n, v, found)
      end do
      call cpu_time(t1); print '(a,a,f8.3,a)', label, ', gdpool:       ', (t1-t0)*1e6/m, ' us/call'
      ! Watchers: set a watch and check it, as the SCLK and frame code does
      ! when it switches between data sets.
      call cpu_time(t0)
      do i = 1, m / 10
         call c%swpool('BENCH_AGENT', 1_ip, ['BENCH_X'])
         call c%cvpool('BENCH_AGENT', found)
      end do
      call cpu_time(t1); print '(a,a,f8.3,a)', label, ', swpool+cvpool:', (t1-t0)*1e6/(m/10), ' us/call'
   end subroutine run
end program bench_pool
