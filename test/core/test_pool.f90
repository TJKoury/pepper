!> The kernel pool through the module API: each instance has its own pool,
!> errors go to the context, and text kernels (including @-dates, which
!> use the time parser) load into the instance of the context. Watches
!> (SWPOOL, CVPOOL, DWPOOL) belong to the context that sets them.
program test_pool

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   type(pepper_spice), target :: spice, other
   type(pepper_context) :: ctx, ctx2
   integer :: failures = 0

   ctx = spice%new_context()
   ctx2 = other%new_context()

   call test_put_get()
   call test_errors()
   call test_legacy_separate()
   call test_text_kernel()
   call test_watches()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_pool: all checks passed'

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

   !> Two contexts of one instance watch the same variable with the same
   !> agent name: each gets its own notices.
   subroutine test_watches()
      type(pepper_context) :: a, b
      logical :: update
      character(25) :: short
      a = spice%new_context()
      b = spice%new_context()
      call a%swpool('AGENT', 1_ip, [character(32) :: 'WATCH_X'])
      call b%swpool('AGENT', 2_ip, [character(32) :: 'WATCH_X', 'WATCH_Y'])
      call a%cvpool('AGENT', update)
      call check(update, 'watch: a notice after swpool (a)')
      call a%cvpool('AGENT', update)
      call check(.not. update, 'watch: no notice after the check (a)')
      call b%cvpool('AGENT', update)
      call check(update, 'watch: a notice after swpool (b, not taken by a)')
      call b%cvpool('AGENT', update)
      call check(.not. update, 'watch: no notice after the check (b)')
      call a%cvpool('OTHER', update)
      call check(.not. update, 'watch: no notice for an unknown agent')

      call a%pdpool('WATCH_Y', 1_ip, [1.0_wp])
      call a%cvpool('AGENT', update)
      call check(.not. update, 'watch: a does not watch WATCH_Y')
      call b%cvpool('AGENT', update)
      call check(update, 'watch: b watches WATCH_Y')

      call b%pipool('WATCH_X', 1_ip, [1_ip])
      call a%pdpool('UNWATCHED', 1_ip, [2.0_wp])
      call a%cvpool('AGENT', update)
      call check(update, 'watch: a sees the update of WATCH_X by b')
      call b%cvpool('AGENT', update)
      call check(update, 'watch: b sees its update of WATCH_X')

      call a%dvpool('WATCH_X')
      call b%dwpool('AGENT')
      call check(b%failed(), 'watch: dwpool with an unchecked update is an error')
      call b%getmsg('SHORT', short)
      call check(short == 'SPICE(UPDATEPENDING)', 'watch: SPICE(UPDATEPENDING)')
      call b%reset()
      call b%cvpool('AGENT', update)
      call check(update, 'watch: dvpool posts a notice')
      call b%dwpool('AGENT')
      call check(.not. b%failed(), 'watch: dwpool after the check')
      call b%cvpool('AGENT', update)
      call check(.not. update, 'watch: no notices after dwpool')

      call a%clpool()
      call a%cvpool('AGENT', update)
      call check(update, 'watch: clpool posts a notice')
      call check(.not. a%failed() .and. .not. b%failed(), 'watch: no errors')
   end subroutine test_watches

   !> Values stored in one instance's pool are found there, not in another.
   subroutine test_put_get()
      real(wp) :: values(5)
      integer(ip) :: n
      logical :: found
      call ctx%pdpool('TEST_X', 3_ip, [1.0_wp, 2.0_wp, 3.0_wp])
      call ctx%gdpool('TEST_X', 1_ip, 5_ip, n, values, found)
      call check(found .and. n == 3 .and. all(values(1:3) == [1.0_wp, 2.0_wp, 3.0_wp]), &
                 'pdpool/gdpool')
      call ctx2%gdpool('TEST_X', 1_ip, 5_ip, n, values, found)
      call check(.not. found, 'pools of two instances are separate')
      call check(.not. ctx%failed() .and. .not. ctx2%failed(), 'no errors')
   end subroutine test_put_get

   !> An invalid argument signals an error in the context.
   subroutine test_errors()
      real(wp) :: values(5)
      integer(ip) :: n
      logical :: found
      character(25) :: short
      call ctx%gdpool('TEST_X', 1_ip, 0_ip, n, values, found)
      call ctx%getmsg('SHORT', short)
      call check(ctx%failed() .and. short == 'SPICE(BADARRAYSIZE)', 'room 0: '//short)
      call ctx%reset()
   end subroutine test_errors

   !> The legacy interface uses its own (default) instance.
   subroutine test_legacy_separate()
      real(wp) :: values(5)
      integer(ip) :: n
      logical :: found
      external :: gdpool
      call gdpool('TEST_X', 1_ip, 5_ip, n, values, found)
      call check(.not. found, 'legacy pool is separate')
   end subroutine test_legacy_separate

   !> Load a text kernel into an instance; @-dates become seconds past J2000.
   subroutine test_text_kernel()
      character(*), parameter :: file = 'test_pool_kernel.tk'
      real(wp) :: values(5)
      integer(ip) :: n, unit
      logical :: found
      character(32) :: cvals(2)
      character(1840) :: msg
      open (newunit=unit, file=file, status='replace', action='write')
      write (unit, '(a)') 'A text kernel for test_pool.', '', '\begindata', &
         "TEST_NUMBERS = ( 1.5, -2, 3E2 )", &
         "TEST_DATE    = @2000-01-01T12:00:00", &
         "TEST_NAMES   = ( 'ALPHA', 'BETA' )", '', '\begintext'
      close (unit)
      call ctx%ldpool(file)
      if (ctx%failed()) then
         call ctx%getmsg('SHORT', msg)
         print '(2a)', 'ldpool: ', trim(msg)
         call ctx%getmsg('LONG', msg)
         print '(2a)', 'ldpool: ', trim(msg)
      end if
      call check(.not. ctx%failed(), 'ldpool')
      call ctx%gdpool('TEST_NUMBERS', 1_ip, 5_ip, n, values, found)
      call check(found .and. n == 3 .and. all(values(1:3) == [1.5_wp, -2.0_wp, 300.0_wp]), &
                 'numbers from the kernel')
      call ctx%gdpool('TEST_DATE', 1_ip, 5_ip, n, values, found)
      call check(found .and. n == 1 .and. values(1) == 0.0_wp, '@-date (J2000 = 0)')
      call ctx%gcpool('TEST_NAMES', 1_ip, 2_ip, n, cvals, found)
      call check(found .and. n == 2 .and. cvals(1) == 'ALPHA' .and. cvals(2) == 'BETA', &
                 'strings from the kernel')
      ! The other instance can load it too (its own reader and time-parser
      ! state).
      call ctx2%ldpool(file)
      call ctx2%gdpool('TEST_DATE', 1_ip, 5_ip, n, values, found)
      call check(found .and. values(1) == 0.0_wp, 'second instance loads the kernel')
      open (newunit=unit, file=file, status='old')
      close (unit, status='delete')
   end subroutine test_text_kernel

end program test_pool
