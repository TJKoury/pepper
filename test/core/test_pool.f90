!> The kernel pool through the module API: each instance has its own pool,
!> changed through the instance (errors go to its own context), and text
!> kernels (including @-dates, which use the time parser) load into it.
!> Watches (SWPOOL, CVPOOL, DWPOOL) belong to the context that sets them.
program test_pool

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   type(pepper_spice), target :: spice, other
   type(pepper_context), pointer :: ctx  !! spice's own context (it loads the kernels)
   type(pepper_context), pointer :: ctx2  !! other's own context
   integer :: failures = 0

   ctx => spice%context()
   ctx2 => other%context()

   call test_put_get()
   call test_errors()
   call test_text_kernel()
   call test_watches()
   call test_long_line()
   call test_line_terminators()
   call test_rdtext_lines()

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

      call spice%pdpool('WATCH_Y', 1_ip, [1.0_wp])
      call a%cvpool('AGENT', update)
      call check(.not. update, 'watch: a does not watch WATCH_Y')
      call b%cvpool('AGENT', update)
      call check(update, 'watch: b watches WATCH_Y')

      call spice%pipool('WATCH_X', 1_ip, [1_ip])
      call spice%pdpool('UNWATCHED', 1_ip, [2.0_wp])
      call a%cvpool('AGENT', update)
      call check(update, 'watch: a sees the update of WATCH_X by b')
      call b%cvpool('AGENT', update)
      call check(update, 'watch: b sees its update of WATCH_X')

      call spice%dvpool('WATCH_X')
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
      ! a new watch after dwpool reuses the freed slot
      call b%swpool('AGENT2', 1_ip, [character(32) :: 'WATCH_X'])
      call b%cvpool('AGENT2', update)
      call check(update .and. .not. b%failed(), 'watch: new watch after dwpool')

      call spice%clpool()
      call a%cvpool('AGENT', update)
      call check(update, 'watch: clpool posts a notice')
      call check(.not. a%failed() .and. .not. b%failed(), 'watch: no errors')
   end subroutine test_watches

   !> Values stored in one instance's pool are found there, not in another.
   subroutine test_put_get()
      real(wp) :: values(5)
      integer(ip) :: n
      logical :: found
      call spice%pdpool('TEST_X', 3_ip, [1.0_wp, 2.0_wp, 3.0_wp])
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
      call spice%ldpool(file)
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
      call other%ldpool(file)
      call ctx2%gdpool('TEST_DATE', 1_ip, 5_ip, n, values, found)
      call check(found .and. values(1) == 0.0_wp, 'second instance loads the kernel')
      open (newunit=unit, file=file, status='old')
      close (unit, status='delete')
   end subroutine test_text_kernel

   !> A text kernel line longer than 132 characters is cut there without
   !> an error (SPICELIB's behavior, kept and documented): a value past
   !> column 132 is lost.
   subroutine test_long_line()
      character(*), parameter :: file = 'test_pool_long_line.tk'
      character(:), allocatable :: line
      real(wp) :: values(5)
      integer(ip) :: n, unit
      logical :: found
      line = 'TEST_LONG = ( 1, 2,'
      line = line // repeat(' ', 140 - len(line)) // '3'
      open (newunit=unit, file=file, status='replace', action='write')
      write (unit, '(a)') '\begindata', line, ')', '\begintext'
      close (unit)
      call spice%ldpool(file)
      call check(.not. ctx%failed(), 'long line: no error')
      call ctx%gdpool('TEST_LONG', 1_ip, 5_ip, n, values, found)
      call check(found .and. n == 2 .and. all(values(1:2) == [1.0_wp, 2.0_wp]), &
                 'long line: the value past column 132 is lost')
      call ctx%reset()
      open (newunit=unit, file=file, status='old')
      close (unit, status='delete')
   end subroutine test_long_line

   !> Text kernels load whatever their line terminators (LF, CR-LF or a lone
   !> CR), with LDPOOL and with FURNSH: SPICELIB's FURNSH rejected a file
   !> whose terminators were not the platform's (SPICE(INCOMPATIBLEEOL)).
   subroutine test_line_terminators()
      character(*), parameter :: file = 'test_pool_eol.tk'
      character(*), parameter :: lf = achar(10), cr = achar(13)
      character(*), parameter :: names(3) = [character(5) :: 'LF', 'CR-LF', 'CR']
      character(2) :: eol
      character(1840) :: msg
      real(wp) :: values(5)
      integer(ip) :: n, unit
      integer :: k, how
      logical :: found
      do k = 1, size(names)
         select case (k)
         case (1); eol = lf
         case (2); eol = cr // lf
         case (3); eol = cr
         end select
         do how = 1, 2
            open (newunit=unit, file=file, status='replace', access='stream', form='unformatted')
            write (unit) 'A text kernel.' // trim(eol) // trim(eol) // '\begindata' // trim(eol) // &
               'TEST_EOL = ( 1, 2,' // trim(eol) // '  3 )' // trim(eol) // &
               "TEST_EOL_NAME = 'X'" // trim(eol) // '\begintext' // trim(eol)
            close (unit)
            call ctx%reset()
            call spice%clpool()
            if (how == 1) then
               call spice%ldpool(file)
            else
               call spice%furnsh(file)
            end if
            if (ctx%failed()) then
               call ctx%getmsg('SHORT', msg)
               print '(4a)', trim(names(k)), ': ', trim(msg)
            end if
            call ctx%gdpool('TEST_EOL', 1_ip, 5_ip, n, values, found)
            call check(.not. ctx%failed() .and. found .and. n == 3 .and. &
                       all(values(1:3) == [1.0_wp, 2.0_wp, 3.0_wp]), &
                       'line terminators ' // trim(names(k)) // merge(' (ldpool)', ' (furnsh)', how == 1))
            if (how == 2) call spice%unload(file)
         end do
      end do
      call ctx%reset()
      open (newunit=unit, file=file, status='old')
      close (unit, status='delete')
   end subroutine test_line_terminators

   !> RDTEXT splits the lines itself (the file is read as a stream): a
   !> CR-LF across its reads of 512 bytes, a lone CR, an empty line, a line
   !> longer than the caller's variable (truncated), and a last line without
   !> a terminator.
   subroutine test_rdtext_lines()
      character(*), parameter :: file = 'test_pool_rdtext.txt'
      character(*), parameter :: lf = achar(10), cr = achar(13)
      character(1000) :: line
      character(10) :: short
      logical :: eof
      integer :: unit
      open (newunit=unit, file=file, status='replace', access='stream', form='unformatted')
      write (unit) repeat('a', 511) // cr // lf // 'b' // cr // lf // 'c' // cr // cr // 'last'
      close (unit)
      call ctx%rdtext(file, line, eof)
      call check(.not. eof .and. line == repeat('a', 511), 'rdtext: a CR-LF across two reads')
      call ctx%rdtext(file, line, eof)
      call check(.not. eof .and. line == 'b', 'rdtext: the line after it')
      call ctx%rdtext(file, line, eof)
      call check(.not. eof .and. line == 'c', 'rdtext: a lone CR')
      call ctx%rdtext(file, line, eof)
      call check(.not. eof .and. line == ' ', 'rdtext: an empty line')
      call ctx%rdtext(file, line, eof)
      call check(.not. eof .and. line == 'last', 'rdtext: a last line without a terminator')
      call ctx%rdtext(file, line, eof)
      call check(eof .and. line == ' ', 'rdtext: the end of the file')
      ! a variable shorter than the line: truncated, and the next line is next
      call ctx%rdtext(file, short, eof)
      call check(.not. eof .and. short == repeat('a', 10), 'rdtext: a long line truncated')
      call ctx%rdtext(file, short, eof)
      call check(.not. eof .and. short == 'b', 'rdtext: the line after a truncated one')
      call ctx%cltext(file)
      call check(.not. ctx%failed(), 'rdtext: no error')
      open (newunit=unit, file=file, status='old')
      close (unit, status='delete')
   end subroutine test_rdtext_lines

end program test_pool
