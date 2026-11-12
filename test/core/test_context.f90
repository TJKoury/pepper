!> Unit tests of pepper_instance: error handling in contexts.
program test_context

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   type(pepper_spice), target :: spice
   integer :: failures = 0

   call test_native()
   call test_independent()
   call test_actions()
   call test_settings()
   call test_lengths()
   call test_report()
   call test_shared_files()
   call test_destroy()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_context: all checks passed'

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

   subroutine test_native()
      type(pepper_context) :: ctx
      character(1840) :: msg
      ctx = spice%new_context()
      call check(.not. ctx%failed(), 'new context')
      call check(associated(ctx%instance(), spice), 'instance')

      call ctx%setmsg('Body # at # (# s) not found.')
      call ctx%errch('#', '  MOON  ')
      call ctx%errint('#', 42_ip)
      call ctx%errdp('#', 1.5_wp)
      call ctx%sigerr('SPICE(CELLTOOSMALL)')
      call check(ctx%failed(), 'failed after sigerr')
      call ctx%getmsg('short', msg)
      call check(msg == 'SPICE(CELLTOOSMALL)', 'short message')
      call ctx%getmsg('LONG', msg)
      call check(msg == 'Body MOON at 42 (1.5000000000000E+00 s) not found.', 'long message')
      call ctx%getmsg('Explain', msg)
      call check(msg /= ' ', 'explanation')

      ! Under RETURN, only the first error is kept.
      call ctx%setmsg('Second error.')
      call ctx%sigerr('SPICE(SECOND)')
      call ctx%getmsg('SHORT', msg)
      call check(msg == 'SPICE(CELLTOOSMALL)', 'first error kept (short)')
      call ctx%getmsg('LONG', msg)
      call check(msg(1:4) == 'Body', 'first error kept (long)')

      call ctx%reset()
      call ctx%getmsg('SHORT', msg)
      call check(.not. ctx%failed() .and. msg == ' ', 'reset')

      ! A missing marker leaves the message unchanged.
      call ctx%setmsg('No markers.')
      call ctx%errch('#', 'x')
      call ctx%getmsg('LONG', msg)
      call check(msg == 'No markers.', 'missing marker')
   end subroutine test_native

   !> Contexts are independent: an error in one does not affect another.
   subroutine test_independent()
      type(pepper_context) :: a, b
      a = spice%new_context()
      b = spice%new_context()
      call a%sigerr('SPICE(A)')
      call check(a%failed() .and. .not. b%failed(), 'independent contexts')
   end subroutine test_independent

   subroutine test_actions()
      type(pepper_context) :: ctx
      character(7) :: action
      ctx = spice%new_context()
      action = ' '
      call ctx%erract('GET', action)
      call check(action == 'RETURN', 'default action RETURN')
      action = 'ignore'
      call ctx%erract('set', action)
      call ctx%sigerr('SPICE(IGNORED)')
      call check(.not. ctx%failed(), 'IGNORE')
      action = 'BOGUS'
      call ctx%erract('SET', action)   ! ignored too (the action is IGNORE)
      action = 'RETURN'
      call ctx%erract('SET', action)
      action = 'BOGUS'
      call ctx%erract('SET', action)
      call check(ctx%failed(), 'invalid action signals an error')
   end subroutine test_actions

   !> The output device (NULL for a new context) can be set to SPICELIB's
   !> SCREEN; under RETURN the first error is kept, as by SPICELIB.
   subroutine test_settings()
      type(pepper_context) :: ctx
      character(255) :: text
      character(80) :: msg
      ctx = spice%new_context()
      text = ' '
      call ctx%errdev('GET', text)
      call check(text == 'NULL', 'default device: '//trim(text))
      text = 'SCREEN'
      call ctx%errdev('SET', text)
      text = ' '
      call ctx%errdev('GET', text)
      call check(text == 'SCREEN', 'device set: '//trim(text))
      text = 'NULL'
      call ctx%errdev('SET', text)
      call ctx%sigerr('SPICE(FIRSTERROR)')
      call ctx%sigerr('SPICE(SECONDERROR)')
      call ctx%getmsg('SHORT', msg)
      call check(ctx%failed() .and. msg == 'SPICE(FIRSTERROR)', 'first error kept')
      call ctx%reset()
      call check(.not. ctx%failed(), 'reset')
   end subroutine test_settings

   !> Messages and the device have no length limit (SPICELIB's are 25,
   !> 1840 and 255 characters): getmsg cuts to the caller's variable,
   !> message returns the whole text.
   subroutine test_lengths()
      type(pepper_context) :: ctx
      character(25) :: short
      character(1840) :: long
      character(:), allocatable :: text, path
      character(400) :: dev
      ctx = spice%new_context()
      ! ET2LST's short message has 26 characters
      call ctx%sigerr('SPICE(CANTGETROTATIONTYPE)')
      call check(ctx%message('SHORT') == 'SPICE(CANTGETROTATIONTYPE)', 'a 26-character short message')
      call ctx%getmsg('SHORT', short)
      call check(short == 'SPICE(CANTGETROTATIONTYPE', 'getmsg cuts to the variable')
      call ctx%reset()
      call check(ctx%message('SHORT') == '' .and. ctx%message('LONG') == '', 'reset clears the messages')

      ! a long message of 5000 characters, with a marker past 1840
      call ctx%setmsg(repeat('ab ', 1500) // 'value #.')
      call ctx%errint('#', 42_ip)
      call ctx%sigerr('SPICE(LONGMESSAGE)')
      text = ctx%message('LONG')
      call check(len(text) == 4500 + 9 .and. text(4501:) == 'value 42.', 'a 4509-character long message')
      call ctx%getmsg('LONG', long)
      call check(long == text(1:1840), 'getmsg cuts the long message')
      call ctx%reset()

      ! an invalid option
      text = ctx%message('BOGUS')
      call check(text == '' .and. ctx%failed(), 'message: invalid option')
      call ctx%reset()

      ! a device name longer than 255 characters ('./' repeated)
      path = repeat('./', 140) // 'test_context_device.txt'
      dev = path
      call ctx%errdev('SET', dev)
      call check(.not. ctx%failed(), 'a 303-character device name')
      dev = ' '
      call ctx%errdev('GET', dev)
      call check(dev == path, 'the device name is kept whole')
   end subroutine test_lengths

   !> The report written to a device: lines of at most 80 characters,
   !> the long message broken between words, a word longer than a line
   !> split across lines.
   subroutine test_report()
      type(pepper_context) :: ctx
      character(*), parameter :: file = 'test_context_report.txt'
      character(:), allocatable :: msg, all
      character(200) :: line, dev
      integer :: unit, ios, longest
      logical :: exists
      ctx = spice%new_context()
      inquire (file=file, exist=exists)
      if (exists) then
         open (newunit=unit, file=file, status='old')
         close (unit, status='delete')
      end if
      msg = repeat('word ', 30) // repeat('X', 200) // ' end ' // repeat('Y', 85) // ' last.'
      dev = file
      call ctx%errdev('SET', dev)
      call ctx%setmsg(msg)
      call ctx%sigerr('SPICE(REPORTED)')
      dev = 'NULL'
      call ctx%errdev('SET', dev)
      ! the library keeps the report file open (SPICELIB's WRLINE)
      inquire (file=file, opened=exists, number=unit)
      if (exists) close (unit)
      ! the report: no line longer than 80; its text without blanks
      ! contains the message without blanks
      all = ''
      longest = 0
      open (newunit=unit, file=file, status='old', action='read')
      do
         read (unit, '(a)', iostat=ios) line
         if (ios /= 0) exit
         longest = max(longest, len_trim(line))
         all = all // no_blanks(line)
      end do
      close (unit, status='delete')
      call check(longest <= 80, 'report: lines of at most 80 characters')
      call check(index(all, no_blanks(msg)) > 0, 'report: the whole long message')
      call check(index(all, 'SPICE(REPORTED)') > 0, 'report: the short message')
   end subroutine test_report

   !> A string without its blanks.
   pure function no_blanks(s) result(t)
      character(*), intent(in) :: s  !! the string
      character(:), allocatable :: t
      integer :: i
      t = ''
      do i = 1, len(s)
         if (s(i:i) /= ' ') t = t // s(i:i)
      end do
   end function no_blanks

   !> A file read by a context stays connected to the context's unit until
   !> the context next uses its unit table, and each instance opens its
   !> files on units of its own: loading a file again after KCLEAR, and
   !> loading into a second instance a file the first one has open, are not
   !> external opens (SPICE(EXTERNALOPEN) before; found by PepperyPy).
   subroutine test_shared_files()
      use pepper_test_kernels, only: write_spk, circle_state, delete_file
      type(pepper_spice), target :: other
      type(pepper_context) :: c1, c2
      character(*), parameter :: spk = 'test_context_shared.bsp'
      real(wp) :: state(6), lt
      c1 = spice%new_context()
      call write_spk(c1, spk, 1001_ip, 0_ip, 7000.0_wp, 0.0_wp)
      call spice%furnsh(spk)
      call c1%spkgeo(1001_ip, 100.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c1%failed() .and. maxval(abs(state - circle_state(7000.0_wp, 0.0_wp, 100.0_wp))) < 1e-6_wp, &
                 'a context reads the file')
      call spice%kclear()
      call spice%furnsh(spk)
      call check(.not. spice%failed(), 'the file loaded again after KCLEAR (the context not used since)')
      call c1%spkgeo(1001_ip, 200.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c1%failed(), 'the context reads the file loaded again')
      call other%furnsh(spk)
      call check(.not. other%failed(), 'a second instance loads the file the first one has open')
      c2 = other%new_context()
      call c2%spkgeo(1001_ip, 300.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c2%failed() .and. maxval(abs(state - circle_state(7000.0_wp, 0.0_wp, 300.0_wp))) < 1e-6_wp, &
                 'a context of the second instance reads it')
      call c1%spkgeo(1001_ip, 300.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c1%failed(), 'the first instance''s context still reads it')
      call spice%kclear()
      call other%kclear()
      call delete_file(spk)
   end subroutine test_shared_files

   !> ctx%destroy and spice%destroy: the context's state and units go, the
   !> instance's kernels, pool and memory go, and both can be used again.
   subroutine test_destroy()
      use pepper_test_kernels, only: write_spk, circle_state, delete_file
      type(pepper_spice), target :: inst
      type(pepper_context) :: c
      character(*), parameter :: spk = 'test_context_destroy.bsp'
      real(wp) :: state(6), lt, values(1)
      integer(ip) :: count, n
      logical :: found
      c = inst%new_context()
      call write_spk(c, spk, 1001_ip, 0_ip, 7000.0_wp, 0.0_wp)
      call inst%furnsh(spk)
      call inst%pdpool('DESTROY_TEST', 1_ip, [1.5_wp])
      call c%spkgeo(1001_ip, 100.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c%failed(), 'a context reads the file before destroy')
      call c%sigerr('SPICE(TESTERROR)')
      call c%destroy()
      call check(.not. c%failed(), 'a destroyed context has no error')
      c = inst%new_context()
      call c%spkgeo(1001_ip, 200.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c%failed() .and. maxval(abs(state - circle_state(7000.0_wp, 0.0_wp, 200.0_wp))) < 1e-6_wp, &
                 'a context made again after destroy reads the file')
      call c%destroy()
      call inst%destroy()
      call check(inst%kernel_memory() == 0, 'a destroyed instance holds no kernel memory')
      c = inst%new_context()
      call c%ktotal('ALL', count)
      call check(count == 0, 'a destroyed instance has no kernels')
      call c%gdpool('DESTROY_TEST', 1_ip, 1_ip, n, values, found)
      call check(.not. found, 'a destroyed instance has an empty kernel pool')
      call c%spkgeo(1001_ip, 300.0_wp, 'J2000', 0_ip, state, lt)
      call check(c%failed(), 'a destroyed instance has no ephemeris')
      call c%reset()
      call inst%furnsh(spk)
      call check(.not. inst%failed(), 'a destroyed instance loads the file again')
      call c%spkgeo(1001_ip, 300.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c%failed() .and. maxval(abs(state - circle_state(7000.0_wp, 0.0_wp, 300.0_wp))) < 1e-6_wp, &
                 'and reads it')
      call c%destroy()
      call inst%destroy()
      call delete_file(spk)
   end subroutine test_destroy

end program test_context
