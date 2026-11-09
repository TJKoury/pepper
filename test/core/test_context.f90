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

end program test_context
