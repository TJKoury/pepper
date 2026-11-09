!> Time conversion through the module API with two contexts: the leapseconds
!> kernel loaded through one context is used by the other (UNITIM watches
!> its variables with a watch of its own in each context), a change of the
!> variables is seen by both, and the TIMDEF defaults are settings of the
!> instance.
program test_time

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   character(*), parameter :: lsk = 'test_time.tls'
   type(pepper_spice), target :: spice
   type(pepper_context) :: a, b
   integer :: failures = 0
   real(wp) :: et
   character(32) :: value

   a = spice%new_context()
   b = spice%new_context()

   ! Without a leapseconds kernel, STR2ET fails (as SPICELIB's does, even
   ! for TDB): b's time routines have run before a loads the kernel.
   call b%str2et('2000 JAN 01 12:00:00 TDB', et)
   call b%getmsg('SHORT', value)
   call check(b%failed() .and. value == 'SPICE(NOLEAPSECONDS)', 'str2et without a leapseconds kernel')
   call b%reset()

   ! UTC needs the leapseconds: 32 leap seconds before 2000, so noon UTC is
   ! ET 32 + 32.184 (TAI-TDT) + the small TDB-TDT term.
   call delete_file()
   call write_lsk(32.0_wp)
   call a%furnsh(lsk)
   call check(.not. a%failed(), 'a loads the leapseconds kernel')
   call b%str2et('2000 JAN 01 12:00:00 UTC', et)
   call check(.not. b%failed() .and. abs(et - 64.184_wp) < 0.01_wp, &
              'b converts UTC with the kernel loaded by a')
   call a%str2et('2000 JAN 01 12:00:00 UTC', et)
   call check(.not. a%failed() .and. abs(et - 64.184_wp) < 0.01_wp, 'a converts UTC too')

   ! a changes DELTA_T_A: both contexts see the change
   call a%pdpool('DELTET/DELTA_T_A', 1_ip, [42.184_wp])
   call b%str2et('2000 JAN 01 12:00:00 UTC', et)
   call check(abs(et - 74.184_wp) < 0.01_wp, 'b sees the new DELTET/DELTA_T_A')
   call a%str2et('2000 JAN 01 12:00:00 UTC', et)
   call check(abs(et - 74.184_wp) < 0.01_wp, 'a sees the new DELTET/DELTA_T_A')

   ! TIMDEF: a default set through one context applies to all (noon of
   ! 2000 January 1 TDB is ET 0)
   value = 'TDB'
   call a%timdef('SET', 'SYSTEM', value)
   call b%str2et('2000 JAN 01 12:00:00', et)
   call check(.not. b%failed() .and. abs(et) < 1.0e-9_wp, 'b uses the default system set by a')
   value = 'UTC'
   call a%timdef('SET', 'SYSTEM', value)

   call a%kclear()
   call delete_file()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_time: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: msg
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
         if (a%failed()) then
            call a%getmsg('LONG', msg)
            print '(2a)', '   a: ', trim(msg)
         end if
         if (b%failed()) then
            call b%getmsg('LONG', msg)
            print '(2a)', '   b: ', trim(msg)
         end if
      end if
   end subroutine check

   !> Remove the test file if it exists.
   subroutine delete_file()
      logical :: exists
      integer :: unit
      inquire (file=lsk, exist=exists)
      if (exists) then
         open (newunit=unit, file=lsk, status='old')
         close (unit, status='delete')
      end if
   end subroutine delete_file

   !> Write a leapseconds kernel with one entry: DELTA_AT = leaps from 1999.
   subroutine write_lsk(leaps)
      real(wp), intent(in) :: leaps  !! TAI-UTC (leap seconds) from 1999 JAN 1
      integer :: unit
      open (newunit=unit, file=lsk, status='new', action='write')
      write (unit, '(a)') 'KPL/LSK'
      write (unit, '(a)') '\begindata'
      write (unit, '(a)') 'DELTET/DELTA_T_A = 32.184'
      write (unit, '(a)') 'DELTET/K = 1.657D-3'
      write (unit, '(a)') 'DELTET/EB = 1.671D-2'
      write (unit, '(a)') 'DELTET/M = ( 6.239996D0 1.99096871D-7 )'
      write (unit, '(a, f4.1, a)') 'DELTET/DELTA_AT = ( ', leaps, ', @1999-JAN-1 )'
      write (unit, '(a)') '\begintext'
      close (unit)
   end subroutine write_lsk

end program test_time
