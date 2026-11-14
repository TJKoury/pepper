!> Time conversion through the module API with two contexts: the leapseconds
!> kernel loaded through one context is used by the other (UNITIM watches
!> its variables with a watch of its own in each context), a change of the
!> variables is seen by both, and the TIMDEF defaults are settings of the
!> instance.
program test_time

   use pepper_kinds, only: ip, wp
   use pepper_instance_module, only: pepper_instance, pepper_context, zztpats
   use pepper_arrays, only: orderc

   implicit none

   character(*), parameter :: lsk = 'test_time.tls'
   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: a  !! the instance's context (it loads the kernels)
   type(pepper_context) :: b
   integer :: failures = 0
   real(wp) :: et, value_dp
   character(32) :: value

   a => spice%context()
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
   call spice%furnsh(lsk)
   call check(.not. a%failed(), 'a loads the leapseconds kernel')
   call b%str2et('2000 JAN 01 12:00:00 UTC', et)
   call check(.not. b%failed() .and. abs(et - 64.184_wp) < 0.01_wp, &
              'b converts UTC with the kernel loaded by a')
   call a%str2et('2000 JAN 01 12:00:00 UTC', et)
   call check(.not. a%failed() .and. abs(et - 64.184_wp) < 0.01_wp, 'a converts UTC too')

   ! a changes DELTA_T_A: both contexts see the change
   call spice%pdpool('DELTET/DELTA_T_A', 1_ip, [42.184_wp])
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

   ! DELTET keeps its values until the pool changes: b's DELTET sees a's
   ! change of the leapseconds (one more leap second before 2000)
   call b%deltet(0.0_wp, 'UTC', et)
   call check(abs(et - 74.184_wp) < 0.01_wp, 'deltet in b')
   call spice%pdpool('DELTET/DELTA_AT', 2_ip, [33.0_wp, -1.0e8_wp])
   call b%deltet(0.0_wp, 'UTC', et)
   call check(abs(et - 75.184_wp) < 0.01_wp, 'deltet in b sees the new DELTET/DELTA_AT')
   call b%deltet(et, 'ET', value_dp)
   call check(abs(value_dp - et) < 1.0e-6_wp, 'deltet for an ET epoch')

   call spice%kclear()
   call delete_file()

   ! Without the variables, DELTET signals the error on every call, and
   ! works once they are defined
   call b%deltet(0.0_wp, 'UTC', et)
   call b%getmsg('SHORT', value)
   call check(b%failed() .and. value == 'SPICE(KERNELVARNOTFOUND)', 'deltet without the variables')
   call b%reset()
   call b%deltet(0.0_wp, 'UTC', et)
   call check(b%failed(), 'deltet without the variables, again')
   call b%reset()
   call write_lsk(32.0_wp)
   call spice%furnsh(lsk)
   call b%deltet(0.0_wp, 'UTC', et)
   call check(.not. b%failed() .and. abs(et - 64.184_wp) < 0.01_wp, 'deltet once the kernel is loaded')
   call spice%kclear()
   call delete_file()

   ! TPARTV's patterns are stored sorted (in the order ORDERC gave
   ! SPICELIB): ORDERC leaves them as they are
   block
      character(32) :: known(300), meanng(300)
      integer(ip) :: n, i, order(300)
      call check(zztpats(a, 300_ip, n, known, meanng), 'zztpats')
      call orderc(known, n, order)
      call check(all(order(1:n) == [(i, i = 1, n)]), 'the time patterns are in ORDERC''s order')
   end block

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
      logical :: still_open  !! the file is connected to a unit (Windows can't delete it)
      inquire (file=lsk, exist=exists)
      if (exists) then
         inquire (file=lsk, opened=still_open)
         if (still_open) error stop 'a file to delete is still open (Windows cannot delete it): '//lsk
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
