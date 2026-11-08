!> Body names and codes through the module API, with several contexts on
!> one instance: built-in names, names defined with boddef (recorded in the
!> instance, so every context sees them), and names from the kernel pool
!> (each context's cache is rebuilt when the pool changes).
program test_bodies

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   type(pepper_spice), target :: spice
   type(pepper_context) :: a, b
   integer :: failures = 0

   a = spice%new_context()
   b = spice%new_context()

   call test_builtin()
   call test_boddef()
   call test_pool_names()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_bodies: all checks passed'

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

   !> Built-in names, in both directions.
   subroutine test_builtin()
      integer(ip) :: code
      character(36) :: name
      logical :: found
      call a%bodn2c('EARTH', code, found)
      call check(found .and. code == 399, 'bodn2c EARTH')
      call b%bodc2n(301_ip, name, found)
      call check(found .and. name == 'MOON', 'bodc2n 301')
      call a%bods2c('-82', code, found)
      call check(found .and. code == -82, 'bods2c of an integer string')
      call check(.not. a%failed() .and. .not. b%failed(), 'no errors')
   end subroutine test_builtin

   !> A name defined through one context is known to all contexts.
   subroutine test_boddef()
      integer(ip) :: code
      character(36) :: name
      logical :: found
      call b%bodn2c('PEPPER_PROBE', code, found)   ! b's tables exist before the definition
      call check(.not. found, 'not defined yet')
      call a%boddef('PEPPER_PROBE', -999001_ip)
      call a%bodn2c('PEPPER_PROBE', code, found)
      call check(found .and. code == -999001, 'boddef seen by the defining context')
      call b%bodn2c('PEPPER_PROBE', code, found)
      call check(found .and. code == -999001, 'boddef seen by another context')
      call b%bodc2n(-999001_ip, name, found)
      call check(found .and. name == 'PEPPER_PROBE', 'bodc2n of the defined code')
   end subroutine test_boddef

   !> Names from a text kernel loaded after both contexts built their tables.
   subroutine test_pool_names()
      character(*), parameter :: file = 'test_bodies_kernel.tk'
      integer(ip) :: code, unit
      logical :: found
      call a%bodn2c('PEPPER_LANDER', code, found)
      call b%bodn2c('PEPPER_LANDER', code, found)
      call check(.not. found, 'kernel name not loaded yet')
      open (newunit=unit, file=file, status='replace', action='write')
      write (unit, '(a)') '\begindata', "NAIF_BODY_NAME += 'PEPPER_LANDER'", &
         'NAIF_BODY_CODE += -999002', '\begintext'
      close (unit)
      call a%ldpool(file)
      call a%bodn2c('PEPPER_LANDER', code, found)
      call check(found .and. code == -999002, 'kernel name, loading context')
      call b%bodn2c('PEPPER_LANDER', code, found)
      call check(found .and. code == -999002, 'kernel name, other context')
      call check(.not. a%failed() .and. .not. b%failed(), 'no errors after loading')
      open (newunit=unit, file=file, status='old')
      close (unit, status='delete')
   end subroutine test_pool_names

end program test_bodies
