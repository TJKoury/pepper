!> Unit tests of NPARSD: correctly rounded values (Fortran's READ), and
!> SPICELIB's forms and errors.
program test_nparsd

   use pepper_kinds, only: ip, wp
   use pepper_constants, only: pi
   use pepper_strings, only: nparsd

   implicit none

   integer :: failures = 0

   ! correctly rounded: the same as Fortran's READ of the literal (SPICELIB
   ! was 1 to 5 ulps off for these; spicelib_bugs.md)
   call same('1.8200000000000001E+00', '1.8200000000000001E+00')
   call same('1.3271244004193938E+11', '1.3271244004193938E+11')
   call same('-1.4D-12', '-1.4E-12')
   call same('6.644300993056522D-09', '6.644300993056522E-09')
   call same('0.1', '0.1')
   call same('123456789012345678901234567890', '123456789012345678901234567890')
   call same('1.7976931348623157D308', '1.7976931348623157E308')
   call same('2.2250738585072014D-308', '2.2250738585072014E-308')
   ! SPICELIB's forms
   call value('+12.2 e-1', 1.22_wp)
   call value('-3. 1415 9276', -3.14159276_wp)
   call value('1,000,000', 1.0e6_wp)
   call value('E10', 1.0e10_wp)
   call value('-E-2', -0.01_wp)
   call value('-.E3', -1000.0_wp)
   call value('5.', 5.0_wp)
   call value('.5', 0.5_wp)
   call value('1E', 1.0_wp)
   call value('1D+', 1.0_wp)
   call value('pi', pi)
   call value('-Pi', -pi)
   call value('1E-400', 0.0_wp)
   ! errors (the pointer: SPICELIB's)
   call fails('', 1, 'blank')
   call fails('1E400', 6, 'too large')
   call fails('+', 2, 'could not be recognized')
   call fails('.', 2, 'could not be recognized')
   call fails('1..2', 3, 'unexpected decimal point')
   call fails('1-2', 2, 'unexpected sign')
   call fails('1x', 2, 'unexpected character')
   call fails('p', 1, 'could not be recognized')

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_nparsd: all checks passed'

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

   !> NPARSD gives the same value as Fortran's READ.
   subroutine same(string, fortran)
      character(*), intent(in) :: string  !! the input of NPARSD
      character(*), intent(in) :: fortran  !! the same number for READ
      real(wp) :: x, y
      character(200) :: error
      integer(ip) :: ptr
      read (fortran, *) y
      call nparsd(string, x, error, ptr)
      call check(error == ' ' .and. x == y, 'nparsd: '//string//' as READ')
   end subroutine same

   !> NPARSD gives a value.
   subroutine value(string, expected)
      character(*), intent(in) :: string  !! the input of NPARSD
      real(wp), intent(in) :: expected  !! its value
      real(wp) :: x
      character(200) :: error
      integer(ip) :: ptr
      call nparsd(string, x, error, ptr)
      call check(error == ' ' .and. ptr == 0 .and. x == expected, 'nparsd: '//string)
   end subroutine value

   !> NPARSD signals an error.
   subroutine fails(string, at, what)
      character(*), intent(in) :: string  !! the input of NPARSD
      integer, intent(in) :: at  !! the expected pointer
      character(*), intent(in) :: what  !! a part of the expected message
      real(wp) :: x
      character(200) :: error
      integer(ip) :: ptr
      x = 7.0_wp
      call nparsd(string, x, error, ptr)
      call check(ptr == at .and. index(error, what) > 0, 'nparsd error: "'//string//'" ('// &
                 trim(error)//')')
   end subroutine fails

end program test_nparsd
