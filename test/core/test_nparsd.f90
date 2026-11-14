!> Unit tests of NPARSD: correctly rounded values (Fortran's READ, bit for
!> bit, also where NPARSD computes them exactly without READ), and
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
   ! the bounds of the exact path without READ (digits up to 2**53, powers
   ! of ten up to 10**22), and either side of them
   call same('9007199254740992', '9007199254740992')      ! 2**53
   call same('9007199254740993', '9007199254740993')      ! 2**53 + 1: READ
   call same('9007199254740992E22', '9007199254740992E22')
   call same('9007199254740992E23', '9007199254740992E23')
   call same('1E-22', '1E-22')
   call same('1E-23', '1E-23')
   call same('123.456E-20', '123.456E-20')
   call same('1500000000000000000000000000', '1500000000000000000000000000')  ! trailing zeros
   call same('0.000000000000000000000000001', '0.000000000000000000000000001')
   call same('-0.0', '-0.0')
   call same('0E5', '0E5')
   call random_against_read(200000)
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
      use, intrinsic :: iso_fortran_env, only: int64
      character(*), intent(in) :: string  !! the input of NPARSD
      character(*), intent(in) :: fortran  !! the same number for READ
      real(wp) :: x, y
      character(200) :: error
      integer(ip) :: ptr
      read (fortran, *) y
      call nparsd(string, x, error, ptr)
      call check(error == ' ' .and. transfer(x, 0_int64) == transfer(y, 0_int64), &
                 'nparsd: '//string//' as READ, bit for bit')
   end subroutine same

   !> N random numbers (1 to 18 digits, a decimal point anywhere or none,
   !> leading and trailing zeros, exponents from -40 to 40 or none, signs):
   !> NPARSD's value is READ's, bit for bit.
   subroutine random_against_read(n)
      use, intrinsic :: iso_fortran_env, only: int64
      integer, intent(in) :: n  !! how many numbers
      character(60) :: text
      character(200) :: error
      real(wp) :: x, y, r(8)
      integer(ip) :: ptr
      integer :: k, j, nd, nbad
      nbad = 0
      do k = 1, n
         call random_number(r)
         text = ' '
         if (r(1) < 0.3_wp) text = '-'
         nd = 1 + int(r(2)*18)
         do j = 1, nd
            call random_number(r(8))
            if (r(3) < 0.2_wp .and. j > nd - 3) then
               text = trim(text)//'0'                       ! trailing zeros
            else
               text = trim(text)//achar(iachar('0') + int(r(8)*10))
            end if
            if (r(4) < 0.7_wp .and. j == 1 + int(r(5)*nd)) text = trim(text)//'.'
         end do
         if (r(6) < 0.7_wp) then
            write (text(len_trim(text) + 1:), '(a, i0)') 'E', int(r(7)*81) - 40
         end if
         read (text, *) y
         call nparsd(trim(text), x, error, ptr)
         if (error /= ' ' .or. transfer(x, 0_int64) /= transfer(y, 0_int64)) then
            nbad = nbad + 1
            if (nbad <= 5) print '(3a, 2es26.17)', '   ', trim(text), ': nparsd, READ =', x, y
         end if
      end do
      call check(nbad == 0, 'nparsd = READ, bit for bit, for random numbers')
   end subroutine random_against_read

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
