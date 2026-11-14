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
   call same('-0.0', '-0.0')  ! a zero (its sign: the compiler's)
   call same('0E5', '0E5')
   ! exact ties (decimal values halfway between two doubles): NPARSD rounds
   ! to even, as IEEE arithmetic does, whatever the compiler's READ does
   call value('730183456592122000.E-1', 73018345659212192.0_wp)
   call value('475699515413137000.E-1', 47569951541313696.0_wp)
   call check(tie('730183456592122000.E-1', 73018345659212192.0_wp, 73018345659212208.0_wp), &
              'tie: an exact midpoint')
   call check(.not. tie('730183456592122001', 73018345659212192.0_wp, 73018345659212208.0_wp), &
              'tie: not a midpoint')
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
      character(*), intent(in) :: string  !! the input of NPARSD
      character(*), intent(in) :: fortran  !! the same number for READ
      real(wp) :: x, y
      character(200) :: error
      integer(ip) :: ptr
      read (fortran, *) y
      call nparsd(string, x, error, ptr)
      call check(error == ' ' .and. same_bits(x, y), 'nparsd: '//string//' as READ, bit for bit')
   end subroutine same

   !> N random numbers (1 to 18 digits, a decimal point anywhere or none,
   !> leading and trailing zeros, exponents from -40 to 40 or none, signs):
   !> NPARSD's value is READ's, bit for bit.
   subroutine random_against_read(n)
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
         if (error /= ' ' .or. .not. (same_bits(x, y) .or. tie(trim(text), x, y))) then
            nbad = nbad + 1
            if (nbad <= 5) print '(3a, 2es26.17)', '   ', trim(text), ': nparsd, READ =', x, y
         end if
      end do
      call check(nbad == 0, 'nparsd = READ, bit for bit, for random numbers')
   end subroutine random_against_read

   !> X and Y are the same double bit for bit, but for the sign of a zero:
   !> compilers differ there (ifx's READ, and its fast floating-point
   !> model, give +0 for "-0", gfortran -0), and -0 == +0.
   logical function same_bits(x, y)
      use, intrinsic :: iso_fortran_env, only: int64
      real(wp), intent(in) :: x, y  !! the values
      same_bits = (x == 0.0_wp .and. y == 0.0_wp) .or. transfer(x, 0_int64) == transfer(y, 0_int64)
   end function same_bits

   !> TEXT, an integer, is exactly halfway between the adjacent doubles X
   !> and Y: compilers' READ may break the tie either way (the Fortran
   !> standard leaves it to them: ifx rounds away from zero, gfortran to
   !> even, as IEEE arithmetic and NPARSD's exact path do).
   logical function tie(text, x, y)
      use, intrinsic :: iso_fortran_env, only: int64
      character(*), intent(in) :: text  !! the number ([-]digits[.digits][E[+-]digits])
      real(wp), intent(in) :: x, y  !! the two values
      integer(int64) :: v, ix, iy
      integer :: i, k, ndec, e
      logical :: point
      character(len(text)) :: digits
      tie = .false.
      if (abs(x - y) /= spacing(max(abs(x), abs(y))) .or. abs(x) < 2.0_wp**53 .or. abs(x) > 1.0e18_wp) return
      ! the digits without sign and point, the digits after the point, the exponent
      digits = ' '
      k = 0
      ndec = 0
      e = 0
      point = .false.
      do i = 1, len(text)
         select case (text(i:i))
         case ('0':'9')
            k = k + 1
            digits(k:k) = text(i:i)
            if (point) ndec = ndec + 1
         case ('.')
            point = .true.
         case ('E', 'e')
            read (text(i + 1:), *) e
            exit
         end select
      end do
      e = e - ndec  ! the value is digits * 10**e
      do while (e < 0)  ! drop trailing zeros (an integer: they must cover e)
         if (k == 0) return
         if (digits(k:k) /= '0') return
         k = k - 1
         e = e + 1
      end do
      if (k + e > 18) return
      v = 0
      do i = 1, k
         v = 10*v + (iachar(digits(i:i)) - iachar('0'))
      end do
      v = v*10_int64**e
      ix = int(abs(x), int64)
      iy = int(abs(y), int64)
      tie = ix + iy == 2*v
   end function tie

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
