!> The in-place string routines of [[pepper_strings]] (ucase_inplace,
!> repmc_inplace, ...) must give the same results as the original routines
!> called with a separate output string of the same length, which is what
!> SPICELIB code gets when it passes one variable as both strings.
program test_strings_inplace

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context
   use pepper_strings, only: astrip, cmprss, lcase, nextwd, replch, repmc, ucase, zzinssub, &
      zzrepsub, astrip_inplace, cmprss_inplace, lcase_inplace, nextwd_inplace, repmc_inplace, &
      repmd_inplace, repmi_inplace, replch_inplace, ucase_inplace, zzinssub_inplace, &
      zzrepsub_inplace

   implicit none

   integer, parameter :: ntrials = 20000  ! random strings per routine
   character(*), parameter :: alphabet = '  aZ#@b(),.~' // achar(9)  ! characters of the random strings

   type(pepper_spice), target :: spice
   type(pepper_context) :: ctx
   integer :: failures = 0
   integer :: trial

   ctx = spice%new_context()

   do trial = 1, ntrials
      call check_all(random_string(), random_string(), random_int(-3, 20), random_int(-3, 20))
   end do

   ! Edge cases: empty strings and markers or values that are blank.
   call check_all('', '', 1, 0)
   call check_all('#', ' ', 1, 1)
   call check_all(' # ', '  ', 2, 1)
   call check_all('ab#cd', ' #  ', 3, 3)

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_strings_inplace: all checks passed'

contains

   !> Compare every in-place routine with its original for one input.
   subroutine check_all(str, other, i, j)
      character(*), intent(in) :: str  !! input string
      character(*), intent(in) :: other  !! second string (marker, value, inserted substring)
      integer, intent(in) :: i  !! first integer argument (position, count)
      integer, intent(in) :: j  !! second integer argument (position)

      character(len(str)) :: a  ! result of the in-place routine
      character(len(str)) :: b  ! result of the original routine
      character(len(str)) :: wa  ! word from nextwd_inplace
      character(len(str)) :: wb  ! word from nextwd
      character(:), allocatable :: marker  ! a marker taken from STR, so that it is found
      integer(ip) :: ival
      real(wp) :: dval

      a = str
      call ucase_inplace(a)
      call ucase(str, b)
      call check(a == b, 'ucase', str)

      a = str
      call lcase_inplace(a)
      call lcase(str, b)
      call check(a == b, 'lcase', str)

      a = str
      call replch_inplace(a, '#', '@')
      call replch(str, '#', '@', b)
      call check(a == b, 'replch', str)

      a = str
      call cmprss_inplace(' ', int(max(0, i/4), ip), a)
      call cmprss(' ', int(max(0, i/4), ip), str, b)
      call check(a == b, 'cmprss', str)

      a = str
      call astrip_inplace(a, 'Z', 'b')
      call astrip(str, 'Z', 'b', b)
      call check(a == b, 'astrip', str)

      a = str
      call nextwd_inplace(a, wa)
      call nextwd(str, wb, b)
      call check(a == b .and. wa == wb, 'nextwd', str)

      a = str
      b = str
      call zzinssub_inplace(a, other, int(i, ip))
      call zzinssub(str, other, int(i, ip), b)
      call check(a == b, 'zzinssub', str, other, i)

      ! zzrepsub leaves OUT undefined if RIGHT < LEFT-1, and reads before IN
      ! if LEFT < 1 or writes blanks before OUT if RIGHT > LEN(IN) (see
      ! spicelib_bugs.md).
      if (i >= 1 .and. j >= i - 1 .and. j <= len(str)) then
         a = str
         b = str
         call zzrepsub_inplace(a, int(i, ip), int(j, ip), other)
         call zzrepsub(str, int(i, ip), int(j, ip), other, b)
         call check(a == b, 'zzrepsub', str, other, i, j)
      end if

      ! Markers: one that may be absent (OTHER), and one taken from STR.
      marker = other
      if (len(str) > 0) marker = ' ' // str(min(len(str), max(1, i)):min(len(str), max(1, i) + 1)) // ' '

      a = str
      b = str
      call repmc_inplace(a, marker, other)
      call repmc(str, marker, other, b)
      call check(a == b, 'repmc', str, marker // '|' // other)

      a = str
      b = str
      call repmc_inplace(a, other, marker)
      call repmc(str, other, marker, b)
      call check(a == b, 'repmc (absent marker)', str, other)

      ival = int(i, ip)*int(j, ip)*1000003_ip - 77
      a = str
      b = str
      call repmi_inplace(a, marker, ival)
      call ctx%repmi(str, marker, ival, b)
      call check(a == b, 'repmi', str, marker)

      dval = real(i, wp)*1.234567e5_wp/real(j + 4, wp)
      a = str
      b = str
      call repmd_inplace(a, marker, dval, int(max(1, i), ip))
      call ctx%repmd(str, marker, dval, int(max(1, i), ip), b)
      call check(a == b, 'repmd', str, marker)
   end subroutine check_all

   !> A random string of 0 to 30 characters from ALPHABET.
   function random_string() result(s)
      character(:), allocatable :: s  !! the string
      integer :: k, n
      n = random_int(0, 30)
      allocate (character(n) :: s)
      do k = 1, len(s)
         s(k:k) = alphabet(random_int(1, len(alphabet)):)
      end do
   end function random_string

   !> A random integer in [lo, hi].
   integer function random_int(lo, hi)
      integer, intent(in) :: lo  !! smallest value
      integer, intent(in) :: hi  !! largest value
      real :: r
      call random_number(r)
      random_int = lo + min(hi - lo, int(r*(hi - lo + 1)))
   end function random_int

   !> Record a failed comparison.
   subroutine check(ok, what, str, other, i, j)
      logical, intent(in) :: ok  !! the results agree
      character(*), intent(in) :: what  !! the routine compared
      character(*), intent(in) :: str  !! the input string
      character(*), intent(in), optional :: other  !! the second string argument
      integer, intent(in), optional :: i  !! the first integer argument
      integer, intent(in), optional :: j  !! the second integer argument
      if (ok) return
      failures = failures + 1
      if (failures > 20) return
      print '(4a)', 'FAILED: ', what, ' "', str // '"'
      if (present(other)) print '(3a)', '        other "', other, '"'
      if (present(i)) print '(a, i0)', '        i ', i
      if (present(j)) print '(a, i0)', '        j ', j
   end subroutine check

end program test_strings_inplace
