!> pepper_hash: name_hash must give the same values as SPICELIB's ZZHASH2
!> (a copy of whose algorithm is in this test).
program test_hash

   use pepper_kinds, only: ip
   use pepper_hash, only: name_hash

   implicit none

   character(40) :: words(12)
   integer(ip) :: divisors(4) = [1_ip, 7_ip, 26003_ip, 31580640_ip]
   integer :: i, j, failures = 0
   character(1) :: c

   words = [character(40) :: ' ', 'A', 'a', 'BODY399_RADII', 'body399_radii', 'Hakuna-Matata!', &
            'A A', '~`"''?>,<;:\|}]{[=+)(*&^%$#@!/._-', 'DELTET/DELTA_AT', 'X' // achar(200), &
            'FRAME_-82000_NAME', 'a very long kernel pool variable name']
   do i = 1, size(words)
      do j = 1, size(divisors)
         if (name_hash(trim(words(i)), divisors(j)) /= zzhash2(trim(words(i)), divisors(j))) then
            failures = failures + 1
            print '(3a,i0)', 'FAILED: "', trim(words(i)), '" divisor ', divisors(j)
         end if
      end do
   end do
   ! Every single character.
   do i = 0, 255
      c = achar(i)
      if (name_hash('Q' // c // 'Z', 26003_ip) /= zzhash2('Q' // c // 'Z', 26003_ip)) then
         failures = failures + 1
         print '(a,i0)', 'FAILED: character ', i
      end if
   end do
   if (failures > 0) stop 1
   print '(a)', 'test_hash: all checks passed'

contains

   !> SPICELIB's ZZHASH2 (N0067, in ZZPHSH), without its error checks: the
   !> hash of a word (up to its first blank) for a divisor.
   integer(ip) function zzhash2(word, m2)
      character(*), intent(in) :: word  !! the word
      integer(ip), intent(in) :: m2  !! the divisor
      character(*), parameter :: chars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-_./!@#$%^&*()+=[{]}|' // &
         achar(92) // ':;<,>?''"`~'
      integer(ip), parameter :: base = 68
      integer(ip) :: val(0:128), f, i
      val = 0
      do i = 1, len(chars)
         val(iachar(chars(i:i))) = i
      end do
      do i = iachar('a'), iachar('z')
         val(i) = val(i - 32)
      end do
      f = 0
      do i = 1, len(word)
         if (word(i:i) == ' ') exit
         f = val(min(128, iachar(word(i:i)))) + f*base
         f = mod(f, m2)
      end do
      zzhash2 = mod(f*base, m2) + 1
   end function zzhash2

end program test_hash
