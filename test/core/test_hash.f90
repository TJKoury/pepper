!> pepper_hash: name_hash must give the same values as SPICELIB's ZZHASH2.
program test_hash

   use pepper_kinds, only: ip
   use pepper_hash, only: name_hash

   implicit none

   integer(ip), external :: zzhash2
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

end program test_hash
