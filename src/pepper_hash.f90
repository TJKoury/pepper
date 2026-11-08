!> Hashing of names (kernel pool variables, ...).
!>
!> name_hash computes the same values as SPICELIB's ZZHASH2, without its
!> saved table and divisor: the value of a character is its position in
!> hash_chars (letters case-insensitive; other characters count 0), and a
!> blank ends the name.
module pepper_hash

   use pepper_kinds, only: ip

   implicit none

   private

   public :: name_hash

   integer(ip), parameter :: base = 68  !! number of characters with a value

   !> The characters with a value: the value is the position (1 to 68).
   character(68), parameter :: hash_chars = &
      '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-_./!@#$%^&*()+=[{]}|' // achar(92) // &
      ':;<,>?''"`~'

contains

   !> Hash value of a name, from 1 to divisor. The name ends at its first
   !> blank. divisor must be from 1 to huge(1_ip)/68 - 1 (not checked).
   pure integer(ip) function name_hash(word, divisor)
      character(*), intent(in) :: word  !! the name
      integer(ip), intent(in) :: divisor  !! number of hash values (size of the hash table)
      integer(ip) :: f, i, c
      f = 0
      do i = 1, len(word)
         if (word(i:i) == ' ') exit
         c = iachar(word(i:i))
         if (c >= iachar('a') .and. c <= iachar('z')) c = c - (iachar('a') - iachar('A'))
         if (c > 127) c = 0
         f = mod(index(hash_chars, achar(c)) + f*base, divisor)
      end do
      name_hash = mod(f*base, divisor) + 1
   end function name_hash

end module pepper_hash
