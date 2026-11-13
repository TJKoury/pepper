!> Integers packed into double precision words (PEPPER): the one place in
!> the library where the bits of doubles are read as integers, or the bits
!> of integers stored as doubles.
!>
!> DAF summaries (segment descriptors) and DAF summary records hold their
!> integer components packed two to a double precision word, as SPICELIB
!> wrote them through EQUIVALENCE statements. Such a word is not a number:
!> as a real it is typically a denormal or a NaN, so it must only be
!> copied, never compared or used in arithmetic (denormals compare equal
!> to zero when they are flushed to zero, as is ifx's default; a NaN is not
!> equal even to itself). These functions convert between the two views
!> with TRANSFER, bit for bit.
module pepper_packing

   use pepper_kinds, only: ip, wp

   implicit none

   private

   public :: pack_integers, unpack_integers

contains

   !> The double precision words holding integers packed two to a word, in
   !> order (the first integer in the first half of the first word, in
   !> memory order); the second half of the last word is zero when the
   !> number of integers is odd.
   pure function pack_integers(ints) result(words)
      integer(ip), intent(in) :: ints(:)  !! the integers
      real(wp) :: words((size(ints) + 1)/2)  !! the words
      integer(ip) :: padded(2*size(words))
      padded(1:size(ints)) = ints
      padded(size(ints) + 1:) = 0
      words = transfer(padded, 0.0_wp, size(words))
   end function pack_integers

   !> The first N integers packed in double precision words, as
   !> [[pack_integers]] packs them. WORDS must hold at least N integers
   !> (`2*size(words) >= n`).
   pure function unpack_integers(words, n) result(ints)
      real(wp), intent(in) :: words(:)  !! the words
      integer(ip), intent(in) :: n  !! the number of integers
      integer(ip) :: ints(n)  !! the integers
      ints = transfer(words, 0_ip, n)
   end function unpack_integers

end module pepper_packing
