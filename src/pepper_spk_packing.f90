!> The data of SPK segments of types 9 and 13, as arrays (PEPPER; no
!> SPICELIB counterpart).
!>
!> SPKW09 and SPKW13 write these arrays to an SPK file, and the kernel
!> builder ([[kernel_builder]]) puts them in a DAF made in memory, so the
!> two layouts cannot differ. A segment's data (SPK Required Reading,
!> types 9 and 13) is:
!>
!>     states (6 per epoch), epochs, every 100th epoch (a directory),
!>     then two control words:
!>        type 9:  the degree of the Lagrange polynomials, the number of states
!>        type 13: the window size (half the degree + 1) minus 1, the number of states
!>
!> The procedures have no state and do no checks: the writers and the
!> builder check their inputs first.
module pepper_spk_packing

   use pepper_kinds, only: ip, wp

   implicit none

   private

   public :: spk09_data, spk13_data, spk_data_size

   integer(ip), parameter :: dirsiz = 100  !! a directory entry every DIRSIZ epochs
   integer(ip), parameter :: statsz = 6  !! components of a state

contains

   !> The size of the data of a type 9 or 13 segment of `n` states.
   pure integer(ip) function spk_data_size(n) result(nwords)
      integer(ip), intent(in) :: n  !! the number of states
      nwords = statsz*n + n + (n - 1)/dirsiz + 2
   end function spk_data_size

   !> The data of an SPK segment of type 9 (Lagrange interpolation, unequal
   !> time steps).
   pure function spk09_data(states, epochs, degree) result(data)
      real(wp), intent(in) :: states(:, :)  !! the states (6, n)
      real(wp), intent(in) :: epochs(:)  !! their epochs (n)
      integer(ip), intent(in) :: degree  !! the degree of the Lagrange polynomials
      real(wp), allocatable :: data(:)  !! the segment's data
      call pack_segment(states, epochs, real(degree, wp), data)
   end function spk09_data

   !> The data of an SPK segment of type 13 (Hermite interpolation, unequal
   !> time steps).
   pure function spk13_data(states, epochs, degree) result(data)
      real(wp), intent(in) :: states(:, :)  !! the states (6, n)
      real(wp), intent(in) :: epochs(:)  !! their epochs (n)
      integer(ip), intent(in) :: degree  !! the degree of the Hermite polynomials (odd)
      real(wp), allocatable :: data(:)  !! the segment's data
      call pack_segment(states, epochs, real((degree + 1)/2 - 1, wp), data)
   end function spk13_data

   !> The layout shared by types 9 and 13; `control` is the first control
   !> word.
   pure subroutine pack_segment(states, epochs, control, data)
      real(wp), intent(in) :: states(:, :)  !! the states (6, n)
      real(wp), intent(in) :: epochs(:)  !! their epochs (n)
      real(wp), intent(in) :: control  !! the first control word
      real(wp), allocatable, intent(out) :: data(:)  !! the segment's data
      integer(ip) :: n, i, k
      n = size(epochs, kind=ip)
      allocate (data(spk_data_size(n)))
      data(1:statsz*n) = reshape(states(1:statsz, 1:n), [statsz*n])
      k = statsz*n
      data(k + 1:k + n) = epochs(1:n)
      k = k + n
      do i = 1, (n - 1)/dirsiz
         data(k + i) = epochs(dirsiz*i)
      end do
      k = k + (n - 1)/dirsiz
      data(k + 1) = control
      data(k + 2) = real(n, wp)
   end subroutine pack_segment

end module pepper_spk_packing
