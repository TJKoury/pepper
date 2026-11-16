!> The data of SPK segments of types 9 and 13, and of CK segments of type
!> 5, as arrays (PEPPER; no SPICELIB counterpart).
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
!> A CK type 5 segment's data (CK Required Reading) is:
!>
!>     packets (PACKSZ per epoch), epochs, every 100th epoch (a directory),
!>     the start times of the interpolation intervals, every 100th of them,
!>     then five control words: the clock rate, the subtype, the window
!>     size, the number of intervals, the number of packets
!>
!> The procedures have no state and do no checks: the writers and the
!> builder check their inputs first.
module pepper_spk_packing

   use pepper_kinds, only: ip, wp

   implicit none

   private

   public :: spk09_data, spk13_data, spk_data_size, ck05_data

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

   !> The data of a CK segment of type 5 (CKW05's layout).
   pure function ck05_data(subtype, winsiz, epochs, packets, rate, starts) result(data)
      integer(ip), intent(in) :: subtype  !! the subtype (0 to 3)
      integer(ip), intent(in) :: winsiz  !! the interpolation window size
      real(wp), intent(in) :: epochs(:)  !! the epochs (n; encoded SCLK, or ET)
      real(wp), intent(in) :: packets(:)  !! the packets (packsz*n)
      real(wp), intent(in) :: rate  !! the clock rate (seconds per tick)
      real(wp), intent(in) :: starts(:)  !! the start times of the interpolation intervals
      real(wp), allocatable :: data(:)  !! the segment's data
      integer(ip) :: n, nints, k, i
      n = size(epochs, kind=ip)
      nints = size(starts, kind=ip)
      allocate (data(size(packets) + n + (n - 1)/dirsiz + nints + (nints - 1)/dirsiz + 5))
      k = size(packets, kind=ip)
      data(1:k) = packets
      data(k + 1:k + n) = epochs
      k = k + n
      do i = 1, (n - 1)/dirsiz
         data(k + i) = epochs(dirsiz*i)
      end do
      k = k + (n - 1)/dirsiz
      data(k + 1:k + nints) = starts
      k = k + nints
      do i = 1, (nints - 1)/dirsiz
         data(k + i) = starts(dirsiz*i)
      end do
      k = k + (nints - 1)/dirsiz
      data(k + 1:k + 5) = [rate, real(subtype, wp), real(winsiz, wp), real(nints, wp), real(n, wp)]
   end function ck05_data

end module pepper_spk_packing
