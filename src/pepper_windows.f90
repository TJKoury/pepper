!> Windows and sets as plain arrays (PEPPER).
!>
!> PEPPER's routines that return a window (a time coverage, a GF search
!> result) give it as an array `intervals(2, n)`: each column is one
!> interval [start, stop], the intervals sorted and disjoint. Sets of ID
!> codes are sorted integer arrays without duplicates. Programs use them
!> directly (`size(intervals, 2)`, loops) and convert them to SPICELIB's
!> cells, whose first element is index -5, only to call the cell, set and
!> window routines (`ctx%wnunid`, `ctx%insrti`, ...):
!>
!>     real(wp), allocatable :: cover(:,:), cell(:)
!>     call ctx%spk_coverage('de440.bsp', 301, cover)
!>     call window_to_cell(cover, cell)
!>     call ctx%wncond(3600.0_wp, 3600.0_wp, cell)
!>     call cell_to_window(cell, cover)
!>
!> [[interval_list]] builds a window one interval at a time, as SPICELIB's
!> WNINSD does in a cell, without a size limit.
module pepper_windows

   use pepper_kinds, only: ip, wp

   implicit none

   private

   public :: interval_list, window_to_cell, cell_to_window, set_to_cell, cell_to_set, &
      append_id, sort_unique

   !> A window being built: intervals(:, 1:n) are sorted and disjoint. The
   !> array grows as intervals are inserted.
   type :: interval_list
      integer(ip) :: n = 0  !! the number of intervals
      real(wp), allocatable :: intervals(:,:)  !! the intervals, shape (2, capacity); columns 1 to n are used
   contains
      procedure :: insert => interval_list_insert
      procedure :: window => interval_list_window
   end type interval_list

contains

   !> Insert the interval [left, right] (left <= right) into a window: its
   !> union with the window, as SPICELIB's WNINSD (intervals that overlap or
   !> touch are merged).
   pure subroutine interval_list_insert(list, left, right)
      class(interval_list), intent(inout) :: list  !! the window
      real(wp), intent(in) :: left  !! the start of the interval
      real(wp), intent(in) :: right  !! the end of the interval

      real(wp), allocatable :: tmp(:,:)
      integer(ip) :: i, j, k
      logical :: new

      if (.not. allocated(list%intervals)) allocate (list%intervals(2, 16))

      ! The first interval whose end is not before LEFT.
      i = 1
      if (list%n > 0) then
         if (left > list%intervals(2, list%n)) i = list%n + 1
      end if
      do while (i <= list%n)
         if (list%intervals(2, i) >= left) exit
         i = i + 1
      end do

      new = i > list%n
      if (.not. new) new = right < list%intervals(1, i)
      if (new) then
         ! Disjoint from its neighbors: a new interval at position i.
         if (list%n == size(list%intervals, 2)) then
            allocate (tmp(2, 2*size(list%intervals, 2)))
            tmp(:, 1:list%n) = list%intervals(:, 1:list%n)
            call move_alloc(tmp, list%intervals)
         end if
         list%intervals(:, i + 1:list%n + 1) = list%intervals(:, i:list%n)
         list%intervals(:, i) = [left, right]
         list%n = list%n + 1
      else
         ! Merge with interval i and every later one that it reaches.
         list%intervals(1, i) = min(left, list%intervals(1, i))
         list%intervals(2, i) = max(right, list%intervals(2, i))
         j = i + 1
         do while (j <= list%n)
            if (list%intervals(1, j) > list%intervals(2, i)) exit
            list%intervals(2, i) = max(list%intervals(2, i), list%intervals(2, j))
            j = j + 1
         end do
         k = list%n - j + 1  ! intervals after the merged ones
         list%intervals(:, i + 1:i + k) = list%intervals(:, j:list%n)
         list%n = i + k
      end if
   end subroutine interval_list_insert

   !> The window as an array of shape (2, n).
   pure subroutine interval_list_window(list, intervals)
      class(interval_list), intent(in) :: list  !! the window
      real(wp), allocatable, intent(out) :: intervals(:,:)  !! its intervals
      if (list%n == 0) then
         allocate (intervals(2, 0))
      else
         intervals = list%intervals(:, 1:list%n)
      end if
   end subroutine interval_list_window

   !> A SPICELIB window cell (first index -5) holding the intervals of a
   !> window array.
   pure subroutine window_to_cell(intervals, cell, room)
      real(wp), intent(in) :: intervals(:,:)  !! the window, shape (2, n)
      real(wp), allocatable, intent(out) :: cell(:)  !! the cell, with bounds (-5:size)
      integer(ip), intent(in), optional :: room  !! the cell's size (at least 2n); default 2n
      integer(ip) :: n, sz
      n = 2*size(intervals, 2, ip)
      sz = n
      if (present(room)) sz = max(n, room)
      allocate (cell(-5:sz))
      cell = 0.0_wp
      cell(-1) = real(sz, wp)
      cell(0) = real(n, wp)
      cell(1:n) = reshape(intervals, [n])
   end subroutine window_to_cell

   !> The intervals of a SPICELIB window cell (first index -5) as an array.
   pure subroutine cell_to_window(cell, intervals)
      real(wp), intent(in) :: cell(-5:)  !! the cell
      real(wp), allocatable, intent(out) :: intervals(:,:)  !! the window, shape (2, n)
      integer(ip) :: n
      n = int(cell(0), ip)/2
      intervals = reshape(cell(1:2*n), [2_ip, n])
   end subroutine cell_to_window

   !> A SPICELIB integer set cell (first index -5) holding a set of IDs.
   pure subroutine set_to_cell(ids, cell, room)
      integer(ip), intent(in) :: ids(:)  !! the set: sorted, without duplicates
      integer(ip), allocatable, intent(out) :: cell(:)  !! the cell, with bounds (-5:size)
      integer(ip), intent(in), optional :: room  !! the cell's size (at least size(ids)); default size(ids)
      integer(ip) :: n, sz
      n = size(ids, kind=ip)
      sz = n
      if (present(room)) sz = max(n, room)
      allocate (cell(-5:sz))
      cell = 0
      cell(-1) = sz
      cell(0) = n
      cell(1:n) = ids
   end subroutine set_to_cell

   !> The elements of a SPICELIB integer cell (first index -5) as an array.
   pure subroutine cell_to_set(cell, ids)
      integer(ip), intent(in) :: cell(-5:)  !! the cell
      integer(ip), allocatable, intent(out) :: ids(:)  !! its elements
      ids = cell(1:cell(0))
   end subroutine cell_to_set

   !> Append an ID to an array that grows as needed: ids(1:n) are used.
   pure subroutine append_id(ids, n, id)
      integer(ip), allocatable, intent(inout) :: ids(:)  !! the IDs (allocated by the first call if needed)
      integer(ip), intent(inout) :: n  !! the number of IDs in use
      integer(ip), intent(in) :: id  !! the ID to append
      integer(ip), allocatable :: tmp(:)
      if (.not. allocated(ids)) allocate (ids(16))
      if (n == size(ids)) then
         allocate (tmp(2*size(ids)))
         tmp(1:n) = ids(1:n)
         call move_alloc(tmp, ids)
      end if
      n = n + 1
      ids(n) = id
   end subroutine append_id

   !> Sort an array of IDs and remove its duplicates.
   pure subroutine sort_unique(ids)
      integer(ip), allocatable, intent(inout) :: ids(:)  !! the IDs; on output, sorted and without duplicates
      integer(ip) :: i, j, n, gap, t
      n = size(ids, kind=ip)
      ! Shell sort (Ciura's gaps would do as well: these sets are small).
      gap = n/2
      do while (gap > 0)
         do i = gap + 1, n
            t = ids(i)
            j = i
            do while (j > gap)
               if (ids(j - gap) <= t) exit
               ids(j) = ids(j - gap)
               j = j - gap
            end do
            ids(j) = t
         end do
         gap = gap/2
      end do
      j = min(1_ip, n)
      do i = 2, n
         if (ids(i) /= ids(j)) then
            j = j + 1
            ids(j) = ids(i)
         end if
      end do
      ids = ids(1:j)
   end subroutine sort_unique

end module pepper_windows
