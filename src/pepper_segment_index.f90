!> PEPPER: an index of the segments of the loaded SPK, PCK, CK or DSK files,
!> by body (or instrument), in search order.
!>
!> SPICELIB's segment buffers (SPKBSR, PCKBSR, CKBSR, ZZDSKBSR) find a
!> body's segments lazily: the first query of a body reads the segment
!> summaries of every loaded file (opening each), and each keeps fixed
!> tables of bodies and segments (10 MB per table for SPK and CK). Here the
!> segments are indexed once, when a file is loaded, in a structure of the
!> instance that the queries only read (kernels are loaded before queries
!> run). A context keeps only what makes repeated queries fast: the segment
!> returned last for each body and the interval in which it stays the
!> answer (`segment_reuse`), as SPICELIB did.
!>
!> The search order is SPICELIB's: the most recently loaded file first, and
!> within a file the last segment first. Each body's list holds its
!> segments in increasing priority (the last element is searched first), so
!> loading a file appends.
module pepper_segment_index

   use pepper_kinds, only: ip, wp
   use pepper_containers, only: int_map, grow, grown_capacity

   implicit none

   private

   !> Length of a DAF segment identifier (the name of a DAF summary of an
   !> SPK, PCK or CK file).
   integer(ip), parameter, public :: segid_length = 40

   !> The segments of one key (body or instrument), in increasing priority.
   type :: key_list
      integer(ip), allocatable :: seg(:)  !! the segments (slots of the index)
      integer(ip) :: n = 0  !! number of segments
   end type key_list

   !> The segments of the loaded files of one kind (SPK, PCK, CK or DSK).
   type, public :: segment_index
      integer(ip) :: ndescr = 0  !! number of reals in a segment's descriptor
      integer(ip) :: nidescr = 0  !! number of integers in a segment's descriptor
      integer(ip), allocatable :: files(:)  !! handles of the loaded files, in load order
      integer(ip) :: nfiles = 0  !! number of loaded files
      integer(ip), allocatable :: handle(:)  !! file of each segment (0: a free slot)
      integer(ip), allocatable :: key(:)  !! body (or instrument) of each segment
      real(wp), allocatable :: tbeg(:)  !! start of each segment's coverage
      real(wp), allocatable :: tend(:)  !! end of each segment's coverage
      real(wp), allocatable :: descr(:, :)  !! real descriptor of each segment (packed DAF summary, or DSK descriptor)
      integer(ip), allocatable :: idescr(:, :)  !! integer descriptor of each segment (DAF integer components, or DLA descriptor)
      character(segid_length), allocatable :: ident(:)  !! identifier of each segment (DAF kinds)
      integer(ip) :: nslots = 0  !! number of segment slots used (some may be free)
      integer(ip), allocatable :: free(:)  !! free segment slots
      integer(ip) :: nfree = 0  !! number of free slots
      type(int_map) :: keymap  !! list number of each key
      type(key_list), allocatable :: lists(:)  !! the segments of each key
      integer(ip) :: nlists = 0  !! number of lists
   contains
      procedure :: init => index_init
      procedure :: file_loaded => index_file_loaded
      procedure :: add_file => index_add_file
      procedure :: add => index_add
      procedure :: remove_file => index_remove_file
      procedure :: list => index_list
      procedure :: count => index_count
      procedure :: segment => index_segment
   end type segment_index

   !> The segment returned last for each key, and the interval in which it
   !> is still the answer (per context). Valid for one state of the index
   !> (its epoch).
   type, public :: segment_reuse
      integer(ip) :: epoch = -1  !! the segment_epoch of the instance these entries were made in
      type(int_map) :: keymap  !! entry of each key
      integer(ip), allocatable :: seg(:)  !! the segment returned last (0: none)
      real(wp), allocatable :: lb(:)  !! lower bound (exclusive) of the interval
      real(wp), allocatable :: ub(:)  !! upper bound (exclusive) of the interval
      integer(ip) :: n = 0  !! number of entries
   contains
      procedure :: reset => reuse_reset
      procedure :: find => reuse_find
      procedure :: store => reuse_store
      procedure :: forget => reuse_forget
   end type segment_reuse

contains

   !> Set the sizes of the descriptors, and empty the index.
   pure subroutine index_init(self, ndescr, nidescr)
      class(segment_index), intent(inout) :: self  !! the index
      integer(ip), intent(in) :: ndescr  !! reals in a segment's descriptor
      integer(ip), intent(in) :: nidescr  !! integers in a segment's descriptor
      self%ndescr = ndescr
      self%nidescr = nidescr
      self%nfiles = 0
      self%nslots = 0
      self%nfree = 0
      self%nlists = 0
      call self%keymap%clear()
   end subroutine index_init

   !> The file is loaded (in the index's file list).
   pure logical function index_file_loaded(self, handle) result(loaded)
      class(segment_index), intent(in) :: self  !! the index
      integer(ip), intent(in) :: handle  !! the file's handle
      loaded = .false.
      if (self%nfiles > 0) loaded = any(self%files(1:self%nfiles) == handle)
   end function index_file_loaded

   !> Record a file as loaded last (after its segments were added).
   pure subroutine index_add_file(self, handle)
      class(segment_index), intent(inout) :: self  !! the index
      integer(ip), intent(in) :: handle  !! the file's handle
      self%nfiles = self%nfiles + 1
      call grow(self%files, self%nfiles)
      self%files(self%nfiles) = handle
   end subroutine index_add_file

   !> Add a segment of the file being loaded (in the order of the file:
   !> each segment added has a higher priority than those before).
   pure subroutine index_add(self, handle, key, tbeg, tend, descr, idescr, ident)
      class(segment_index), intent(inout) :: self  !! the index
      integer(ip), intent(in) :: handle  !! the file's handle
      integer(ip), intent(in) :: key  !! the body (or instrument)
      real(wp), intent(in) :: tbeg  !! start of the coverage
      real(wp), intent(in) :: tend  !! end of the coverage
      real(wp), intent(in) :: descr(:)  !! the real descriptor (ndescr values)
      integer(ip), intent(in) :: idescr(:)  !! the integer descriptor (nidescr values)
      character(*), intent(in) :: ident  !! the identifier (blank for DSK)
      integer(ip) :: s, l
      logical :: found
      if (self%nfree > 0) then
         s = self%free(self%nfree)
         self%nfree = self%nfree - 1
      else
         self%nslots = self%nslots + 1
         s = self%nslots
         call grow(self%handle, s)
         call grow(self%key, s)
         call grow(self%tbeg, s)
         call grow(self%tend, s)
         call grow(self%descr, s, max(self%ndescr, 1_ip))
         call grow(self%idescr, s, max(self%nidescr, 1_ip))
         if (.not. allocated(self%ident)) allocate (self%ident(0))
         call grow(self%ident, s)
      end if
      self%handle(s) = handle
      self%key(s) = key
      self%tbeg(s) = tbeg
      self%tend(s) = tend
      if (self%ndescr > 0) self%descr(1:self%ndescr, s) = descr(1:self%ndescr)
      if (self%nidescr > 0) self%idescr(1:self%nidescr, s) = idescr(1:self%nidescr)
      self%ident(s) = ident
      call self%keymap%get(key, l, found)
      if (.not. found) then
         self%nlists = self%nlists + 1
         l = self%nlists
         call grow_lists(self%lists, l)
         self%lists(l)%n = 0
         call self%keymap%set(key, l)
      end if
      associate (lst => self%lists(l))
         lst%n = lst%n + 1
         call grow(lst%seg, lst%n)
         lst%seg(lst%n) = s
      end associate
   end subroutine index_add

   !> Remove a file and its segments from the index.
   pure subroutine index_remove_file(self, handle)
      class(segment_index), intent(inout) :: self  !! the index
      integer(ip), intent(in) :: handle  !! the file's handle
      integer(ip) :: i, j, s, l, n
      logical :: found
      logical, allocatable :: touched(:)
      ! The file.
      do i = 1, self%nfiles
         if (self%files(i) == handle) then
            self%files(i:self%nfiles - 1) = self%files(i + 1:self%nfiles)
            self%nfiles = self%nfiles - 1
            exit
         end if
      end do
      ! Its segments: free the slots, then drop them from the lists of
      ! their keys (each list once).
      allocate (touched(self%nlists))
      touched = .false.
      do s = 1, self%nslots
         if (self%handle(s) /= handle) cycle
         self%handle(s) = 0
         self%nfree = self%nfree + 1
         call grow(self%free, self%nfree)
         self%free(self%nfree) = s
         call self%keymap%get(self%key(s), l, found)
         if (found) touched(l) = .true.
      end do
      do l = 1, self%nlists
         if (.not. touched(l)) cycle
         associate (lst => self%lists(l))
            n = 0
            do j = 1, lst%n
               s = lst%seg(j)
               if (self%handle(s) /= 0) then
                  n = n + 1
                  lst%seg(n) = s
               end if
            end do
            lst%n = n
         end associate
      end do
   end subroutine index_remove_file

   !> The list number of a key (0 if it has no segments).
   pure integer(ip) function index_list(self, key) result(l)
      class(segment_index), intent(in) :: self  !! the index
      integer(ip), intent(in) :: key  !! the body (or instrument)
      logical :: found
      call self%keymap%get(key, l, found)
      if (.not. found) l = 0
   end function index_list

   !> The number of segments of a list (0 for list 0).
   pure integer(ip) function index_count(self, l) result(n)
      class(segment_index), intent(in) :: self  !! the index
      integer(ip), intent(in) :: l  !! the list number
      n = 0
      if (l > 0) n = self%lists(l)%n
   end function index_count

   !> The I-th segment of list L in increasing priority (I = count is
   !> searched first).
   pure integer(ip) function index_segment(self, l, i) result(s)
      class(segment_index), intent(in) :: self  !! the index
      integer(ip), intent(in) :: l  !! the list number
      integer(ip), intent(in) :: i  !! the position in the list
      s = self%lists(l)%seg(i)
   end function index_segment

   !> Grow an array of key lists, keeping its contents.
   pure subroutine grow_lists(a, needed)
      type(key_list), allocatable, intent(inout) :: a(:)  !! the lists
      integer(ip), intent(in) :: needed  !! the number of lists needed
      type(key_list), allocatable :: tmp(:)
      integer(ip) :: i, n
      if (.not. allocated(a)) then
         allocate (a(grown_capacity(0_ip, needed)))
         return
      end if
      n = size(a, kind=ip)
      if (n >= needed) return
      allocate (tmp(grown_capacity(n, needed)))
      do i = 1, n
         call move_alloc(a(i)%seg, tmp(i)%seg)
         tmp(i)%n = a(i)%n
      end do
      call move_alloc(tmp, a)
   end subroutine grow_lists

   !> Empty the cache, for a new state of the index.
   pure subroutine reuse_reset(self, epoch)
      class(segment_reuse), intent(inout) :: self  !! the cache
      integer(ip), intent(in) :: epoch  !! the instance's segment_epoch
      self%epoch = epoch
      self%n = 0
      call self%keymap%clear()
   end subroutine reuse_reset

   !> The entry of a key (0 if it has none).
   pure integer(ip) function reuse_find(self, key) result(e)
      class(segment_reuse), intent(in) :: self  !! the cache
      integer(ip), intent(in) :: key  !! the body (or instrument)
      logical :: found
      call self%keymap%get(key, e, found)
      if (.not. found) e = 0
   end function reuse_find

   !> Store the segment returned for a key and its re-use interval.
   pure subroutine reuse_store(self, key, seg, lb, ub)
      class(segment_reuse), intent(inout) :: self  !! the cache
      integer(ip), intent(in) :: key  !! the body (or instrument)
      integer(ip), intent(in) :: seg  !! the segment
      real(wp), intent(in) :: lb  !! lower bound (exclusive) of the interval
      real(wp), intent(in) :: ub  !! upper bound (exclusive) of the interval
      integer(ip) :: e
      logical :: found
      call self%keymap%get(key, e, found)
      if (.not. found) then
         self%n = self%n + 1
         e = self%n
         call grow(self%seg, e)
         call grow(self%lb, e)
         call grow(self%ub, e)
         call self%keymap%set(key, e)
      end if
      self%seg(e) = seg
      self%lb(e) = lb
      self%ub(e) = ub
   end subroutine reuse_store

   !> Forget the segment of an entry (its interval no longer applies).
   pure subroutine reuse_forget(self, e)
      class(segment_reuse), intent(inout) :: self  !! the cache
      integer(ip), intent(in) :: e  !! the entry
      if (e > 0) self%seg(e) = 0
   end subroutine reuse_forget

end module pepper_segment_index
