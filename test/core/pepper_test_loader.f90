!> A kernel loader for the tests ([[kernel_loader]]), and its made-up file
!> format: a header of `KEY = value` lines, then blocks of rows
!> `et x y z vx vy vz` (TDB seconds past J2000, km, km/s):
!>
!>     SIMPLE_EPHEMERIS
!>     # a comment
!>     OBJECT = -10001              (a NAIF ID code, or a name)
!>     CENTER = 399
!>     FRAME = J2000
!>     INTERPOLATION = HERMITE 7    (LAGRANGE n or HERMITE n)
!>     ADAPT = YES                  (optional: short blocks lower their degree)
!>     BOUNDS = 30.0 3570.0         (optional: the next block's coverage)
!>     BEGIN_DATA
!>     0.0  -6648.08  160.991  0.0  -0.17  -7.74  0.0
!>     ...
!>     END_DATA
!>
!> Each block is a segment; the header's keys may change between blocks
!> (a block uses the values set before it; BOUNDS applies to one block).
!> Blocks that share an epoch are a discontinuity (the later block wins
!> there); blocks with time between them leave a gap.
!>
!> The loader keeps the number of rows of each file it loaded (data an SPK
!> has no place for), and counts its loads and unloads.
module pepper_test_loader

   use pepper_kinds, only: ip, wp
   use pepper_containers, only: string
   use pepper_instance_module, only: kernel_loader, kernel_builder, pepper_context

   implicit none

   private

   public :: simple_loader, write_simple

   !> The loader of the test format.
   type, extends(kernel_loader) :: simple_loader
      logical :: sniff = .true.  !! detect accepts the files (else only by name or extension)
      integer :: loads = 0  !! files loaded
      integer :: unloads = 0  !! files unloaded
      type(string), allocatable :: files(:)  !! the files loaded (kept data)
      integer, allocatable :: rows(:)  !! their numbers of rows (kept data)
   contains
      procedure :: load => simple_load
      procedure :: detect => simple_detect
      procedure :: unload => simple_unload
      procedure :: rows_of => simple_rows_of
   end type simple_loader

contains

   !> Read a file of the test format into the builder.
   subroutine simple_load(self, ctx, kb, file)
      class(simple_loader), intent(inout) :: self  !! the loader
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      type(kernel_builder), intent(inout) :: kb  !! the builder
      character(*), intent(in) :: file  !! the file
      character(1024) :: line, key, value
      character(16) :: method
      real(wp), allocatable :: epochs(:), states(:, :)
      real(wp) :: row(7), bounds(2)
      integer(ip) :: body, center, degree, nline, nrow, total
      integer :: lun, ios, eq
      logical :: in_data, adapt, have_bounds, found
      character(:), allocatable :: frame

      open (newunit=lun, file=file, status='OLD', action='READ', iostat=ios)
      if (ios /= 0) then
         call ctx%setmsg('The file ''#'' could not be opened.')
         call ctx%errch('#', file)
         call ctx%sigerr('SPICE(FILEOPENFAILED)')
         return
      end if
      body = 0
      center = 0
      frame = ' '
      method = ' '
      degree = 0
      adapt = .false.
      have_bounds = .false.
      in_data = .false.
      nline = 0
      total = 0
      allocate (epochs(16), states(6, 16))
      nrow = 0
      do
         read (lun, '(a)', iostat=ios) line
         if (ios /= 0) exit
         nline = nline + 1
         if (nline == 1) then
            if (line /= 'SIMPLE_EPHEMERIS') then
               call error('the first line is not SIMPLE_EPHEMERIS')
               exit
            end if
            cycle
         end if
         line = adjustl(line)
         if (line == ' ' .or. line(1:1) == '#') cycle
         if (in_data) then
            if (line == 'END_DATA') then
               call add_block()
               if (ctx%failed()) exit
               in_data = .false.
               have_bounds = .false.
               cycle
            end if
            read (line, *, iostat=ios) row
            if (ios /= 0) then
               call error('a row is not 7 numbers')
               exit
            end if
            if (nrow == size(epochs)) call grow_rows()
            nrow = nrow + 1
            epochs(nrow) = row(1)
            states(:, nrow) = row(2:7)
            cycle
         end if
         if (line == 'BEGIN_DATA') then
            in_data = .true.
            nrow = 0
            cycle
         end if
         eq = index(line, '=')
         if (eq == 0) then
            call error('a header line is not KEY = value')
            exit
         end if
         key = adjustl(line(1:eq - 1))
         value = adjustl(line(eq + 1:))
         select case (trim(key))
         case ('OBJECT', 'CENTER')
            read (value, *, iostat=ios) row(1)
            if (ios == 0) then
               if (trim(key) == 'OBJECT') body = nint(row(1), ip)
               if (trim(key) == 'CENTER') center = nint(row(1), ip)
            else
               if (trim(key) == 'OBJECT') call ctx%bodn2c(trim(value), body, found)
               if (trim(key) == 'CENTER') call ctx%bodn2c(trim(value), center, found)
               if (.not. found) then
                  call error('the body '//trim(value)//' is not known')
                  exit
               end if
            end if
         case ('FRAME')
            frame = trim(value)
         case ('INTERPOLATION')
            read (value, *, iostat=ios) method, degree
            if (ios /= 0) then
               call error('INTERPOLATION is not a method and a degree')
               exit
            end if
         case ('ADAPT')
            adapt = trim(value) == 'YES'
         case ('BOUNDS')
            read (value, *, iostat=ios) bounds
            if (ios /= 0) then
               call error('BOUNDS is not two numbers')
               exit
            end if
            have_bounds = .true.
         case default
            call error('unknown key '//trim(key))
            exit
         end select
      end do
      close (lun)
      if (ctx%failed()) return
      if (in_data) then
         call error('the last block has no END_DATA')
         return
      end if
      call kb%set_format('SIMPLE_EPHEMERIS (test format)')
      self%loads = self%loads + 1
      call keep(trim(file), total)

   contains

      !> Signal an error at the current line.
      subroutine error(what)
         character(*), intent(in) :: what  !! what is wrong
         call ctx%setmsg('The file ''#'', line #: #.')
         call ctx%errch('#', file)
         call ctx%errint('#', nline)
         call ctx%errch('#', what)
         call ctx%sigerr('SPICE(BADSIMPLEFILE)')
      end subroutine error

      !> Add the block just read as a segment, with SPICE's interpolation
      !> the file names (any other method is an error).
      subroutine add_block()
         total = total + nrow
         select case (trim(method))
         case ('LAGRANGE')
            if (have_bounds) then
               call kb%spk_lagrange(ctx, body, center, frame, epochs(1:nrow), states(:, 1:nrow), degree, &
                                    tbeg=bounds(1), tend=bounds(2), ident='SIMPLE', adapt=adapt)
            else
               call kb%spk_lagrange(ctx, body, center, frame, epochs(1:nrow), states(:, 1:nrow), degree, &
                                    ident='SIMPLE', adapt=adapt)
            end if
         case ('HERMITE')
            if (have_bounds) then
               call kb%spk_hermite(ctx, body, center, frame, epochs(1:nrow), states(:, 1:nrow), degree, &
                                   tbeg=bounds(1), tend=bounds(2), ident='SIMPLE', adapt=adapt)
            else
               call kb%spk_hermite(ctx, body, center, frame, epochs(1:nrow), states(:, 1:nrow), degree, &
                                   ident='SIMPLE', adapt=adapt)
            end if
         case default
            call ctx%setmsg('The file ''#'', line #: the interpolation ''#'' is not one PEPPER supports '// &
                            '(LAGRANGE or HERMITE).')
            call ctx%errch('#', file)
            call ctx%errint('#', nline)
            call ctx%errch('#', trim(method))
            call ctx%sigerr('SPICE(UNSUPPORTEDINTERP)')
         end select
      end subroutine add_block

      !> Make room for more rows.
      subroutine grow_rows()
         real(wp), allocatable :: e(:), s(:, :)
         allocate (e(2*size(epochs)), s(6, 2*size(epochs)))
         e(1:nrow) = epochs(1:nrow)
         s(:, 1:nrow) = states(:, 1:nrow)
         call move_alloc(e, epochs)
         call move_alloc(s, states)
      end subroutine grow_rows

      !> Keep the number of rows of the file.
      subroutine keep(name, n)
         character(*), intent(in) :: name  !! the file
         integer(ip), intent(in) :: n  !! its rows
         type(string), allocatable :: f(:)
         if (.not. allocated(self%files)) then
            allocate (self%files(0), self%rows(0))
         end if
         allocate (f(size(self%files) + 1))
         f(1:size(self%files)) = self%files
         f(size(f))%s = name
         call move_alloc(f, self%files)
         self%rows = [self%rows, int(n)]
      end subroutine keep

   end subroutine simple_load

   !> A file of the test format starts with SIMPLE_EPHEMERIS.
   logical function simple_detect(self, file, head) result(accept)
      class(simple_loader), intent(in) :: self  !! the loader
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: head  !! its first bytes
      accept = self%sniff .and. len(file) > 0 .and. head(1:min(16, len(head))) == 'SIMPLE_EPHEMERIS'
   end function simple_detect

   !> Forget what was kept for an unloaded file.
   subroutine simple_unload(self, file)
      class(simple_loader), intent(inout) :: self  !! the loader
      character(*), intent(in) :: file  !! the file unloaded
      integer :: i
      self%unloads = self%unloads + 1
      if (.not. allocated(self%files)) return
      do i = size(self%files), 1, -1
         if (self%files(i)%s == file) then
            self%files = [self%files(1:i - 1), self%files(i + 1:)]
            self%rows = [self%rows(1:i - 1), self%rows(i + 1:)]
            return
         end if
      end do
   end subroutine simple_unload

   !> The rows kept for a file (-1 if none).
   integer function simple_rows_of(self, file) result(n)
      class(simple_loader), intent(in) :: self  !! the loader
      character(*), intent(in) :: file  !! the file
      integer :: i
      n = -1
      if (.not. allocated(self%files)) return
      do i = size(self%files), 1, -1
         if (self%files(i)%s == file) then
            n = self%rows(i)
            return
         end if
      end do
   end function simple_rows_of

   !> Write a file of the test format: the header lines, then blocks of
   !> rows (block b is rows first(b) to last(b); a block may start with
   !> its own header lines, `extra(b)`, e.g. a new BOUNDS). The numbers
   !> have 17 significant digits: they read back exactly.
   subroutine write_simple(file, header, epochs, states, first, last, extra)
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: header(:)  !! the header lines (after SIMPLE_EPHEMERIS)
      real(wp), intent(in) :: epochs(:)  !! the epochs of all the rows
      real(wp), intent(in) :: states(:, :)  !! their states
      integer, intent(in) :: first(:)  !! the first row of each block
      integer, intent(in) :: last(:)  !! the last row of each block
      character(*), intent(in), optional :: extra(:)  !! a header line before each block (blank: none)
      integer :: lun, b, k
      open (newunit=lun, file=file, status='REPLACE', action='WRITE')
      write (lun, '(a)') 'SIMPLE_EPHEMERIS'
      write (lun, '(a)') '# written by the PEPPER tests'
      do k = 1, size(header)
         write (lun, '(a)') trim(header(k))
      end do
      do b = 1, size(first)
         if (present(extra)) then
            if (extra(b) /= ' ') write (lun, '(a)') trim(extra(b))
         end if
         write (lun, '(a)') 'BEGIN_DATA'
         do k = first(b), last(b)
            write (lun, '(7es25.16e3)') epochs(k), states(:, k)
         end do
         write (lun, '(a)') 'END_DATA'
      end do
      close (lun)
   end subroutine write_simple

end module pepper_test_loader
