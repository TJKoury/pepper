!> A kernel loader for the tests ([[kernel_loader]]), and its made-up file
!> format: a header of `KEY = value` lines, then blocks of rows
!> `et x y z vx vy vz` (TDB seconds past J2000, km, km/s):
!>
!>     SIMPLE_EPHEMERIS
!>     # a comment
!>     OBJECT = -10001              (a NAIF ID code, or a name)
!>     OBJECT_CODE = -10001         (optional: the code of OBJECT's name, which
!>                                   the kernel then defines: define_body)
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
!> A model instead of rows: `CIRCULAR = r w phase t0 t1` makes the loader
!> add a programmatic ephemeris (add_ephemeris: a circle of radius r km,
!> angular rate w rad/s, phase at ET 0, from t0 to t1), computed by
!> circle_orbit at any epoch, for the OBJECT, CENTER and FRAME set before.
!>
!> Frames: `ROTATING_FRAME = name code center base` starts a frame,
!> `ROTATION_INTERPOLATION = LAGRANGE 7` (or HERMITE n) sets its method, and
!> a block BEGIN_ROTATIONS ... END_ROTATIONS has rows `et q0 q1 q2 q3`
!> (the quaternion from the base to the frame), optionally followed by the
!> angular velocity (3 more numbers), a table of rotations
!> (frame_rotations). `SPIN_FRAME = name code center base rate` is a model:
!> a frame spinning about the base's Z axis at `rate` rad/s (spin_frame,
!> add_frame).
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
   use pepper_instance_module, only: kernel_loader, kernel_builder, pepper_context, ephemeris_provider, &
                                     frame_provider

   implicit none

   private

   public :: simple_loader, write_simple, circle_orbit, spin_frame, spin_matrix, last_head_len

   integer :: last_head_len = -1  !! the length of the head the last detect was given

   !> The model of the test format's SPIN_FRAME line: a frame turning about
   !> its base frame's Z axis, by `rate*et` (the evaluator a loader gives
   !> add_frame).
   type, extends(frame_provider) :: spin_frame
      real(wp) :: rate = 0.0_wp  !! the spin rate (rad/s)
      integer(ip) :: base = 1  !! the base frame's code (J2000)
   contains
      procedure :: transform => spin_frame_transform
   end type spin_frame

   !> The model of the test format's CIRCULAR line: a circle in the xy
   !> plane (the evaluator a loader gives add_ephemeris).
   type, extends(ephemeris_provider) :: circle_orbit
      real(wp) :: r = 0.0_wp  !! the radius (km)
      real(wp) :: w = 0.0_wp  !! the angular rate (rad/s)
      real(wp) :: phase = 0.0_wp  !! the phase at ET 0 (rad)
   contains
      procedure :: state => circle_orbit_state
   end type circle_orbit

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
      logical :: in_data, adapt, have_bounds, found, in_rot
      character(:), allocatable :: frame, objname
      character(64) :: fname, fbase
      character(16) :: rmethod
      integer(ip) :: fcode, fcenter, rdegree, ncol
      real(wp), allocatable :: rq(:, :), rav(:, :), rep(:)
      real(wp) :: rrow(8), srate

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
      objname = ' '
      method = ' '
      fname = ' '
      fbase = ' '
      rmethod = 'LAGRANGE'
      rdegree = 7
      fcode = 0
      fcenter = 0
      in_rot = .false.
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
         if (in_rot) then
            if (line == 'END_ROTATIONS') then
               call add_rotations()
               if (ctx%failed()) exit
               in_rot = .false.
               cycle
            end if
            if (nrow == 0) then
               ! the first row tells whether the rows have angular velocities
               read (line, *, iostat=ios) rrow(1:8)
               ncol = merge(8, 5, ios == 0)
            end if
            read (line, *, iostat=ios) rrow(1:ncol)
            if (ios /= 0) then
               call error('a rotation row is not 5 (or 8) numbers')
               exit
            end if
            nrow = nrow + 1
            if (nrow > size(rep)) call grow_rotations()
            rep(nrow) = rrow(1)
            rq(:, nrow) = rrow(2:5)
            if (ncol == 8) rav(:, nrow) = rrow(6:8)
            cycle
         end if
         if (line == 'BEGIN_ROTATIONS') then
            if (fname == ' ') then
               call error('BEGIN_ROTATIONS with no ROTATING_FRAME before it')
               exit
            end if
            in_rot = .true.
            nrow = 0
            allocate (rep(16), rq(4, 16), rav(3, 16))
            cycle
         end if
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
            else if (trim(key) == 'OBJECT') then
               ! a name: known already, or defined by the file (OBJECT_CODE)
               objname = trim(value)
               call ctx%bodn2c(objname, body, found)
               if (.not. found) body = 0
            else
               call ctx%bodn2c(trim(value), center, found)
               if (.not. found) then
                  call error('the body '//trim(value)//' is not known')
                  exit
               end if
            end if
         case ('OBJECT_CODE')
            read (value, *, iostat=ios) row(1)
            if (ios /= 0 .or. objname == ' ') then
               call error('OBJECT_CODE is not a code, or no OBJECT name precedes it')
               exit
            end if
            body = nint(row(1), ip)
            call kb%define_body(ctx, objname, body)
         case ('FRAME')
            frame = trim(value)
         case ('INTERPOLATION')
            read (value, *, iostat=ios) method, degree
            if (ios /= 0) then
               call error('INTERPOLATION is not a method and a degree')
               exit
            end if
         case ('CIRCULAR')
            read (value, *, iostat=ios) row(1:5)
            if (ios /= 0) then
               call error('CIRCULAR is not five numbers')
               exit
            end if
            if (body == 0) then
               call error('the body '//objname//' is not known (and no OBJECT_CODE defines it)')
               exit
            end if
            call kb%add_ephemeris(ctx, body, center, frame, row(4), row(5), &
                                  circle_orbit(r=row(1), w=row(2), phase=row(3)), ident='SIMPLE CIRCULAR')
         case ('ROTATING_FRAME')
            read (value, *, iostat=ios) fname, fcode, fcenter, fbase
            if (ios /= 0) then
               call error('ROTATING_FRAME is not a name, a code, a center and a base frame')
               exit
            end if
         case ('ROTATION_INTERPOLATION')
            read (value, *, iostat=ios) rmethod, rdegree
            if (ios /= 0) then
               call error('ROTATION_INTERPOLATION is not a method and a degree')
               exit
            end if
         case ('SPIN_FRAME')
            read (value, *, iostat=ios) fname, fcode, fcenter, fbase, srate
            if (ios /= 0) then
               call error('SPIN_FRAME is not a name, a code, a center, a base frame and a rate')
               exit
            end if
            block
               integer(ip) :: bcode
               call ctx%namfrm(trim(fbase), bcode)
               call kb%add_frame(ctx, trim(fname), fcode, fcenter, spin_frame(rate=srate, base=bcode))
            end block
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
      if (in_data .or. in_rot) then
         call error('the last block has no END_DATA or END_ROTATIONS')
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
         if (body == 0) then
            call error('the body '//objname//' is not known (and no OBJECT_CODE defines it)')
            return
         end if
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

      !> Add the block of rotations just read as a table of the frame.
      subroutine add_rotations()
         if (ncol == 8) then
            call kb%frame_rotations(ctx, trim(fname), fcode, fcenter, trim(fbase), rep(1:nrow), rq(:, 1:nrow), &
                                    rdegree, av=rav(:, 1:nrow), method=rmethod)
         else
            call kb%frame_rotations(ctx, trim(fname), fcode, fcenter, trim(fbase), rep(1:nrow), rq(:, 1:nrow), &
                                    rdegree, method=rmethod)
         end if
         deallocate (rep, rq, rav)
      end subroutine add_rotations

      !> Make room for more rotations.
      subroutine grow_rotations()
         real(wp), allocatable :: e(:), q(:, :), w(:, :)
         allocate (e(2*size(rep)), q(4, 2*size(rep)), w(3, 2*size(rep)))
         e(1:nrow - 1) = rep(1:nrow - 1)
         q(:, 1:nrow - 1) = rq(:, 1:nrow - 1)
         w(:, 1:nrow - 1) = rav(:, 1:nrow - 1)
         call move_alloc(e, rep)
         call move_alloc(q, rq)
         call move_alloc(w, rav)
      end subroutine grow_rotations

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

   !> The matrix from the base frame to the spinning frame at ET (a rotation
   !> of the axes by RATE*ET about Z: ROTATE's).
   pure function spin_matrix(rate, et) result(m)
      real(wp), intent(in) :: rate  !! the spin rate (rad/s)
      real(wp), intent(in) :: et  !! the epoch
      real(wp) :: m(3, 3)
      real(wp) :: c, s
      c = cos(rate*et)
      s = sin(rate*et)
      m = reshape([c, -s, 0.0_wp, s, c, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
   end function spin_matrix

   !> The state transformation from the spinning frame to its base:
   !> [R 0; dR/dt R], R the transpose of spin_matrix.
   subroutine spin_frame_transform(self, ctx, frame, et, xform, relative, found)
      class(spin_frame), intent(in) :: self  !! the model
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: frame  !! the frame's code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: xform(6, 6)  !! the transformation from the frame to RELATIVE
      integer(ip), intent(out) :: relative  !! the base frame
      logical, intent(out) :: found  !! always
      real(wp) :: r(3, 3), dr(3, 3), c, s
      c = cos(self%rate*et)
      s = sin(self%rate*et)
      r = transpose(spin_matrix(self%rate, et))
      dr = self%rate*reshape([-s, c, 0.0_wp, -c, -s, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp], [3, 3])
      xform = 0.0_wp
      xform(1:3, 1:3) = r
      xform(4:6, 4:6) = r
      xform(4:6, 1:3) = dr
      relative = self%base
      found = .not. ctx%failed() .and. frame /= 0
   end subroutine spin_frame_transform

   !> The state on the circle at ET.
   subroutine circle_orbit_state(self, ctx, body, et, state, found)
      class(circle_orbit), intent(in) :: self  !! the model
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state
      logical, intent(out) :: found  !! a state is available
      real(wp) :: a
      a = self%phase + self%w*et
      state = [self%r*cos(a), self%r*sin(a), 0.0_wp, -self%r*self%w*sin(a), self%r*self%w*cos(a), 0.0_wp]
      found = .not. ctx%failed() .and. body /= 0
   end subroutine circle_orbit_state

   !> A file of the test format starts with SIMPLE_EPHEMERIS.
   logical function simple_detect(self, file, head) result(accept)
      class(simple_loader), intent(in) :: self  !! the loader
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: head  !! its first bytes
      last_head_len = len(head)
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
