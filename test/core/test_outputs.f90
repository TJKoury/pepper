!> Phase 7 (limits.md): the routines whose outputs PEPPER sizes. Each is
!> checked against its SPICELIB form, and past the size where that form
!> fails or truncates.
program test_outputs

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context, gf_scalar
   use pepper_containers, only: string
   use pepper_test_kernels, only: write_spk
   use pepper_windows, only: interval_list, window_to_cell, cell_to_window, set_to_cell, cell_to_set

   implicit none

   type(pepper_spice), target :: spice
   type(pepper_context), pointer :: ctx  !! the instance's own context
   integer :: failures = 0

   ctx => spice%context()

   call test_pool()
   call test_body_values()
   call test_fov()
   call test_names()
   call test_kernel_data()
   call test_interval_list()
   call test_converters()
   call test_spk_coverage()
   call test_ck_coverage()
   call test_pck_coverage()
   call test_dsk_objects()
   call test_comments()
   call test_dsk_data()
   call test_ek_values()
   call test_limb_terminator()
   call test_gf()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_outputs: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
      end if
   end subroutine check

   !> Print and clear the context's error, if any; true if there was none.
   logical function no_error(what)
      character(*), intent(in) :: what  !! what was called
      character(1840) :: msg
      no_error = .not. ctx%failed()
      if (.not. no_error) then
         call ctx%getmsg('SHORT', msg)
         print '(3a)', what, ': ', trim(msg)
         call ctx%getmsg('LONG', msg)
         print '(3a)', what, ': ', trim(msg)
         call ctx%reset()
      end if
   end function no_error

   !> The short error message, and reset the error state.
   function short_error() result(msg)
      character(40) :: msg  !! the short message
      call ctx%getmsg('SHORT', msg)
      call ctx%reset()
   end function short_error

   !> Write lines to a text kernel and load it.
   subroutine load_text_kernel(file, lines)
      character(*), intent(in) :: file  !! the file name
      character(*), intent(in) :: lines(:)  !! the lines
      integer :: unit, i
      open (newunit=unit, file=file, status='replace', action='write')
      write (unit, '(a)') '\begindata'
      do i = 1, size(lines)
         write (unit, '(a)') trim(lines(i))
      end do
      write (unit, '(a)') '\begintext'
      close (unit)
      call spice%furnsh(file)
   end subroutine load_text_kernel

   !> Delete a file.
   subroutine delete(file, missing_ok)
      character(*), intent(in) :: file  !! the file name
      logical, intent(in), optional :: missing_ok  !! no error if the file does not exist
      integer :: unit, iostat
      if (present(missing_ok)) then
         open (newunit=unit, file=file, status='old', iostat=iostat)
         if (iostat /= 0) return
      else
         open (newunit=unit, file=file, status='old')
      end if
      close (unit, status='delete')
   end subroutine delete

   !> pool_values, pool_integers, pool_strings against GDPOOL, GIPOOL,
   !> GCPOOL, on variables longer than a caller's buffer.
   subroutine test_pool()
      integer(ip), parameter :: n = 5000
      real(wp), allocatable :: d(:)
      real(wp) :: dbuf(n)
      integer(ip), allocatable :: iv(:)
      integer(ip) :: ibuf(n), k, m
      type(string), allocatable :: c(:)
      character(200) :: cbuf(3)
      character(:), allocatable :: long
      logical :: found

      long = repeat('abcdefghij', 20)
      call spice%pdpool('P7_D', n, [(real(k, wp) + 0.25_wp, k = 1, n)])
      call spice%pcpool('P7_C', 3_ip, [character(200) :: 'one', long, ''])
      call check(no_error('pdpool/pcpool'), 'pool setters')

      call ctx%pool_values('P7_D', d, found)
      call ctx%gdpool('P7_D', 1_ip, n, m, dbuf, found)
      call check(found .and. size(d) == n .and. m == n, 'pool_values: all 5000 values')
      call check(all(d == dbuf(1:n)), 'pool_values = gdpool')

      call ctx%pool_integers('P7_D', iv, found)
      call ctx%gipool('P7_D', 1_ip, n, m, ibuf, found)
      call check(found .and. size(iv) == n .and. all(iv == ibuf(1:n)), 'pool_integers = gipool')

      call ctx%pool_strings('P7_C', c, found)
      call ctx%gcpool('P7_C', 1_ip, 3_ip, m, cbuf, found)
      call check(found .and. size(c) == 3, 'pool_strings: 3 values')
      if (size(c) == 3) then
         call check(c(1)%s == 'one' .and. c(2)%s == long .and. len(c(2)%s) == 200, &
                    'pool_strings: whole values')
         call check(c(1)%s == cbuf(1) .and. c(2)%s == cbuf(2), 'pool_strings = gcpool')
      end if

      ! Not found, and the wrong type: size 0, no error.
      call ctx%pool_values('P7_NONE', d, found)
      call check(.not. found .and. size(d) == 0, 'pool_values: not found')
      call ctx%pool_values('P7_C', d, found)
      call check(.not. found .and. size(d) == 0, 'pool_values: a character variable')
      call ctx%pool_strings('P7_D', c, found)
      call check(.not. found .and. size(c) == 0, 'pool_strings: a numeric variable')
      call check(no_error('pool not found'), 'pool: no error when not found')

      ! An integer out of range: SPICE(INTOUTOFRANGE), as GIPOOL.
      call spice%pdpool('P7_BIG', 2_ip, [1.0_wp, 1.0e12_wp])
      call ctx%pool_integers('P7_BIG', iv, found)
      call check(short_error() == 'SPICE(INTOUTOFRANGE)' .and. size(iv) == 0, &
                 'pool_integers: out of range')
      call spice%clpool()
   end subroutine test_pool

   !> body_values by name and by code against BODVRD/BODVCD, on more values
   !> than a caller's buffer.
   subroutine test_body_values()
      character(*), parameter :: file = 'test_outputs_body.tpc'
      real(wp), allocatable :: v(:)
      real(wp) :: buf(3)
      integer(ip) :: dim, k
      character(80) :: lines(3)

      lines(1) = 'BODY399_RADII = ( 6378.1366 6378.1366 6356.7519 )'
      lines(2) = 'BODY399_P7_MANY = ( 1 2 3 4 5 6 7 8 9 10 11 12 )'
      lines(3) = "BODY399_P7_TEXT = 'abc'"
      call load_text_kernel(file, lines)
      call check(no_error('furnsh'), 'load the body constants')

      call ctx%body_values('EARTH', 'RADII', v)
      call ctx%bodvrd('EARTH', 'RADII', 3_ip, dim, buf)
      call check(size(v) == 3 .and. dim == 3 .and. all(v == buf), 'body_values(name) = bodvrd')
      call ctx%body_values(399_ip, 'RADII', v)
      call check(size(v) == 3 .and. all(v == buf), 'body_values(code) = bodvcd')
      call ctx%body_values('399', 'P7_MANY', v)
      call check(size(v) == 12 .and. all(v == [(real(k, wp), k = 1, 12)]), 'body_values: 12 values')
      call ctx%bodvcd(399_ip, 'P7_MANY', 3_ip, dim, buf)
      call check(short_error() == 'SPICE(ARRAYTOOSMALL)', 'bodvcd: array too small')

      ! The errors of BODVRD/BODVCD, with an empty result.
      call ctx%body_values('NO_SUCH_BODY', 'RADII', v)
      call check(short_error() == 'SPICE(NOTRANSLATION)' .and. size(v) == 0, 'body_values: unknown body')
      call ctx%body_values(399_ip, 'P7_NONE', v)
      call check(short_error() == 'SPICE(KERNELVARNOTFOUND)' .and. size(v) == 0, &
                 'body_values: no such item')
      call ctx%body_values(399_ip, 'P7_TEXT', v)
      call check(short_error() == 'SPICE(TYPEMISMATCH)' .and. size(v) == 0, 'body_values: not numeric')

      call spice%unload(file)
      call delete(file)
   end subroutine test_body_values

   !> fov by code and by name against GETFOV/GETFVN: a polygon with more
   !> boundary vectors than GETFOV's room, a frame name longer than its
   !> argument, and an FOV given by angles.
   subroutine test_fov()
      character(*), parameter :: file = 'test_outputs_fov.ti'
      integer(ip), parameter :: nv = 40
      character(*), parameter :: frame = 'A_FRAME_NAME_THAT_IS_LONGER_THAN_THIRTY_TWO_CHARACTERS'
      character(:), allocatable :: shape, frm
      character(20) :: shape20, frame20
      real(wp), allocatable :: bounds(:,:)
      real(wp) :: bsight(3), bs2(3), b2(3, nv)
      character(200) :: lines(15)
      integer(ip) :: k, n
      real(wp) :: a

      lines(1) = "INS-1001_FOV_FRAME = '" // frame // "'"
      lines(2) = "INS-1001_FOV_SHAPE = 'POLYGON'"
      lines(3) = 'INS-1001_BORESIGHT = ( 0 0 1 )'
      lines(4) = ''
      lines(5) = "INS-1002_FOV_FRAME = 'J2000'"
      lines(6) = "INS-1002_FOV_SHAPE = 'RECTANGLE'"
      lines(7) = 'INS-1002_BORESIGHT = ( 0 0 1 )'
      lines(8) = "INS-1002_FOV_CLASS_SPEC = 'ANGLES'"
      lines(9) = 'INS-1002_FOV_REF_VECTOR = ( 1 0 0 )'
      lines(10) = 'INS-1002_FOV_REF_ANGLE = 2'
      lines(11) = 'INS-1002_FOV_CROSS_ANGLE = 1'
      lines(12) = "INS-1002_FOV_ANGLE_UNITS = 'DEGREES'"
      lines(13) = "NAIF_BODY_NAME += 'P7_CAMERA'"
      lines(14) = 'NAIF_BODY_CODE += -1001'
      lines(15) = ''
      ! The corners take more than one line of 132 characters: write them
      ! as a continued list.
      block
         integer :: unit, i
         open (newunit=unit, file=file, status='replace', action='write')
         write (unit, '(a)') '\begindata'
         do i = 1, size(lines)
            write (unit, '(a)') trim(lines(i))
         end do
         write (unit, '(a)') 'INS-1001_FOV_BOUNDARY_CORNERS = ('
         do k = 1, nv
            a = 2*acos(-1.0_wp)*k/nv
            write (unit, '(3(1x,f0.6))') cos(a), sin(a), 10.0_wp
         end do
         write (unit, '(a)') ')', '\begintext'
         close (unit)
      end block
      call spice%furnsh(file)
      call check(no_error('furnsh'), 'load the IK')

      call ctx%fov(-1001_ip, shape, frm, bsight, bounds)
      call check(no_error('fov'), 'fov: no error')
      call check(shape == 'POLYGON' .and. frm == frame, 'fov: shape and the whole frame name')
      call check(size(bounds, 1) == 3 .and. size(bounds, 2) == nv, 'fov: all 40 vectors')
      call ctx%getfov(-1001_ip, nv, shape20, frame20, bs2, n, b2)
      call check(n == nv .and. all(bsight == bs2), 'getfov with room 40')
      if (size(bounds, 2) == nv) call check(all(bounds == b2), 'fov = getfov')
      call check(frame20 == frame(1:20), 'getfov cuts the frame name')
      call ctx%getfov(-1001_ip, 10_ip, shape20, frame20, bs2, n, b2)
      call check(short_error() == 'SPICE(BOUNDARYTOOBIG)', 'getfov: room 10 is too small')

      call ctx%fov('P7_CAMERA', shape, frm, bsight, bounds)
      call check(no_error('fov(name)') .and. size(bounds, 2) == nv, 'fov by name')

      ! An FOV given by angles: 4 vectors for a rectangle.
      call ctx%fov(-1002_ip, shape, frm, bsight, bounds)
      call ctx%getfov(-1002_ip, 4_ip, shape20, frame20, bs2, n, b2)
      call check(no_error('fov(angles)') .and. shape == 'RECTANGLE' .and. frm == 'J2000' .and. &
                 size(bounds, 2) == 4 .and. n == 4, 'fov: rectangle by angles')
      if (size(bounds, 2) == 4) call check(all(bounds == b2(:, 1:4)), 'fov = getfov (angles)')

      ! Errors: those of GETFOV/GETFVN, with empty results.
      call ctx%fov(-1003_ip, shape, frm, bsight, bounds)
      call check(short_error() == 'SPICE(FRAMEMISSING)' .and. size(bounds, 2) == 0 .and. &
                 shape == '' .and. frm == '', 'fov: no such instrument')
      call ctx%fov('NO_SUCH_CAMERA', shape, frm, bsight, bounds)
      call check(short_error() == 'SPICE(IDCODENOTFOUND)' .and. size(bounds, 2) == 0, &
                 'fov: unknown instrument name')

      call spice%unload(file)
      call delete(file)
   end subroutine test_fov

   !> body_name, frame_name, surface_name against BODC2N, FRMNAM, SRFC2S,
   !> with names longer than SPICELIB's 36 and 32 characters.
   subroutine test_names()
      character(*), parameter :: file = 'test_outputs_names.tk'
      character(*), parameter :: body = 'A BODY NAME LONGER THAN THIRTY-SIX CHARACTERS'
      character(*), parameter :: frame = 'A_FRAME_NAME_LONGER_THAN_THIRTY_TWO_CHARACTERS'
      character(*), parameter :: surface = 'A SURFACE NAME LONGER THAN THIRTY-SIX CHARACTERS'
      character(:), allocatable :: name
      character(36) :: name36
      character(200) :: lines(13)
      logical :: found, isname

      lines(1) = "NAIF_BODY_NAME += '" // body // "'"
      lines(2) = 'NAIF_BODY_CODE += 1234567'
      lines(3) = 'FRAME_' // frame // ' = 1400001'
      lines(4) = "FRAME_1400001_NAME = '" // frame // "'"
      lines(5) = 'FRAME_1400001_CLASS = 4'
      lines(6) = 'FRAME_1400001_CLASS_ID = 1400001'
      lines(7) = 'FRAME_1400001_CENTER = 399'
      lines(8) = 'TKFRAME_1400001_RELATIVE = ''J2000'''
      lines(9) = "TKFRAME_1400001_SPEC = 'MATRIX'"
      lines(10) = 'TKFRAME_1400001_MATRIX = ( 1 0 0 0 1 0 0 0 1 )'
      lines(11) = "NAIF_SURFACE_NAME += '" // surface // "'"
      lines(12) = 'NAIF_SURFACE_CODE += 7'
      lines(13) = 'NAIF_SURFACE_BODY += 499'
      call load_text_kernel(file, lines)
      call check(no_error('furnsh'), 'load the names')

      call ctx%body_name(1234567_ip, name, found)
      call ctx%bodc2n(1234567_ip, name36, found)
      call check(found .and. name == body .and. name36 == body(1:36), 'body_name: whole name; bodc2n cuts it')
      call ctx%body_name(399_ip, name, found)
      call check(found .and. name == 'EARTH', 'body_name: built-in')
      call ctx%body_name(-77777_ip, name, found)
      call check(.not. found .and. name == '', 'body_name: no name')

      call ctx%frame_name(1400001_ip, name, found)
      call ctx%frmnam(1400001_ip, name36)
      call check(found .and. name == frame .and. name36 == frame(1:36), 'frame_name: whole name')
      call ctx%frame_name(1_ip, name, found)
      call check(found .and. name == 'J2000', 'frame_name: built-in')
      call ctx%frame_name(-77777_ip, name, found)
      call check(.not. found .and. name == '', 'frame_name: unknown frame')

      call ctx%surface_name(7_ip, 499_ip, name, found)
      call ctx%srfc2s(7_ip, 499_ip, name36, isname)
      call check(found .and. isname .and. name == surface .and. name36 == surface(1:36), &
                 'surface_name: whole name; srfc2s cuts it')
      call ctx%surface_name(8_ip, 499_ip, name, found)
      call ctx%srfc2s(8_ip, 499_ip, name36, isname)
      call check(.not. found .and. name == '' .and. .not. isname .and. name36 == '8', &
                 'surface_name: no name (srfc2s gives the code)')
      call check(no_error('names'), 'names: no error')

      call spice%unload(file)
      call delete(file)
   end subroutine test_names

   !> kernel_data and kernel_info against KDATA and KINFO, with a file
   !> name longer than the caller's buffer.
   subroutine test_kernel_data()
      character(*), parameter :: file = 'test_outputs_a_kernel_with_a_rather_long_file_name.tk'
      character(*), parameter :: meta = 'test_outputs_meta.tm'
      character(:), allocatable :: f, t, s
      character(20) :: f20, t20, s20
      integer(ip) :: handle, h20, count
      logical :: found
      integer :: unit

      open (newunit=unit, file=file, status='replace', action='write')
      write (unit, '(a)') '\begindata', 'P7_X = 1', '\begintext'
      close (unit)
      open (newunit=unit, file=meta, status='replace', action='write')
      write (unit, '(a)') '\begindata', "KERNELS_TO_LOAD = ( '" // file // "' )", '\begintext'
      close (unit)
      call spice%kclear()
      call spice%furnsh(meta)
      call check(no_error('furnsh'), 'load the meta-kernel')
      call ctx%ktotal('ALL', count)
      call check(count == 2, 'two kernels')

      call ctx%kernel_data(2_ip, 'ALL', f, t, s, handle, found)
      call ctx%kdata(2_ip, 'ALL', f20, t20, s20, h20, found)
      call check(found .and. f == file .and. t == 'TEXT' .and. s == meta .and. handle == 0, &
                 'kernel_data: whole names')
      call check(f20 == file(1:20) .and. s20 == meta(1:20), 'kdata cuts the names')
      call ctx%kernel_data(1_ip, 'META', f, t, s, handle, found)
      call check(found .and. f == meta .and. t == 'META' .and. s == '', 'kernel_data: the meta-kernel')
      call ctx%kernel_data(3_ip, 'ALL', f, t, s, handle, found)
      call check(.not. found .and. f == '' .and. t == '' .and. s == '', 'kernel_data: no 3rd kernel')

      call ctx%kernel_info(file, t, s, handle, found)
      call check(found .and. t == 'TEXT' .and. s == meta, 'kernel_info: whole names')
      call ctx%kernel_info('not_loaded.tk', t, s, handle, found)
      call check(.not. found .and. t == '' .and. s == '', 'kernel_info: not loaded')
      call check(no_error('kernel data'), 'kernel data: no error')

      call spice%kclear()
      call delete(file)
      call delete(meta)
   end subroutine test_kernel_data

   !> interval_list builds the same windows as WNINSD: random intervals,
   !> some overlapping, some touching.
   subroutine test_interval_list()
      integer, parameter :: n = 3000
      type(interval_list) :: list
      real(wp), allocatable :: cell(:), w(:,:)
      real(wp) :: r(2), a, b
      integer :: k
      allocate (cell(-5:2*n))
      call ctx%ssized(int(2*n, ip), cell)
      do k = 1, n
         call random_number(r)
         a = real(floor(r(1)*100000), wp)
         b = a + real(floor(r(2)*40), wp)  ! often touching or overlapping (integer endpoints)
         call list%insert(a, b)
         call ctx%wninsd(a, b, cell)
      end do
      call list%window(w)
      call check(no_error('wninsd'), 'interval_list: wninsd ran')
      call check(size(w, 2) == ctx%cardd(cell)/2, 'interval_list: as many intervals as wninsd')
      if (size(w, 2) == ctx%cardd(cell)/2) &
         call check(all(reshape(w, [2*size(w, 2)]) == cell(1:ctx%cardd(cell))), 'interval_list = wninsd')
      call check(all(w(1, :) <= w(2, :)), 'interval_list: start <= stop')
      call check(all(w(1, 2:) > w(2, :size(w, 2) - 1)), 'interval_list: sorted and disjoint')
      ! a few by hand
      list = interval_list()
      call list%insert(5.0_wp, 6.0_wp)
      call list%insert(1.0_wp, 2.0_wp)
      call list%insert(2.0_wp, 3.0_wp)   ! touches [1, 2]
      call list%insert(10.0_wp, 10.0_wp) ! a singleton
      call list%insert(0.0_wp, 5.5_wp)   ! covers [1, 3] and reaches [5, 6]
      call list%window(w)
      call check(size(w, 2) == 2, 'interval_list: by hand, 2 intervals')
      if (size(w, 2) == 2) call check(all(w == reshape([0.0_wp, 6.0_wp, 10.0_wp, 10.0_wp], [2, 2])), &
                                      'interval_list: by hand, the intervals')
      list = interval_list()
      call list%window(w)
      call check(size(w, 1) == 2 .and. size(w, 2) == 0, 'interval_list: empty window')
   end subroutine test_interval_list

   !> window_to_cell, cell_to_window, set_to_cell, cell_to_set, and the
   !> cells working with the SPICELIB window and set routines.
   subroutine test_converters()
      real(wp) :: w(2, 3)
      real(wp), allocatable :: cell(:), w2(:,:)
      integer(ip), allocatable :: icell(:), ids(:)
      w = reshape([1.0_wp, 2.0_wp, 4.0_wp, 5.0_wp, 7.0_wp, 9.0_wp], [2, 3])
      call window_to_cell(w, cell, room=20_ip)
      call check(lbound(cell, 1) == -5 .and. ctx%sized(cell) == 20 .and. ctx%cardd(cell) == 6, &
                 'window_to_cell: size and cardinality')
      call ctx%wninsd(2.0_wp, 4.0_wp, cell)  ! joins the first two
      call check(no_error('wninsd') .and. ctx%wncard(cell) == 2, 'window_to_cell: a valid window')
      call cell_to_window(cell, w2)
      call check(size(w2, 2) == 2, 'cell_to_window: 2 intervals')
      if (size(w2, 2) == 2) call check(all(w2 == reshape([1.0_wp, 5.0_wp, 7.0_wp, 9.0_wp], [2, 2])), &
                                       'cell_to_window: the intervals')
      call set_to_cell([2_ip, 3_ip, 5_ip], icell, room=10_ip)
      call ctx%insrti(4_ip, icell)
      call check(no_error('insrti') .and. ctx%cardi(icell) == 4, 'set_to_cell: a valid set')
      call cell_to_set(icell, ids)
      call check(all(ids == [2_ip, 3_ip, 4_ip, 5_ip]), 'cell_to_set')
   end subroutine test_converters

   !> spk_coverage and spk_objects against SPKCOV and SPKOBJ, with more
   !> intervals than a small window holds.
   subroutine test_spk_coverage()
      character(*), parameter :: file = 'test_outputs.bsp'
      integer(ip), parameter :: nseg = 300
      real(wp), allocatable :: cover(:,:), cell(:), w(:,:)
      integer(ip), allocatable :: ids(:), icell(:)
      real(wp) :: states(6, 2)
      integer(ip) :: h, i
      states = 0
      states(1, :) = 1.0e5_wp
      call delete(file, missing_ok=.true.)
      call ctx%spkopn(file, 'test_outputs', 0_ip, h)
      do i = 1, nseg
         call ctx%spkw08(h, 1000_ip, 399_ip, 'J2000', 100.0_wp*i, 100.0_wp*i + 50, 'SEG', 1_ip, 2_ip, &
                         states, 100.0_wp*i, 50.0_wp)
      end do
      ! overlaps [200, 250] and touches [100, 150]: one interval [100, 260]
      call ctx%spkw08(h, 1000_ip, 399_ip, 'J2000', 150.0_wp, 260.0_wp, 'SEG', 1_ip, 2_ip, &
                      states, 150.0_wp, 110.0_wp)
      call ctx%spkw08(h, 2000_ip, 399_ip, 'J2000', 0.0_wp, 1.0_wp, 'SEG', 1_ip, 2_ip, states, 0.0_wp, 1.0_wp)
      call ctx%spkcls(h)
      call check(no_error('write SPK'), 'write the SPK')

      call ctx%spk_coverage(file, 1000_ip, cover)
      call check(no_error('spk_coverage') .and. size(cover, 2) == nseg - 1, 'spk_coverage: 299 intervals')
      if (size(cover, 2) == nseg - 1) then
         call check(all(cover(:, 1) == [100.0_wp, 260.0_wp]) .and. &
                    all(cover(:, nseg - 1) == [100.0_wp*nseg, 100.0_wp*nseg + 50]), 'spk_coverage: the intervals')
         allocate (cell(-5:1000))
         call ctx%ssized(1000_ip, cell)
         call ctx%spkcov(file, 1000_ip, cell)
         call cell_to_window(cell, w)
         call check(size(w, 2) == nseg - 1, 'spkcov: the same count')
         if (size(w, 2) == nseg - 1) call check(all(w == cover), 'spk_coverage = spkcov')
      end if
      ! SPKCOV adds to the window it is given.
      call window_to_cell(reshape([-10.0_wp, 0.5_wp], [2, 1]), cell, room=10_ip)
      call ctx%spkcov(file, 2000_ip, cell)
      call cell_to_window(cell, w)
      call check(no_error('spkcov') .and. size(w, 2) == 1, 'spkcov: union with the cell')
      if (size(w, 2) == 1) call check(all(w(:, 1) == [-10.0_wp, 1.0_wp]), 'spkcov: union with the cell (values)')
      call window_to_cell(reshape([real(wp) ::], [2, 0]), cell, room=20_ip)
      call ctx%spkcov(file, 1000_ip, cell)
      call check(short_error() == 'SPICE(WINDOWEXCESS)', 'spkcov: a window of 10 intervals is too small')

      call ctx%spk_objects(file, ids)
      call check(size(ids) == 2, 'spk_objects: 2 objects')
      if (size(ids) == 2) call check(all(ids == [1000_ip, 2000_ip]), 'spk_objects: the IDs')
      call set_to_cell([integer(ip) ::], icell, room=10_ip)
      call ctx%spkobj(file, icell)
      call check(ctx%cardi(icell) == 2 .and. all(icell(1:2) == ids), 'spk_objects = spkobj')

      ! Errors: those of SPKCOV, with an empty result.
      call ctx%spk_coverage('no_such_file.bsp', 1000_ip, cover)
      call check(short_error() /= ' ' .and. size(cover, 2) == 0, 'spk_coverage: no file')
      call delete(file)
   end subroutine test_spk_coverage

   !> ck_coverage and ck_objects against CKCOV and CKOBJ: a segment of 300
   !> pointing intervals (level INTERVAL), in ticks.
   subroutine test_ck_coverage()
      character(*), parameter :: file = 'test_outputs.bc'
      integer(ip), parameter :: nrec = 300, inst = -77001
      real(wp), allocatable :: cover(:,:), cell(:), w(:,:)
      integer(ip), allocatable :: ids(:), icell(:)
      real(wp) :: start(nrec), stop(nrec), quats(4, nrec), avvs(3, nrec), rates(nrec)
      integer(ip) :: h, k
      do k = 1, nrec
         start(k) = 100.0_wp*k
         stop(k) = 100.0_wp*k + 50
      end do
      quats = 0
      quats(1, :) = 1
      avvs = 0
      rates = 1
      call delete(file, missing_ok=.true.)
      call ctx%ckopn(file, 'test_outputs', 0_ip, h)
      call ctx%ckw02(h, start(1), stop(nrec), inst, 'J2000', 'SEG', nrec, start, stop, quats, avvs, rates)
      call ctx%ckw02(h, 0.0_wp, 10.0_wp, inst - 1, 'J2000', 'SEG', 1_ip, [0.0_wp], [10.0_wp], &
                     quats(:, 1:1), avvs(:, 1:1), rates(1:1))
      call ctx%ckcls(h)
      call check(no_error('write CK'), 'write the CK')

      call ctx%ck_coverage(file, inst, .false., 'INTERVAL', 0.0_wp, 'SCLK', cover)
      call check(no_error('ck_coverage') .and. size(cover, 2) == nrec, 'ck_coverage: 300 intervals')
      allocate (cell(-5:1000))
      call ctx%ssized(1000_ip, cell)
      call ctx%ckcov(file, inst, .false., 'INTERVAL', 0.0_wp, 'SCLK', cell)
      call cell_to_window(cell, w)
      if (size(cover, 2) == nrec .and. size(w, 2) == nrec) then
         call check(all(cover == w) .and. all(cover(1, :) == start) .and. all(cover(2, :) == stop), &
                    'ck_coverage = ckcov (INTERVAL)')
      end if
      call ctx%ck_coverage(file, inst, .false., 'SEGMENT', 0.0_wp, 'SCLK', cover)
      call check(size(cover, 2) == 1, 'ck_coverage: SEGMENT level')
      if (size(cover, 2) == 1) call check(all(cover(:, 1) == [start(1), stop(nrec)]), 'ck_coverage: the segment')
      ! tolerance: intervals 50 apart are joined by a tolerance of 25 ticks
      call ctx%ck_coverage(file, inst, .false., 'INTERVAL', 25.0_wp, 'SCLK', cover)
      call check(size(cover, 2) == 1, 'ck_coverage: tolerance joins the intervals')
      call window_to_cell(reshape([real(wp) ::], [2, 0]), cell, room=100_ip)
      call ctx%ckcov(file, inst, .false., 'INTERVAL', 0.0_wp, 'SCLK', cell)
      call check(short_error() == 'SPICE(WINDOWEXCESS)', 'ckcov: a window of 50 intervals is too small')
      call ctx%ck_coverage(file, inst, .false., 'BAD', 0.0_wp, 'SCLK', cover)
      call check(short_error() == 'SPICE(INVALIDOPTION)' .and. size(cover, 2) == 0, 'ck_coverage: bad level')

      call ctx%ck_objects(file, ids)
      call set_to_cell([integer(ip) ::], icell, room=10_ip)
      call ctx%ckobj(file, icell)
      call check(size(ids) == 2 .and. ctx%cardi(icell) == 2, 'ck_objects: 2 objects')
      if (size(ids) == 2) call check(all(ids == [inst - 1, inst]) .and. all(icell(1:2) == ids), &
                                     'ck_objects = ckobj')
      call delete(file)
   end subroutine test_ck_coverage

   !> pck_coverage and pck_frames against PCKCOV and PCKFRM.
   subroutine test_pck_coverage()
      character(*), parameter :: file = 'test_outputs.bpc'
      real(wp), allocatable :: cover(:,:), cell(:), w(:,:)
      integer(ip), allocatable :: ids(:), icell(:)
      real(wp) :: cdata(3, 3)
      integer(ip) :: h, k
      cdata = 0
      call delete(file, missing_ok=.true.)
      call ctx%pckopn(file, 'test_outputs', 0_ip, h)
      do k = 1, 3
         call ctx%pckw02(h, 3000_ip, 'J2000', 1000.0_wp*k, 1000.0_wp*k + 100, 'SEG', 100.0_wp, 1_ip, 2_ip, &
                         cdata, 1000.0_wp*k)
      end do
      call ctx%pckw02(h, 3001_ip, 'J2000', 0.0_wp, 100.0_wp, 'SEG', 100.0_wp, 1_ip, 2_ip, cdata, 0.0_wp)
      call ctx%pckcls(h)
      call check(no_error('write PCK'), 'write the PCK')
      call ctx%pck_coverage(file, 3000_ip, cover)
      allocate (cell(-5:100))
      call ctx%ssized(100_ip, cell)
      call ctx%pckcov(file, 3000_ip, cell)
      call cell_to_window(cell, w)
      call check(no_error('pck_coverage') .and. size(cover, 2) == 3 .and. size(w, 2) == 3, &
                 'pck_coverage: 3 intervals')
      if (size(cover, 2) == 3 .and. size(w, 2) == 3) call check(all(cover == w), 'pck_coverage = pckcov')
      call ctx%pck_frames(file, ids)
      call set_to_cell([integer(ip) ::], icell, room=10_ip)
      call ctx%pckfrm(file, icell)
      call check(size(ids) == 2 .and. ctx%cardi(icell) == 2, 'pck_frames: 2 frames')
      if (size(ids) == 2) call check(all(ids == [3000_ip, 3001_ip]) .and. all(icell(1:2) == ids), &
                                     'pck_frames = pckfrm')
      call delete(file)
   end subroutine test_pck_coverage

   !> dsk_objects and dsk_surfaces against DSKOBJ and DSKSRF, with more
   !> surfaces than a small set holds.
   subroutine test_dsk_objects()
      character(*), parameter :: file = 'test_outputs.bds'
      integer(ip), parameter :: nv = 6, np = 8, nsurf = 30
      real(wp), parameter :: pi = acos(-1.0_wp), r = 1000.0_wp
      real(wp) :: vrtces(3, nv), spaixd(10), corpar(10), mncor3, mxcor3
      integer(ip) :: plates(3, np), handle, i
      integer(ip), allocatable :: work(:, :), spaixi(:), ids(:), icell(:)
      vrtces = reshape([r, 0.0_wp, 0.0_wp, -r, 0.0_wp, 0.0_wp, 0.0_wp, r, 0.0_wp, &
                        0.0_wp, -r, 0.0_wp, 0.0_wp, 0.0_wp, r, 0.0_wp, 0.0_wp, -r], [3, nv])
      plates = reshape([1, 3, 5, 3, 2, 5, 2, 4, 5, 4, 1, 5, 3, 1, 6, 2, 3, 6, 4, 2, 6, 1, 4, 6], [3, np])
      allocate (work(2, 10000), spaixi(200000))
      corpar = 0
      call ctx%dskmi2(nv, vrtces, np, plates, 1.0_wp, 2_ip, 10000_ip, 10000_ip, 10000_ip, .true., &
                      200000_ip, work, spaixd, spaixi)
      call ctx%dskrb2(nv, vrtces, np, plates, 1_ip, corpar, mncor3, mxcor3)
      call delete(file, missing_ok=.true.)
      call ctx%dskopn(file, file, 0_ip, handle)
      do i = nsurf, 1, -1  ! surfaces in reverse order, and each twice
         call ctx%dskw02(handle, 499_ip, i, 1_ip, 'IAU_MARS', 1_ip, corpar, -pi, pi, -pi/2, pi/2, &
                         mncor3, mxcor3, -1.0e9_wp, 1.0e9_wp, nv, vrtces, np, plates, spaixd, spaixi)
         call ctx%dskw02(handle, 499_ip, i, 1_ip, 'IAU_MARS', 1_ip, corpar, -pi, pi, -pi/2, pi/2, &
                         mncor3, mxcor3, -1.0e9_wp, 1.0e9_wp, nv, vrtces, np, plates, spaixd, spaixi)
      end do
      call ctx%dskw02(handle, 401_ip, 7_ip, 1_ip, 'IAU_MARS', 1_ip, corpar, -pi, pi, -pi/2, pi/2, &
                      mncor3, mxcor3, -1.0e9_wp, 1.0e9_wp, nv, vrtces, np, plates, spaixd, spaixi)
      call ctx%dskcls(handle, .true.)
      call check(no_error('write DSK'), 'write the DSK')

      call ctx%dsk_objects(file, ids)
      call set_to_cell([integer(ip) ::], icell, room=10_ip)
      call ctx%dskobj(file, icell)
      call check(no_error('dsk_objects') .and. size(ids) == 2 .and. ctx%cardi(icell) == 2, 'dsk_objects: 2 bodies')
      if (size(ids) == 2) call check(all(ids == [401_ip, 499_ip]) .and. all(icell(1:2) == ids), &
                                     'dsk_objects = dskobj')
      call ctx%dsk_surfaces(file, 499_ip, ids)
      call check(size(ids) == nsurf, 'dsk_surfaces: 30 surfaces')
      if (size(ids) == nsurf) call check(all(ids == [(i, i = 1, nsurf)]), 'dsk_surfaces: sorted, no duplicates')
      call set_to_cell([integer(ip) ::], icell, room=100_ip)
      call ctx%dsksrf(file, 499_ip, icell)
      call check(ctx%cardi(icell) == nsurf .and. all(icell(1:nsurf) == ids), 'dsk_surfaces = dsksrf')
      call set_to_cell([integer(ip) ::], icell, room=10_ip)
      call ctx%dsksrf(file, 499_ip, icell)
      call check(short_error() == 'SPICE(CELLTOOSMALL)', 'dsksrf: a set of 10 is too small')
      call ctx%dsk_surfaces(file, 402_ip, ids)
      call check(no_error('dsk_surfaces') .and. size(ids) == 0, 'dsk_surfaces: no such body')
      call delete(file)
   end subroutine test_dsk_objects

   !> Write the octahedron DSK of `test_dsk_objects` with one segment.
   subroutine write_dsk(file)
      character(*), intent(in) :: file  !! the DSK to write
      call write_dsk_for(file, 499_ip, 'IAU_MARS')
   end subroutine write_dsk

   !> Write an octahedron DSK (radius 1000 km) with one segment for a body
   !> in a frame; with `twin`, a second octahedron 3000 km up the z axis (a
   !> shape whose cuts can have several limb points).
   subroutine write_dsk_for(file, body, frame, twin)
      character(*), intent(in) :: file  !! the DSK to write
      integer(ip), intent(in) :: body  !! the body
      character(*), intent(in) :: frame  !! its body-fixed frame
      logical, intent(in), optional :: twin  !! two octahedra
      real(wp), parameter :: pi = acos(-1.0_wp), r = 1000.0_wp
      real(wp) :: v1(3, 6), spaixd(10), corpar(10), mncor3, mxcor3
      real(wp), allocatable :: vrtces(:,:)
      integer(ip) :: p1(3, 8), handle, nv, np, k
      integer(ip), allocatable :: work(:, :), spaixi(:), plates(:,:)
      v1 = reshape([r, 0.0_wp, 0.0_wp, -r, 0.0_wp, 0.0_wp, 0.0_wp, r, 0.0_wp, &
                    0.0_wp, -r, 0.0_wp, 0.0_wp, 0.0_wp, r, 0.0_wp, 0.0_wp, -r], [3, 6])
      p1 = reshape([1, 3, 5, 3, 2, 5, 2, 4, 5, 4, 1, 5, 3, 1, 6, 2, 3, 6, 4, 2, 6, 1, 4, 6], [3, 8])
      vrtces = v1
      plates = p1
      if (present(twin)) then
         if (twin) then
            vrtces = reshape([[(v1(:, k), k = 1, 6)], &
                              [(v1(:, k) + [0.0_wp, 0.0_wp, 3*r], k = 1, 6)]], [3, 12])
            plates = reshape([reshape(p1, [24]), reshape(p1 + 6, [24])], [3, 16])
         end if
      end if
      nv = size(vrtces, 2)
      np = size(plates, 2)
      allocate (work(2, 10000), spaixi(200000))
      corpar = 0
      call ctx%dskmi2(nv, vrtces, np, plates, 1.0_wp, 2_ip, 10000_ip, 10000_ip, 10000_ip, .true., &
                      200000_ip, work, spaixd, spaixi)
      call ctx%dskrb2(nv, vrtces, np, plates, 1_ip, corpar, mncor3, mxcor3)
      call delete(file, missing_ok=.true.)
      call ctx%dskopn(file, file, 0_ip, handle)
      call ctx%dskw02(handle, body, 1_ip, 1_ip, frame, 1_ip, corpar, -pi, pi, -pi/2, pi/2, &
                      mncor3, mxcor3, -1.0e9_wp, 1.0e9_wp, nv, vrtces, np, plates, spaixd, spaixi)
      call ctx%dskcls(handle, .true.)
   end subroutine write_dsk_for

   !> daf_comments and das_comments against DAFEC and DASEC: more lines
   !> than one comment record holds, and a line longer than a caller's
   !> buffer.
   subroutine test_comments()
      character(*), parameter :: spk = 'test_outputs_comments.bsp', dsk = 'test_outputs_comments.bds'
      integer(ip), parameter :: nl = 60
      character(900) :: text(nl)
      character(900) :: buf(nl + 10)
      character(80) :: short(10)
      type(string), allocatable :: lines(:)
      real(wp) :: states(6, 2)
      integer(ip) :: h, k, n
      logical :: done, same
      do k = 1, nl
         write (text(k), '(a, i0)') 'comment line ', k
      end do
      text(7) = repeat('long-', 180)  ! 900 characters
      text(8) = ''

      ! A DAF (an SPK) with comments.
      states = 0
      call delete(spk, missing_ok=.true.)
      call ctx%spkopn(spk, 'test_outputs', 10_ip, h)
      call ctx%spkw08(h, 1000_ip, 399_ip, 'J2000', 0.0_wp, 1.0_wp, 'SEG', 1_ip, 2_ip, states, 0.0_wp, 1.0_wp)
      call ctx%dafac(h, nl, text)
      call ctx%spkcls(h)
      call ctx%dafopr(spk, h)
      call check(no_error('write DAF comments'), 'write the DAF comments')
      ! DAFEC reads 5 lines first: daf_comments must not move its position.
      call ctx%dafec(h, 5_ip, n, buf, done)
      call ctx%daf_comments(h, lines)
      call check(no_error('daf_comments') .and. size(lines) == nl, 'daf_comments: 60 lines')
      if (size(lines) == nl) then
         same = .true.
         do k = 1, nl
            same = same .and. lines(k)%s == trim(text(k))
         end do
         call check(same, 'daf_comments: the lines')
         call check(len(lines(7)%s) == 900, 'daf_comments: a 900-character line')
      end if
      call ctx%dafec(h, int(size(buf), ip), n, buf, done)
      call check(n == nl - 5 .and. done .and. buf(1) == text(6), 'dafec: its position is kept')
      call ctx%dafcls(h)
      call ctx%dafopr(spk, h)
      call ctx%dafec(h, 10_ip, n, short, done)
      call check(short_error() == 'SPICE(COMMENTTOOLONG)', 'dafec: 80 characters are too few')
      call ctx%dafcls(h)
      call delete(spk)

      ! A DAS file (a DSK) with comments.
      call write_dsk(dsk)
      call ctx%dasopw(dsk, h)
      call ctx%dasac(h, nl, text)
      call ctx%dascls(h)
      call ctx%dasopr(dsk, h)
      call check(no_error('write DAS comments'), 'write the DAS comments')
      call ctx%das_comments(h, lines)
      call ctx%dasec(h, int(size(buf), ip), n, buf, done)
      call check(no_error('das_comments') .and. size(lines) == nl .and. n == nl .and. done, &
                 'das_comments: 60 lines')
      if (size(lines) == nl) then
         same = .true.
         do k = 1, nl
            same = same .and. lines(k)%s == trim(text(k)) .and. buf(k) == text(k)
         end do
         call check(same, 'das_comments = dasec')
      end if
      call ctx%dascls(h)

      ! No comments: no lines.
      call write_dsk(dsk)
      call ctx%dasopr(dsk, h)
      call ctx%das_comments(h, lines)
      call check(no_error('das_comments (none)') .and. size(lines) == 0, 'das_comments: no comments')
      call ctx%dascls(h)
      call delete(dsk)
   end subroutine test_comments

   !> dsk_plates and dsk_vertices against DSKP02 and DSKV02.
   subroutine test_dsk_data()
      character(*), parameter :: dsk = 'test_outputs_data.bds'
      integer(ip), allocatable :: plates(:,:)
      real(wp), allocatable :: vertices(:,:)
      integer(ip) :: h, dladsc(8), p2(3, 8), n, nv, np
      real(wp) :: v2(3, 6)
      logical :: found
      call write_dsk(dsk)
      call ctx%dasopr(dsk, h)
      call ctx%dlabfs(h, dladsc, found)
      call ctx%dskz02(h, dladsc, nv, np)
      call ctx%dsk_plates(h, dladsc, plates)
      call ctx%dskp02(h, dladsc, 1_ip, 8_ip, n, p2)
      call check(no_error('dsk_plates') .and. found .and. np == 8 .and. size(plates, 2) == 8, 'dsk_plates: 8 plates')
      if (size(plates, 2) == 8) call check(all(plates == p2), 'dsk_plates = dskp02')
      call ctx%dsk_vertices(h, dladsc, vertices)
      call ctx%dskv02(h, dladsc, 1_ip, 6_ip, n, v2)
      call check(nv == 6 .and. size(vertices, 2) == 6, 'dsk_vertices: 6 vertices')
      if (size(vertices, 2) == 6) call check(all(vertices == v2), 'dsk_vertices = dskv02')
      call ctx%dascls(h)
      call delete(dsk)
   end subroutine test_dsk_data

   !> ek_values against EKRCED, EKRCEI and EKRCEC: arrays longer than a
   !> caller's buffer, a 1500-character string, a null entry.
   subroutine test_ek_values()
      character(*), parameter :: ek = 'test_outputs.bes'
      integer(ip), parameter :: nd = 500, ni = 300
      character(32) :: cnames(5)
      character(64) :: decls(5)
      real(wp), allocatable :: d(:)
      integer(ip), allocatable :: iv(:)
      type(string), allocatable :: c(:)
      real(wp) :: dbuf(nd)
      integer(ip) :: ibuf(ni), h, segno, recno, n, k, seg
      character(20) :: abuf(3)
      character(:), allocatable :: long
      logical :: isnull
      long = repeat('0123456789', 150)
      cnames = [character(32) :: 'D', 'I', 'S', 'A', 'X']
      decls = [character(64) :: 'DATATYPE = DOUBLE PRECISION, SIZE = VARIABLE', &
               'DATATYPE = INTEGER, SIZE = VARIABLE', 'DATATYPE = CHARACTER*(*)', &
               'DATATYPE = CHARACTER*(20), SIZE = VARIABLE', 'DATATYPE = DOUBLE PRECISION, NULLS_OK = TRUE']
      call delete(ek, missing_ok=.true.)
      call ctx%ekopn(ek, ek, 0_ip, h)
      call ctx%ekbseg(h, 'P7', 5_ip, cnames, decls, segno)
      call ctx%ekappr(h, segno, recno)
      call ctx%ekaced(h, segno, recno, 'D', nd, [(real(k, wp)/4, k = 1, nd)], .false.)
      call ctx%ekacei(h, segno, recno, 'I', ni, [(k*7, k = 1, ni)], .false.)
      call ctx%ekacec(h, segno, recno, 'S', 1_ip, [long], .false.)
      call ctx%ekacec(h, segno, recno, 'A', 3_ip, [character(20) :: 'alpha', 'beta', 'gamma'], .false.)
      call ctx%ekaced(h, segno, recno, 'X', 1_ip, [1.5_wp], .false.)
      call ctx%ekappr(h, segno, recno)
      call ctx%ekaced(h, segno, recno, 'D', 1_ip, [2.0_wp], .false.)
      call ctx%ekacei(h, segno, recno, 'I', 1_ip, [2_ip], .false.)
      call ctx%ekacec(h, segno, recno, 'S', 1_ip, ['short'], .false.)
      call ctx%ekacec(h, segno, recno, 'A', 1_ip, [character(20) :: 'one'], .false.)
      call ctx%ekaced(h, segno, recno, 'X', 1_ip, [0.0_wp], .true.)
      call ctx%ekcls(h)
      call ctx%ekopr(ek, h)
      call check(no_error('write EK'), 'write the EK')

      call ctx%ek_values(h, 1_ip, 1_ip, 'D', d, isnull)
      seg = 1
      call ctx%ekrced(h, seg, 1_ip, 'D', n, dbuf, isnull)
      call check(no_error('ek_values') .and. size(d) == nd .and. n == nd, 'ek_values: 500 reals')
      if (size(d) == nd) call check(all(d == dbuf), 'ek_values = ekrced')
      call ctx%ek_values(h, 1_ip, 1_ip, 'I', iv, isnull)
      call ctx%ekrcei(h, seg, 1_ip, 'I', n, ibuf, isnull)
      call check(size(iv) == ni .and. n == ni, 'ek_values: 300 integers')
      if (size(iv) == ni) call check(all(iv == ibuf), 'ek_values = ekrcei')
      call ctx%ek_values(h, 1_ip, 1_ip, 'S', c, isnull)
      call check(size(c) == 1, 'ek_values: one string')
      if (size(c) == 1) call check(c(1)%s == long, 'ek_values: the whole 1500-character string')
      call ctx%ek_values(h, 1_ip, 1_ip, 'A', c, isnull)
      call ctx%ekrcec(h, seg, 1_ip, 'A', n, abuf, isnull)
      call check(size(c) == 3 .and. n == 3, 'ek_values: 3 strings')
      if (size(c) == 3) call check(c(1)%s == 'alpha' .and. c(3)%s == 'gamma' .and. abuf(2) == c(2)%s, &
                                   'ek_values = ekrcec')
      call ctx%ek_values(h, 1_ip, 2_ip, 'S', c, isnull)
      call check(size(c) == 1 .and. .not. isnull, 'ek_values: record 2')
      if (size(c) == 1) call check(c(1)%s == 'short', 'ek_values: record 2 value')
      call ctx%ek_values(h, 1_ip, 2_ip, 'X', d, isnull)
      call check(no_error('ek_values (null)') .and. isnull .and. size(d) == 0, 'ek_values: a null entry')
      ! the wrong type: EKRCED's error, an empty result
      call ctx%ek_values(h, 1_ip, 1_ip, 'I', d, isnull)
      call check(short_error() == 'SPICE(WRONGDATATYPE)' .and. size(d) == 0, 'ek_values: wrong type')
      call ctx%ek_values(h, 1_ip, 1_ip, 'D', c, isnull)
      call check(short_error() == 'SPICE(WRONGDATATYPE)' .and. size(c) == 0, 'ek_values: wrong type (string)')
      call ctx%ekcls(h)
      call delete(ek)
   end subroutine test_ek_values

   !> limb_points and terminator_points against LIMBPT and TERMPT, for an
   !> ellipsoid and a DSK shape; LIMBPT's room is checked against the total.
   subroutine test_limb_terminator()
      character(*), parameter :: spk = 'test_outputs_limb.bsp', tk = 'test_outputs_limb.tk', &
                                 dsk = 'test_outputs_limb.bds'
      integer(ip), parameter :: ncuts = 36, maxn = 400
      real(wp), parameter :: pi = acos(-1.0_wp), et = 1.0e5_wp
      character(120) :: lines(11)
      character(40) :: methods(3), tmethods(3)
      integer(ip), allocatable :: npts(:)
      real(wp), allocatable :: points(:,:), epochs(:), vecs(:,:)
      integer(ip) :: npts2(maxn), m, n, k, nc
      real(wp) :: points2(3, maxn), epochs2(maxn), vecs2(3, maxn), rs

      lines(1) = 'BODY1000_RADII = ( 3000 2000 1000 )'
      lines(2) = 'BODY1002_RADII = ( 100 100 100 )'
      lines(3) = 'FRAME_P7_FIXED = 1400100'
      lines(4) = "FRAME_1400100_NAME = 'P7_FIXED'"
      lines(5) = 'FRAME_1400100_CLASS = 4'
      lines(6) = 'FRAME_1400100_CLASS_ID = 1400100'
      lines(7) = 'FRAME_1400100_CENTER = 1000'
      lines(8) = "TKFRAME_1400100_RELATIVE = 'J2000'"
      lines(9) = "TKFRAME_1400100_SPEC = 'MATRIX'"
      lines(10) = 'TKFRAME_1400100_MATRIX = ( 1 0 0 0 1 0 0 0 1 )'
      lines(11) = ''
      call load_text_kernel(tk, lines)
      call write_spk(ctx, spk, 1000_ip, 399_ip, 1.0e5_wp, 0.0_wp)
      call write_spk(ctx, spk // '2', 1001_ip, 399_ip, 1.0e6_wp, 0.3_wp)
      call write_spk(ctx, spk // '3', 1002_ip, 399_ip, 1.0e8_wp, 2.0_wp)
      call spice%furnsh(spk)
      call spice%furnsh(spk // '2')
      call spice%furnsh(spk // '3')
      ! a DSK octahedron for body 1000 in P7_FIXED
      call write_dsk_for(dsk, 1000_ip, 'P7_FIXED')
      call spice%furnsh(dsk)
      call check(no_error('limb kernels'), 'limb: kernels')

      methods = [character(40) :: 'TANGENT/ELLIPSOID', 'TANGENT/DSK/UNPRIORITIZED', &
                 'TANGENT/DSK/UNPRIORITIZED']
      tmethods = [character(40) :: 'UMBRAL/TANGENT/ELLIPSOID', 'UMBRAL/TANGENT/DSK/UNPRIORITIZED', &
                  'UMBRAL/TANGENT/DSK/UNPRIORITIZED']
      do k = 1, 3
         nc = ncuts
         rs = 2*pi/ncuts
         if (k == 3) then
            ! the twin octahedra, with 8 cuts close to +z: each crosses both
            ! octahedra, so it has several limb points
            call spice%unload(dsk)
            call write_dsk_for(dsk, 1000_ip, 'P7_FIXED', twin=.true.)
            call spice%furnsh(dsk)
            nc = 8
            rs = 0.05_wp
         end if
         call ctx%limb_points(methods(k), '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                              [0.0_wp, 0.0_wp, 1.0_wp], rs, nc, 1.0e-4_wp, 1.0e-9_wp, &
                              npts, points, epochs, vecs)
         call ctx%limbpt(methods(k), '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                         [0.0_wp, 0.0_wp, 1.0_wp], rs, nc, 1.0e-4_wp, 1.0e-9_wp, &
                         maxn, npts2, points2, epochs2, vecs2)
         call check(no_error('limb_points ' // trim(methods(k))), 'limb_points: no error, ' // trim(methods(k)))
         n = sum(npts2(1:nc))
         call check(size(npts) == nc .and. size(points, 2) == n .and. size(epochs) == n .and. &
                    size(vecs, 2) == n .and. n >= nc, 'limb_points: sizes, ' // trim(methods(k)))
         if (size(points, 2) == n .and. size(npts) == nc) then
            call check(all(npts == npts2(1:nc)) .and. all(points == points2(:, 1:n)) .and. &
                       all(epochs == epochs2(1:n)) .and. all(vecs == vecs2(:, 1:n)), &
                       'limb_points = limbpt, ' // trim(methods(k)))
         end if
         if (k == 3) then
            call check(n > nc, 'twin octahedra: several limb points in a cut')
            ! LIMBPT with room for one point fewer than the total but at
            ! least one per cut (SPICELIB's LIMBPT wrote past its arrays).
            call ctx%limbpt(methods(k), '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                            [0.0_wp, 0.0_wp, 1.0_wp], rs, nc, 1.0e-4_wp, 1.0e-9_wp, &
                            n - 1, npts2, points2, epochs2, vecs2)
            call check(short_error() == 'SPICE(OUTOFROOM)', 'limbpt: room for one point fewer')
         end if

         call ctx%terminator_points(tmethods(k), '1002', '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                                    [0.0_wp, 0.0_wp, 1.0_wp], rs, nc, 1.0e-4_wp, 1.0e-9_wp, &
                                    npts, points, epochs, vecs)
         call ctx%termpt(tmethods(k), '1002', '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                         [0.0_wp, 0.0_wp, 1.0_wp], rs, nc, 1.0e-4_wp, 1.0e-9_wp, &
                         maxn, npts2, points2, epochs2, vecs2)
         call check(no_error('terminator_points ' // trim(tmethods(k))), &
                    'terminator_points: no error, ' // trim(tmethods(k)))
         m = sum(npts2(1:nc))
         call check(size(npts) == nc .and. size(points, 2) == m .and. m >= nc, &
                    'terminator_points: sizes, ' // trim(tmethods(k)))
         if (size(points, 2) == m .and. size(npts) == nc) then
            call check(all(npts == npts2(1:nc)) .and. all(points == points2(:, 1:m)) .and. &
                       all(epochs == epochs2(1:m)) .and. all(vecs == vecs2(:, 1:m)), &
                       'terminator_points = termpt, ' // trim(tmethods(k)))
         end if
         print '(a, i0, a, i0, a, i0, a)', '   ' // trim(methods(k)) // ', ', nc, ' cuts: ', n, &
            ' limb points, ', m, ' terminator points'
         ! TERMPT with room for exactly the total (SPICELIB's TERMPT took a
         ! cut's room once per point: it failed here when a cut had two).
         if (m > nc) then
            call ctx%termpt(tmethods(k), '1002', '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                            [0.0_wp, 0.0_wp, 1.0_wp], rs, nc, 1.0e-4_wp, 1.0e-9_wp, &
                            m, npts2, points2, epochs2, vecs2)
            call check(no_error('termpt (room m)'), 'termpt: room for exactly the total')
            call ctx%termpt(tmethods(k), '1002', '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                            [0.0_wp, 0.0_wp, 1.0_wp], rs, nc, 1.0e-4_wp, 1.0e-9_wp, &
                            m - 1, npts2, points2, epochs2, vecs2)
            call check(short_error() == 'SPICE(OUTOFROOM)', 'termpt: room for one point fewer')
         end if
      end do

      ! Errors: those of LIMBPT, with empty results.
      call ctx%limb_points('TANGENT/ELLIPSOID', 'NO_SUCH_BODY', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                           [0.0_wp, 0.0_wp, 1.0_wp], 2*pi/ncuts, ncuts, 1.0e-4_wp, 1.0e-9_wp, &
                           npts, points, epochs, vecs)
      call check(short_error() == 'SPICE(IDCODENOTFOUND)' .and. size(npts) == 0 .and. &
                 size(points, 2) == 0, 'limb_points: unknown target')
      call ctx%limb_points('TANGENT/ELLIPSOID', '1000', et, 'P7_FIXED', 'NONE', 'CENTER', '1001', &
                           [0.0_wp, 0.0_wp, 1.0_wp], 2*pi/ncuts, 0_ip, 1.0e-4_wp, 1.0e-9_wp, &
                           npts, points, epochs, vecs)
      call check(short_error() == 'SPICE(INVALIDCOUNT)', 'limb_points: no cuts')

      call spice%unload(dsk)
      call spice%unload(spk)
      call spice%unload(spk // '2')
      call spice%unload(spk // '3')
      call spice%unload(tk)
      call delete(dsk)
      call delete(spk)
      call delete(spk // '2')
      call delete(spk // '3')
      call delete(tk)
   end subroutine test_limb_terminator

   !> The GF searches with windows as arrays against their SPICELIB forms
   !> given large enough cells: results of hundreds of intervals, more than
   !> a small cell holds.
   subroutine test_gf()
      character(*), parameter :: spk = 'test_outputs_gf.bsp'
      integer(ip), parameter :: mw = 8000, nw = 15, n = 10001
      real(wp), parameter :: w = 2*acos(-1.0_wp)/1.0e4_wp, step = 500.0_wp
      real(wp), allocatable :: cnf(:,:), r(:,:), r2(:,:), cell(:), res(:), work(:,:), work2(:,:), epochs(:), &
                               states(:,:)
      character(32) :: qpnams(3), qcpars(3)
      real(wp) :: qdpars(1)
      integer(ip) :: qipars(1), h, k
      logical :: qlpars(1)

      ! body 2000: x = 1e6 cos(w t), y = 3e5 km (100 periods in 1e6 s);
      ! body 3000 fixed at (0, 1e6, 0)
      allocate (epochs(n), states(6, n))
      do k = 1, n
         epochs(k) = 100.0_wp*(k - 1)
         states(:, k) = [1.0e6_wp*cos(w*epochs(k)), 3.0e5_wp, 0.0_wp, -1.0e6_wp*w*sin(w*epochs(k)), 0.0_wp, 0.0_wp]
      end do
      call delete(spk, missing_ok=.true.)
      call ctx%spkopn(spk, 'test_outputs', 0_ip, h)
      call ctx%spkw09(h, 2000_ip, 0_ip, 'J2000', 0.0_wp, epochs(n), 'OSC', 7_ip, n, states, epochs)
      call ctx%spkw09(h, 3000_ip, 0_ip, 'J2000', 0.0_wp, epochs(n), 'FIXED', 1_ip, 2_ip, &
                      reshape([0.0_wp, 1.0e6_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, &
                               0.0_wp, 1.0e6_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp], [6, 2]), [0.0_wp, epochs(n)])
      call ctx%spkcls(h)
      call spice%furnsh(spk)
      call check(no_error('GF SPK'), 'GF: write and load the SPK')

      cnf = reshape([10.0_wp, 0.5e6_wp, 0.6e6_wp, epochs(n) - 10], [2, 2])
      call window_to_cell(cnf, cell)
      allocate (res(-5:mw), work(-5:mw, nw), work2(-5:100, nw))

      call ctx%gf_distance('2000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, step, cnf, r)
      call ctx%ssized(mw, res)
      call ctx%gfdist('2000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, step, cell, mw, nw, work, res)
      call compare('gf_distance', r, res, 150)
      ! the SPICELIB form with a cell of 50 intervals
      call ctx%ssized(100_ip, res)
      call ctx%gfdist('2000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, step, cell, 100_ip, nw, work2, res)
      call check(short_error() == 'SPICE(WINDOWEXCESS)', 'gfdist: 50 intervals are too few')

      call ctx%gf_position('2000', 'J2000', 'NONE', '0', 'RECTANGULAR', 'X', '>', 0.0_wp, 0.0_wp, step, cnf, r)
      call ctx%ssized(mw, res)
      call ctx%gfposc('2000', 'J2000', 'NONE', '0', 'RECTANGULAR', 'X', '>', 0.0_wp, 0.0_wp, step, cell, &
                      mw, nw, work, res)
      call compare('gf_position', r, res, 50)

      call ctx%gf_range_rate('2000', 'NONE', '0', 'LOCMAX', 0.0_wp, 0.0_wp, step, cnf, r)
      call ctx%ssized(mw, res)
      call ctx%gfrr('2000', 'NONE', '0', 'LOCMAX', 0.0_wp, 0.0_wp, step, cell, mw, nw, work, res)
      call compare('gf_range_rate', r, res, 50)

      call ctx%gf_separation('2000', 'POINT', ' ', '3000', 'POINT', ' ', 'NONE', '0', '<', 0.6_wp, 0.0_wp, &
                             step, cnf, r)
      call ctx%ssized(mw, res)
      call ctx%gfsep('2000', 'POINT', ' ', '3000', 'POINT', ' ', 'NONE', '0', '<', 0.6_wp, 0.0_wp, &
                     step, cell, mw, nw, work, res)
      call compare('gf_separation', r, res, 50)

      call ctx%gf_phase('2000', '3000', 'NONE', '0', '>', 2.0_wp, 0.0_wp, step, cnf, r)
      call ctx%ssized(mw, res)
      call ctx%gfpa('2000', '3000', 'NONE', '0', '>', 2.0_wp, 0.0_wp, step, cell, mw, nw, work, res)
      call compare('gf_phase', r, res, 50)

      ! user-defined quantities: sin(2 pi t / 1000) > 0.5, 1000 intervals
      call ctx%gf_user_scalar(sine, decreasing, '>', 0.5_wp, 0.0_wp, 50.0_wp, cnf, r)
      call ctx%ssized(mw, res)
      call ctx%gfuds(sine, decreasing, '>', 0.5_wp, 0.0_wp, 50.0_wp, cell, mw, nw, work, res)
      call compare('gf_user_scalar', r, res, 900)
      call ctx%gf_user_boolean(sine, above, 50.0_wp, cnf, r2)
      call ctx%ssized(mw, res)
      call ctx%gfudb(sine, above, 50.0_wp, cell, res)
      call compare('gf_user_boolean', r2, res, 900)
      if (size(r, 2) == size(r2, 2)) call check(maxval(abs(r - r2)) < 1.0e-4_wp, &
                                                'gf_user_boolean = gf_user_scalar')

      ! GFEVNT with the caller's step routine: the distance search again
      qpnams = [character(32) :: 'TARGET', 'OBSERVER', 'ABCORR']
      qcpars = [character(32) :: '2000', '0', 'NONE']
      qdpars = 0
      qipars = 0
      qlpars = .false.
      call ctx%gf_distance('2000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, step, cnf, r)
      call ctx%gf_event(fixed_step, refine, 'DISTANCE', 3_ip, qpnams, qcpars, qdpars, qipars, qlpars, &
                        '<', 5.0e5_wp, 1.0e-6_wp, 0.0_wp, step, cnf, .false., rep_init, rep_update, &
                        rep_final, .false., no_bail, r2)
      call check(no_error('gf_event') .and. size(r2, 2) == size(r, 2), 'gf_event: as many intervals as gf_distance')
      if (size(r2, 2) == size(r, 2)) call check(maxval(abs(r - r2)) < 1.0e-4_wp, 'gf_event = gf_distance')

      ! Errors: those of the search, with an empty result.
      call ctx%gf_distance('2000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, -1.0_wp, cnf, r)
      call check(short_error() /= ' ' .and. size(r, 2) == 0, 'gf_distance: a negative step')
      call ctx%gf_distance('NO_SUCH_BODY', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, step, cnf, r)
      call check(short_error() == 'SPICE(IDCODENOTFOUND)' .and. size(r, 2) == 0, 'gf_distance: unknown target')
      ! A step too small for the windows to be allocated.
      call ctx%gf_distance('2000', 'NONE', '0', '<', 5.0e5_wp, 0.0_wp, 1.0e-12_wp, cnf, r)
      call check(short_error() == 'SPICE(INSUFFICIENTMEMORY)' .and. size(r, 2) == 0, &
                 'gf_distance: windows too large to allocate')

      call spice%unload(spk)
      call delete(spk)
   end subroutine test_gf

   !> Compare a window array with a result cell; at least MINN intervals.
   subroutine compare(what, r, cell, minn)
      character(*), intent(in) :: what  !! the search
      real(wp), intent(in) :: r(:,:)  !! the window array
      real(wp), intent(in) :: cell(-5:)  !! the result cell
      integer, intent(in) :: minn  !! the least number of intervals expected
      integer(ip) :: n
      n = ctx%cardd(cell)/2
      call check(no_error(what), what // ': no error')
      call check(size(r, 2) == n .and. n >= minn, what // ': as many intervals as the SPICELIB form')
      if (size(r, 2) == n .and. n > 0) call check(all(reshape(r, [2*n]) == cell(1:2*n)), what // ': the same intervals')
   end subroutine compare

   !> sin(2 pi et / 1000) (a GF_SCALAR).
   subroutine sine(c, et, value)
      class(pepper_context), intent(inout) :: c  !! the context
      real(wp), intent(in) :: et  !! the time
      real(wp), intent(out) :: value  !! the value
      value = sin(2*acos(-1.0_wp)*et/1000.0_wp)
   end subroutine sine

   !> Whether the function is decreasing (a GF_DECR_X).
   subroutine decreasing(c, udfunc, et, xbool)
      class(pepper_context), intent(inout) :: c  !! the context
      procedure(gf_scalar) :: udfunc  !! the function
      real(wp), intent(in) :: et  !! the time
      logical, intent(out) :: xbool  !! decreasing at ET
      call c%uddc(udfunc, et, 1.0_wp, xbool)
   end subroutine decreasing

   !> Whether the function is above 0.5 (a GF_BOOLEAN).
   subroutine above(c, udfunc, et, xbool)
      class(pepper_context), intent(inout) :: c  !! the context
      procedure(gf_scalar) :: udfunc  !! the function
      real(wp), intent(in) :: et  !! the time
      logical, intent(out) :: xbool  !! the condition at ET
      real(wp) :: v
      call udfunc(c, et, v)
      xbool = v > 0.5_wp
   end subroutine above

   !> A constant step of 500 s (a GF_STEP).
   subroutine fixed_step(c, time, step)
      class(pepper_context), intent(inout) :: c  !! the context
      real(wp), intent(in) :: time  !! the time
      real(wp), intent(out) :: step  !! the step
      step = 500.0_wp
      if (.false.) print *, time
   end subroutine fixed_step

   !> Bisection (a GF_REFINE).
   subroutine refine(c, t1, t2, s1, s2, t)
      class(pepper_context), intent(inout) :: c  !! the context
      real(wp), intent(in) :: t1  !! one time
      real(wp), intent(in) :: t2  !! the other time
      logical, intent(in) :: s1  !! the condition at T1
      logical, intent(in) :: s2  !! the condition at T2
      real(wp), intent(out) :: t  !! the time between
      t = 0.5_wp*(t1 + t2)
      if (.false.) print *, s1, s2
   end subroutine refine

   !> No progress report (GF_REP_INIT, GF_REP_UPDATE, GF_REP_FINAL).
   subroutine rep_init(c, window, begmss, endmss)
      class(pepper_context), intent(inout) :: c  !! the context
      real(wp), intent(in) :: window(-5:*)  !! the window
      character(*), intent(in) :: begmss  !! the text before
      character(*), intent(in) :: endmss  !! the text after
   end subroutine rep_init

   !> No progress report.
   subroutine rep_update(c, ivbeg, ivend, time)
      class(pepper_context), intent(inout) :: c  !! the context
      real(wp), intent(in) :: ivbeg  !! the start of the interval
      real(wp), intent(in) :: ivend  !! its end
      real(wp), intent(in) :: time  !! the time
   end subroutine rep_update

   !> No progress report.
   subroutine rep_final(c)
      class(pepper_context), intent(inout) :: c  !! the context
   end subroutine rep_final

   !> No interrupt (a GF_BAIL).
   logical function no_bail(c)
      class(pepper_context), intent(inout) :: c  !! the context
      no_bail = .false.
   end function no_bail

end program test_outputs
