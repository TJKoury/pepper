!> Small synthetic kernels for tests and benchmarks that need many of them:
!> enough files, bodies, frames or pool variables to go past SPICELIB's
!> fixed limits (see limits.md) in seconds.
!>
!> The kernels are written to the working directory under names starting
!> with a prefix chosen by the caller; `delete_file` removes them.
module pepper_test_kernels

   use pepper_kinds, only: ip, wp
   use pepper_constants, only: twopi
   use pepper_instance, only: pepper_context

   implicit none

   private

   public :: write_lsk, write_spk, write_pool_kernel, write_body_names, write_tk_frames
   public :: kernel_name, delete_file, body_name, frame_name, circle_state

   !> Number of states of a segment written by `write_spk`.
   integer, parameter :: nstates = 11
   !> Time between the states of a segment (s): the segments cover 0 to 1e6 s.
   real(wp), parameter :: step = 1.0e5_wp

contains

   !> The name of file number `i` of a set: `<prefix><i><ext>`.
   function kernel_name(prefix, i, ext) result(name)
      character(*), intent(in) :: prefix  !! the start of the name
      integer(ip), intent(in) :: i  !! the number of the file
      character(*), intent(in) :: ext  !! the extension, with its dot
      character(:), allocatable :: name
      character(12) :: buf
      write (buf, '(i0)') i
      name = prefix // trim(buf) // ext
   end function kernel_name

   !> The name of body number `i` written by `write_body_names`.
   function body_name(i) result(name)
      integer(ip), intent(in) :: i  !! the number of the body
      character(:), allocatable :: name
      character(12) :: buf
      write (buf, '(i0)') i
      name = 'PEPPER_BODY_' // trim(buf)
   end function body_name

   !> The name of frame number `i` written by `write_tk_frames`.
   function frame_name(i) result(name)
      integer(ip), intent(in) :: i  !! the number of the frame
      character(:), allocatable :: name
      character(12) :: buf
      write (buf, '(i0)') i
      name = 'PEPPER_TK_' // trim(buf)
   end function frame_name

   !> Delete a file if it exists.
   subroutine delete_file(file)
      character(*), intent(in) :: file  !! the file
      logical :: exists
      integer :: unit
      inquire (file=file, exist=exists)
      if (exists) then
         open (newunit=unit, file=file, status='old')
         close (unit, status='delete')
      end if
   end subroutine delete_file

   !> Write a leapseconds kernel (one leap second entry: TAI-UTC = 32 s
   !> from 1999).
   subroutine write_lsk(file)
      character(*), intent(in) :: file  !! the file to write
      integer :: unit
      call delete_file(file)
      open (newunit=unit, file=file, status='new', action='write')
      write (unit, '(a)') 'KPL/LSK', '\begindata', &
         'DELTET/DELTA_T_A = 32.184', &
         'DELTET/K = 1.657D-3', &
         'DELTET/EB = 1.671D-2', &
         'DELTET/M = ( 6.239996D0 1.99096871D-7 )', &
         'DELTET/DELTA_AT = ( 32, @1999-JAN-1 )', &
         '\begintext'
      close (unit)
   end subroutine write_lsk

   !> Write an SPK with one type 9 segment: `body` on a circle of radius
   !> `r` (km) around `center` in J2000, with angular rate 1e-6 rad/s,
   !> phase `phase` (rad) at ET 0, from ET 0 to 1e6 s. The state at `et`
   !> is given by `circle_state`.
   subroutine write_spk(ctx, file, body, center, r, phase, frame)
      type(pepper_context), intent(inout) :: ctx  !! the context used to write the file
      character(*), intent(in) :: file  !! the file to write
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! the center of motion
      real(wp), intent(in) :: r  !! the radius (km)
      real(wp), intent(in) :: phase  !! the phase at ET 0 (rad)
      integer(ip), intent(in), optional :: frame  !! the frame ID of the segment (default J2000)
      real(wp) :: epochs(nstates), states(6, nstates), sum(5)
      integer(ip) :: h, frid
      integer :: k
      call delete_file(file)
      epochs = [(step*(k - 1), k = 1, nstates)]
      do k = 1, nstates
         states(:, k) = circle_state(r, phase, epochs(k))
      end do
      call ctx%dafonw(file, 'SPK', 2_ip, 6_ip, 'PEPPER test SPK', 0_ip, h)
      frid = 1
      if (present(frame)) frid = frame
      call ctx%dafps(2_ip, 6_ip, [epochs(1), epochs(nstates)], [body, center, frid, 9_ip, 0_ip, 0_ip], sum)
      call ctx%dafbna(h, sum, 'CIRCLE')
      ! type 9: the states, the epochs, no epoch directory (fewer than 100
      ! epochs), the degree and the number of states
      call ctx%dafada(reshape(states, [6*nstates]), int(6*nstates, ip))
      call ctx%dafada(epochs, int(nstates, ip))
      call ctx%dafada([7.0_wp, real(nstates, wp)], 2_ip)
      call ctx%dafena()
      call ctx%dafcls(h)
   end subroutine write_spk

   !> The state on the circle of `write_spk` at `et`.
   pure function circle_state(r, phase, et) result(state)
      real(wp), intent(in) :: r  !! the radius (km)
      real(wp), intent(in) :: phase  !! the phase at ET 0 (rad)
      real(wp), intent(in) :: et  !! the epoch
      real(wp) :: state(6)
      real(wp), parameter :: w = 1.0e-6_wp
      real(wp) :: a
      a = phase + w*et
      state = [r*cos(a), r*sin(a), 0.0_wp, -r*w*sin(a), r*w*cos(a), 0.0_wp]
   end function circle_state

   !> Write a text kernel with `nvars` numeric variables
   !> `<prefix><i>` (i = 1 to `nvars`), each with `nvals` values
   !> i, i+1, ...
   subroutine write_pool_kernel(file, prefix, nvars, nvals)
      character(*), intent(in) :: file  !! the file to write
      character(*), intent(in) :: prefix  !! the start of the variable names
      integer(ip), intent(in) :: nvars  !! the number of variables
      integer(ip), intent(in) :: nvals  !! the number of values of each
      integer :: unit
      integer(ip) :: i, j, k
      call delete_file(file)
      open (newunit=unit, file=file, status='new', action='write')
      write (unit, '(a)') 'KPL/PCK', '\begindata'
      do i = 1, nvars
         write (unit, '(a,i0,a)') prefix, i, ' = ('
         ! ten values per line, within the 132-character line limit
         do j = 0, nvals - 1, 10
            write (unit, '(10(1x,i0))') (i + j + k, k = 0, min(9_ip, nvals - 1 - j))
         end do
         write (unit, '(a)') ')'
      end do
      write (unit, '(a)') '\begintext'
      close (unit)
   end subroutine write_pool_kernel

   !> Write a text kernel defining `n` body names `body_name(i)` with the
   !> codes `first + i - 1`.
   subroutine write_body_names(file, n, first)
      character(*), intent(in) :: file  !! the file to write
      integer(ip), intent(in) :: n  !! the number of names
      integer(ip), intent(in) :: first  !! the code of the first one
      integer :: unit
      integer(ip) :: i
      call delete_file(file)
      open (newunit=unit, file=file, status='new', action='write')
      write (unit, '(a)') 'KPL/FK', '\begindata'
      do i = 1, n
         write (unit, '(3a)') 'NAIF_BODY_NAME += ''', body_name(i), ''''
         write (unit, '(a,i0)') 'NAIF_BODY_CODE += ', first + i - 1
      end do
      write (unit, '(a)') '\begintext'
      close (unit)
   end subroutine write_body_names

   !> Write a frame kernel defining `n` TK frames `frame_name(i)` with the
   !> IDs `first + i - 1`, each relative to J2000 and rotated about Z by
   !> i/1000 degrees.
   subroutine write_tk_frames(file, n, first)
      character(*), intent(in) :: file  !! the file to write
      integer(ip), intent(in) :: n  !! the number of frames
      integer(ip), intent(in) :: first  !! the ID of the first one
      integer :: unit
      integer(ip) :: i, id
      call delete_file(file)
      open (newunit=unit, file=file, status='new', action='write')
      write (unit, '(a)') 'KPL/FK', '\begindata'
      do i = 1, n
         id = first + i - 1
         write (unit, '(3a,i0)') 'FRAME_', frame_name(i), ' = ', id
         write (unit, '(a,i0,3a)') 'FRAME_', id, '_NAME = ''', frame_name(i), ''''
         write (unit, '(a,i0,a)') 'FRAME_', id, '_CLASS = 4'
         write (unit, '(a,i0,a,i0)') 'FRAME_', id, '_CLASS_ID = ', id
         write (unit, '(a,i0,a)') 'FRAME_', id, '_CENTER = 399'
         write (unit, '(a,i0,a)') 'TKFRAME_', id, '_RELATIVE = ''J2000'''
         write (unit, '(a,i0,a)') 'TKFRAME_', id, '_SPEC = ''ANGLES'''
         write (unit, '(a,i0,a)') 'TKFRAME_', id, '_UNITS = ''DEGREES'''
         write (unit, '(a,i0,a)') 'TKFRAME_', id, '_AXES = ( 3, 1, 3 )'
         write (unit, '(a,i0,a,es24.16,a)') 'TKFRAME_', id, '_ANGLES = ( ', i/1000.0_wp, ', 0, 0 )'
      end do
      write (unit, '(a)') '\begintext'
      close (unit)
   end subroutine write_tk_frames

end module pepper_test_kernels
