!> SPK segment buffers through the module API: the table of loaded files
!> belongs to the instance, the segment caches to the contexts. Context b
!> fills its cache by searching; context a then loads and unloads files, and
!> b's searches see each change (b discards its stale cache).
program test_segments

   use pepper_kinds, only: ip, wp
   use pepper_instance_module, only: pepper_instance, pepper_context

   implicit none

   character(*), parameter :: file1 = 'test_segments_1.bsp'
   character(*), parameter :: file2 = 'test_segments_2.bsp'
   ! SPK summaries: two doubles (start and stop times) and six integers
   ! (body, center, frame, type, and the begin and end addresses).
   integer(ip), parameter :: nd = 2, ni = 6
   integer(ip), parameter :: body = 399
   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: a  !! the instance's context (it loads the kernels)
   type(pepper_context) :: b
   integer(ip) :: h1, h2
   integer :: failures = 0

   a => spice%context()
   b = spice%new_context()

   call delete_file(file1)
   call delete_file(file2)
   ! file 1 covers [0, 100], file 2 (loaded later: higher priority) [40, 60]
   call write_spk(file1, 0.0_wp, 100.0_wp, 'SEGMENT 1')
   call write_spk(file2, 40.0_wp, 60.0_wp, 'SEGMENT 2')

   call spice%spklef(file1, h1)
   call check(.not. a%failed(), 'a loads file 1')
   call expect(b, 50.0_wp, h1, 'SEGMENT 1', 'b finds the segment of file 1')
   call expect(a, 50.0_wp, h1, 'SEGMENT 1', 'a finds the segment of file 1')

   call spice%spklef(file2, h2)
   call check(.not. a%failed(), 'a loads file 2')
   call expect(b, 50.0_wp, h2, 'SEGMENT 2', 'b finds the segment of file 2 (loaded by a)')
   call expect(b, 80.0_wp, h1, 'SEGMENT 1', 'b finds file 1 outside file 2''s coverage')
   call expect(a, 50.0_wp, h2, 'SEGMENT 2', 'a finds the segment of file 2')

   call spice%spkuef(h2)
   call check(.not. a%failed(), 'a unloads file 2')
   call expect(b, 50.0_wp, h1, 'SEGMENT 1', 'b finds file 1 again (file 2 unloaded by a)')

   ! before time 0, no loaded file covers the body
   call expect(b, -10.0_wp, 0_ip, '', 'b finds nothing outside the coverage')

   ! with no files loaded, a search is an error (SPICE(NOLOADEDFILES))
   call spice%spkuef(h1)
   call no_files(b, 'b: no files loaded (all unloaded by a)')
   call no_files(a, 'a: no files loaded')

   call delete_file(file1)
   call delete_file(file2)

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_segments: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: msg
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
         if (a%failed()) then
            call a%getmsg('LONG', msg)
            print '(2a)', '   a: ', trim(msg)
         end if
         if (b%failed()) then
            call b%getmsg('LONG', msg)
            print '(2a)', '   b: ', trim(msg)
         end if
      end if
   end subroutine check

   !> Search for a segment of the body at a time, and check the file and the
   !> identifier of the segment found (handle 0: none is found).
   subroutine expect(ctx, et, handle, ident, what)
      type(pepper_context), intent(inout) :: ctx  !! the searching context
      real(wp), intent(in) :: et  !! the time
      integer(ip), intent(in) :: handle  !! handle of the expected file, or 0
      character(*), intent(in) :: ident  !! identifier of the expected segment
      character(*), intent(in) :: what  !! what is checked
      integer(ip) :: h
      real(wp) :: descr(5)
      character(40) :: id
      logical :: found
      call ctx%spksfs(body, et, h, descr, id, found)
      if (handle == 0) then
         call check(.not. ctx%failed() .and. .not. found, what)
      else
         call check(.not. ctx%failed() .and. found .and. h == handle .and. id == ident, what)
      end if
   end subroutine expect

   !> Check that a search signals SPICE(NOLOADEDFILES), and reset the error.
   subroutine no_files(ctx, what)
      type(pepper_context), intent(inout) :: ctx  !! the searching context
      character(*), intent(in) :: what  !! what is checked
      integer(ip) :: h
      real(wp) :: descr(5)
      character(40) :: id
      character(80) :: msg
      logical :: found
      call ctx%spksfs(body, 50.0_wp, h, descr, id, found)
      call ctx%getmsg('SHORT', msg)
      if (.not. (ctx%failed() .and. msg == 'SPICE(NOLOADEDFILES)')) then
         failures = failures + 1
         print '(3a)', 'FAILED: ', what, ' (' // trim(msg) // ')'
      end if
      call ctx%reset()
   end subroutine no_files

   !> Remove a test file if it exists.
   subroutine delete_file(file)
      character(*), intent(in) :: file  !! the file
      logical :: exists
      integer :: unit
      logical :: still_open  !! the file is connected to a unit (Windows can't delete it)
      inquire (file=file, exist=exists)
      if (exists) then
         inquire (file=file, opened=still_open)
         if (still_open) error stop 'a file to delete is still open (Windows cannot delete it): '//file
         open (newunit=unit, file=file, status='old')
         close (unit, status='delete')
      end if
   end subroutine delete_file

   !> Write an SPK-like DAF with one segment for the body (center 10, frame
   !> 1, type 2) covering [t0, t1]: the segment buffer reads only the
   !> summaries and the names.
   subroutine write_spk(file, t0, t1, ident)
      character(*), intent(in) :: file  !! the file to write
      real(wp), intent(in) :: t0  !! start of the coverage
      real(wp), intent(in) :: t1  !! end of the coverage
      character(*), intent(in) :: ident  !! the segment identifier
      integer(ip) :: h
      real(wp) :: sum(nd + (ni + 1)/2)
      call a%dafonw(file, 'SPK', nd, ni, 'PEPPER test SPK', 0_ip, h)
      call a%dafps(nd, ni, [t0, t1], [body, 10_ip, 1_ip, 2_ip, 0_ip, 0_ip], sum)
      call a%dafbna(h, sum, ident)
      call a%dafada([1.0_wp, 2.0_wp, 3.0_wp], 3_ip)
      call a%dafena()
      call a%dafcls(h)
      call check(.not. a%failed(), 'write ' // file)
   end subroutine write_spk

end program test_segments
