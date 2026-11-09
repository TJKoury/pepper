!> The kernel manager through the module API: context a loads a meta-kernel
!> (which loads an SPK and a text kernel), unloads kernels and clears them;
!> the kernel table, the pool and the loaded files belong to the instance,
!> so context b sees each change.
program test_keeper

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   character(*), parameter :: spk = 'test_keeper.bsp'
   character(*), parameter :: text = 'test_keeper.tpc'
   character(*), parameter :: meta = 'test_keeper.tm'
   ! SPK summaries: two doubles (start and stop times) and six integers
   ! (body, center, frame, type, and the begin and end addresses).
   integer(ip), parameter :: nd = 2, ni = 6
   integer(ip), parameter :: body = 399
   type(pepper_spice), target :: spice
   type(pepper_context) :: a, b
   integer :: failures = 0

   a = spice%new_context()
   b = spice%new_context()

   call delete_files()
   call write_spk()
   call write_text(text, [character(80) :: '\begindata', 'PEPPER_TEST = ( 1.5, 2.5 )'])
   call write_text(meta, [character(80) :: '\begindata', &
                          'KERNELS_TO_LOAD = ( ''' // spk // ''', ''' // text // ''' )'])

   call a%furnsh(meta)
   call check(.not. a%failed(), 'a loads the meta-kernel')
   call count_is(b, 'ALL', 3, 'b counts the meta-kernel and its two kernels')
   call count_is(b, 'SPK', 1, 'b counts the SPK')
   call count_is(b, 'TEXT', 1, 'b counts the text kernel')
   call source_is(b, spk, meta, 'b sees the source of the SPK')
   call variable_is(b, .true., 'b sees the text kernel''s variable')
   call segment_found(b, .true., 'b finds the SPK''s segment')

   call a%unload(text)
   call check(.not. a%failed(), 'a unloads the text kernel')
   call count_is(b, 'TEXT', 0, 'b: the text kernel is gone')
   call variable_is(b, .false., 'b: the text kernel''s variable is gone')
   call segment_found(b, .true., 'b still finds the SPK''s segment')

   call a%furnsh(text)
   call a%kclear()
   call check(.not. a%failed(), 'a clears all kernels')
   call count_is(b, 'ALL', 0, 'b: no kernels after kclear')
   call variable_is(b, .false., 'b: the pool is empty after kclear')
   call segment_found(b, .false., 'b: no SPK loaded after kclear')

   ! b loads a kernel; a sees it
   call b%furnsh(spk)
   call count_is(a, 'SPK', 1, 'a counts the SPK loaded by b')
   call segment_found(a, .true., 'a finds the segment of the SPK loaded by b')
   call b%kclear()

   call delete_files()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_keeper: all checks passed'

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

   !> Check the number of loaded kernels of a kind.
   subroutine count_is(ctx, kind, expected, what)
      type(pepper_context), intent(inout) :: ctx  !! the context that counts
      character(*), intent(in) :: kind  !! the kinds of kernels (as for KTOTAL)
      integer, intent(in) :: expected  !! the expected number
      character(*), intent(in) :: what  !! what is checked
      integer(ip) :: count
      call ctx%ktotal(kind, count)
      call check(.not. ctx%failed() .and. count == expected, what)
   end subroutine count_is

   !> Check the meta-kernel a kernel was loaded from (KINFO).
   subroutine source_is(ctx, file, source, what)
      type(pepper_context), intent(inout) :: ctx  !! the context that looks
      character(*), intent(in) :: file  !! the kernel
      character(*), intent(in) :: source  !! the expected source
      character(*), intent(in) :: what  !! what is checked
      character(80) :: filtyp, srcfil
      integer(ip) :: handle
      logical :: found
      call ctx%kinfo(file, filtyp, srcfil, handle, found)
      call check(.not. ctx%failed() .and. found .and. srcfil == source .and. &
                 filtyp == 'SPK' .and. handle /= 0, what)
   end subroutine source_is

   !> Check whether the text kernel's variable is in the pool.
   subroutine variable_is(ctx, present, what)
      type(pepper_context), intent(inout) :: ctx  !! the context that looks
      logical, intent(in) :: present  !! the variable is expected
      character(*), intent(in) :: what  !! what is checked
      real(wp) :: values(2)
      integer(ip) :: n
      logical :: found
      call ctx%gdpool('PEPPER_TEST', 1_ip, 2_ip, n, values, found)
      if (present) then
         call check(.not. ctx%failed() .and. found .and. n == 2 .and. &
                    all(values == [1.5_wp, 2.5_wp]), what)
      else
         call check(.not. ctx%failed() .and. .not. found, what)
      end if
   end subroutine variable_is

   !> Check whether a segment for the body is found (with no SPK loaded, the
   !> search signals SPICE(NOLOADEDFILES), which is reset).
   subroutine segment_found(ctx, expected, what)
      type(pepper_context), intent(inout) :: ctx  !! the searching context
      logical, intent(in) :: expected  !! a segment is expected
      character(*), intent(in) :: what  !! what is checked
      integer(ip) :: h
      real(wp) :: descr(5)
      character(40) :: id
      character(80) :: msg
      logical :: found
      call ctx%spksfs(body, 50.0_wp, h, descr, id, found)
      if (expected) then
         call check(.not. ctx%failed() .and. found .and. id == 'SEGMENT', what)
      else
         call ctx%getmsg('SHORT', msg)
         call check(ctx%failed() .and. msg == 'SPICE(NOLOADEDFILES)', what)
         call ctx%reset()
      end if
   end subroutine segment_found

   !> Remove the test files that exist.
   subroutine delete_files()
      character(32) :: files(3)
      logical :: exists
      integer :: unit, k
      files = [character(32) :: spk, text, meta]
      do k = 1, size(files)
         inquire (file=trim(files(k)), exist=exists)
         if (exists) then
            open (newunit=unit, file=trim(files(k)), status='old')
            close (unit, status='delete')
         end if
      end do
   end subroutine delete_files

   !> Write a text kernel with the lines given.
   subroutine write_text(file, lines)
      character(*), intent(in) :: file  !! the file to write
      character(*), intent(in) :: lines(:)  !! its lines
      integer :: unit, k
      open (newunit=unit, file=file, status='new', action='write')
      write (unit, '(a)') 'KPL/PCK'
      do k = 1, size(lines)
         write (unit, '(a)') trim(lines(k))
      end do
      write (unit, '(a)') '\begintext'
      close (unit)
   end subroutine write_text

   !> Write an SPK-like DAF with one segment for the body (center 10, frame
   !> 1, type 2) covering [0, 100]: the segment buffer reads only the
   !> summaries and the names.
   subroutine write_spk()
      integer(ip) :: h
      real(wp) :: sum(nd + (ni + 1)/2)
      call a%dafonw(spk, 'SPK', nd, ni, 'PEPPER test SPK', 0_ip, h)
      call a%dafps(nd, ni, [0.0_wp, 100.0_wp], [body, 10_ip, 1_ip, 2_ip, 0_ip, 0_ip], sum)
      call a%dafbna(h, sum, 'SEGMENT')
      call a%dafada([1.0_wp, 2.0_wp, 3.0_wp], 3_ip)
      call a%dafena()
      call a%dafcls(h)
      call check(.not. a%failed(), 'write ' // spk)
   end subroutine write_spk

end program test_keeper
