!> DAF files through the module API: one context writes a DAF; the file
!> belongs to the instance, and two contexts search it at the same time,
!> interleaved, each with its own search state and units.
program test_daf

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   character(*), parameter :: file = 'test_daf.daf'
   ! The summaries: one double, and four integers: the array number, its
   ! length, and (filled in by DAF) the begin and end addresses.
   integer(ip), parameter :: nd = 1, ni = 4
   type(pepper_spice), target :: spice
   type(pepper_context) :: a, b
   integer(ip) :: handle
   integer :: failures = 0

   a = spice%new_context()
   b = spice%new_context()

   call delete_file()
   call write_daf()
   call a%dafopr(file, handle)
   call check(.not. a%failed(), 'dafopr')
   call test_interleaved_searches()
   call test_read_data()
   call a%dafcls(handle)
   call check(.not. a%failed(), 'dafcls')
   call delete_file()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_daf: all checks passed'

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

   !> Remove the test file if it exists.
   subroutine delete_file()
      logical :: exists
      integer :: unit
      inquire (file=file, exist=exists)
      if (exists) then
         open (newunit=unit, file=file, status='old')
         close (unit, status='delete')
      end if
   end subroutine delete_file

   !> Write a DAF with three arrays: array k has the values 100*k + (1, 2, ...)
   !> (k + 2 of them), and the summary (dc = k, ic = (k, k + 2)).
   subroutine write_daf()
      integer(ip) :: h, k, i
      real(wp) :: sum(nd + (ni + 1)/2)
      real(wp) :: data(10)
      call a%dafonw(file, 'TEST', nd, ni, 'PEPPER test DAF', 0_ip, h)
      call check(.not. a%failed(), 'dafonw')
      do k = 1, 3
         call a%dafps(nd, ni, [real(k, wp)], [k, k + 2, 0_ip, 0_ip], sum)
         call a%dafbna(h, sum, 'ARRAY')
         data(1:k + 2) = [(100.0_wp*k + i, i = 1, k + 2)]
         call a%dafada(data, k + 2)
         call a%dafena()
      end do
      call a%dafcls(h)
      call check(.not. a%failed(), 'write the DAF')
   end subroutine write_daf

   !> The summary's integer components (the array number and its length).
   subroutine array_of(ctx, k, n)
      type(pepper_context), intent(inout) :: ctx  !! the searching context
      integer(ip), intent(out) :: k  !! array number from the summary
      integer(ip), intent(out) :: n  !! array length from the summary
      real(wp) :: sum(nd + (ni + 1)/2), dc(nd)
      integer(ip) :: ic(ni)
      call ctx%dafgs(sum)
      call ctx%dafus(sum, nd, ni, dc, ic)
      k = ic(1)
      n = ic(2)
   end subroutine array_of

   !> Two searches of the same file, interleaved: each context keeps its own
   !> position.
   subroutine test_interleaved_searches()
      logical :: found
      integer(ip) :: k, n
      call a%dafbfs(handle)
      call b%dafbfs(handle)
      call a%daffna(found)                     ! a: array 1
      call check(found, 'a finds the first array')
      call b%daffna(found)                     ! b: array 1
      call b%daffna(found)                     ! b: array 2
      call array_of(b, k, n)
      call check(found .and. k == 2 .and. n == 4, 'b is at the second array')
      call array_of(a, k, n)
      call check(k == 1 .and. n == 3, 'a is still at the first array')
      call a%daffna(found)
      call a%daffna(found)
      call array_of(a, k, n)
      call check(found .and. k == 3, 'a is at the third array')
      call a%daffna(found)
      call check(.not. found, 'a: no more arrays')
      call b%daffna(found)
      call array_of(b, k, n)
      call check(found .and. k == 3, 'b is at the third array')
      call check(.not. a%failed() .and. .not. b%failed(), 'no errors in the searches')
   end subroutine test_interleaved_searches

   !> Both contexts read the data of the third array (each with its own unit).
   subroutine test_read_data()
      real(wp) :: sum(nd + (ni + 1)/2), dc(nd), data(5)
      integer(ip) :: ic(ni), i
      logical :: found
      ! The third array's addresses, from a search by b.
      call b%dafbfs(handle)
      do i = 1, 3
         call b%daffna(found)
      end do
      call b%dafgs(sum)
      call b%dafus(sum, nd, ni, dc, ic)
      call check(ic(4) - ic(3) + 1 == 5, 'addresses of the third array')
      call a%dafgda(handle, ic(3), ic(4), data)
      call check(.not. a%failed() .and. all(data == [(300.0_wp + i, i = 1, 5)]), &
                 'a reads the third array')
      call b%dafgda(handle, ic(3), ic(4), data)
      call check(.not. b%failed() .and. all(data == [(300.0_wp + i, i = 1, 5)]), &
                 'b reads the third array')
   end subroutine test_read_data

end program test_daf
