!> DAF files through the module API: one context writes a DAF; the file
!> belongs to the instance, and two contexts search it at the same time,
!> interleaved, each with its own search state and units.
program test_daf

   use pepper_kinds, only: ip, wp
   use pepper_instance_module, only: pepper_instance, pepper_context

   implicit none

   character(*), parameter :: file = 'test_daf.daf'
   ! The summaries: one double, and four integers: the array number, its
   ! length, and (filled in by DAF) the begin and end addresses.
   integer(ip), parameter :: nd = 1, ni = 4
   type(pepper_instance), target :: spice
   type(pepper_context) :: a, b
   integer(ip) :: handle
   integer :: failures = 0

   a = spice%new_context()
   b = spice%new_context()

   call test_pack_unpack()
   call delete_file()
   call write_daf()
   call test_units_closed()
   call a%dafopr(file, handle)
   call check(.not. a%failed(), 'dafopr')
   call test_interleaved_searches()
   call test_read_data()
   call a%dafcls(handle)
   call check(.not. a%failed(), 'dafcls')
   call test_rewrite_seen()
   call test_text_transfer()
   ! b's units for the closed file (Windows cannot delete an open file)
   call b%close_unloaded_files()
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
      logical :: still_open  !! the file is connected to a unit (Windows can't delete it)
      inquire (file=file, exist=exists)
      if (exists) then
         inquire (file=file, opened=still_open)
         if (still_open) error stop 'a file to delete is still open (Windows cannot delete it): '//file
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

   !> DAFPS and DAFUS: integers whose packed words are NaN patterns or
   !> denormals as reals (negative, small) round trip bit for bit; an odd
   !> number of integers leaves the last word's second half zero; nothing
   !> is written past the summary.
   subroutine test_pack_unpack()
      integer(ip), parameter :: ics(5) = [-1_ip, -82_ip, huge(0_ip), -huge(0_ip) - 1_ip, 3_ip]
      real(wp) :: sum(6), dc(2)
      integer(ip) :: ic(5), halves(2)
      sum = 7.0_wp
      call a%dafps(2_ip, 5_ip, [1.5_wp, -2.5_wp], ics, sum)
      call a%dafus(sum, 2_ip, 5_ip, dc, ic)
      call check(all(dc == [1.5_wp, -2.5_wp]) .and. all(ic == ics), 'dafps/dafus round trip')
      halves = transfer(sum(3), 0_ip, 2)
      call check(all(halves == ics(1:2)), 'dafps: two integers to a word, in order')
      halves = transfer(sum(5), 0_ip, 2)
      call check(halves(1) == ics(5) .and. halves(2) == 0, 'dafps: odd NI, the last half word is zero')
      call check(sum(6) == 7.0_wp, 'dafps: nothing past the summary')
      sum = 7.0_wp
      call a%dafps(2_ip, 0_ip, [1.5_wp, -2.5_wp], ic, sum)
      call check(all(sum(3:) == 7.0_wp), 'dafps: NI = 0, no integer words')
   end subroutine test_pack_unpack

   !> DAFB2T writes a DAF in the old text transfer format, DAFT2B reads it
   !> back (its doubles through decimal_value): the same summaries and data,
   !> bit for bit. DAFB2T writes the doubles with list-directed WRITE, whose
   !> number of digits is the compiler's (gfortran 17, enough for any
   !> double; ifx about 15, so 1/3 or HUGE do not come back): the values are
   !> decimals of at most 15 significant digits, which any compiler writes
   !> back exactly (DBL_DIG), from 1E-300 to 1E300.
   subroutine test_text_transfer()
      use, intrinsic :: iso_fortran_env, only: int64
      character(*), parameter :: daf1 = 'test_daf_xfr1.daf', daf2 = 'test_daf_xfr2.daf', xfr = 'test_daf.xfr'
      ! one double and two integers per summary: the integers are the
      ! array's begin and end addresses (set by DAF)
      real(wp) :: values(12, 2), r(12), sum1(2), sum2(2), dc1(1), dc2(1), data1(12), data2(12)
      integer(ip) :: h1, h2, k, ic1(2), ic2(2)
      integer :: unit
      logical :: found1, found2, same
      character(30) :: text
      call remove(daf1)
      call remove(daf2)
      call remove(xfr)
      call random_number(r)
      values(:, 1) = [0.333333333333333_wp, -0.285714285714286_wp, 31415926535.8979_wp, 1.0e-300_wp, &
                      -0.1_wp, 0.0_wp, 1.0e300_wp, 2.2250738585073e-308_wp, 123456789012345.0_wp, &
                      6.02214076e23_wp, 1.0_wp, -1.5e-7_wp]
      do k = 1, 12  ! random decimals of 15 significant digits, 1E-300 to 1E300
         write (text, '(es23.14e3)') (r(k) - 0.5_wp)*10.0_wp**int(r(k)*600 - 300)
         read (text, *) values(k, 2)
      end do
      call a%dafonw(daf1, 'TEST', 1_ip, 2_ip, 'PEPPER transfer test', 0_ip, h1)
      do k = 1, 2
         call a%dafps(1_ip, 2_ip, [values(3, k)], [0_ip, 0_ip], sum1)
         call a%dafbna(h1, sum1, 'ARRAY')
         call a%dafada(values(:, k), 12_ip)
         call a%dafena()
      end do
      call a%dafcls(h1)
      open (newunit=unit, file=xfr, status='new', action='write')
      call a%dafb2t(daf1, int(unit, ip))
      close (unit)
      open (newunit=unit, file=xfr, status='old', action='read')
      call a%daft2b(int(unit, ip), daf2, 0_ip)
      close (unit)
      call check(.not. a%failed(), 'text transfer: DAFB2T and DAFT2B')
      call a%dafopr(daf1, h1)
      call b%dafopr(daf2, h2)
      call a%dafbfs(h1)
      call b%dafbfs(h2)
      same = .true.
      do k = 1, 2
         call a%daffna(found1)
         call b%daffna(found2)
         same = same .and. found1 .and. found2
         if (.not. same) exit
         call a%dafgs(sum1)
         call b%dafgs(sum2)
         call a%dafus(sum1, 1_ip, 2_ip, dc1, ic1)
         call b%dafus(sum2, 1_ip, 2_ip, dc2, ic2)
         call a%dafgda(h1, ic1(1), ic1(2), data1)
         call b%dafgda(h2, ic2(1), ic2(2), data2)
         same = same .and. transfer(dc1(1), 0_int64) == transfer(dc2(1), 0_int64) &
                .and. all(transfer(data2, 0_int64, 12) == transfer(values(:, k), 0_int64, 12)) &
                .and. all(transfer(data1, 0_int64, 12) == transfer(values(:, k), 0_int64, 12))
      end do
      call a%daffna(found1)
      call b%daffna(found2)
      call check(same .and. .not. found1 .and. .not. found2 .and. .not. a%failed() .and. .not. b%failed(), &
                 'text transfer: the same summaries and data, bit for bit')
      call a%dafcls(h1)
      call b%dafcls(h2)
      call b%close_unloaded_files()
      call remove(daf1)
      call remove(daf2)
      call remove(xfr)
   end subroutine test_text_transfer

   !> Remove a file if it exists.
   subroutine remove(name)
      character(*), intent(in) :: name  !! the file
      integer :: unit, ios
      open (newunit=unit, file=name, status='old', iostat=ios)
      if (ios == 0) close (unit, status='delete')
   end subroutine remove

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
   !> A context that read a file closes its unit when it goes away: c reads
   !> through a unit of its own, a closes the file, and once c is gone the file
   !> is not connected to any unit.
   subroutine test_units_closed()
      integer(ip) :: h
      real(wp) :: sum(nd + (ni + 1)/2)
      logical :: found, opened
      call a%dafopr(file, h)
      block
         type(pepper_context) :: c
         c = spice%new_context()
         call c%dafbfs(h)
         call c%daffna(found)
         call c%dafgs(sum)
         call check(.not. c%failed() .and. found, 'c reads the DAF')
      end block
      call a%dafcls(h)
      inquire (file=file, opened=opened)
      call check(.not. opened, 'no unit is connected to the file after c is gone')
   end subroutine test_units_closed

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

   !> A record rewritten through one context is seen by another context that
   !> had it in its record buffer.
   subroutine test_rewrite_seen()
      integer(ip) :: h, recno, i
      real(wp) :: rec(128), back(128)
      logical :: found
      call a%dafopw(file, h)
      call check(.not. a%failed(), 'dafopw')
      ! The first data record is the record after the first summary and
      ! name records; any record works here: use record 4 (data).
      recno = 4
      call b%dafgdr(h, recno, 1_ip, 128_ip, back, found)      ! now in b's buffer
      call check(found .and. .not. b%failed(), 'b reads the record')
      rec = [(real(-i, wp), i = 1, 128)]
      call a%dafwdr(h, recno, rec)
      call check(.not. a%failed(), 'a rewrites the record')
      call b%dafgdr(h, recno, 1_ip, 128_ip, back, found)
      call check(found .and. all(back == rec), 'b sees the rewritten record')
      call a%dafgdr(h, recno, 1_ip, 128_ip, back, found)
      call check(found .and. all(back == rec), 'a sees its own write')
      call a%dafcls(h)
   end subroutine test_rewrite_seen

end program test_daf
