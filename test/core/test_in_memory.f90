!> Binary kernels loaded in memory (FURNSH's `in_memory`, the instance's
!> `set_in_memory`): the same states as from the file, to the last bit, in
!> every context; the memory is the file's size and is freed by UNLOAD and
!> KCLEAR; a meta-kernel passes the choice to its binary kernels; a file
!> loaded twice stays in memory until its last unload; a big-endian copy
!> of the file is translated when it is read into memory.
program test_in_memory

   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context
   use pepper_test_kernels, only: write_spk, delete_file

   implicit none

   character(*), parameter :: spk = 'test_in_memory.bsp'
   !> the same SPK for the instance reading the file (a file can be loaded
   !> by one instance at a time: gfortran connects a file to one unit)
   character(*), parameter :: spk_file = 'test_in_memory_file.bsp'
   character(*), parameter :: meta = 'test_in_memory.tm'
   character(*), parameter :: spk_big = 'test_in_memory_big.bsp'  !! the SPK in big-endian format
   integer(ip), parameter :: body = 1001, center = 399
   integer(ip), parameter :: nq = 1000  !! epochs compared
   type(pepper_spice), target :: from_file, in_memory
   type(pepper_context), pointer :: f, m
   type(pepper_context) :: m2
   integer(int64) :: size_bytes
   integer :: failures = 0

   f => from_file%context()
   m => in_memory%context()
   m2 = in_memory%new_context()

   call write_spk(f, spk, body, center, 7000.0_wp, 0.3_wp)
   call write_spk(f, spk_file, body, center, 7000.0_wp, 0.3_wp)
   call write_meta()
   call write_big_endian(spk, spk_big)
   inquire (file=spk, size=size_bytes)

   ! one instance reads the file as needed, the other has it in memory
   call from_file%furnsh(spk_file)
   call in_memory%furnsh(spk, in_memory=.true.)
   call check(.not. from_file%failed() .and. .not. in_memory%failed(), 'both instances load the SPK')
   call check(from_file%kernel_memory() == 0, 'the file read as needed takes no memory')
   call check(in_memory%kernel_memory() == size_bytes, 'the file in memory takes its size')
   call same_states(m, 'the instance''s context: the same states as from the file')
   call same_states(m2, 'another context: the same states as from the file')

   call in_memory%unload(spk)
   call check(in_memory%kernel_memory() == 0, 'UNLOAD frees the memory')
   call check(.not. found(m2), 'after UNLOAD the segment is gone')

   ! a meta-kernel passes in_memory to the binary kernels it lists
   call in_memory%furnsh(meta, in_memory=.true.)
   call check(in_memory%kernel_memory() == size_bytes, 'a meta-kernel loads its SPK in memory')
   call same_states(m2, 'loaded by a meta-kernel: the same states')
   call in_memory%kclear()
   call check(in_memory%kernel_memory() == 0, 'KCLEAR frees the memory')

   ! the instance's choice, and the argument overriding it
   call in_memory%set_in_memory(.true.)
   call in_memory%furnsh(spk)
   call check(in_memory%kernel_memory() == size_bytes, 'set_in_memory: FURNSH loads in memory')
   call in_memory%kclear()
   call in_memory%furnsh(spk, in_memory=.false.)
   call check(in_memory%kernel_memory() == 0, 'in_memory=.false. overrides set_in_memory')
   call in_memory%kclear()
   call in_memory%set_in_memory(.false.)

   ! loaded twice, once in memory: in memory until the last unload
   call in_memory%furnsh(spk)
   call in_memory%furnsh(spk, in_memory=.true.)
   call check(in_memory%kernel_memory() == size_bytes, 'a second load in memory puts the file in memory')
   call same_states(m2, 'loaded twice: the same states')
   call in_memory%unload(spk)
   call in_memory%unload(spk)
   call check(in_memory%kernel_memory() == 0, 'unloaded twice: the memory is freed')
   call check(.not. in_memory%failed(), 'no error in the instance in memory')

   ! a non-native file (big-endian) is translated into the copy
   call in_memory%furnsh(spk_big, in_memory=.true.)
   call check(.not. in_memory%failed(), 'the big-endian SPK loads in memory')
   call check(in_memory%kernel_memory() == size_bytes, 'the big-endian SPK takes its size')
   call same_states(m2, 'big-endian, in memory: the same states as the native file')
   call in_memory%kclear()

   call from_file%kclear()
   call in_memory%kclear()
   call delete_file(spk_big)
   call delete_file(spk)
   call delete_file(spk_file)
   call delete_file(meta)

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_in_memory: all checks passed'

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

   !> The states of `body` from `ctx` equal those of the instance reading
   !> the file, bitwise, at `nq` epochs over the segment.
   subroutine same_states(ctx, what)
      type(pepper_context), intent(inout) :: ctx  !! a context of the instance in memory
      character(*), intent(in) :: what  !! what is checked
      real(wp) :: et, s1(6), s2(6), lt
      integer(ip) :: k
      logical :: same
      same = .true.
      do k = 1, nq
         et = 1.0e6_wp*real(mod(k*7919_ip, nq), wp)/nq
         call f%spkgeo(body, et, 'J2000', center, s1, lt)
         call ctx%spkgeo(body, et, 'J2000', center, s2, lt)
         same = same .and. all(s1 == s2)
      end do
      call check(same .and. .not. ctx%failed() .and. .not. f%failed(), what)
   end subroutine same_states

   !> The segment of `body` is found by `ctx`.
   logical function found(ctx)
      type(pepper_context), intent(inout) :: ctx  !! the context
      real(wp) :: s(6), lt
      call ctx%spkgeo(body, 0.0_wp, 'J2000', center, s, lt)
      found = .not. ctx%failed()
      call ctx%reset()
   end function found

   !> Write `to`, the DAF `from` (native little-endian, as written on this
   !> machine) in the big-endian binary file format: in the file record, the
   !> integers and the format name; in the summary records, the doubles
   !> (control words and the doubles of the summaries) and the integers
   !> (packed two to a double); the data records' doubles. The name records
   !> are characters, unchanged.
   subroutine write_big_endian(from, to)
      character(*), intent(in) :: from  !! the native file
      character(*), intent(in) :: to  !! the big-endian file
      integer(int64) :: n
      integer :: unit, nd, ni, ss, nsum, rec, next, k, j, off
      character(1), allocatable :: b(:)
      logical, allocatable :: done(:)
      inquire (file=from, size=n)
      allocate (b(n))
      open (newunit=unit, file=from, access='stream', form='unformatted', status='old', action='read')
      read (unit) b
      close (unit)
      allocate (done(n/1024))
      done = .false.
      nd = transfer(b(9:12), nd)
      ni = transfer(b(13:16), ni)
      rec = transfer(b(77:80), rec)  ! FWARD: the first summary record
      ! the file record: ND, NI, FWARD, BWARD, FREE, and the format name
      do k = 9, 85, 4
         if (k == 9 .or. k == 13 .or. k == 77 .or. k == 81 .or. k == 85) b(k:k + 3) = b(k + 3:k:-1)
      end do
      b(89:96) = transfer('BIG-IEEE', b(1:8))
      done(1) = .true.
      ss = nd + (ni + 1)/2  ! the size of a summary (doubles)
      do while (rec > 0)
         off = (rec - 1)*1024
         next = nint(transfer(b(off + 1:off + 8), 1.0_wp))
         nsum = nint(transfer(b(off + 17:off + 24), 1.0_wp))
         do k = 0, 2  ! the control words
            j = off + 8*k + 1
            b(j:j + 7) = b(j + 7:j:-1)
         end do
         do k = 0, nsum - 1
            do j = 0, nd - 1  ! the doubles of the summary
               b(off + 24 + 8*(k*ss + j) + 1:off + 24 + 8*(k*ss + j) + 8) = &
                  b(off + 24 + 8*(k*ss + j) + 8:off + 24 + 8*(k*ss + j) + 1:-1)
            end do
            do j = 0, ni - 1  ! its integers
               b(off + 24 + 8*(k*ss + nd) + 4*j + 1:off + 24 + 8*(k*ss + nd) + 4*j + 4) = &
                  b(off + 24 + 8*(k*ss + nd) + 4*j + 4:off + 24 + 8*(k*ss + nd) + 4*j + 1:-1)
            end do
         end do
         done(rec) = .true.
         done(rec + 1) = .true.  ! its name record
         rec = next
      end do
      do k = 1, size(done)  ! the data records: doubles
         if (done(k)) cycle
         do j = 0, 127
            off = (k - 1)*1024 + 8*j + 1
            b(off:off + 7) = b(off + 7:off:-1)
         end do
      end do
      call delete_file(to)
      open (newunit=unit, file=to, access='stream', form='unformatted', status='new', action='write')
      write (unit) b
      close (unit)
   end subroutine write_big_endian

   !> A meta-kernel that loads the SPK.
   subroutine write_meta()
      integer :: unit
      call delete_file(meta)
      open (newunit=unit, file=meta, status='new', action='write')
      write (unit, '(a)') 'KPL/MK', '\begindata', 'KERNELS_TO_LOAD = ( ''' // spk // ''' )', '\begintext'
      close (unit)
   end subroutine write_meta

end program test_in_memory
