!> Kernels built in memory and kernel loaders (PEPPER;
!> plans/external_loaders_plan.md).
!>
!> - Programmatic kernels: a kernel_builder loaded with load_kernel gives
!>   bitwise the states of an SPK file written by SPKW09/SPKW13 from the
!>   same arrays; KDATA, KTOTAL, SPKCOV, UNLOAD; replacing by name; keep;
!>   more segments than one summary record holds; write_kernel; the
!>   builder's checks and adapt; an unknown frame at the load.
!> - A kernel loader, for the made-up format of pepper_test_loader: FURNSH
!>   by extension, by detection, by name; reserved names; conflicts; SPICE
!>   binary files never claimed; errors with the line, loading nothing;
!>   unsupported interpolation; discontinuities, gaps, bounds; the
!>   loader's kept data and unload callbacks; meta-kernels with
!>   KERNEL_LOADERS.
!> - Body names a kernel defines (define_body): usable while it is
!>   loaded, in contexts made before the load too; removed by UNLOAD and
!>   KCLEAR, an earlier BODDEF of the name in effect again; the latest
!>   kernel wins; the kernel pool's names take precedence.
!> - The round trip: test format -> kernel -> .bsp file -> test format
!>   (from the SPK's segments) -> kernel: the same states, bitwise, and
!>   the same rows.
program test_loaders

   use pepper_module, only: ip, wp, pepper_instance, pepper_context, kernel_builder, kernel_loader
   use pepper_test_kernels, only: circle_state, delete_file
   use pepper_test_loader, only: simple_loader, write_simple

   implicit none

   integer, parameter :: n = 150  !! rows of the test trajectory (more than 100: an epoch directory)
   real(wp), parameter :: step = 60.0_wp  !! seconds between rows
   real(wp), parameter :: radius = 7000.0_wp  !! km
   integer(ip), parameter :: sc = -10001  !! the test body
   character(*), parameter :: ref_bsp = 'test_loaders_ref.bsp'  !! SPKW13 and SPKW09 from the arrays
   character(*), parameter :: prog_bsp = 'test_loaders_prog.bsp'  !! a programmatic kernel written out
   character(*), parameter :: many_bsp = 'test_loaders_many.bsp'  !! 60 segments written out
   character(*), parameter :: sim_a = 'test_loaders_a.sim'  !! the test format, by extension
   character(*), parameter :: sim_b = 'test_loaders_b.txt'  !! the same, found by detect
   character(*), parameter :: sim_c = 'test_loaders_c.dat'  !! the same, by name only
   character(*), parameter :: rt_sim = 'test_loaders_rt.sim'  !! the round trip's file
   character(*), parameter :: rt_bsp = 'test_loaders_rt.bsp'  !! ... written to an SPK
   character(*), parameter :: rt_back = 'test_loaders_rt_back.sim'  !! ... and back
   character(*), parameter :: bad_sim = 'test_loaders_bad.sim'  !! broken files
   character(*), parameter :: meta = 'test_loaders.tm'  !! a meta-kernel
   character(*), parameter :: meta_bad = 'test_loaders_bad.tm'  !! KERNEL_LOADERS not in pairs
   character(*), parameter :: m1 = 'test_loaders_m1.sim'  !! listed by the meta-kernel
   character(*), parameter :: m2 = 'test_loaders_m2.dat'  !! listed, with a loader in KERNEL_LOADERS
   type(pepper_instance), target :: spice, ref, other
   type(pepper_context), pointer :: ctx, rctx, octx
   real(wp) :: epochs(n), states(6, n)
   integer :: failures = 0, k

   ctx => spice%context()
   rctx => ref%context()
   octx => other%context()
   call delete_files()
   epochs = [(step*(k - 1), k = 1, n)]
   do k = 1, n
      states(:, k) = circle_state(radius, 0.3_wp, epochs(k))
   end do
   call write_reference()

   call test_programmatic()
   call test_builder_checks()
   call test_many_segments()
   call test_loader()
   call test_choice()
   call test_round_trip()
   call test_discontinuities()
   call test_file_errors()
   call test_meta_kernel()
   call test_names()

   call spice%kclear()
   call ref%kclear()
   call other%kclear()
   call delete_files()
   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_loaders: all checks passed'

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

   !> Check that `c` failed with the short message `short` (and reset it).
   subroutine check_error(c, short, what)
      type(pepper_context), intent(inout) :: c  !! the context
      character(*), intent(in) :: short  !! the expected short message
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: msg
      if (.not. c%failed()) then
         call check(.false., what//' (no error)')
         return
      end if
      call c%getmsg('SHORT', msg)
      if (trim(msg) /= short) print '(4a)', '   ', trim(msg), ' instead of ', short
      call check(trim(msg) == short, what)
      call c%reset()
   end subroutine check_error

   !> Check that `c` has no error (reporting it, and resetting).
   subroutine check_ok(c, what)
      type(pepper_context), intent(inout) :: c  !! the context
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: msg
      logical :: ok
      ok = .not. c%failed()
      if (.not. ok) then
         call c%getmsg('LONG', msg)
         print '(2a)', '   ', trim(msg)
         call c%reset()
      end if
      call check(ok, what)
   end subroutine check_ok

   !> The state of `body` relative to `center` in J2000 at `et` (zeros and
   !> `ok` false on an error, which is reset).
   subroutine state_of(c, body, center, et, state, ok)
      type(pepper_context), intent(inout) :: c  !! the context
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! the center
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state
      logical, intent(out) :: ok  !! no error
      real(wp) :: lt
      call c%spkgeo(body, et, 'J2000', center, state, lt)
      ok = .not. c%failed()
      if (.not. ok) then
         state = 0.0_wp
         call c%reset()
      end if
   end subroutine state_of

   !> True if two contexts give bitwise the same states of `body` at
   !> `nt` epochs spread over [t0, t1] (the ends and many points between
   !> rows).
   logical function same_states(a, b, body, t0, t1, nt) result(same)
      type(pepper_context), intent(inout) :: a  !! a context
      type(pepper_context), intent(inout) :: b  !! another context
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: t0  !! the first epoch
      real(wp), intent(in) :: t1  !! the last epoch
      integer, intent(in) :: nt  !! the number of epochs
      real(wp) :: sa(6), sb(6), et
      logical :: oka, okb
      integer :: i
      same = .true.
      do i = 1, nt
         et = t0 + (t1 - t0)*real(i - 1, wp)/real(nt - 1, wp)
         call state_of(a, body, 399_ip, et, sa, oka)
         call state_of(b, body, 399_ip, et, sb, okb)
         if (.not. (oka .and. okb) .or. any(sa /= sb)) then
            if (same) print '(a, es24.16, 2l2)', '   first difference at ', et, oka, okb
            same = .false.
         end if
      end do
   end function same_states

   !> The reference SPK, written by SPKW13 (body SC) and SPKW09 (body
   !> SC-1) from the trajectory's arrays, loaded in REF.
   subroutine write_reference()
      integer(ip) :: h
      call rctx%spkopn(ref_bsp, 'REFERENCE', 0_ip, h)
      call rctx%spkw13(h, sc, 399_ip, 'J2000', epochs(1), epochs(n), 'REF13', 7_ip, int(n, ip), states, epochs)
      call rctx%spkw09(h, sc - 1, 399_ip, 'J2000', epochs(1), epochs(n), 'REF09', 8_ip, int(n, ip), states, epochs)
      call rctx%spkcls(h)
      call ref%furnsh(ref_bsp)
      call check_ok(rctx, 'the reference SPK')
   end subroutine write_reference

   !> A programmatic kernel: the same arrays as the reference, the same
   !> states; the kernel table, coverage, write_kernel, replacing, keep,
   !> unload, and an unknown frame.
   subroutine test_programmatic()
      type(kernel_builder) :: kb, kb2
      character(:), allocatable :: loader, format
      character(256) :: file, srcfil
      character(32) :: filtyp
      real(wp), allocatable :: cover(:, :)
      real(wp) :: s(6), s2(6)
      integer(ip) :: count, handle
      logical :: found, ok, ok2

      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', epochs, states, 7_ip, ident='PROG13')
      call kb%spk_lagrange(ctx, sc - 1, 399_ip, 'J2000', epochs, states, 8_ip, ident='PROG09')
      call check(kb%nsegments() == 2, 'the builder has two segments')
      call spice%load_kernel('prog', kb)
      call check_ok(ctx, 'load_kernel')
      call check(kb%nsegments() == 0, 'the builder is emptied by the load')

      call check(same_states(ctx, rctx, sc, epochs(1), epochs(n), 997), &
                 'programmatic kernel: type 13 states bitwise those of SPKW13')
      call check(same_states(ctx, rctx, sc - 1, epochs(1), epochs(n), 997), &
                 'programmatic kernel: type 9 states bitwise those of SPKW09')

      call ctx%ktotal('SPK', count)
      call check(count == 1, 'KTOTAL counts it as an SPK')
      call ctx%kdata(1_ip, 'SPK', file, filtyp, srcfil, handle, found)
      call check(found .and. file == 'prog' .and. filtyp == 'SPK' .and. handle > 0, 'KDATA: its name, type SPK, a handle')
      call ctx%kernel_format('prog', loader, format, found)
      call check(found .and. loader == ' ' .and. format == '(programmatic)', 'kernel_format of a programmatic kernel')
      call ctx%spk_coverage('prog', sc, cover)
      call check(size(cover, 2) == 1, 'SPKCOV by the kernel''s name: one interval')
      if (size(cover, 2) == 1) call check(cover(1, 1) == epochs(1) .and. cover(2, 1) == epochs(n), 'its coverage')
      call check_comments(handle)

      ! to a real SPK file: loaded on its own, the same states
      call spice%write_kernel('prog', prog_bsp)
      call check_ok(ctx, 'write_kernel')
      call other%furnsh(prog_bsp)
      call check_ok(octx, 'the written SPK loads')
      call check(same_states(ctx, octx, sc, epochs(1), epochs(n), 501), 'the written SPK: the same states (type 13)')
      call check(same_states(ctx, octx, sc - 1, epochs(1), epochs(n), 501), 'the written SPK: the same states (type 9)')
      call other%unload(prog_bsp)
      call spice%write_kernel('prog', prog_bsp)
      call check_error(ctx, 'SPICE(FILEEXISTS)', 'write_kernel does not write over a file')
      call spice%write_kernel('no such kernel', 'test_loaders_none.bsp')
      call check_error(ctx, 'SPICE(NOSUCHKERNEL)', 'write_kernel of an unknown kernel')

      ! replacing by name: the new data, one kernel
      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', epochs, 2.0_wp*states, 7_ip)
      call spice%load_kernel('prog', kb)
      call check_ok(ctx, 'load_kernel again under the same name')
      call ctx%ktotal('SPK', count)
      call check(count == 1, 'the kernel is replaced (one SPK loaded)')
      call state_of(ctx, sc, 399_ip, epochs(10), s, ok)
      call check(ok .and. maxval(abs(s - 2.0_wp*states(:, 10))) < 1.0e-8_wp, 'the replacement''s states')
      call state_of(ctx, sc - 1, 399_ip, epochs(10), s, ok)
      call check(.not. ok, 'the replaced kernel''s other body is gone')

      ! keep: one builder loaded into two instances
      call kb2%spk_hermite(ctx, sc - 2, 399_ip, 'J2000', epochs, states, 5_ip)
      call spice%load_kernel('kept', kb2, keep=.true.)
      call check(kb2%nsegments() == 1, 'keep: the builder keeps its segments')
      call other%load_kernel('kept', kb2)
      call check_ok(octx, 'the kept builder loaded into another instance')
      call state_of(ctx, sc - 2, 399_ip, 1000.0_wp, s, ok)
      call state_of(octx, sc - 2, 399_ip, 1000.0_wp, s2, ok2)
      call check(ok .and. ok2 .and. all(s == s2), 'keep: the same states in both instances')

      ! unload
      call spice%unload('prog')
      call state_of(ctx, sc, 399_ip, epochs(10), s, ok)
      call check(.not. ok, 'UNLOAD of a programmatic kernel')
      call spice%unload('kept')
      call other%unload('kept')

      ! an unknown frame, found at the load: nothing loaded, the builder unchanged
      call kb%clear()
      call kb%spk_hermite(ctx, sc, 399_ip, 'NOSUCHFRAME', epochs, states, 7_ip)
      call check_ok(ctx, 'a segment in an unknown frame can be added')
      call spice%load_kernel('badframe', kb)
      call check_error(ctx, 'SPICE(INVALIDREFFRAME)', 'an unknown frame at load_kernel')
      call check(kb%nsegments() == 1, 'the builder is unchanged after a failed load')
      call ctx%ktotal('ALL', count)
      call check(count == 0, 'a failed load loads nothing')
      call spice%load_kernel(' ', kb)
      call check_error(ctx, 'SPICE(BLANKFILENAME)', 'a blank kernel name')
   end subroutine test_programmatic

   !> A DAF built in memory has no comments.
   subroutine check_comments(handle)
      use pepper_containers, only: string
      integer(ip), intent(in) :: handle  !! the handle
      type(string), allocatable :: lines(:)
      call ctx%daf_comments(handle, lines)
      call check_ok(ctx, 'daf_comments of a kernel built in memory')
      call check(size(lines) == 0, 'a kernel built in memory has no comments')
   end subroutine check_comments

   !> The builder's checks (SPKW09's and SPKW13's), and adapt.
   subroutine test_builder_checks()
      type(kernel_builder) :: kb
      real(wp) :: e(3), s(6, 3)
      e = [0.0_wp, 60.0_wp, 120.0_wp]
      s = states(:, 1:3)
      call kb%spk_lagrange(ctx, sc, 399_ip, 'J2000', e(1:2), s(:, 1:2), 8_ip)
      call check_error(ctx, 'SPICE(TOOFEWSTATES)', 'too few states for the degree')
      call kb%spk_lagrange(ctx, sc, 399_ip, 'J2000', e(1:2), s(:, 1:2), 8_ip, adapt=.true.)
      call check_ok(ctx, 'adapt: two states')
      call check(kb%nsegments() == 1, 'adapt: two states make a segment')
      call kb%spk_lagrange(ctx, sc, 399_ip, 'J2000', e(1:1), s(:, 1:1), 8_ip, adapt=.true.)
      call check_ok(ctx, 'adapt: one state')
      call check(kb%nsegments() == 1, 'adapt: one state is dropped')
      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', e, s, 6_ip)
      call check_error(ctx, 'SPICE(INVALIDDEGREE)', 'an even Hermite degree')
      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', e, s, 29_ip)
      call check_error(ctx, 'SPICE(INVALIDDEGREE)', 'a degree above 27')
      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', [0.0_wp, 120.0_wp, 60.0_wp], s, 3_ip)
      call check_error(ctx, 'SPICE(TIMESOUTOFORDER)', 'epochs out of order')
      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', e, s, 3_ip, tbeg=-1.0_wp)
      call check_error(ctx, 'SPICE(BADDESCRTIMES)', 'bounds outside the epochs')
      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', e, s(:, 1:2), 3_ip)
      call check_error(ctx, 'SPICE(INVALIDSIZE)', 'states of the wrong shape')
      call kb%spk_hermite(ctx, 399_ip, 399_ip, 'J2000', e, s, 3_ip)
      call check_error(ctx, 'SPICE(BARYCENTEREPHEM)', 'a body that is its own center')
      call kb%spk_hermite(ctx, sc, 399_ip, 'J2000', e, s, 3_ip, ident=repeat('x', 41))
      call check_error(ctx, 'SPICE(SEGIDTOOLONG)', 'an identifier longer than 40 characters')
      call check(kb%nsegments() == 1, 'failed adds add nothing')
   end subroutine test_builder_checks

   !> More segments than a summary record holds (25): 60 segments of 6
   !> states, sharing their end epochs.
   subroutine test_many_segments()
      type(kernel_builder) :: kb
      real(wp), allocatable :: cover(:, :)
      real(wp) :: s(6)
      integer :: i, j
      logical :: ok
      do i = 1, 60
         j = 2*(i - 1) + 1
         call kb%spk_hermite(ctx, sc - 3, 399_ip, 'J2000', epochs(j:j + 2), states(:, j:j + 2), 3_ip)
      end do
      call check(kb%nsegments() == 60, '60 segments')
      call spice%load_kernel('many', kb)
      call check_ok(ctx, 'a kernel of 60 segments')
      call ctx%spk_coverage('many', sc - 3, cover)
      call check(size(cover, 2) == 1, '60 touching segments: one interval')
      do i = 1, 121
         call state_of(ctx, sc - 3, 399_ip, epochs(i), s, ok)
         if (.not. ok .or. maxval(abs(s - states(:, i))) > 1.0e-8_wp) then
            call check(.false., '60 segments: the states at the rows')
            exit
         end if
      end do
      call spice%write_kernel('many', many_bsp)
      call other%furnsh(many_bsp)
      call check(same_states(ctx, octx, sc - 3, epochs(1), epochs(121), 1201), '60 segments written to a file')
      call other%unload(many_bsp)
      call spice%unload('many')
   end subroutine test_many_segments

   !> The test loader by extension and by detection; its kept data.
   subroutine test_loader()
      type(simple_loader) :: ld
      class(kernel_loader), pointer :: p
      character(:), allocatable :: loader, format
      integer(ip) :: count
      logical :: found

      call spice%add_loader('Simple', ld, extensions=['.sim'])
      call check_ok(ctx, 'add_loader')
      call write_simple(sim_a, [character(40) :: 'OBJECT = -10001', 'CENTER = EARTH', 'FRAME = J2000', &
                                                 'INTERPOLATION = HERMITE 7'], epochs, states, [1], [n])
      call spice%furnsh(sim_a)
      call check_ok(ctx, 'FURNSH of a file of the test format (by extension)')
      call check(same_states(ctx, rctx, sc, epochs(1), epochs(n), 997), &
                 'loaded file: the states of SPKW13 from the same rows, bitwise')
      call ctx%kernel_format(sim_a, loader, format, found)
      call check(found .and. loader == 'SIMPLE' .and. format == 'SIMPLE_EPHEMERIS (test format)', &
                 'kernel_format: the loader and the description')
      call ctx%ktotal('SPK', count)
      call check(count == 1, 'the file is an SPK for KTOTAL')

      call spice%loader('SIMPLE', p)
      call check(associated(p), 'loader() finds the loader')
      if (associated(p)) then
         select type (p)
         type is (simple_loader)
            call check(p%loads == 1 .and. p%rows_of(sim_a) == n, 'the loader kept the rows of the file')
         class default
            call check(.false., 'the loader''s type')
         end select
      end if

      ! by detection: the same content, an extension no loader has
      call write_simple(sim_b, [character(40) :: 'OBJECT = -10002', 'CENTER = 399', 'FRAME = J2000', &
                                                 'INTERPOLATION = LAGRANGE 8'], epochs, states, [1], [n])
      call spice%furnsh(sim_b)
      call check_ok(ctx, 'FURNSH of a file found by detect')
      call ctx%kernel_format(sim_b, loader, format, found)
      call check(found .and. loader == 'SIMPLE', 'detect: read by the loader')
      call check(same_states(ctx, rctx, sc - 1, epochs(1), epochs(n), 997), &
                 'detected file: the states of SPKW09 from the same rows, bitwise')

      ! unload: the callback, the kept data freed
      call spice%unload(sim_a)
      call spice%loader('SIMPLE', p)
      select type (p)
      type is (simple_loader)
         call check(p%unloads == 1 .and. p%rows_of(sim_a) == -1, 'UNLOAD calls the loader''s unload')
      end select
      call spice%unload(sim_b)
   end subroutine test_loader

   !> How FURNSH chooses: by name, reserved names, conflicts, SPICE files
   !> never claimed, unknown loaders.
   subroutine test_choice()
      type(simple_loader) :: quiet
      character(:), allocatable :: loader, format
      character(256) :: file, srcfil
      character(32) :: filtyp
      integer(ip) :: handle
      logical :: found

      quiet%sniff = .false.
      call spice%add_loader('NOSNIFF', quiet)
      call write_simple(sim_c, [character(40) :: 'OBJECT = -10001', 'CENTER = 399', 'FRAME = J2000', &
                                                 'INTERPOLATION = HERMITE 7'], epochs, states, [1], [n])
      call spice%furnsh(sim_c, loader='nosniff')
      call check_ok(ctx, 'FURNSH with loader=')
      call ctx%kernel_format(sim_c, loader, format, found)
      call check(found .and. loader == 'NOSNIFF', 'loader= chooses the loader')
      call check(same_states(ctx, rctx, sc, epochs(1), epochs(n), 101), 'loader=: the states')
      call spice%unload(sim_c)

      ! a reserved name: PEPPER's own loaders (a text kernel, here)
      call spice%furnsh(sim_a, loader='TEXT')
      call check_ok(ctx, 'FURNSH with a reserved loader name')
      call ctx%kdata(1_ip, 'ALL', file, filtyp, srcfil, handle, found)
      call check(found .and. filtyp == 'TEXT', 'a reserved name: loaded by PEPPER''s own loaders')
      call spice%unload(sim_a)

      call spice%furnsh(sim_a, loader='nope')
      call check_error(ctx, 'SPICE(UNKNOWNLOADER)', 'an unknown loader')
      call spice%add_loader('text', quiet)
      call check_error(ctx, 'SPICE(RESERVEDLOADERNAME)', 'a reserved name cannot be registered')
      call spice%add_loader('SIMPLE', quiet)
      call check_error(ctx, 'SPICE(LOADERCONFLICT)', 'a name registered twice')
      call spice%add_loader('OTHER', quiet, extensions=['.SIM'])
      call check_error(ctx, 'SPICE(LOADERCONFLICT)', 'an extension registered twice')
      call spice%add_loader('OTHER', quiet, extensions=['.SIM'], replace=.true.)
      call check_ok(ctx, 'an extension taken over with replace')
      call spice%furnsh(sim_a)
      call ctx%kernel_format(sim_a, loader, format, found)
      call check(found .and. loader == 'OTHER', 'the extension''s new loader')
      call spice%unload(sim_a)
      call spice%remove_loader('OTHER')
      call spice%add_loader('SIMPLE', quiet, extensions=['.sim'], replace=.true.)
      call check_ok(ctx, 'a loader replaced')

      ! SPICE's binary kernels are never claimed by extension or detect
      call spice%add_loader('BSP', quiet, extensions=['.bsp'])
      call spice%furnsh(prog_bsp)
      call ctx%kernel_format(prog_bsp, loader, format, found)
      call check(found .and. loader == ' ', 'an SPK file is loaded as an SPK, whatever loaders claim .bsp')
      call spice%unload(prog_bsp)
      call spice%furnsh(prog_bsp, loader='simple')
      call check_error(ctx, 'SPICE(BADSIMPLEFILE)', 'a loader named for an SPK file reads it (and fails)')
      call ctx%ktotal('ALL', handle)
      call check(handle == 0, 'nothing loaded after the loader failed')
      call spice%remove_loader('BSP')
      call spice%remove_loader('NOSNIFF')
      call spice%remove_loader('SIMPLE')
   end subroutine test_choice

   !> The round trip: test format -> kernel -> .bsp -> test format -> kernel.
   subroutine test_round_trip()
      type(simple_loader) :: ld
      real(wp), allocatable :: s2(:, :)
      integer(ip) :: h1, h2
      integer :: nseg1, nseg2

      call spice%add_loader('SIMPLE', ld, extensions=['.sim'])
      call ref%add_loader('SIMPLE', ld, extensions=['.sim'])
      ! two blocks: a discontinuity at row 80 (its velocity changed), the
      ! second block with bounds inside its rows, Lagrange then Hermite
      s2 = states
      s2(4:6, 80) = 1.01_wp*states(4:6, 80)
      call write_round_trip_file(s2)
      call spice%furnsh(rt_sim)
      call check_ok(ctx, 'round trip: the file loads')

      call spice%write_kernel(rt_sim, rt_bsp)
      call check_ok(ctx, 'round trip: written to an SPK')
      call other%furnsh(rt_bsp)
      call check_ok(octx, 'round trip: the SPK loads')
      call check(same_states(ctx, octx, sc, epochs(1), epochs(n - 5), 2003), &
                 'round trip: the SPK gives the kernel''s states, bitwise')

      ! back: the SPK's segments to the test format
      h1 = handle_of(ctx, rt_sim)
      h2 = handle_of(octx, rt_bsp)
      call spk_to_simple(octx, h2, rt_back, nseg2)
      call check_ok(octx, 'round trip: the SPK read back')
      call check(nseg2 == 2, 'round trip: two segments')
      ! the kernel in memory read the same way (its summary and name records)
      call spk_to_simple(ctx, h1, 'test_loaders_rt_mem.sim', nseg1)
      call check_ok(ctx, 'round trip: the kernel in memory read as a DAF')
      call check(nseg1 == 2 .and. same_file('test_loaders_rt_mem.sim', rt_back), &
                 'round trip: the kernel in memory and the SPK have the same segments')
      call check(same_rows(rt_sim, rt_back), 'round trip: the rows read back are the rows of the file')

      call ref%furnsh(rt_back)
      call check_ok(rctx, 'round trip: the file read back loads')
      call check(same_states(ctx, rctx, sc, epochs(1), epochs(n - 5), 2003), &
                 'round trip: the file read back gives the same states, bitwise')
      call ref%unload(rt_back)
      call other%unload(rt_bsp)
      call spice%unload(rt_sim)
      call spice%remove_loader('SIMPLE')
      call ref%remove_loader('SIMPLE')
   end subroutine test_round_trip

   !> The round trip's file: rows 1 to 80 (Lagrange 8), and rows 80 to
   !> N-5 (Hermite 7) with bounds inside, the velocity at row 80 changed.
   subroutine write_round_trip_file(s2)
      real(wp), intent(in) :: s2(:, :)  !! the states of the second block
      integer :: lun, k
      open (newunit=lun, file=rt_sim, status='REPLACE', action='WRITE')
      write (lun, '(a)') 'SIMPLE_EPHEMERIS'
      write (lun, '(a)') 'OBJECT = -10001'
      write (lun, '(a)') 'CENTER = 399'
      write (lun, '(a)') 'FRAME = J2000'
      write (lun, '(a)') 'INTERPOLATION = LAGRANGE 8'
      write (lun, '(a)') 'BEGIN_DATA'
      do k = 1, 80
         write (lun, '(7es25.16e3)') epochs(k), states(:, k)
      end do
      write (lun, '(a)') 'END_DATA'
      write (lun, '(a)') 'INTERPOLATION = HERMITE 7'
      write (lun, '(a, 2es25.16e3)') 'BOUNDS = ', epochs(80), epochs(n - 5)
      write (lun, '(a)') 'BEGIN_DATA'
      do k = 80, n
         write (lun, '(7es25.16e3)') epochs(k), s2(:, k)
      end do
      write (lun, '(a)') 'END_DATA'
      close (lun)
   end subroutine write_round_trip_file

   !> The handle of a loaded kernel.
   integer(ip) function handle_of(c, name) result(handle)
      type(pepper_context), intent(inout) :: c  !! the context
      character(*), intent(in) :: name  !! the kernel
      character(256) :: file, srcfil
      character(32) :: filtyp
      integer(ip) :: i, count
      logical :: found
      handle = 0
      call c%ktotal('SPK', count)
      do i = 1, count
         call c%kdata(i, 'SPK', file, filtyp, srcfil, handle, found)
         if (file == name) return
      end do
      handle = 0
   end function handle_of

   !> Write the segments of an SPK (types 9 and 13) in the test format:
   !> a block per segment, its interpolation and bounds from the segment,
   !> its rows from the segment's data (the DAF read through its handle).
   subroutine spk_to_simple(c, handle, file, nseg)
      type(pepper_context), intent(inout) :: c  !! the context
      integer(ip), intent(in) :: handle  !! the SPK's handle
      character(*), intent(in) :: file  !! the file to write
      integer, intent(out) :: nseg  !! the number of segments
      real(wp) :: sum(5), dc(2), ctl(2)
      real(wp), allocatable :: data(:)
      integer(ip) :: ic(6), m, deg
      character(40) :: name
      integer :: lun, k
      logical :: fnd
      nseg = 0
      open (newunit=lun, file=file, status='REPLACE', action='WRITE')
      write (lun, '(a)') 'SIMPLE_EPHEMERIS'
      call c%dafbfs(handle)
      call c%daffna(fnd)
      do while (fnd .and. .not. c%failed())
         call c%dafgs(sum)
         call c%dafus(sum, 2_ip, 6_ip, dc, ic)
         call c%dafgn(name)
         call c%dafgda(handle, ic(6) - 1, ic(6), ctl)
         m = nint(ctl(2), ip)
         if (ic(4) == 9) then
            deg = nint(ctl(1), ip)
            write (lun, '(a, i0)') 'INTERPOLATION = LAGRANGE ', deg
         else
            deg = 2*(nint(ctl(1), ip) + 1) - 1
            write (lun, '(a, i0)') 'INTERPOLATION = HERMITE ', deg
         end if
         write (lun, '(a, i0)') 'OBJECT = ', ic(1)
         write (lun, '(a, i0)') 'CENTER = ', ic(2)
         write (lun, '(a)') 'FRAME = J2000'
         write (lun, '(a, 2es25.16e3)') 'BOUNDS = ', dc
         allocate (data(7*m))
         call c%dafgda(handle, ic(5), ic(5) + 7*m - 1, data)
         write (lun, '(a)') 'BEGIN_DATA'
         do k = 1, m
            write (lun, '(7es25.16e3)') data(6*m + k), data(6*(k - 1) + 1:6*k)
         end do
         write (lun, '(a)') 'END_DATA'
         deallocate (data)
         nseg = nseg + 1
         if (trim(name) /= 'SIMPLE') call check(.false., 'the segment''s name')
         call c%daffna(fnd)
      end do
      close (lun)
   end subroutine spk_to_simple

   !> True if two files have the same lines.
   logical function same_file(a, b) result(same)
      character(*), intent(in) :: a  !! a file
      character(*), intent(in) :: b  !! another file
      character(512) :: la, lb
      integer :: ua, ub, ia, ib
      open (newunit=ua, file=a, status='OLD', action='READ')
      open (newunit=ub, file=b, status='OLD', action='READ')
      same = .true.
      do
         read (ua, '(a)', iostat=ia) la
         read (ub, '(a)', iostat=ib) lb
         if (ia /= 0 .or. ib /= 0) then
            same = same .and. ia == ib
            exit
         end if
         same = same .and. la == lb
      end do
      close (ua)
      close (ub)
   end function same_file

   !> True if two files of the test format have the same rows (the lines
   !> between BEGIN_DATA and END_DATA, compared as numbers).
   logical function same_rows(a, b) result(same)
      character(*), intent(in) :: a  !! a file
      character(*), intent(in) :: b  !! another file
      real(wp), allocatable :: ra(:, :), rb(:, :)
      call read_rows(a, ra)
      call read_rows(b, rb)
      same = size(ra, 2) == size(rb, 2)
      if (same) same = all(ra == rb)
   end function same_rows

   !> The rows of a file of the test format.
   subroutine read_rows(file, rows)
      character(*), intent(in) :: file  !! the file
      real(wp), allocatable, intent(out) :: rows(:, :)  !! the rows (7, n)
      character(512) :: line
      real(wp) :: r(7)
      real(wp), allocatable :: tmp(:, :)
      integer :: lun, ios, m
      logical :: in_data
      allocate (rows(7, 0))
      in_data = .false.
      m = 0
      open (newunit=lun, file=file, status='OLD', action='READ')
      do
         read (lun, '(a)', iostat=ios) line
         if (ios /= 0) exit
         line = adjustl(line)
         if (line == 'BEGIN_DATA') then
            in_data = .true.
         else if (line == 'END_DATA') then
            in_data = .false.
         else if (in_data) then
            read (line, *) r
            m = m + 1
            allocate (tmp(7, m))
            tmp(:, 1:m - 1) = rows
            tmp(:, m) = r
            call move_alloc(tmp, rows)
         end if
      end do
      close (lun)
   end subroutine read_rows

   !> Discontinuities (the later block wins at the shared epoch), gaps
   !> (two coverage intervals), bounds, adapt.
   subroutine test_discontinuities()
      type(simple_loader) :: ld
      real(wp), allocatable :: cover(:, :)
      real(wp) :: s(6), e3(101), s3(6, 101)
      logical :: ok

      call spice%add_loader('SIMPLE', ld, extensions=['.sim'])
      ! rows 1 to 50, then rows 50 to 100 with the velocity at row 50 halved
      e3 = [epochs(1:50), epochs(50:100)]
      s3(:, 1:50) = states(:, 1:50)
      s3(:, 51:101) = states(:, 50:100)
      s3(4:6, 51) = 0.5_wp*states(4:6, 50)
      call write_simple('test_loaders_disc.sim', [character(40) :: 'OBJECT = -10001', 'CENTER = 399', &
                        'FRAME = J2000', 'INTERPOLATION = HERMITE 7'], e3, s3, [1, 51], [50, 101])
      call spice%furnsh('test_loaders_disc.sim')
      call check_ok(ctx, 'a discontinuity: loads')
      call state_of(ctx, sc, 399_ip, epochs(50), s, ok)
      call check(ok .and. maxval(abs(s(4:6) - s3(4:6, 51))) < 1.0e-12_wp, &
                 'a discontinuity: the later block''s state at the shared epoch')
      call state_of(ctx, sc, 399_ip, epochs(50) - 1.0e-3_wp, s, ok)
      call check(ok .and. abs(s(4) - states(4, 50)) < 1.0e-6_wp, 'a discontinuity: the earlier block just before')
      call ctx%spk_coverage('test_loaders_disc.sim', sc, cover)
      call check(size(cover, 2) == 1, 'a discontinuity: one coverage interval')
      call spice%unload('test_loaders_disc.sim')

      call write_simple_blocks('test_loaders_gap.sim', 'INTERPOLATION = HERMITE 7', states, [1, 60], [40, 100])
      call spice%furnsh('test_loaders_gap.sim')
      call ctx%spk_coverage('test_loaders_gap.sim', sc, cover)
      call check(size(cover, 2) == 2, 'a gap: two coverage intervals')
      call state_of(ctx, sc, 399_ip, epochs(50), s, ok)
      call check(.not. ok, 'a gap: no state in it')
      call spice%unload('test_loaders_gap.sim')

      ! bounds inside the rows
      call write_simple('test_loaders_bounds.sim', [character(60) :: 'OBJECT = -10001', 'CENTER = 399', &
                        'FRAME = J2000', 'INTERPOLATION = HERMITE 7', 'BOUNDS = 600.0 3000.0'], &
                        epochs, states, [1], [n])
      call spice%furnsh('test_loaders_bounds.sim')
      call ctx%spk_coverage('test_loaders_bounds.sim', sc, cover)
      call check(size(cover, 2) == 1, 'bounds: one interval')
      if (size(cover, 2) == 1) call check(cover(1, 1) == 600.0_wp .and. cover(2, 1) == 3000.0_wp, 'bounds: the coverage')
      call spice%unload('test_loaders_bounds.sim')

      ! adapt: a block of two rows, a block of one
      call write_simple_blocks('test_loaders_short.sim', 'INTERPOLATION = LAGRANGE 8', states, [1, 20, 21], [20, 21, 21])
      call spice%furnsh('test_loaders_short.sim')
      call check_error(ctx, 'SPICE(TOOFEWSTATES)', 'a block too short for its degree')
      call write_simple_blocks('test_loaders_short.sim', 'ADAPT = YES', states, [1, 20, 21], [20, 21, 21], &
                               'INTERPOLATION = LAGRANGE 8')
      call spice%furnsh('test_loaders_short.sim')
      call check_ok(ctx, 'adapt: short blocks')
      call state_of(ctx, sc, 399_ip, 0.5_wp*(epochs(20) + epochs(21)), s, ok)
      call check(ok, 'adapt: the two-row block covers its interval')
      call spice%unload('test_loaders_short.sim')
      call spice%remove_loader('SIMPLE')
   end subroutine test_discontinuities

   !> A file of the test format with several blocks of the trajectory.
   subroutine write_simple_blocks(file, line, s, first, last, line2)
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: line  !! a header line
      real(wp), intent(in) :: s(:, :)  !! the states
      integer, intent(in) :: first(:)  !! the blocks' first rows
      integer, intent(in) :: last(:)  !! their last rows
      character(*), intent(in), optional :: line2  !! another header line
      character(60) :: h2
      h2 = '# -'
      if (present(line2)) h2 = line2
      call write_simple(file, [character(60) :: 'OBJECT = -10001', 'CENTER = 399', 'FRAME = J2000', line, h2], &
                        epochs, s, first, last)
   end subroutine write_simple_blocks

   !> Broken files: the error names the line, and nothing is loaded.
   subroutine test_file_errors()
      type(simple_loader) :: ld
      character(1840) :: msg
      integer(ip) :: count
      integer :: lun

      call spice%add_loader('SIMPLE', ld, extensions=['.sim'])
      open (newunit=lun, file=bad_sim, status='REPLACE', action='WRITE')
      write (lun, '(a)') 'SIMPLE_EPHEMERIS'
      write (lun, '(a)') 'OBJECT = -10001'
      write (lun, '(a)') 'CENTER = 399'
      write (lun, '(a)') 'FRAME = J2000'
      write (lun, '(a)') 'INTERPOLATION = HERMITE 3'
      write (lun, '(a)') 'BEGIN_DATA'
      write (lun, '(a)') '0 7000 0 0 0 7 0'
      write (lun, '(a)') '60 6999 420 0 -0.01 7 0'
      write (lun, '(a)') '120 6998 bad 0 0 7 0'
      write (lun, '(a)') 'END_DATA'
      close (lun)
      call spice%furnsh(bad_sim)
      call ctx%getmsg('LONG', msg)
      call check(index(msg, 'line 9') > 0, 'the error names the line')
      call check_error(ctx, 'SPICE(BADSIMPLEFILE)', 'a bad number')
      call ctx%ktotal('ALL', count)
      call check(count == 0, 'nothing loaded after an error')

      call write_simple(bad_sim, [character(40) :: 'OBJECT = -10001', 'CENTER = 399', 'FRAME = J2000', &
                                                   'INTERPOLATION = SPLINE 5'], epochs, states, [1], [n])
      call spice%furnsh(bad_sim)
      call check_error(ctx, 'SPICE(UNSUPPORTEDINTERP)', 'an interpolation PEPPER does not support')
      call write_simple(bad_sim, [character(40) :: 'OBJECT = -10001', 'CENTER = 399', 'FRAME = J2000', &
                                                   'INTERPOLATION = HERMITE 6'], epochs, states, [1], [n])
      call spice%furnsh(bad_sim)
      call check_error(ctx, 'SPICE(INVALIDDEGREE)', 'an even Hermite degree in a file')
      call write_simple(bad_sim, [character(40) :: 'OBJECT = -10001', 'CENTER = 399', 'FRAME = NOSUCHFRAME', &
                                                   'INTERPOLATION = HERMITE 7'], epochs, states, [1], [n])
      call spice%furnsh(bad_sim)
      call check_error(ctx, 'SPICE(INVALIDREFFRAME)', 'an unknown frame in a file')
      call ctx%ktotal('ALL', count)
      call check(count == 0, 'nothing loaded after the errors')
      call spice%remove_loader('SIMPLE')
   end subroutine test_file_errors

   !> A meta-kernel: a file by extension, another named in KERNEL_LOADERS;
   !> UNLOAD of the meta-kernel calls the unload callbacks; KCLEAR too.
   subroutine test_meta_kernel()
      use pepper_containers, only: string
      type(simple_loader) :: ld, quiet
      class(kernel_loader), pointer :: p
      character(:), allocatable :: loader, format
      type(string), allocatable :: values(:)
      integer(ip) :: count
      integer :: lun
      logical :: found

      quiet%sniff = .false.
      call spice%add_loader('SIMPLE', ld, extensions=['.sim'])
      call spice%add_loader('NOSNIFF', quiet)
      call write_simple(m1, [character(40) :: 'OBJECT = -10001', 'CENTER = 399', 'FRAME = J2000', &
                                              'INTERPOLATION = HERMITE 7'], epochs, states, [1], [n])
      call write_simple(m2, [character(40) :: 'OBJECT = -10002', 'CENTER = 399', 'FRAME = J2000', &
                                              'INTERPOLATION = LAGRANGE 8'], epochs, states, [1], [n])
      open (newunit=lun, file=meta, status='REPLACE', action='WRITE')
      write (lun, '(a)') 'KPL/MK'
      write (lun, '(a)') '\begindata'
      write (lun, '(a)') "PATH_SYMBOLS = ( 'D' )"
      write (lun, '(a)') "PATH_VALUES = ( '.' )"
      write (lun, '(a)') "KERNELS_TO_LOAD = ( '$D/"//m1//"', '"//m2//"' )"
      write (lun, '(a)') "KERNEL_LOADERS = ( '"//m2//"', 'NOSNIFF' )"
      write (lun, '(a)') '\begintext'
      close (lun)
      call spice%furnsh(meta)
      call check_ok(ctx, 'a meta-kernel with KERNEL_LOADERS')
      call ctx%ktotal('SPK', count)
      call check(count == 2, 'the meta-kernel loaded two kernels')
      call ctx%kernel_format(m2, loader, format, found)
      call check(found .and. loader == 'NOSNIFF', 'KERNEL_LOADERS chose the loader')
      call ctx%kernel_format('./'//m1, loader, format, found)
      call check(found .and. loader == 'SIMPLE', 'a path symbol, then the extension')
      call check(same_states(ctx, rctx, sc - 1, epochs(1), epochs(n), 301), 'the meta-kernel''s file: the states')
      call ctx%pool_strings('KERNEL_LOADERS', values, found)
      call check(.not. found, 'KERNEL_LOADERS is removed from the pool after the load')

      call spice%unload(meta)
      call ctx%ktotal('ALL', count)
      call check(count == 0, 'UNLOAD of the meta-kernel unloads its files')
      call spice%loader('SIMPLE', p)
      select type (p)
      type is (simple_loader)
         call check(p%unloads == 1, 'the unload callback of the meta-kernel''s file')
      end select
      call spice%loader('NOSNIFF', p)
      select type (p)
      type is (simple_loader)
         call check(p%unloads == 1, 'the unload callback of the KERNEL_LOADERS file')
      end select

      ! KCLEAR calls the callbacks too
      call spice%furnsh(meta)
      call spice%kclear()
      call spice%loader('SIMPLE', p)
      select type (p)
      type is (simple_loader)
         call check(p%unloads == 2 .and. p%loads == 2, 'KCLEAR calls the unload callbacks')
      end select

      ! KERNEL_LOADERS not in pairs
      open (newunit=lun, file=meta_bad, status='REPLACE', action='WRITE')
      write (lun, '(a)') 'KPL/MK'
      write (lun, '(a)') '\begindata'
      write (lun, '(a)') "KERNELS_TO_LOAD = ( '"//m2//"' )"
      write (lun, '(a)') "KERNEL_LOADERS = ( '"//m2//"' )"
      write (lun, '(a)') '\begintext'
      close (lun)
      call spice%furnsh(meta_bad)
      call check_error(ctx, 'SPICE(BADKERNELLOADERS)', 'KERNEL_LOADERS with an odd number of values')
      call spice%kclear()
      call spice%remove_loader('SIMPLE')
      call spice%remove_loader('NOSNIFF')
   end subroutine test_meta_kernel

   !> Body names defined by kernels (define_body): from a loader's file and
   !> from a programmatic kernel.
   subroutine test_names()
      type(simple_loader) :: ld
      type(kernel_builder) :: kb
      type(pepper_context) :: pre
      character(64) :: name
      real(wp) :: s(6), lt
      integer(ip) :: code
      logical :: found

      call spice%add_loader('SIMPLE', ld, extensions=['.sim'])
      pre = spice%new_context()  ! made, and its name tables built, before the loads
      call pre%bodn2c('EARTH', code, found)
      call spice%boddef('SC_ONE', -5_ip)  ! the program's own definition of the name
      call write_simple('test_loaders_n1.sim', [character(40) :: 'OBJECT = SC_ONE', 'OBJECT_CODE = -10001', &
                        'CENTER = 399', 'FRAME = J2000', 'INTERPOLATION = HERMITE 7'], epochs, states, [1], [n])
      call write_simple('test_loaders_n2.sim', [character(40) :: 'OBJECT = SC_ONE', 'OBJECT_CODE = -10002', &
                        'CENTER = 399', 'FRAME = J2000', 'INTERPOLATION = HERMITE 7'], epochs, states, [1], [n])
      call spice%furnsh('test_loaders_n1.sim')
      call check_ok(ctx, 'a file that defines a body name')
      call pre%bodn2c('SC_ONE', code, found)
      call check(found .and. code == -10001, 'the kernel''s name, in a context made before the load')
      call pre%bodc2n(-10001_ip, name, found)
      call check(found .and. name == 'SC_ONE', 'the kernel''s code to name')
      call pre%spkezr('SC_ONE', 1000.0_wp, 'J2000', 'NONE', 'EARTH', s, lt)
      call check_ok(pre, 'SPKEZR by the kernel''s name')
      call check(maxval(abs(s - circle_state(radius, 0.3_wp, 1000.0_wp))) < 1.0e-6_wp, 'the state by the name')

      call spice%furnsh('test_loaders_n2.sim')
      call pre%bodn2c('SC_ONE', code, found)
      call check(found .and. code == -10002, 'the latest kernel''s definition wins')
      call spice%unload('test_loaders_n2.sim')
      call pre%bodn2c('SC_ONE', code, found)
      call check(found .and. code == -10001, 'UNLOAD: the earlier kernel''s definition again')
      call pre%bodc2n(-10002_ip, name, found)
      call check(.not. found, 'UNLOAD: the code''s name is gone')

      ! the kernel pool's names take precedence
      call spice%pcpool('NAIF_BODY_NAME', 1_ip, ['SC_ONE'])
      call spice%pipool('NAIF_BODY_CODE', 1_ip, [-77_ip])
      call pre%bodn2c('SC_ONE', code, found)
      call check(found .and. code == -77, 'the kernel pool''s name over the kernel''s')
      call spice%dvpool('NAIF_BODY_NAME')
      call spice%dvpool('NAIF_BODY_CODE')

      call spice%unload('test_loaders_n1.sim')
      call pre%bodn2c('SC_ONE', code, found)
      call check(found .and. code == -5, 'UNLOAD: the program''s BODDEF again')
      call pre%bodc2n(-10001_ip, name, found)
      call check(.not. found, 'UNLOAD: no name for the kernel''s code')

      ! a programmatic kernel's names; KCLEAR removes them, not BODDEF's
      call kb%define_body(ctx, 'PROG_SC', -10003_ip)
      call kb%define_body(ctx, ' ', -10004_ip)
      call check_error(ctx, 'SPICE(BLANKNAMEASSIGNED)', 'a blank name')
      call kb%spk_hermite(ctx, -10003_ip, 399_ip, 'J2000', epochs, states, 7_ip)
      call spice%load_kernel('named', kb)
      call ctx%bodn2c('PROG_SC', code, found)
      call check(found .and. code == -10003, 'a programmatic kernel''s name')
      call spice%furnsh('test_loaders_n1.sim')
      call spice%kclear()
      call pre%bodn2c('PROG_SC', code, found)
      call check(.not. found, 'KCLEAR removes the kernels'' names')
      call pre%bodn2c('SC_ONE', code, found)
      call check(found .and. code == -5, 'KCLEAR keeps the program''s BODDEF')
      call pre%destroy()
      call spice%remove_loader('SIMPLE')
   end subroutine test_names

   !> Delete the test's files.
   subroutine delete_files()
      call delete_file(ref_bsp)
      call delete_file(prog_bsp)
      call delete_file(many_bsp)
      call delete_file(sim_a)
      call delete_file(sim_b)
      call delete_file(sim_c)
      call delete_file(rt_sim)
      call delete_file(rt_bsp)
      call delete_file(rt_back)
      call delete_file('test_loaders_rt_mem.sim')
      call delete_file(bad_sim)
      call delete_file(meta)
      call delete_file(meta_bad)
      call delete_file(m1)
      call delete_file(m2)
      call delete_file('test_loaders_disc.sim')
      call delete_file('test_loaders_gap.sim')
      call delete_file('test_loaders_bounds.sim')
      call delete_file('test_loaders_short.sim')
      call delete_file('test_loaders_none.bsp')
      call delete_file('test_loaders_n1.sim')
      call delete_file('test_loaders_n2.sim')
   end subroutine delete_files

end program test_loaders
