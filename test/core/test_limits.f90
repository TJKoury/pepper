!> Tests past SPICELIB's fixed limits (limits.md): file names longer than
!> 255 characters, more kernels than the 5000 (5300 through FURNSH) that
!> SPICELIB's file tables held, the priority of SPK files after
!> unloading, more DAFs written at once than DAFANA's 20, and comments
!> read from many DAFs in turn (DAFEC's table), and the kernel pool past
!> its limits (variables, values, name and string lengths), and more
!> kernel frames than FRAMEX's caches held (5209).
program test_limits

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context
   use pepper_test_kernels, only: write_lsk, write_spk, kernel_name, delete_file, circle_state, &
      write_tk_frames, frame_name

   implicit none

   character(*), parameter :: prefix = 'test_limits_'
   type(pepper_spice), target :: spice
   type(pepper_context) :: ctx
   integer :: failures = 0

   ctx = spice%new_context()
   call test_long_paths()
   call test_priorities()
   call test_many_spks()
   call test_many_writers()
   call test_many_comment_readers()
   call test_pool()
   call test_frames()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_limits: all checks passed'

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

   !> Report the error of a context (if any) as a failed check, and reset it.
   subroutine check_ok(what)
      character(*), intent(in) :: what  !! what was done
      if (ctx%failed()) then
         call check(.false., what // ': ' // ctx%message('SHORT') // ' ' // ctx%message('LONG'))
         call ctx%reset()
      end if
   end subroutine check_ok

   !> The largest difference (km) between the position of a state and
   !> that of a circle of write_spk.
   real(wp) function position_error(state, r, phase, et)
      real(wp), intent(in) :: state(6)  !! the state
      real(wp), intent(in) :: r  !! the radius of the circle (km)
      real(wp), intent(in) :: phase  !! its phase at ET 0 (rad)
      real(wp), intent(in) :: et  !! the epoch
      real(wp) :: expected(6)
      expected = circle_state(r, phase, et)
      position_error = maxval(abs(state(1:3) - expected(1:3)))
   end function position_error

   !> Kernels through paths longer than 255 characters (SPICELIB's
   !> FILEN and FILSIZ): an SPK and a text kernel through FURNSH, and a
   !> meta-kernel whose KERNELS_TO_LOAD name and PATH_VALUES path are
   !> that long. The paths repeat './' to get their length without
   !> creating directories.
   subroutine test_long_paths()
      character(:), allocatable :: dots, spk, tk, meta
      character(1024) :: file, filtyp, source
      real(wp) :: state(6), lt, values(2)
      integer(ip) :: handle, n, count, unit
      logical :: found
      dots = repeat('./', 150)
      spk = dots // prefix // 'long.bsp'
      tk = dots // prefix // 'long.tk'
      meta = prefix // 'long.tm'
      call check(len(spk) > 255 .and. len(tk) > 255, 'long paths: longer than 255')

      call write_spk(ctx, spk, 900001_ip, 0_ip, 1.0e5_wp, 0.5_wp)
      call check_ok('long paths: writing the SPK')
      call ctx%furnsh(spk)
      call check_ok('long paths: furnsh of the SPK')
      call ctx%spkgeo(900001_ip, 1.0e3_wp, 'J2000', 0_ip, state, lt)
      call check_ok('long paths: spkgeo')
      call check(position_error(state, 1.0e5_wp, 0.5_wp, 1.0e3_wp) < 1.0e-3_wp, 'long paths: the state')
      call ctx%kinfo(spk, filtyp, source, handle, found)
      call check(found .and. filtyp == 'SPK', 'long paths: kinfo by the long name')
      call ctx%kdata(1_ip, 'SPK', file, filtyp, source, handle, found)
      call check(found .and. file == spk, 'long paths: kdata returns the whole name')
      call ctx%unload(spk)
      call ctx%ktotal('ALL', count)
      call check(count == 0, 'long paths: unload by the long name')

      open (newunit=unit, file=tk, status='replace', action='write')
      write (unit, '(a)') '\begindata', 'TEST_LIMITS_X = ( 1.5, 2.5 )', '\begintext'
      close (unit)
      call ctx%furnsh(tk)
      call check_ok('long paths: furnsh of the text kernel')
      call ctx%gdpool('TEST_LIMITS_X', 1_ip, 2_ip, n, values, found)
      call check(found .and. n == 2 .and. values(2) == 2.5_wp, 'long paths: the text kernel''s values')
      call ctx%unload(tk)
      call ctx%gdpool('TEST_LIMITS_X', 1_ip, 2_ip, n, values, found)
      call check(.not. found, 'long paths: unload of the text kernel')

      ! A meta-kernel: the SPK through a path symbol whose value has 300
      ! characters, and the text kernel by its long name; both strings
      ! are continued over several pool values ('+').
      open (newunit=unit, file=meta, status='replace', action='write')
      write (unit, '(a)') '\begindata', 'PATH_SYMBOLS = ( ''D'' )', 'PATH_VALUES = ('
      call write_continued(unit, dots(1:len(dots) - 1))
      write (unit, '(a)') ')', 'KERNELS_TO_LOAD = ( ''$D/' // prefix // 'long.bsp''', ' '
      call write_continued(unit, tk)
      write (unit, '(a)') ')', '\begintext'
      close (unit)
      call ctx%furnsh(meta)
      call check_ok('long paths: furnsh of the meta-kernel')
      call ctx%ktotal('ALL', count)
      call check(count == 3, 'long paths: the meta-kernel and its two kernels')
      call ctx%spkgeo(900001_ip, 1.0e3_wp, 'J2000', 0_ip, state, lt)
      call check_ok('long paths: spkgeo through the meta-kernel')
      call ctx%gdpool('TEST_LIMITS_X', 1_ip, 2_ip, n, values, found)
      call check(found, 'long paths: the text kernel through the meta-kernel')
      call ctx%unload(meta)
      call ctx%ktotal('ALL', count)
      call check(count == 0, 'long paths: unload of the meta-kernel')
      call check_ok('long paths')
      call delete_file(spk)
      call delete_file(tk)
      call delete_file(meta)
   end subroutine test_long_paths

   !> Write a string as a continued kernel pool string: pieces of 60
   !> characters, each but the last ending with '+'.
   subroutine write_continued(unit, text)
      integer(ip), intent(in) :: unit  !! the unit of the kernel
      character(*), intent(in) :: text  !! the string
      integer :: b, e
      b = 1
      do
         e = min(b + 59, len(text))
         if (e < len(text)) then
            write (unit, '(3a)') '''', text(b:e), '+'''
         else
            write (unit, '(3a)') '''', text(b:e), ''''
            exit
         end if
         b = e + 1
      end do
   end subroutine write_continued

   !> The priority of SPK files after unloading: the last loaded file
   !> wins; unloading a second load of a file puts it back at its first
   !> place; unloading a file loaded once leaves the others in order.
   subroutine test_priorities()
      character(:), allocatable :: a, b, c
      integer(ip), parameter :: body = 900002
      a = prefix // 'prio_a.bsp'
      b = prefix // 'prio_b.bsp'
      c = prefix // 'prio_c.bsp'
      call write_spk(ctx, a, body, 0_ip, 1.0e5_wp, 0.0_wp)
      call write_spk(ctx, b, body, 0_ip, 2.0e5_wp, 0.0_wp)
      call write_spk(ctx, c, body, 0_ip, 3.0e5_wp, 0.0_wp)
      call check_ok('priorities: writing the SPKs')

      call ctx%furnsh(a)
      call ctx%furnsh(b)
      call ctx%furnsh(a)
      call check(radius() == 1.0e5_wp, 'priorities: a loaded again wins')
      call ctx%unload(a)        ! the second load of a: a is back below b
      call check(radius() == 2.0e5_wp, 'priorities: b after unloading the second a')
      call ctx%unload(a)
      call check(radius() == 2.0e5_wp, 'priorities: b after unloading a')
      call ctx%furnsh(a)
      call ctx%furnsh(c)
      call ctx%unload(c)        ! loaded once: a stays above b
      call check(radius() == 1.0e5_wp, 'priorities: a after unloading c')
      call ctx%unload(b)
      call check(radius() == 1.0e5_wp, 'priorities: a after unloading b')
      call ctx%kclear()
      call check_ok('priorities')
      call delete_file(a)
      call delete_file(b)
      call delete_file(c)
   end subroutine test_priorities

   !> The radius (km) of the orbit of the body of test_priorities in the
   !> SPK with the highest priority.
   real(wp) function radius()
      real(wp) :: state(6), lt
      call ctx%spkgeo(900002_ip, 0.0_wp, 'J2000', 0_ip, state, lt)
      radius = state(1)
      if (ctx%failed()) then
         radius = -1
         call check_ok('priorities: spkgeo')
      end if
   end function radius

   !> 6000 SPKs (one body each) through FURNSH, more than SPICELIB's
   !> 5000 (file tables) and 5300 (KEEPER): all load and are found;
   !> unloading every other one leaves the others. (The bodies checked are
   !> a few around the old limits: the first query of a body reads the
   !> summaries of every file loaded after its own, see limits.md.)
   subroutine test_many_spks()
      integer(ip), parameter :: n = 6000, first = 910001
      integer(ip), parameter :: checked(14) = [1, 2, 1000, 2999, 3000, 4999, 5000, 5001, 5299, &
                                              5300, 5301, 5302, 5999, 6000]
      real(wp) :: state(6), lt, err
      integer(ip) :: i, k, count
      logical :: ok
      do i = 1, n
         call write_spk(ctx, kernel_name(prefix // 'many_', i, '.bsp'), first + i - 1, 0_ip, &
                        1.0e5_wp, 1.0e-3_wp*i)
      end do
      call check_ok('many SPKs: writing')
      do i = 1, n
         call ctx%furnsh(kernel_name(prefix // 'many_', i, '.bsp'))
         if (ctx%failed()) exit
      end do
      call check_ok('many SPKs: furnsh')
      call ctx%ktotal('SPK', count)
      call check(count == n, 'many SPKs: all loaded')
      err = 0
      do k = 1, size(checked)
         i = checked(k)
         call ctx%spkgeo(first + i - 1, 1.0e3_wp, 'J2000', 0_ip, state, lt)
         err = max(err, position_error(state, 1.0e5_wp, 1.0e-3_wp*i, 1.0e3_wp))
      end do
      call check_ok('many SPKs: spkgeo')
      call check(err < 1.0e-3_wp, 'many SPKs: the states')

      do i = 2, n, 2
         call ctx%unload(kernel_name(prefix // 'many_', i, '.bsp'))
      end do
      call check_ok('many SPKs: unload')
      call ctx%ktotal('SPK', count)
      call check(count == n/2, 'many SPKs: half unloaded')
      ok = .true.
      do k = 1, size(checked)
         i = checked(k)
         call ctx%spkgeo(first + i - 1, 1.0e3_wp, 'J2000', 0_ip, state, lt)
         if (mod(i, 2_ip) == 0) then
            ok = ok .and. ctx%failed()
            call ctx%reset()
         else
            ok = ok .and. .not. ctx%failed() .and. &
                 position_error(state, 1.0e5_wp, 1.0e-3_wp*i, 1.0e3_wp) < 1.0e-3_wp
         end if
      end do
      call check(ok, 'many SPKs: the unloaded bodies are gone, the others found')
      call ctx%kclear()
      call ctx%ktotal('ALL', count)
      call check(count == 0, 'many SPKs: kclear')
      call check_ok('many SPKs')
      do i = 1, n
         call delete_file(kernel_name(prefix // 'many_', i, '.bsp'))
      end do
   end subroutine test_many_spks

   !> 40 DAFs (SPKs) written at once, more than DAFANA's 20: an array is
   !> begun in each, then each is finished in turn (DAFCAD), and every
   !> SPK gives its states.
   subroutine test_many_writers()
      integer(ip), parameter :: n = 40, first = 920001, nstates = 11
      integer(ip) :: handles(n), i, k
      real(wp) :: sum(5), epochs(nstates), states(6, nstates), state(6), lt, err
      epochs = [(1.0e5_wp*(k - 1), k = 1, nstates)]
      do i = 1, n
         call delete_file(kernel_name(prefix // 'writer_', i, '.bsp'))
         call ctx%dafonw(kernel_name(prefix // 'writer_', i, '.bsp'), 'SPK', 2_ip, 6_ip, 'writers', 0_ip, &
                         handles(i))
         call ctx%dafps(2_ip, 6_ip, [epochs(1), epochs(nstates)], [first + i - 1, 0_ip, 1_ip, 9_ip, 0_ip, 0_ip], &
                        sum)
         call ctx%dafbna(handles(i), sum, 'WRITER')
      end do
      call check_ok('writers: begin the arrays')
      do i = 1, n
         do k = 1, nstates
            states(:, k) = circle_state(1.0e5_wp, 1.0e-3_wp*i, epochs(k))
         end do
         call ctx%dafcad(handles(i))
         call ctx%dafada(reshape(states, [6*nstates]), 6*nstates)
         call ctx%dafada(epochs, nstates)
         call ctx%dafada([7.0_wp, real(nstates, wp)], 2_ip)
         call ctx%dafena()
         call ctx%dafcls(handles(i))
      end do
      call check_ok('writers: finish the arrays')
      err = 0
      do i = 1, n
         call ctx%furnsh(kernel_name(prefix // 'writer_', i, '.bsp'))
         call ctx%spkgeo(first + i - 1, 1.0e3_wp, 'J2000', 0_ip, state, lt)
         err = max(err, position_error(state, 1.0e5_wp, 1.0e-3_wp*i, 1.0e3_wp))
      end do
      call check_ok('writers: read the SPKs')
      call check(err < 1.0e-3_wp, 'writers: the states')
      call ctx%kclear()
      do i = 1, n
         call delete_file(kernel_name(prefix // 'writer_', i, '.bsp'))
      end do
   end subroutine test_many_writers

   !> The comments of 20 DAFs read in turn, one line per call, so that
   !> DAFEC keeps its place in all of them at once (its table starts with
   !> 8 entries; SPICELIB's had 5000).
   subroutine test_many_comment_readers()
      integer(ip), parameter :: n = 20, nlines = 5
      integer(ip) :: handles(n), i, k, got
      character(80) :: lines(nlines), line(1)
      logical :: done, ok
      do i = 1, n
         call write_spk(ctx, kernel_name(prefix // 'comments_', i, '.bsp'), 930000_ip + i, 0_ip, 1.0e5_wp, 0.0_wp)
         call ctx%dafopw(kernel_name(prefix // 'comments_', i, '.bsp'), handles(i))
         do k = 1, nlines
            write (lines(k), '(a,i0,a,i0)') 'file ', i, ' line ', k
         end do
         call ctx%dafac(handles(i), nlines, lines)
         call ctx%dafcls(handles(i))
         call ctx%dafopr(kernel_name(prefix // 'comments_', i, '.bsp'), handles(i))
      end do
      call check_ok('comments: writing')
      ok = .true.
      do k = 1, nlines
         do i = 1, n
            call ctx%dafec(handles(i), 1_ip, got, line, done)
            write (lines(1), '(a,i0,a,i0)') 'file ', i, ' line ', k
            ok = ok .and. got == 1 .and. line(1) == lines(1) .and. (done .eqv. (k == nlines))
         end do
      end do
      call check_ok('comments: reading')
      call check(ok, 'comments: the lines of each file, in turn')
      do i = 1, n
         call ctx%dafcls(handles(i))
         call delete_file(kernel_name(prefix // 'comments_', i, '.bsp'))
      end do
   end subroutine test_many_comment_readers

   !> The kernel pool past SPICELIB's limits: 100000 variables (MAXVAR
   !> 26003), a variable of 10**6 values (MAXVAL 400000), names and string
   !> values of 200 characters (32 and 80), a watch on a long name, and
   !> SZPOOL reporting unlimited.
   subroutine test_pool()
      integer(ip), parameter :: nvars = 100000, nvals = 1000000
      character(:), allocatable :: long_name, long_value
      character(32) :: name
      character(300) :: cval(1)
      real(wp), allocatable :: big(:), got(:)
      real(wp) :: value(1)
      integer(ip) :: i, n
      logical :: found, ok, update
      call ctx%clpool()
      do i = 1, nvars
         write (name, '(a,i0)') 'TEST_LIMITS_VAR_', i
         call ctx%pdpool(name, 1_ip, [real(i, wp)])
      end do
      call check_ok('pool: 100000 variables')
      ok = .true.
      do i = 1, nvars, 997
         write (name, '(a,i0)') 'TEST_LIMITS_VAR_', i
         call ctx%gdpool(name, 1_ip, 1_ip, n, value, found)
         ok = ok .and. found .and. n == 1 .and. value(1) == i
      end do
      call check(ok, 'pool: the values of 100000 variables')

      allocate (big(nvals))
      do i = 1, nvals
         big(i) = i
      end do
      call ctx%pdpool('TEST_LIMITS_BIG', nvals, big)
      allocate (got(nvals))
      call ctx%gdpool('TEST_LIMITS_BIG', 1_ip, nvals, n, got, found)
      call check(found .and. n == nvals .and. all(got == big), 'pool: a variable of 10**6 values')
      call ctx%gdpool('TEST_LIMITS_BIG', nvals, 1_ip, n, value, found)
      call check(found .and. n == 1 .and. value(1) == nvals, 'pool: the last of 10**6 values')

      long_name = 'TEST_LIMITS_' // repeat('N', 188)
      long_value = repeat('v', 199) // 'w'
      call ctx%swpool('TEST_LIMITS_AGENT_WITH_A_NAME_LONGER_THAN_32', 1_ip, [long_name])
      call ctx%cvpool('TEST_LIMITS_AGENT_WITH_A_NAME_LONGER_THAN_32', update)
      call ctx%pcpool(long_name, 1_ip, [long_value])
      call ctx%gcpool(long_name, 1_ip, 1_ip, n, cval, found)
      call check(found .and. n == 1 .and. cval(1) == long_value, 'pool: a 200-character name and value')
      call ctx%cvpool('TEST_LIMITS_AGENT_WITH_A_NAME_LONGER_THAN_32', update)
      call check(update, 'pool: a watch on a 200-character name')
      call ctx%gnpool('TEST_LIMITS_NNN*', 1_ip, 1_ip, n, cval, found)
      call check(found .and. n == 1 .and. cval(1) == long_name, 'pool: gnpool finds the long name')
      call ctx%dwpool('TEST_LIMITS_AGENT_WITH_A_NAME_LONGER_THAN_32')

      call ctx%szpool('MAXVAR', n, found)
      call check(found .and. n == huge(1_ip), 'pool: szpool reports unlimited')

      call ctx%dvpool('TEST_LIMITS_VAR_5')
      call ctx%gdpool('TEST_LIMITS_VAR_5', 1_ip, 1_ip, n, value, found)
      call check(.not. found, 'pool: dvpool')
      call ctx%pipool('TEST_LIMITS_VAR_5', 1_ip, [-5_ip])
      call ctx%gdpool('TEST_LIMITS_VAR_5', 1_ip, 1_ip, n, value, found)
      call check(found .and. value(1) == -5, 'pool: a variable re-created after dvpool')
      call ctx%clpool()
      call check_ok('pool')
   end subroutine test_pool

   !> 20000 TK frames, each looked up by name and by ID once: more distinct
   !> kernel frames than FRAMEX's caches held (5209; SPICELIB signaled
   !> SPICE(HASHISFULL) for the next one). TK frames also take 10 pool
   !> variables each: 200000, past the pool's 26003.
   subroutine test_frames()
      integer(ip), parameter :: n = 20000, first = 1700001
      character(*), parameter :: file = prefix // 'frames.tf'
      character(32) :: name
      real(wp) :: rot(3, 3)
      integer(ip) :: i, code, cent, frclss, clssid
      logical :: found, ok
      call write_tk_frames(file, n, first)
      call ctx%furnsh(file)
      call check_ok('frames: loading 20000 TK frames')
      ok = .true.
      do i = 1, n
         call ctx%namfrm(frame_name(i), code)
         ok = ok .and. code == first + i - 1
         call ctx%frmnam(first + i - 1, name)
         ok = ok .and. name == frame_name(i)
         call ctx%frinfo(first + i - 1, cent, frclss, clssid, found)
         ok = ok .and. found .and. cent == 399 .and. frclss == 4 .and. clssid == first + i - 1
         if (ctx%failed()) exit
      end do
      call check_ok('frames: lookups')
      call check(ok, 'frames: names, IDs and data of 20000 frames')
      do i = 1, n, 4999
         call ctx%pxform(frame_name(i), 'J2000', 0.0_wp, rot)
         ok = ok .and. abs(atan2(rot(1, 2), rot(1, 1)) - i/1000.0_wp*acos(-1.0_wp)/180) < 1.0e-12_wp
      end do
      call check_ok('frames: pxform')
      call check(ok, 'frames: rotations')
      call ctx%unload(file)
      call delete_file(file)
   end subroutine test_frames

end program test_limits
