!> Tests past SPICELIB's fixed limits (limits.md): file names longer than
!> 255 characters, more kernels than the 5000 (5300 through FURNSH) that
!> SPICELIB's file tables held, the priority of SPK files after
!> unloading, more DAFs written at once than DAFANA's 20, and comments
!> read from many DAFs in turn (DAFEC's table), and the kernel pool past
!> its limits (variables, values, name and string lengths), and more
!> kernel frames than FRAMEX's caches held (5209), body names past
!> ZZBODTRN's limits, surface names past ZZSRFTRN's, type 1 SCLKs past
!> SC01's, and PCK bodies with more nutation/precession terms than TISBOD
!> and BODEUL held (200).
program test_limits

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context, zzprsmet
   use pepper_test_kernels, only: write_lsk, write_spk, kernel_name, delete_file, circle_state, &
      write_tk_frames, frame_name, write_body_names, body_name

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
   call test_bodies()
   call test_surfaces()
   call test_sclk()
   call test_nutation()
   call test_frame_names()

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

   !> Body names past ZZBODTRN's limits: 20000 kernel pool names (NROOM
   !> 14983), 500 BODDEF definitions (161 after the 692 built-in names),
   !> and 60-character names (MAXL 36) both ways.
   subroutine test_bodies()
      integer(ip), parameter :: n = 20000, first = 2000001, ndef = 500
      character(*), parameter :: file = prefix // 'names.tf'
      character(:), allocatable :: long
      character(80) :: name
      integer(ip) :: i, code
      logical :: found, ok
      call write_body_names(file, n, first)
      call ctx%furnsh(file)
      call check_ok('bodies: loading 20000 names')
      ok = .true.
      do i = 1, n, 37
         call ctx%bodn2c(body_name(i), code, found)
         ok = ok .and. found .and. code == first + i - 1
         call ctx%bodc2n(first + i - 1, name, found)
         ok = ok .and. found .and. name == body_name(i)
      end do
      call check(ok, 'bodies: 20000 kernel names')
      call ctx%unload(file)
      call delete_file(file)

      do i = 1, ndef
         write (name, '(a,i0)') 'TEST_LIMITS_BODDEF_', i
         call ctx%boddef(name, 3000000 + i)
      end do
      call check_ok('bodies: 500 boddef definitions')
      ok = .true.
      do i = 1, ndef, 7
         write (name, '(a,i0)') 'TEST_LIMITS_BODDEF_', i
         call ctx%bodn2c(name, code, found)
         ok = ok .and. found .and. code == 3000000 + i
      end do
      call check(ok, 'bodies: 500 boddef definitions found')

      long = 'A Body Name Of Sixty Characters ' // repeat('X', 28)
      call ctx%boddef(long, 3100001)
      call ctx%bodn2c('  a body  name of sixty characters ' // repeat('x', 28), code, found)
      call check(found .and. code == 3100001, 'bodies: a 60-character boddef name, normalized')
      call ctx%bodc2n(3100001, name, found)
      call check(found .and. name == long, 'bodies: bodc2n returns the 60-character name')
      call ctx%pcpool('NAIF_BODY_NAME', 1_ip, [repeat('K', 60)])
      call ctx%pipool('NAIF_BODY_CODE', 1_ip, [3100002_ip])
      call ctx%bodn2c(repeat('k', 60), code, found)
      call check(found .and. code == 3100002, 'bodies: a 60-character kernel name')
      call ctx%bodc2n(3100002, name, found)
      call check(found .and. name == repeat('K', 60), 'bodies: bodc2n of a 60-character kernel name')
      call ctx%clpool()
      call check_ok('bodies')
   end subroutine test_bodies

   !> Surface names past ZZSRFTRN's limits: 3000 surfaces (MXNSRF 2000), a
   !> 60-character name (SFNMLN 36), and a method string listing 150
   !> surfaces (MAXSRF 100).
   subroutine test_surfaces()
      integer(ip), parameter :: n = 3000, body = 499
      character(32) :: names(n)
      character(80) :: name
      character(:), allocatable :: method
      character(20) :: shape, subtyp, pntdef, trmtyp
      integer(ip) :: ids(n), bodies(n), i, code, nsurf
      integer(ip), allocatable :: srflst(:)
      logical :: found, pri, ok
      do i = 1, n
         write (names(i), '(a,i0)') 'TEST LIMITS SURFACE ', i
         ids(i) = 1000 + i
      end do
      bodies = body
      call ctx%pcpool('NAIF_SURFACE_NAME', n, names)
      call ctx%pipool('NAIF_SURFACE_CODE', n, ids)
      call ctx%pipool('NAIF_SURFACE_BODY', n, bodies)
      ok = .true.
      do i = 1, n, 29
         call ctx%srfs2c('test  limits surface ' // trim(names(i)(21:)), 'MARS', code, found)
         ok = ok .and. found .and. code == ids(i)
         call ctx%srfc2s(ids(i), body, name, found)
         ok = ok .and. found .and. name == names(i)
      end do
      call check_ok('surfaces: 3000 names')
      call check(ok, 'surfaces: 3000 names, both ways')

      call ctx%pcpool('NAIF_SURFACE_NAME', 1_ip, [repeat('S', 60)])
      call ctx%pipool('NAIF_SURFACE_CODE', 1_ip, [7_ip])
      call ctx%pipool('NAIF_SURFACE_BODY', 1_ip, [body])
      call ctx%srfs2c(repeat('s', 60), 'MARS', code, found)
      call check(found .and. code == 7, 'surfaces: a 60-character name')
      call ctx%srfc2s(7_ip, body, name, found)
      call check(found .and. name == repeat('S', 60), 'surfaces: srfc2s of a 60-character name')

      call ctx%pcpool('NAIF_SURFACE_NAME', n, names)
      call ctx%pipool('NAIF_SURFACE_CODE', n, ids)
      call ctx%pipool('NAIF_SURFACE_BODY', n, bodies)
      method = 'INTERCEPT/DSK/UNPRIORITIZED/SURFACES = '
      do i = 1, 150
         method = method // '"' // trim(names(i)) // '"'
         if (i < 150) method = method // ', '
      end do
      allocate (srflst(len(method)/2 + 1))
      call zzprsmet(ctx, body, method, size(srflst, kind=ip), shape, subtyp, pri, nsurf, srflst, &
                    pntdef, trmtyp)
      call check_ok('surfaces: parsing 150 surfaces')
      call check(nsurf == 150 .and. all(srflst(1:150) == ids(1:150)), 'surfaces: a method with 150 surfaces')
      call ctx%clpool()
      call check_ok('surfaces')
   end subroutine test_surfaces

   !> Define a type 1 spacecraft clock in the pool: two fields (moduli
   !> 1000000 and 256: 256 ticks per second), TDB, NPART partitions of
   !> 102400 ticks each, NREC coefficient records 40 s apart (one second
   !> per unit of the first field), so that the encoded SCLK is 256 times
   !> the TDB.
   subroutine define_clock(sc, npart, nrec)
      integer(ip), intent(in) :: sc  !! the clock ID (negative)
      integer(ip), intent(in) :: npart  !! number of partitions
      integer(ip), intent(in) :: nrec  !! number of coefficient records
      real(wp), allocatable :: coef(:), pstart(:), pend(:)
      character(40) :: id
      integer(ip) :: k
      write (id, '(i0)') -sc
      allocate (coef(3*nrec), pstart(npart), pend(npart))
      do k = 1, nrec
         coef(3*k - 2) = 256.0_wp*40*(k - 1)
         coef(3*k - 1) = 40.0_wp*(k - 1)
         coef(3*k) = 1.0_wp  ! seconds per unit of the first field (256 ticks)
      end do
      pstart = 0
      pend = 102400
      call ctx%pipool('SCLK_DATA_TYPE_' // trim(id), 1_ip, [1_ip])
      call ctx%pipool('SCLK01_TIME_SYSTEM_' // trim(id), 1_ip, [1_ip])
      call ctx%pipool('SCLK01_N_FIELDS_' // trim(id), 1_ip, [2_ip])
      call ctx%pdpool('SCLK01_MODULI_' // trim(id), 2_ip, [1000000.0_wp, 256.0_wp])
      call ctx%pdpool('SCLK01_OFFSETS_' // trim(id), 2_ip, [0.0_wp, 0.0_wp])
      call ctx%pipool('SCLK01_OUTPUT_DELIM_' // trim(id), 1_ip, [1_ip])
      call ctx%pdpool('SCLK_PARTITION_START_' // trim(id), npart, pstart)
      call ctx%pdpool('SCLK_PARTITION_END_' // trim(id), npart, pend)
      call ctx%pdpool('SCLK01_COEFFICIENTS_' // trim(id), 3*nrec, coef)
   end subroutine define_clock

   !> Type 1 SCLKs past SC01's limits: a clock with 100500 coefficient
   !> records (MXCOEF 100000) and 12000 partitions (MXPART 9999), used at an
   !> epoch in partition 10001, and 150 clocks used in turn (MXNCLK 100).
   subroutine test_sclk()
      real(wp) :: sclkdp, et
      character(60) :: str
      integer(ip) :: i
      logical :: ok
      call define_clock(-77_ip, 12000_ip, 100500_ip)
      call check_ok('sclk: defining the big clock')
      et = 4.0e6_wp
      call ctx%sce2c(-77_ip, et, sclkdp)
      call check_ok('sclk: sce2c')
      call check(abs(sclkdp - 256*et) < 1.0e-3_wp, 'sclk: sce2c past 100000 records')
      call ctx%sct2e(-77_ip, sclkdp, et)
      call check(abs(et - 4.0e6_wp) < 1.0e-6_wp, 'sclk: sct2e past 100000 records')
      call ctx%sce2s(-77_ip, 4.0e6_wp, str)
      call check_ok('sclk: sce2s')
      call check(str(1:6) == '10001/', 'sclk: a string in partition 10001: ' // trim(str))
      call ctx%scs2e(-77_ip, str, et)
      call check_ok('sclk: scs2e')
      call check(abs(et - 4.0e6_wp) < 1.0e-2_wp, 'sclk: scs2e in partition 10001')

      do i = 1, 150
         call define_clock(-1000_ip - i, 1_ip, 2_ip)
      end do
      ok = .true.
      do i = 1, 150
         call ctx%sce2c(-1000_ip - i, real(i, wp), sclkdp)
         ok = ok .and. abs(sclkdp - 256*i) < 1.0e-6_wp
      end do
      do i = 150, 1, -1
         call ctx%sce2c(-1000_ip - i, real(i, wp), sclkdp)
         ok = ok .and. abs(sclkdp - 256*i) < 1.0e-6_wp
      end do
      call check_ok('sclk: 150 clocks')
      call check(ok, 'sclk: 150 clocks in turn')
      call ctx%clpool()
   end subroutine test_sclk

   !> A body with 300 nutation/precession angles (MAXANG 200) gives the
   !> same orientation (TISBOD, BODEUL) as the body with only its first
   !> angle, the coefficients of the other 299 being zero.
   subroutine test_nutation()
      real(wp) :: tsipm(6, 6), tsipm1(6, 6), angles(600), ra, dec, w, lambda, ra1, dec1, w1
      real(wp) :: coef(300)
      integer(ip) :: i
      do i = 1, 300
         angles(2*i - 1) = 10.0_wp*i
         angles(2*i) = 1000.0_wp*i
      end do
      coef = 0
      coef(1) = 0.5_wp
      call define_nutation(9001_ip, 300_ip, angles, coef)
      call define_nutation(9002_ip, 1_ip, angles, coef)
      call ctx%tisbod('J2000', 9001_ip, 1.0e8_wp, tsipm)
      call ctx%tisbod('J2000', 9002_ip, 1.0e8_wp, tsipm1)
      call check_ok('nutation: tisbod with 300 angles')
      call check(maxval(abs(tsipm - tsipm1)) < 1.0e-14_wp .and. maxval(abs(tsipm)) > 0, &
                 'nutation: tisbod with 300 angles')
      call ctx%bodeul(9001_ip, 1.0e8_wp, ra, dec, w, lambda)
      call ctx%bodeul(9002_ip, 1.0e8_wp, ra1, dec1, w1, lambda)
      call check_ok('nutation: bodeul with 300 angles')
      call check(abs(ra - ra1) + abs(dec - dec1) + abs(w - w1) < 1.0e-12_wp, 'nutation: bodeul with 300 angles')
      call ctx%clpool()
   end subroutine test_nutation

   !> PCK constants of a body with its own nutation/precession angles
   !> (the first n of ANGLES) and coefficients (the first n of COEF).
   subroutine define_nutation(body, n, angles, coef)
      integer(ip), intent(in) :: body  !! the body ID code
      integer(ip), intent(in) :: n  !! number of angles
      real(wp), intent(in) :: angles(:)  !! angle pairs (constant, rate)
      real(wp), intent(in) :: coef(:)  !! coefficients of the angles
      character(40) :: b
      write (b, '(a,i0,a)') 'BODY', body, '_'
      call ctx%pdpool(trim(b) // 'POLE_RA', 3_ip, [10.0_wp, 0.1_wp, 0.0_wp])
      call ctx%pdpool(trim(b) // 'POLE_DEC', 3_ip, [60.0_wp, -0.1_wp, 0.0_wp])
      call ctx%pdpool(trim(b) // 'PM', 3_ip, [20.0_wp, 300.0_wp, 0.0_wp])
      call ctx%pdpool(trim(b) // 'NUT_PREC_ANGLES', 2*n, angles(1:2*n))
      call ctx%pdpool(trim(b) // 'NUT_PREC_RA', n, coef(1:n))
      call ctx%pdpool(trim(b) // 'NUT_PREC_DEC', n, coef(1:n))
      call ctx%pdpool(trim(b) // 'NUT_PREC_PM', n, coef(1:n))
      call ctx%pdpool(trim(b) // 'LONG_AXIS', 1_ip, [0.0_wp])
   end subroutine define_nutation

   !> Frame names longer than 32 characters (FRNMLN), which differ only
   !> after the 32nd: TK frames, a TK frame relative to one of them, a
   !> dynamic frame defined by its name (FRAME_<name>_<item> is then longer
   !> than 32 characters, VARNAMETOOLONG in SPICELIB), a product and a
   !> switch frame of them, frames of objects and classes, and states in
   !> those frames.
   subroutine test_frame_names()
      character(*), parameter :: stem = 'A_FRAME_NAME_LONGER_THAN_THIRTY_TWO_CHARACTERS_'
      character(*), parameter :: dyn = 'A_DYNAMIC_FRAME_NAME_LONGER_THAN_THIRTY_TWO_CHARACTERS'
      character(*), parameter :: spk = prefix // 'frame_names.bsp'
      character(80) :: name
      integer(ip) :: i, code, cent, idset(-5:10)
      logical :: found
      real(wp) :: rot(3, 3), rot2(3, 3), xf(6, 6), st(6), st2(6), lt
      real(wp), parameter :: dpr = 180/acos(-1.0_wp)
      call define_tk(stem // '1', 1800001_ip, 'J2000', 10.0_wp)
      call define_tk(stem // '2', 1800002_ip, 'J2000', 20.0_wp)
      call define_tk(stem // '3', 1800003_ip, stem // '1', 5.0_wp)
      call define_mod(dyn, 1800004_ip)
      call define_mod('SHORT_MOD', 1800005_ip)
      call check_ok('frame names: definitions')
      do i = 1, 3
         call ctx%namfrm(stem // achar(48 + i), code)
         call ctx%frmnam(1800000_ip + i, name)
         call check(code == 1800000 + i .and. name == stem // achar(48 + i), 'frame names: namfrm/frmnam ' // achar(48 + i))
      end do
      call ctx%namfrm(dyn, code)
      call check(code == 1800004, 'frame names: namfrm of the dynamic frame')
      call check_ok('frame names: namfrm')
      call ctx%pxform(stem // '1', 'J2000', 0.0_wp, rot)
      call check(abs(atan2(rot(1, 2), rot(1, 1))*dpr - 10) < 1.0e-12_wp, 'frame names: pxform 1')
      call ctx%pxform(stem // '2', 'J2000', 0.0_wp, rot)
      call check(abs(atan2(rot(1, 2), rot(1, 1))*dpr - 20) < 1.0e-12_wp, 'frame names: pxform 2')
      call ctx%pxform(stem // '3', 'J2000', 0.0_wp, rot)
      call check(abs(atan2(rot(1, 2), rot(1, 1))*dpr - 15) < 1.0e-12_wp, 'frame names: pxform 3')
      call ctx%pxform(stem // '1', stem // '2', 0.0_wp, rot)
      call check(abs(atan2(rot(1, 2), rot(1, 1))*dpr + 10) < 1.0e-12_wp, 'frame names: pxform 1 to 2')
      call ctx%sxform(stem // '2', 'J2000', 0.0_wp, xf)
      call check(abs(atan2(xf(1, 2), xf(1, 1))*dpr - 20) < 1.0e-12_wp, 'frame names: sxform 2')
      call check_ok('frame names: pxform')
      call ctx%pxform(dyn, 'J2000', 1.0e8_wp, rot)
      call ctx%pxform('SHORT_MOD', 'J2000', 1.0e8_wp, rot2)
      call check_ok('frame names: pxform of the dynamic frame')
      call check(maxval(abs(rot - rot2)) < 1.0e-15_wp .and. maxval(abs(rot)) > 0, &
                 'frame names: pxform of the dynamic frame')
      ! Frames of an object (OBJECT_<ID or name>_FRAME), by class, and
      ! the list of TK frames.
      call ctx%pcpool('OBJECT_900001_FRAME', 1_ip, [stem // '2'])
      call ctx%cidfrm(900001_ip, code, name, found)
      call check(found .and. code == 1800002 .and. name == stem // '2', 'frame names: cidfrm')
      call ctx%cnmfrm('900001', code, name, found)
      call check(found .and. code == 1800002 .and. name == stem // '2', 'frame names: cnmfrm')
      call ctx%ccifrm(4_ip, 1800003_ip, code, name, cent, found)
      call check(found .and. code == 1800003 .and. name == stem // '3' .and. cent == 399, 'frame names: ccifrm')
      call ctx%ssizei(10_ip, idset)
      call ctx%kplfrm(4_ip, idset)
      call check(ctx%cardi(idset) == 3 .and. all(idset(1:3) == [1800001, 1800002, 1800003]), 'frame names: kplfrm')
      call check_ok('frame names: frames of objects and classes')
      ! A product frame and a switch frame built on the long names give
      ! what the same frames built on a short name give.
      call define_tk('SHORT_TK', 1800006_ip, 'J2000', 10.0_wp)
      call define_product(stem // 'PRODUCT', 1800007_ip, stem // '1')
      call define_product('SHORT_PRODUCT', 1800008_ip, 'SHORT_TK')
      call ctx%pxform(stem // 'PRODUCT', 'J2000', 0.0_wp, rot)
      call ctx%pxform('SHORT_PRODUCT', 'J2000', 0.0_wp, rot2)
      call check_ok('frame names: product frame')
      call check(maxval(abs(rot - rot2)) < 1.0e-15_wp .and. abs(rot(1, 1) - 1) > 1.0e-3_wp, 'frame names: product frame')
      call define_switch(stem // 'SWITCH', 1800009_ip, stem // '2')
      call ctx%pxform(stem // 'SWITCH', 'J2000', 0.0_wp, rot)
      call ctx%pxform(stem // '2', 'J2000', 0.0_wp, rot2)
      call check_ok('frame names: switch frame')
      call check(maxval(abs(rot - rot2)) < 1.0e-15_wp, 'frame names: switch frame')
      call write_spk(ctx, spk, 900001_ip, 0_ip, 1.0e5_wp, 0.0_wp)
      call ctx%furnsh(spk)
      call ctx%spkezr('900001', 0.0_wp, stem // '1', 'NONE', '0', st, lt)
      call ctx%spkezr('900001', 0.0_wp, stem // '2', 'NONE', '0', st2, lt)
      call check_ok('frame names: spkezr')
      call check(abs(atan2(st(2), st(1))*dpr - 10) < 1.0e-9_wp .and. abs(atan2(st2(2), st2(1))*dpr - 20) < 1.0e-9_wp, &
                 'frame names: spkezr')
      call ctx%spkpos('900001', 0.0_wp, stem // '1', 'NONE', '0', st, lt)
      call ctx%spkpos('900001', 0.0_wp, stem // '2', 'NONE', '0', st2, lt)
      call check_ok('frame names: spkpos')
      call check(abs(atan2(st(2), st(1))*dpr - 10) < 1.0e-9_wp .and. abs(atan2(st2(2), st2(1))*dpr - 20) < 1.0e-9_wp, &
                 'frame names: spkpos')
      call ctx%unload(spk)
      call delete_file(spk)
      call ctx%clpool()
      call check_ok('frame names: cleanup')
   end subroutine test_frame_names

   !> Define a TK frame rotated by ANGLE degrees about Z from RELTO.
   subroutine define_tk(name, id, relto, angle)
      character(*), intent(in) :: name  !! the frame name
      integer(ip), intent(in) :: id  !! the frame ID
      character(*), intent(in) :: relto  !! the frame it is relative to
      real(wp), intent(in) :: angle  !! the rotation (degrees)
      character(40) :: f, t
      write (f, '(a,i0,a)') 'FRAME_', id, '_'
      write (t, '(a,i0,a)') 'TKFRAME_', id, '_'
      call ctx%pipool('FRAME_' // name, 1_ip, [id])
      call ctx%pcpool(trim(f) // 'NAME', 1_ip, [name])
      call ctx%pipool(trim(f) // 'CLASS', 1_ip, [4_ip])
      call ctx%pipool(trim(f) // 'CLASS_ID', 1_ip, [id])
      call ctx%pipool(trim(f) // 'CENTER', 1_ip, [399_ip])
      call ctx%pcpool(trim(t) // 'RELATIVE', 1_ip, [relto])
      call ctx%pcpool(trim(t) // 'SPEC', 1_ip, ['ANGLES'])
      call ctx%pcpool(trim(t) // 'UNITS', 1_ip, ['DEGREES'])
      call ctx%pipool(trim(t) // 'AXES', 3_ip, [3_ip, 1_ip, 3_ip])
      call ctx%pdpool(trim(t) // 'ANGLES', 3_ip, [angle, 0.0_wp, 0.0_wp])
   end subroutine define_tk

   !> Define a mean-of-date dynamic frame (Earth, IAU 1976) by its name.
   subroutine define_mod(name, id)
      character(*), intent(in) :: name  !! the frame name
      integer(ip), intent(in) :: id  !! the frame ID
      character(40) :: f
      write (f, '(a,i0,a)') 'FRAME_', id, '_'
      call ctx%pipool('FRAME_' // name, 1_ip, [id])
      call ctx%pcpool(trim(f) // 'NAME', 1_ip, [name])
      call ctx%pipool(trim(f) // 'CLASS', 1_ip, [5_ip])
      call ctx%pipool(trim(f) // 'CLASS_ID', 1_ip, [id])
      call ctx%pipool(trim(f) // 'CENTER', 1_ip, [399_ip])
      call ctx%pcpool('FRAME_' // name // '_RELATIVE', 1_ip, ['J2000'])
      call ctx%pcpool('FRAME_' // name // '_DEF_STYLE', 1_ip, ['PARAMETERIZED'])
      call ctx%pcpool('FRAME_' // name // '_FAMILY', 1_ip, ['MEAN_EQUATOR_AND_EQUINOX_OF_DATE'])
      call ctx%pcpool('FRAME_' // name // '_PREC_MODEL', 1_ip, ['EARTH_IAU_1976'])
      call ctx%pcpool('FRAME_' // name // '_ROTATION_STATE', 1_ip, ['ROTATING'])
   end subroutine define_mod

   !> Define a product frame (a dynamic frame) from FACTOR to J2000.
   subroutine define_product(name, id, factor)
      character(*), intent(in) :: name  !! the frame name
      integer(ip), intent(in) :: id  !! the frame ID
      character(*), intent(in) :: factor  !! the "from" frame
      character(40) :: f
      write (f, '(a,i0,a)') 'FRAME_', id, '_'
      call ctx%pipool('FRAME_' // name, 1_ip, [id])
      call ctx%pcpool(trim(f) // 'NAME', 1_ip, [name])
      call ctx%pipool(trim(f) // 'CLASS', 1_ip, [5_ip])
      call ctx%pipool(trim(f) // 'CLASS_ID', 1_ip, [id])
      call ctx%pipool(trim(f) // 'CENTER', 1_ip, [399_ip])
      call ctx%pcpool(trim(f) // 'RELATIVE', 1_ip, ['J2000'])
      call ctx%pcpool(trim(f) // 'DEF_STYLE', 1_ip, ['PARAMETERIZED'])
      call ctx%pcpool(trim(f) // 'FAMILY', 1_ip, ['PRODUCT'])
      call ctx%pcpool(trim(f) // 'FROM_FRAMES', 1_ip, [factor])
      call ctx%pcpool(trim(f) // 'TO_FRAMES', 1_ip, ['J2000'])
   end subroutine define_product

   !> Define a switch frame aligned with a single base frame.
   subroutine define_switch(name, id, base)
      character(*), intent(in) :: name  !! the frame name
      integer(ip), intent(in) :: id  !! the frame ID
      character(*), intent(in) :: base  !! the base frame
      character(40) :: f
      write (f, '(a,i0,a)') 'FRAME_', id, '_'
      call ctx%pipool('FRAME_' // name, 1_ip, [id])
      call ctx%pcpool(trim(f) // 'NAME', 1_ip, [name])
      call ctx%pipool(trim(f) // 'CLASS', 1_ip, [6_ip])
      call ctx%pipool(trim(f) // 'CLASS_ID', 1_ip, [id])
      call ctx%pipool(trim(f) // 'CENTER', 1_ip, [399_ip])
      call ctx%pcpool(trim(f) // 'ALIGNED_WITH', 1_ip, [base])
   end subroutine define_switch

end program test_limits
