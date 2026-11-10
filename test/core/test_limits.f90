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
   use pepper_instance, only: pepper_spice, pepper_context, zzprsmet, zzelvupy, zzekspsh, zzekspop, zzeksrd, zzeksupd, zzekstop, zzekscln
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
   call test_search_order()
   call test_open_files()
   call test_leapseconds()
   call test_fov_vertices()
   call test_dsk_hits()
   call test_ek_scratch()
   call test_ek_tables()
   call test_ek_queries()
   call test_ek_strings()

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
      real(wp) :: state(6), lt, err
      integer(ip) :: i, count
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
      ! Every body (the first query of each read the summaries of every
      ! file loaded after its own before phase 5).
      err = 0
      do i = 1, n
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
      do i = 1, n
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

   !> 300 leapseconds (SPICELIB: DELTET's buffer of 200, BUFFERTOOSMALL;
   !> TTRANS's table of 140, the later ones silently dropped): Delta ET
   !> after the last one, and a UTC string (TTRANS) that agrees with it.
   subroutine test_leapseconds()
      integer(ip), parameter :: n = 300
      real(wp) :: dat(2*n), delta, delta2, et, sp2000
      character(40) :: utc
      character(80) :: errmsg
      integer(ip) :: i
      ! TAI-UTC = 9 + i seconds from midnight UTC, 180*i - 10000 days past J2000
      do i = 1, n
         dat(2*i - 1) = 9 + i
         dat(2*i) = (180.0_wp*i - 10000)*86400 - 43200
      end do
      call ctx%clpool()
      call ctx%pdpool('DELTET/DELTA_T_A', 1_ip, [32.184_wp])
      call ctx%pdpool('DELTET/K', 1_ip, [1.657e-3_wp])
      call ctx%pdpool('DELTET/EB', 1_ip, [1.671e-2_wp])
      call ctx%pdpool('DELTET/M', 2_ip, [6.239996_wp, 1.99096871e-7_wp])
      call ctx%pdpool('DELTET/DELTA_AT', 2*n, dat)
      call ctx%deltet(dat(2*n) + 1.0e6_wp, 'UTC', delta)
      call check_ok('leapseconds: deltet')
      call check(abs(delta - (32.184_wp + 9 + n)) < 0.01_wp, 'leapseconds: deltet after the last one')
      et = dat(2*n) + 1.0e6_wp + delta
      call ctx%deltet(et, 'ET', delta2)
      call check(abs(delta2 - delta) < 1.0e-6_wp, 'leapseconds: deltet (ET)')
      call ctx%et2utc(et, 'ISOC', 3_ip, utc)
      call ctx%tparse(utc, sp2000, errmsg)
      call check_ok('leapseconds: et2utc')
      call check(errmsg == ' ' .and. abs(et - sp2000 - delta) < 1.0e-3_wp, 'leapseconds: et2utc after the last one')
      call ctx%str2et(utc, et)
      call check_ok('leapseconds: str2et')
      call check(abs(et - (sp2000 + delta)) < 1.0e-3_wp, 'leapseconds: str2et after the last one')
      call ctx%clpool()
   end subroutine test_leapseconds

   !> A polygonal FOV with 12000 boundary vectors (SPICELIB: MAXVRT and
   !> MAXFOV, 10000): rays in and out of it (FOVRAY, through ZZGFFVIN),
   !> and ellipses in and out of its pyramid (ZZELVUPY).
   subroutine test_fov_vertices()
      integer(ip), parameter :: n = 12000
      real(wp), parameter :: half = 10*acos(-1.0_wp)/180  ! half angle of the FOV cone
      real(wp) :: bounds(3, n), ellips(9), a
      logical :: visible, found
      integer(ip) :: i
      do i = 1, n
         a = 2*acos(-1.0_wp)*(i - 1)/n
         bounds(:, i) = [sin(half)*cos(a), sin(half)*sin(a), cos(half)]
      end do
      call ctx%clpool()
      call ctx%pcpool('INS-900001_FOV_SHAPE', 1_ip, ['POLYGON'])
      call ctx%pcpool('INS-900001_FOV_FRAME', 1_ip, ['J2000'])
      call ctx%pdpool('INS-900001_BORESIGHT', 3_ip, [0.0_wp, 0.0_wp, 1.0_wp])
      call ctx%pdpool('INS-900001_FOV_BOUNDARY_CORNERS', 3*n, reshape(bounds, [3*n]))
      call ctx%fovray('-900001', [0.1_wp, 0.0_wp, 1.0_wp], 'J2000', 'NONE', '399', 0.0_wp, visible)
      call check_ok('FOV vertices: fovray')
      call check(visible, 'FOV vertices: a ray inside')
      call ctx%fovray('-900001', [0.2_wp, 0.0_wp, 1.0_wp], 'J2000', 'NONE', '399', 0.0_wp, visible)
      call check(.not. visible, 'FOV vertices: a ray outside')
      call ctx%cgv2el([0.0_wp, 0.0_wp, 100.0_wp], [1.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 1.0_wp, 0.0_wp], ellips)
      call zzelvupy(ctx, ellips, [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], n, bounds, found)
      call check_ok('FOV vertices: zzelvupy')
      call check(found, 'FOV vertices: an ellipse inside')
      call ctx%cgv2el([50.0_wp, 0.0_wp, 100.0_wp], [1.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 1.0_wp, 0.0_wp], ellips)
      call zzelvupy(ctx, ellips, [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], n, bounds, found)
      call check(.not. found, 'FOV vertices: an ellipse outside')
      call ctx%clpool()
   end subroutine test_fov_vertices

   !> 1100 DSK segments of one body that all contain a point
   !> (SPICELIB: 1000 hits, ZZDSKBUN's TOOMANYHITS and ZZDSKBUX's
   !> BUFFERTOOSMALL): the normal at a surface point (SRFNRM) and a ray
   !> intercept (DSKXV). The shape is an octahedron of radius 1000 km.
   subroutine test_dsk_hits()
      character(*), parameter :: dsk = prefix // 'hits.bds'
      integer(ip), parameter :: nseg = 1100, nv = 6, np = 8
      real(wp), parameter :: pi = acos(-1.0_wp), r = 1000.0_wp
      real(wp) :: vrtces(3, nv), spaixd(10), corpar(10), mncor3, mxcor3, normal(3), xpt(3)
      integer(ip) :: plates(3, np), handle, i
      integer(ip), allocatable :: work(:, :), spaixi(:)
      logical :: found(1)
      vrtces = reshape([r, 0.0_wp, 0.0_wp, -r, 0.0_wp, 0.0_wp, 0.0_wp, r, 0.0_wp, &
                        0.0_wp, -r, 0.0_wp, 0.0_wp, 0.0_wp, r, 0.0_wp, 0.0_wp, -r], [3, nv])
      ! outward plates: +x,+y,+z ... in each octant
      plates = reshape([1, 3, 5, 3, 2, 5, 2, 4, 5, 4, 1, 5, 3, 1, 6, 2, 3, 6, 4, 2, 6, 1, 4, 6], [3, np])
      allocate (work(2, 10000), spaixi(200000))
      corpar = 0
      call ctx%dskmi2(nv, vrtces, np, plates, 1.0_wp, 2_ip, 10000_ip, 10000_ip, 10000_ip, .true., &
                      200000_ip, work, spaixd, spaixi)
      call ctx%dskrb2(nv, vrtces, np, plates, 1_ip, corpar, mncor3, mxcor3)
      call delete_file(dsk)
      call ctx%dskopn(dsk, dsk, 0_ip, handle)
      do i = 1, nseg
         call ctx%dskw02(handle, 499_ip, 1_ip, 1_ip, 'IAU_MARS', 1_ip, corpar, -pi, pi, -pi/2, pi/2, &
                         mncor3, mxcor3, -1.0e9_wp, 1.0e9_wp, nv, vrtces, np, plates, spaixd, spaixi)
      end do
      call ctx%dskcls(handle, .true.)
      call check_ok('DSK hits: write')
      call ctx%furnsh(dsk)
      call ctx%srfnrm('DSK/UNPRIORITIZED', '499', 0.0_wp, 'IAU_MARS', 1_ip, [r/3, r/3, r/3], normal)
      call check_ok('DSK hits: srfnrm')
      call check(maxval(abs(normal - 1/sqrt(3.0_wp))) < 1.0e-9_wp, 'DSK hits: srfnrm normal')
      call ctx%dskxv(.false., '499', 0_ip, [0_ip], 0.0_wp, 'IAU_MARS', 1_ip, [2*r, 2*r, 2*r], &
                     [-1.0_wp, -1.0_wp, -1.0_wp], xpt, found)
      call check_ok('DSK hits: dskxv')
      call check(found(1) .and. maxval(abs(xpt - r/3)) < 1.0e-6_wp, 'DSK hits: dskxv intercept')
      call ctx%unload(dsk)
      call delete_file(dsk)
   end subroutine test_dsk_hits

   !> The EK scratch area past its in-memory part (2500000 integers,
   !> which now grows; beyond it the stack goes to a scratch DAS, opened
   !> then): push 3000000 integers, read and update ranges that cross the
   !> boundary, pop across it, and clean up.
   subroutine test_ek_scratch()
      integer(ip), parameter :: n = 3000000, memsiz = 2500000
      integer(ip), allocatable :: data(:), back(:)
      integer(ip) :: top, i
      allocate (data(n), back(n))
      do i = 1, n
         data(i) = i
      end do
      call zzekspsh(ctx, n/2, data)
      call zzekspsh(ctx, n - n/2, data(n/2 + 1:))
      call zzekstop(ctx, top)
      call check_ok('EK scratch: push')
      call check(top == n, 'EK scratch: stack top')
      call zzeksrd(ctx, memsiz - 10_ip, memsiz + 10_ip, back)
      call check(all(back(1:21) == data(memsiz - 10:memsiz + 10)), 'EK scratch: read across the boundary')
      call zzeksupd(ctx, memsiz - 1_ip, memsiz + 2_ip, [-1_ip, -2_ip, -3_ip, -4_ip])
      call zzeksrd(ctx, memsiz - 1_ip, memsiz + 2_ip, back)
      call check(all(back(1:4) == [-1, -2, -3, -4]), 'EK scratch: update across the boundary')
      data(memsiz - 1:memsiz + 2) = [-1, -2, -3, -4]
      call zzekspop(ctx, n - memsiz + 100, back)
      call check(all(back(1:n - memsiz + 100) == data(memsiz - 99:n)), 'EK scratch: pop across the boundary')
      call zzekscln(ctx)
      call zzekstop(ctx, top)
      call check_ok('EK scratch: clean up')
      call check(top == 0, 'EK scratch: empty after clean up')
      call zzekspsh(ctx, 3_ip, [7_ip, 8_ip, 9_ip])
      call zzekspop(ctx, 3_ip, back)
      call check(all(back(1:3) == [7, 8, 9]), 'EK scratch: reused after clean up')
      call zzekscln(ctx)
   end subroutine test_ek_scratch

   !> Loaded EKs past EKQMGR's old tables (20 files, 200 segments, 100
   !> tables, 500 column attribute entries, 10000 column descriptors): 25
   !> files of 10 segments with 50 integer columns each; segments 1 to 5
   !> of a file in tables of their own (T<file>_<k>), 6 to 10 in the table
   !> SHARED of every file. Queries over all files, then after unloading
   !> 10 of them, and after loading one again.
   subroutine test_ek_tables()
      integer(ip), parameter :: nfile = 25, ncol = 50
      character(32) :: cnames(ncol), table
      character(80) :: decls(ncol), errmsg
      character(:), allocatable :: ek
      integer(ip) :: f, k, c, handle, segno, recno, nrows, n, val, sum, expect
      logical :: error, null, found
      do c = 1, ncol
         write (cnames(c), '(a, i0)') 'C', c
         decls(c) = 'DATATYPE = INTEGER'
      end do
      do f = 1, nfile
         ek = kernel_name(prefix // 'ek', f, '.bes')
         call delete_file(ek)
         call ctx%ekopn(ek, ek, 0_ip, handle)
         do k = 1, 10
            if (k <= 5) then
               write (table, '(a, i0, a, i0)') 'T', f, '_', k
            else
               table = 'SHARED'
            end if
            call ctx%ekbseg(handle, table, ncol, cnames, decls, segno)
            call ctx%ekappr(handle, segno, recno)
            do c = 1, ncol
               call ctx%ekacei(handle, segno, recno, cnames(c), 1_ip, [1000*f + 10*k + c], .false.)
            end do
         end do
         call ctx%ekcls(handle)
         call ctx%furnsh(ek)
         if (ctx%failed()) then
            call check_ok('EK tables: write and load 25 EKs')
            return
         end if
      end do
      call ctx%ekntab(n)
      call check(n == 5*nfile + 1, 'EK tables: 126 tables')
      ! every file's 5 SHARED rows: C2 = 1000 f + 10 k + 2
      call ctx%ekfind('SELECT C2 FROM SHARED ORDER BY C2', nrows, error, errmsg)
      call check_ok('EK tables: query SHARED')
      call check(.not. error .and. nrows == 5*nfile, 'EK tables: SHARED rows')
      sum = 0
      do k = 1, nrows
         call ctx%ekgi(1_ip, k, 1_ip, val, null, found)
         sum = sum + val
      end do
      expect = 0
      do f = 1, nfile
         do k = 6, 10
            expect = expect + 1000*f + 10*k + 2
         end do
      end do
      call check(sum == expect, 'EK tables: SHARED values')
      call ctx%ekfind('SELECT C50 FROM T25_5 WHERE C1 = 25051', nrows, error, errmsg)
      call ctx%ekgi(1_ip, 1_ip, 1_ip, val, null, found)
      call check(.not. error .and. nrows == 1 .and. val == 25100, 'EK tables: a table of the last file')
      ! unload files 1 to 10
      do f = 1, 10
         call ctx%unload(kernel_name(prefix // 'ek', f, '.bes'))
      end do
      call ctx%ekntab(n)
      call check(n == 5*(nfile - 10) + 1, 'EK tables: tables after unloading')
      call ctx%ekfind('SELECT C2 FROM SHARED', nrows, error, errmsg)
      call check(.not. error .and. nrows == 5*(nfile - 10), 'EK tables: SHARED rows after unloading')
      call ctx%ekfind('SELECT C1 FROM T3_1', nrows, error, errmsg)
      call check(error, 'EK tables: a table of an unloaded file is gone')
      call ctx%furnsh(kernel_name(prefix // 'ek', 3_ip, '.bes'))
      call ctx%ekfind('SELECT C1 FROM T3_1', nrows, error, errmsg)
      call ctx%ekgi(1_ip, 1_ip, 1_ip, val, null, found)
      call check(.not. error .and. nrows == 1 .and. val == 3011, 'EK tables: loaded again')
      call check_ok('EK tables: queries')
      do f = 1, nfile
         ek = kernel_name(prefix // 'ek', f, '.bes')
         call ctx%unload(ek)
         call delete_file(ek)
      end do
      call ctx%ekntab(n)
      call check(n == 0, 'EK tables: none after unloading all')
   end subroutine test_ek_tables

   !> EK queries past the old parser limits: a WHERE clause of 150 OR'ed
   !> comparisons (about 2700 characters and 600 tokens; SPICELIB:
   !> MAXQRY = 2000, the rest cut off, and MAXTOK = 500), and one whose
   !> normal form (an OR of ANDs) has 128 conjunctions of 7 relations,
   !> more than its work pools start with (they grow); and past the
   !> counts: 250 conjunctions (MXJRS 200), 1024 constraints after
   !> normalization (MAXCON 1000), 60 SELECT and 12 ORDER BY columns
   !> (MAXSEL 50, MAXORD 10), a join of 11 tables (MAXTAB, MXJOIN 10) with
   !> 120 join constraints (MXJCON 100).
   subroutine test_ek_queries()
      character(*), parameter :: ek = prefix // 'queries.bes'
      character(:), allocatable :: query
      character(80) :: errmsg
      character(16) :: num
      integer(ip) :: handle, segno, recno, i, nrows, val, sum
      logical :: error, null, found
      call delete_file(ek)
      call ctx%ekopn(ek, ek, 0_ip, handle)
      call ctx%ekbseg(handle, 'Q', 1_ip, ['C1'], ['DATATYPE = INTEGER'], segno)
      do i = 1, 1000
         call ctx%ekappr(handle, segno, recno)
         call ctx%ekacei(handle, segno, recno, 'C1', 1_ip, [i], .false.)
      end do
      call ctx%ekcls(handle)
      call ctx%furnsh(ek)
      call check_ok('EK queries: write and load')
      query = 'SELECT C1 FROM Q WHERE C1 = 1'
      do i = 2, 150
         write (num, '(i0)') i
         query = query // '   OR   C1   =   ' // trim(num)
      end do
      call ctx%ekfind(query, nrows, error, errmsg)
      call check_ok('EK queries: 150 comparisons')
      call check(.not. error .and. nrows == 150 .and. len(query) > 2000, 'EK queries: 150 comparisons, ' // trim(errmsg))
      sum = 0
      do i = 1, nrows
         call ctx%ekgi(1_ip, i, 1_ip, val, null, found)
         sum = sum + val
      end do
      call check(sum == 150*151/2, 'EK queries: the 150 rows')
      query = 'SELECT C1 FROM Q WHERE (C1 = 1 OR C1 = 2)'
      do i = 3, 8
         write (num, '(i0)') i
         query = query // ' AND (C1 = 1 OR C1 = ' // trim(num) // ')'
      end do
      call ctx%ekfind(query, nrows, error, errmsg)
      call ctx%ekgi(1_ip, 1_ip, 1_ip, val, null, found)
      call check_ok('EK queries: 128 conjunctions')
      call check(.not. error .and. nrows == 1 .and. val == 1, 'EK queries: 128 conjunctions, ' // trim(errmsg))
      ! 250 conjunctions (SPICELIB: MXJRS = 200 join row sets)
      query = 'SELECT C1 FROM Q WHERE C1 = 1'
      do i = 2, 250
         write (num, '(i0)') i
         query = query // ' OR C1 = ' // trim(num)
      end do
      call ctx%ekfind(query, nrows, error, errmsg)
      call check_ok('EK queries: 250 conjunctions')
      call check(.not. error .and. nrows == 250, 'EK queries: 250 conjunctions, ' // trim(errmsg))
      ! 1024 constraints after normalization (SPICELIB: MAXCON = 1000)
      query = 'SELECT C1 FROM Q WHERE (C1 = 1 OR C1 = 2)'
      do i = 3, 9
         write (num, '(i0)') i
         query = query // ' AND (C1 = 1 OR C1 = ' // trim(num) // ')'
      end do
      call ctx%ekfind(query, nrows, error, errmsg)
      call ctx%ekgi(1_ip, 1_ip, 1_ip, val, null, found)
      call check_ok('EK queries: 1024 constraints')
      call check(.not. error .and. nrows == 1 .and. val == 1, 'EK queries: 1024 constraints, ' // trim(errmsg))
      ! 60 SELECT columns, 12 ORDER BY columns (SPICELIB: MAXSEL = 50, MAXORD = 10)
      query = 'SELECT C1' // repeat(', C1', 59) // ' FROM Q WHERE C1 < 4 ORDER BY C1' // repeat(', C1', 11)
      call ctx%ekfind(query, nrows, error, errmsg)
      call ctx%ekgi(60_ip, 3_ip, 1_ip, val, null, found)
      call check_ok('EK queries: 60 SELECT and 12 ORDER BY columns')
      call check(.not. error .and. nrows == 3 .and. val == 3, 'EK queries: 60 SELECT columns, ' // trim(errmsg))
      ! 11 tables, with 120 join constraints in one conjunction (SPICELIB:
      ! MAXTAB = MXJOIN = 10 tables, MXJCON = 100 join constraints)
      query = 'SELECT A11.C1 FROM Q A1'
      do i = 2, 11
         write (num, '(i0)') i
         query = query // ', Q A' // trim(num)
      end do
      query = query // ' WHERE A1.C1 = 7'
      do i = 2, 11
         write (num, '(i0)') i
         query = query // ' AND A' // trim(num) // '.C1 = A1.C1'
      end do
      do i = 1, 110
         query = query // ' AND A2.C1 = A1.C1'
      end do
      call ctx%ekfind(query, nrows, error, errmsg)
      call ctx%ekgi(1_ip, 1_ip, 1_ip, val, null, found)
      call check_ok('EK queries: 11 tables')
      call check(.not. error .and. nrows == 1 .and. val == 7, 'EK queries: 11 tables, ' // trim(errmsg))
      call ctx%unload(ek)
      call delete_file(ek)
   end subroutine test_ek_queries

   !> EK queries compare whole strings (SPICELIB: the first MAXSTR = 1024
   !> characters). The strings agree in their first 1200 characters. Rows
   !> are added one by one to columns with and without an index, and with
   !> the fast loader to columns of fixed-length strings: fixed-count
   !> (class 9, whose strings fit in a page) longer than the sort key of
   !> ZZEKJSRT (32), and longer than MAXSTR.
   subroutine test_ek_strings()
      character(*), parameter :: ek = prefix // 'strings.bes'
      character(*), parameter :: tails = 'cabd'
      character(1200) :: base
      character(1500) :: fixed(3), cdata
      character(40) :: short(3)
      character(:), allocatable :: query
      character(80) :: errmsg
      integer(ip) :: handle, segno, recno, i, nrows, rcptrs(3), wkindx(3)
      logical :: error, null, found, ok
      base = repeat('x', len(base))
      call delete_file(ek)
      call ctx%ekopn(ek, ek, 0_ip, handle)
      call ctx%ekbseg(handle, 'S', 2_ip, ['V', 'U'], &
                      ['DATATYPE = CHARACTER*(*), INDEXED = TRUE', &
                       'DATATYPE = CHARACTER*(*)                '], segno)
      do i = 1, 4
         call ctx%ekappr(handle, segno, recno)
         call ctx%ekacec(handle, segno, recno, 'V', 1_ip, [base // tails(i:i)], .false.)
         call ctx%ekacec(handle, segno, recno, 'U', 1_ip, [base // tails(i:i)], .false.)
      end do
      recno = 4
      call ctx%ekdelr(handle, segno, recno)
      do i = 1, 3
         short(i) = repeat('w', 34) // tails(i:i)
         fixed(i) = base // tails(i:i)
      end do
      call ctx%ekifld(handle, 'F', 1_ip, 3_ip, ['W'], ['DATATYPE = CHARACTER*(40), FIXED_COUNT = TRUE'], segno, rcptrs)
      call ctx%ekaclc(handle, segno, 'W', short, [1_ip, 1_ip, 1_ip], [.false., .false., .false.], rcptrs, wkindx)
      call ctx%ekffld(handle, segno, rcptrs)
      call ctx%ekifld(handle, 'G', 1_ip, 3_ip, ['L'], ['DATATYPE = CHARACTER*(1500)'], segno, rcptrs)
      call ctx%ekaclc(handle, segno, 'L', fixed, [1_ip, 1_ip, 1_ip], [.false., .false., .false.], rcptrs, wkindx)
      call ctx%ekffld(handle, segno, rcptrs)
      call ctx%ekcls(handle)
      call ctx%furnsh(ek)
      call check_ok('EK strings: write and load')
      ! an indexed and a plain column: =, LIKE
      query = 'SELECT V FROM S WHERE V = ''' // base // 'b'''
      call ctx%ekfind(query, nrows, error, errmsg)
      call check_ok('EK strings: indexed =')
      call check(.not. error .and. nrows == 1, 'EK strings: indexed =, ' // trim(errmsg))
      query = 'SELECT U FROM S WHERE U = ''' // base // 'b'''
      call ctx%ekfind(query, nrows, error, errmsg)
      call check_ok('EK strings: =')
      call check(.not. error .and. nrows == 1, 'EK strings: =, ' // trim(errmsg))
      call ctx%ekfind('SELECT U FROM S WHERE U LIKE ''*b''', nrows, error, errmsg)
      call check_ok('EK strings: LIKE')
      call check(.not. error .and. nrows == 1, 'EK strings: LIKE, ' // trim(errmsg))
      ! ORDER BY, after the deletion of a row from the index (ZZEKFRX)
      call ctx%ekfind('SELECT V FROM S ORDER BY V', nrows, error, errmsg)
      ok = .not. error .and. nrows == 3
      do i = 1, min(nrows, 3_ip)
         call ctx%ekgc(1_ip, i, 1_ip, cdata, null, found)
         ok = ok .and. cdata(1201:1201) == 'abc'(i:i)
      end do
      call check_ok('EK strings: ORDER BY')
      call check(ok, 'EK strings: ORDER BY, ' // trim(errmsg))
      ! joins
      call ctx%ekfind('SELECT S1.U FROM S S1, S S2 WHERE S1.U = S2.U', nrows, error, errmsg)
      call check_ok('EK strings: join =')
      call check(.not. error .and. nrows == 3, 'EK strings: join =, ' // trim(errmsg))
      call ctx%ekfind('SELECT S1.U FROM S S1, S S2 WHERE S1.U < S2.U', nrows, error, errmsg)
      call check_ok('EK strings: join <')
      call check(.not. error .and. nrows == 3, 'EK strings: join <, ' // trim(errmsg))
      call ctx%ekfind('SELECT S1.U FROM S S1, S S2 WHERE S1.U LIKE S2.U', nrows, error, errmsg)
      call check_ok('EK strings: join LIKE')
      call check(.not. error .and. nrows == 3, 'EK strings: join LIKE, ' // trim(errmsg))
      ! fixed-length strings: 40 characters (ORDER BY), 1500 (=)
      call ctx%ekfind('SELECT W FROM F ORDER BY W', nrows, error, errmsg)
      ok = .not. error .and. nrows == 3
      do i = 1, min(nrows, 3_ip)
         call ctx%ekgc(1_ip, i, 1_ip, cdata, null, found)
         ok = ok .and. cdata(35:35) == 'abc'(i:i)
      end do
      call check_ok('EK strings: ORDER BY, 40 characters')
      call check(ok, 'EK strings: ORDER BY, 40 characters, ' // trim(errmsg))
      query = 'SELECT L FROM G WHERE L = ''' // base // 'b'''
      call ctx%ekfind(query, nrows, error, errmsg)
      call ctx%ekgc(1_ip, 1_ip, 1_ip, cdata, null, found)
      call check_ok('EK strings: = 1500 characters')
      call check(.not. error .and. nrows == 1 .and. cdata(1201:1201) == 'b', &
                 'EK strings: = 1500 characters, ' // trim(errmsg))
      call ctx%unload(ek)
      call delete_file(ek)
   end subroutine test_ek_strings

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

   !> The number of files a context keeps open (SPICELIB's unit table,
   !> UTSIZE 23) is an instance setting: a context of an instance set to
   !> 64 queries 60 SPKs in turn, twice; the setting has a minimum of 4.
   subroutine test_open_files()
      integer(ip), parameter :: n = 60, first = 940001
      type(pepper_spice), target :: spice2
      type(pepper_context) :: ctx2
      real(wp) :: state(6), lt, err
      integer(ip) :: i, pass
      call check(spice2%max_open_files() == 23, 'open files: default')
      call spice2%set_max_open_files(2_ip)
      call check(spice2%max_open_files() == 4, 'open files: minimum')
      call spice2%set_max_open_files(64_ip)
      call check(spice2%max_open_files() == 64, 'open files: set')
      ctx2 = spice2%new_context()
      do i = 1, n
         call write_spk(ctx2, kernel_name(prefix // 'open_', i, '.bsp'), first + i - 1, 0_ip, &
                        1.0e5_wp, 1.0e-3_wp*i)
         call ctx2%furnsh(kernel_name(prefix // 'open_', i, '.bsp'))
      end do
      err = 0
      do pass = 1, 2
         do i = 1, n
            call ctx2%spkgeo(first + i - 1, 1.0e3_wp, 'J2000', 0_ip, state, lt)
            err = max(err, position_error(state, 1.0e5_wp, 1.0e-3_wp*i, 1.0e3_wp))
         end do
      end do
      call check(.not. ctx2%failed() .and. err < 1.0e-3_wp, 'open files: queries')
      call ctx2%kclear()
      do i = 1, n
         call delete_file(kernel_name(prefix // 'open_', i, '.bsp'))
      end do
   end subroutine test_open_files

   !> The order in which segments are searched (SPICELIB's): the file
   !> loaded last first, and within a file the last segment first; a file
   !> loaded again moves to the top, and unloading it undoes the last load
   !> (the file goes back to its first place); unloading a file loaded once
   !> removes its segments.
   !> The queries move back and forth so that the segment returned last for
   !> a body is reused only inside its re-use interval. SPK: the radius of
   !> the state tells the segment. CK (type 2, whose segments may cover a
   !> time without data for it, so the search goes on to the next segment):
   !> the rotation angle tells the segment.
   subroutine test_search_order()
      character(*), parameter :: spka = prefix // 'order_a.bsp', spkb = prefix // 'order_b.bsp'
      character(*), parameter :: cka = prefix // 'order_a.bc', ckb = prefix // 'order_b.bc'
      integer(ip), parameter :: body = 930001, inst = -930000
      real(wp), parameter :: times(10) = [100, 600, 850, 950, 1200, 100, 850, 1499, 501, 1600]
      real(wp), parameter :: radii(10) = [1000, 2000, 3000, 2000, 2000, 1000, 3000, 2000, 2000, 0]
      integer(ip) :: i
      logical :: ok
      call write_segments(spka, body, [0.0_wp, 500.0_wp], [1000.0_wp, 1500.0_wp], [1000.0_wp, 2000.0_wp])
      call write_segments(spkb, body, [800.0_wp], [900.0_wp], [3000.0_wp])
      call ctx%furnsh(spka)
      call ctx%furnsh(spkb)
      call check_ok('search order: furnsh')
      ok = .true.
      do i = 1, size(times)
         ok = ok .and. radius_at(body, times(i)) == radii(i)
      end do
      call check(ok, 'search order: SPK, two files, overlapping segments')
      call ctx%furnsh(spka)
      call check(radius_at(body, 850.0_wp) == 2000 .and. radius_at(body, 100.0_wp) == 1000 .and. &
                 radius_at(body, 880.0_wp) == 2000, 'search order: SPK, a file loaded again is searched first')
      ! UNLOAD undoes the last load: the file is back below the other.
      call ctx%unload(spka)
      call check(radius_at(body, 850.0_wp) == 3000 .and. radius_at(body, 100.0_wp) == 1000 .and. &
                 radius_at(body, 899.0_wp) == 3000, 'search order: SPK, the second load undone')
      call ctx%unload(spka)
      call check(radius_at(body, 850.0_wp) == 3000 .and. radius_at(body, 100.0_wp) == 0 .and. &
                 radius_at(body, 899.0_wp) == 3000, 'search order: SPK, unloaded')
      call ctx%kclear()
      call check_ok('search order: SPK')

      call write_ck(cka, inst, [0.0_wp], [100.0_wp], [0.0_wp], [100.0_wp], [10.0_wp])
      call write_ck(ckb, inst, [0.0_wp], [100.0_wp], [40.0_wp], [60.0_wp], [30.0_wp])
      call ctx%furnsh(cka)
      call ctx%furnsh(ckb)
      call check_ok('search order: CK furnsh')
      call check(angle_at(inst, 50.0_wp) == 30 .and. angle_at(inst, 20.0_wp) == 10 .and. angle_at(inst, 55.0_wp) == 30 .and. &
                 angle_at(inst, 70.0_wp) == 10 .and. angle_at(inst, 150.0_wp) == 0 .and. angle_at(inst, 45.0_wp) == 30, &
                 'search order: CK, the search goes on past a segment without data')
      call ctx%furnsh(cka)
      call check(angle_at(inst, 50.0_wp) == 10 .and. angle_at(inst, 20.0_wp) == 10, 'search order: CK, a file loaded again')
      call ctx%unload(cka)
      call check(angle_at(inst, 20.0_wp) == 10 .and. angle_at(inst, 50.0_wp) == 30, &
                 'search order: CK, the second load undone')
      call ctx%unload(cka)
      call check(angle_at(inst, 20.0_wp) == 0 .and. angle_at(inst, 50.0_wp) == 30, 'search order: CK, unloaded')
      call ctx%kclear()
      call check_ok('search order: CK')
      call delete_file(spka)
      call delete_file(spkb)
      call delete_file(cka)
      call delete_file(ckb)
   end subroutine test_search_order

   !> The radius of the body's state at ET (0 if it has none).
   real(wp) function radius_at(body, et) result(r)
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: et  !! the epoch
      real(wp) :: state(6), lt
      call ctx%spkgeo(body, et, 'J2000', 0_ip, state, lt)
      if (ctx%failed()) then
         call ctx%reset()
         r = 0
      else
         r = anint(norm2(state(1:3)))
      end if
   end function radius_at

   !> The rotation angle (degrees) of the instrument at TICKS (0 if it
   !> has no pointing).
   real(wp) function angle_at(inst, ticks) result(a)
      integer(ip), intent(in) :: inst  !! the instrument
      real(wp), intent(in) :: ticks  !! the encoded SCLK time
      real(wp) :: cmat(3, 3), clkout
      logical :: found
      call ctx%ckgp(inst, ticks, 0.0_wp, 'J2000', cmat, clkout, found)
      a = 0
      if (found) a = anint(abs(atan2(cmat(1, 2), cmat(1, 1)))*180/acos(-1.0_wp))
   end function angle_at

   !> Write an SPK whose segments for the body (in this order) cover
   !> [T0, T1] with a circle of radius R.
   subroutine write_segments(file, body, t0, t1, r)
      character(*), intent(in) :: file  !! the file
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: t0(:)  !! start of each segment
      real(wp), intent(in) :: t1(:)  !! end of each segment
      real(wp), intent(in) :: r(:)  !! radius of each segment
      integer, parameter :: m = 11
      real(wp) :: epochs(m), states(6, m), sum(5)
      integer(ip) :: h
      integer :: j, k
      call delete_file(file)
      call ctx%dafonw(file, 'SPK', 2_ip, 6_ip, 'PEPPER test SPK', 0_ip, h)
      do j = 1, size(t0)
         epochs = [(t0(j) + (t1(j) - t0(j))*(k - 1)/(m - 1), k = 1, m)]
         do k = 1, m
            states(:, k) = circle_state(r(j), 0.0_wp, epochs(k))
         end do
         call ctx%dafps(2_ip, 6_ip, [t0(j), t1(j)], [body, 0_ip, 1_ip, 9_ip, 0_ip, 0_ip], sum)
         call ctx%dafbna(h, sum, 'ORDER')
         call ctx%dafada(reshape(states, [6*m]), int(6*m, ip))
         call ctx%dafada(epochs, int(m, ip))
         call ctx%dafada([7.0_wp, real(m, wp)], 2_ip)
         call ctx%dafena()
      end do
      call ctx%dafcls(h)
   end subroutine write_segments

   !> Write a CK (type 2) whose segments for the instrument cover
   !> [B, E], each with one pointing interval [S, T] of rotation ANGLE
   !> (degrees) about Z.
   subroutine write_ck(file, inst, b, e, s, t, angle)
      character(*), intent(in) :: file  !! the file
      integer(ip), intent(in) :: inst  !! the instrument
      real(wp), intent(in) :: b(:)  !! start of each segment
      real(wp), intent(in) :: e(:)  !! end of each segment
      real(wp), intent(in) :: s(:)  !! start of each segment's interval
      real(wp), intent(in) :: t(:)  !! end of each segment's interval
      real(wp), intent(in) :: angle(:)  !! rotation of each segment (degrees)
      integer(ip) :: h
      integer :: j
      real(wp) :: half
      call delete_file(file)
      call ctx%ckopn(file, 'PEPPER test CK', 0_ip, h)
      do j = 1, size(b)
         half = angle(j)*acos(-1.0_wp)/360
         call ctx%ckw02(h, b(j), e(j), inst, 'J2000', 'ORDER', 1_ip, [s(j)], [t(j)], &
                        reshape([cos(half), 0.0_wp, 0.0_wp, sin(half)], [4, 1]), &
                        reshape([0.0_wp, 0.0_wp, 0.0_wp], [3, 1]), [1.0_wp])
      end do
      call ctx%ckcls(h)
   end subroutine write_ck

end program test_limits
