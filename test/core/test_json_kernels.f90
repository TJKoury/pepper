!> Unit tests of JSON kernels (pepper_json, and their loading by LDPOOL
!> and FURNSH).
program test_json_kernels

   use pepper_kinds, only: ip, wp
   use pepper_containers, only: string
   use pepper_instance, only: pepper_spice, pepper_context
   use pepper_json, only: json_assignment, json_kernel_parse, json_numeric, json_character
   use pepper_test_kernels, only: write_spk, write_lsk, delete_file

   implicit none

   integer :: failures = 0

   call test_parser()
   call test_parser_errors()
   call test_equivalence()
   call test_extension()
   call test_errors_atomic()
   call test_watches_bodies_frames()
   call test_keeper()
   call test_meta()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_json_kernels: all checks passed'

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

   !> Write a text file (lines without trailing blanks).
   subroutine write_file(file, lines)
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: lines(:)  !! its lines
      integer :: unit, k
      call delete_file(file)
      open (newunit=unit, file=file, status='new', action='write')
      do k = 1, size(lines)
         write (unit, '(a)') trim(lines(k))
      end do
      close (unit)
   end subroutine write_file

   !> The parser on valid kernels.
   subroutine test_parser()
      type(json_assignment), allocatable :: items(:)
      integer(ip) :: n, line
      logical :: ok
      character(:), allocatable :: short, long
      character(*), parameter :: nl = achar(10)

      call json_kernel_parse( &
         char(239)//char(187)//char(191)//'// a comment'//nl// &
         '{ "A": 1, "+A": [2, -3.5e2, true, false], // the end of a line'//nl// &
         '  "S": "x\"y\\z\/\né😀", "T": ["@2000-01-01", 4,], "E": "",'//nl// &
         '  "LIST": ["a", "b", ], }'//nl//'// done', items, n, ok, short, long, line)
      call check(ok, 'parser: a valid kernel ('//short//' '//long//')')
      if (.not. ok) return
      call check(n == 6, 'parser: six assignments')
      call check(items(1)%name == 'A' .and. .not. items(1)%append .and. items(1)%kind == json_numeric .and. &
                 items(1)%n == 1 .and. items(1)%numbers(1) == 1.0_wp, 'parser: a number')
      call check(items(2)%name == 'A' .and. items(2)%append .and. items(2)%n == 4, 'parser: an append')
      call check(all(items(2)%numbers(1:4) == [2.0_wp, -350.0_wp, 1.0_wp, 0.0_wp]), &
                 'parser: numbers, exponents, true and false')
      call check(items(3)%kind == json_character .and. &
                 items(3)%texts(1)%s == 'x"y\z/'//achar(10)//char(195)//char(169)//char(240)//char(159)// &
                 char(152)//char(128), 'parser: escapes, \u and a surrogate pair (UTF-8)')
      call check(items(3)%line == 3, 'parser: the line of a name')
      call check(items(4)%kind == json_numeric .and. items(4)%n == 2 .and. items(4)%is_time(1) .and. &
                 .not. items(4)%is_time(2) .and. items(4)%texts(1)%s == '2000-01-01', &
                 'parser: a time and a number in an array, a trailing comma')
      call check(items(5)%kind == json_character .and. items(5)%texts(1)%s == '', 'parser: an empty string')
      call check(items(6)%n == 2 .and. items(6)%texts(2)%s == 'b', 'parser: strings, a trailing comma')

      call json_kernel_parse('{}', items, n, ok, short, long, line)
      call check(ok .and. n == 0, 'parser: an empty object')
   end subroutine test_parser

   !> The parser's errors: the short message and the line.
   subroutine test_parser_errors()
      character(*), parameter :: nl = achar(10)
      call expect('', 'SPICE(JSONSYNTAXERROR)', 1, 'an empty text')
      call expect('[1]', 'SPICE(JSONSYNTAXERROR)', 1, 'not an object')
      call expect('{"A": 1}'//nl//'x', 'SPICE(JSONSYNTAXERROR)', 2, 'text after the object')
      call expect('{"A": 1'//nl//'"B": 2}', 'SPICE(JSONSYNTAXERROR)', 2, 'a missing comma')
      call expect('{"A": 1,', 'SPICE(JSONSYNTAXERROR)', 1, 'a missing }')
      call expect('/* c */ {"A": 1}', 'SPICE(JSONSYNTAXERROR)', 1, 'a block comment')
      call expect('{"A": 01}', 'SPICE(JSONSYNTAXERROR)', 1, 'a leading zero')
      call expect('{"A": 1.}', 'SPICE(JSONSYNTAXERROR)', 1, 'no digits after the point')
      call expect('{"A": .5}', 'SPICE(JSONSYNTAXERROR)', 1, 'no digits before the point')
      call expect('{"A": 1e}', 'SPICE(JSONSYNTAXERROR)', 1, 'no exponent digits')
      call expect('{"A": "x}', 'SPICE(JSONSYNTAXERROR)', 1, 'an unterminated string')
      call expect('{"A": "x\q"}', 'SPICE(JSONSYNTAXERROR)', 1, 'an unknown escape')
      call expect('{"A": "\u12"}', 'SPICE(JSONSYNTAXERROR)', 1, 'a short \u')
      call expect('{"A": "\ud83d"}', 'SPICE(JSONSYNTAXERROR)', 1, 'an unpaired surrogate')
      call expect('{"A": "a'//nl//'b"}', 'SPICE(JSONSYNTAXERROR)', 1, 'a line end in a string')
      call expect('{"A": tru}', 'SPICE(JSONSYNTAXERROR)', 1, 'a bad literal')
      call expect('{A: 1}', 'SPICE(JSONSYNTAXERROR)', 1, 'an unquoted name')
      call expect('{'//nl//nl//'"A": null}', 'SPICE(BADVARASSIGN)', 3, 'null')
      call expect('{"A": {"B": 1}}', 'SPICE(BADVARASSIGN)', 1, 'a nested object')
      call expect('{"A": [[1]]}', 'SPICE(BADVARASSIGN)', 1, 'a nested array')
      call expect('{"A": []}', 'SPICE(BADVARASSIGN)', 1, 'an empty array')
      call expect('{"A": [1, "x"]}', 'SPICE(BADVARASSIGN)', 1, 'a mixed array')
      call expect('{"A": ["x", "@2000-01-01"]}', 'SPICE(BADVARASSIGN)', 1, 'strings and a time')
      call expect('{"": 1}', 'SPICE(BADVARNAME)', 1, 'an empty name')
      call expect('{"+": 1}', 'SPICE(BADVARNAME)', 1, 'an append without a name')
      call expect('{"A B": 1}', 'SPICE(BADVARNAME)', 1, 'a blank in a name')
   end subroutine test_parser_errors

   !> Parse a text that has an error, and check the error.
   subroutine expect(text, code, at, what)
      character(*), intent(in) :: text  !! the text
      character(*), intent(in) :: code  !! the expected short message
      integer, intent(in) :: at  !! the expected line
      character(*), intent(in) :: what  !! the case
      type(json_assignment), allocatable :: items(:)
      integer(ip) :: n, line
      logical :: ok
      character(:), allocatable :: short, long
      call json_kernel_parse(text, items, n, ok, short, long, line)
      call check(.not. ok .and. short == code .and. line == at .and. n == 0 .and. len_trim(long) > 0, &
                 'parser error: '//what//' (got '//short//' on line '//trim(itoa(line))//': '//long//')')
   end subroutine expect

   !> An integer as text (left-justified).
   function itoa(i) result(s)
      integer(ip), intent(in) :: i  !! the integer
      character(24) :: s
      write (s, '(i0)') i
   end function itoa

   !> The same data as a text kernel and as a JSON kernel give the same
   !> pool.
   subroutine test_equivalence()
      type(pepper_spice), target :: tpc, json
      type(pepper_context) :: ct, cj
      type(string), allocatable :: names(:), jnames(:), sv(:), jsv(:)
      real(wp), allocatable :: dv(:), jdv(:)
      logical :: found, jfound, same
      character(1) :: t, jt
      integer(ip) :: k, n, jn
      call write_lsk('test_json_eq.tls')
      call write_file('test_json_eq.tpc', [character(80) :: &
         'KPL/PCK', '\begindata', &
         'BODY399_RADII = ( 6378.1366 6378.1366 6356.7519 )', &
         'BODY399_GM = 398600.435436', &
         'LIST = ( ''a'' ''b c'' )', &
         'LIST += ''d''', &
         'NUMS = 1', 'NUMS += ( 2 3 )', &
         'EPOCH = @2000-JAN-01/12:00', &
         'EPOCHS = ( @1972-JAN-1 @1999-JUL-1 10 )', &
         'MSG = ''it''''s''', &
         'REPLACED = 1', 'REPLACED = ''x''', &
         'ODD/NAME.1 = -0.5', &
         '\begintext'])
      call write_file('test_json_eq.json', [character(80) :: &
         '// the same as test_json_eq.tpc', '{', &
         '  "BODY399_RADII": [6378.1366, 6378.1366, 6356.7519],', &
         '  "BODY399_GM": 398600.435436,', &
         '  "LIST": ["a", "b c"],', &
         '  "+LIST": "d",', &
         '  "NUMS": 1, "+NUMS": [2, 3],', &
         '  "EPOCH": "@2000-JAN-01/12:00",', &
         '  "EPOCHS": ["@1972-JAN-1", "@1999-JUL-1", 10],', &
         '  "MSG": "it''s",', &
         '  "REPLACED": 1, "REPLACED": "x",', &
         '  "ODD/NAME.1": -0.5', '}'])
      call tpc%furnsh('test_json_eq.tls')
      call tpc%furnsh('test_json_eq.tpc')
      call json%furnsh('test_json_eq.tls')
      call json%furnsh('test_json_eq.json')
      call check(.not. tpc%failed() .and. .not. json%failed(), 'equivalence: both load')
      ct = tpc%new_context()
      cj = json%new_context()
      call ct%pool_names('*', names)
      call cj%pool_names('*', jnames)
      call check(size(names) == size(jnames) .and. size(names) > 10, 'equivalence: the same number of variables')
      do k = 1, size(names)
         call ct%dtpool(names(k)%s, found, n, t)
         call cj%dtpool(names(k)%s, jfound, jn, jt)
         same = jfound .and. n == jn .and. t == jt
         if (same .and. t == 'N') then
            call ct%pool_values(names(k)%s, dv, found)
            call cj%pool_values(names(k)%s, jdv, jfound)
            same = all(dv == jdv)
         else if (same .and. t == 'C') then
            call ct%pool_strings(names(k)%s, sv, found)
            call cj%pool_strings(names(k)%s, jsv, jfound)
            same = all([(sv(n)%s == jsv(n)%s, n=1, size(sv))])
         end if
         call check(same, 'equivalence: the variable '//names(k)%s)
      end do
      call ct%destroy()
      call cj%destroy()
      call tpc%destroy()
      call json%destroy()
      call delete_file('test_json_eq.tls')
      call delete_file('test_json_eq.tpc')
      call delete_file('test_json_eq.json')
   end subroutine test_equivalence

   !> Files named *.json (any case) are JSON kernels; others are text
   !> kernels.
   subroutine test_extension()
      type(pepper_spice), target :: spice
      type(pepper_context) :: c
      real(wp), allocatable :: v(:)
      logical :: found
      call write_file('test_json_ext.JSON', [character(20) :: '{"EXT_A": 1}'])
      call write_file('test_json_ext.txt', [character(20) :: '{"EXT_B": 2}'])
      call spice%furnsh('test_json_ext.JSON')
      call spice%furnsh('test_json_ext.txt')
      call check(.not. spice%failed(), 'extension: both load')
      c = spice%new_context()
      call c%pool_values('EXT_A', v, found)
      call check(found, 'extension: .JSON is a JSON kernel')
      call c%pool_values('EXT_B', v, found)
      call check(.not. found, 'extension: JSON in a .txt file is a text kernel (with no data)')
      call c%destroy()
      call spice%destroy()
      call delete_file('test_json_ext.JSON')
      call delete_file('test_json_ext.txt')
   end subroutine test_extension

   !> Errors from the loader, and nothing of a kernel with an error is
   !> loaded.
   subroutine test_errors_atomic()
      type(pepper_spice), target :: spice
      type(pepper_context) :: c
      type(pepper_context), pointer :: own
      real(wp), allocatable :: v(:)
      logical :: found
      own => spice%context()
      call write_lsk('test_json_err.tls')
      call spice%furnsh('test_json_err.tls')
      call spice%pdpool('NUM', 1_ip, [1.0_wp])
      call spice%pcpool('STR', 1_ip, ['s'])
      call load_error(spice, '{"OK1": 1, "BAD": null}', 'SPICE(BADVARASSIGN)')
      call load_error(spice, '{"OK1": 1,'//achar(10)//' "+NUM": "x"}', 'SPICE(TYPEMISMATCH)')
      call load_error(spice, '{"OK1": 1, "+STR": 2}', 'SPICE(TYPEMISMATCH)')
      call load_error(spice, '{"OK1": 1, "+STR": "@2000-01-01"}', 'SPICE(TYPEMISMATCH)')
      call load_error(spice, '{"OK1": "s", "+OK1": 2}', 'SPICE(TYPEMISMATCH)')
      call load_error(spice, '{"OK1": 1, "T": "@not a time"}', 'SPICE(BADTIMESPEC)')
      call load_error(spice, '{"OK1": 1, "T": "@"}', 'SPICE(BADTIMESPEC)')
      call load_error(spice, '{"OK1": 1, "T": 2', 'SPICE(JSONSYNTAXERROR)')
      call spice%furnsh('test_json_missing.json')
      call check(spice%failed(), 'loader: a missing file')
      call own%reset()
      ! an append that changes a variable's type is fine after a replacement
      call write_file('test_json_err.json', [character(40) :: '{"NUM": "a", "+NUM": "b"}'])
      call spice%furnsh('test_json_err.json')
      call check(.not. spice%failed(), 'loader: an append after a replacement of another type')
      c = spice%new_context()
      call c%pool_values('OK1', v, found)
      call check(.not. found, 'loader: a kernel with an error loads nothing')
      call c%destroy()
      call spice%destroy()
      call delete_file('test_json_err.tls')
      call delete_file('test_json_err.json')
   end subroutine test_errors_atomic

   !> Load a kernel with an error, and check the error.
   subroutine load_error(spice, text, code)
      type(pepper_spice), target, intent(inout) :: spice  !! the instance
      character(*), intent(in) :: text  !! the kernel
      character(*), intent(in) :: code  !! the expected short message
      type(pepper_context), pointer :: own
      character(80) :: short
      own => spice%context()
      call write_file('test_json_err.json', [character(len(text)) :: text])
      call spice%furnsh('test_json_err.json')
      call own%getmsg('SHORT', short)
      call check(spice%failed() .and. short == code, 'loader error '//code//': '//text//' (got '// &
                 trim(short)//')')
      call own%reset()
   end subroutine load_error

   !> A JSON kernel's changes reach watches, body names and frames.
   subroutine test_watches_bodies_frames()
      type(pepper_spice), target :: spice
      type(pepper_context) :: c
      logical :: update, found
      integer(ip) :: code
      real(wp) :: r(3, 3)
      c = spice%new_context()
      call c%swpool('AGENT', 1_ip, ['WATCHED'])
      call c%cvpool('AGENT', update)
      call write_file('test_json_bf.json', [character(80) :: &
         '{ "WATCHED": 1,', &
         '  "NAIF_BODY_NAME": "SPOCK", "NAIF_BODY_CODE": -1000,', &
         '  "FRAME_JSONFRAME": 1400000, "FRAME_1400000_NAME": "JSONFRAME",', &
         '  "FRAME_1400000_CLASS": 4, "FRAME_1400000_CLASS_ID": 1400000,', &
         '  "FRAME_1400000_CENTER": 399, "TKFRAME_1400000_RELATIVE": "J2000",', &
         '  "TKFRAME_1400000_SPEC": "ANGLES", "TKFRAME_1400000_UNITS": "DEGREES",', &
         '  "TKFRAME_1400000_AXES": [3, 1, 3], "TKFRAME_1400000_ANGLES": [90, 0, 0] }'])
      call spice%furnsh('test_json_bf.json')
      call check(.not. spice%failed(), 'frames: the kernel loads')
      call c%cvpool('AGENT', update)
      call check(update, 'watches: a JSON kernel''s variable updates its watch')
      call c%bodn2c('SPOCK', code, found)
      call check(found .and. code == -1000, 'bodies: a name defined in JSON')
      call c%pxform('J2000', 'JSONFRAME', 0.0_wp, r)
      call check(.not. c%failed() .and. abs(r(1, 2) + 1.0_wp) < 1e-12_wp .and. abs(r(2, 1) - 1.0_wp) < 1e-12_wp, &
                 'frames: a TK frame defined in JSON')
      call c%destroy()
      call spice%destroy()
      call delete_file('test_json_bf.json')
   end subroutine test_watches_bodies_frames

   !> KEEPER: kdata, ktotal, unload and kclear with JSON kernels.
   subroutine test_keeper()
      type(pepper_spice), target :: spice
      type(pepper_context) :: c
      real(wp), allocatable :: v(:)
      logical :: found
      integer(ip) :: count, handle
      character(256) :: file, srcfil
      character(32) :: filtyp
      call write_file('test_json_k1.json', [character(40) :: '{"K1": 1, "SHARED": 1}'])
      call write_file('test_json_k2.tpc', [character(40) :: 'KPL/PCK', '\begindata', 'K2 = 2', 'SHARED += 2', &
                                           '\begintext'])
      call spice%furnsh('test_json_k1.json')
      call spice%furnsh('test_json_k2.tpc')
      c = spice%new_context()
      call c%ktotal('TEXT', count)
      call check(count == 2, 'keeper: two TEXT kernels')
      call c%kdata(1_ip, 'ALL', file, filtyp, srcfil, handle, found)
      call check(found .and. file == 'test_json_k1.json' .and. filtyp == 'TEXT', 'keeper: kdata of a JSON kernel')
      call c%pool_values('SHARED', v, found)
      call check(found .and. size(v) == 2, 'keeper: a text kernel appends to a JSON kernel''s variable')
      call spice%unload('test_json_k2.tpc')
      call c%pool_values('K1', v, found)
      call check(found, 'keeper: a JSON kernel is loaded again after the unload of a text kernel')
      call c%pool_values('SHARED', v, found)
      call check(found .and. size(v) == 1, 'keeper: ... with its own values only')
      call spice%furnsh('test_json_k2.tpc')
      call spice%unload('test_json_k1.json')
      call c%pool_values('K1', v, found)
      call check(.not. found, 'keeper: unload of a JSON kernel')
      call c%pool_values('K2', v, found)
      call check(found, 'keeper: ... keeps the text kernel')
      call spice%furnsh('test_json_k1.json')
      call spice%kclear()
      call c%ktotal('ALL', count)
      call c%pool_values('K1', v, found)
      call check(count == 0 .and. .not. found, 'keeper: kclear')
      call check(.not. spice%failed() .and. .not. c%failed(), 'keeper: no errors')
      call c%destroy()
      call spice%destroy()
      call delete_file('test_json_k1.json')
      call delete_file('test_json_k2.tpc')
   end subroutine test_keeper

   !> Meta-kernels: a JSON meta-kernel (with path symbols) listing text,
   !> JSON and SPK files, and a text meta-kernel listing a JSON kernel.
   subroutine test_meta()
      type(pepper_spice), target :: spice
      type(pepper_context) :: c
      real(wp), allocatable :: v(:)
      real(wp) :: state(6), lt
      logical :: found
      integer(ip) :: count, handle
      character(256) :: file, srcfil
      character(32) :: filtyp
      c = spice%new_context()
      call write_spk(c, 'test_json_meta.bsp', 1001_ip, 0_ip, 7000.0_wp, 0.0_wp)
      call write_file('test_json_meta1.tpc', [character(40) :: 'KPL/PCK', '\begindata', 'M1 = 1', '\begintext'])
      call write_file('test_json_meta2.json', [character(40) :: '{"M2": 2}'])
      call write_file('test_json_meta.json', [character(80) :: &
         '{ "PATH_VALUES": ["."], "PATH_SYMBOLS": ["HERE"],', &
         '  "KERNELS_TO_LOAD": ["$HERE/test_json_meta1.tpc", "$HERE/test_json_meta2.json",', &
         '                      "$HERE/test_json_meta.bsp"],', &
         '  "M0": 0 }'])
      call spice%furnsh('test_json_meta.json')
      call check(.not. spice%failed(), 'meta: a JSON meta-kernel loads')
      call c%ktotal('ALL', count)
      call check(count == 4, 'meta: the meta-kernel and its three kernels')
      call c%kdata(1_ip, 'ALL', file, filtyp, srcfil, handle, found)
      call check(found .and. filtyp == 'META', 'meta: the JSON kernel is a META kernel')
      call c%pool_values('M0', v, found)
      call check(found, 'meta: the meta-kernel''s own variable')
      call c%pool_values('M1', v, found)
      call check(found, 'meta: the text kernel it lists')
      call c%pool_values('M2', v, found)
      call check(found, 'meta: the JSON kernel it lists')
      call c%spkgeo(1001_ip, 100.0_wp, 'J2000', 0_ip, state, lt)
      call check(.not. c%failed(), 'meta: the SPK it lists')
      call c%dtpool('KERNELS_TO_LOAD', found, count, filtyp)
      call check(.not. found, 'meta: KERNELS_TO_LOAD is removed')
      call spice%unload('test_json_meta.json')
      call c%ktotal('ALL', count)
      call check(count == 0, 'meta: unload of the JSON meta-kernel unloads its kernels')
      ! a text meta-kernel listing a JSON kernel
      call write_file('test_json_meta.tm', [character(80) :: 'KPL/MK', '\begindata', &
         'KERNELS_TO_LOAD = ( ''test_json_meta2.json'' )', '\begintext'])
      call spice%furnsh('test_json_meta.tm')
      call c%pool_values('M2', v, found)
      call check(.not. spice%failed() .and. found, 'meta: a text meta-kernel listing a JSON kernel')
      ! path values and file names longer than 80 characters (SPICELIB's
      ! limit of pool strings; STPOOL and SEPOOL read them whole)
      call spice%kclear()
      call write_file('test_json_meta.json', [character(300) :: &
         '{ "PATH_VALUES": ["'//repeat('./', 50)//'."], "PATH_SYMBOLS": ["HERE"],', &
         '  "KERNELS_TO_LOAD": ["$HERE/test_json_meta1.tpc", "'//repeat('./', 50)//'test_json_meta2.json"] }'])
      call spice%furnsh('test_json_meta.json')
      call c%pool_values('M1', v, found)
      call check(.not. spice%failed() .and. found, 'meta: a path value longer than 80 characters')
      call c%pool_values('M2', v, found)
      call check(found, 'meta: a file name longer than 80 characters')
      call c%destroy()
      call spice%destroy()
      call delete_file('test_json_meta.bsp')
      call delete_file('test_json_meta1.tpc')
      call delete_file('test_json_meta2.json')
      call delete_file('test_json_meta.json')
      call delete_file('test_json_meta.tm')
   end subroutine test_meta

end program test_json_kernels
