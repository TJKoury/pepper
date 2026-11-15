!> The sources of the kernel pool's variables (PEPPER): pool_sources (the
!> assignments in effect of a variable: their kernels, lines and first
!> values), pool_defined_by (the variables a kernel assigned) and
!> pool_write_json (the pool as a JSON kernel, with the sources in
!> comments, which loads back to the same variables).
program test_pool_sources

   use pepper_module, only: ip, wp, pepper_instance, pepper_context, string, frame_procedure

   implicit none

   character(*), parameter :: ka = 'test_pool_sources_a.tpc'  !! a text kernel
   character(*), parameter :: kb = 'test_pool_sources_b.tpc'  !! a text kernel that changes ka's variables
   character(*), parameter :: kc = 'test_pool_sources_c.json'  !! a JSON kernel that appends
   character(*), parameter :: out = 'test_pool_sources_out.json'  !! the pool written by pool_write_json
   type(pepper_instance), target :: spice, again
   type(pepper_context), pointer :: ctx  !! spice's own context
   integer :: failures = 0

   ctx => spice%context()
   call delete_files()
   call write_kernels()

   call test_text_kernels()
   call test_program_sources()
   call test_unload()
   call test_write_json()

   call spice%kclear()
   call again%kclear()
   call delete_files()
   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_pool_sources: all checks passed'

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

   !> The sources of a variable, compared with the expected ones.
   subroutine check_sources(name, sources, lines, first, what)
      character(*), intent(in) :: name  !! the variable
      character(*), intent(in) :: sources(:)  !! the expected sources
      integer(ip), intent(in) :: lines(:)  !! the expected lines
      integer(ip), intent(in) :: first(:)  !! the expected first values
      character(*), intent(in) :: what  !! what is checked
      type(string), allocatable :: s(:)
      integer(ip), allocatable :: l(:), f(:)
      logical :: found, same
      integer :: k
      call ctx%pool_sources(name, s, l, found, f)
      same = found .and. size(s) == size(sources)
      if (same) then
         do k = 1, size(s)
            same = same .and. s(k)%s == trim(sources(k)) .and. l(k) == lines(k) .and. f(k) == first(k)
         end do
      end if
      if (.not. same .and. found) then
         do k = 1, size(s)
            print '(4a, i0, a, i0)', '   ', name, ': ', s(k)%s, l(k), ' first ', f(k)
         end do
      end if
      call check(same, what)
   end subroutine check_sources

   !> The variables a source assigned, compared with the expected ones.
   subroutine check_defined_by(source, names, what)
      character(*), intent(in) :: source  !! the source
      character(*), intent(in) :: names(:)  !! the expected variables, in the pool's order
      character(*), intent(in) :: what  !! what is checked
      type(string), allocatable :: n(:)
      logical :: same
      integer :: k
      call ctx%pool_defined_by(source, n)
      same = size(n) == size(names)
      if (same) then
         do k = 1, size(n)
            same = same .and. n(k)%s == trim(names(k))
         end do
      end if
      call check(same, what)
   end subroutine check_defined_by

   !> Two text kernels: = and += of one variable, and a variable of the
   !> first replaced by the second.
   subroutine test_text_kernels()
      call spice%furnsh(ka)
      call spice%furnsh(kb)
      call check(.not. spice%failed(), 'text kernels: loaded')
      call check_sources('X', [character(40) :: ka, kb], [2_ip, 2_ip], [1_ip, 4_ip], &
                         'text kernels: X from ka (=) and kb (+=)')
      call check_sources('Y', [character(40) :: kb], [3_ip], [1_ip], &
                         'text kernels: Y replaced by kb (=)')
      call check_sources('NAME', [character(40) :: ka], [4_ip], [1_ip], &
                         'text kernels: NAME from ka, on the line where it begins')
      call check_defined_by(ka, [character(8) :: 'X', 'NAME'], &
                            'text kernels: ka defined X and NAME (Y was replaced)')
      call check_defined_by(kb, [character(8) :: 'X', 'Y'], 'text kernels: kb defined X and Y')
      call check_defined_by('nothing.tpc', [character(8) ::], 'text kernels: an unknown source defined nothing')
      block
         type(string), allocatable :: s(:)
         integer(ip), allocatable :: l(:)
         logical :: found
         call ctx%pool_sources('NO_SUCH_VARIABLE', s, l, found)
         call check(.not. found .and. size(s) == 0 .and. size(l) == 0, 'text kernels: an unknown variable')
      end block
   end subroutine test_text_kernels

   !> The values set by a program: PDPOOL, LMPOOL, a JSON kernel's append,
   !> a programmatic frame.
   subroutine test_program_sources()
      type(frame_procedure) :: provider
      call spice%pdpool('P', 2_ip, [1.5_wp, 2.5_wp])
      call check_sources('P', [character(40) :: '(PDPOOL)'], [0_ip], [1_ip], 'program: PDPOOL')
      call spice%lmpool([character(40) :: 'L1 = 1', 'L2 = ( ''a'',', '''b'' )'], 3_ip)
      call check_sources('L1', [character(40) :: '(LMPOOL)'], [1_ip], [1_ip], 'program: LMPOOL, line 1')
      call check_sources('L2', [character(40) :: '(LMPOOL)'], [2_ip], [1_ip], 'program: LMPOOL, line 2')
      call spice%furnsh(kc)
      call check(.not. spice%failed(), 'program: the JSON kernel loaded')
      call check_sources('X', [character(40) :: ka, kb, kc], [2_ip, 2_ip, 3_ip], [1_ip, 4_ip, 6_ip], &
                         'program: X appended by the JSON kernel ("+X")')
      provider%f => identity_frame
      call spice%add_frame('TEST_SOURCES_FRAME', 1400901_ip, 399_ip, provider)
      call check(.not. spice%failed(), 'program: the programmatic frame added')
      call check_sources('FRAME_TEST_SOURCES_FRAME', [character(40) :: '(add_frame)'], [0_ip], [1_ip], &
                         'program: a programmatic frame''s definition')
      call spice%pipool('I', 1_ip, [7_ip])
      call check_sources('I', [character(40) :: '(PIPOOL)'], [0_ip], [1_ip], &
                         'program: PIPOOL after add_frame (its own tag again)')
   end subroutine test_program_sources

   !> Unloading kb: the pool is loaded again from the other kernels, and
   !> keeps no assignment of kb.
   subroutine test_unload()
      call spice%unload(kb)
      call check(.not. spice%failed(), 'unload: kb')
      call check_sources('X', [character(40) :: ka, kc], [2_ip, 3_ip], [1_ip, 4_ip], &
                         'unload: X from ka and kc')
      call check_sources('Y', [character(40) :: ka], [3_ip], [1_ip], 'unload: Y from ka again')
      call check_defined_by(kb, [character(8) ::], 'unload: kb defined nothing')
   end subroutine test_unload

   !> pool_write_json: the comments name the sources, and the file loads
   !> into another instance with the same variables and values (numbers
   !> that need 17 digits, huge and tiny ones, whole numbers, strings with
   !> quotes and backslashes, an array long enough to be written on several
   !> lines).
   subroutine test_write_json()
      type(pepper_context), pointer :: c2
      type(string), allocatable :: names(:), names2(:), s1(:), s2(:)
      real(wp), allocatable :: v1(:), v2(:)
      real(wp) :: long(40)
      character(200) :: line
      logical :: f1, f2, same, comment_x, comment_p
      integer :: unit, ios, k, i
      do i = 1, 40
         long(i) = i/7.0_wp
      end do
      call spice%pdpool('NUMBERS', 7_ip, [0.1_wp, 1.0_wp/3, 1.0e300_wp, -2.5e-300_wp, 123456789.0_wp, &
                                          -42.0_wp, 0.0_wp])
      call spice%pdpool('LONG', 40_ip, long)
      call spice%pcpool('STRINGS', 4_ip, [character(20) :: 'it''s', 'a "quote"', 'back\slash', &
                                          'tab' // achar(9) // 'tab'])
      open (newunit=unit, file=out, status='replace', action='write')
      call ctx%pool_write_json(int(unit, ip))
      close (unit)
      call check(.not. spice%failed(), 'write_json: written')
      ! the comments
      comment_x = .false.
      comment_p = .false.
      open (newunit=unit, file=out, status='old', action='read')
      do
         read (unit, '(a)', iostat=ios) line
         if (ios /= 0) exit
         if (index(line, '"X":') > 0) comment_x = index(line, '// ' // ka // ' line 2; ' // kc // ' line 3') > 0
         if (index(line, '"NUMBERS":') > 0) comment_p = index(line, '// (PDPOOL)') > 0
      end do
      close (unit)
      call check(comment_x, 'write_json: the comment of X names its kernels and lines')
      call check(comment_p, 'write_json: the comment of NUMBERS names PDPOOL')
      ! loaded into another instance: the same variables and values
      c2 => again%context()
      call again%furnsh(out)
      call check(.not. again%failed(), 'write_json: the file loads as a JSON kernel')
      call ctx%pool_names('*', names)
      call c2%pool_names('*', names2)
      call check(size(names) == size(names2), 'write_json: the same number of variables')
      same = size(names) == size(names2)
      do k = 1, size(names)
         if (.not. same) exit
         same = names(k)%s == names2(k)%s
         call ctx%pool_values(names(k)%s, v1, f1)
         call c2%pool_values(names(k)%s, v2, f2)
         if (f1) then
            same = same .and. f2 .and. size(v1) == size(v2)
            if (same) same = all(v1 == v2)
         else
            call ctx%pool_strings(names(k)%s, s1, f1)
            call c2%pool_strings(names(k)%s, s2, f2)
            same = same .and. f1 .and. f2 .and. size(s1) == size(s2)
            do i = 1, size(s1)
               if (same) same = s1(i)%s == s2(i)%s
            end do
         end if
         if (.not. same) print '(2a)', '   different: ', names(k)%s
      end do
      call check(same, 'write_json: every variable has the same values (exactly)')
      call check_defined_by_in(c2, out, size(names), 'write_json: the loaded file is the source of every variable')
   end subroutine test_write_json

   !> The number of variables a source defined, in a context.
   subroutine check_defined_by_in(c, source, n, what)
      type(pepper_context), intent(inout) :: c  !! the context
      character(*), intent(in) :: source  !! the source
      integer, intent(in) :: n  !! the expected number of variables
      character(*), intent(in) :: what  !! what is checked
      type(string), allocatable :: names(:)
      call c%pool_defined_by(source, names)
      call check(size(names) == n, what)
   end subroutine check_defined_by_in

   !> A programmatic frame that is its relative frame.
   subroutine identity_frame(ctx, frame, et, xform, relative, found)
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: frame  !! the frame
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: xform(6, 6)  !! the transformation to the relative frame
      integer(ip), intent(out) :: relative  !! the relative frame
      logical, intent(out) :: found  !! the transformation is available
      integer :: i
      xform = 0.0_wp
      do i = 1, 6
         xform(i, i) = 1.0_wp
      end do
      relative = 1_ip
      found = .true.
   end subroutine identity_frame

   !> Write the kernels of the test.
   subroutine write_kernels()
      call write_file(ka, [character(40) :: &
                           '\begindata', &
                           'X = ( 1 2 3 )', &
                           'Y = 4', &
                           'NAME = ( ''one'',', &
                           '         ''two'' )', &
                           '\begintext'])
      call write_file(kb, [character(40) :: &
                           '\begindata', &
                           'X += ( 4 5 )', &
                           'Y = 7', &
                           '\begintext'])
      call write_file(kc, [character(40) :: &
                           '// appends to X', &
                           '{', &
                           '    "+X": [6]', &
                           '}'])
   end subroutine write_kernels

   !> Write a file of lines.
   subroutine write_file(file, lines)
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: lines(:)  !! its lines
      integer :: unit, k
      open (newunit=unit, file=file, status='replace', action='write')
      do k = 1, size(lines)
         write (unit, '(a)') trim(lines(k))
      end do
      close (unit)
   end subroutine write_file

   !> Delete the files of the test.
   subroutine delete_files()
      character(*), parameter :: files(4) = [character(40) :: ka, kb, kc, out]
      integer :: unit, ios, k
      logical :: exists
      do k = 1, size(files)
         inquire (file=trim(files(k)), exist=exists)
         if (exists) then
            open (newunit=unit, file=trim(files(k)), status='old', iostat=ios)
            if (ios == 0) close (unit, status='delete')
         end if
      end do
   end subroutine delete_files

end program test_pool_sources
