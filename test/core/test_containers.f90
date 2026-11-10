!> pepper_containers: growable arrays, the hash maps (against a plain
!> reference table, through many random insertions, removals and
!> rehashes) and the growth of a linked list pool (checked with the LNK
!> routines).
program test_containers

   use pepper_kinds, only: ip, wp
   use pepper_containers, only: string, string_map, int_map, grow, grown_capacity, lnkgrow
   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   integer :: failures = 0
   integer(ip) :: seed = 12345

   call test_grow()
   call test_string_map()
   call test_string_map_random()
   call test_int_map()
   call test_int_map_random()
   call test_lnkgrow()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_containers: all checks passed'

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

   !> A pseudo-random integer from 1 to n (a fixed sequence: the
   !> Park-Miller generator).
   integer(ip) function random(n)
      integer(ip), intent(in) :: n  !! the range
      seed = int(mod(int(seed, 8)*48271_8, 2147483647_8), ip)
      random = mod(seed, n) + 1
   end function random

   !> The key of number i in the string-map tests.
   function key_of(i) result(key)
      integer(ip), intent(in) :: i  !! the number
      character(:), allocatable :: key
      character(12) :: buf
      write (buf, '(i0)') i
      key = 'KEY_' // trim(buf)
   end function key_of

   !> grow keeps the contents, fills the new elements, allocates
   !> unallocated arrays and does nothing when there is room.
   subroutine test_grow()
      integer(ip), allocatable :: ia(:), i2(:, :)
      real(wp), allocatable :: ra(:), r2(:, :)
      logical, allocatable :: la(:)
      character(5), allocatable :: ca(:)
      type(string), allocatable :: sa(:)
      integer(ip) :: i

      call check(grown_capacity(0_ip, 1_ip) == 8, 'grown_capacity: minimum')
      call check(grown_capacity(10_ip, 11_ip) == 20, 'grown_capacity: doubles')
      call check(grown_capacity(10_ip, 50_ip) == 50, 'grown_capacity: at least needed')
      call check(grown_capacity(huge(1_ip)/2, huge(1_ip)/2 + 1) == huge(1_ip), 'grown_capacity: no overflow')

      call grow(ia, 3_ip)
      call check(size(ia) == 8 .and. all(ia == 0), 'grow int: new')
      do i = 1, 1000
         call grow(ia, i)
         ia(i) = i
      end do
      call check(size(ia) >= 1000 .and. all(ia(1:1000) == [(i, i = 1, 1000)]), 'grow int: appends')
      call check(all(ia(1001:) == 0), 'grow int: new elements are zero')
      i = size(ia, kind=ip)
      call grow(ia, 5_ip)
      call check(size(ia) == i, 'grow int: no change when there is room')

      call grow(ra, 20_ip)
      ra(20) = 1.5_wp
      call grow(ra, 21_ip)
      call check(size(ra) == 40 .and. ra(20) == 1.5_wp .and. all(ra(21:) == 0), 'grow real')

      call grow(la, 9_ip)
      la(9) = .true.
      call grow(la, 10_ip)
      call check(la(9) .and. .not. any(la(10:)) .and. count(la) == 1, 'grow logical')

      call grow(ca, 2_ip)
      call check(len(ca) == 5 .and. all(ca == ' '), 'grow char: new')
      ca(8) = 'ABCDE'
      call grow(ca, 9_ip)
      call check(ca(8) == 'ABCDE' .and. all(ca(9:) == ' ') .and. size(ca) == 16, 'grow char')

      call grow(sa, 1_ip)
      sa(1)%s = 'a long string'
      sa(8)%s = 'x'
      call grow(sa, 100_ip)
      call check(sa(1)%s == 'a long string' .and. sa(8)%s == 'x' .and. .not. allocated(sa(9)%s) &
                 .and. size(sa) == 100, 'grow string')

      call grow(i2, 3_ip, rows=2_ip)
      call check(size(i2, 1) == 2 .and. size(i2, 2) == 8, 'grow int 2d: new')
      i2(:, 8) = [7, 8]
      call grow(i2, 9_ip, rows=99_ip)
      call check(size(i2, 1) == 2 .and. size(i2, 2) == 16 .and. all(i2(:, 8) == [7, 8]) &
                 .and. all(i2(:, 9:) == 0), 'grow int 2d')

      call grow(r2, 1_ip, rows=6_ip)
      r2(:, 1) = 1.0_wp
      call grow(r2, 9_ip, rows=6_ip)
      call check(all(r2(:, 1) == 1.0_wp) .and. all(r2(:, 2:) == 0) .and. size(r2, 2) == 16, 'grow real 2d')
   end subroutine test_grow

   !> The basic operations of a string map.
   subroutine test_string_map()
      type(string_map) :: m
      integer(ip) :: v, i
      logical :: found

      call m%get('X', v, found)
      call check(.not. found .and. v == 0 .and. m%count() == 0, 'string_map: empty map')
      call m%remove('X')
      call check(m%count() == 0, 'string_map: remove from an empty map')

      call m%set('ALPHA', 1_ip)
      call m%set('BETA', 2_ip)
      call m%set('alpha', 3_ip)
      call m%get('ALPHA', v, found)
      call check(found .and. v == 1, 'string_map: get')
      call m%get('alpha', v, found)
      call check(found .and. v == 3, 'string_map: keys are case-sensitive')
      call m%get('ALPHA   ', v, found)
      call check(found .and. v == 1, 'string_map: trailing blanks of the query')
      call m%set('BETA  ', 4_ip)
      call m%get('BETA', v, found)
      call check(found .and. v == 4 .and. m%count() == 3, 'string_map: trailing blanks of the key')
      call m%get(' ALPHA', v, found)
      call check(.not. found, 'string_map: leading blanks are significant')
      call m%set(' ', 5_ip)
      call m%get('', v, found)
      call check(found .and. v == 5, 'string_map: a blank key')
      call m%remove('ALPHA')
      call m%get('ALPHA', v, found)
      call check(.not. found .and. m%count() == 3, 'string_map: remove')

      ! a key much longer than SPICELIB's names
      call m%set(repeat('LONG_', 1000), 6_ip)
      call m%get(repeat('LONG_', 1000), v, found)
      call check(found .and. v == 6, 'string_map: a 5000-character key')

      do i = 1, 100000
         call m%set(key_of(i), i)
      end do
      call check(m%count() == 100004, 'string_map: 100000 keys')
      found = .true.
      do i = 1, 100000
         call m%get(key_of(i), v, found)
         if (.not. found .or. v /= i) exit
      end do
      call check(found .and. v == 100000, 'string_map: all 100000 keys found')

      call m%clear()
      call m%get('BETA', v, found)
      call check(.not. found .and. m%count() == 0, 'string_map: clear')
      call m%set('BETA', 7_ip)
      call m%get('BETA', v, found)
      call check(found .and. v == 7, 'string_map: use after clear')
   end subroutine test_string_map

   !> Random insertions, updates and removals in a string map agree with a
   !> reference table (the removals exercise the backward shift).
   subroutine test_string_map_random()
      integer(ip), parameter :: nkeys = 5000, nops = 200000
      type(string_map) :: m
      integer(ip) :: ref(nkeys), v, i, k, op, n
      logical :: found, ok
      ref = -1
      ok = .true.
      do i = 1, nops
         k = random(nkeys)
         op = random(10_ip)
         if (op <= 5) then
            call m%set(key_of(k), i)
            ref(k) = i
         else if (op <= 8) then
            call m%remove(key_of(k))
            ref(k) = -1
         else
            call m%get(key_of(k), v, found)
            if (found .neqv. ref(k) >= 0) ok = .false.
            if (found .and. v /= ref(k)) ok = .false.
         end if
      end do
      n = 0
      do k = 1, nkeys
         call m%get(key_of(k), v, found)
         if (found .neqv. ref(k) >= 0) ok = .false.
         if (found .and. v /= ref(k)) ok = .false.
         if (found) n = n + 1
      end do
      call check(ok, 'string_map: random operations agree with the reference')
      call check(n == m%count(), 'string_map: count after random operations')
   end subroutine test_string_map_random

   !> The basic operations of an integer map.
   subroutine test_int_map()
      type(int_map) :: m
      integer(ip) :: v, i
      logical :: found, ok

      call m%get(0_ip, v, found)
      call check(.not. found .and. v == 0, 'int_map: empty map')
      call m%set(-huge(1_ip) - 1, 1_ip)
      call m%set(huge(1_ip), 2_ip)
      call m%set(0_ip, 3_ip)
      call m%get(-huge(1_ip) - 1, v, found)
      call check(found .and. v == 1, 'int_map: the most negative key')
      call m%get(huge(1_ip), v, found)
      call check(found .and. v == 2, 'int_map: the largest key')
      call m%get(0_ip, v, found)
      call check(found .and. v == 3 .and. m%count() == 3, 'int_map: key 0')
      call m%remove(huge(1_ip))
      call m%get(huge(1_ip), v, found)
      call check(.not. found .and. m%count() == 2, 'int_map: remove')

      ! frame-like IDs: consecutive and spaced
      ok = .true.
      do i = 1, 50000
         call m%set(1400000 + i, i)
         call m%set(-82000 - 1000*i, -i)
      end do
      do i = 1, 50000
         call m%get(1400000 + i, v, found)
         if (.not. found .or. v /= i) ok = .false.
         call m%get(-82000 - 1000*i, v, found)
         if (.not. found .or. v /= -i) ok = .false.
      end do
      call check(ok .and. m%count() == 100002, 'int_map: 100000 keys')
      call m%clear()
      call check(m%count() == 0, 'int_map: clear')
   end subroutine test_int_map

   !> Random insertions, updates and removals in an integer map agree with
   !> a reference table.
   subroutine test_int_map_random()
      integer(ip), parameter :: nkeys = 5000, nops = 200000
      type(int_map) :: m
      integer(ip) :: ref(nkeys), v, i, k, op, n
      logical :: found, ok
      ref = -1
      ok = .true.
      do i = 1, nops
         k = random(nkeys)
         op = random(10_ip)
         ! keys spread over a wide range, including negative ones
         if (op <= 5) then
            call m%set(key(k), i)
            ref(k) = i
         else if (op <= 8) then
            call m%remove(key(k))
            ref(k) = -1
         else
            call m%get(key(k), v, found)
            if (found .neqv. ref(k) >= 0) ok = .false.
            if (found .and. v /= ref(k)) ok = .false.
         end if
      end do
      n = 0
      do k = 1, nkeys
         call m%get(key(k), v, found)
         if (found .neqv. ref(k) >= 0) ok = .false.
         if (found .and. v /= ref(k)) ok = .false.
         if (found) n = n + 1
      end do
      call check(ok, 'int_map: random operations agree with the reference')
      call check(n == m%count(), 'int_map: count after random operations')
   end subroutine test_int_map_random

   !> The integer key of number k in the random test.
   pure integer(ip) function key(k)
      integer(ip), intent(in) :: k  !! the number
      key = (k - 2500)*65536 + 7
   end function key

   !> A grown pool keeps its lists, and its new nodes can be allocated and
   !> linked like the old ones.
   subroutine test_lnkgrow()
      integer(ip), parameter :: lbpool = -5
      type(pepper_spice), target :: spice
      type(pepper_context) :: ctx
      integer(ip), allocatable :: pool(:, :)
      integer(ip) :: a(6), b(4), node, i, n
      logical :: ok

      ctx = spice%new_context()
      allocate (pool(2, lbpool:10))
      call ctx%lnkini(10_ip, pool)
      ! list A: 6 nodes, list B: 3 nodes; 1 free node is left
      do i = 1, 6
         call ctx%lnkan(pool, a(i))
         if (i > 1) call ctx%lnkila(a(i - 1), a(i), pool)
      end do
      do i = 1, 3
         call ctx%lnkan(pool, b(i))
         if (i > 1) call ctx%lnkila(b(i - 1), b(i), pool)
      end do
      call ctx%lnkfsl(a(3), a(3), pool)  ! free one node in the middle of A
      a(3:5) = a(4:6)
      call check(ctx%lnknfn(pool) == 2, 'lnkgrow: two free nodes before')

      call lnkgrow(pool, 5_ip)
      call check(ubound(pool, 2) == 10 .and. ctx%lnksiz(pool) == 10, 'lnkgrow: no shrinking')
      call lnkgrow(pool, 1000_ip)
      call check(lbound(pool, 2) == lbpool .and. ubound(pool, 2) == 1000, 'lnkgrow: bounds')
      call check(ctx%lnksiz(pool) == 1000 .and. ctx%lnknfn(pool) == 992, 'lnkgrow: size and free nodes')

      ! the lists are unchanged
      ok = ctx%lnkhl(a(5), pool) == a(1) .and. ctx%lnktl(a(1), pool) == a(5)
      node = a(1)
      do i = 2, 5
         node = ctx%lnknxt(node, pool)
         ok = ok .and. node == a(i)
      end do
      ok = ok .and. ctx%lnknxt(a(5), pool) <= 0 .and. ctx%lnkprv(a(1), pool) <= 0
      ok = ok .and. ctx%lnktl(b(1), pool) == b(3) .and. ctx%lnkprv(b(3), pool) == b(2)
      call check(ok, 'lnkgrow: the lists are kept')

      ! all the free nodes (old and new) can be allocated, and added to B
      n = 0
      do i = 1, 992
         call ctx%lnkan(pool, node)
         if (ctx%failed()) exit
         call ctx%lnkila(ctx%lnktl(b(1), pool), node, pool)
         n = n + 1
      end do
      call check(n == 992 .and. ctx%lnknfn(pool) == 0, 'lnkgrow: all free nodes allocated')
      call check(ctx%lnktl(b(1), pool) == node .and. ctx%lnkhl(node, pool) == b(1), 'lnkgrow: B grew')
      call ctx%lnkan(pool, node)
      call check(ctx%failed(), 'lnkgrow: the full pool signals NOFREENODES')
      call ctx%reset()
      ! a node index is never handed out twice: B has 995 distinct nodes
      n = 1
      node = b(1)
      do
         node = ctx%lnknxt(node, pool)
         if (node <= 0) exit
         n = n + 1
      end do
      call check(n == 995, 'lnkgrow: list B has 995 nodes')
      call check(.not. ctx%failed(), 'lnkgrow: no errors')
   end subroutine test_lnkgrow

end program test_containers
