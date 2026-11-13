!> The parser of JSON text kernels (PEPPER; no SPICELIB counterpart).
!>
!> A JSON kernel is a text kernel written as one JSON object (the format of
!> [jsonspice](https://github.com/jacobwilliams/jsonspice)), in a file named
!> `*.json`:
!>```json
!>     // a comment
!>     {
!>         "BODY399_RADII": [6378.1366, 6378.1366, 6356.7519],
!>         "+NAIF_BODY_NAME": "SPOCK",       // "+NAME": append (the "+=" of text kernels)
!>         "+NAIF_BODY_CODE": -1000,
!>         "EPOCH": "@2026-NOV-11 12:00",    // "@": a time (UTC seconds past J2000)
!>         "FLAG": true,                     // true and false: 1 and 0
!>     }
!>```
!> The syntax is JSON (RFC 8259) with two extensions: `//` comments (to the
!> end of the line) and trailing commas in objects and arrays. A UTF-8 byte
!> order mark at the start is skipped. Values are numbers, strings, `true`,
!> `false`, or flat arrays of them; an array holds strings only, or numbers
!> and times only. `null`, nested objects and arrays, and empty arrays are
!> errors. A string starting with `@` is a time; the loader converts it
!> (TPARSE, as for `@` values in text kernels). A number beyond the range
!> of double precision is an error (SPICE(NUMBEREXPECTED), as in text
!> kernels; a smaller one than the smallest is zero); a string can't have
!> a NUL (`\u0000`) or a lone UTF-16 surrogate (`\ud800` to `\udfff` not
!> in a pair).
!>
!> [[json_kernel_parse]] turns the text into the list of assignments, in
!> the text's order. It has no state and does no I/O: the loader (LDPOOL,
!> in pepper_pool) reads the file and applies the assignments to the
!> kernel pool.
module pepper_json

   use pepper_kinds, only: ip, wp
   use pepper_containers, only: string

   implicit none

   private

   public :: json_assignment, json_kernel_parse
   public :: json_numeric, json_character

   integer, parameter :: json_numeric = 1  !! an assignment of numbers (and times)
   integer, parameter :: json_character = 2  !! an assignment of strings

   !> One `"NAME": value` of a JSON kernel.
   type :: json_assignment
      character(:), allocatable :: name  !! the variable (without the `+` of an append)
      logical :: append = .false.  !! `"+NAME"`: append to the variable
      integer :: kind = 0  !! [[json_numeric]] or [[json_character]]
      integer(ip) :: n = 0  !! number of values
      real(wp), allocatable :: numbers(:)  !! the numbers (numeric; times: set by the loader)
      logical, allocatable :: is_time(:)  !! the value is a time (numeric)
      type(string), allocatable :: texts(:)  !! the strings (character), or the times' text (without the `@`)
      integer(ip) :: line = 0  !! the line of the name, for the messages
   end type json_assignment

   !> The parser's state: its position in the text, and the first error.
   type :: json_cursor
      integer(ip) :: pos = 1  !! the next character
      integer(ip) :: line = 1  !! its line
      logical :: ok = .true.  !! no error so far
      character(:), allocatable :: short  !! the short error message
      character(:), allocatable :: long  !! the long error message
      integer(ip) :: error_line = 0  !! the line of the error
   end type json_cursor

contains

   !> Parse a JSON kernel: the assignments in the order of the text. On an
   !> error, `ok` is false, no assignments are returned, `short` is the
   !> short error message (SPICE(JSONSYNTAXERROR) for the syntax,
   !> SPICE(BADVARASSIGN) for a value a kernel variable cannot have,
   !> SPICE(BADVARNAME) for a name), `long` explains it and `line` is where
   !> it is.
   pure subroutine json_kernel_parse(text, items, nitems, ok, short, long, line)
      character(*), intent(in) :: text  !! the kernel's text
      type(json_assignment), allocatable, intent(out) :: items(:)  !! the assignments (the first `nitems`)
      integer(ip), intent(out) :: nitems  !! the number of assignments
      logical, intent(out) :: ok  !! the text is a valid JSON kernel
      character(:), allocatable, intent(out) :: short  !! the short error message (blank if ok)
      character(:), allocatable, intent(out) :: long  !! the long error message (blank if ok)
      integer(ip), intent(out) :: line  !! the line of the error (0 if ok)
      type(json_cursor) :: c
      type(json_assignment) :: item
      logical :: done

      nitems = 0
      allocate (items(16))
      parse: block
         ! a UTF-8 byte order mark
         if (len(text) >= 3) then
            if (text(1:3) == char(239)//char(187)//char(191)) c%pos = 4
         end if
         call skip_blanks(text, c)
         if (.not. c%ok) exit parse
         if (.not. at(text, c, '{')) then
            call syntax(c, 'the kernel must be one JSON object, starting with "{"')
            exit parse
         end if
         c%pos = c%pos + 1
         do
            call skip_blanks(text, c)
            if (.not. c%ok) exit parse
            if (at(text, c, '}')) exit              ! the end (also after a trailing comma)
            call read_assignment(text, c, item)
            if (.not. c%ok) exit parse
            call add_item(items, nitems, item)
            call skip_blanks(text, c)
            if (.not. c%ok) exit parse
            call separator(text, c, '}', done)
            if (.not. c%ok) exit parse
            if (done) exit
         end do
         c%pos = c%pos + 1
         call skip_blanks(text, c)
         if (.not. c%ok) exit parse
         if (c%pos <= len(text)) call syntax(c, 'text after the end of the kernel''s object')
      end block parse
      ok = c%ok
      if (ok) then
         short = ' '
         long = ' '
         line = 0
      else
         call move_alloc(c%short, short)
         call move_alloc(c%long, long)
         line = c%error_line
         nitems = 0
      end if
   end subroutine json_kernel_parse

   !> True if the next character is `ch`.
   pure logical function at(text, c, ch)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(in) :: c  !! the parser
      character, intent(in) :: ch  !! the character
      at = .false.
      if (c%pos <= len(text)) at = text(c%pos:c%pos) == ch
   end function at

   !> Skip blanks, line ends and `//` comments.
   pure subroutine skip_blanks(text, c)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(inout) :: c  !! the parser
      integer(ip) :: k
      do while (c%pos <= len(text))
         select case (text(c%pos:c%pos))
         case (' ', achar(9), achar(13))
            c%pos = c%pos + 1
         case (achar(10))
            c%pos = c%pos + 1
            c%line = c%line + 1
         case ('/')
            if (c%pos < len(text)) then
               if (text(c%pos + 1:c%pos + 1) == '/') then
                  k = index(text(c%pos:), achar(10))
                  if (k == 0) then
                     c%pos = len(text) + 1
                  else
                     c%pos = c%pos + k - 1    ! on the line end (counted above)
                  end if
                  cycle
               else if (text(c%pos + 1:c%pos + 1) == '*') then
                  call syntax(c, 'block comments (/* */) are not supported; use // comments')
                  return
               end if
            end if
            return
         case default
            return
         end select
      end do
   end subroutine skip_blanks

   !> After a value: a comma (`done` false) or the closing character
   !> (`done` true; the parser stays on it).
   pure subroutine separator(text, c, closing, done)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(inout) :: c  !! the parser
      character, intent(in) :: closing  !! `}` or `]`
      logical, intent(out) :: done  !! the closing character is next
      done = .false.
      if (at(text, c, closing)) then
         done = .true.
      else if (at(text, c, ',')) then
         c%pos = c%pos + 1
      else if (c%pos > len(text)) then
         call syntax(c, 'unexpected end of the text: missing "'//closing//'"')
      else
         call syntax(c, 'expected "," or "'//closing//'", found "'//text(c%pos:c%pos)//'"')
      end if
   end subroutine separator

   !> `"NAME": value`
   pure subroutine read_assignment(text, c, a)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(inout) :: c  !! the parser
      type(json_assignment), intent(out) :: a  !! the assignment
      character(:), allocatable :: key
      integer(ip) :: k
      logical :: done
      if (.not. at(text, c, '"')) then
         if (c%pos > len(text)) then
            call syntax(c, 'unexpected end of the text: missing "}"')
         else
            call syntax(c, 'expected a variable name (a string), found "'//text(c%pos:c%pos)//'"')
         end if
         return
      end if
      a%line = c%line
      call read_string(text, c, key)
      if (.not. c%ok) return
      a%append = .false.
      if (len(key) > 0) then
         if (key(1:1) == '+') then
            a%append = .true.
            key = key(2:)
         end if
      end if
      if (len(key) == 0) then
         call fail(c, 'SPICE(BADVARNAME)', 'a variable name is empty', a%line)
         return
      end if
      do k = 1, len(key)
         if (iachar(key(k:k)) <= 32 .or. iachar(key(k:k)) >= 127) then
            call fail(c, 'SPICE(BADVARNAME)', 'the variable name "'//key//'" has a blank or a '// &
                      'character that is not printable ASCII', a%line)
            return
         end if
      end do
      call move_alloc(key, a%name)
      call skip_blanks(text, c)
      if (.not. c%ok) return
      if (.not. at(text, c, ':')) then
         call syntax(c, 'expected ":" after the variable name "'//a%name//'"')
         return
      end if
      c%pos = c%pos + 1
      call skip_blanks(text, c)
      if (.not. c%ok) return
      allocate (a%numbers(1), a%is_time(1), a%texts(1))
      if (at(text, c, '[')) then
         c%pos = c%pos + 1
         do
            call skip_blanks(text, c)
            if (.not. c%ok) return
            if (at(text, c, ']')) exit
            call read_scalar(text, c, a)
            if (.not. c%ok) return
            call skip_blanks(text, c)
            if (.not. c%ok) return
            call separator(text, c, ']', done)
            if (.not. c%ok) return
            if (done) exit
         end do
         c%pos = c%pos + 1
         if (a%n == 0) then
            call fail(c, 'SPICE(BADVARASSIGN)', 'the variable "'//a%name//'" is assigned an empty array', a%line)
            return
         end if
      else
         call read_scalar(text, c, a)
      end if
   end subroutine read_assignment

   !> One value (a scalar): appended to the assignment's values.
   pure subroutine read_scalar(text, c, a)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(inout) :: c  !! the parser
      type(json_assignment), intent(inout) :: a  !! the assignment
      character(:), allocatable :: s
      real(wp) :: x
      integer :: kind
      logical :: time
      if (c%pos > len(text)) then
         call syntax(c, 'unexpected end of the text: missing a value')
         return
      end if
      x = 0.0_wp
      time = .false.
      select case (text(c%pos:c%pos))
      case ('"')
         call read_string(text, c, s)
         if (.not. c%ok) return
         if (len(s) > 0) time = s(1:1) == '@'
         if (time) then
            kind = json_numeric
            s = s(2:)
         else
            kind = json_character
         end if
      case ('-', '0':'9')
         call read_number(text, c, x)
         if (.not. c%ok) return
         kind = json_numeric
      case ('t', 'f', 'n')
         if (literal(text, c, 'true')) then
            x = 1.0_wp
            c%pos = c%pos + 4
         else if (literal(text, c, 'false')) then
            x = 0.0_wp
            c%pos = c%pos + 5
         else if (literal(text, c, 'null')) then
            call fail(c, 'SPICE(BADVARASSIGN)', 'the variable "'//a%name//'" is assigned null', c%line)
            return
         else
            call syntax(c, 'unexpected "'//text(c%pos:c%pos)//'"')
            return
         end if
         kind = json_numeric
      case ('{')
         call fail(c, 'SPICE(BADVARASSIGN)', 'the variable "'//a%name//'" is assigned an object; '// &
                   'nested objects are not supported', c%line)
         return
      case ('[')
         call fail(c, 'SPICE(BADVARASSIGN)', 'the variable "'//a%name//'" is assigned a nested array', c%line)
         return
      case default
         call syntax(c, 'unexpected "'//text(c%pos:c%pos)//'"')
         return
      end select
      if (a%n == 0) then
         a%kind = kind
      else if (kind /= a%kind) then
         call fail(c, 'SPICE(BADVARASSIGN)', 'the array assigned to "'//a%name//'" mixes strings with '// &
                   'numbers or times', c%line)
         return
      end if
      call grow_values(a)
      a%n = a%n + 1
      a%is_time(a%n) = time
      a%numbers(a%n) = x
      if (kind == json_character .or. time) call move_alloc(s, a%texts(a%n)%s)
   end subroutine read_scalar

   !> True if the next characters are the literal `w`.
   pure logical function literal(text, c, w)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(in) :: c  !! the parser
      character(*), intent(in) :: w  !! `true`, `false` or `null`
      literal = .false.
      if (c%pos + len(w) - 1 > len(text)) return
      literal = text(c%pos:c%pos + len(w) - 1) == w
   end function literal

   !> A JSON number: `-?(0|[1-9][0-9]*)(.[0-9]+)?([eE][+-]?[0-9]+)?`,
   !> correctly rounded (Fortran's READ). Beyond the range of double
   !> precision (READ gives an infinity): SPICE(NUMBEREXPECTED), as for
   !> text kernels (NPARSD).
   pure subroutine read_number(text, c, x)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(inout) :: c  !! the parser
      real(wp), intent(out) :: x  !! its value
      integer(ip) :: b, p
      integer :: ios
      x = 0.0_wp
      b = c%pos
      p = b
      if (text(p:p) == '-') p = p + 1
      if (text_at(p) == '0') then
         p = p + 1
      else if (digit(p)) then
         do while (digit(p))
            p = p + 1
         end do
      else
         call syntax(c, 'a number has no digits after "-"')
         return
      end if
      if (text_at(p) == '.') then
         p = p + 1
         if (.not. digit(p)) then
            call syntax(c, 'a number has no digits after its decimal point')
            return
         end if
         do while (digit(p))
            p = p + 1
         end do
      end if
      if (text_at(p) == 'e' .or. text_at(p) == 'E') then
         p = p + 1
         if (text_at(p) == '+' .or. text_at(p) == '-') p = p + 1
         if (.not. digit(p)) then
            call syntax(c, 'a number has no digits in its exponent')
            return
         end if
         do while (digit(p))
            p = p + 1
         end do
      end if
      read (text(b:p - 1), *, iostat=ios) x
      if (ios /= 0) then
         call syntax(c, 'the number "'//text(b:p - 1)//'" cannot be read')
         return
      end if
      if (abs(x) > huge(x)) then
         call fail(c, 'SPICE(NUMBEREXPECTED)', 'the number "'//text(b:p - 1)//'" is beyond the range '// &
                   'of double precision', c%line)
         return
      end if
      c%pos = p
   contains
      !> The character at q (a blank after the end of the text).
      pure character function text_at(q)
         integer(ip), intent(in) :: q  !! the position
         text_at = ' '
         if (q <= len(text)) text_at = text(q:q)
      end function text_at
      !> True if the character at q is a digit.
      pure logical function digit(q)
         integer(ip), intent(in) :: q  !! the position
         digit = text_at(q) >= '0' .and. text_at(q) <= '9'
      end function digit
   end subroutine read_number

   !> A JSON string (the parser is on its opening quote), with its escapes
   !> resolved (`\uXXXX`: UTF-8; not `\u0000`, nor a lone surrogate).
   pure subroutine read_string(text, c, s)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(inout) :: c  !! the parser
      character(:), allocatable, intent(out) :: s  !! the string
      character(:), allocatable :: buf
      integer(ip) :: p, e, n
      integer :: code, low
      ! its end, for the length of the buffer (escapes only shorten it)
      e = c%pos + 1
      do
         if (e > len(text)) then
            call syntax(c, 'a string has no closing quote')
            return
         end if
         if (text(e:e) == '"') exit
         if (text(e:e) == '\') e = e + 1
         e = e + 1
      end do
      allocate (character(e - c%pos - 1) :: buf)
      n = 0
      p = c%pos + 1
      do while (p < e)
         if (iachar(text(p:p)) < 32) then
            call syntax(c, 'a string has a control character (a line end?); use an escape, such as \n')
            return
         end if
         if (text(p:p) /= '\') then
            call put(buf, n, text(p:p))
            p = p + 1
            cycle
         end if
         p = p + 1
         select case (text(p:p))
         case ('"', '\', '/')
            call put(buf, n, text(p:p))
         case ('b')
            call put(buf, n, achar(8))
         case ('f')
            call put(buf, n, achar(12))
         case ('n')
            call put(buf, n, achar(10))
         case ('r')
            call put(buf, n, achar(13))
         case ('t')
            call put(buf, n, achar(9))
         case ('u')
            call hex4(text, c, p + 1, code)
            if (.not. c%ok) return
            if (code == 0) then
               call syntax(c, 'a string has a NUL character (\u0000)')
               return
            end if
            if (code >= 56320 .and. code <= 57343) then
               call syntax(c, 'a \u escape has an unpaired surrogate (a low surrogate first)')
               return
            end if
            p = p + 4
            ! a surrogate pair: one code point
            if (code >= 55296 .and. code <= 56319) then
               low = -1
               if (p + 6 <= e - 1) then
                  if (text(p + 1:p + 2) == '\u') call hex4(text, c, p + 3, low)
                  if (.not. c%ok) return
               end if
               if (low < 56320 .or. low > 57343) then
                  call syntax(c, 'a \u escape has an unpaired surrogate')
                  return
               end if
               code = 65536 + (code - 55296)*1024 + (low - 56320)
               p = p + 6
            end if
            call put_utf8(buf, n, code)
         case default
            call syntax(c, 'a string has an unknown escape "\'//text(p:p)//'"')
            return
         end select
         p = p + 1
      end do
      s = buf(1:n)
      c%pos = e + 1
   end subroutine read_string

   !> Append a character to buf(1:n).
   pure subroutine put(buf, n, ch)
      character(*), intent(inout) :: buf  !! the buffer
      integer(ip), intent(inout) :: n  !! its length used
      character, intent(in) :: ch  !! the character
      n = n + 1
      buf(n:n) = ch
   end subroutine put

   !> Append a code point to buf(1:n), encoded in UTF-8 (an escape takes 6
   !> or 12 characters of the text, more than its 1 to 4 bytes: the buffer
   !> has room).
   pure subroutine put_utf8(buf, n, cp)
      character(*), intent(inout) :: buf  !! the buffer
      integer(ip), intent(inout) :: n  !! its length used
      integer, intent(in) :: cp  !! the code point
      if (cp < 128) then
         call put(buf, n, achar(cp))
      else if (cp < 2048) then
         call put(buf, n, char(192 + cp/64))
         call put(buf, n, char(128 + mod(cp, 64)))
      else if (cp < 65536) then
         call put(buf, n, char(224 + cp/4096))
         call put(buf, n, char(128 + mod(cp/64, 64)))
         call put(buf, n, char(128 + mod(cp, 64)))
      else
         call put(buf, n, char(240 + cp/262144))
         call put(buf, n, char(128 + mod(cp/4096, 64)))
         call put(buf, n, char(128 + mod(cp/64, 64)))
         call put(buf, n, char(128 + mod(cp, 64)))
      end if
   end subroutine put_utf8

   !> The value of the four hexadecimal digits at q (a syntax error if
   !> they are not).
   pure subroutine hex4(text, c, q, value)
      character(*), intent(in) :: text  !! the text
      type(json_cursor), intent(inout) :: c  !! the parser
      integer(ip), intent(in) :: q  !! the position of the first digit
      integer, intent(out) :: value  !! the value
      integer :: ios
      value = 0
      if (q + 3 > len(text)) then
         call syntax(c, 'a \u escape has fewer than four hexadecimal digits')
         return
      end if
      if (verify(text(q:q + 3), '0123456789abcdefABCDEF') /= 0) then
         call syntax(c, 'a \u escape has fewer than four hexadecimal digits')
         return
      end if
      read (text(q:q + 3), '(z4)', iostat=ios) value
   end subroutine hex4

   !> Make room for one more value.
   pure subroutine grow_values(a)
      type(json_assignment), intent(inout) :: a  !! the assignment
      real(wp), allocatable :: x(:)
      logical, allocatable :: t(:)
      type(string), allocatable :: s(:)
      integer(ip) :: k, m
      if (a%n < size(a%numbers)) return
      m = 2*size(a%numbers)
      allocate (x(m), t(m), s(m))
      x(1:a%n) = a%numbers(1:a%n)
      t(1:a%n) = a%is_time(1:a%n)
      do k = 1, a%n
         if (allocated(a%texts(k)%s)) call move_alloc(a%texts(k)%s, s(k)%s)
      end do
      call move_alloc(x, a%numbers)
      call move_alloc(t, a%is_time)
      call move_alloc(s, a%texts)
   end subroutine grow_values

   !> Add an assignment to the list (moved: `a` is left empty).
   pure subroutine add_item(items, nitems, a)
      type(json_assignment), allocatable, intent(inout) :: items(:)  !! the list
      integer(ip), intent(inout) :: nitems  !! its length used
      type(json_assignment), intent(inout) :: a  !! the assignment
      type(json_assignment), allocatable :: tmp(:)
      integer(ip) :: k
      if (nitems == size(items)) then
         allocate (tmp(2*size(items)))
         do k = 1, nitems
            call move_item(items(k), tmp(k))
         end do
         call move_alloc(tmp, items)
      end if
      nitems = nitems + 1
      call move_item(a, items(nitems))
   end subroutine add_item

   !> Move an assignment (its allocations without copying).
   pure subroutine move_item(from, to)
      type(json_assignment), intent(inout) :: from  !! the assignment moved (left empty)
      type(json_assignment), intent(inout) :: to  !! the assignment replaced
      if (allocated(from%name)) call move_alloc(from%name, to%name)
      to%append = from%append
      to%kind = from%kind
      to%n = from%n
      to%line = from%line
      if (allocated(from%numbers)) call move_alloc(from%numbers, to%numbers)
      if (allocated(from%is_time)) call move_alloc(from%is_time, to%is_time)
      if (allocated(from%texts)) call move_alloc(from%texts, to%texts)
   end subroutine move_item

   !> A syntax error at the parser's line.
   pure subroutine syntax(c, what)
      type(json_cursor), intent(inout) :: c  !! the parser
      character(*), intent(in) :: what  !! what is wrong
      call fail(c, 'SPICE(JSONSYNTAXERROR)', what, c%line)
   end subroutine syntax

   !> Record the first error.
   pure subroutine fail(c, code, what, where)
      type(json_cursor), intent(inout) :: c  !! the parser
      character(*), intent(in) :: code  !! the short message
      character(*), intent(in) :: what  !! the long message
      integer(ip), intent(in) :: where  !! the line
      if (.not. c%ok) return
      c%ok = .false.
      c%short = code
      c%long = what
      c%error_line = where
   end subroutine fail

end module pepper_json
