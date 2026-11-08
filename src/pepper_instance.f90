!> The SPICE instance and the per-thread context.
!>
!> All PEPPER state belongs to an instance, type(pepper_spice): the loaded
!> kernels, the kernel pool, ... (added in the coming phases). Each thread
!> that uses an instance works through its own type(pepper_context),
!> created with spice%new_context(). A context holds the per-call mutable
!> state, starting with the error status and messages. A context must not
!> be used by two threads at the same time, and the instance must outlive
!> its contexts:
!>
!>     type(pepper_spice), target :: spice
!>     type(pepper_context) :: ctx
!>     ctx = spice%new_context()
!>
!> Errors work as in SPICELIB, but per context and without a traceback: a
!> routine that fails signals an error (ctx%sigerr, with a short message
!> and an optional long message built with ctx%setmsg/errch/errdp/errint),
!> and callers check ctx%failed(). The default error action of a context is
!> RETURN, and nothing is printed: the caller gets the messages with
!> ctx%getmsg and clears the error with ctx%reset(). The actions REPORT,
!> ABORT and DEFAULT print the messages to error_unit (ABORT and DEFAULT
!> then stop the program).
!>
!> A legacy context (only used by the legacy SPICELIB interface in
!> src/legacy) forwards all error handling to SPICELIB's global error
!> routines (SIGERR, FAILED, ...), so that the legacy interface behaves
!> exactly as SPICELIB does, including ERRACT, ERRDEV and ERRPRT.
module pepper_instance

   use, intrinsic :: iso_fortran_env, only: error_unit
   use pepper_kinds, only: ip, wp
   use pepper_errors, only: expln

   implicit none

   private

   public :: pepper_spice, pepper_context

   integer, parameter, public :: short_message_length = 25    !! SPICELIB's short message length
   integer, parameter, public :: long_message_length = 1840   !! SPICELIB's long message length
   integer, parameter, public :: explanation_length = 80      !! length of an explanation (EXPLN)

   ! Error actions (ERRACT).
   integer, parameter :: action_abort = 1, action_report = 2, action_return = 3, &
                         action_ignore = 4, action_default = 5
   character(7), parameter :: action_names(5) = &
      [character(7) :: 'ABORT', 'REPORT', 'RETURN', 'IGNORE', 'DEFAULT']

   !> A SPICE instance: the state shared by all its contexts.
   type :: pepper_spice
      private
      !> Incremented by every change of the loaded kernels or the kernel
      !> pool; caches in contexts are valid while it is unchanged.
      integer(ip) :: generation = 0
   contains
      procedure, non_overridable :: new_context
   end type pepper_spice

   !> The per-thread state for using an instance.
   type :: pepper_context
      private
      type(pepper_spice), pointer :: spice => null()  !! the instance
      logical :: legacy = .false.  !! error handling by SPICELIB's global routines
      ! Error status and messages (native contexts).
      logical :: failure = .false.  !! an error was signaled (and not reset)
      logical :: messages_allowed = .true.  !! the long message may be changed
      integer :: action = action_return  !! error action (index in action_names)
      character(short_message_length) :: short = ' '  !! short error message
      character(long_message_length) :: long = ' '  !! long error message
   contains
      procedure, non_overridable :: instance
      procedure, non_overridable :: is_legacy
      procedure, non_overridable :: failed => ctx_failed
      procedure, non_overridable :: sigerr => ctx_sigerr
      procedure, non_overridable :: setmsg => ctx_setmsg
      procedure, non_overridable :: errch => ctx_errch
      procedure, non_overridable :: errdp => ctx_errdp
      procedure, non_overridable :: errint => ctx_errint
      procedure, non_overridable :: reset => ctx_reset
      procedure, non_overridable :: getmsg => ctx_getmsg
      procedure, non_overridable :: erract => ctx_erract
   end type pepper_context

contains

   !> A new context for this instance. With legacy = .true., its error
   !> handling is SPICELIB's global one (for the legacy interface only).
   function new_context(this, legacy) result(ctx)
      class(pepper_spice), target, intent(in) :: this  !! the instance (must have the target attribute)
      logical, intent(in), optional :: legacy  !! .true. for the legacy interface's context (default .false.)
      type(pepper_context) :: ctx
      ctx%spice => this
      if (present(legacy)) ctx%legacy = legacy
   end function new_context

   !> The instance this context belongs to.
   function instance(this) result(spice)
      class(pepper_context), intent(in) :: this  !! the context
      type(pepper_spice), pointer :: spice
      spice => this%spice
   end function instance

   !> True for a legacy context (error handling by SPICELIB's global routines).
   logical function is_legacy(this)
      class(pepper_context), intent(in) :: this  !! the context
      is_legacy = this%legacy
   end function is_legacy

   !> True if an error has been signaled and not reset.
   logical function ctx_failed(this)
      class(pepper_context), intent(in) :: this  !! the context
      interface
         logical function failed()   ! SPICELIB
         end function failed
      end interface
      if (this%legacy) then
         ctx_failed = failed()
      else
         ctx_failed = this%failure
      end if
   end function ctx_failed

   !> Signal an error with the short message msg (e.g. 'SPICE(BADINPUT)').
   !> Under the RETURN action only the first error is kept: once the
   !> context has failed, further errors and message changes are ignored
   !> until reset().
   subroutine ctx_sigerr(this, msg)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: msg  !! short error message, e.g. 'SPICE(BADINPUT)' (at most 25 characters)
      interface
         subroutine sigerr(msg)   ! SPICELIB
            character(*), intent(in) :: msg
         end subroutine sigerr
      end interface
      if (this%legacy) then
         call sigerr(msg)
         return
      end if
      if (this%action == action_ignore) return
      if (this%action /= action_return .or. .not. this%failure) then
         this%failure = .true.
         this%short = msg
         if (this%action /= action_return) call report(this)
         this%messages_allowed = this%action /= action_return
      else
         this%messages_allowed = .false.
      end if
      if (this%action == action_abort .or. this%action == action_default) then
         error stop 'PEPPER: error (abort)'
      end if
   end subroutine ctx_sigerr

   !> Set the long error message (before calling sigerr). Markers in it
   !> (e.g. '#') are replaced by errch, errdp and errint.
   subroutine ctx_setmsg(this, msg)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: msg  !! long error message (at most 1840 characters), with markers to replace
      interface
         subroutine setmsg(msg)   ! SPICELIB
            character(*), intent(in) :: msg
         end subroutine setmsg
      end interface
      if (this%legacy) then
         call setmsg(msg)
      else if (this%messages_allowed) then
         this%long = msg
      end if
   end subroutine ctx_setmsg

   !> Replace the first occurrence of marker in the long message with the
   !> string (without its leading and trailing blanks).
   subroutine ctx_errch(this, marker, string)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      character(*), intent(in) :: string  !! text that replaces the marker
      interface
         subroutine errch(marker, string)   ! SPICELIB
            character(*), intent(in) :: marker, string
         end subroutine errch
      end interface
      if (this%legacy) then
         call errch(marker, string)
      else
         call replace_marker(this, marker, trim(adjustl(string)))
      end if
   end subroutine ctx_errch

   !> Replace the first occurrence of marker in the long message with the
   !> number (14 significant digits).
   subroutine ctx_errdp(this, marker, number)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      real(wp), intent(in) :: number  !! number that replaces the marker
      character(32) :: text
      interface
         subroutine errdp(marker, number)   ! SPICELIB
            import :: wp
            character(*), intent(in) :: marker
            real(wp), intent(in) :: number
         end subroutine errdp
      end interface
      if (this%legacy) then
         call errdp(marker, number)
      else
         write (text, '(es24.13e3)') number
         call replace_marker(this, marker, trim(adjustl(text)))
      end if
   end subroutine ctx_errdp

   !> Replace the first occurrence of marker in the long message with the
   !> integer.
   subroutine ctx_errint(this, marker, number)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      integer(ip), intent(in) :: number  !! integer that replaces the marker
      character(16) :: text
      interface
         subroutine errint(marker, number)   ! SPICELIB
            import :: ip
            character(*), intent(in) :: marker
            integer(ip), intent(in) :: number
         end subroutine errint
      end interface
      if (this%legacy) then
         call errint(marker, number)
      else
         write (text, '(i0)') number
         call replace_marker(this, marker, trim(text))
      end if
   end subroutine ctx_errint

   !> Clear the error status and the messages.
   subroutine ctx_reset(this)
      class(pepper_context), intent(inout) :: this  !! the context
      interface
         subroutine reset()   ! SPICELIB
         end subroutine reset
      end interface
      if (this%legacy) then
         call reset()
      else
         this%failure = .false.
         this%short = ' '
         this%long = ' '
         this%messages_allowed = .true.
      end if
   end subroutine ctx_reset

   !> Get an error message: option 'SHORT', 'LONG' or 'EXPLAIN' (the
   !> explanation of the short message), case-insensitive.
   subroutine ctx_getmsg(this, option, msg)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: option  !! 'SHORT', 'LONG' or 'EXPLAIN'
      character(*), intent(out) :: msg  !! the message
      interface
         subroutine getmsg(option, msg)   ! SPICELIB
            character(*), intent(in) :: option
            character(*), intent(out) :: msg
         end subroutine getmsg
      end interface
      if (this%legacy) then
         call getmsg(option, msg)
         return
      end if
      select case (upper(adjustl(option)))
      case ('SHORT')
         msg = this%short
      case ('LONG')
         msg = this%long
      case ('EXPLAIN')
         call expln(this%short, msg)
      case default
         call this%setmsg('Illegal option: #.')
         call this%errch('#', option)
         call this%sigerr('SPICE(INVALIDMSGTYPE)')
      end select
   end subroutine ctx_getmsg

   !> Get (op = 'GET') or set (op = 'SET') the error action: 'ABORT',
   !> 'REPORT', 'RETURN', 'IGNORE' or 'DEFAULT'.
   subroutine ctx_erract(this, op, action)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: op  !! 'GET' or 'SET'
      character(*), intent(inout) :: action  !! the action (output for 'GET', input for 'SET')
      integer :: k
      interface
         subroutine erract(op, action)   ! SPICELIB
            character(*), intent(in) :: op
            character(*), intent(inout) :: action
         end subroutine erract
      end interface
      if (this%legacy) then
         call erract(op, action)
         return
      end if
      select case (upper(adjustl(op)))
      case ('GET')
         action = action_names(this%action)
      case ('SET')
         k = findloc(action_names, upper(adjustl(action)), dim=1)
         if (k > 0) then
            this%action = k
         else
            call this%setmsg('Invalid action: #.')
            call this%errch('#', action)
            call this%sigerr('SPICE(INVALIDACTION)')
         end if
      case default
         call this%setmsg('Invalid operation: #.')
         call this%errch('#', op)
         call this%sigerr('SPICE(INVALIDOPERATION)')
      end select
   end subroutine ctx_erract

   ! ------------------------------------------------------------------------
   ! Helpers

   !> Replace the first occurrence of marker in the long message (if
   !> messages may be changed; see sigerr).
   subroutine replace_marker(this, marker, string)
      type(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker, e.g. '#' (leading and trailing blanks ignored)
      character(*), intent(in) :: string  !! replacement text (used as is)
      character(:), allocatable :: mark
      integer :: k
      if (.not. this%messages_allowed) return
      mark = trim(adjustl(marker))
      if (len(mark) == 0) return
      k = index(this%long, mark)
      if (k == 0) return
      this%long = this%long(:k - 1) // string // this%long(k + len(mark):)
   end subroutine replace_marker

   !> Print the error messages of a native context to error_unit.
   subroutine report(this)
      type(pepper_context), intent(in) :: this  !! the context
      character(explanation_length) :: explain
      call expln(this%short, explain)
      write (error_unit, '(a)') '', '================================================', &
         'PEPPER error: ' // trim(this%short)
      if (explain /= ' ') write (error_unit, '(a)') trim(explain)
      if (this%long /= ' ') write (error_unit, '(a)') '', trim(this%long)
      write (error_unit, '(a)') '================================================'
   end subroutine report

   !> Uppercase version of a string (ASCII letters).
   pure function upper(s) result(u)
      character(*), intent(in) :: s  !! the string
      character(len(s)) :: u
      integer :: i
      u = s
      do i = 1, len(s)
         if (u(i:i) >= 'a' .and. u(i:i) <= 'z') u(i:i) = achar(iachar(u(i:i)) - 32)
      end do
   end function upper

end module pepper_instance
