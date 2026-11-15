!> The legacy interface's routines of the subsystem errors (1 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_errors

   implicit none

   private
   public :: expln

contains

       !> Return the explanation of a short error message.
       !>
       !> Legacy interface: calls expln of module pepper_errors.
       subroutine expln ( msg, expl )
      use pepper_errors, only: pepper_expln => expln
      implicit none
      character(*) :: msg  !! A short error message.
      character(*) :: expl  !! The explanation of the short error message.
      ! PEPPER: msg is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_expln ( (msg), expl )
      end subroutine expln

end module pepper_legacy_errors
