!> The default SPICE instance and context of the legacy SPICELIB interface.
!>
!> The legacy external procedures (SPICELIB's names and arguments) have no
!> context argument; they call the module procedures with this context.
!> The legacy error routines (SIGERR, FAILED, ERRACT, ...; in
!> pepper_legacy_errsys) act on this context's error state, which starts with
!> SPICELIB's settings (action DEFAULT, output to the screen). Like SPICELIB
!> itself, the legacy interface is not thread-safe.
!> This is the only global state in PEPPER, and the module API never uses
!> it.
module pepper_legacy_context

   use pepper_instance, only: pepper_spice, pepper_context

   implicit none

   private

   public :: legacy_context

   type(pepper_spice), target, save :: default_spice  !! the default instance
   type(pepper_context), target, save :: default_context  !! its legacy context
   logical, save :: initialized = .false.  !! default_context has been created

contains

   !> The context of the legacy interface (created on the first call).
   function legacy_context() result(ctx)
      type(pepper_context), pointer :: ctx  !! the default context
      character(255) :: setting
      if (.not. initialized) then
         default_context = default_spice%new_context()
         ! SPICELIB's initial error settings
         setting = 'DEFAULT'
         call default_context%erract('SET', setting)
         setting = 'SCREEN'
         call default_context%errdev('SET', setting)
         initialized = .true.
      end if
      ctx => default_context
   end function legacy_context

end module pepper_legacy_context
