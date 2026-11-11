!> The default SPICE instance and context of the legacy SPICELIB interface.
!>
!> The legacy external procedures (SPICELIB's names and arguments) have no
!> context argument; they call the module procedures with this context, the
!> default instance's own context, or the methods of the instance for the
!> procedures that change the kernels (FURNSH, LDPOOL, BODDEF, ...), which
!> signal their errors in the same context.
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

   public :: legacy_context, legacy_spice

   type(pepper_spice), target, save :: default_spice  !! the default instance
   logical, save :: initialized = .false.  !! the default instance's context has its legacy settings

contains

   !> The context of the legacy interface (created on the first call).
   function legacy_context() result(ctx)
      type(pepper_context), pointer :: ctx  !! the default context
      character(255) :: setting
      if (.not. initialized) then
         ctx => default_spice%context()
         ! SPICELIB's initial error settings
         setting = 'DEFAULT'
         call ctx%erract('SET', setting)
         setting = 'SCREEN'
         call ctx%errdev('SET', setting)
         ! PEPPER_IN_MEMORY=1 (or YES) in the environment: FURNSH reads the
         ! binary kernels whole into memory (to run NAIF's test suite that
         ! way; programs call PEPPER_SET_IN_MEMORY instead)
         call get_environment_variable('PEPPER_IN_MEMORY', setting)
         if (setting == '1' .or. setting == 'YES' .or. setting == 'yes') &
            call default_spice%set_in_memory(.true.)
         initialized = .true.
      end if
      ctx => default_spice%context()
   end function legacy_context

   !> The default instance, for the legacy procedures that change the
   !> kernels (its context, legacy_context, has the legacy settings first).
   function legacy_spice() result(spice)
      type(pepper_spice), pointer :: spice  !! the default instance
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      spice => default_spice
   end function legacy_spice

end module pepper_legacy_context
