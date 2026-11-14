!> SPICELIB's interface to PEPPER: SPICELIB's routines, with their names and
!> arguments, on a default instance and context. A program written for
!> SPICELIB uses this module (`use pepper_legacy`) instead of declaring
!> SPICELIB's functions itself; NAIF's test suite (tspice) runs through it.
!>
!> Each routine calls the module API (pepper_instance_module) with the default
!> context, [[legacy_context]], or for the routines that change the kernels
!> (FURNSH, LDPOOL, BODDEF, ...) the default instance, [[legacy_spice]],
!> whose errors are signaled in the same context. The error routines (SIGERR,
!> FAILED, ERRACT, ...) act on that context, which starts with SPICELIB's
!> settings (action DEFAULT, output to the screen). Like SPICELIB itself, this
!> interface is not thread-safe: threads use the module API, a context each.
!>
!> The routines are in one module per subsystem (pepper_legacy_<subsystem>),
!> the default instance in pepper_legacy_context and the callbacks in
!> pepper_legacy_callbacks (tools/split_legacy_module.py split the merged
!> module of tools/make_legacy_module.py: one module took gigabytes of RAM
!> to compile with ifx); this module uses them all.
module pepper_legacy

   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_legacy_callbacks, only: legacy_zzdsk_compare, legacy_zzspk_state, &
      legacy_zzsfx_near, legacy_zzsfx_maxrad, legacy_zzsfx_rayx, legacy_zztan_cond, &
      legacy_gf_step, legacy_gf_refine, legacy_gf_rep_init, legacy_gf_rep_update, &
      legacy_gf_rep_final, legacy_gf_bail, legacy_gf_cond, legacy_gf_ref, legacy_gf_scalar, &
      legacy_gf_boolean, legacy_gf_decr, legacy_gf_decr_x
   use pepper_legacy_array_ops
   use pepper_legacy_arrays
   use pepper_legacy_bodies
   use pepper_legacy_cells
   use pepper_legacy_ck_api
   use pepper_legacy_constants
   use pepper_legacy_daf
   use pepper_legacy_das
   use pepper_legacy_dsk_files
   use pepper_legacy_dsk_write
   use pepper_legacy_dynamic
   use pepper_legacy_ek
   use pepper_legacy_ek_storage
   use pepper_legacy_errors
   use pepper_legacy_errsys
   use pepper_legacy_files
   use pepper_legacy_frames
   use pepper_legacy_geometry
   use pepper_legacy_gf
   use pepper_legacy_keeper
   use pepper_legacy_math
   use pepper_legacy_numerics
   use pepper_legacy_orientation
   use pepper_legacy_pck_api
   use pepper_legacy_pool
   use pepper_legacy_records
   use pepper_legacy_rotations
   use pepper_legacy_sclk
   use pepper_legacy_sclk_api
   use pepper_legacy_segments
   use pepper_legacy_spk_api
   use pepper_legacy_spk_callbacks
   use pepper_legacy_strings
   use pepper_legacy_symbols
   use pepper_legacy_text
   use pepper_legacy_time
   use pepper_legacy_time_api
   use pepper_legacy_traceback
   use pepper_legacy_transforms
   use pepper_legacy_writers

   implicit none

   public

end module pepper_legacy
