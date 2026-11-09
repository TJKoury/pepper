! PEPPER: the legacy interface of the SPK routines with a state routine
! argument (ZZSPKFAO, ZZSPKFAP, ZZSPKFAT, ZZSPKFLT, ZZSPKFZO, ZZSPKFZT).
! Their module procedures take a state routine with the context as first
! argument (zzspk_state); a legacy caller passes one without it (SPICELIB's
! interface). The wrapper keeps the caller's routine in a procedure pointer
! and passes the module procedure a bridge that calls it. (The legacy layer
! has one context and is not for concurrent use, so a module variable is
! fine here.)
module pepper_legacy_spk_state

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_context

   implicit none

   private
   public :: legacy_state, current_state, bridge_state

   abstract interface
      !> A state routine of SPICELIB's interface (e.g. ZZCVXSTA).
      subroutine legacy_state ( et, ref, center, state )
         import :: ip, wp
         real(wp) :: et  !! epoch (TDB seconds past J2000)
         character(*) :: ref  !! inertial reference frame of the state
         integer(ip) :: center  !! ID code of the center of motion
         real(wp) :: state ( 6 )  !! state of the object
      end subroutine legacy_state
   end interface

   !> The legacy caller's state routine (during a call of a wrapper below).
   procedure(legacy_state), pointer :: current_state => null()

contains

   !> A zzspk_state routine that calls the legacy caller's state routine.
   subroutine bridge_state ( ctx, et, ref, center, state )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: et  !! epoch (TDB seconds past J2000)
      character(*), intent(in) :: ref  !! inertial reference frame of the state
      integer(ip), intent(out) :: center  !! ID code of the center of motion
      real(wp), intent(out) :: state ( 6 )  !! state of the object
      real(wp) :: t
      t = et
      call current_state ( t, ref, center, state )
   end subroutine bridge_state

end module pepper_legacy_spk_state

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkflt with the legacy context
      !> (and TRGSUB through pepper_legacy_spk_state).
      subroutine zzspkflt ( trgsub, et, ref, abcorr, stobs, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkflt => zzspkflt
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_spk_state, only: legacy_state, current_state, bridge_state
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      include 'zzabcorr.inc'
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: stobs  ( 6 )  !! State of the observer relative to the SSB.
      real(wp) :: starg  ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_state), pointer :: outer
      ctx => legacy_context()
      outer => current_state
      current_state => trgsub
      call pepper_zzspkflt ( ctx, bridge_state, et, ref, abcorr, stobs, starg, lt, dlt )
      current_state => outer
      end subroutine zzspkflt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfao with the legacy context
      !> (and OBSSUB through pepper_legacy_spk_state).
      subroutine zzspkfao ( targ, et, ref, abcorr, obssub, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkfao => zzspkfao
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_spk_state, only: legacy_state, current_state, bridge_state
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_state) :: obssub  !! routine returning the observer's state (relative to the SSB)
      include 'zzabcorr.inc'
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_state), pointer :: outer
      ctx => legacy_context()
      outer => current_state
      current_state => obssub
      call pepper_zzspkfao ( ctx, targ, et, ref, abcorr, bridge_state, starg, lt, dlt )
      current_state => outer
      end subroutine zzspkfao

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfap with the legacy context
      !> (and TRGSUB through pepper_legacy_spk_state).
      subroutine zzspkfap ( trgsub, et, ref, abcorr, stobs, accobs, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkfap => zzspkfap
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_spk_state, only: legacy_state, current_state, bridge_state
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      include 'zzabcorr.inc'
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: stobs  ( 6 )  !! State of the observer relative to the SSB.
      real(wp) :: accobs ( 3 )  !! Acceleration of the observer relative to the SSB.
      real(wp) :: starg  ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_state), pointer :: outer
      ctx => legacy_context()
      outer => current_state
      current_state => trgsub
      call pepper_zzspkfap ( ctx, bridge_state, et, ref, abcorr, stobs, accobs, starg, lt, dlt )
      current_state => outer
      end subroutine zzspkfap

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfat with the legacy context
      !> (and TRGSUB through pepper_legacy_spk_state).
      subroutine zzspkfat ( trgsub, et, ref, abcorr, obs, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkfat => zzspkfat
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_spk_state, only: legacy_state, current_state, bridge_state
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      include 'zzabcorr.inc'
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obs  !! Observer.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_state), pointer :: outer
      ctx => legacy_context()
      outer => current_state
      current_state => trgsub
      call pepper_zzspkfat ( ctx, bridge_state, et, ref, abcorr, obs, starg, lt, dlt )
      current_state => outer
      end subroutine zzspkfat

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfzo with the legacy context
      !> (and OBSSUB through pepper_legacy_spk_state).
      subroutine zzspkfzo ( targ, et, ref, abcorr, obssub, starg, lt )
      use pepper_instance, only: pepper_context, pepper_zzspkfzo => zzspkfzo
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_spk_state, only: legacy_state, current_state, bridge_state
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_state) :: obssub  !! routine returning the observer's state (relative to the SSB)
      include              'frmtyp.inc'
      include              'zzabcorr.inc'
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output state vector.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      procedure(legacy_state), pointer :: outer
      ctx => legacy_context()
      outer => current_state
      current_state => obssub
      call pepper_zzspkfzo ( ctx, targ, et, ref, abcorr, bridge_state, starg, lt )
      current_state => outer
      end subroutine zzspkfzo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfzt with the legacy context
      !> (and TRGSUB through pepper_legacy_spk_state).
      subroutine zzspkfzt ( trgsub, et, ref, abcorr, obs, starg, lt )
      use pepper_instance, only: pepper_context, pepper_zzspkfzt => zzspkfzt
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_spk_state, only: legacy_state, current_state, bridge_state
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      include              'frmtyp.inc'
      include              'zzabcorr.inc'
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output state vector.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obs  !! Observing body.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      procedure(legacy_state), pointer :: outer
      ctx => legacy_context()
      outer => current_state
      current_state => trgsub
      call pepper_zzspkfzt ( ctx, bridge_state, et, ref, abcorr, obs, starg, lt )
      current_state => outer
      end subroutine zzspkfzt
