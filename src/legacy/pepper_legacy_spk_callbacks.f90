! Legacy interface: ZZSPKFAO, ZZSPKFAP, ZZSPKFAT, ZZSPKFLT, ZZSPKFZO and
! ZZSPKFZT, whose state routine argument takes the context in the module
! API; the caller's routine (SPICELIB's form) is bridged through
! pepper_legacy_callbacks.

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkflt with the legacy context
      !> (and TRGSUB through pepper_legacy_callbacks).
      subroutine zzspkflt ( trgsub, et, ref, abcorr, stobs, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkflt => zzspkflt
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_callbacks, only: legacy_zzspk_state, current_zzspk_state, bridge_zzspk_state
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      procedure(legacy_zzspk_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: stobs  ( 6 )  !! State of the observer relative to the SSB.
      real(wp) :: starg  ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzspk_state), pointer :: outer
      ctx => legacy_context()
      outer => current_zzspk_state
      current_zzspk_state => trgsub
      call pepper_zzspkflt ( ctx, bridge_zzspk_state, et, ref, abcorr, stobs, starg, lt, dlt )
      current_zzspk_state => outer
      end subroutine zzspkflt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfao with the legacy context
      !> (and OBSSUB through pepper_legacy_callbacks).
      subroutine zzspkfao ( targ, et, ref, abcorr, obssub, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkfao => zzspkfao
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_callbacks, only: legacy_zzspk_state, current_zzspk_state, bridge_zzspk_state
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      procedure(legacy_zzspk_state) :: obssub  !! routine returning the observer's state (relative to the SSB)
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzspk_state), pointer :: outer
      ctx => legacy_context()
      outer => current_zzspk_state
      current_zzspk_state => obssub
      call pepper_zzspkfao ( ctx, targ, et, ref, abcorr, bridge_zzspk_state, starg, lt, dlt )
      current_zzspk_state => outer
      end subroutine zzspkfao

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfap with the legacy context
      !> (and TRGSUB through pepper_legacy_callbacks).
      subroutine zzspkfap ( trgsub, et, ref, abcorr, stobs, accobs, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkfap => zzspkfap
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_callbacks, only: legacy_zzspk_state, current_zzspk_state, bridge_zzspk_state
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      procedure(legacy_zzspk_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: stobs  ( 6 )  !! State of the observer relative to the SSB.
      real(wp) :: accobs ( 3 )  !! Acceleration of the observer relative to the SSB.
      real(wp) :: starg  ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzspk_state), pointer :: outer
      ctx => legacy_context()
      outer => current_zzspk_state
      current_zzspk_state => trgsub
      call pepper_zzspkfap ( ctx, bridge_zzspk_state, et, ref, abcorr, stobs, accobs, starg, lt, dlt )
      current_zzspk_state => outer
      end subroutine zzspkfap

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfat with the legacy context
      !> (and TRGSUB through pepper_legacy_callbacks).
      subroutine zzspkfat ( trgsub, et, ref, abcorr, obs, starg, lt, dlt )
      use pepper_instance, only: pepper_context, pepper_zzspkfat => zzspkfat
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_callbacks, only: legacy_zzspk_state, current_zzspk_state, bridge_zzspk_state
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      procedure(legacy_zzspk_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obs  !! Observer.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzspk_state), pointer :: outer
      ctx => legacy_context()
      outer => current_zzspk_state
      current_zzspk_state => trgsub
      call pepper_zzspkfat ( ctx, bridge_zzspk_state, et, ref, abcorr, obs, starg, lt, dlt )
      current_zzspk_state => outer
      end subroutine zzspkfat

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfzo with the legacy context
      !> (and OBSSUB through pepper_legacy_callbacks).
      subroutine zzspkfzo ( targ, et, ref, abcorr, obssub, starg, lt )
      use pepper_instance, only: pepper_context, pepper_zzspkfzo => zzspkfzo
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_callbacks, only: legacy_zzspk_state, current_zzspk_state, bridge_zzspk_state
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      implicit none
      procedure(legacy_zzspk_state) :: obssub  !! routine returning the observer's state (relative to the SSB)
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output state vector.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzspk_state), pointer :: outer
      ctx => legacy_context()
      outer => current_zzspk_state
      current_zzspk_state => obssub
      call pepper_zzspkfzo ( ctx, targ, et, ref, abcorr, bridge_zzspk_state, starg, lt )
      current_zzspk_state => outer
      end subroutine zzspkfzo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzspkfzt with the legacy context
      !> (and TRGSUB through pepper_legacy_callbacks).
      subroutine zzspkfzt ( trgsub, et, ref, abcorr, obs, starg, lt )
      use pepper_instance, only: pepper_context, pepper_zzspkfzt => zzspkfzt
      use pepper_legacy_context, only: legacy_context
      use pepper_legacy_callbacks, only: legacy_zzspk_state, current_zzspk_state, bridge_zzspk_state
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      implicit none
      procedure(legacy_zzspk_state) :: trgsub  !! routine returning the target's state (relative to the SSB)
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output state vector.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obs  !! Observing body.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzspk_state), pointer :: outer
      ctx => legacy_context()
      outer => current_zzspk_state
      current_zzspk_state => trgsub
      call pepper_zzspkfzt ( ctx, bridge_zzspk_state, et, ref, abcorr, obs, starg, lt )
      current_zzspk_state => outer
      end subroutine zzspkfzt
