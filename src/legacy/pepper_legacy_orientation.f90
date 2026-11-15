!> The legacy interface's routines of the subsystem orientation (15 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_orientation

   use pepper_instance_module, only: pepper_context, pepper_zzdynfid => zzdynfid, &
      pepper_zznamfrm => zznamfrm, pepper_zznofcon => zznofcon, pepper_zzswffet => zzswffet, &
      pepper_zzteme => zzteme, pepper_zzswfxfm => zzswfxfm
   use pepper_legacy_context, only: legacy_context, legacy_spice

   implicit none

   private
   public :: sctype, tisbod, tkfram, zzdynfid, zznamfrm, zznofcon, zzswffet, zzteme, sce2c, &
      spke10, tipbod, ckfrot, ckfxfm, spkpvn, zzswfxfm

contains

      !> Return the spacecraft clock type for a specified spacecraft.
      !>
      !> Legacy interface: calls ctx%sctype with the legacy context.
      integer(ip) function sctype ( sc )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: sc  !! NAIF spacecraft ID code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sctype = ctx%sctype ( sc )
      end function sctype

      !> Return a 6x6 matrix that transforms states in inertial
      !> coordinates to states in body-equator-and-prime-meridian
      !> coordinates.
      !>
      !> Legacy interface: calls ctx%tisbod with the legacy context.
      subroutine tisbod ( ref, body, et, tsipm )
      use pepper_kinds, only: ip, wp
      use pepper_inc_errhnd
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*) :: ref  !! ID of inertial reference frame to transform from
      integer(ip) :: body  !! ID code of body
      real(wp) :: et  !! Epoch of transformation
      real(wp) :: tsipm   ( 6,6 )  !! Transformation (state), inertial to prime meridian
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tisbod ( ref, body, et, tsipm )
      end subroutine tisbod

      !> Find the position rotation matrix from a Text Kernel (TK) frame
      !> with the specified frame class ID to its base frame.
      !>
      !> Legacy interface: calls ctx%tkfram with the legacy context.
      subroutine tkfram ( frcode, rot, frame, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: frcode  !! Frame class ID of a TK frame.
      real(wp) :: rot   ( 3, 3 )  !! Rotation matrix from TK frame to frame FRAME.
      integer(ip) :: frame  !! Frame ID of the base reference.
      logical :: found  !! .TRUE. if the rotation could be determined.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tkfram ( frcode, rot, frame, found )
      end subroutine tkfram

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdynfid with the legacy context.
      subroutine zzdynfid ( frname, frcode, item, idcode )
      use pepper_kinds, only: ip
      use pepper_inc_zzdyn
      implicit none
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      character(*) :: item  !! Item associated with frame definition.
      integer(ip) :: idcode  !! Output kernel variable.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdynfid ( ctx, frname, frcode, item, idcode )
      end subroutine zzdynfid

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zznamfrm with the legacy context.
      subroutine zznamfrm ( usrctr, savnam, savcde, frname, frcode )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: usrctr    ( ctrsiz )  !! POOL state counter saved by the caller.
      character(*) :: savnam  !! Frame name saved by the caller.
      integer(ip) :: savcde  !! Frame ID code saved by the caller.
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: frname is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zznamfrm ( ctx, usrctr, savnam, savcde, (frname), frcode )
      end subroutine zznamfrm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zznofcon with the legacy context.
      subroutine zznofcon ( et, frame1, endp1, frame2, endp2, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      implicit none
      real(wp) :: et  !! Epoch.
      integer(ip) :: frame1  !! "From" frame ID code.
      integer(ip) :: endp1  !! "From" path endpoint frame ID code.
      integer(ip) :: frame2  !! "To" frame ID code.
      integer(ip) :: endp2  !! "To" path endpoint frame ID code.
      character(*) :: errmsg  !! Long error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zznofcon ( ctx, et, frame1, endp1, frame2, endp2, errmsg )
      end subroutine zznofcon

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzswffet with the legacy context.
      subroutine zzswffet ( framid, hdfram, frpool, fidlst, basbeg, &
                            free,   bascnt, usetim, binary, clsses, &
                            clsids, baslst, starts, stops,  framat )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzswtchf
      implicit none
      integer(ip) :: framid  !! The frame ID of a switch frame.
      integer(ip) :: hdfram ( * )  !! Array of hash collision list head nodes.
      integer(ip) :: frpool ( lbsngl : * )  !! Hash collision list pool.
      integer(ip) :: fidlst ( * )  !! Frame IDs corresponding to list nodes.
      integer(ip) :: basbeg ( * )  !! Start indices of base lists of switch frames.
      integer(ip) :: free  !! First free index in base frame arrays.
      integer(ip) :: bascnt ( * )  !! Counts of base frames for each switch frame.
      logical :: usetim ( * )  !! Logical flags indicating presence of time bounds.
      logical :: binary ( * )  !! Logical flags indicating binary search usage.
      integer(ip) :: clsses ( * )  !! Classes of base frames.
      integer(ip) :: clsids ( * )  !! Frame class IDs of base frames.
      integer(ip) :: baslst ( * )  !! Array containing lists of base frame IDs.
      real(wp) :: starts ( * )  !! Lists of interval start times.
      real(wp) :: stops ( * )  !! Lists of interval stop times.
      integer(ip) :: framat  !! Index in FIDLST of input switch frame ID.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzswffet ( ctx, framid, hdfram, frpool, fidlst, basbeg, free, bascnt, usetim, binary, clsses, clsids, baslst, starts, stops, framat )
      end subroutine zzswffet

      !> J2000 to TEME, and TEME to J2000, probably.
      !>
      !> Legacy interface: calls zz routine zzteme with the legacy context.
      subroutine zzteme( et, j2tm, tm2j )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: et  !! Epoch of the state transformation operator.
      real(wp) :: j2tm    ( 6, 6 )  !! The state transformation operator J2000 to TEME.
      real(wp) :: tm2j    ( 6, 6 )  !! The state transformation operator TEME to J2000.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzteme ( ctx, et, j2tm, tm2j )
      end subroutine zzteme

      !> Convert ephemeris seconds past J2000 (ET) to continuous encoded
      !> spacecraft clock (`ticks').  Non-integral tick values may be
      !> returned.
      !>
      !> Legacy interface: calls ctx%sce2c with the legacy context.
      subroutine sce2c ( sc, et, sclkdp )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: sc  !! NAIF spacecraft ID code.
      real(wp) :: et  !! Ephemeris time, seconds past J2000.
      real(wp) :: sclkdp  !! SCLK, encoded as ticks since spacecraft clock start. SCLKDP need not be integral.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sce2c ( sc, et, sclkdp )
      end subroutine sce2c

      !> Evaluate a single SPK data record from a segment of type 10
      !> (NORAD two-line element sets.). This evaluator uses algorithms
      !> as described in Vallado 2006 [4].
      !>
      !> Legacy interface: calls ctx%spke10 with the legacy context.
      subroutine spke10 ( et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: pi
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: state    ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke10 ( et, record, state )
      end subroutine spke10

      !> Return a 3x3 matrix that transforms positions in inertial
      !> coordinates to positions in body-equator-and-prime-meridian
      !> coordinates.
      !>
      !> Legacy interface: calls ctx%tipbod with the legacy context.
      subroutine tipbod ( ref, body, et, tipm )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: ref  !! ID of inertial reference frame to transform from.
      integer(ip) :: body  !! ID code of body.
      real(wp) :: et  !! Epoch of transformation.
      real(wp) :: tipm   ( 3, 3 )  !! Position transformation matrix, inertial to prime meridian.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tipbod ( ref, body, et, tipm )
      end subroutine tipbod

      !> Find the position rotation matrix from a C-kernel (CK) frame with
      !> the specified frame class ID (CK ID) to the base frame of the
      !> highest priority CK segment containing orientation data for this
      !> CK frame at the time requested.
      !>
      !> Legacy interface: calls ctx%ckfrot with the legacy context.
      subroutine ckfrot ( inst, et, rotate, ref, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: inst  !! Frame class ID (CK ID) of a CK frame.
      real(wp) :: et  !! Epoch measured in seconds past J2000 TDB.
      real(wp) :: rotate  ( 3, 3 )  !! Rotation matrix from CK frame to frame REF.
      integer(ip) :: ref  !! Frame ID of the base reference.
      logical :: found  !! .TRUE. when requested pointing is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckfrot ( inst, et, rotate, ref, found )
      end subroutine ckfrot

      !> Find the state transformation matrix from a C-kernel (CK) frame
      !> with the specified frame class ID (CK ID) to the base frame of
      !> the highest priority CK segment containing orientation and
      !> angular velocity data for this CK frame at the time requested.
      !>
      !> Legacy interface: calls ctx%ckfxfm with the legacy context.
      subroutine ckfxfm ( inst, et, xform, ref, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: inst  !! Frame class ID (CK ID) of a CK frame.
      real(wp) :: et  !! Epoch measured in seconds past J2000 TDB.
      real(wp) :: xform  ( 6, 6 )  !! Transformation from CK frame to frame REF.
      integer(ip) :: ref  !! Frame ID of the base reference.
      logical :: found  !! .TRUE. when requested pointing is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckfxfm ( inst, et, xform, ref, found )
      end subroutine ckfxfm

      !> Return, for a specified SPK segment and time, the state (position
      !> and velocity) of the segment's target body relative to its center
      !> of motion.
      !>
      !> Legacy interface: calls ctx%spkpvn with the legacy context.
      subroutine spkpvn ( handle, descr, et, ref, state, center )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spkrec
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      integer(ip) :: ref  !! Target reference frame.
      real(wp) :: state    ( 6 )  !! Position, velocity.
      integer(ip) :: center  !! Center of state.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: descr is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkpvn ( handle, (descr), et, ref, state, center )
      end subroutine spkpvn

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzswfxfm with the legacy context.
      subroutine zzswfxfm ( infrm, et, ndim, xform, outfrm, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzswtchf
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: infrm  !! Frame ID code of switch frame.
      real(wp) :: et  !! Time, expressed as seconds past J2000 TDB.
      integer(ip) :: ndim  !! Dimension: 3 or 6.
      real(wp) :: xform ( ndim, ndim )  !! Transformation from switch to selected base frame.
      integer(ip) :: outfrm  !! ID code of selected base frame.
      logical :: found  !! Flag indicating whether transformation was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzswfxfm ( ctx, infrm, et, ndim, xform, outfrm, found )
      end subroutine zzswfxfm

end module pepper_legacy_orientation
