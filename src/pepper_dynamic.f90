!> Dynamic frames (ZZDYNFRM, ZZDYNROT). A dynamic frame defined by states of
!> bodies or by other frames calls the frame and SPK routines (FRMCHG, SPKEZ,
!> ...), which may evaluate other dynamic frames: the calls nest to any depth.
!> SPICELIB, without recursion, used copies of those routines one and two
!> levels down (ZZ*0, ZZ*1) and refused a third level; the copies are gone
!> (the legacy interface in tspice maps their names to the routines). A
!> frame whose definition depends on itself is an error (the context keeps
!> the frames being evaluated).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> pepper_instance). Moved here from src/spicelib by
!> tools/modularize_ctx.py; the external procedures with the original
!> interface are in tspice/src/legacy/pepper_legacy_dynamic.f90.
submodule (pepper_instance) pepper_dynamic

   implicit none

contains

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     For a specified dynamic frame, find the transformation
      !>     that maps states from the dynamic frame to its base frame.
      !>
      !>## Detailed Input
      !>
      !>     INFRAM      is the frame ID code for a dynamic reference frame.
      !>                 Note that this interface differs from that of TKFRAM,
      !>                 which uses a class ID to identify the frame.
      !>
      !>                 In this routine, we refer this frame both as the
      !>                 "input frame" and the "defined frame."
      !>
      !>     CENTER      is NAIF ID code for the center of the frame
      !>                 designated by INFRAM.  This code, although derivable
      !>                 from INFRAM, is passed in for convenience.
      !>
      !>     ET          is an epoch in ephemeris seconds past J2000 for which
      !>                 the caller requests a state transformation matrix.
      !>
      !>## Detailed Output
      !>
      !>     XFORM       is a 6x6 matrix that transforms states relative to
      !>                 INFRAM to states relative to BASFRM.
      !>
      !>     BASFRM      is the frame ID code of the base frame associated
      !>                 with INFRAM.  The 6x6 matrix XFORM transforms states
      !>                 relative to INFRAM to states relative to BASFRM. The
      !>                 state transformation is performed by left-multiplying
      !>                 by XFORM a state expressed relative to INFRAM. This
      !>                 is easily accomplished via the subroutine call shown
      !>                 below.
      !>
      !>                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
      !>
      !>## Parameters
      !>
      !>     See include file zzdyn.inc.
      !>
      !>## Exceptions
      !>
      !>     1)  If a dynamic frame evaluation requires unavailable kernel
      !>         data, the error will be diagnosed by routines in the call
      !>         tree of this routine.
      !>
      !>     2)  If a precession model is used to implement a frame centered
      !>         at a body for which the model is not applicable, the error
      !>         SPICE(INVALIDSELECTION) will be signaled.
      !>
      !>     3)  If a nutation model is used to implement a frame centered
      !>         at a body for which the model is not applicable, the error
      !>         SPICE(INVALIDSELECTION) will be signaled.
      !>
      !>     4)  If an obliquity model is used to implement a frame centered
      !>         at a body for which the model is not applicable, the error
      !>         SPICE(INVALIDSELECTION) will be signaled.
      !>
      !>     5)  If an unrecognized precession model is specified, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     6)  If an unrecognized nutation model is specified, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     7)  If an unrecognized obliquity model is specified, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     8)  If an attempt to look up the center of a frame does
      !>         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
      !>         signaled.
      !>
      !>     9)  In a two-vector frame definition, if a constant vector
      !>         specification method is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     10) In a two-vector frame definition, if a vector definition
      !>         method is not recognized, the error SPICE(NOTSUPPORTED)
      !>         is signaled.
      !>
      !>     11) If an unrecognized dynamic frame family is specified, the
      !>          error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     12) If an unrecognized dynamic frame definition style is
      !>         specified, the error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     13) If an unrecognized dynamic frame rotation state is
      !>         specified, the error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     14) If both a freeze epoch and a rotation state are specified,
      !>         the error SPICE(FRAMEDEFERROR) is signaled.
      !>
      !>     15) If neither a freeze epoch nor a rotation state are specified
      !>         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
      !>         signaled.
      !>
      !>     16) In a two-vector frame definition, if an invalid axis
      !>         specification is encountered, the error SPICE(INVALIDAXIS) is
      !>         signaled.
      !>
      !>     17) In a two-vector frame definition using a target near point
      !>         vector, if the body-fixed frame associated with the target
      !>         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
      !>
      !>     18) If the state of the near point on a target as seen from
      !>         an observer cannot be computed, the error
      !>         SPICE(DEGENERATECASE) is signaled.
      !>
      !>     19) If a dynamic frame evaluation requires excessive recursion
      !>         depth, the error will be diagnosed by routines in the
      !>         call tree of this routine.
      !>
      !>     20) When a two-vector dynamic frame is evaluated, if the
      !>         primary and secondary vectors have angular separation less
      !>         than the minimum allowed value, or if the angular separation
      !>         differs from Pi by less than the minimum allowed value, the
      !>         error SPICE(DEGENERATECASE) is signaled.  The default minimum
      !>         separation is given by the parameter LBSEP; this value may be
      !>         overridden by supplying a different value in the frame
      !>         definition.
      !>
      !>     21) If invalid units occur in a frame definition, the error
      !>         will be diagnosed by a routine in the call tree of this
      !>         routine.
      !>
      !>     22) If an invalid Euler axis sequence occurs in a frame
      !>         definition, the error will be diagnosed by a routine in the
      !>         call tree of this routine.
      !>
      !>     23) If a body RADII vector has other than exactly thee elements,
      !>         the error SPICE(INVALIDCOUNT) is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     24) If a body RADII vector has any element less-than or
      !>         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     1) SPK files containing data for each observer and target
      !>        are required to support two-vector frames.  Note that
      !>        observer-target pairs can be implicit, as in the case
      !>        of a constant vector whose frame is evaluated at a
      !>        light-time corrected epoch:  the light time the frame
      !>        center to an observer must be computable in this case,
      !>        which implies the state of the frame center as seen by
      !>        the observer must be computable.
      !>
      !>     2) Any of SPK, CK, PCK, and frame kernels will also be required
      !>        if any frames referenced in the definition of INFRAM (as a
      !>        base frame, velocity vector frame, or constant vector frame)
      !>        require them, or if any vectors used to define INFRAM require
      !>        these data in order to be computable.
      !>
      !>     3) When CK data are required, one or more associated SCLK kernels
      !>        ---normally, one kernel per spacecraft clock---are
      !>        required as well.  A leapseconds kernel may be required
      !>        whenever an SCLK kernel is required.
      !>
      !>     4) When a two-vector frame is defined using a target near point,
      !>        a PCK file giving orientation and providing a triaxial shape
      !>        model for the target body is required.
      !>
      !>## Particulars
      !>
      !>     Currently only parameterized dynamic frames are supported by
      !>     this routine.
      !>
      !>     Currently supported parameterized dynamic families are:
      !>
      !>        Two-vector
      !>        ==========
      !>
      !>           Vector definitions
      !>           ------------------
      !>           Observer-target position
      !>           Observer-target velocity
      !>           Near point on target
      !>           Constant vector in specified frame
      !>
      !>        Mean Equator and Equinox of Date
      !>        ================================
      !>
      !>           Bodies and models
      !>           -----------------
      !>           Earth:  1976 IAU precession model
      !>
      !>        Mean Ecliptic and Equinox of Date
      !>        ================================
      !>
      !>           Bodies and models
      !>           -----------------
      !>           Earth:  1976 IAU precession model
      !>                   1980 IAU mean obliquity model
      !>
      !>        True Equator and Equinox of Date
      !>        ================================
      !>
      !>           Bodies and models
      !>           -----------------
      !>           Earth:  1976 IAU precession model
      !>                   1980 IAU nutation model
      !>
      !>        Euler frames
      !>        ============
      !>
      !>           Euler angle definitions
      !>           -----------------------
      !>           Polynomial
      !>
      !>        Product frames
      !>        ==============
      !>
      !>           Product definitions
      !>           -------------------
      !>           Array of "from" frames
      !>           Array of "to" frames
      !>
      !>## Examples
      !>
      !>     See FRMGET.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) Many numerical problems can occur when dynamic frames
      !>        are evaluated.  Users must determine whether dynamic frame
      !>        definitions are suitable for their applications. See the
      !>        Exceptions section for a list of possible problems.
      !>
      !>     3) Use of aberration corrections may lead to severe loss of
      !>        accuracy in state transformation derivatives.
      !>
      !>     4) Two-vector frame definitions can suffer extreme loss of
      !>         precision due to near-singular geometry.
      !>
      !>     5) Two-vector frame definitions involving velocity vectors
      !>        require numerical differentiation in order to compute the
      !>        derivative of the state transformation.  Such derivatives may
      !>        have low accuracy.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW)
      !>
      !>        Support for product frames was added. Re-ordered header
      !>        sections.
      !>
      !>        Body radii accessed from kernel pool using ZZGFTREB.
      !>
      !>        Bug fix: aberration correction strings for observer-target
      !>        velocity vectors are now parsed using ZZVALCOR. This improves
      !>        error checking. Note that aberration correction strings for
      !>        constant vectors used in two-vector frame specifications are
      !>        still parsed using ZZPRSCOR.
      !>
      !>        Bug fix: calls to FAILED after calls to ZZDYNVAC were added.
      !>
      !>        Added some FAILED() calls.
      !>
      !>-    SPICELIB Version 1.1.1, 02-MAR-2010 (NJB)
      !>
      !>        Typo in Brief_I/O section was corrected: "Class ID"
      !>        was changed to "Frame ID." Corrected order of header
      !>        sections.
      !>
      !>-    SPICELIB Version 1.1.0, 23-OCT-2005 (NJB)
      !>
      !>        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
      !>
      !>        Call to ZZBODVCD was replaced with call to BODVCD.
      !>
      !>-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB)
      !>
      !>     SPICELIB functions
      !>
      !>     Local parameters
      !>
      !>     Local variables
      !>
      !>     Saved variables
      !>
      !>     Initial values
      module procedure zzdynfrm
!
!     PEPPER: the evaluation (dynamic_frame_xform) may need other dynamic frames,
!     evaluated by nested calls of this routine through FRMCHG, REFCHG,
!     SPKEZ, ... (SPICELIB used copies of those routines for one more
!     level, and signaled SPICE(RECURSIONTOODEEP) for a third). The frames
!     being evaluated are kept in the context: a frame that is already
!     among them is defined in terms of itself.
!
      use pepper_kinds, only: ip, wp
      implicit none
      call enter_dynamic_frame ( ctx, infram )
      if ( ctx%failed() ) return
      call dynamic_frame_xform ( ctx, infram, center, et, xform, basfrm )
      ctx%ndynamic = ctx%ndynamic - 1
      end procedure zzdynfrm

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     For a specified dynamic frame, find the rotation that maps
      !>     positions from the dynamic frame to its base frame.
      !>
      !>## Detailed Input
      !>
      !>     INFRAM      is the frame ID code for a dynamic reference frame.
      !>                 Note that this interface differs from that of TKFRAM,
      !>                 which uses a class ID to identify the frame.
      !>
      !>                 In this routine, we refer this frame both as the
      !>                 "input frame" and the "defined frame."
      !>
      !>     CENTER      is NAIF ID code for the center of the frame
      !>                 designated by INFRAM.  This code, although derivable
      !>                 from INFRAM, is passed in for convenience.
      !>
      !>     ET          is an epoch in ephemeris seconds past J2000 for which
      !>                 the caller requests a rotation matrix.
      !>
      !>## Detailed Output
      !>
      !>     ROTATE      is a 3x3 rotation matrix that transforms positions
      !>                 relative to INFRAM to positions relative to BASFRM.
      !>
      !>     BASFRM      is the frame ID code of the base frame associated
      !>                 with INFRAM.  The 3x3 matrix ROTATE transforms
      !>                 positions relative to INFRAM to positions relative to
      !>                 BASFRM. The position transformation is performed by
      !>                 left-multiplying by ROTATE a position expressed
      !>                 relative to INFRAM. This is easily accomplished via
      !>                 the subroutine call shown below.
      !>
      !>                    CALL MXV ( ROTATE, INPOS, OUTPOS )
      !>
      !>## Parameters
      !>
      !>     See include file zzdyn.inc.
      !>
      !>## Exceptions
      !>
      !>     1)  If a dynamic frame evaluation requires unavailable kernel
      !>         data, the error will be diagnosed by routines in the
      !>         call tree of this routine.
      !>
      !>     2)  If a precession model is used to implement a frame centered
      !>         at a body for which the model is not applicable, the error
      !>         SPICE(INVALIDSELECTION) will be signaled.
      !>
      !>     3)  If a nutation model is used to implement a frame centered
      !>         at a body for which the model is not applicable, the error
      !>         SPICE(INVALIDSELECTION) will be signaled.
      !>
      !>     4)  If an obliquity model is used to implement a frame centered
      !>         at a body for which the model is not applicable, the error
      !>         SPICE(INVALIDSELECTION) will be signaled.
      !>
      !>     5)  If an unrecognized precession model is specified, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     6)  If an unrecognized nutation model is specified, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     7)  If an unrecognized obliquity model is specified, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     8)  If an attempt to look up the center of a frame does
      !>         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
      !>         signaled.
      !>
      !>     9)  In a two-vector frame definition, if a constant vector
      !>         specification method is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     10) In a two-vector frame definition, if a vector definition
      !>         method is not recognized, the error SPICE(NOTSUPPORTED)
      !>         is signaled.
      !>
      !>     11) If an unrecognized dynamic frame family is specified, the
      !>          error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     12) If an unrecognized dynamic frame definition style is
      !>         specified, the error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     13) If an unrecognized dynamic frame rotation state is
      !>         specified, the error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     14) If both a freeze epoch and a rotation state are specified,
      !>         the error SPICE(FRAMEDEFERROR) is signaled.
      !>
      !>     15) If neither a freeze epoch nor a rotation state are specified
      !>         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
      !>         signaled.
      !>
      !>     16) In a two-vector frame definition, if an invalid axis
      !>         specification is encountered, the error SPICE(INVALIDAXIS) is
      !>         signaled.
      !>
      !>     17) In a two-vector frame definition using a target near point
      !>         vector, if the body-fixed frame associated with the target
      !>         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
      !>
      !>     18) If a dynamic frame evaluation requires excessive recursion
      !>         depth, the error will be diagnosed by routines in the call
      !>         tree of this routine.
      !>
      !>     19) When a two-vector dynamic frame is evaluated, if the
      !>         primary and secondary vectors have angular separation less
      !>         than the minimum allowed value, or if the angular separation
      !>         differs from Pi by less than the minimum allowed value, the
      !>         error SPICE(DEGENERATECASE) is signaled.  The default minimum
      !>         separation is given by the parameter LBSEP; this value may be
      !>         overridden by supplying a different value in the frame
      !>         definition.
      !>
      !>     20) If invalid units occur in a frame definition, the error
      !>         will be diagnosed by a routine in the call tree of this
      !>         routine.
      !>
      !>     21) If an invalid Euler axis sequence occurs in a frame
      !>         definition, the error will be diagnosed by a routine in the
      !>         call tree of this routine.
      !>
      !>     22) If a body RADII vector has other than exactly thee elements,
      !>         the error SPICE(INVALIDCOUNT) is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     23) If a body RADII vector has any element less-than or
      !>         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     1) SPK files containing data for each observer and target
      !>        are required to support two-vector frames.  Note that
      !>        observer-target pairs can be implicit, as in the case
      !>        of a constant vector whose frame is evaluated at a
      !>        light-time corrected epoch:  the light time the frame
      !>        center to an observer must be computable in this case,
      !>        which implies the state of the frame center as seen by
      !>        the observer must be computable.
      !>
      !>     2) Any of SPK, CK, PCK, and frame kernels will also be required
      !>        if any frames referenced in the definition of INFRAM (as a
      !>        base frame, velocity vector frame, or constant vector frame)
      !>        require them, or if any vectors used to define INFRAM require
      !>        these data in order to be computable.
      !>
      !>     3) When CK data are required, one or more associated SCLK kernels
      !>        ---normally, one kernel per spacecraft clock---are
      !>        required as well.  A leapseconds kernel may be required
      !>        whenever an SCLK kernel is required.
      !>
      !>     4) When a two-vector frame is defined using a target near point,
      !>        a PCK file giving orientation and providing a triaxial shape
      !>        model for the target body is required.
      !>
      !>## Particulars
      !>
      !>     Currently only parameterized dynamic frames are supported by
      !>     this routine.
      !>
      !>     Currently supported parameterized dynamic families are:
      !>
      !>        Two-vector
      !>        ==========
      !>
      !>           Vector definitions
      !>           ------------------
      !>           Observer-target position
      !>           Observer-target velocity
      !>           Near point on target
      !>           Constant vector in specified frame
      !>
      !>        Mean Equator and Equinox of Date
      !>        ================================
      !>
      !>           Bodies and models
      !>           -----------------
      !>           Earth:  1976 IAU precession model
      !>
      !>        Mean Ecliptic and Equinox of Date
      !>        ================================
      !>
      !>           Bodies and models
      !>           -----------------
      !>           Earth:  1976 IAU precession model
      !>                   1980 IAU mean obliquity model
      !>
      !>        True Equator and Equinox of Date
      !>        ================================
      !>
      !>           Bodies and models
      !>           -----------------
      !>           Earth:  1976 IAU precession model
      !>                   1980 IAU nutation model
      !>
      !>        Euler frames
      !>        ============
      !>
      !>           Euler angle definitions
      !>           -----------------------
      !>           Polynomial
      !>
      !>        Product frames
      !>        ==============
      !>
      !>           Product definitions
      !>           -------------------
      !>           Array of "from" frames
      !>           Array of "to" frames
      !>
      !>## Examples
      !>
      !>     See ROTGET.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) Many numerical problems can occur when dynamic frames
      !>        are evaluated.  Users must determine whether dynamic frame
      !>        definitions are suitable for their applications. See the
      !>        Exceptions section for a list of possible problems.
      !>
      !>     3) Two-vector frame definitions can suffer extreme loss of
      !>         precision due to near-singular geometry.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW)
      !>
      !>        Support for product frames was added.
      !>
      !>        Body radii accessed from kernel pool using ZZGFTREB.
      !>
      !>        Bug fix: aberration correction strings for observer-target
      !>        velocity vectors are now parsed using ZZVALCOR. This improves
      !>        error checking. Note that aberration correction strings for
      !>        constant vectors used in two-vector frame specifications are
      !>        still parsed using ZZPRSCOR.
      !>
      !>        Code clean-up: unnecessary call to ZZPRSCOR was removed.
      !>
      !>        Added and removed some FAILED() calls.
      !>
      !>-    SPICELIB Version 1.1.0, 24-OCT-2005 (NJB)
      !>
      !>        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
      !>
      !>        Call to ZZBODVCD was replaced with call to BODVCD.
      !>
      !>-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB)
      !>
      !>     SPICELIB functions
      !>
      !>     Local parameters
      !>
      !>     Local variables
      !>
      !>     Saved variables
      !>
      !>     Initial values
      module procedure zzdynrot
!
!     PEPPER: the evaluation (dynamic_frame_rotation) may need other dynamic frames,
!     evaluated by nested calls of this routine through FRMCHG, REFCHG,
!     SPKEZ, ... (SPICELIB used copies of those routines for one more
!     level, and signaled SPICE(RECURSIONTOODEEP) for a third). The frames
!     being evaluated are kept in the context: a frame that is already
!     among them is defined in terms of itself.
!
      use pepper_kinds, only: ip, wp
      implicit none
      call enter_dynamic_frame ( ctx, infram )
      if ( ctx%failed() ) return
      call dynamic_frame_rotation ( ctx, infram, center, et, rotate, basfrm )
      ctx%ndynamic = ctx%ndynamic - 1
      end procedure zzdynrot

      !> PEPPER: the evaluation of ZZDYNFRM (SPICELIB's body of the
      !> routine; see zzdynfrm).
      recursive subroutine dynamic_frame_xform ( ctx, infram, center, et, xform, basfrm )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: latrec, mxmg, mxvg, polyds, vhat, vsep
      use pepper_strings, only: cmprss, eqstr, intstr, ucase
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzabcorr
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: infram  !! the dynamic frame
      integer(ip), intent(in) :: center  !! ID code for the center of the input frame.
      real(wp), intent(in) :: et  !! An epoch in seconds past J2000 TDB.
      real(wp), intent(out) :: xform ( 6, 6 )  !! The requested state transformation matrix.
      integer(ip), intent(out) :: basfrm  !! Frame ID of base frame associated with INFRAM.
      real(wp) :: epoch_1(1)  !! one-element copy of epoch (array argument)
      real(wp) :: minsep_1(1)  !! one-element copy of minsep (array argument)
      real(wp) :: dec_1(1)  !! one-element copy of dec (array argument)
      real(wp) :: ra_1(1)  !! one-element copy of ra (array argument)
      real(wp) :: lat_1(1)  !! one-element copy of lat (array argument)
      real(wp) :: lon_1(1)  !! one-element copy of lon (array argument)
      real(wp) :: t0_1(1)  !! one-element copy of t0 (array argument)
      character(*), parameter :: rname = 'ZZDYNFRM'
      integer(ip), parameter :: timlen = 50
      integer(ip), parameter :: vnamln = 4
      character(corlen)     abcorr
      character(kvlen)      axname
      character(:), allocatable :: basnam
      character(:), allocatable :: cfrmnm
      character(:), allocatable :: ctrnam
      logical :: namfnd
      character(corlen)     cvcorr
      character(kvlen)      dynstl
      character(kvlen)      dynfam
      type(string), allocatable :: fframs(:)
      character(:), allocatable :: inname
      character(kvlen)      nutmod
      character(kvlen)      oblmod
      character(kvlen)      prcmod
      character(kvlen)      rotsta
      character(kvlen)      spec
      type(string), allocatable :: tframs(:)
      character(timlen)     timstr
      character(kvlen)      tmpfam
      character(kvlen)      units
      character(kvlen)      vecdef ( 2 )
      character(:), allocatable :: velfrm
      character(vnamln), parameter :: vname  ( 2 ) = &
         [ character(vnamln) :: kwpri, kwsec ]
      real(wp)              acc    ( 3 )
      real(wp)              angles ( 2 )
      real(wp)              coeffs ( maxcof, 3 )
      real(wp)              ctrpos ( 3 )
      real(wp)              dec
      real(wp)              delta
      real(wp)              dirvec ( 3 )
      real(wp)              dmob
      real(wp)              epoch
      real(wp)              eulang ( 6 )
      real(wp)              fet
      real(wp)              lat
      real(wp)              lon
      real(wp)              lt
      real(wp)              minsep
      real(wp)              mob
      real(wp)              nutxf  ( 6, 6 )
      real(wp)              oblxf  ( 6, 6 )
      real(wp)              poly   ( 0 : 1 )
      real(wp)              precxf ( 6, 6 )
      real(wp)              ra
      real(wp)              radii  ( 3 )
      real(wp)              s2     ( 6, 2 )
      real(wp)              sep
      real(wp)              stalt  ( 2 )
      real(wp)              stemp  ( 6 )
      real(wp)              stnear ( 6 )
      real(wp)              stobs  ( 6 )
      real(wp)              tarray ( 3 )
      real(wp)              t0
      real(wp)              varray ( 3, 3 )
      real(wp)              vet
      real(wp)              vflt
      real(wp)              xf2000 ( 6, 6 )
      real(wp)              xfinv  ( 6, 6 )
      real(wp)              xftemp ( 6, 6 )
      real(wp)              xipm   ( 6, 6 )
      real(wp)              xout   ( 6, 6 )
      integer(ip)           axis   ( 2 )
      integer(ip)           cfrmid
      integer(ip)           cvobs
      integer(ip)           degs   ( 3 )
      integer(ip)           frcid
      integer(ip)           frcls
      integer(ip)           frctr
      integer(ip)           frid
      integer(ip)           fromid
      integer(ip)           i
      integer(ip)           iaxes  ( 3 )
      integer(ip)           j
      integer(ip)           m
      integer(ip)           n
      integer(ip)           obs
      integer(ip)           targ
      integer(ip)           toid
      logical               corblk ( nabcor )
      logical               fnd
      logical               frozen
      logical               inert
      logical               meanec
      logical               meaneq
      logical               negate
      logical               ofdate
      logical               trueeq
      if ( .not. allocated ( ctx%zzdynfrm_state ) ) allocate ( ctx%zzdynfrm_state )
      associate ( axes => ctx%zzdynfrm_state%axes, &
                  earth => ctx%zzdynfrm_state%earth, &
                  first => ctx%zzdynfrm_state%first, &
                  j2000 => ctx%zzdynfrm_state%j2000, &
                  itmabc => ctx%zzdynfrm_state%itmabc, &
                  itmaxe => ctx%zzdynfrm_state%itmaxe, &
                  itmcof => ctx%zzdynfrm_state%itmcof, &
                  itmdec => ctx%zzdynfrm_state%itmdec, &
                  itmfrm => ctx%zzdynfrm_state%itmfrm, &
                  itmlat => ctx%zzdynfrm_state%itmlat, &
                  itmlon => ctx%zzdynfrm_state%itmlon, &
                  itmobs => ctx%zzdynfrm_state%itmobs, &
                  itmra => ctx%zzdynfrm_state%itmra, &
                  itmsep => ctx%zzdynfrm_state%itmsep, &
                  itmspc => ctx%zzdynfrm_state%itmspc, &
                  itmtrg => ctx%zzdynfrm_state%itmtrg, &
                  itmunt => ctx%zzdynfrm_state%itmunt, &
                  itmvdf => ctx%zzdynfrm_state%itmvdf, &
                  itmvec => ctx%zzdynfrm_state%itmvec )
      if ( ctx%failed() ) return

      if ( first ) then
!
!        Get the ID code for the J2000 frame.
!
         call irfnum ( ctx, 'J2000', j2000 )

!
!        Get the ID code for the earth (we needn't check the found
!        flag).
!
         call bodn2c ( ctx, 'EARTH', earth, fnd )

!
!        Initialize "item" strings used to create kernel variable
!        names.
!
         do i = 1, 2
!
!           Vector axis:
!
            itmaxe(i) = vname(i) // kwvaxi
!
!           Vector definition:
!
            itmvdf(i) = vname(i) // kwvcdf
!
!           Vector aberration correction:
!
            itmabc(i) = vname(i) // kwvabc
!
!           Vector frame:
!
            itmfrm(i) = vname(i) // kwvfrm
!
!           Vector observer:
!
            itmobs(i) = vname(i) // kwvobs
!
!           Vector target:
!
            itmtrg(i) = vname(i) // kwvtrg
!
!           Vector longitude:
!
            itmlon(i) = vname(i) // kwlon
!
!           Vector latitude:
!
            itmlat(i) = vname(i) // kwlat
!
!           Vector right ascension:
!
            itmra(i)  = vname(i) // kwra
!
!           Vector declination:
!
            itmdec(i) = vname(i) // kwdec
!
!           Vector units:
!
            itmunt(i) = vname(i) // kwunit
!
!           Constant vector coordinate specification:
!
            itmspc(i) = vname(i) // kwvspc
!
!           Constant vector in Cartesian coordinates, literal value:
!
            itmvec(i) = vname(i) // kwvect

         end do

         first = .false.

      end if

!
!     Initialize the output arguments.
!
      xform = 0.0_wp

      basfrm = 0

!
!     Initialize certain variables to ensure that we don't do
!     arithmetic operations using bogus, possibly large,
!     undefined values.
!
      nutxf = 0.0_wp
      oblxf = 0.0_wp
      precxf = 0.0_wp
      xf2000 = 0.0_wp
      xfinv = 0.0_wp
      xipm = 0.0_wp

      mob    = 0.0_wp
      dmob   = 0.0_wp
      t0     = 0.0_wp
      frozen = .false.

!
!     Get the input frame name.
!
      call frame_name_of ( ctx, infram, inname, namfnd )

!
!     We need the name of the base frame.
!
      call zzdynfid ( ctx, inname, infram, kwbfrm, basfrm )

      if ( ctx%failed() ) return

      call frame_name_of ( ctx, basfrm, basnam, namfnd )

!
!     The output frame code and name are set.
!
!     Look up the dynamic frame definition style from the kernel pool.
!     The kernel variable's name might be specified by name or ID.
!
      call zzdynvac ( ctx, inname, infram, kwstyl, 1, n, dynstl )

      if ( ctx%failed() ) return

!
!     At this time, the only supported dynamic frame definition style is
!     PARAMETERIZED.
!
      if (  eqstr( dynstl, kvparm )  ) then
!
!        Parameterized dynamic frames belong to families.  Look up
!        the family for this frame.
!
         call zzdynvac ( ctx, inname, infram, kwffam, 1, n, dynfam )

         if ( ctx%failed() ) return

         call cmprss ( ' ', 0, dynfam, tmpfam )
         call ucase  ( tmpfam,         dynfam )

!
!        Determine whether we have an "of-date" frame family.
!        The logical flags used here and respective meanings are:
!
!           MEANEQ   Mean equator and equinox of date
!           TRUEEQ   True equator and equinox of date
!           MEANEC   Mean ecliptic and equinox of date
!
         meaneq  =  dynfam == kvmeqt
         trueeq  =  dynfam == kvteqt
         meanec  =  dynfam == kvmecl

         ofdate  =  meaneq .or. meanec .or. trueeq

!
!        Set the evaluation epoch T0.  Normally this epoch is ET,
!        but if the frame is frozen, the freeze epoch from the
!        frame definition is used.
!
!        Read the freeze epoch into T0 if a freeze epoch was
!        specified; let FROZEN receive the FOUND flag value
!        returned by ZZDYNOAD.
!
         call zzdynoad ( ctx, inname, infram, kwfrez, 1, n, t0_1, frozen )
         t0 = t0_1(1)

         if ( ctx%failed() ) return

         if ( .not. frozen ) then
!
!           Normal case:  just use the input epoch.
!
            t0 = et

         end if

!
!        Look up the rotation state keyword.  Rather than checking
!        FAILED() after every call, we'll do it after we're
!        done with processing the rotation state.
!
         call zzdynoac ( ctx, inname, infram, kwrsta, 1, n, rotsta, fnd )

         if ( ctx%failed() ) return

!
!        If the frame is frozen, the rotation state keyword *must be
!        absent*.
!
         if ( frozen .and. fnd ) then

            call ctx%setmsg ( 'Definition of frame # contains both ' // &
                          '# and # '                             // &
                          'keywords; at most one of these must ' // &
                          'be present in the frame definition. ' // &
                          'This situation is usually caused by ' // &
                          'an error in a frame kernel in which ' // &
                          'the frame is defined.'                )
            call ctx%errch  ( '#',  inname                           )
            call ctx%errch  ( '#',  kwfrez                           )
            call ctx%errch  ( '#',  kwrsta                           )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                 )
            return

         end if

!
!        If the frame belongs to an "of date" family, either the
!        rotation state must be specified or the frame must be
!        frozen.
!
         if (  ofdate  .and.  (.not. frozen) .and. (.not. fnd )  ) then

            call ctx%setmsg ( 'Definition of frame #, which belongs to ' // &
                          'parameterized dynamic frame family #, '   // &
                          'contains neither # nor # keywords; '      // &
                          'frames in this family require exactly '   // &
                          'one of these in their frame definitions. '// &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  inname                               )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  kwfrez                               )
            call ctx%errch  ( '#',  kwrsta                               )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                     )
            return

         end if

!
!        Set the rotation state logical flag indicating whether
!        the state is 'INERTIAL'.
!
         if ( fnd ) then
!
!           A rotation state keyword was found.
!
!           We know the state is not frozen if we arrive here.
!
            inert  =  eqstr ( rotsta, kvinrt )

            if ( .not. inert ) then
!
!              Catch invalid rotation states here.
!
               if (  .not.  eqstr ( rotsta, kvrotg )  ) then

                  call ctx%setmsg ( 'Definition of frame # contains '     // &
                                '# specification #. The only valid '  // &
                                'rotation states are # or #. '        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  kwrsta                          )
                  call ctx%errch  ( '#',  rotsta                          )
                  call ctx%errch  ( '#',  kvrotg                          )
                  call ctx%errch  ( '#',  kvinrt                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

         else
!
!           The state is not inertial unless there's a ROTATION_STATE
!           keyword assignment telling us it is.
!
            inert  = .false.

         end if

!
!        INERT and FROZEN are both set. The evaluation epoch T0 is also
!        set.
!
!        The following code block performs actions specific to
!        the various dynamic frame families.
!
         if ( ofdate ) then
!
!           Fetch the name of the true equator and equinox of date
!           precession model.
!
            call zzdynvac ( ctx, inname, infram, kwprcm, 1, n, prcmod )

            if ( ctx%failed() ) return

!
!           Get the precession transformation.
!
            if (  eqstr( prcmod, kvm001 )  ) then
!
!              This is the 1976 IAU earth precession model.
!
!              Make sure the center of the input frame is the earth.
!
               if ( center /= earth ) then

                  call body_name_of ( ctx, center, ctrnam, fnd )

                  if ( .not. fnd ) then
                     ctrnam = repeat ( ' ', 11 )
                     call intstr ( center, ctrnam )
                     ctrnam = trim ( ctrnam )
                  end if

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'frame center # and precession '      // &
                                'model #. This precession model '     // &
                                'is not applicable to body #. '       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%errch  ( '#',  kvm001                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'             )
                  return

               end if
!
!              Look up the precession transformation.
!
               call zzeprc76 ( ctx, t0, precxf )

!
!              If we're in the mean-of-date case, invert this
!              transformation to obtain the mapping from the
!              mean-of-date frame to J2000.
!
               if ( meaneq ) then
                  call invstm ( ctx, precxf, xftemp )
               end if

            else

               call ctx%setmsg ( 'Definition of frame # specifies '    // &
                             'precession model #, which is not '   // &
                             'recognized. '                        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  prcmod                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

!
!           At this point the precession transformation PRECXF is set.
!           If INFRAM is a mean equator and equinox of date frame, the
!           inverse of PRECXF is currently stored in XFTEMP.

            if ( trueeq ) then
!
!              We need a nutation transformation as well. Get the name
!              of the nutation model.
!
               call zzdynvac ( ctx, inname, infram, kwnutm, 1, n, nutmod )

               if ( ctx%failed() ) return

!
!              Get the nutation transformation.
!
               if (  eqstr( nutmod, kvm002 )  ) then
!
!                 This is the 1980 IAU earth nutation model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call body_name_of ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        ctrnam = repeat ( ' ', 11 )
                        call intstr ( center, ctrnam )
                        ctrnam = trim ( ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and nutation '     // &
                                   'model #. This '                   // &
                                   'nutation model is not applicable '// &
                                   'to body #.  This situation is '   // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm002                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Look up the nutation transformation.
!
                  call zzenut80 ( ctx, t0, nutxf )

!
!                 Find the transformation from the J2000 frame to the
!                 earth true of date frame.  Invert.
!
                  call mxmg   ( nutxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( ctx, xfinv, xftemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'nutation model #, which is not '     // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  nutmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            else if ( meanec ) then
!
!              We need a mean obliquity transformation as well.
!              Get the name of the obliquity model.
!
               call zzdynvac ( ctx, inname, infram, kwobqm, 1, n, oblmod )

               if ( ctx%failed() ) return

!
!              Get the obliquity transformation.
!
               if (  eqstr( oblmod, kvm003 )  ) then
!
!                 This is the 1980 IAU earth mean obliquity of
!                 date model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call body_name_of ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        ctrnam = repeat ( ' ', 11 )
                        call intstr ( center, ctrnam )
                        ctrnam = trim ( ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and obliquity '    // &
                                   'model #.  This obliquity model '  // &
                                   'is not applicable to body #. '    // &
                                   'This situation is '               // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm003                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Create the obliquity transformation. First look up
!                 the obliquity state (angle and angular rate).
!
                  call zzmobliq ( ctx, t0, mob, dmob )

!
!                 The obliquity rotation is about the mean-of-date
!                 x-axis.  The other Euler angles are identically
!                 zero; the axes are arbitrary, as long as the
!                 middle axis is distinct from the other two.
!
                  eulang = 0.0_wp
                  eulang(3) = mob
                  eulang(6) = dmob

                  call eul2xf ( ctx, eulang, 1, 3, 1, oblxf )

!
!                 Find the transformation from the J2000 to the
!                 earth mean ecliptic of date frame.  Invert.
!
                  call mxmg   ( oblxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( ctx, xfinv, xftemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'obliquity model #, which is not '    // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  oblmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

!
!           At this point, XFTEMP contains the mapping from the
!           specified mean of date or true of date frame to J2000.
!
!           If the base frame is not J2000, we must find the
!           transformation from J2000 to the base frame.
!
            if ( basfrm /= j2000 ) then

               call frmchg ( ctx, j2000,  basfrm, t0,      xf2000 )
               call mxmg     ( xf2000, xftemp, 6, 6, 6, xform  )

            else
!
!              Otherwise, XFTEMP is the matrix we want.
!
               xform = xftemp

            end if

!
!           Now XFORM is the state transformation mapping from
!           the input frame INFRAM to the base frame BASFRM.
!
!           This is the end of the work specific to "of-date" frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kv2vec ) then
!
!           The frame belongs to the TWO-VECTOR family.
!
!           Initialize the array S2.
!
            s2 = 0.0_wp

!
!           Fetch the specifications of the primary and secondary
!           axes.
!
            do i = 1, 2
!
!              Get the name of the axis associated with the Ith
!              defining vector.
!
               call zzdynvac ( ctx, inname, infram, itmaxe(i), 1, n, axname )

               if ( ctx%failed() ) return

               call cmprss ( ' ', 0, axname, axname )
               call ucase  ( axname,         axname )

!
!              Set the sign flag associated with the axis.
!
               negate  =  axname(1:1) == '-'

               call cmprss ( '-', 0, axname, axname )
               call cmprss ( '+', 0, axname, axname )

               axis(i) = findloc(axes, axname, dim=1)

               if ( axis(i) == 0 ) then

                  call ctx%setmsg ( 'Definition of frame # associates '   // &
                                'vector # with axis #.  The only '    // &
                                'valid axis values are { X, -X, Y, '  // &
                                '-Y, Z, -Z }. '                       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errint ( '#',  i                               )
                  call ctx%errch  ( '#',  axname                          )
                  call ctx%sigerr ( 'SPICE(INVALIDAXIS)'                  )
                  return

               end if

!
!              Find out how the vector is defined:
!
!                 - Observer-target position vector
!                 - Observer-target velocity vector
!                 - Observer-target near point vector
!                 - Constant vector
!
!              VECDEF(I) indicates the vector definition method
!              for the Ith vector.
!
               call zzdynvac ( ctx, inname,  infram,    itmvdf(i), &
                               1,       n,         vecdef(i) )

               if ( ctx%failed() ) return

               call cmprss ( ' ',    0, vecdef(i), vecdef(i) )
               call ucase  ( vecdef(i),            vecdef(i) )

               if (  vecdef(i) == kvposv  ) then
!
!                 The vector is the position of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 Look up the Ith state vector in the J2000 frame.
!
                  call spkez ( ctx, targ,    t0,   'J2000', &
                                  abcorr,  obs,  s2(1,i),  lt )

                  if ( ctx%failed() ) return

!
!                 At this point, S2(*,I) contains position and
!                 velocity relative to frame J2000.
!

               else if ( vecdef(i) == kvvelv ) then
!
!                 The vector is the velocity of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 We need to know the frame in which the velocity is
!                 defined.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )
                  call frame_name_of ( ctx, frid, velfrm, namfnd )

                  if ( ctx%failed() ) return

!
!                 Obtain the velocity vector in the specified velocity
!                 frame.  Also obtain bracketing vectors to support
!                 discrete differentiation.  (See notes in zzdyn.inc
!                 regarding definition of DELTA.)
!
                  delta = max ( 1.0_wp,  (2.0_wp**qexp) * t0 )

                  tarray = [t0-delta, t0, t0+delta]

                  do j = 1, 3

                     call spkez ( ctx, targ,   tarray(j), velfrm, &
                                     abcorr, obs,       stemp,  lt )
!
!                    We compute the derivative using unit
!                    velocity vectors.
!
                     if ( ctx%failed() ) return

                     call vhat ( stemp(4), varray(1,j) )

                  end do

!
!                 Compute acceleration and fill in the velocity state
!                 vector S2(*,I).
!
                  call qderiv ( ctx, 3, &
                                varray(1,1),  varray(1,3),  delta, acc )

                  s2(1:3,i) = varray(:,2)
                  s2(4:6,i) = acc

!
!                 We need the epoch VET at which VELFRM is evaluated.
!                 This epoch will be used to transform the velocity's
!                 "state" vector from VELFRM to J2000.
!
!                 Set the default value of VET here.
!
                  vet = t0

!
!                 Parse the aberration correction.  Find the epoch used
!                 to evaluate the velocity vector's frame.
!
                  call zzvalcor ( ctx, abcorr, corblk )

                  if ( corblk(ltidx) ) then
!
!                    Light time correction is used.  The epoch used
!                    to evaluate the velocity vector's frame depends
!                    on the frame's observer and center.
!
!                    Look up the velocity frame's center.
!
                     call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                     if ( .not. fnd ) then

                        call ctx%setmsg ( 'In definition of frame #, ' // &
                                      'the frame associated with ' // &
                                      'a velocity vector has '     // &
                                      'frame ID code #, but no '   // &
                                      'frame center, frame class, '// &
                                      'or frame class ID was '     // &
                                      'found by FRINFO.  This '    // &
                                      'situation MAY be caused '   // &
                                      'by an error in a frame '    // &
                                      'kernel in which the frame ' // &
                                      'is defined. The problem '   // &
                                      'also could be indicative '  // &
                                      'of a SPICELIB bug.'         )
                        call ctx%errch  ( '#',  inname                 )
                        call ctx%errint ( '#',  frid                   )
                        call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                        return

                     end if

!
!                    If the velocity frame is non-inertial, we'll need
!                    to compute the evaluation epoch for this frame.
!
                     if ( frcls /= inertl ) then
!
!                       Obtain light time from the observer to the
!                       frame's center; find the evaluation epoch VET
!                       for the frame.

                        call spkezp ( ctx, frctr,  t0,  'J2000', &
                                        abcorr, obs, ctrpos,  vflt )
                        call zzcorepc ( ctx, abcorr, t0,  vflt,    vet  )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 The velocity frame's evaluation epoch VET is now set.
!
!                 We must rotate the velocity vector and transform the
!                 acceleration from the velocity frame (evaluated at
!                 VET) to the output frame at T0.  We'll do this in two
!                 stages, first mapping velocity and acceleration into
!                 the J2000 frame.
!
                  if ( frid /= j2000 ) then

                     call frmchg ( ctx, frid,    j2000,  vet,   xf2000  )

                     if ( ctx%failed() ) return

                     call mxvg     ( xf2000,  s2(1,i), 6, 6, stemp   )
                     s2(:,i) = stemp

                  end if

!
!                 At this point, S2(*,I) contains velocity and
!                 acceleration relative to frame J2000.
!

               else if ( vecdef(i) == kvnear ) then
!
!                 The vector points from an observer to the
!                 sub-observer point (nearest point to the observer) on
!                 the target body.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 The vector points from the observer to the nearest
!                 point on the target. We need the state of the near
!                 point relative to the observer.
!
!                 We'll look up the state of the target center relative
!                 to the observer and the state of the near point
!                 relative to the target center, both in the body-fixed
!                 frame associated with the target.
!
!                 Look up the body-fixed frame associated with the
!                 target body.
!
                  call frame_of_center ( ctx, targ, cfrmid, cfrmnm, fnd )

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'Definition of frame # requires ' // &
                                   'definition of body-fixed frame ' // &
                                   'associated with target body #. ' // &
                                   'A call to CIDFRM indicated no '  // &
                                   'body-fixed frame is associated ' // &
                                   'with the target body.  This '    // &
                                   'situation can arise when a '     // &
                                   'frame kernel defining the '      // &
                                   'target''s body-fixed frame  '    // &
                                   'lacks the OBJECT_<ID>_FRAME '    // &
                                   'or OBJECT_<name>_FRAME '         // &
                                   'keywords.  The problem also '    // &
                                   'could be caused by an '          // &
                                   'error in a frame kernel in '     // &
                                   'which the parameterized '        // &
                                   'two-vector dynamic frame # '     // &
                                   'is defined.'                     )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     return

                  end if

!
!                 Get the radii of the target body.
!
                  call zzgftreb ( ctx, targ, radii )

!
!                 Look up the Ith state vector in the target-fixed
!                 frame.  Negate the vector to obtain the target-to-
!                 observer vector.
!
                  call spkez ( ctx, targ,    t0,       cfrmnm, &
                                  abcorr,  obs,      stemp,    lt )

!
!                 We check FAILED() here because VMINUG is a simple
!                 arithmetic routine that doesn't return on entry.
!
                  if ( ctx%failed() ) return

                  stobs = -stemp

                  call dnearp   ( ctx, stobs,   radii(1), radii(2), radii(3), &
                                  stnear,  stalt,    fnd               )

                  if ( ctx%failed() ) return

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'In definition of frame #, '      // &
                                   'vector # is defined by the '     // &
                                   'near point on body # as seen '   // &
                                   'from body #.  The state of this '// &
                                   'near point was not found. '      // &
                                   'See the routine DNEARP for '     // &
                                   'an explanation.  This '          // &
                                   'situation MAY be caused '        // &
                                   'by an error in a frame '         // &
                                   'kernel in which the frame '      // &
                                   'is defined. The problem '        // &
                                   'also could be indicative '       // &
                                   'of a SPICELIB bug.'              )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  i                           )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errint ( '#',  obs                         )
                     call ctx%sigerr ( 'SPICE(DEGENERATECASE)'           )
                     return

                  end if

!
!                 Find the observer-near point state in the target
!                 body-fixed frame.
!
                  stemp = stnear - stobs

!
!                 Transform the state to frame J2000.  To get the
!                 required transformation matrix, we'll need to obtain
!                 the epoch associated with CNMFRM.  Parse the
!                 aberration correction and adjust the frame evaluation
!                 epoch as needed.
!
                  call zzcorepc ( ctx, abcorr, t0, lt, fet )

!
!                 Obtain the matrix for transforming state vectors
!                 from the target center frame to the J2000 frame and
!                 apply it to the observer-to-near point state vector.
!
                  call frmchg ( ctx, cfrmid, j2000, fet, xipm )

                  if ( ctx%failed() ) return

                  call mxvg ( xipm, stemp, 6, 6, s2(1,i) )

!
!                 At this point, S2(*,I) contains position and
!                 velocity of the near point on the target as
!                 seen by the observer, relative to frame J2000.
!

               else if ( vecdef(i) == kvcons ) then
!
!                 The vector is constant in a specified frame.
!
!                 We need a 3-vector and an associated reference
!                 frame relative to which the vector is specified.
!
!                 Look up the ID of the frame first.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )

!
!                 Let FET ("frame ET") be the evaluation epoch for
!                 the constant vector's frame.  By default, this
!                 frame is just T0, but if we're using light time
!                 corrections, FET must be adjusted for one-way
!                 light time between the frame's center and the
!                 observer.
!
!                 Set the default value of FET here.
!
                  fet = t0

!
!                 Optionally, there is an aberration correction
!                 associated with the constant vector's frame.
!                 If so, an observer must be associated with the
!                 frame.  Look up the correction first.
!
                  call zzdynoac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      cvcorr,    fnd )

                  if ( .not. fnd ) then
                     cvcorr = 'NONE'
                  end if

                  call zzprscor ( ctx, cvcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( .not. corblk(geoidx) ) then
!
!                    We need to apply an aberration correction to
!                    the constant vector.
!
!                    Check for errors in the aberration correction
!                    specification.
!
!                       - Light time and stellar aberration corrections
!                         are mutually exclusive.
!
                     if ( corblk(ltidx) .and. corblk(stlidx) ) then

                        call ctx%setmsg ( 'Definition of frame # '       // &
                                      'specifies aberration '        // &
                                      'correction # for constant '   // &
                                      'vector.  Light time and '     // &
                                      'stellar aberration '          // &
                                      'corrections are mutually '    // &
                                      'exclusive for constant '      // &
                                      'vectors used in two-vector '  // &
                                      'parameterized dynamic frame ' // &
                                      'definitions.  This situation '// &
                                      'is usually caused by an '     // &
                                      'error in a frame kernel in '  // &
                                      'which the frame is defined.'  )
                        call ctx%errch  ( '#',  inname                   )
                        call ctx%errch  ( '#',  cvcorr                   )
                        call ctx%sigerr ( 'SPICE(INVALIDOPTION)'         )
                        return

                     end if

                     if ( corblk(ltidx) ) then
!
!                       Light time correction is used.  The epoch used
!                       to evaluate the constant vector's frame depends
!                       on the frame's observer and center.
!
!                       Look up the constant vector frame's center.
!
                        call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                        if ( .not. fnd ) then

                           call ctx%setmsg ( 'In definition of frame #, ' // &
                                         'the frame associated with ' // &
                                         'a constant vector has '     // &
                                         'frame ID code #, but no '   // &
                                         'frame center, frame class, '// &
                                         'or frame class ID was '     // &
                                         'found by FRINFO.  This '    // &
                                         'situation MAY be caused '   // &
                                         'by an error in a frame '    // &
                                         'kernel in which the frame ' // &
                                         'is defined. The problem '   // &
                                         'also could be indicative '  // &
                                         'of a SPICELIB bug.'         )
                           call ctx%errch  ( '#',  inname                 )
                           call ctx%errint ( '#',  frid                   )
                           call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                           return

                        end if
!
!                       If the constant vector frame is non-inertial,
!                       we'll need to compute the evaluation epoch for
!                       this frame.
!
                        if ( frcls /= inertl ) then
!
!                          Look up the observer associated with the
!                          constant vector's frame.  This observer,
!                          together with the frame's center, determines
!                          the evaluation epoch for the frame.
!
                           call zzdynbid ( ctx, inname,    infram, &
                                           itmobs(i), cvobs   )

!
!                          Obtain light time from the observer to the
!                          frame's center.
!
                           call spkezp ( ctx, frctr,  t0,   'J2000', &
                                           cvcorr, cvobs, ctrpos,  lt )

!
!                          Re-set the evaluation epoch for the frame.
!
                           call zzcorepc ( ctx, cvcorr, t0, lt, fet )

                           if ( ctx%failed() ) return

                        end if
!
!                       The constant vector frame's evaluation epoch
!                       FET has been set.
!

                     else if ( corblk(stlidx) ) then
!
!                       Stellar aberration case.
!
!                       The constant vector must be corrected for
!                       stellar aberration induced by the observer's
!                       velocity relative to the solar system
!                       barycenter.  First, find this velocity in
!                       the J2000 frame.  We'll apply the correction
!                       later, when the constant vector has been
!                       transformed to the J2000 frame.
!
                        call zzdynbid ( ctx, inname,    infram, &
                                        itmobs(i), cvobs   )

                        call spkssb ( ctx, cvobs, t0, 'J2000', stobs )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 At this point FET is the frame evaluation epoch
!                 for the frame associated with the constant vector.
!
!                 If stellar aberration correction has been specified,
!                 STOBS is the state of the observer relative to the
!                 solar system barycenter, expressed in the J2000
!                 frame.
!
!                 Get the constant vector specification.
!
                  call zzdynvac ( ctx, inname,  infram,  itmspc(i), &
                                  1,       n,       spec       )

                  if ( ctx%failed() ) return

                  call cmprss ( ' ', 0, spec, spec )
                  call ucase  ( spec,         spec )

                  if ( spec == kvrecc ) then
!
!                    The coordinate system is rectangular.
!
!                    Look up the constant vector.
!
                     call zzdynvad ( ctx, inname, infram, itmvec(i), &
                                     3,      n,      dirvec    )

                  else if (      ( spec == kvlatc ) &
                            .or. ( spec == kvradc ) ) then
!
!                    The coordinate system is latitudinal or RA/DEC.
!
!                    Look up the units associated with the angles.
!
                     call zzdynvac ( ctx, inname,  infram,  itmunt(i), &
                                     1,       n,       units     )

                     if ( spec == kvlatc ) then
!
!                       Look up longitude and latitude.
!
                        call zzdynvad ( ctx, inname, infram, itmlon(i), 1, n, lon_1 )
                        lon = lon_1(1)

                        call zzdynvad ( ctx, inname, infram, itmlat(i), 1, n, lat_1 )
                        lat = lat_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, lon, units, kvradn, angles(1) )
                        call convrt ( ctx, lat, units, kvradn, angles(2) )

                     else
!
!                       Look up RA and DEC.
!
                        call zzdynvad ( ctx, inname, infram, itmra(i), 1, n, ra_1 )
                        ra = ra_1(1)

                        call zzdynvad ( ctx, inname, infram, itmdec(i), 1, n, dec_1 )
                        dec = dec_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, ra,  units, kvradn, angles(1) )
                        call convrt ( ctx, dec, units, kvradn, angles(2) )

                     end if

                     if ( ctx%failed() ) return

!
!                    Now  produce a direction vector.
!
                     call latrec ( 1.0_wp, angles(1), angles(2), dirvec )

                  else

                     call ctx%setmsg ( 'Definition of two-vector '       // &
                                   'parameterized dynamic frame # '  // &
                                   'includes constant vector '       // &
                                   'specification #, which is '      // &
                                   'not supported.  This situation ' // &
                                   'is usually caused by an error '  // &
                                   'in a frame kernel in which '     // &
                                   'the frame is defined.'           )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errch  ( '#',  spec                        )
                     call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                     return

                  end if

!
!                 At this point, the Cartesian coordinates of the
!                 vector relative to the constant vector frame
!                 are stored in DIRVEC.
!
!                 Convert the direction vector to the J2000 frame.
!                 Fill in the state vector.  The velocity in the
!                 constant vector's frame is zero.
!
                  s2(1:3,i) = dirvec
                  s2(4:6,i) = 0.0_wp

                  if ( frid /= j2000 ) then

                     call frmchg ( ctx, frid, j2000, fet, xipm )

                     if ( ctx%failed() ) return

                     call mxvg ( xipm,  s2(1,i), 6, 6, stemp   )
                     s2(:,i) = stemp

                  end if

!
!                 The state of the constant vector is now represented
!                 in the J2000 frame, but we may still need to
!                 apply a stellar aberration correction.
!
                  if ( corblk(stlidx) ) then
!
!                    Perform the stellar aberration correction
!                    appropriate to the radiation travel sense.

                     if ( corblk(xmtidx) ) then
!
!                       The correction is for transmission.
!
                        call stlabx ( ctx, s2(1,i), stobs(4), stemp )

                     else
!
!                       The correction is for reception.
!
                        call stelab ( ctx, s2(1,i), stobs(4), stemp )

                     end if

                     if ( ctx%failed() ) return

!
!                    Update the position portion of S2(*,I).
!
                     s2(1:3,i) = stemp(1:3)

                  end if

!
!                 At this point, S2(*,I) contains position and velocity
!                 of the constant (constant relative to its associated
!                 frame, that is) vector as seen by the observer,
!                 relative to frame J2000.
!

               else

                  call ctx%setmsg ( 'Definition of two-vector '       // &
                                'parameterized dynamic frame # '  // &
                                'includes vector '                // &
                                'definition #, which is '         // &
                                'not supported.  This situation ' // &
                                'is usually caused by an error '  // &
                                'in a frame kernel in which '     // &
                                'the frame is defined.'           )
                  call ctx%errch  ( '#',  inname                      )
                  call ctx%errch  ( '#',  vecdef(i)                   )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                  return

               end if

!
!              Negate the state vector if the axis has negative sign.
!
               if ( negate ) then
                  stemp = -s2(:,i)
                  s2(:,i) = stemp
               end if

            end do

!
!           Look up the lower bound for the angular separation of
!           the defining vectors.  Use the default value if none
!           was supplied.
!
            call zzdynoad ( ctx, inname, infram, itmsep, 1, n, minsep_1, fnd )
            minsep = minsep_1(1)

            if ( ctx%failed() ) return

            if ( .not. fnd ) then
               minsep = lbsep
            end if

!
!           Now use our states to compute our state transformation
!           matrix.
!
!           Check the angular separation of the defining vectors. We
!           want to ensure that the vectors are not too close to being
!           linearly dependent.  We can handle both cases---separation
!           close to 0 or separation close to Pi---by comparing the
!           sine of the separation to the sine of the separation limit.
!
            sep = vsep ( s2(1,1), s2(1,2) )

            if ( sin(sep) < sin(minsep) ) then

               call etcal  ( ctx, t0, timstr )

               call ctx%setmsg ( 'Angular separation of vectors defining '// &
                             'two-vector parameterized dynamic frame '// &
                             '# is # (radians); minimum allowed diff' // &
                             'erence of separation from 0 or Pi is # '// &
                             'radians.  Evaluation epoch is #.  Extre'// &
                             'me loss of precision can occur when '   // &
                             'defining vectors are nearly linearly '  // &
                             'dependent.  This type of error can be ' // &
                             'due to using a dynamic frame outside of'// &
                             ' the time range for which it is meant. '// &
                             'It also can be due to a conceptual '    // &
                             'error pertaining to the frame''s defin' // &
                             'ition, or to an implementation error '  // &
                             'in the frame kernel containing the '    // &
                             'frame definition. However, if you wish '// &
                             'to proceed with this computation, '     // &
                             'the # keyword can be used in '          // &
                             'the frame definition to adjust the '    // &
                             'separation limit.'                      )
               call ctx%errch  ( '#',  inname                             )
               call ctx%errdp  ( '#',  sep                                )
               call ctx%errdp  ( '#',  minsep                             )
               call ctx%errch  ( '#',  timstr                             )
               call ctx%errch  ( '#',  kwatol                             )
               call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                  )
               return

            end if

!
!           We have both states expressed relative to frame J2000
!           at this point.  Find the transformation from INNAME to
!           the frame J2000, then from J2000 to frame BASNAM.
!
            call zztwovxf ( ctx, s2(1,1), axis(1), s2(1,2), axis(2), xform )

            if ( basfrm /= j2000 ) then

               xftemp = xform

               call frmchg ( ctx, j2000, basfrm,  t0, xf2000 )

               if ( ctx%failed() ) return

               call mxmg ( xf2000,  xftemp,  6,  6,  6, xform  )

            end if

!
!           This is the end of the work specific to two-vector frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kveulr ) then
!
!           The frame belongs to the Euler family.
!
!           We expect to see specifications of an axis sequence, units,
!           and angles via polynomial coefficients.  We also expect
!           to see an ET epoch.
!
!           Look up the epoch first.  Let DELTA represent the offset
!           of T0 relative to the epoch.
!
!           Initialize EPOCH so subtraction doesn't overflow if EPOCH
!           is invalid due to a lookup error.
!
            epoch = 0.0_wp

            call zzdynvad ( ctx, inname, infram, kwepoc, 1, n, epoch_1 )
            epoch = epoch_1(1)

            delta = t0 - epoch

!
!           Now the axis sequence.
!
            call zzdynvai ( ctx, inname, infram, kweuax, 3, n, iaxes )

!
!           Now the coefficients for the angles.
!
            do i = 1, 3
!
!              Initialize N so subtraction doesn't overflow if N
!              is invalid due to a lookup error.
!
               n = 0

               call zzdynvad ( ctx, inname,  infram,  itmcof(i), &
                               maxcof,  n,       coeffs(1,i) )

!
!              Set the polynomial degree for the Ith angle.
!
               degs(i) = n - 1

            end do

!
!           Look up the units associated with the angles.
!
            call zzdynvac ( ctx, inname, infram, kwunit, 1, n, units )

            if ( ctx%failed() ) return

!
!           Evaluate the angles and their derivatives at DELTA.  Convert
!           angles from input units to radians and radians/sec.
!
            do i = 1, 3

               call polyds ( coeffs(1,i), degs(i), 1, delta, poly )
!
!              Convert units.  Fill in the Euler angle state vector.
!
               call convrt ( ctx, poly(0), units, kvradn, eulang(i  ) )
               call convrt ( ctx, poly(1), units, kvradn, eulang(i+3) )

            end do

!
!           Produce a state transformation matrix that maps from
!           the defined frame to the base frame.
!
            call eul2xf ( ctx, eulang, iaxes(1), iaxes(2), iaxes(3), xform )

!
!           This is the end of the work specific to Euler frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kvprod ) then
!
!           The frame belongs to the product family.
!
!           We expect to see specifications of "from" and "to" frames
!           that make up the product.
!
!           Look up the "from" and "to" frames that define the product.
!
            call dynamic_frame_strings ( ctx, inname, infram, kwffrm, mxnfac, m, fframs )
            call dynamic_frame_strings ( ctx, inname, infram, kwtfrm, mxnfac, n, tframs )

            if ( ctx%failed() ) return

            if ( n /= m ) then

               call ctx%setmsg ( 'Definition of product ' &
               //            'parameterized dynamic frame # ' &
               //            'has # "from" frames and # "to" ' &
               //            'frames. These counts must match.' )
               call ctx%errch  ( '#',  inname                       )
               call ctx%errint ( '#',  m                            )
               call ctx%errint ( '#',  n                            )
               call ctx%sigerr ( 'SPICE(BADFRAMECOUNT)'             )
               return

            end if

!
!           The product of the factors is the transformation from the
!           base frame to the output frame. We need the inverse of
!           this transformation.
!
!           Compute the inverse of the product of the factors; place
!           the result in XFORM.
!
            call namfrm ( ctx, fframs(1)%s, fromid )
            call namfrm ( ctx, tframs(1)%s, toid   )

            call frmchg ( ctx, toid, fromid, t0, xform )

            if ( ctx%failed() ) return

            do i = 2, n

               xftemp = xform

               call namfrm ( ctx, fframs(i)%s, fromid )
               call namfrm ( ctx, tframs(i)%s, toid   )

               call frmchg ( ctx, toid, fromid, t0, xform )

               if ( ctx%failed() ) return

               call mxmg  ( xform, xftemp, 6, 6, 6, xout  )
               xform = xout

            end do

         else

            call ctx%setmsg ( 'Dynamic frame family # (in definition of '// &
                          'frame #) is not supported. '              // &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  inname                               )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            return

         end if
!
!        This is the end of the IF block that processes the
!        parameterized dynamic frame families.
!

      else

         call ctx%setmsg ( 'Dynamic frame style # (in definition of ' // &
                       'frame #) is not supported. '              // &
                       'This situation is usually caused by '     // &
                       'an error in a frame kernel in which '     // &
                       'the frame is defined.'                    )
         call ctx%errch  ( '#',  dynstl                               )
         call ctx%errch  ( '#',  inname                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

      if ( ctx%failed() ) return

!
!     At this point XFORM is the state transformation matrix mapping
!     from the input frame INFRAM to the base frame BASFRM.
!
!     If the frame has rotation state 'INERTIAL', the frame must have
!     zero derivative with respect to any inertial frame. Set the
!     derivative block accordingly.
!
      if ( inert ) then
!
!        See whether the base frame is inertial.
!
         call irfnum ( ctx, basnam, j )

         if ( j > 0 ) then
!
!           The base frame is  a recognized inertial frame.  Zero
!           out the derivative block.
!
            do i = 1, 3
               xform(4:6,i) = 0.0_wp
            end do

         else
!
!           The base frame is *not* a recognized inertial frame.
!
!           Create the state transformation matrix that maps from the
!           defined frame to J2000.  Zero out the derivative block of
!           this matrix.  Convert the resulting matrix to the state
!           transformation from the defined frame to the output frame.
!
            call frmchg ( ctx, basfrm, j2000, t0, xf2000 )

            if ( ctx%failed() ) return

            call mxmg ( xf2000, xform, 6, 6, 6, xftemp )

            do i = 1, 3
               xftemp(4:6,i) = 0.0_wp
            end do

!
!           XFTEMP now represents the transformation from a
!           constant frame matching the defined frame at T0 to the
!           J2000 frame. Produce the transformation from this constant
!           frame to the output frame.
!
!           To avoid introducing additional round-off error into
!           the rotation blocks of XFORM, we overwrite only the
!           derivative block of XFORM with the derivative block
!           of the "inertial" transformation.
!
            call invstm ( ctx, xf2000, xfinv )
            call mxmg   ( xfinv,  xftemp, 6, 6, 6, xout )

            do i = 1, 3
               xform(4:6,i) = xout(4:6,i)
            end do

         end if

      end if

!
!     If the frame is frozen, zero out the derivative block of the
!     transformation matrix.
!
      if ( frozen ) then

         do i = 1, 3
            xform(4:6,i) = 0.0_wp
         end do

      end if

!
!     XFORM and BASFRM are set.
!

      return
      end associate
      end subroutine dynamic_frame_xform

      !> PEPPER: the evaluation of ZZDYNROT (SPICELIB's body of the
      !> routine; see zzdynrot).
      recursive subroutine dynamic_frame_rotation ( ctx, infram, center, et, rotate, basfrm )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: latrec, mxm, mxv, polyds, vhat, vsep
      use pepper_strings, only: cmprss, eqstr, intstr, ucase
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzabcorr
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: infram  !! the dynamic frame
      integer(ip), intent(in) :: center  !! ID code for the center of the input frame.
      real(wp), intent(in) :: et  !! An epoch in seconds past J2000 TDB.
      real(wp), intent(out) :: rotate ( 3, 3 )  !! The requested rotation matrix.
      integer(ip), intent(out) :: basfrm  !! Frame ID of base frame associated with INFRAM.
      real(wp) :: epoch_1(1)  !! one-element copy of epoch (array argument)
      real(wp) :: minsep_1(1)  !! one-element copy of minsep (array argument)
      real(wp) :: dec_1(1)  !! one-element copy of dec (array argument)
      real(wp) :: ra_1(1)  !! one-element copy of ra (array argument)
      real(wp) :: lat_1(1)  !! one-element copy of lat (array argument)
      real(wp) :: lon_1(1)  !! one-element copy of lon (array argument)
      real(wp) :: t0_1(1)  !! one-element copy of t0 (array argument)
      character(*), parameter :: rname = 'ZZDYNROT'
      integer(ip), parameter :: timlen = 50
      integer(ip), parameter :: vnamln = 4
      character(corlen)     abcorr
      character(kvlen)      axname
      character(:), allocatable :: basnam
      character(:), allocatable :: cfrmnm
      character(:), allocatable :: ctrnam
      logical :: namfnd
      character(corlen)     cvcorr
      character(kvlen)      dynstl
      character(kvlen)      dynfam
      type(string), allocatable :: fframs(:)
      character(:), allocatable :: inname
      character(kvlen)      nutmod
      character(kvlen)      oblmod
      character(kvlen)      prcmod
      character(kvlen)      rotsta
      character(kvlen)      spec
      type(string), allocatable :: tframs(:)
      character(timlen)     timstr
      character(kvlen)      tmpfam
      character(kvlen)      units
      character(kvlen)      vecdef ( 2 )
      character(:), allocatable :: velfrm
      character(vnamln), parameter :: vname  ( 2 ) = &
         [ character(vnamln) :: kwpri, kwsec ]
      real(wp)              alt
      real(wp)              angles ( 2 )
      real(wp)              coeffs ( maxcof, 3 )
      real(wp)              ctrpos ( 3 )
      real(wp)              dec
      real(wp)              delta
      real(wp)              dirvec ( 3 )
      real(wp)              dmob
      real(wp)              epoch
      real(wp)              eulang ( 3 )
      real(wp)              fet
      real(wp)              lat
      real(wp)              lon
      real(wp)              lt
      real(wp)              minsep
      real(wp)              mob
      real(wp)              nutxf  ( 6, 6 )
      real(wp)              oblr   ( 3, 3 )
      real(wp)              pnear  ( 3 )
      real(wp)              pobs   ( 3 )
      real(wp)              poly   ( 0 : 1 )
      real(wp)              precxf ( 6, 6 )
      real(wp)              ptemp  ( 3 )
      real(wp)              r2000  ( 3, 3 )
      real(wp)              ra
      real(wp)              radii  ( 3 )
      real(wp)              rinv   ( 3, 3 )
      real(wp)              rnut   ( 3, 3 )
      real(wp)              rout   ( 3, 3 )
      real(wp)              rprec  ( 3, 3 )
      real(wp)              rtemp  ( 3, 3 )
      real(wp)              sep
      real(wp)              stemp  ( 6 )
      real(wp)              stobs  ( 6 )
      real(wp)              tipm   ( 3, 3 )
      real(wp)              t0
      real(wp)              v2     ( 3, 2 )
      real(wp)              vet
      real(wp)              vflt
      integer(ip)           axis   ( 2 )
      integer(ip)           cfrmid
      integer(ip)           cvobs
      integer(ip)           degs   ( 3 )
      integer(ip)           frcls
      integer(ip)           frctr
      integer(ip)           frcid
      integer(ip)           frid
      integer(ip)           fromid
      integer(ip)           i
      integer(ip)           iaxes  ( 3 )
      integer(ip)           m
      integer(ip)           n
      integer(ip)           obs
      integer(ip)           targ
      integer(ip)           toid
      logical               corblk ( nabcor )
      logical               fnd
      logical               frozen
      logical               meanec
      logical               meaneq
      logical               negate
      logical               ofdate
      logical               trueeq
      if ( .not. allocated ( ctx%zzdynrot_state ) ) allocate ( ctx%zzdynrot_state )
      associate ( axes => ctx%zzdynrot_state%axes, &
                  earth => ctx%zzdynrot_state%earth, &
                  first => ctx%zzdynrot_state%first, &
                  j2000 => ctx%zzdynrot_state%j2000, &
                  itmabc => ctx%zzdynrot_state%itmabc, &
                  itmaxe => ctx%zzdynrot_state%itmaxe, &
                  itmcof => ctx%zzdynrot_state%itmcof, &
                  itmdec => ctx%zzdynrot_state%itmdec, &
                  itmfrm => ctx%zzdynrot_state%itmfrm, &
                  itmlat => ctx%zzdynrot_state%itmlat, &
                  itmlon => ctx%zzdynrot_state%itmlon, &
                  itmobs => ctx%zzdynrot_state%itmobs, &
                  itmra => ctx%zzdynrot_state%itmra, &
                  itmsep => ctx%zzdynrot_state%itmsep, &
                  itmspc => ctx%zzdynrot_state%itmspc, &
                  itmtrg => ctx%zzdynrot_state%itmtrg, &
                  itmunt => ctx%zzdynrot_state%itmunt, &
                  itmvdf => ctx%zzdynrot_state%itmvdf, &
                  itmvec => ctx%zzdynrot_state%itmvec )
      if ( ctx%failed() ) return

      if ( first ) then
!
!        Get the ID code for the J2000 frame.
!
         call irfnum ( ctx, 'J2000', j2000 )

!
!        Get the ID code for the earth (we needn't check the found
!        flag).
!
         call bodn2c ( ctx, 'EARTH', earth, fnd )

!
!        Initialize "item" strings used to create kernel variable
!        names.
!
         do i = 1, 2
!
!           Vector axis:
!
            itmaxe(i) = vname(i) // kwvaxi
!
!           Vector definition:
!
            itmvdf(i) = vname(i) // kwvcdf
!
!           Vector aberration correction:
!
            itmabc(i) = vname(i) // kwvabc
!
!           Vector frame:
!
            itmfrm(i) = vname(i) // kwvfrm
!
!           Vector observer:
!
            itmobs(i) = vname(i) // kwvobs
!
!           Vector target:
!
            itmtrg(i) = vname(i) // kwvtrg
!
!           Vector longitude:
!
            itmlon(i) = vname(i) // kwlon
!
!           Vector latitude:
!
            itmlat(i) = vname(i) // kwlat
!
!           Vector right ascension:
!
            itmra(i)  = vname(i) // kwra
!
!           Vector declination:
!
            itmdec(i) = vname(i) // kwdec
!
!           Vector units:
!
            itmunt(i) = vname(i) // kwunit
!
!           Constant vector coordinate specification:
!
            itmspc(i) = vname(i) // kwvspc
!
!           Constant vector in cartesian coordinates, literal value:
!
            itmvec(i) = vname(i) // kwvect

         end do

         first = .false.

      end if

!
!     Initialize the output arguments.
!
      rotate = 0.0_wp

      basfrm = 0

!
!     Initialize certain variables to ensure that we don't do
!     arithmetic operations using bogus, possibly large,
!     undefined values.
!
      nutxf = 0.0_wp
      oblr = 0.0_wp
      precxf = 0.0_wp
      r2000 = 0.0_wp
      rtemp = 0.0_wp
      rinv = 0.0_wp
      tipm = 0.0_wp

      mob    = 0.0_wp
      dmob   = 0.0_wp
      t0     = 0.0_wp
      frozen = .false.

!
!     Get the input frame name.
!
      call frame_name_of ( ctx, infram, inname, namfnd )

!
!     We need the name of the base frame.
!
      call zzdynfid ( ctx, inname, infram, kwbfrm, basfrm )

      if ( ctx%failed() ) return

      call frame_name_of ( ctx, basfrm, basnam, namfnd )

!
!     The output frame code and name are set.
!
!     Look up the dynamic frame definition style from the kernel pool.
!     The kernel variable's name might be specified by name or ID.
!
      call zzdynvac ( ctx, inname, infram, kwstyl, 1, n, dynstl )

      if ( ctx%failed() ) return

!
!     At this time, the only supported dynamic frame definition style is
!     PARAMETERIZED.
!
      if (  eqstr( dynstl, kvparm )  ) then
!
!        Parameterized dynamic frames belong to families.  Look up
!        the family for this frame.
!
         call zzdynvac ( ctx, inname, infram, kwffam, 1, n, dynfam )

         if ( ctx%failed() ) return

         call cmprss ( ' ', 0, dynfam, tmpfam )
         call ucase  ( tmpfam,         dynfam )

!
!        Determine whether we have an "of-date" frame family.
!        The logical flags used here and respective meanings are:
!
!           MEANEQ   Mean equator and equinox of date
!           TRUEEQ   True equator and equinox of date
!           MEANEC   Mean ecliptic and equinox of date
!
         meaneq  =  dynfam == kvmeqt
         trueeq  =  dynfam == kvteqt
         meanec  =  dynfam == kvmecl

         ofdate  =  meaneq .or. meanec .or. trueeq

!
!        Set the evaluation epoch T0.  Normally this epoch is ET,
!        but if the frame is frozen, the freeze epoch from the
!        frame definition is used.
!
!        Read the freeze epoch into T0 if a freeze epoch was
!        specified; let FROZEN receive the FOUND flag value
!        returned by ZZDYNOAD.
!
         call zzdynoad ( ctx, inname, infram, kwfrez, 1, n, t0_1, frozen )
         t0 = t0_1(1)

         if ( ctx%failed() ) return

         if ( .not. frozen ) then
!
!           Normal case:  just use the input epoch.
!
            t0 = et

         end if

!
!        Look up the rotation state keyword.  In this routine,
!        the rotation state keyword is examined only to support
!        semantic checking:  there's no use made of the fact that
!        the rotation state is 'ROTATING' or 'INERTIAL'.
!
         call zzdynoac ( ctx, inname, infram, kwrsta, 1, n, rotsta, fnd )

         if ( ctx%failed() ) return

         if ( fnd ) then
!
!           Catch invalid rotation states here.
!
            if (      ( .not. eqstr( rotsta, kvrotg ) ) &
                .and. ( .not. eqstr( rotsta, kvinrt ) )  ) then

               call ctx%setmsg ( 'Definition of frame # contains '     // &
                             '# specification #. The only valid '  // &
                             'rotation states are # or #. '        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  kwrsta                          )
               call ctx%errch  ( '#',  rotsta                          )
               call ctx%errch  ( '#',  kvrotg                          )
               call ctx%errch  ( '#',  kvinrt                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

         end if
!
!        If the frame is frozen, the rotation state keyword *must be
!        absent*.
!
         if ( frozen .and. fnd ) then

            call ctx%setmsg ( 'Definition of frame # contains both ' // &
                          '# and # '                             // &
                          'keywords; at most one of these must ' // &
                          'be present in the frame definition. ' // &
                          'This situation is usually caused by ' // &
                          'an error in a frame kernel in which ' // &
                          'the frame is defined.'                )
            call ctx%errch  ( '#',  inname                           )
            call ctx%errch  ( '#',  kwfrez                           )
            call ctx%errch  ( '#',  kwrsta                           )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                 )
            return

         end if

!
!        If the frame belongs to an "of date" family, either the
!        rotation state must be specified or the frame must be
!        frozen.
!
         if (  ofdate  .and.  (.not. frozen) .and. (.not. fnd )  ) then

            call ctx%setmsg ( 'Definition of frame #, which belongs to ' // &
                          'parameterized dynamic frame family #, '   // &
                          'contains neither # nor # keywords; '      // &
                          'frames in this family require exactly '   // &
                          'one of these in their frame definitions. '// &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  inname                               )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  kwfrez                               )
            call ctx%errch  ( '#',  kwrsta                               )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                     )
            return

         end if

!
!        The evaluation epoch T0 is set.
!
!        In this routine, unlike its companion ZZDYNFRM, there is no
!        need to make further reference to the rotation state.  Hence
!        the flag INERT used in ZZDYNFRM doesn't appear here.
!
!        The following code block performs actions specific to
!        the various dynamic frame families.
!
         if ( ofdate ) then
!
!           Fetch the name of the true equator and equinox of date
!           precession model.
!
            call zzdynvac ( ctx, inname, infram, kwprcm, 1, n, prcmod )

            if ( ctx%failed() ) return

!
!           Get the precession transformation.
!
            if (  eqstr( prcmod, kvm001 )  ) then
!
!              This is the 1976 IAU earth precession model.
!
!              Make sure the center of the input frame is the earth.
!
               if ( center /= earth ) then

                  call body_name_of ( ctx, center, ctrnam, fnd )

                  if ( .not. fnd ) then
                     ctrnam = repeat ( ' ', 11 )
                     call intstr ( center, ctrnam )
                     ctrnam = trim ( ctrnam )
                  end if

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'frame center # and precession '      // &
                                'model #. This precession model '     // &
                                'is not applicable to body #. '       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%errch  ( '#',  kvm001                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'             )
                  return

               end if

!
!              Look up the precession transformation.  Extract
!              the precession rotation matrix.
!
               call zzeprc76 ( ctx, t0, precxf )

               rprec(:,1) = precxf(1:3,1)
               rprec(:,2) = precxf(1:3,2)
               rprec(:,3) = precxf(1:3,3)

!
!              If we're in the mean-of-date case, invert this
!              transformation to obtain the mapping from the
!              mean-of-date frame to J2000.
!
               if ( meaneq ) then
                  rtemp = transpose(rprec)
               end if

            else

               call ctx%setmsg ( 'Definition of frame # specifies '    // &
                             'precession model #, which is not '   // &
                             'recognized. '                        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  prcmod                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

!
!           At this point the precession transformation REPREC is set.
!           If INFRAM is a mean equator and equinox of date frame, the
!           inverse of REPREC is currently stored in RTEMP.

            if ( trueeq ) then
!
!              We need a nutation transformation as well. Get the name
!              of the nutation model.
!
               call zzdynvac ( ctx, inname, infram, kwnutm, 1, n, nutmod )

               if ( ctx%failed() ) return

!
!              Get the nutation transformation.
!
               if (  eqstr( nutmod, kvm002 )  ) then
!
!                 This is the 1980 IAU earth nutation model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call body_name_of ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        ctrnam = repeat ( ' ', 11 )
                        call intstr ( center, ctrnam )
                        ctrnam = trim ( ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and nutation '     // &
                                   'model #. This '                   // &
                                   'nutation model is not applicable '// &
                                   'to body #.  This situation is '   // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm002                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Look up the nutation transformation.  Extract
!                 the nutation rotation matrix.
!
                  call zzenut80 ( ctx, t0, nutxf )

                  rnut(:,1) = nutxf(1:3,1)
                  rnut(:,2) = nutxf(1:3,2)
                  rnut(:,3) = nutxf(1:3,3)

!
!                 Find the rotation from the J2000 frame to the earth
!                 true of date frame.  Invert.
!
                  call mxm   ( rnut, rprec, rinv )
                  rtemp = transpose(rinv)

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'nutation model #, which is not '     // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  nutmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            else if ( meanec ) then
!
!              We need a mean obliquity transformation as well.
!              Get the name of the obliquity model.
!
               call zzdynvac ( ctx, inname, infram, kwobqm, 1, n, oblmod )

               if ( ctx%failed() ) return

!
!              Get the obliquity transformation.
!
               if (  eqstr( oblmod, kvm003 )  ) then
!
!                 This is the 1980 IAU earth mean obliquity of
!                 date model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call body_name_of ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        ctrnam = repeat ( ' ', 11 )
                        call intstr ( center, ctrnam )
                        ctrnam = trim ( ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and obliquity '    // &
                                   'model #.  This obliquity model '  // &
                                   'is not applicable to body #. '    // &
                                   'This situation is '               // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm003                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Create the obliquity transformation.
!                 First look up the obliquity state.
!
                  call zzmobliq ( ctx, t0, mob, dmob )

!
!                 The obliquity rotation is about the mean-of-date
!                 x-axis.  The other Euler angles are identically
!                 zero; the axes are arbitrary, as long as the
!                 middle axis is distinct from the other two.
!
                  call eul2m ( ctx, 0.0_wp, 0.0_wp, mob, 1, 3, 1, oblr )

!
!                 Find the rotation from the J2000 to the
!                 earth mean ecliptic of date frame.  Invert.
!
                  call mxm   ( oblr, rprec, rinv )
                  rtemp = transpose(rinv)

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'obliquity model #, which is not '    // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  oblmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

!
!           At this point, RTEMP contains the rotation from the
!           specified mean of date or true of date frame to J2000.
!
!           If the base frame is not J2000, we must find the
!           transformation from J2000 to the base frame.

            if ( basfrm /= j2000 ) then

               call refchg ( ctx, j2000,  basfrm, t0,  r2000  )
               call mxm      ( r2000,  rtemp,       rotate )

            else
!
!              Otherwise, RTEMP is the matrix we want.
!
               rotate = rtemp

            end if
!
!           Now ROTATE is the state transformation mapping from
!           the input frame INFRAM to the base frame BASFRM.
!
!           This is the end of the work specific to "of-date" frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kv2vec ) then
!
!           The frame belongs to the TWO-VECTOR family.
!
!           Fetch the specifications of the primary and secondary
!           axes.
!
            v2 = 0.0_wp

            do i = 1, 2
!
!              Get the name of the axis associated with the Ith
!              defining vector.
!
               call zzdynvac ( ctx, inname, infram, itmaxe(i), 1, n, axname )

               if ( ctx%failed() ) return

               call cmprss ( ' ', 0, axname, axname )
               call ucase  ( axname,         axname )

!
!              Set the sign flag associated with the axis.
!
               negate  =  axname(1:1) == '-'

               call cmprss ( '-', 0, axname, axname )
               call cmprss ( '+', 0, axname, axname )

               axis(i) = findloc(axes, axname, dim=1)

               if ( axis(i) == 0 ) then

                  call ctx%setmsg ( 'Definition of frame # associates '   // &
                                'vector # with axis #.  The only '    // &
                                'valid axis values are { X, -X, Y, '  // &
                                '-Y, Z, -Z }. '                       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errint ( '#',  i                               )
                  call ctx%errch  ( '#',  axname                          )
                  call ctx%sigerr ( 'SPICE(INVALIDAXIS)'                  )
                  return

               end if

!
!              Find out how the vector is defined:
!
!                 - Observer-target position vector
!                 - Observer-target velocity vector
!                 - Observer-target near point vector
!                 - Constant vector
!
!              VECDEF(I) indicates the vector definition method
!              for the Ith vector.
!
               call zzdynvac ( ctx, inname,  infram,  itmvdf(i), &
                               1,       n,       vecdef(i)   )

               if ( ctx%failed() ) return

               call cmprss ( ' ',    0, vecdef(i), vecdef(i) )
               call ucase  ( vecdef(i),            vecdef(i) )

               if (  vecdef(i) == kvposv  ) then
!
!                 The vector is the position of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 Look up the Ith position vector in the J2000 frame.
!
                  call spkezp ( ctx, targ,    t0,   'J2000', &
                                  abcorr,  obs,  v2(1,i),  lt )

                  if ( ctx%failed() ) return

!
!                 At this point, V2(*,I) contains position relative to
!                 frame J2000.
!

               else if ( vecdef(i) == kvvelv ) then
!
!                 The vector is the velocity of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 We need to know the frame in which the velocity is
!                 defined.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )
                  call frame_name_of ( ctx, frid, velfrm, namfnd )

!
!                 Look up the Ith velocity vector in the velocity frame.
!
                  call spkez ( ctx, targ,    t0,   velfrm, &
                                  abcorr,  obs,  stemp,  lt )

                  if ( ctx%failed() ) return

!
!                 We'll work with the unit velocity vector.
!
                  call vhat ( stemp(4), v2(1,i) )

!
!                 We need the epoch VET at which VELFRM is evaluated.
!                 This epoch will be used to transform the velocity
!                 vector from VELFRM to J2000.
!
!                 Set the default value of VET here.
!
                  vet = t0

!
!                 Parse the aberration correction.  Capture the
!                 epoch used to evaluate the velocity vector's frame.
!
                  call zzvalcor ( ctx, abcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( corblk(ltidx) ) then
!
!                    Light time correction is used.  The epoch used
!                    to evaluate the velocity vector's frame depends
!                    on the frame's observer and center.
!
!                    Look up the velocity frame's center.
!
                     call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                     if ( .not. fnd ) then

                        call ctx%setmsg ( 'In definition of frame #, ' // &
                                      'the frame associated with ' // &
                                      'a velocity vector has '     // &
                                      'frame ID code #, but no '   // &
                                      'frame center, frame class, '// &
                                      'or frame class ID was '     // &
                                      'found by FRINFO.  This '    // &
                                      'situation MAY be caused '   // &
                                      'by an error in a frame '    // &
                                      'kernel in which the frame ' // &
                                      'is defined. The problem '   // &
                                      'also could be indicative '  // &
                                      'of a SPICELIB bug.'         )
                        call ctx%errch  ( '#',  inname                 )
                        call ctx%errint ( '#',  frid                   )
                        call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                        return

                     end if

                     if ( frcls /= inertl ) then
!
!                       Obtain light time from the observer to the
!                       frame's center.
!
                        call spkezp ( ctx, frctr,  t0,  'J2000', &
                                        abcorr, obs, ctrpos,  vflt )
                        call zzcorepc ( ctx, abcorr, t0,  vflt,    vet  )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 The velocity frame evaluation epoch VET is now set.
!
!                 We must rotate the velocity vector from the velocity
!                 frame (evaluated at VET) to the output frame at T0.
!                 We'll do this in two stages, first mapping velocity
!                 into the J2000 frame.
!
                  if ( frid /= j2000 ) then

                     call refchg ( ctx, frid,   j2000,  vet,      r2000 )

                     if ( ctx%failed() ) return

                     call mxv      ( r2000,          v2(1,i),  ptemp )
                     v2(:,i) = ptemp

                  end if

!
!                 At this point, V2(*,I) contains velocity
!                 relative to frame J2000.

               else if ( vecdef(i) == kvnear ) then
!
!                 The vector points from an observer to the near
!                 point to the observer on the target body.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 The vector points from an observer to the
!                 sub-observer point (nearest point to the observer) on
!                 the target body.  We need the position of the near
!                 point relative to the observer.
!
!                 We'll look up the position of the target center
!                 relative to the observer, as well as the position of
!                 the near point relative to the target center, both in
!                 the body-fixed frame associated with the target.
!
!                 Look up the body-fixed frame associated with the
!                 target body.
!
                  call frame_of_center ( ctx, targ, cfrmid, cfrmnm, fnd )

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'Definition of frame # requires ' // &
                                   'definition of body-fixed frame ' // &
                                   'associated with target body #. ' // &
                                   'A call to CIDFRM indicated no '  // &
                                   'body-fixed frame is associated ' // &
                                   'with the target body.  This '    // &
                                   'situation can arise when a '     // &
                                   'frame kernel defining the '      // &
                                   'target''s body-fixed frame  '    // &
                                   'lacks the OBJECT_<ID>_FRAME '    // &
                                   'or OBJECT_<name>_FRAME '         // &
                                   'keywords.  The problem also '    // &
                                   'could be caused by an '          // &
                                   'error in a frame kernel in '     // &
                                   'which the parameterized '        // &
                                   'two-vector dynamic frame # '     // &
                                   'is defined.'                     )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     return

                  end if

!
!                 Get the radii of the target body.
!
                  call zzgftreb ( ctx, targ, radii )

                  if ( ctx%failed() ) return

!
!                 Look up the Ith position vector in the target-fixed
!                 frame.  Negate the vector to obtain the target-to-
!                 observer vector.
!
                  call spkezp ( ctx, targ,   t0,   cfrmnm, &
                                  abcorr, obs,  ptemp,   lt )

!
!                 We check FAILED() here because VMINUS is a simple
!                 arithmetic routine that doesn't return on entry.
!
                  if ( ctx%failed() ) return

                  pobs = -ptemp

                  call nearpt   ( ctx, pobs,   radii(1), radii(2), radii(3), &
                                  pnear,  alt                          )

                  if ( ctx%failed() ) return

!
!                 Find the observer-near point vector in the current
!                 frame CFRMNM.
!
                  ptemp = pnear - pobs

!
!                 Rotate the vector to frame J2000.  To get the required
!                 rotation matrix, we'll need to obtain the epoch
!                 associated with CNMFRM.  Parse the aberration
!                 correction and adjust the frame evaluation epoch as
!                 needed.
!
                  call zzcorepc ( ctx, abcorr, t0, lt, fet )

!
!                 Obtain the matrix for transforming position vectors
!                 from the target center frame to the J2000 frame and
!                 apply it to the observer-to-near point position
!                 vector.
!
                  call refchg ( ctx, cfrmid, j2000, fet, tipm )

                  if ( ctx%failed() ) return

                  call mxv ( tipm, ptemp, v2(1,i) )

!
!                 At this point, V2(*,I) contains position of the near
!                 point on the target as seen by the observer, relative
!                 to frame J2000.
!

               else if ( vecdef(i) == kvcons ) then
!
!                 The vector is constant in a specified frame.
!
!                 We need a 3-vector and an associated reference
!                 frame relative to which the vector is specified.
!
!                 Look up the ID of the frame first.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )

!
!                 Let FET ("frame ET") be the evaluation epoch for
!                 the constant vector's frame.  By default, this
!                 frame is just T0, but if we're using light time
!                 corrections, FET must be adjusted for one-way
!                 light time between the frame's center and the
!                 observer.
!
!                 Set the default value of FET here.
!
                  fet = t0

!
!                 Optionally, there is an aberration correction
!                 associated with the constant vector's frame.
!                 If so, an observer must be associated with the
!                 frame.  Look up the correction first.
!
                  call zzdynoac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      cvcorr,    fnd )

                  if ( ctx%failed() ) return

                  if ( .not. fnd ) then
                     cvcorr = 'NONE'
                  end if

                  call zzprscor ( ctx, cvcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( .not. corblk(geoidx) ) then
!
!                    We need to apply an aberration correction to
!                    the constant vector.
!
!                    Check for errors in the aberration correction
!                    specification.
!
!                       - Light time and stellar aberration corrections
!                         are mutually exclusive.
!
                     if ( corblk(ltidx) .and. corblk(stlidx) ) then

                        call ctx%setmsg ( 'Definition of frame # '       // &
                                      'specifies aberration '        // &
                                      'correction # for constant '   // &
                                      'vector.  Light time and '     // &
                                      'stellar aberration '          // &
                                      'corrections are mutually '    // &
                                      'exclusive for constant '      // &
                                      'vectors used in two-vector '  // &
                                      'parameterized dynamic frame ' // &
                                      'definitions.  This situation '// &
                                      'is usually caused by an '     // &
                                      'error in a frame kernel in '  // &
                                      'which the frame is defined.'  )
                        call ctx%errch  ( '#',  inname                   )
                        call ctx%errch  ( '#',  cvcorr                   )
                        call ctx%sigerr ( 'SPICE(INVALIDOPTION)'         )
                        return

                     end if

                     if ( corblk(ltidx) ) then
!
!                       Light time correction is used.  The epoch used
!                       to evaluate the constant vector's frame depends
!                       on the frame's observer and center.
!
!                       Look up the constant vector frame's center.
!
                        call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                        if ( .not. fnd ) then

                           call ctx%setmsg ( 'In definition of frame #, ' // &
                                         'the frame associated with ' // &
                                         'a constant vector has '     // &
                                         'frame ID code #, but no '   // &
                                         'frame center, frame class, '// &
                                         'or frame class ID was '     // &
                                         'found by FRINFO.  This '    // &
                                         'situation MAY be caused '   // &
                                         'by an error in a frame '    // &
                                         'kernel in which the frame ' // &
                                         'is defined. The problem '   // &
                                         'also could be indicative '  // &
                                         'of a SPICELIB bug.'         )
                           call ctx%errch  ( '#',  inname                 )
                           call ctx%errint ( '#',  frid                   )
                           call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                           return

                        end if

                        if ( frcls /= inertl ) then
!
!                          Look up the observer associated with the
!                          constant vector's frame.  This observer,
!                          together with the frame's center, determines
!                          the evaluation epoch for the frame.
!
                           call zzdynbid ( ctx, inname,    infram, &
                                           itmobs(i), cvobs   )

!
!                          Obtain light time from the observer to the
!                          frame's center.
!
                           call spkezp ( ctx, frctr,  t0,   'J2000', &
                                           cvcorr, cvobs, ctrpos,  lt )

!
!                          Find the evaluation epoch for the frame.
!
                           call zzcorepc ( ctx, cvcorr, t0, lt, fet )

                           if ( ctx%failed() ) return

                        end if

                     else if ( corblk(stlidx) ) then
!
!                       Stellar aberration case.
!
!                       The constant vector must be corrected for
!                       stellar aberration induced by the observer's
!                       velocity relative to the solar system
!                       barycenter.  First, find this velocity in
!                       the J2000 frame.  We'll apply the correction
!                       later, when the constant vector has been
!                       transformed to the J2000 frame.
!
                        call zzdynbid ( ctx, inname,    infram, &
                                        itmobs(i), cvobs   )

                        call spkssb ( ctx, cvobs, t0, 'J2000', stobs )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 Get the constant vector specification.
!
                  call zzdynvac ( ctx, inname,  infram,  itmspc(i), &
                                  1,       n,       spec       )

                  if ( ctx%failed() ) return

                  call cmprss ( ' ', 0, spec, spec )
                  call ucase  ( spec,         spec )

                  if ( spec == kvrecc ) then
!
!                    The coordinate system is rectangular.
!
!                    Look up the constant vector.
!
                     call zzdynvad ( ctx, inname, infram, itmvec(i), &
                                     3,      n,      dirvec    )

                  else if (      ( spec == kvlatc ) &
                            .or. ( spec == kvradc ) ) then
!
!                    The coordinate system is latitudinal or RA/DEC.
!
!                    Look up the units associated with the angles.
!
                     call zzdynvac ( ctx, inname,  infram,  itmunt(i), &
                                     1,       n,       units     )

                     if ( spec == kvlatc ) then
!
!                       Look up longitude and latitude.
!
                        call zzdynvad ( ctx, inname, infram, itmlon(i), 1, n, lon_1 )
                        lon = lon_1(1)

                        call zzdynvad ( ctx, inname, infram, itmlat(i), 1, n, lat_1 )
                        lat = lat_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, lon, units, kvradn, angles(1) )
                        call convrt ( ctx, lat, units, kvradn, angles(2) )

                     else
!
!                       Look up RA and DEC.
!
                        call zzdynvad ( ctx, inname, infram, itmra(i), 1, n, ra_1 )
                        ra = ra_1(1)

                        call zzdynvad ( ctx, inname, infram, itmdec(i), 1, n, dec_1 )
                        dec = dec_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, ra,  units, kvradn, angles(1) )
                        call convrt ( ctx, dec, units, kvradn, angles(2) )

                     end if

                     if ( ctx%failed() ) return

!
!                    Now  produce a direction vector.
!
                     call latrec ( 1.0_wp, angles(1), angles(2), dirvec )

                  else

                     call ctx%setmsg ( 'Definition of two-vector '       // &
                                   'parameterized dynamic frame # '  // &
                                   'includes constant vector '       // &
                                   'specification #, which is '      // &
                                   'not supported.  This situation ' // &
                                   'is usually caused by an error '  // &
                                   'in a frame kernel in which '     // &
                                   'the frame is defined.'           )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errch  ( '#',  spec                        )
                     call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                     return

                  end if

!
!                 At this point, the Cartesian coordinates of the
!                 vector relative to the constant vector frame
!                 are stored in DIRVEC.
!
                  if ( frid == j2000 ) then

                     v2(:,i) = dirvec

                  else
!
!                    Convert the direction vector to the J2000 frame.
!
                     call refchg ( ctx, frid, j2000, fet, r2000 )

                     if ( ctx%failed() ) return

                     call mxv ( r2000, dirvec, v2(1,i) )

                  end if

!
!                 The constant vector is now represented
!                 in the J2000 frame, but we may still need to
!                 apply a stellar aberration correction.
!
                  if ( corblk(stlidx) ) then
!
!                    Perform the correction appropriate to the
!                    radiation travel sense.
!
                     if ( corblk(xmtidx) ) then
!
!                       The correction is for transmission.
!
                        call stlabx ( ctx, v2(1,i), stobs(4), ptemp )

                     else
!
!                       The correction is for reception.
!
                        call stelab ( ctx, v2(1,i), stobs(4), ptemp )

                     end if

                     if ( ctx%failed() ) return

                     v2(:,i) = ptemp

                  end if
!
!                 At this point, V2(*,I) contains the constant
!                 (constant relative to its associated frame, that is)
!                 vector as seen by the observer, relative to frame
!                 J2000.
!

               else

                  call ctx%setmsg ( 'Definition of two-vector '       // &
                                'parameterized dynamic frame # '  // &
                                'includes vector '                // &
                                'definition #, which is '         // &
                                'not supported.  This situation ' // &
                                'is usually caused by an error '  // &
                                'in a frame kernel in which '     // &
                                'the frame is defined.'           )
                  call ctx%errch  ( '#',  inname                      )
                  call ctx%errch  ( '#',  vecdef(i)                   )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                  return

               end if

!
!              Negate the vector if the axis has negative sign.
!
               if ( negate ) then
                  ptemp = -v2(:,i)
                  v2(:,i) = ptemp
               end if

            end do

!
!           Look up the lower bound for the angular separation of
!           the defining vectors.  Use the default value if none
!           was supplied.
!
            call zzdynoad ( ctx, inname, infram, itmsep, 1, n, minsep_1, fnd )
            minsep = minsep_1(1)

            if ( ctx%failed() ) return

            if ( .not. fnd ) then
               minsep = lbsep
            end if

!
!           Now use our vectors to compute our position transformation
!           matrix.
!
!           Check the angular separation of the defining vectors. We
!           want to ensure that the vectors are not too close to being
!           linearly dependent.  We can handle both cases---separation
!           close to 0 or separation close to Pi---by comparing the
!           sine of the separation to the sine of the separation limit.
!
            sep = vsep ( v2(1,1), v2(1,2) )

            if ( sin(sep) < sin(minsep) ) then

               call etcal  ( ctx, t0, timstr )

               call ctx%setmsg ( 'Angular separation of vectors defining '// &
                             'two-vector parameterized dynamic frame '// &
                             '# is # (radians); minimum allowed diff' // &
                             'erence of separation from 0 or Pi is # '// &
                             'radians.  Evaluation epoch is #.  Extre'// &
                             'me loss of precision can occur when '   // &
                             'defining vectors are nearly linearly '  // &
                             'dependent.  This type of error can be ' // &
                             'due to using a dynamic frame outside of'// &
                             ' the time range for which it is meant. '// &
                             'It also can be due to a conceptual '    // &
                             'error pertaining to the frame''s defin' // &
                             'ition, or to an implementation error '  // &
                             'in the frame kernel containing the '    // &
                             'frame definition. However, if you wish '// &
                             'to proceed with this computation, '     // &
                             'the # keyword can be used in '          // &
                             'the frame definition to adjust the '    // &
                             'separation limit.'                      )
               call ctx%errch  ( '#',  inname                             )
               call ctx%errdp  ( '#',  sep                                )
               call ctx%errdp  ( '#',  minsep                             )
               call ctx%errch  ( '#',  timstr                             )
               call ctx%errch  ( '#',  kwatol                             )
               call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                  )
               return

            end if
!
!           We have both positions expressed relative to frame J2000
!           at this point.  Find the transformation from INNAME to
!           the frame J2000, then from J2000 to frame BASNAM.
!
            call twovec   ( ctx, v2(1,1), axis(1), v2(1,2), axis(2), rinv )

            rotate = transpose(rinv)

            if ( basfrm /= j2000 ) then

               rtemp = rotate

               call refchg ( ctx, j2000,   basfrm,  t0,  r2000  )

               if ( ctx%failed() ) return

               call mxm ( r2000, rtemp, rotate )

            end if

!
!           ROTATE is set.
!
!           This is the end of the work specific to two-vector frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kveulr ) then
!
!           The frame belongs to the Euler family.
!
!           We expect to specifications of an axis sequence, units,
!           and angles via polynomial coefficients.  We also expect
!           to see an ET epoch.
!
!           Look up the epoch first.  Let DELTA represent the offset
!           of T0 relative to the epoch.
!
!           Initialize EPOCH so subtraction doesn't overflow if EPOCH
!           is invalid due to a lookup error.
!
            epoch = 0.0_wp

            call zzdynvad ( ctx, inname, infram, kwepoc, 1, n, epoch_1 )
            epoch = epoch_1(1)

            delta = t0 - epoch

!
!           Now the axis sequence.
!
            call zzdynvai ( ctx, inname, infram, kweuax, 3, n, iaxes )

!
!           Now the coefficients for the angles.
!
            do i = 1, 3
!
!              Initialize N so subtraction doesn't overflow if N
!              is invalid due to a lookup error.
!
               n = 0

               call zzdynvad ( ctx, inname,  infram,  itmcof(i), &
                               maxcof,  n,       coeffs(1,i) )

!
!              Set the polynomial degree for the Ith angle.
!
               degs(i) = n - 1

            end do

!
!           Look up the units associated with the angles.
!
            call zzdynvac ( ctx, inname, infram, kwunit, 1, n, units )

            if ( ctx%failed() ) return

!
!           Evaluate the angles at DELTA.  Convert angles from input
!           units to radians.
!
            do i = 1, 3

               call polyds ( coeffs(1,i), degs(i), 0, delta, poly )
!
!              Convert units.  Fill in the Euler angle vector.
!
               call convrt ( ctx, poly(0), units, kvradn, eulang(i) )

            end do

!
!           Produce a position transformation matrix that maps from
!           the defined frame to the base frame.
!
            call eul2m ( ctx, eulang(1), eulang(2), eulang(3), &
                         iaxes(1),  iaxes(2),  iaxes(3),  rotate )

!
!           This is the end of the work specific to Euler frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kvprod ) then
!
!           The frame belongs to the product family.
!
!           We expect to see specifications of "from" and "to" frames
!           that make up the product.
!
!           Look up the "from" and "to" frames that define the product.
!
            call dynamic_frame_strings ( ctx, inname, infram, kwffrm, mxnfac, m, fframs )
            call dynamic_frame_strings ( ctx, inname, infram, kwtfrm, mxnfac, n, tframs )

            if ( ctx%failed() ) return

            if ( n /= m ) then

               call ctx%setmsg ( 'Definition of product ' &
               //            'parameterized dynamic frame # ' &
               //            'has # "from" frames and # "to" ' &
               //            'frames. These counts must match.' )
               call ctx%errch  ( '#',  inname                       )
               call ctx%errint ( '#',  m                            )
               call ctx%errint ( '#',  n                            )
               call ctx%sigerr ( 'SPICE(BADFRAMECOUNT)'             )
               return

            end if

!
!           The product of the factors is the transformation from the
!           base frame to the output frame. We need the inverse of
!           this transformation.
!
!           Compute the inverse of the product of the factors; place
!           the result in ROTATE.
!
            call namfrm ( ctx, fframs(1)%s, fromid )
            call namfrm ( ctx, tframs(1)%s, toid   )

            call refchg ( ctx, toid, fromid, t0, rotate )

            if ( ctx%failed() ) return

            do i = 2, n

               rtemp = rotate

               call namfrm ( ctx, fframs(i)%s, fromid )
               call namfrm ( ctx, tframs(i)%s, toid   )

               call refchg ( ctx, toid, fromid, t0, rotate )

               if ( ctx%failed() ) return

               call mxm   ( rotate, rtemp, rout   )
               rotate = rout

            end do

         else

            call ctx%setmsg ( 'Dynamic frame family # (in definition of '// &
                          'frame #) is not supported. '              // &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  inname                               )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            return

         end if
!
!        This is the end of the IF block that processes the
!        parameterized dynamic frame families.
!

      else

         call ctx%setmsg ( 'Dynamic frame style # (in definition of ' // &
                       'frame #) is not supported. '              // &
                       'This situation is usually caused by '     // &
                       'an error in a frame kernel in which '     // &
                       'the frame is defined.'                    )
         call ctx%errch  ( '#',  dynstl                               )
         call ctx%errch  ( '#',  inname                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

!
!     At this point ROTATE is the position transformation matrix
!     mapping from the input frame INFRAM to the base frame BASFRM.
!
!     ROTATE and BASFRM is set.
!
      return
      end associate
      end subroutine dynamic_frame_rotation

      !> PEPPER: record that the dynamic frame FRAME is being evaluated
      !> (ZZDYNFRM, ZZDYNROT), or signal SPICE(RECURSIONTOODEEP) if it is
      !> already: its definition depends on itself.
      subroutine enter_dynamic_frame ( ctx, frame )
      use pepper_containers, only: grow
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frame  !! the frame ID
      integer(ip) :: i
      character(:), allocatable :: chain
      character(12) :: buf
      if ( any ( ctx%dynamic_frames(1:ctx%ndynamic) == frame ) ) then
         chain = ''
         do i = 1, ctx%ndynamic
            write ( buf, '(i0)' ) ctx%dynamic_frames(i)
            chain = chain // trim ( buf ) // ' -> '
         end do
         write ( buf, '(i0)' ) frame
         call ctx%setmsg ( 'The dynamic frame # is defined in terms of itself: ' // &
                           'evaluating it needs, in turn, the frames #. ' // &
                           'A dynamic frame''s definition must not depend on the frame.' )
         call ctx%errint ( '#', frame )
         call ctx%errch  ( '#', chain // trim ( buf ) )
         call ctx%sigerr ( 'SPICE(RECURSIONTOODEEP)' )
         return
      end if
      call grow ( ctx%dynamic_frames, ctx%ndynamic + 1 )
      ctx%ndynamic = ctx%ndynamic + 1
      ctx%dynamic_frames(ctx%ndynamic) = frame
      end subroutine enter_dynamic_frame

end submodule pepper_dynamic
