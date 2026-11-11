!> The parameters defined below are used by the SPICELIB dynamic frame subsystem.
!>
!> SPICE private file intended solely for the support of SPICE routines.  Users should not include this file directly due to the volatile nature of this file
!>
!>## Parameters
!>
!>     This file declares parameters required by the dynamic
!>     frame routines of the SPICELIB frame subsystem.
!>
!>## Restrictions
!>
!>     The parameter BDNMLN is this routine must be kept
!>     consistent with the parameter MAXL defined in
!>
!>        zzbodtrn.inc
!>
!>## Author and Institution
!>
!>     N.J. Bachman    (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 2.0.0, 22-SEP-2020 (NJB)
!>
!>        Updated to support the product frame family.
!>
!>-    SPICELIB Version 1.1.0, 12-JAN-2005 (NJB)
!>
!>        Parameters KWX, KWY, KWZ renamed to KVX, KVY, KVZ.
!>
!>-    SPICELIB Version 1.0.0, 22-DEC-2004 (NJB)
module pepper_inc_zzdyn

   use pepper_kinds, only: ip, wp

   implicit none

   public

!
!     String length parameters
!     ========================
!
!
!     Kernel variable name length.  This parameter must be
!     kept consistent with the parameter MAXLEN used in the
!     POOL umbrella routine.
!
   integer(ip), parameter :: kvnmln = 32

!
!     Length of a character kernel pool datum. This parameter must be
!     kept consistent with the parameter MAXCHR used in the POOL
!     umbrella routine.
!
   integer(ip), parameter :: kvlen = 80

!
!     Reference frame name length.  This parameter must be
!     kept consistent with the parameter WDSIZE used in the
!     FRAMEX umbrella routine.
!
   integer(ip), parameter :: frnmln = 32

!
!     Body name length.  This parameter is used to provide a level
!     of indirection so the dynamic frame source code doesn't
!     have to change if the name of this SPICELIB-scope parameter
!     is changed.  The value MAXL used here is defined in the
!     INCLUDE file
!
!        zzbodtrn.inc
!
!     Current value of MAXL = 36
!
   integer(ip), parameter :: bdnmln = 36

!
!     Numeric parameters
!     ===================================
!
!     The parameter MAXCOF is the maximum number of polynomial
!     coefficients that may be used to define an Euler angle
!     in an "Euler frame" definition
!
   integer(ip), parameter :: maxcof = 20

!
!     The parameter MXNFAC is the maximum number of factors in
!     a product frame.
!
   integer(ip), parameter :: mxnfac = 10

!
!     The parameter LBSEP is the default angular separation limit for
!     the vectors defining a two-vector frame.  The angular separation
!     of the vectors must differ from Pi and 0 by at least this amount.
!
   real(wp), parameter :: lbsep = 1.0e-3_wp

!
!     The parameter QEXP is used to determine the width of
!     the interval DELTA used for the discrete differentiation
!     of velocity in the routines ZZDYNFRM, ZZDYNROT, and their
!     recursive analogs.  This parameter is appropriate for
!     64-bit IEEE double precision numbers; when SPICELIB
!     is hosted on platforms where longer mantissas are supported,
!     this parameter (and hence this INCLUDE file) will become
!     platform-dependent.
!
!     The choice of QEXP is based on heuristics.  It's believed to
!     be a reasonable choice obtainable without expensive computation.
!
!     QEXP is the largest power of 2 such that
!
!        1.D0 + 2**QEXP  =  1.D0
!
!     Given an epoch T0 at which a discrete derivative is to be
!     computed, this choice provides a value of DELTA that usually
!     contributes no round-off error in the computation of the function
!     evaluation epochs
!
!        T0 +/- DELTA
!
!     while providing the largest value of DELTA having this form that
!     causes the order of the error term O(DELTA**2) in the quadratic
!     function approximation to round to zero.  Note that the error
!     itself will normally be small but doesn't necessarily round to
!     zero.  Note also that the small function approximation error
!     is not a measurement of the error in the discrete derivative
!     itself.
!
!     For ET values T0 > 2**27 seconds past J2000, the value of
!     DELTA will be set to
!
!        T0 * 2**QEXP
!
!     For smaller values of T0, DELTA should be set to 1.D0.
!
   integer(ip), parameter :: qexp = -27

!
!     Frame kernel parameters
!     =======================
!
!     Parameters relating to kernel variable names (keywords) start
!     with the letters
!
!        KW
!
!     Parameters relating to kernel variable values start with the
!     letters
!
!        KV
!
!
!     Generic parameters
!     ---------------------------------
!
!     Token used to build the base frame keyword:
!
   character(*), parameter :: kwbfrm = 'RELATIVE'

!
!     Frame definition style parameters
!     ---------------------------------
!
!     Token used to build the frame definition style keyword:
!
   character(*), parameter :: kwstyl = 'DEF_STYLE'

!
!     Token indicating parameterized dynamic frame.
!
   character(*), parameter :: kvparm = 'PARAMETERIZED'

!
!     Freeze epoch parameters
!     ---------------------------------
!
!     Token used to build the freeze epoch keyword:
!
   character(*), parameter :: kwfrez = 'FREEZE_EPOCH'

!
!     Rotation state parameters
!     ---------------------------------
!
!     Token used to build the rotation state keyword:
!
   character(*), parameter :: kwrsta = 'ROTATION_STATE'

!
!     Token indicating rotating rotation state:
!
   character(*), parameter :: kvrotg = 'ROTATING'

!
!     Token indicating inertial rotation state:
!
   character(*), parameter :: kvinrt = 'INERTIAL'

!
!     Frame family parameters
!     ---------------------------------
!
!     Token used to build the frame family keyword:
!
   character(*), parameter :: kwffam = 'FAMILY'

!
!     Token indicating mean equator and equinox of date frame.
!
   character(*), parameter :: kvmeqt = 'MEAN_EQUATOR_AND_EQUINOX_OF_DATE'

!
!     Token indicating mean ecliptic and equinox of date frame.
!
   character(*), parameter :: kvmecl = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE'

!
!     Token indicating true equator and equinox of date frame.
!
   character(*), parameter :: kvteqt = 'TRUE_EQUATOR_AND_EQUINOX_OF_DATE'

!
!     Token indicating two-vector frame.
!
   character(*), parameter :: kv2vec = 'TWO-VECTOR'

!
!     Token indicating Euler frame.
!
   character(*), parameter :: kveulr = 'EULER'

!
!     Token indicating product frame.
!
   character(*), parameter :: kvprod = 'PRODUCT'

!
!     "Of date" frame family parameters
!     ---------------------------------
!
!     Token used to build the precession model keyword:
!
   character(*), parameter :: kwprcm = 'PREC_MODEL'

!
!     Token used to build the nutation model keyword:
!
   character(*), parameter :: kwnutm = 'NUT_MODEL'

!
!     Token used to build the obliquity model keyword:
!
   character(*), parameter :: kwobqm = 'OBLIQ_MODEL'

!
!     Mathematical models used to define "of date" frames will
!     likely accrue over time.  We will simply assign them
!     numbers.
!
!
!     Token indicating the Lieske earth precession model:
!
   character(*), parameter :: kvm001 = 'EARTH_IAU_1976'

!
!     Token indicating the IAU 1980 earth nutation model:
!
   character(*), parameter :: kvm002 = 'EARTH_IAU_1980'

!
!     Token indicating the IAU 1980 earth mean obliqity of
!     date model.  Note the name matches that of the preceding
!     nutation model---this is intentional.  The keyword
!     used in the kernel variable definition indicates what
!     kind of model is being defined.
!
   character(*), parameter :: kvm003 = 'EARTH_IAU_1980'

!
!     Two-vector frame family parameters
!     ---------------------------------
!
!     Token used to build the vector axis keyword:
!
   character(*), parameter :: kwvaxi = 'AXIS'

!
!     Tokens indicating axis values:
!
   character(*), parameter :: kvx = 'X'

   character(*), parameter :: kvy = 'Y'

   character(*), parameter :: kvz = 'Z'

!
!     Prefixes used for primary and secondary vector definition
!     keywords:
!
   character(*), parameter :: kwpri = 'PRI_'

   character(*), parameter :: kwsec = 'SEC_'

!
!     Token used to build the vector definition keyword:
!
   character(*), parameter :: kwvcdf = 'VECTOR_DEF'

!
!     Token indicating observer-target position vector:
!
   character(*), parameter :: kvposv = 'OBSERVER_TARGET_POSITION'

!
!     Token indicating observer-target velocity vector:
!
   character(*), parameter :: kvvelv = 'OBSERVER_TARGET_VELOCITY'

!
!     Token indicating observer-target near point vector:
!
   character(*), parameter :: kvnear = 'TARGET_NEAR_POINT'

!
!     Token indicating constant vector:
!
   character(*), parameter :: kvcons = 'CONSTANT'

!
!     Token used to build the vector observer keyword:
!
   character(*), parameter :: kwvobs = 'OBSERVER'

!
!     Token used to build the vector target keyword:
!
   character(*), parameter :: kwvtrg = 'TARGET'

!
!     Token used to build the vector frame keyword:
!
   character(*), parameter :: kwvfrm = 'FRAME'

!
!     Token used to build the vector aberration correction keyword:
!
   character(*), parameter :: kwvabc = 'ABCORR'

!
!     Token used to build the constant vector specification keyword:
!
   character(*), parameter :: kwvspc = 'SPEC'

!
!     Token indicating rectangular coordinates used to
!     specify constant vector:
!
   character(*), parameter :: kvrecc = 'RECTANGULAR'

!
!     Token indicating latitudinal coordinates used to
!     specify constant vector:
!
   character(*), parameter :: kvlatc = 'LATITUDINAL'

!
!     Token indicating RA/DEC coordinates used to
!     specify constant vector:
!
   character(*), parameter :: kvradc = 'RA/DEC'

!
!     Token used to build the cartesian vector literal keyword:
!
   character(*), parameter :: kwvect = 'VECTOR'

!
!     Token used to build the constant vector latitude keyword:
!
   character(*), parameter :: kwlat = 'LATITUDE'

!
!     Token used to build the constant vector longitude keyword:
!
   character(*), parameter :: kwlon = 'LONGITUDE'

!
!     Token used to build the constant vector right ascension keyword:
!
   character(*), parameter :: kwra = 'RA'

!
!     Token used to build the constant vector declination keyword:
!
   character(*), parameter :: kwdec = 'DEC'

!
!     Token used to build the angular separation tolerance keyword:
!
   character(*), parameter :: kwatol = 'ANGLE_SEP_TOL'

!
!     See the section "Physical unit parameters" below for additional
!     parameters applicable to two-vector frames.
!

!
!     Euler frame family parameters
!     ---------------------------------
!
!     Token used to build the epoch keyword:
!
   character(*), parameter :: kwepoc = 'EPOCH'

!
!     Token used to build the Euler axis sequence keyword:
!
   character(*), parameter :: kweuax = 'AXES'

!
!     Tokens used to build the Euler angle coefficients keywords:
!
   character(*), parameter :: kweac1 = 'ANGLE_1_COEFFS'

   character(*), parameter :: kweac2 = 'ANGLE_2_COEFFS'

   character(*), parameter :: kweac3 = 'ANGLE_3_COEFFS'

!
!     See the section "Physical unit parameters" below for additional
!     parameters applicable to Euler frames.
!

!
!     Product frame family parameters
!     ---------------------------------
!
   character(*), parameter :: kwffrm = 'FROM_FRAMES'

   character(*), parameter :: kwtfrm = 'TO_FRAMES'

!
!     Physical unit parameters
!     ---------------------------------
!
!     Token used to build the units keyword:
!
   character(*), parameter :: kwunit = 'UNITS'

!
!     Token indicating radians:
!
   character(*), parameter :: kvradn = 'RADIANS'

!
!     Token indicating degrees:
!
   character(*), parameter :: kvdegr = 'DEGREES'

end module pepper_inc_zzdyn
