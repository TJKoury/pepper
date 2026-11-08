
! $ Abstract
!
!     Include file zzdyn.inc
!
!     SPICE private file intended solely for the support of SPICE
!     routines.  Users should not include this file directly due
!     to the volatile nature of this file
!
!     The parameters defined below are used by the SPICELIB dynamic
!     frame subsystem.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Parameters
!
!     This file declares parameters required by the dynamic
!     frame routines of the SPICELIB frame subsystem.
!
! $ Restrictions
!
!     The parameter BDNMLN is this routine must be kept
!     consistent with the parameter MAXL defined in
!
!        zzbodtrn.inc
!
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 22-SEP-2020 (NJB)
!
!        Updated to support the product frame family.
!
!-    SPICELIB Version 1.1.0, 12-JAN-2005 (NJB)
!
!        Parameters KWX, KWY, KWZ renamed to KVX, KVY, KVZ.
!
!-    SPICELIB Version 1.0.0, 22-DEC-2004 (NJB)
!
!-&


!
!     String length parameters
!     ========================
!
!
!     Kernel variable name length.  This parameter must be
!     kept consistent with the parameter MAXLEN used in the
!     POOL umbrella routine.
!
      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

!
!     Length of a character kernel pool datum. This parameter must be
!     kept consistent with the parameter MAXCHR used in the POOL
!     umbrella routine.
!
      INTEGER               KVLEN
      PARAMETER           ( KVLEN  = 80 )

!
!     Reference frame name length.  This parameter must be
!     kept consistent with the parameter WDSIZE used in the
!     FRAMEX umbrella routine.
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

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
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )


!
!     Numeric parameters
!     ===================================
!
!     The parameter MAXCOF is the maximum number of polynomial
!     coefficients that may be used to define an Euler angle
!     in an "Euler frame" definition
!
      INTEGER               MAXCOF
      PARAMETER           ( MAXCOF = 20 )

!
!     The parameter MXNFAC is the maximum number of factors in
!     a product frame.
!
      INTEGER               MXNFAC
      PARAMETER           ( MXNFAC = 10 )

!
!     The parameter LBSEP is the default angular separation limit for
!     the vectors defining a two-vector frame.  The angular separation
!     of the vectors must differ from Pi and 0 by at least this amount.
!
      DOUBLE PRECISION      LBSEP
      PARAMETER           ( LBSEP  = 1.D-3 )

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
      INTEGER               QEXP
      PARAMETER           ( QEXP = -27 )


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
      CHARACTER*(*)         KWBFRM
      PARAMETER           ( KWBFRM = 'RELATIVE' )

!
!     Frame definition style parameters
!     ---------------------------------
!
!     Token used to build the frame definition style keyword:
!
      CHARACTER*(*)         KWSTYL
      PARAMETER           ( KWSTYL = 'DEF_STYLE' )

!
!     Token indicating parameterized dynamic frame.
!
      CHARACTER*(*)         KVPARM
      PARAMETER           ( KVPARM = 'PARAMETERIZED' )


!
!     Freeze epoch parameters
!     ---------------------------------
!
!     Token used to build the freeze epoch keyword:
!
      CHARACTER*(*)         KWFREZ
      PARAMETER           ( KWFREZ = 'FREEZE_EPOCH' )

!
!     Rotation state parameters
!     ---------------------------------
!
!     Token used to build the rotation state keyword:
!
      CHARACTER*(*)         KWRSTA
      PARAMETER           ( KWRSTA = 'ROTATION_STATE' )

!
!     Token indicating rotating rotation state:
!
      CHARACTER*(*)         KVROTG
      PARAMETER           ( KVROTG = 'ROTATING' )

!
!     Token indicating inertial rotation state:
!
      CHARACTER*(*)         KVINRT
      PARAMETER           ( KVINRT = 'INERTIAL' )

!
!     Frame family parameters
!     ---------------------------------
!
!     Token used to build the frame family keyword:
!
      CHARACTER*(*)         KWFFAM
      PARAMETER           ( KWFFAM = 'FAMILY' )

!
!     Token indicating mean equator and equinox of date frame.
!
      CHARACTER*(*)       KVMEQT
      PARAMETER         ( KVMEQT = 'MEAN_EQUATOR_AND_EQUINOX_OF_DATE'  )

!
!     Token indicating mean ecliptic and equinox of date frame.
!
      CHARACTER*(*)       KVMECL
      PARAMETER         ( KVMECL = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE' )

!
!     Token indicating true equator and equinox of date frame.
!
      CHARACTER*(*)       KVTEQT
      PARAMETER         ( KVTEQT = 'TRUE_EQUATOR_AND_EQUINOX_OF_DATE'  )

!
!     Token indicating two-vector frame.
!
      CHARACTER*(*)         KV2VEC
      PARAMETER           ( KV2VEC = 'TWO-VECTOR'  )

!
!     Token indicating Euler frame.
!
      CHARACTER*(*)         KVEULR
      PARAMETER           ( KVEULR = 'EULER'  )

!
!     Token indicating product frame.
!
      CHARACTER*(*)         KVPROD
      PARAMETER           ( KVPROD = 'PRODUCT'  )


!
!     "Of date" frame family parameters
!     ---------------------------------
!
!     Token used to build the precession model keyword:
!
      CHARACTER*(*)         KWPRCM
      PARAMETER           ( KWPRCM = 'PREC_MODEL' )

!
!     Token used to build the nutation model keyword:
!
      CHARACTER*(*)         KWNUTM
      PARAMETER           ( KWNUTM = 'NUT_MODEL' )

!
!     Token used to build the obliquity model keyword:
!
      CHARACTER*(*)         KWOBQM
      PARAMETER           ( KWOBQM = 'OBLIQ_MODEL' )

!
!     Mathematical models used to define "of date" frames will
!     likely accrue over time.  We will simply assign them
!     numbers.
!
!
!     Token indicating the Lieske earth precession model:
!
      CHARACTER*(*)         KVM001
      PARAMETER           ( KVM001 = 'EARTH_IAU_1976' )

!
!     Token indicating the IAU 1980 earth nutation model:
!
      CHARACTER*(*)         KVM002
      PARAMETER           ( KVM002 = 'EARTH_IAU_1980' )

!
!     Token indicating the IAU 1980 earth mean obliqity of
!     date model.  Note the name matches that of the preceding
!     nutation model---this is intentional.  The keyword
!     used in the kernel variable definition indicates what
!     kind of model is being defined.
!
      CHARACTER*(*)         KVM003
      PARAMETER           ( KVM003 = 'EARTH_IAU_1980' )


!
!     Two-vector frame family parameters
!     ---------------------------------
!
!     Token used to build the vector axis keyword:
!
      CHARACTER*(*)         KWVAXI
      PARAMETER           ( KWVAXI = 'AXIS' )

!
!     Tokens indicating axis values:
!
      CHARACTER*(*)         KVX
      PARAMETER           ( KVX    = 'X' )

      CHARACTER*(*)         KVY
      PARAMETER           ( KVY    = 'Y' )

      CHARACTER*(*)         KVZ
      PARAMETER           ( KVZ    = 'Z' )

!
!     Prefixes used for primary and secondary vector definition
!     keywords:
!
      CHARACTER*(*)         KWPRI
      PARAMETER           ( KWPRI  = 'PRI_' )

      CHARACTER*(*)         KWSEC
      PARAMETER           ( KWSEC  = 'SEC_' )

!
!     Token used to build the vector definition keyword:
!
      CHARACTER*(*)         KWVCDF
      PARAMETER           ( KWVCDF = 'VECTOR_DEF' )

!
!     Token indicating observer-target position vector:
!
      CHARACTER*(*)         KVPOSV
      PARAMETER           ( KVPOSV = 'OBSERVER_TARGET_POSITION' )

!
!     Token indicating observer-target velocity vector:
!
      CHARACTER*(*)         KVVELV
      PARAMETER           ( KVVELV = 'OBSERVER_TARGET_VELOCITY' )

!
!     Token indicating observer-target near point vector:
!
      CHARACTER*(*)         KVNEAR
      PARAMETER           ( KVNEAR = 'TARGET_NEAR_POINT' )

!
!     Token indicating constant vector:
!
      CHARACTER*(*)         KVCONS
      PARAMETER           ( KVCONS = 'CONSTANT' )

!
!     Token used to build the vector observer keyword:
!
      CHARACTER*(*)         KWVOBS
      PARAMETER           ( KWVOBS = 'OBSERVER' )

!
!     Token used to build the vector target keyword:
!
      CHARACTER*(*)         KWVTRG
      PARAMETER           ( KWVTRG = 'TARGET' )

!
!     Token used to build the vector frame keyword:
!
      CHARACTER*(*)         KWVFRM
      PARAMETER           ( KWVFRM = 'FRAME' )

!
!     Token used to build the vector aberration correction keyword:
!
      CHARACTER*(*)         KWVABC
      PARAMETER           ( KWVABC = 'ABCORR' )

!
!     Token used to build the constant vector specification keyword:
!
      CHARACTER*(*)         KWVSPC
      PARAMETER           ( KWVSPC = 'SPEC' )

!
!     Token indicating rectangular coordinates used to
!     specify constant vector:
!
      CHARACTER*(*)         KVRECC
      PARAMETER           ( KVRECC = 'RECTANGULAR' )

!
!     Token indicating latitudinal coordinates used to
!     specify constant vector:
!
      CHARACTER*(*)         KVLATC
      PARAMETER           ( KVLATC = 'LATITUDINAL' )

!
!     Token indicating RA/DEC coordinates used to
!     specify constant vector:
!
      CHARACTER*(*)         KVRADC
      PARAMETER           ( KVRADC = 'RA/DEC' )

!
!     Token used to build the cartesian vector literal keyword:
!
      CHARACTER*(*)         KWVECT
      PARAMETER           ( KWVECT = 'VECTOR' )

!
!     Token used to build the constant vector latitude keyword:
!
      CHARACTER*(*)         KWLAT
      PARAMETER           ( KWLAT  = 'LATITUDE' )

!
!     Token used to build the constant vector longitude keyword:
!
      CHARACTER*(*)         KWLON
      PARAMETER           ( KWLON  = 'LONGITUDE' )

!
!     Token used to build the constant vector right ascension keyword:
!
      CHARACTER*(*)         KWRA
      PARAMETER           ( KWRA   = 'RA' )

!
!     Token used to build the constant vector declination keyword:
!
      CHARACTER*(*)         KWDEC
      PARAMETER           ( KWDEC  = 'DEC' )

!
!     Token used to build the angular separation tolerance keyword:
!
      CHARACTER*(*)         KWATOL
      PARAMETER           ( KWATOL = 'ANGLE_SEP_TOL' )

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
      CHARACTER*(*)         KWEPOC
      PARAMETER           ( KWEPOC = 'EPOCH' )

!
!     Token used to build the Euler axis sequence keyword:
!
      CHARACTER*(*)         KWEUAX
      PARAMETER           ( KWEUAX = 'AXES' )

!
!     Tokens used to build the Euler angle coefficients keywords:
!
      CHARACTER*(*)         KWEAC1
      PARAMETER           ( KWEAC1 = 'ANGLE_1_COEFFS' )

      CHARACTER*(*)         KWEAC2
      PARAMETER           ( KWEAC2 = 'ANGLE_2_COEFFS' )

      CHARACTER*(*)         KWEAC3
      PARAMETER           ( KWEAC3 = 'ANGLE_3_COEFFS' )

!
!     See the section "Physical unit parameters" below for additional
!     parameters applicable to Euler frames.
!

!
!     Product frame family parameters
!     ---------------------------------
!
      CHARACTER*(*)         KWFFRM
      PARAMETER           ( KWFFRM = 'FROM_FRAMES'  )

      CHARACTER*(*)         KWTFRM
      PARAMETER           ( KWTFRM = 'TO_FRAMES'  )


!
!     Physical unit parameters
!     ---------------------------------
!
!     Token used to build the units keyword:
!
      CHARACTER*(*)         KWUNIT
      PARAMETER           ( KWUNIT = 'UNITS' )

!
!     Token indicating radians:
!
      CHARACTER*(*)         KVRADN
      PARAMETER           ( KVRADN = 'RADIANS' )

!
!     Token indicating degrees:
!
      CHARACTER*(*)         KVDEGR
      PARAMETER           ( KVDEGR = 'DEGREES' )


!
!     End of include file zzdyn.inc
!
