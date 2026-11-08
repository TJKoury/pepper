C$Procedure F_DYN01 ( Dynamic Frame Test Family 01 )
 
      SUBROUTINE F_DYN01 ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the first
C     subset of dynamic frame routines.  Only two-vector frames
C     are used in these tests.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzdyn.inc'
 
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C
C     Tests done here include, but are not necessarily limited to:
C
C        - Test state transformation from SXFORM:
C
C            o Make sure upper left and lower right blocks match.
C            o Test determinant of rotation blocks
C            o Test norms of rows and columns
C
C        - Compare SXFORM and PXFORM results.
C
C        - Compare SXFORM derivative to discrete derivative
C          computed using PXFORM.  Check absolute and relative
C          derivative errors.
C
C        - Compute state transformation via a local computation.
C          Compare rotation portion of transformation matrix
C          from SXFORM to locally computed result.
C
C        - Make sure that when rotation state is inertial, when a
C          matrix mapping from base frame to an inertial frame is
C          right-multiplied by matrix produced by SXFORM, resulting
C          matrix has zero derivative.
C
C        - Make sure that when rotation state is frozen, when a
C          matrix produced by SXFORM has zero derivative.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0, 05-JUN-2021 (BVS)
C
C        Added missing 'D' to literal DP constants.
C
C        Added SAVE statements for ABPAIR whose values are
C        initialized using DATA.
C
C        Re-ordered header sections.
C
C-    TSPICE Version 2.0.0, 09-NOV-2005 (NJB)
C
C        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
C        Tolerance was increase for GSE-to-J2000 test from 5.D-8 to
C        6.D-8.
C
C-    TSPICE Version 1.0.0, 10-JAN-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORMG
 
      INTEGER               ISRCHC
      INTEGER               PRODAI
 
      LOGICAL               EQSTR
 
C
C     Local Parameters
C
      CHARACTER*(*)         OTP
      PARAMETER           ( OTP    = 'OBSERVER_TARGET_POSITION' )
 
      CHARACTER*(*)         OTV
      PARAMETER           ( OTV    = 'OBSERVER_TARGET_VELOCITY' )
 
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'test_dyn.tpc' )
 
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'test_dyn.bsp' )
 
C
C     Tolerance levels for various tests.
C
      DOUBLE PRECISION      DM16
      PARAMETER           ( DM16   = 1.D-16 )
 
      DOUBLE PRECISION      DM15
      PARAMETER           ( DM15   = 1.D-15 )
 
      DOUBLE PRECISION      DM14
      PARAMETER           ( DM14   = 1.D-14 )
 
      DOUBLE PRECISION      DM12
      PARAMETER           ( DM12   = 1.D-12 )
 
      DOUBLE PRECISION      DM10
      PARAMETER           ( DM10   = 1.D-10 )
 
      DOUBLE PRECISION      DM9
      PARAMETER           ( DM9    = 1.D-9 )
 
      DOUBLE PRECISION      DM7
      PARAMETER           ( DM7    = 1.D-7 )
 
      DOUBLE PRECISION      DM6
      PARAMETER           ( DM6    = 1.D-6 )
 
      DOUBLE PRECISION      DM5
      PARAMETER           ( DM5    = 1.D-5 )
 
      DOUBLE PRECISION      DM4
      PARAMETER           ( DM4    = 1.D-4 )
 
      DOUBLE PRECISION      DM3
      PARAMETER           ( DM3    = 1.D-3 )
 
      DOUBLE PRECISION      DM2
      PARAMETER           ( DM2    = 1.D-2 )
 
      DOUBLE PRECISION      DM1
      PARAMETER           ( DM1    = 1.D-1 )
 
      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 15 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               MAXDEF
      PARAMETER           ( MAXDEF = 50 )
 
      INTEGER               MAXTOK
      PARAMETER           ( MAXTOK = 7 )
 
C
C     Parameters controlling frame definitions:
C
 
C
C     Number of dimensions of the test parameter space:
C
      INTEGER               NDIMS
      PARAMETER           ( NDIMS  = 5 )
 
C
C     Number of aberration correction settings:
C
      INTEGER               NABCRR
      PARAMETER           ( NABCRR = 11 )
 
C
C     Number of aberration correction settings used for the
C     full cartesian product test:
C
      INTEGER               NABPRS
      PARAMETER           ( NABPRS = 5 )
C      PARAMETER           ( NABPRS = 1 )
 
C
C     The constant ABCIDX refers to the ordinal position of the
C     dimension corresponding to aberration correction in the parameter
C     space.
C
      INTEGER               ABCIDX
      PARAMETER           ( ABCIDX = 1 )
 
C
C     Number of axis definition cases:
C
      INTEGER               NAXDEF
      PARAMETER           ( NAXDEF = 6 )
 
C
C     Number of base frame cases:
C
      INTEGER               NBASEF
      PARAMETER           ( NBASEF = 2 )
 
C
C     The constant BFRIDX refers to the ordinal position of the
C     dimension corresponding to base frame in the parameter
C     space.
C
      INTEGER               BFRIDX
      PARAMETER           ( BFRIDX = ABCIDX + 1 )
 
C
C     Number of bodies:
C
      INTEGER               NBODS
      PARAMETER           ( NBODS   = 5 )
 
C
C     Number of observer-target body pairs:
C
      INTEGER               NOTPRS
      PARAMETER           ( NOTPRS  = 5 )
 
      INTEGER               OTPIDX
      PARAMETER           ( OTPIDX  = BFRIDX + 1 )
 
C
C     Number of vector definitions:
C
      INTEGER               NVECDF
      PARAMETER           ( NVECDF = 4 )
 
C
C     Number of vector frames:
C
      INTEGER               NVECFR
      PARAMETER           ( NVECFR = 5 )
 
C
C     Number of extended vector definitions.  These definitions
C     include every combination of frame-dependent definition type
C     and frame (for example, velocity and observer frame, constant
C     and J2000 frame, etc.)
C
      INTEGER               NXVCDF
      PARAMETER           ( NXVCDF = NVECDF - 2 + 2*NVECFR )
 
      INTEGER               PRIIDX
      PARAMETER           ( PRIIDX = OTPIDX + 1 )
 
      INTEGER               SECIDX
      PARAMETER           ( SECIDX = PRIIDX + 1 )
 
      INTEGER               ORDLEN
      PARAMETER           ( ORDLEN = 4 )
 
C
C     Other parameters
C
C     Number of coordinate systems used to represent constant vectors:
C
      INTEGER               NCOSYS
      PARAMETER           ( NCOSYS = 3 )
 
      INTEGER               NUNITS
      PARAMETER           ( NUNITS = 2 )
 
C
C     Local Variables
C
      CHARACTER*(ABCLEN)    ABCORR ( NABCRR )
      CHARACTER*(LNSIZE)    ANGSTR ( 2, 2 )
      CHARACTER*(LNSIZE)    AXDEF  ( 2, NAXDEF )
      CHARACTER*(LNSIZE)    AXNAME
      CHARACTER*(NAMLEN)    BASEFR ( NBASEF )
      CHARACTER*(NAMLEN)    BFRAME
      CHARACTER*(NAMLEN)    BODIES ( NBODS  )
      CHARACTER*(LNSIZE)    CASMSG
      CHARACTER*(NAMLEN)    CENTER
      CHARACTER*(ABCLEN)    COR
      CHARACTER*(LNSIZE)    CORSYS ( NCOSYS )
      CHARACTER*(LNSIZE)    DEFTXT ( MAXDEF )
      CHARACTER*(LNSIZE)    DEFTX2 ( MAXDEF )
      CHARACTER*(NAMLEN)    FRNAME
      CHARACTER*(NAMLEN)    OBS
      CHARACTER*(ORDLEN)    ORD
      CHARACTER*(ABCLEN)    PRICOR
      CHARACTER*(NAMLEN)    PRIFRM
      CHARACTER*(NAMLEN)    PRIOBS
      CHARACTER*(NAMLEN)    PRITRG
      CHARACTER*(LNSIZE)    PRIVDF
      CHARACTER*(NAMLEN)    PRIVF
      CHARACTER*(ABCLEN)    SECCOR
      CHARACTER*(NAMLEN)    SECFRM
      CHARACTER*(NAMLEN)    SECOBS
      CHARACTER*(NAMLEN)    SECTRG
      CHARACTER*(LNSIZE)    SECVDF
      CHARACTER*(NAMLEN)    SECVF
      CHARACTER*(NAMLEN)    TARG
      CHARACTER*(LNSIZE)    TMPSTR
      CHARACTER*(LNSIZE)    TOKENS ( MAXTOK )
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(LNSIZE)    UNITS  ( NUNITS )
      CHARACTER*(LNSIZE)    VDF
      CHARACTER*(LNSIZE)    VECDEF ( NVECDF )
      CHARACTER*(LNSIZE)    VECFRM ( NVECFR )
      CHARACTER*(LNSIZE)    VECSTR ( 3 )
      CHARACTER*(LNSIZE)    VFR
      CHARACTER*(NAMLEN)    VFRAME
 
C
C     We'll use the kernel variable name "stem"
C
C        FRAME_<ID code>_
C
C     The length declared below (18) is the length of such a string
C     where the ID code contains 11 digits.
C
      CHARACTER*(18)        FRSTEM
 
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      AXSIGN
      DOUBLE PRECISION      CLT
      DOUBLE PRECISION      CONVEC ( 3 )
      DOUBLE PRECISION      CSTATE ( 6 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DETERR
      DOUBLE PRECISION      DMAG
      DOUBLE PRECISION      DRDIFF ( 3, 3 )
      DOUBLE PRECISION      DRLERR
      DOUBLE PRECISION      DRVBLK ( 3, 3 )
      DOUBLE PRECISION      DRVERR
      DOUBLE PRECISION      E      ( 3, 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETCORR
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAT0
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LON0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAXDER
      DOUBLE PRECISION      NRMERR
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      PRIVEC ( 3 )
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      R2     ( 3, 3 )
      DOUBLE PRECISION      R3     ( 3, 3 )
      DOUBLE PRECISION      R4     ( 3, 3 )
      DOUBLE PRECISION      RMINUS ( 3, 3 )
      DOUBLE PRECISION      RPLUS  ( 3, 3 )
      DOUBLE PRECISION      SECVEC ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XF2000 ( 6, 6 )
      DOUBLE PRECISION      XFB2J  ( 6, 6 )
      DOUBLE PRECISION      XF2    ( 6, 6 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      ZR     ( 3, 3 )
 
 
      INTEGER               ABPAIR ( 2, NABPRS )
      INTEGER               AXINDX
      INTEGER               AXPAIR
      INTEGER               CASE
      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               COORDS ( NDIMS )
      INTEGER               CORIDX
      INTEGER               DIMS   ( NDIMS )
      INTEGER               DX
      INTEGER               FRCODE
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               N
      INTEGER               NCART
      INTEGER               NCOR
      INTEGER               NSKIP
      INTEGER               PRIJ
      INTEGER               SYSIDX
      INTEGER               VFCODE
 
      INTEGER               NPR
 
C
C     This variable is used for debugging.
C
      INTEGER               NOGO
 
 
C
C     OTPAIR is used to store observer-target pairs used to define
C     all vector types other than constant vectors.  The first
C     element of the ith column indicates the observer (the value
C     is an index into the BODIES array) for the primary vector;
C     the second element indicates the target for the primary vector.
C     The third and fourth elements play the same role for the
C     secondary vector.
C
      INTEGER               OTPAIR ( 4, NOTPRS )
 
      LOGICAL               BIGERR
      LOGICAL               FOUND
      LOGICAL               GO
      LOGICAL               ISCON  ( 2 )
      LOGICAL               ISVEL  ( 2 )
      LOGICAL               PRIBLK ( NABCOR )
      LOGICAL               TSTDRV
      LOGICAL               SECBLK ( NABCOR )
 
C
C     Saved variables
C
      SAVE                  ABPAIR
      SAVE                  ANGSTR
      SAVE                  AXDEF
      SAVE                  ABCORR
      SAVE                  BASEFR
      SAVE                  BODIES
      SAVE                  CORSYS
      SAVE                  DIMS
      SAVE                  E
      SAVE                  OTPAIR
      SAVE                  VECDEF
      SAVE                  VECFRM
      SAVE                  VECSTR
      SAVE                  UNITS
      SAVE                  ZR
 
C
C     Initial values
C
      DATA                  ABCORR                                    /
     .                      'NONE',
     .                      'LT',
     .                      'LT+S',
     .                      'XLT',
     .                      'XLT+S',
     .                      'CN',
     .                      'CN+S',
     .                      'XCN',
     .                      'XCN+S',
     .                      'S',
     .                      'XS'                                      /
 
C
C     The elements of ABPAIR are indices into the ABCORR array. ABPAIR
C     has dimensions 2 x NABPRS. Each column of ABPAIR indicates the
C     corrections to be used for the primary and secondary vectors.
C
C     For the special case of constant vectors, the correction
C     'LT+S' is replaced with the correction 'LT', the correction
C     'XLT' is replaced with the correction 'S', and the correction
C     'XLT+S' is replaced with the correction 'XS'.  These replacements
C     are done in-line.
C
      DATA                  ABPAIR                                    /
     .                      1, 1,
     .                      1, 5,
     .                      3, 1,
     .                      5, 3,
     .                      4, 4                                      /
 
C
C     When defining axes, we vary the spacing and case to test the
C     algorithm used to parse axis specifications.
C
      DATA                  AXDEF                                     /
     .                      'Z',      'X',
     .                      '-x',     ' Y ',
     .                      ' Y ',    '-z',
     .                      ' -  y ', '  X',
     .                      ' z',     ' - x ',
     .                      '- Z',    ' - y '                         /
 
C
C     We use both inertial and dynamic base frames.
C
      DATA                  BASEFR                                    /
     .                      'J2000',
     .                      'GSE'                                     /
 
C
C     PHOBOS_RADIAL is an alternative, or additional, dynamic base
C     frame.  At least one dynamic base frame must be used.
C
C
C     .                      'PHOBOS_RADIAL'                          /
 
C
C     Bodies acting as observers and targets.
C
      DATA                  BODIES                                    /
     .                      'PHOBOS',
     .                      'MARS',
     .                      'EARTH',
     .                      'MOON',
     .                      'SUN'                                     /
 
 
C
C     Coordinate systems used to represent constant vectors:
C
      DATA                  CORSYS                                    /
     .                      '''RECTANGULAR''',
     .                      '''LATITUDINAL''',
     .                      '''RA/DEC'''                              /
 
C
C     Euclidean basis vectors.
C
      DATA                  E                                         /
     .                      1.D0,  0.D0,  0.D0,
     .                      0.D0,  1.D0,  0.D0,
     .                      0.D0,  0.D0,  1.D0                        /
 
C
C     The primary vector is always defined using Phobos as the
C     observer and Mars as the target.  The secondary vector
C     may have the same observer and target (used when the second
C     vector is a velocity vector), the same observer and a different
C     target, or different observer and target.
C
      DATA                  OTPAIR                                    /
     .                      1,  2,  1,  2,
     .                      1,  2,  1,  3,
     .                      1,  2,  3,  4,
     .                      3,  4,  3,  4,
     .                      3,  5,  3,  4                             /
 
C
C     VECDEF contains the possible vector definitions.
C
      DATA                  VECDEF                                    /
     .                      'OBSERVER_TARGET_POSITION',
     .                      'TARGET_NEAR_POINT',
     .                      'OBSERVER_TARGET_VELOCITY',
     .                      'CONSTANT'                                /
 
C
C     VECFRM contains frames associated with vectors.  Only velocity
C     and constant vectors actually use frame definitions.
C
      DATA                  VECFRM                                    /
     .                      'J2000',
     .                      'GSE',
     .                      'PHOBOS_RADIAL',
     .                      'TARGET_FRAME',
     .                      'OBSERVER_FRAME'                          /
 
 
C
C     When constant vectors are used and the coordinate system
C     is rectangular, the vectors are picked from this set.
C
      DATA                  VECSTR                                    /
     .                      '( 1,  0,  1 )',
     .                      '( 1,  1,  0 )',
     .                      '( 0,  1,  1 )'                           /
 
C
C     These strings are used for latitudinal coordinates.
C
      DATA                  ANGSTR                                    /
     .                      '-69.761',
     .                      ' 78.565',
     .                      '-1',
     .                      ' 1'                                      /
 
      DATA                  UNITS                                     /
     .                      '''DEGREES''',
     .                      '''RADIANS'''                             /
 
C
C     DIMS defines the dimensions of the cartesian product of the
C     input parameters.  The cardinality of the set comprising the
C     Nth "factor" of the cartesian product is DIMS(N).  The cardinality
C     of the product itself is the product of the elements of DIMS.
C
      DATA                  DIMS                                      /
     .                      NABPRS,
     .                      NBASEF,
     .                      NOTPRS,
     .                      NXVCDF,
     .                      NXVCDF                                    /
 
 
      DATA                  ZR   / 9 * 0.D0 /
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_DYN01' )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create test inputs for comprehensive two-vector ' //
     .             'test.'                                             )
 
C
C     Create and load kernels.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL TSTSPK ( SPK, .TRUE., HANDLE  )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Modify the prime meridian constants for Phobos to
C     make the rotation period deviate more from the orbital
C     period.
C
      CALL VPACK  ( 35.06D0,      100.D0,   0.D0,   V )
      CALL PDPOOL ( 'BODY401_PM', 3,                V )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     We'll need to create two non-inertial frames:  one to
C     use as a base frame and one to use as a frame associated
C     with velocity and constant vectors.  We'll the GSE
C     frame as one and a Mars radial frame as the other.
C
      DEFTXT(1) =
     .'FRAME_GSE                        =  2399000'
      DEFTXT(2) =
     .'FRAME_2399000_NAME               = ''GSE'''
      DEFTXT(3) =
     .'FRAME_2399000_CLASS              =  5'
      DEFTXT(4) =
     .'FRAME_2399000_CLASS_ID           =  2399000'
      DEFTXT(5) =
     .'FRAME_2399000_CENTER             =  399'
      DEFTXT(6) =
     .'FRAME_2399000_RELATIVE           = ''J2000'''
      DEFTXT(7) =
     .'FRAME_2399000_'//KWSTYL//'       = '''// KVPARM //''''
      DEFTXT(8) =
     .'FRAME_2399000_'//KWFFAM//'             = '''// KV2VEC //''''
      DEFTXT(9) =
     .'FRAME_2399000_'//KWPRI//KWVAXI//'       = '''//KVX//''''
      DEFTXT(10) =
     .'FRAME_2399000_'//KWPRI//KWVCDF//'       = '''//KVPOSV//''''
      DEFTXT(11) =
     .'FRAME_2399000_'//KWPRI//KWVOBS//'       = ''EARTH'''
      DEFTXT(12) =
     .'FRAME_2399000_'//KWPRI//KWVTRG//'         = ''SUN'''
      DEFTXT(13) =
     .'FRAME_2399000_'//KWPRI//KWVABC//'         = ''NONE'''
      DEFTXT(14) =
     .'FRAME_2399000_'//KWSEC//KWVAXI//'       = ''-'//KVY//''''
      DEFTXT(15) =
     .'FRAME_2399000_'//KWSEC//KWVCDF//'       =  '''// KVVELV //''''
      DEFTXT(16) =
     .'FRAME_2399000_'//KWSEC//KWVOBS//'       = ''SUN'''
      DEFTXT(17) =
     .'FRAME_2399000_'//KWSEC//KWVTRG//'       = ''EARTH'''
      DEFTXT(18) =
     .'FRAME_2399000_'//KWSEC//KWVABC//'         = ''NONE'''
      DEFTXT(19) =
     .'FRAME_2399000_'//KWSEC//KWVFRM//'          = ''J2000'''
      DEFTXT(20) =
     .'FRAME_2399000_'//KWRSTA//'       =  '''// KVROTG //''''
 
C
C     Load the GSE frame definition.
C
      CALL LMPOOL ( DEFTXT, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Define the PHOBOS_RADIAL frame.
C
      DEFTXT(1) =
     .'FRAME_PHOBOS_RADIAL              =  2401000'
      DEFTXT(2) =
     .'FRAME_2401000_NAME               = ''PHOBOS_RADIAL'''
      DEFTXT(3) =
     .'FRAME_2401000_CLASS              =  5'
      DEFTXT(4) =
     .'FRAME_2401000_CLASS_ID           =  2401000'
      DEFTXT(5) =
     .'FRAME_2401000_CENTER             =  401'
      DEFTXT(6) =
     .'FRAME_2401000_RELATIVE           = ''IAU_MARS'''
      DEFTXT(7) =
     .'FRAME_2401000_'//KWSTYL//'       = '''// KVPARM //''''
      DEFTXT(8) =
     .'FRAME_2401000_'//KWFFAM//'             = '''// KV2VEC //''''
      DEFTXT(9) =
     .'FRAME_2401000_'//KWPRI//KWVAXI//'       = '''//KVZ//''''
      DEFTXT(10) =
     .'FRAME_2401000_'//KWPRI//KWVCDF//'       = '''//KVPOSV//''''
      DEFTXT(11) =
     .'FRAME_2401000_'//KWPRI//KWVOBS//'       = ''PHOBOS'''
      DEFTXT(12) =
     .'FRAME_2401000_'//KWPRI//KWVTRG//'         = ''MARS'''
      DEFTXT(13) =
     .'FRAME_2401000_'//KWPRI//KWVABC//'         = ''NONE'''
      DEFTXT(14) =
     .'FRAME_2401000_'//KWSEC//KWVAXI//'       = ''-'//KVX//''''
      DEFTXT(15) =
     .'FRAME_2401000_'//KWSEC//KWVCDF//'       =  '''// KVVELV //''''
      DEFTXT(16) =
     .'FRAME_2401000_'//KWSEC//KWVOBS//'       = ''PHOBOS'''
      DEFTXT(17) =
     .'FRAME_2401000_'//KWSEC//KWVTRG//'       = ''MARS'''
      DEFTXT(18) =
     .'FRAME_2401000_'//KWSEC//KWVABC//'         = ''NONE'''
      DEFTXT(19) =
     .'FRAME_2401000_'//KWSEC//KWVFRM//'          = ''J2000'''
      DEFTXT(20) =
     .'FRAME_2401000_'//KWRSTA//'       =  '''// KVROTG //''''
 
C
C     Load the PHOBOS_RADIAL frame definition.
C
      CALL LMPOOL ( DEFTXT, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C--- Case: ------------------------------------------------------
C
C
C     First test:  examine the GSE frame.
C
      CALL TCASE ( 'Check GSE-to-J2000 frame transformation.' )
 
      ET = 1.D7
 
      CALL SXFORM ( 'GSE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Perform "sanity checks" on the returned matrix. Make sure the
C     diagonal blocks are identical rotations, compute a discrete
C     derivative and compare to the lower left block, and make sure the
C     upper right block is a zero matrix.
C
      DELTA = 1.D0
 
      CALL PXFORM ( 'GSE', 'J2000', ET-DELTA, RMINUS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PXFORM ( 'GSE', 'J2000', ET+DELTA, RPLUS  )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA,
     .               NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )
 
C
C     Check the error measurements we've made. First up is the
C     determinant error.
C
      TOL = DM14
      CALL CHCKSD ( 'DETERR', DETERR, '~', 0.D0, TOL, OK )
 
C
C     Check norms of rows and columns in the rotation blocks.
C
      CALL CHCKSD ( 'NRMERR', NRMERR, '~', 0.D0, TOL, OK )
 
C
C     Check the absolute derivative error.
C
      TOL = 8*DM14
      CALL CHCKSD ( 'DRVERR', DRVERR, '~', 0.D0, TOL, OK )
 
C
C     Check the relative derivative error.
C
      TOL = 0.6D0 * DM7
      CALL CHCKSD ( 'DRLERR', DRLERR, '~', 0.D0, TOL, OK )
 
 
 
C
C--- Case: ------------------------------------------------------
C
C
C     Second test:  examine the PHOBOS_RADIAL frame.
C
      CALL TCASE ( 'Check PHOBOS_RADIAL-to-IAU_MARS ' //
     .             'frame transformation.'            )
 
      ET = 1.D7
 
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Perform "sanity checks" on the returned matrix. Make sure the
C     diagonal blocks are identical rotations, compute a discrete
C     derivative and compare to the lower left block, and make sure the
C     upper right block is a zero matrix.
C
      DELTA = 1.D0
 
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET-DELTA, RMINUS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET+DELTA, RPLUS  )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA,
     .               NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )
 
C
C     Check the error measurements we've made. First up is the
C     determinant error.
C
      TOL = DM14
      CALL CHCKSD ( 'DETERR', DETERR, '~', 0.D0, TOL, OK )
 
C
C     Check norms of rows and columns in the rotation blocks.
C
      CALL CHCKSD ( 'NRMERR', NRMERR, '~', 0.D0, TOL, OK )
 
C
C     Check the absolute derivative error.
C
      TOL = DM9
      CALL CHCKSD ( 'DRVERR', DRVERR, '~', 0.D0, TOL, OK )
 
C
C     Check the relative derivative error.
C
      TOL = 5 * DM7
      CALL CHCKSD ( 'DRLERR', DRLERR, '~', 0.D0, TOL, OK )
 
 
C
C--- Case: ------------------------------------------------------
C
C
C     Create an inertial version of the PHOBOS_RADIAL frame.
C
      CALL TCASE  ( 'Test INERTIAL version of PHOBOS_RADIAL frame.' )
 
      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )
 
      CALL REPMC  ( DEFTX2(20), KVROTG, KVINRT, DEFTX2(20) )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTX2, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, XF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the PHOBOS_RADIAL to IAU_MARS transformation
C     defined using this frame to that using the standard
C     PHOBOS_RADIAL frame.  Just compare the rotation
C     blocks.
C
      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XFORM(I,J)
            R2(I,J) = XF2  (I,J)
         END DO
      END DO
 
      CALL CHCKAD ( 'Upper left block', R2, '~', R, 9, 0.D0, OK )
 
      DO I = 4, 6
         DO J = 4, 6
            R (I-3,J-3) = XFORM(I,J)
            R2(I-3,J-3) = XF2  (I,J)
         END DO
      END DO
 
      CALL CHCKAD ( 'Lower right block', R2, '~', R, 9, 0.D0, OK )
 
C
C     Check results from PXFORM.
C
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, R3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAD ( 'PXFORM rotation', R3, '~', R2, 9, DM14, OK )
 
C
C     Check the derivative matrix:  when we compose the transformation
C     XF2 with the IAU_MARS-to-J2000 transformation, we should
C     get a result with derivative block zero.
C
      CALL SXFORM ( 'IAU_MARS', 'J2000', ET, XF2000 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL MXMG   ( XF2000, XF2, 6, 6, 6,    XFB2J  )
 
      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XFB2J(3+I,J)
         END DO
      END DO
 
      CALL CHCKAD ( 'Derivative block', R, '~', ZR, 9, DM16, OK )
 
 
 
C
C--- Case: ------------------------------------------------------
C
C
C     Create a frozen version of the PHOBOS_RADIAL frame.
C
      CALL TCASE  ( 'Test FROZEN version of PHOBOS_RADIAL frame.' )
 
C
C     Expunge the optional ROTATN_STATE keyword from the kernel pool.
C
      CALL DVPOOL ( 'FRAME_2401000_'//KWRSTA  )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Initialize the frame definition.
C
      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )
C
C     Blank out the ROTATN_STATE assignment for this frame.
C
      DEFTX2(20) = ' '
 
C
C     Set the freeze epoch equal to 1.E7 seconds past J2000 TDB.
C
      DEFTX2(21) =
     .'FRAME_2401000_'//KWFREZ//'       = @2000-117/05:46:40'
 
      CALL LMPOOL ( DEFTX2, 21 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Although we look up XF2 at ET 0.D0, we should get the
C     same rotation we obtained from the standard PHOBOS_RADIAL
C     frame at ET 1.E7, except that the derivative block has
C     been zeroed out.
C
      CALL CLEARD ( 36, XF2 )
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, XF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Compare the PHOBOS_RADIAL to IAU_MARS transformation
C     defined using this frame to that using the standard
C     PHOBOS_RADIAL frame.  Just compare the rotation
C     blocks.
C
      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XFORM(I,J)
            R2(I,J) = XF2  (I,J)
         END DO
      END DO
 
      CALL CHCKAD ( 'Upper left block', R2, '~', R, 9, 0.D0, OK )
 
      DO I = 4, 6
         DO J = 4, 6
            R (I-3,J-3) = XFORM(I,J)
            R2(I-3,J-3) = XF2  (I,J)
         END DO
      END DO
 
      CALL CHCKAD ( 'Lower right block', R2, '~', R, 9, 0.D0, OK )
 
C
C     Check results from PXFORM.
C
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, R3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAD ( 'PXFORM rotation', R3, '~', R2, 9, DM14, OK )
 
C
C     Check the derivative matrix:  we should have derivative block
C     zero.
C
      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XF2(3+I,J)
         END DO
      END DO
 
      CALL CHCKAD ( 'Derivative block', R, '~', ZR, 9, DM16, OK )
 
 
C
C--- Case: ------------------------------------------------------
C
C
C     Create a new frozen frame that doesn't use a velocity
C     vector in its definition.  The secondary vector will be the
C     Mars-Sun vector. Start with the PHOBOS_RADIAL frame as a
C     template.
C
      CALL TCASE  ( 'Test FROZEN two-position vector frame.' )
 
C
C     Initialize the frame definition.
C
      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )
C
C     Blank out the ROTATN_STATE assignment for this frame.
C
      DEFTX2(20) = ' '
 
      CALL REPMC  ( DEFTX2(15), KVVELV,     KVPOSV,     DEFTX2(15) )
      CALL REPMC  ( DEFTX2(17), 'MARS',     'SUN',      DEFTX2(17) )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Set the freeze epoch equal to 1.E7 seconds past J2000 TDB.
C
      DEFTX2(21) =
     .'FRAME_2401000_'//KWFREZ//'       = @2000-117/05:46:40'
 
      CALL LMPOOL ( DEFTX2, 21 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Although we look up XF2 at ET 0.D0, we should get the
C     same rotation we obtained from the standard PHOBOS_RADIAL
C     frame at ET 1.E7, except that the derivative block has
C     been zeroed out.
C
      CALL CLEARD ( 36, XF2 )
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, XF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Get the upper left rotation block from XF2.
C
      DO I = 1, 3
         DO J = 1, 3
            R2(I,J) = XF2 (I,J)
         END DO
      END DO
 
C
C     Check results from PXFORM.
C
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, R3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAD ( 'PXFORM rotation', R3, '~', R2, 9, DM14, OK )
 
C
C     Check the derivative matrix:  we should have derivative block
C     zero.
C
      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XF2(3+I,J)
         END DO
      END DO
 
      CALL CHCKAD ( 'Derivative block', R, '~', ZR, 9, DM16, OK )
 
 
C
C--- Case: ------------------------------------------------------
C
C
C     Create a new frozen frame with linearly dependent defining
C     vectors.  Both primary and secondary vectors will be the
C     Phobos-Mars position vector. Start with the PHOBOS_RADIAL frame
C     as a template.
C
      CALL TCASE  ( 'Test linearly dependent two-position ' //
     .              'vector frame.' )
 
C
C     Initialize the frame definition.
C
      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )
C
C     Blank out the ROTATN_STATE assignment for this frame.
C
      DEFTX2(20) = ' '
 
      CALL REPMC  ( DEFTX2(15), KVVELV,     KVPOSV,     DEFTX2(15) )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTX2, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Make sure error is caught by both SXFORM and PXFORM.
C
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, XF2 )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )
 
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, R )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )
 
C
C     Expunge the optional FREEZE_EPOCH keyword from the kernel pool.
C
      CALL DVPOOL ( 'FRAME_2401000_'//KWFREZ  )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Restore the original PHOBOS_RADIAL frame definition.
C
      DEFTXT(21) = ' '
      CALL LMPOOL ( DEFTXT, 21 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Proceed to the general test cases.
C
C
C     Because there are so many options that may be used to define
C     a two-vector frame, we can't conveniently loop over every
C     set of possibilities using nested loops:  the loops would be
C     nested too deeply.  Instead, we define a mapping that allows
C     us to map a test case number to a set of input parameters.
C
C     First compute the number of test cases in the cartesian product
C     test.
C
 
      NCART = PRODAI ( DIMS, NDIMS )
 
 
 
C
C     All of the variables initialized here are used for debugging.
C
      NOGO   = 0
      NPR    = 0
C     NNORML = 0
      NCOR   = 0
      NSKIP  = 0
      MAXDER = 0.D0
 
 
      DO CASE = 1, NCART
C
C        Find the multi-dimensional coordinates of the current
C        case in the cartesian product of the test input sets.
C
         CALL MULTIX ( 1, NDIMS, DIMS, CASE, COORDS )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Set frame definition parameters defined by COORDS.
C        Start out with the base frame.
C
         BFRAME = BASEFR ( COORDS(BFRIDX) )
 
C
C        The second dimension refers to the observer-target pairs
C        for the primary and secondary vectors.  (For constant
C        vectors, only the observer is used, and this is only if
C        aberration corrections are used.)
C
         I      = OTPAIR ( 1, COORDS(OTPIDX) )
         J      = OTPAIR ( 2, COORDS(OTPIDX) )
         PRIOBS = BODIES ( I )
         PRITRG = BODIES ( J )
 
         I      = OTPAIR ( 3, COORDS(OTPIDX) )
         J      = OTPAIR ( 4, COORDS(OTPIDX) )
         SECOBS = BODIES ( I )
         SECTRG = BODIES ( J )
 
C
C        The third dimension refers to the extended vector
C        definition for the primary vector.  Extended definitions
C        include frame selections for velocity vectors and
C        constant vectors (those vectors requiring frames as
C        part of their definitions).  The ordering of extended
C        frame definitions is:
C
C            1           )  Observer-target position
C            2           )  Observer-near point position
C            3           )  Velocity vector, frame definition 1
C                             ...
C            2 + NVECFR  )  Velocity vector, frame definition NVECFR
C            3 + NVECFR  )  Constant vector, frame definition 1
C                             ...
C            2 + 2*NVECFR)  Constant vector, frame definition NVECFR
C
C        The fourth dimension provides the analogous information
C        for the secondary vector.
C
C        We'll process the definitions for both vectors in the loop
C        below.
C
 
         DO I = 1, 2
C
C           Indicate that we don't have a velocity vector or
C           constant vector until we know otherwise.
C
            ISVEL(I) = .FALSE.
            ISCON(I) = .FALSE.
 
            IF ( I .EQ. 1 ) THEN
               CORIDX = PRIIDX
            ELSE
               CORIDX = SECIDX
            END IF
 
            J  =  COORDS( CORIDX )
 
            IF ( I .EQ. 1 ) THEN
               PRIJ = J
            END IF
 
            IF ( J .EQ. 1 ) THEN
C
C              The vector is an observer-target position.
C
               VDF = VECDEF(1)
               VFR = ' '
 
            ELSE IF ( J .EQ. 2 ) THEN
 
C
C              The vector is an observer-near point position.
C
               VDF = VECDEF ( 2 )
               VFR = ' '
 
            ELSE IF (       ( J .GE. 3             )
     .                .AND. ( J .LE. 2 + NVECFR )  ) THEN
C
C              The vector is an observer-target velocity.
C              The quantity J-2 is an index into the vector
C              frame array.
C
               VDF      = VECDEF ( 3   )
               VFR      = VECFRM ( J-2 )
               ISVEL(I) = .TRUE.
 
            ELSE
C
C              The primary is constant in the specified frame.
C              The quantity J-2-NVECFR is an index into the vector
C              frame array.
C
               VDF      = VECDEF ( 4 )
               VFR      = VECFRM ( J-2-NVECFR )
               ISCON(I) = .TRUE.
 
            END IF
 
            IF ( I .EQ. 1 ) THEN
               PRIVDF = VDF
               PRIFRM = VFR
            ELSE
               SECVDF = VDF
               SECFRM = VFR
            END IF
 
C
C           Set the aberration correction for the current vector.
C
            J = ABPAIR ( I, COORDS(ABCIDX) )
 
C
C           If the current vector is constant, adjust the
C           aberration correction to make it valid for constant
C           vectors.
C
C           For the special case of constant vectors, the correction
C           'LT+S' is replaced with the correction 'LT', the correction
C           'XLT' is replaced with the correction 'S', and the
C           correction 'XLT+S' is replaced with the correction 'XS'.
C
            IF ( ISCON(I) ) THEN
 
               IF ( ABCORR(J) .EQ. 'LT+S' ) THEN
C
C                 Set the correction for this vector to 'LT'
C
                  J = ISRCHC ( 'LT', NABCRR, ABCORR )
 
               ELSE IF ( ABCORR(J) .EQ. 'XLT' ) THEN
C
C                 Set the correction for this vector to 'S'
C
                  J = ISRCHC ( 'S', NABCRR, ABCORR )
 
 
               ELSE IF ( ABCORR(J) .EQ. 'XLT+S' ) THEN
C
C                 Set the correction for this vector to 'XS'
C
                  J = ISRCHC ( 'XS', NABCRR, ABCORR )
 
               ELSE IF ( ABCORR(J) .EQ. 'NONE' ) THEN
C
C                 Set the correction for this vector to 'NONE'
C
                  J = ISRCHC ( 'NONE', NABCRR, ABCORR )
 
               ELSE
 
                  CALL SETMSG ( 'Unexpected aberration correction # '//
     .                          'was encountered when mapping '      //
     .                          'correction for constant vector #.'  //
     .                          'Case is #.'                         )
                  CALL ERRCH  ( '#',  ABCORR(J)                      )
                  CALL ERRINT ( '#',  I                              )
                  CALL ERRINT ( '#',  CASE                           )
                  CALL SIGERR ( 'SPICE(BUG)'                         )
 
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
                  RETURN
 
               END IF
 
            END IF
 
            IF ( I .EQ. 1 ) THEN
               PRICOR = ABCORR(J)
            ELSE
               SECCOR = ABCORR(J)
            END IF
 
         END DO
 
C
C        Decide whether we need to execute the current case.
C
C        There are certain cases we can skip:
C
C           1) Neither vector is a velocity vector and both
C              vectors have the same observer/target sets.
C              Such vector pairs will be too close to linearly
C              dependent to work with.
C
C           2) Both vectors are velocity vectors.
C
         GO = .TRUE.
 
 
C
C        Use this code to disable testing of frames defined by
C        a pair of velocity vectors.
C
C         IF ( ISVEL(1) .AND. ISVEL(2) ) THEN
C            GO = .FALSE.
C         END IF
 
         J  =  COORDS( PRIIDX )
         K  =  COORDS( SECIDX )
 
 
 
         IF (     (   ISVEL(1)   .AND.   ISVEL(2)    )
     .       .OR. ( ( J .LE. 2 ) .AND. ( K .LE. 2 )  )   )  THEN
C
C           The primary and secondary vectors are either both
C
C              - velocity vectors
C
C              - in the set
C
C                  { OBSERVER_TARGET_POSITION, TARGET_NEAR_POINT }
C
C           In this case we don't want both vectors to be defined
C           by the same pair of objects, because the vectors will
C           be linearly dependent or close to it.
C
            IF (      (      ( PRIOBS .EQ. SECOBS )
     .                  .OR. ( PRIOBS .EQ. SECTRG ) )
     .          .AND. (      ( PRITRG .EQ. SECOBS )
     .                  .OR. ( PRITRG .EQ. SECTRG ) )  ) THEN
 
               GO = .FALSE.
 
            END IF
 
         END IF
 
C
C        Another linear dependency problem:  if the vector frame
C        is PHOBOS_RADIAL, then we don't want cases where the observer
C        and target are from the set { Phobos, Mars } for both
C        vectors.
C
         IF (      ( PRIFRM .EQ. 'PHOBOS_RADIAL' )
     .        .OR. ( SECFRM .EQ. 'PHOBOS_RADIAL' ) ) THEN
 
C
C           Note: this code must change if the observer-target pairs
C           are changed!
C
            IF ( COORDS(OTPIDX) .EQ. 1 ) THEN
 
               GO  = .FALSE.
 
            END IF
 
         END IF
 
C
C        Similar problem for the earth and sun when the velocity
C        frame is GSE:
C
         IF (      ( PRIFRM .EQ. 'GSE' )
     .        .OR. ( SECFRM .EQ. 'GSE' ) ) THEN
 
C
C           Note: this code must change if the observer-target pairs
C           are changed!
C
            IF ( COORDS(OTPIDX) .EQ. 4 ) THEN
 
               GO  = .FALSE.
 
            END IF
 
         END IF
 
 
C
C         Bits used for debugging
C
         IF ( .NOT. GO ) THEN
            NOGO = NOGO + 1
         END IF
 
 
         IF ( GO ) THEN
C
C           Cycle over the vector-axis associations.  There's
C           no interaction between these parameters and the
C           other parameters, so we don't need to try every
C           possible combination of these values with the other
C           inputs.  We just need to use each association.
C
            AXPAIR =  MOD ( CASE, NAXDEF ) + 1
 
C
C           We're ready to create a frame definition.
C
C
C           Create a text buffer containing a frame definition that
C           uses the current frame definition
C
C
C           First comes the frame name-to-frame code assignment.
C
            FRNAME     = 'TWOVEC_DYN_FRAME'
            FRCODE     = -2000000000
 
            DEFTXT(1)  = 'FRAME_#          = #'
            CALL REPMC  ( DEFTXT(1), '#', FRNAME, DEFTXT(1) )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI  ( DEFTXT(1), '#', FRCODE, DEFTXT(1) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           We'll use the kernel variable name "stem"
C
C              FRAME_<ID code>_
C
C           repeatedly, so instead of making thousands of REPMI
C           calls, we just create this stem here.
C
            CALL REPMI ( 'FRAME_#_', '#', FRCODE, FRSTEM )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
 
            DEFTXT(2)  =  FRSTEM // 'NAME             = ''#'''
            CALL REPMC  ( DEFTXT(2), '#', FRNAME, DEFTXT(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Frame class and class ID come next.
C
            DEFTXT(3)   =  FRSTEM // 'CLASS            = 5'
 
            DEFTXT(4)   =  FRSTEM // 'CLASS_ID         = #'
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI   ( DEFTXT(4), '#', FRCODE, DEFTXT(4) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           The frame center is always the primary vector's
C           observer, for now.  The center must be specified
C           via a code.
C
            DEFTXT(5)   =  FRSTEM // 'CENTER           = #'
 
            CALL BODN2C ( PRIOBS, J, FOUND )
 
            IF ( .NOT. FOUND ) THEN
               CALL SETMSG ( 'No ID code for #.' )
               CALL ERRCH  ( '#', PRIOBS         )
               CALL SIGERR ( 'SPICE(BUG)'        )
            END IF
 
            CALL REPMI  ( DEFTXT(5), '#',  J, DEFTXT(5) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Set the base frame.
C
            DEFTXT(6)   =  FRSTEM // 'RELATIVE         = ''#'''
            CALL REPMC  ( DEFTXT(6), '#', BFRAME, DEFTXT(6) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Set the frame definition style and family.
C
            DEFTXT(7)  =
     .      FRSTEM // KWSTYL//'       = '''// KVPARM //''''
            CALL REPMI  ( DEFTXT(7), '#', FRCODE,    DEFTXT(7) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
            DEFTXT(8)  =
     .      FRSTEM // KWFFAM//'             = '''// KV2VEC //''''
            CALL REPMI  ( DEFTXT(8), '#', FRCODE,    DEFTXT(8) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
            DEFTXT(9)  =  FRSTEM//KWRSTA//'       =  '''// KVROTG //''''
            CALL REPMI  ( DEFTXT(9), '#', FRCODE,    DEFTXT(9) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Create definition assignments for the primary and secondary
C           axes.
C
C           At this point, we'll have to abandon hard-coded array
C           indices for the DEFTXT array and instead use the variable
C           DX to represent the current index.
C
            DX = 10
C
C           Blank out the variables containing associated frame names,
C           so we don't end up with residual values from other test
C           cases in these names.
C
            PRIVF  = ' '
            SECVF  = ' '
            VFRAME = ' '
 
 
            DO I = 1, 2
 
               IF ( I .EQ. 1 ) THEN
C
C                 On the first pass, set up definitions for the
C                 primary vector.
C
                  ORD  = 'PRI_'
                  VDF  = PRIVDF
                  VFR  = PRIFRM
                  COR  = PRICOR
                  OBS  = PRIOBS
                  TARG = PRITRG
 
               ELSE
C
C                 On the second pass, set up definitions for the
C                 secondary vector.
C
                  ORD  = 'SEC_'
                  VDF  = SECVDF
                  VFR  = SECFRM
                  COR  = SECCOR
                  OBS  = SECOBS
                  TARG = SECTRG
 
               END IF
 
C
C              Identify the axis { +/-X, +/-Y, +/-Z } associated
C              with the current vector.
C
               DEFTXT(DX) =  FRSTEM // ORD // 'AXIS         = ''#'''
 
               CALL REPMC  ( DEFTXT(DX),      '#',
     .                       AXDEF(I,AXPAIR), DEFTXT(DX) )
 
 
               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1
 
C
C              Set the current vector definition.
C
               DEFTXT(DX) =  FRSTEM // ORD // KWVCDF // '     = ''#'''
               CALL REPMC  ( DEFTXT(DX), '#', VDF, DEFTXT(DX) )
               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1
 
C
C              Set the current vector observer.
C
               DEFTXT(DX) =  FRSTEM // ORD // KWVOBS // '     = ''#'''
               CALL REPMC  ( DEFTXT(DX), '#', OBS, DEFTXT(DX) )
               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1
 
C
C              Set the current vector target.  Constant vectors
C              don't have targets.
C
               IF ( VDF .NE. 'CONSTANT' ) THEN
 
                  DEFTXT(DX) =  FRSTEM // ORD// KWVTRG // '     = ''#'''
                  CALL REPMC  ( DEFTXT(DX), '#', TARG, DEFTXT(DX) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  DX         =  DX + 1
 
               END IF
C
C              Set the current vector's aberration correction.
C
               DEFTXT(DX) =  FRSTEM // ORD// KWVABC // '     = ''#'''
 
               CALL REPMC  ( DEFTXT(DX), '#', COR, DEFTXT(DX) )
               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1
 
C
C              Set the current vector's frame, if necessary.
C              Velocity vectors and constant vectors have associated
C              frames.
C
               IF (     ( VDF .EQ. 'OBSERVER_TARGET_VELOCITY' )
     .             .OR. ( VDF .EQ. 'CONSTANT'                 ) ) THEN
 
                  IF (      ( VFR .EQ. 'J2000'         )
     .                 .OR. ( VFR .EQ. 'IAU_JUPITER'   )
     .                 .OR. ( VFR .EQ. 'GSE'           )
     .                 .OR. ( VFR .EQ. 'IAU_PHOBOS'    )
     .                 .OR. ( VFR .EQ. 'PHOBOS_RADIAL' ) ) THEN
 
                     VFRAME = VFR
 
                  ELSE IF ( VFR .EQ. 'BASE_FRAME' ) THEN
 
                     VFRAME = BFRAME
 
                  ELSE IF ( VFR .EQ. 'OBSERVER_FRAME' ) THEN
 
                     CALL CNMFRM ( OBS, VFCODE, VFRAME, FOUND )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
                     IF ( .NOT. FOUND ) THEN
                        CALL SETMSG ( 'No frame info for #.' )
                        CALL ERRCH  ( '#', OBS               )
                        CALL SIGERR ( 'SPICE(BUG)'           )
                     END IF
 
                  ELSE IF ( VFR .EQ. 'TARGET_FRAME' ) THEN
 
                     CALL CNMFRM ( TARG, VFCODE, VFRAME, FOUND )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
                     IF ( .NOT. FOUND ) THEN
                        CALL SETMSG ( 'No frame info for #.' )
                        CALL ERRCH  ( '#', TARG              )
                        CALL SIGERR ( 'SPICE(BUG)'           )
                     END IF
 
                  ELSE
                     CALL SETMSG( 'Unexpected velocity frame token: #' )
                     CALL ERRCH ( '#',  VFR                            )
                     CALL SIGERR( 'SPICE(BUG)'                         )
                  END IF
 
C
C                 Save the resolved frame names for later testing.
C
                  IF ( I .EQ. 1 ) THEN
                     PRIVF = VFRAME
                  ELSE
                     SECVF = VFRAME
                  END IF
 
                  DEFTXT(DX) = FRSTEM //ORD// KWVFRM //'        = ''#'''
 
                  CALL REPMC  ( DEFTXT(DX), '#', VFRAME, DEFTXT(DX) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  DX         =  DX + 1
 
               END IF
 
C
C              If the vector is constant, define it.  The vector
C              definitions have no interaction with the other defining
C              parameters other than the vector order, so all we need
C              do is make sure all definitions are covered for the
C              primary and secondary vectors.  We'll use the following
C              definitions:
C
C                 ... SPEC      = 'RECTANGULAR'
C                 ... VECTOR    = ( <number1>, <number2>, <number3> )
C
C                 ... SPEC      = 'RECTANGULAR'
C                 ... VECTOR    = ( <number4>, <number5>, <number6> )
C
C                 ... SPEC      = 'RECTANGULAR'
C                 ... VECTOR    = ( <number7>, <number8>, <number9> )
C
C                 ... SPEC      = 'LATITUDINAL'
C                 ... UNITS     = 'DEGREES'
C                 ... LONGITUDE = <number>
C                 ... LATITUDE  = <number>
C
C                 ... SPEC      = 'LATITUDINAL'
C                 ... UNITS     = 'RADIANS'
C                 ... LONGITUDE = <number>
C                 ... LATITUDE  = <number>
C
C
C
               IF ( VDF .EQ. 'CONSTANT' ) THEN
 
                  IF ( I .EQ. 1 ) THEN
                     NCOR = NCOR + 1
                  END IF
 
C
C                 Make the vector selection depend on the case
C                 and whether the axis is primary or secondary.
C
                  J    = MOD ( NCOR + I, 5 ) + 1
 
                  IF ( J .LE. 3 ) THEN
C
C                    This is the rectangular coordinate case.
C
                     SYSIDX     =  1
 
                     DEFTXT(DX) =  FRSTEM // ORD // KWVSPC //
     .                             '         = ' // CORSYS(SYSIDX)
                     DX         =  DX + 1
 
 
                     DEFTXT(DX) =  FRSTEM // ORD // KWVECT //
     .                             '       = '
 
                     CALL SUFFIX ( VECSTR(J), 1,  DEFTXT(DX) )
                     DX         =  DX + 1
 
C
C                    Produce the d.p. constant vector; we'll use this
C                    in later testing.  Start by parsing the string
C                    representing the vector.  The first token will
C                    be a blank (representing the null token to the
C                    left of the left parenthesis), so start with
C                    token #2.
C
                     CALL LPARSM ( VECSTR(J), '(),', 7, N, TOKENS )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
                     DO L = 2, 4
                        CALL PRSDP ( TOKENS(L), CONVEC(L-1) )
                        CALL CHCKXC ( .FALSE., ' ', OK )
                     END DO
 
                  ELSE
C
C                    This is one of the latitudinal or RA/DEC
C                    coordinate cases.
C
                     IF ( J .EQ. 4 ) THEN
C
C                       Use latitudinal coordinates.
C
                        SYSIDX =  2
                     ELSE
C
C                       Use RA/DEC coordinates.
C
                        SYSIDX =  3
                     END IF
 
                     DEFTXT(DX) =  FRSTEM // ORD // KWVSPC //
     .                             '         = ' // CORSYS(SYSIDX)
                     DX         =  DX + 1
 
C
C                    Let K be the index of the units.
C
                     K          = J - 3
 
                     DEFTXT(DX) =  FRSTEM // ORD // KWUNIT //
     .                             '        = ' // UNITS(K)
                     DX         =  DX + 1
 
                     IF ( J .EQ. 4 ) THEN
                        DEFTXT(DX) =  FRSTEM // ORD // KWLON //
     .                                '    = '
                     ELSE
                        DEFTXT(DX) =  FRSTEM // ORD // KWRA //
     .                                '           = '
                     END IF
 
                     CALL SUFFIX ( ANGSTR(1,K), 1,  DEFTXT(DX) )
                     CALL CHCKXC ( .FALSE., ' ', OK )
C
C                    Save the longitude as a d.p. number.
C
                     CALL PRSDP ( ANGSTR(1,K), LON0 )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     DX         =  DX + 1
 
                     IF ( J .EQ. 4 ) THEN
                        DEFTXT(DX) =  FRSTEM // ORD // KWLAT //
     .                                '     = '
                     ELSE
                        DEFTXT(DX) =  FRSTEM // ORD // KWDEC //
     .                                '          = '
                     END IF
 
                     CALL SUFFIX ( ANGSTR(2,K),   1,  DEFTXT(DX) )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     DX         =  DX + 1
C
C                    Save the latitude as a d.p. number.
C
                     CALL PRSDP ( ANGSTR(2,K), LAT0 )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                    Convert the numeric lat/lon values to radians.
C
                     CALL CMPRSS ( '''', 0, UNITS(K), TMPSTR )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
                     CALL CONVRT ( LAT0, TMPSTR, KVRADN, LAT )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL CONVRT ( LON0, TMPSTR, KVRADN, LON )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                    Produce a d.p. constant vector.
C
                     CALL LATREC ( 1.D0, LON, LAT, CONVEC )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
                  END IF
C
C                 Save the constant vector as one of the defining
C                 vectors.
C
                  IF ( I .EQ. 1 ) THEN
                     CALL VEQU ( CONVEC, PRIVEC )
                  ELSE
                     CALL VEQU ( CONVEC, SECVEC )
                  END IF
 
               END IF
 
            END DO
 
 
 
C
C           Enter the frame definition into the kernel pool.
C
            CALL LMPOOL ( DEFTXT, DX-1 )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
 
 
 
 
C
C           Here's where the testing begins.
C
C
C--- Case: ------------------------------------------------------
C
            CALL REPMI  ( 'SXFORM test number #.  Test results ' //
     .                    'against those from PXFORM.',
     .                    '#', CASE, CASMSG )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )
 
C
C           Pick a time value.
C
            ET = 1.D3 * ( CASE - 5000 )
 
C            WRITE (*,*) 'F_DYN01:  SXFORM CALL <<<<<<<<<<<<<<<<<<'
 
            CALL SXFORM ( FRNAME, BFRAME, ET, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C           IF ( .NOT. OK ) THEN
C              DO I = 1, DX-1
C                 WRITE (*,*) DEFTXT(I)
C              END DO
C
C              WRITE (*,*) '=================================== '
C           END IF
C
 
C            WRITE (*,*) 'F_DYN01:  DONE <<<<<<<<<<<<<<<<<<'
 
            CALL PXFORM ( FRNAME, BFRAME, ET, R )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Extract the derivative block from XFORM into DRVBLK.
C           We'll use this block later; we're extracting it here
C           to keep this code close to the associated SXFORM call.
C
            DO I = 1, 3
               DO J = 1, 3
                  DRVBLK(J,I) = XFORM(J+3,I)
               END DO
            END DO
 
            DMAG = VNORMG( DRVBLK, 9 )
 
C
C           Extract the rotation block from XFORM into R2.
C
            DO I = 1, 3
               DO J = 1, 3
                  R2(J,I) = XFORM(J,I)
               END DO
            END DO
 
            CALL CHCKAD ( 'SXFORM vs PXFORM', R2, '~', R, 9, DM12, OK )
 
C
C               These statements may be useful for debugging.
C
C
C               IF ( .NOT. OK ) THEN
C
C                  WRITE (*,*) '=================================== '
C                  WRITE (*,*) 'CASE = ', CASE
C                  WRITE (*,*) COORDS
C                  WRITE (*,*) 'PRIFRM  = ', PRIFRM
C                  WRITE (*,*) 'PRIJ    = ', PRIJ
C                  WRITE (*,*) 'PRIVF   = ', PRIVF
C                  WRITE (*,*) 'VDF     = ', VDF
C                  WRITE (*,*) 'VFRAME  = ', VFRAME
C                  WRITE (*,*) 'DRVERR, DRLERR  = ', DRVERR, DRLERR
C                  WRITE (*,*) 'DMAG    = ', DMAG
C                  WRITE (*,*) 'DM16  /DMAG    = ', DM16  /DMAG
C
C                 DO I = 1, DX-1
C                    WRITE (*,*) DEFTXT(I)
C                 END DO
C
C                 WRITE (*,*) '=================================== '
C
C               END IF
C
 
 
C
C--- Case: ------------------------------------------------------
C
            CALL REPMI  ( 'PXFORM test number #.  ' //
     .                    'Primary vector def = #.',
     .                            '#', CASE,   CASMSG )
            CALL REPMC  ( CASMSG, '#', PRIVDF, CASMSG )
 
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )
 
C
C           Find the primary axis name and sign.
C
            AXNAME = AXDEF ( 1, AXPAIR )
 
            IF ( INDEX( AXNAME, '-' ) .GT. 0 ) THEN
               AXSIGN = -1.D0
            ELSE
               AXSIGN =  1.D0
            END IF
 
            CALL CMPRSS ( '-', 0, AXNAME, AXNAME )
 
            IF ( EQSTR(AXNAME, 'X') ) THEN
               AXINDX = 1
            ELSE IF ( EQSTR(AXNAME, 'Y') ) THEN
               AXINDX = 2
            ELSE
               AXINDX = 3
            END IF
 
C
C           Create the primary vector, specified relative to the
C           base frame.  Map this vector to the current dynamic
C           frame.  The vector should match the current axis
C           specification for the primary defining vector.
C
            IF ( PRIVDF .EQ. 'OBSERVER_TARGET_POSITION' ) THEN
 
               CALL SPKPOS ( PRITRG, ET, 'J2000', PRICOR,
     .                       PRIOBS, V,  LT              )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL PXFORM ( 'J2000', FRNAME, ET, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXV  ( R2,      V,     VTEMP )
               CALL VSCL ( AXSIGN, VTEMP,  V     )
               CALL VHAT ( V,      PRIVEC        )
 
C
C              PRIVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12 ,  OK )
 
 
            ELSE IF ( PRIVDF .EQ. 'TARGET_NEAR_POINT' ) THEN
C
C              Find the specified near point.
C
               CALL SUBPT ( 'NEAR POINT', PRITRG, ET, PRICOR,
     .                      PRIOBS,       SPOINT, ALT         )
 
C
C              We actually need to use the position transformation
C              from PRIVF to FRMNAM for this one.  NOTE:  the
C              epoch of this transformation is critical.  We must
C              use the same epoch as was used by SUBPT.
C
C              Get the light time LT from the observer to target
C              center.
C
               CALL SPKPOS ( PRITRG, ET,    'J2000', PRICOR,
     .                       PRIOBS, POS,   LT               )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Find the epoch associated with the target by
C              adjusting ET by LT.
C
               CALL ZZCOREPC ( PRICOR, ET, LT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Find the position transformation from the target
C              body-fixed frame at ETCORR to J2000.  Convert
C              the body-fixed sub-point position to the J2000
C              frame.
C
               CALL CNMFRM ( PRITRG, J, TRGFRM, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'TARGET FRAME FOUND', FOUND, .TRUE., OK )
 
               CALL PXFORM ( TRGFRM,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXV    ( R2, SPOINT, V )
 
C
C              Compute the observer-to-near point vector in the J2000
C              frame.
C
               CALL VADD ( V, POS, VTEMP )
 
C
C              Find the position transformation J2000 the current frame
C              at ET.  Rotate the observer-near point vector into this
C              frame.
C
               CALL PXFORM ( 'J2000', FRNAME, ET, R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXV    ( R3,      VTEMP,  V )
 
C
C              Negate the vector if indicated, then unitize.
C
               CALL VSCL ( AXSIGN,    V,      VTEMP  )
               CALL VHAT ( VTEMP,     PRIVEC         )
 
C
C              PRIVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12,  OK )
 
 
            ELSE IF ( PRIVDF .EQ. 'OBSERVER_TARGET_VELOCITY' ) THEN
C
C              Look up the velocity vector in the specified frame.
C              Note this choice is critical:  any other frame,
C              unless related by a constant offset to the correct
C              frame, yields an invalid result.
C
               CALL SPKEZR ( PRITRG, ET,    PRIVF, PRICOR,
     .                       PRIOBS, STATE, LT            )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              We actually need to use the position transformation
C              from PRIVF to FRMNAM for this one.  NOTE:  the
C              epoch of this transformation is critical.  We must
C              use the same epoch as was used by SPKEZR.  If
C              PRIVF is non-inertial, we need to adjust ET by
C              the light time from the observer to the center of PRIVF.
C              We can actually finesse the issue of whether the frame
C              is non-inertial, since performing the correction for
C              an inertial frame has no effect.
C
               CALL NAMFRM ( PRIVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )
 
               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )
 
C
C              Get the light time CLT from the observer to the center
C              of the frame in which we looked up the velocity.
C
               CALL SPKEZR ( CENTER, ET,     PRIVF, PRICOR,
     .                       PRIOBS, CSTATE, CLT            )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C              Find the epoch associated with the center of the
C              frame by adjusting ET by CLT.
C
               CALL ZZCOREPC ( PRICOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Find the position transformation from PRIVF at
C              ETCORR to the current frame at ET.  We must do
C              this in two steps, passing through J2000.
C
               CALL PXFORM ( PRIVF,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXM    ( R3,      R2,     R4         )
C
C              Extract the velocity vector.  Rotate to the current
C              frame, negate if indicated, then unitize.
C
               CALL MXV  ( R4,       STATE(4),  V      )
               CALL VSCL ( AXSIGN,   V,         VTEMP  )
               CALL VHAT ( VTEMP,    PRIVEC            )
 
C
C              PRIVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12,  OK )
 
 
 
            ELSE IF ( PRIVDF .EQ. 'CONSTANT' ) THEN
C
C              The primary vector is a constant; the vector is
C              stored in PRIVEC.
C
C              We need to use the position transformation from PRIVF
C              to FRMNAM for this one.  NOTE:  the epoch of this
C              transformation is critical. If PRIVF is non-inertial, we
C              need to adjust ET by the light time from the observer to
C              the center of PRIVF. We can actually finesse the issue
C              of whether the frame is non-inertial, since performing
C              the correction for an inertial frame has no effect.
C
               CALL NAMFRM ( PRIVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )
 
               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )
 
C
C              Parse the aberration correction for the primary
C              vector.  Get the light time to the frame center if
C              light time corrections are used.
C
               CALL ZZPRSCOR ( PRICOR, PRIBLK )
 
               IF ( PRIBLK(LTIDX) ) THEN
C
C                 Get the light time CLT from the observer to the
C                 center of the frame in which the constant vector is
C                 defined.
C
                  CALL SPKEZR ( CENTER, ET,     PRIVF, PRICOR,
     .                          PRIOBS, CSTATE, CLT            )
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
               ELSE
C
C                 The frame is not corrected for light time:
C                 the light time to the frame center is treated as
C                 zero.
C
                  CLT = 0.D0
 
               END IF
 
C
C              Find the epoch associated with the center of the
C              frame by adjusting ET by CLT.
C
               CALL ZZCOREPC ( PRICOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              We now want to express the constant vector in
C              the current dynamic frame at ET.  We must
C              transform the constant vector in two steps,
C              since the epoch associated with the frame in
C              which the constant vector is specified is ETCORR.
C              We first map the constant vector to the J2000
C              frame, then to the current frame.
C
C              When the constant vector is expressed relative
C              to the J2000 frame, we apply the stellar aberration
C              correction if necessary.
C
C              Find the position transformation from PRIVF at
C              ETCORR to J2000.  Apply to PRIVEC.
C
               CALL PXFORM ( PRIVF,   'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ',     OK )
 
               CALL MXV    ( R2,      PRIVEC,  V  )
 
               IF ( PRIBLK(STLIDX) ) THEN
C
C                 Stellar aberration correction is required.
C
C                 Find the state of the observer relative to the
C                 solar system barycenter at ET, in the J2000 frame.
C
                  CALL BODN2C ( PRIOBS,  J,    FOUND )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )
 
                  CALL SPKSSB ( J,       ET,  'J2000', STOBS )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )
 
                  IF ( PRIBLK(XMTIDX) ) THEN
C
C                    Apply transmission stellar aberration correction.
C
                     CALL STLABX ( V, STOBS(4), VTEMP )
 
                  ELSE
C
C                    Apply transmission stellar aberration correction.
C
                     CALL STELAB ( V, STOBS(4), VTEMP )
 
                  END IF
 
                  CALL VEQU ( VTEMP, V )
 
               END IF
C
C              The vector V contains our corrected (if necessary)
C              constant vector relative to J2000.
C
C              Look up the rotation from J2000 to the current frame
C              at ET.  Apply to our constant vector.
C
               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ',    OK         )
               CALL MXV    ( R3,      V,      PRIVEC     )
 
C
C              Negate the constant vector if indicated, then unitize.
C
               CALL VSCL ( AXSIGN,   PRIVEC,  VTEMP  )
               CALL VHAT ( VTEMP,    PRIVEC          )
 
C
C              PRIVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12 ,  OK )
 
 
            ELSE
C
C              We have an unrecognized vector definition.
C
               CALL SETMSG ( 'F_DYN01 encountered vector ' //
     .                       'definition #.'               )
               CALL ERRCH  ( '#',  PRIVDF                  )
               CALL SIGERR ( 'SPICE(BUG)'                  )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
            END IF
 
 
 
C
C
C           We're now going to test the secondary vector.  We could
C           have combined this code in a loop together with the tests
C           for the primary vector, but "unrolling" the loop makes
C           the test code a bit easier to look at.
C
C
C--- Case: ------------------------------------------------------
C
            CALL REPMI  ( 'PXFORM test number #.  ' //
     .                    'Secondary vector def = #.',
     .                            '#', CASE,   CASMSG )
            CALL REPMC  ( CASMSG, '#', SECVDF, CASMSG )
 
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )
 
 
C
C           Find the secondary axis name and sign.
C
            AXNAME = AXDEF ( 2, AXPAIR )
 
            IF ( INDEX( AXNAME, '-' ) .GT. 0 ) THEN
               AXSIGN = -1.D0
            ELSE
               AXSIGN =  1.D0
            END IF
 
            CALL CMPRSS ( '-', 0, AXNAME, AXNAME )
 
            IF ( EQSTR(AXNAME, 'X') ) THEN
               AXINDX = 1
            ELSE IF ( EQSTR(AXNAME, 'Y') ) THEN
               AXINDX = 2
            ELSE
               AXINDX = 3
            END IF
 
C
C           Create the secondary vector, specified relative to the base
C           frame.  Map this vector to the current dynamic frame.  The
C           component of this vector orthogonal to the primary vector
C           should match the current axis specification for the
C           secondary defining vector.
C
            IF ( SECVDF .EQ. 'OBSERVER_TARGET_POSITION' ) THEN
 
               CALL SPKPOS ( SECTRG, ET, 'J2000', SECCOR,
     .                       SECOBS, V,  LT              )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL PXFORM ( 'J2000', FRNAME, ET, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXV  ( R2,      V,     VTEMP )
               CALL VSCL ( AXSIGN, VTEMP,  V     )
 
C
C              PRIVEC is left over from the tests on the primary
C              axis.  Replace SECVEC with its component orthogonal
C              to PRIVEC.  Unitize this component.
C
               CALL VPERP ( V,     PRIVEC,  VTEMP  )
               CALL VHAT  ( VTEMP,          SECVEC )
 
C
C              SECVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12 ,  OK )
 
 
            ELSE IF ( SECVDF .EQ. 'TARGET_NEAR_POINT' ) THEN
C
C              Find the specified near point.
C
               CALL SUBPT ( 'NEAR POINT', SECTRG, ET, SECCOR,
     .                      SECOBS,       SPOINT, ALT         )
 
C
C              We actually need to use the position transformation
C              from SECVF to FRMNAM for this one.  NOTE:  the
C              epoch of this transformation is critical.  We must
C              use the same epoch as was used by SUBPT.
C
C              Get the light time LT from the observer to target
C              center.
C
               CALL SPKPOS ( SECTRG, ET,    'J2000', SECCOR,
     .                       SECOBS, POS,   LT               )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Find the epoch associated with the target by
C              adjusting ET by LT.
C
               CALL ZZCOREPC ( SECCOR, ET, LT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Find the position transformation from the target
C              body-fixed frame at ETCORR to J2000.  Convert
C              the body-fixed sub-point position to the J2000
C              frame.
C
               CALL CNMFRM ( SECTRG, J, TRGFRM, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'TARGET FRAME FOUND', FOUND, .TRUE., OK )
 
               CALL PXFORM ( TRGFRM,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXV    ( R2, SPOINT, V )
 
C
C              Compute the observer-to-near point vector in the J2000
C              frame.
C
               CALL VADD ( V, POS, VTEMP )
 
C
C              Find the position transformation J2000 the current frame
C              at ET.  Rotate the observer-near point vector into this
C              frame.
C
               CALL PXFORM ( 'J2000', FRNAME, ET, R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXV    ( R3,      VTEMP,  V )
 
C
C              Negate the vector if indicated, then unitize.
C
               CALL VSCL ( AXSIGN,    V,      VTEMP  )
 
C
C              PRIVEC is left over from the tests on the primary
C              axis.  Replace SECVEC with its component orthogonal
C              to PRIVEC.  Unitize this component.
C
               CALL VPERP ( VTEMP, PRIVEC,  V      )
               CALL VHAT  ( V,              SECVEC )
 
C
C              SECVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12 ,  OK )
 
 
 
            ELSE IF ( SECVDF .EQ. 'OBSERVER_TARGET_VELOCITY' ) THEN
C
C              Look up the velocity vector in the specified frame.
C              Note this choice is critical:  any other frame,
C              unless related by a constant offset to the correct
C              frame, yields an invalid result.
C
               CALL SPKEZR ( SECTRG, ET,    SECVF, SECCOR,
     .                       SECOBS, STATE, LT            )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C               WRITE (*,*) 'F_DYN01:'
C               WRITE (*,*) 'STATE = ', STATE
C               WRITE (*,*) 'SECVF = ', SECVF
C               WRITE (*,*) 'ET    = ', ET
C               CALL VHAT (STATE(4), UVEL )
C               WRITE (*, * ) 'UVEL = '
C               WRITE (*,'(1X,3E25.17)' ) UVEL
 
 
C
C              We actually need to use the position transformation
C              from SECVF to FRMNAM for this one.  NOTE:  the
C              epoch of this transformation is critical.  We must
C              use the same epoch as was used by SPKEZR.  If
C              SECVF is non-inertial, we need to adjust ET by
C              the light time from the observer to the center of SECVF.
C              We can actually finesse the issue of whether the frame
C              is non-inertial, since performing the correction for
C              an inertial frame has no effect.
C
               CALL NAMFRM ( SECVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )
 
               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )
 
C
C              Get the light time CLT from the observer to the center
C              of the frame in which we looked up the velocity.
C
               CALL SPKEZR ( CENTER, ET,     SECVF, SECCOR,
     .                       SECOBS, CSTATE, CLT            )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C              Find the epoch associated with the center of the
C              frame by adjusting ET by CLT.
C
               CALL ZZCOREPC ( SECCOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Find the position transformation from SECVF at
C              ETCORR to the current frame at ET.  We must do
C              this in two steps, passing through J2000.
C
               CALL PXFORM ( SECVF,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL MXM    ( R3,      R2,     R4         )
C
C              Extract the velocity vector.  Rotate to the current
C              frame, negate if indicated, then unitize.
C
               CALL MXV  ( R4,       STATE(4),  VTEMP     )
               CALL VSCL ( AXSIGN,   VTEMP,            V  )
 
C
C              PRIVEC is left over from the tests on the primary
C              axis.  Replace SECVEC with its component orthogonal
C              to PRIVEC.  Unitize this component.
C
               CALL VPERP ( V,     PRIVEC,  VTEMP  )
               CALL VHAT  ( VTEMP,          SECVEC )
 
C
C              PRIVEC is left over from the tests on the primary
C              axis.  Replace SECVEC with its component orthogonal
C              to PRIVEC.
C
               CALL VPERP ( SECVEC, PRIVEC, VTEMP  )
               CALL VEQU  ( VTEMP,          SECVEC )
 
C
C              SECVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12 ,  OK )
 
 
 
            ELSE IF ( SECVDF .EQ. 'CONSTANT' ) THEN
C
C              The secondary vector is a constant; the vector is
C              stored in SECVEC.
C
C              We need to use the position transformation from SECVF
C              to FRMNAM for this one.  NOTE:  the epoch of this
C              transformation is critical. If SECVF is non-inertial, we
C              need to adjust ET by the light time from the observer to
C              the center of SECVF. We can actually finesse the issue
C              of whether the frame is non-inertial, since performing
C              the correction for an inertial frame has no effect.
C
               CALL NAMFRM ( SECVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )
 
               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )
 
C
C              Parse the aberration correction for the secondary
C              vector.  Get the light time to the frame center if
C              light time corrections are used.
C
               CALL ZZPRSCOR ( SECCOR, SECBLK )
 
               IF ( SECBLK(LTIDX) ) THEN
C
C                 Get the light time CLT from the observer to the
C                 center of the frame in which the constant vector is
C                 defined.
C
                  CALL SPKEZR ( CENTER, ET,     SECVF, SECCOR,
     .                          SECOBS, CSTATE, CLT            )
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
               ELSE
C
C                 The frame is not corrected for light time:
C                 the light time to the frame center is treated as
C                 zero.
C
                  CLT = 0.D0
 
               END IF
 
C
C              Find the epoch associated with the center of the
C              frame by adjusting ET by CLT.
C
               CALL ZZCOREPC ( SECCOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              We now want to express the constant vector in
C              the current dynamic frame at ET.  We must
C              transform the constant vector in two steps,
C              since the epoch associated with the frame in
C              which the constant vector is specified is ETCORR.
C              We first map the constant vector to the J2000
C              frame, then to the current frame.
C
C              When the constant vector is expressed relative
C              to the J2000 frame, we apply the stellar aberration
C              correction if necessary.
C
C              Find the position transformation from SECVF at
C              ETCORR to J2000.  Apply to SECVEC.
C
               CALL PXFORM ( SECVF,   'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ',     OK )
 
               CALL MXV    ( R2,      SECVEC,  V  )
 
               IF ( SECBLK(STLIDX) ) THEN
C
C                 Stellar aberration correction is required.
C
C                 Find the state of the observer relative to the
C                 solar system barycenter at ET, in the J2000 frame.
C
                  CALL BODN2C ( SECOBS,  J,    FOUND )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )
 
                  CALL SPKSSB ( J,       ET,  'J2000', STOBS )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )
 
                  IF ( SECBLK(XMTIDX) ) THEN
C
C                    Apply transmission stellar aberration correction.
C
                     CALL STLABX ( V, STOBS(4), VTEMP )
 
                  ELSE
C
C                    Apply transmission stellar aberration correction.
C
                     CALL STELAB ( V, STOBS(4), VTEMP )
 
                  END IF
 
                  CALL VEQU ( VTEMP, V )
 
               END IF
 
C
C              The vector V contains our (corrected if necessary)
C              constant vector relative to J2000.
C
C              Look up the rotation from J2000 to the current frame
C              at ET.  Apply to our constant vector.
C
               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ',    OK         )
               CALL MXV    ( R3,      V,      SECVEC     )
 
C
C              Negate the constant vector if indicated.
C
               CALL VSCL ( AXSIGN,   SECVEC,  VTEMP  )
 
C
C              PRIVEC is left over from the tests on the primary
C              axis.  Replace SECVEC with its component orthogonal
C              to PRIVEC.  Unitize this component.
C
               CALL VPERP ( VTEMP, PRIVEC,  V      )
               CALL VHAT  ( V,              SECVEC )
 
C
C              SECVEC should match the axis indicated by AXINDX.
C
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~',
     .                       E(1,AXINDX),  3,       DM12 ,  OK )
 
 
            ELSE
C
C              We have an unrecognized vector definition.
C
               CALL SETMSG ( 'F_DYN01 encountered vector ' //
     .                       'definition #.'               )
               CALL ERRCH  ( '#',  SECVDF                  )
               CALL SIGERR ( 'SPICE(BUG)'                  )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
            END IF
 
 
 
 
 
C
C--- Case: ------------------------------------------------------
C
            CALL REPMI  ( 'SXFORM test number #.  Test derivative ' //
     .                    'block.',
     .                    '#', CASE, CASMSG )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )
 
C
C           Perform "sanity checks" on the returned matrix.
C           Make sure the diagonal blocks are identical rotations,
C           compute a discrete derivative and compare to the
C           lower left block, and make sure the upper right
C           block is a zero matrix.
C
            DELTA = 1.D0
 
            CALL PXFORM ( FRNAME, BFRAME, ET-DELTA, RMINUS )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL PXFORM ( FRNAME, BFRAME, ET+DELTA, RPLUS  )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
            CALL T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA,
     .                     NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Check the error measurements we've made.  First
C           up is the determinant error.
C
            TOL = DM14
            CALL CHCKSD ( 'DETERR', DETERR, '~', 0.D0, TOL, OK )
 
C
C           Check norms of rows and columns in the rotation
C           blocks.
C
            CALL CHCKSD ( 'NRMERR', NRMERR, '~', 0.D0, TOL, OK )
 
C
C           Check the derivative error when measured against
C           the discrete derivative.
C
            MAXDER = MAX ( MAXDER, DRVERR )
 
            IF (      ( INDEX(PRICOR, 'S') .GT. 0 )
     .           .OR. ( INDEX(SECCOR, 'S') .GT. 0 ) ) THEN
 
               TOL = DM4
C               TOL = DM5
            ELSE
C               TOL = DM5
               TOL = 5.D-7
            END IF
 
 
            CALL CHCKSD ( 'DRVERR', DRVERR, '~', 0.D0, TOL, OK )
 
 
C
C                 These statements may be useful for debugging.
C
C               IF ( .NOT. OK ) THEN
C
C                  WRITE (*,*) '=================================== '
C                  WRITE (*,*) 'CASE = ', CASE
C                  WRITE (*,*) COORDS
C                  WRITE (*,*) 'PRIFRM  = ', PRIFRM
C                  WRITE (*,*) 'PRIJ    = ', PRIJ
C                  WRITE (*,*) 'PRIVF   = ', PRIVF
C                  WRITE (*,*) 'VDF     = ', VDF
C                  WRITE (*,*) 'SECVDF  = ', SECVDF
C                  WRITE (*,*) 'SECOBS  = ', SECOBS
C                  WRITE (*,*) 'SECTRG  = ', SECTRG
C                  WRITE (*,*) 'SECVF   = ', SECVF
C                  WRITE (*,*) 'VFRAME  = ', VFRAME
C                  WRITE (*,*) 'DRVERR  = ', DRVERR
C                  WRITE (*,*) 'PRIVEC  = ', PRIVEC
C                  WRITE (*,*) 'SECVEC  = ', SECVEC
C                  WRITE (*,*) 'DMAG    = ', DMAG
C                  WRITE (*,*) 'DM16  /DMAG    = ', DM16  /DMAG
C
C                  CALL M33 ( 'DRDIFF', DRDIFF )
C
C                 DO I = 1, DX-1
C                    WRITE (*,*) DEFTXT(I)
C                 END DO
C
C                 WRITE (*,*) '=================================== '
C
C              END IF
 
C
C           Check the derivative relative error.  Perform this
C           test only if both aberration corrections are 'NONE'
C           and if the derivative error is not to close to
C           machine epsilon.
C
C
 
C
C           Decide whether we'll do a derivative test.
C
            TSTDRV = .TRUE.
            TOL    =  0.D0
 
            IF (       ( PRICOR .EQ. 'NONE'  )
     .           .AND. ( SECCOR .EQ. 'NONE'  )  ) THEN
C
C              No aberration corrections are used.
C
C              There are a few special cases where we must expect
C              quite inaccurate answers.  Set the tolerances for
C              these first.
C
C              The Phobos-earth and earth-sun velocity vectors in the
C              IAU_PHOBOS and PHOBOS_RADIAL frames are particularly
C              problematic.
C
               IF  ( DRVERR .LT. DM15 ) THEN
C
C                 The relative error is not meaningful; skip
C                 the test.
C
                  NSKIP  =  NSKIP + 1
                  TSTDRV = .FALSE.
 
               ELSE IF  ( PRIVF  .EQ. 'PHOBOS_RADIAL' ) THEN
 
C                  TOL =  DM5
                  TOL =  DM4
 
               ELSE IF  (       ( PRIOBS .EQ. 'PHOBOS'        )
     .                    .AND. ( PRITRG .EQ. 'MARS'          )
     .                    .AND. ( PRIVF  .EQ. 'IAU_PHOBOS'    )
     .                    .AND. ( PRIVDF .EQ.  OTV            ) ) THEN
 
C                  TOL =  DM4
                  TOL =  DM6
 
                  NPR = NPR + 1
 
C                  WRITE (*,*) 'IAU_PHOBOS, NPR = ', NPR
 
               END IF
 
 
               IF  (       ( SECOBS .EQ. 'PHOBOS'        )
     .               .AND. ( SECTRG .EQ. 'EARTH'         )
     .               .AND. ( SECVF  .EQ. 'IAU_PHOBOS'    )
     .               .AND. ( SECVDF .EQ.  OTV            ) ) THEN
 
                  TOL =  MAX ( TOL, DM4 )
 
               ELSE IF (       ( SECOBS .EQ. 'EARTH'         )
     .                   .AND. ( SECTRG .EQ. 'SUN'           )
     .                   .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' )
     .                   .AND. ( SECVDF .EQ.  OTV            ) ) THEN
 
                  TOL =  MAX ( TOL, DM3 )
 
               ELSE IF (       ( SECOBS .EQ. 'EARTH'         )
     .                   .AND. ( SECTRG .EQ. 'SUN'           )
     .                   .AND. ( SECVF  .EQ. 'IAU-EARTH'     )
     .                   .AND. ( SECVDF .EQ.  OTV            ) ) THEN
 
                  TOL = MAX ( TOL, DM3 )
 
               ELSE IF (       ( SECOBS .EQ. 'PHOBOS'        )
     .                   .AND. ( SECTRG .EQ. 'EARTH'         )
     .                   .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' )
     .                   .AND. ( SECVDF .EQ.  OTV            ) ) THEN
 
                  TOL =  MAX ( TOL, DM2 )
 
 
               END IF
 
 
               IF ( TOL .EQ. 0.D0 ) THEN
 
                  IF ( DMAG .GT. DM15 ) THEN
C
C                    This is the normal case.
C
C                    NNORML = NNORML + 1
 
C                     TOL = MAX ( 10.D0**( -16 - LOG10(DMAG) ),  DM6 )
                     TOL = MAX ( 10.D0**( -16 - LOG10(DMAG) ), DM5 )
 
 
                  ELSE IF ( DMAG .EQ. 0.D0 ) THEN
C
C                    This should happen only for constant frames,
C                    in which case estimated and actual derivatives
C                     should---we think---be identically zero.
C
                     TOL = 0.D0
 
                  ELSE
C
C                    No point in testing the relative error---the
C                    derivative is too small for the discrete estimate
C                    to have any validity.
C
                     NSKIP  =  NSKIP + 1
                     TSTDRV = .FALSE.
 
                  END IF
 
               END IF
C
C              The tolerance has been set for the pure geometric
C              cases.
C
 
            ELSE IF (     ( INDEX(PRICOR, 'LT') .GT. 0  )
     .               .OR. ( INDEX(SECCOR, 'LT') .GT. 0  )
     .               .OR. ( INDEX(PRICOR, 'CN') .GT. 0  )
     .               .OR. ( INDEX(SECCOR, 'CN') .GT. 0  )
     .               .OR. ( INDEX(PRICOR, 'S' ) .GT. 0  )
     .               .OR. ( INDEX(SECCOR, 'S' ) .GT. 0  )  ) THEN
C
C              We're using light time corrections possibly
C              stellar aberration corrections).
C
C              There are several special cases where we must expect
C              quite inaccurate answers.  Set the tolerances for
C              these first.
C
C              The phobos-earth velocity vector in the IAU_PHOBOS
C              frame is particularly problematic.
C
               BIGERR = .FALSE.
 
               IF  (       ( SECOBS .EQ. 'PHOBOS'        )
     .               .AND. ( SECTRG .EQ. 'EARTH'         )
     .               .AND. ( SECVF  .EQ. 'IAU_PHOBOS'    )
     .               .AND. ( SECVDF .EQ.  OTV            ) )  THEN
 
                  BIGERR = .TRUE.
                  TOL    =  DM4
 
 
               ELSE IF (       ( PRIVDF .EQ. 'CONSTANT'      )
     .                   .AND. ( PRIVF  .EQ. 'GSE'           )
     .                   .AND. ( SECOBS .EQ. 'EARTH'         )
     .                   .AND. ( SECTRG .EQ. 'SUN'           ) ) THEN
C
C                 The velocity is just noise for many of these
C                 cases, so the relative error is meaningless.
C
                  BIGERR = .TRUE.
                  TOL    =  0.D0
                  TSTDRV = .FALSE.
 
 
               ELSE IF (       ( SECOBS .EQ. 'EARTH'         )
     .                   .AND. ( SECTRG .EQ. 'SUN'           )
     .                   .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' )
     .                   .AND. ( SECVDF .EQ.  OTV            ) ) THEN
 
                  BIGERR = .TRUE.
                  TOL    =  DM2
 
 
               ELSE IF (       ( SECOBS .EQ. 'PHOBOS'        )
     .                   .AND. ( SECTRG .EQ. 'EARTH'         )
     .                   .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' )
     .                   .AND. ( SECVDF .EQ.  OTV            ) ) THEN
 
                  BIGERR = .TRUE.
                  TOL    =  DM2
 
 
               ELSE IF (       ( PRIVDF .EQ. 'CONSTANT' )
     .                   .AND. ( SECVDF .EQ. 'CONSTANT' ) ) THEN
C
C                 The velocity is just noise for many of these
C                 cases, so the relative error is meaningless.
C
                  BIGERR = .TRUE.
                  TOL    =  0.D0
                  TSTDRV = .FALSE.
 
               END IF
 
 
C
C              Further tolerance adjustments may be needed if we're
C              using stellar aberration.
C
               IF ( TSTDRV ) THEN
 
                  IF ( INDEX(PRICOR, 'S') .GT. 0 ) THEN
 
                     IF  (       ( PRIOBS .EQ. 'PHOBOS' )
     .                     .AND. ( PRIVF  .EQ. 'GSE'    ) )  THEN
 
                        BIGERR = .TRUE.
                        TOL    =  DM1
 
                     ELSE IF (      (PRIOBS .EQ. 'PHOBOS'       )
     .                        .AND. (PRIVF  .EQ. 'PHOBOS_RADIAL') ) THEN
 
                        BIGERR = .TRUE.
                        TOL    =  DM3
 
                     ELSE IF (      (PRIOBS .EQ. 'PHOBOS'  )
     .                        .AND. (PRIVDF .EQ. OTP       ) ) THEN
 
                        BIGERR = .TRUE.
                        TOL    =  DM3
 
                     END IF
 
                     IF (       ( PRIOBS .EQ. 'EARTH'  )
     .                    .AND. ( PRITRG .EQ. 'SUN'    )
     .                    .AND. ( PRIVDF .EQ.  OTV     ) ) THEN
 
                        BIGERR = .TRUE.
 
                        IF ( PRIVF .EQ. 'PHOBOS_RADIAL' ) THEN
C
C                          The derivative in this case suffers
C                          from total loss of precision.
C
 
                           TSTDRV =  .FALSE.
                           TOL    =  0.D0
 
                        ELSE
C
C                          We have about one digit of information here.
C
                           TOL    =  DM1
 
                        END IF
 
                     END IF
 
                  END IF
 
                  IF ( INDEX(SECCOR, 'S') .GT. 0 ) THEN
 
                     IF (       ( SECOBS .EQ. 'EARTH'  )
     .                    .AND. ( SECTRG .EQ. 'SUN'    )
     .                    .AND. ( SECVDF .EQ.  OTV     ) ) THEN
 
                        BIGERR = .TRUE.
 
                        IF ( SECVF .EQ. 'PHOBOS_RADIAL' ) THEN
C
C                          The derivative in this case suffers
C                          from total loss of precision.
C
 
                           TSTDRV =  .FALSE.
                           TOL    =  0.D0
 
                        ELSE
C
C                          We have about one digit of information here.
C
                           TOL    =  DM1
 
                        END IF
 
 
                     ELSE IF (       ( SECOBS .EQ. 'PHOBOS')
     .                         .AND. ( SECTRG .EQ. 'EARTH' ) ) THEN
 
                        BIGERR =  .TRUE.
                        TOL    =  DM1
 
                     ELSE IF (      (SECOBS .EQ. 'EARTH'        )
     .                        .AND. (SECVF  .EQ. 'PHOBOS_RADIAL') ) THEN
 
                        BIGERR =  .TRUE.
                        TOL    =  MAX ( TOL, DM3 )
 
                     END IF
 
                  END IF
 
               END IF
 
C
C              Set the tolerance for the "normal" cases.
C
               IF ( TSTDRV .AND. ( .NOT. BIGERR ) ) THEN
 
                  IF ( DMAG .GT. DM10 ) THEN
C
C                    The derivative magnitude is large enough to
C                    squeeze out about four digits of precision.
C
                     TOL = DM4
 
                  ELSE IF ( DMAG .GT. DM12 ) THEN
 
C
C                    We may expect about two digits of agreement
C                    between the computed as estimated derivative.
C
                     TOL = DM2
 
                  ELSE IF ( DMAG .EQ. 0.D0 ) THEN
 
C
C                    This should happen only for constant frames,
C                    in which case estimated and actual derivatives
C                    should---we think---be identically zero.
C
                     TOL = 0.D0
 
                  ELSE
C
C                    No point in testing the relative error---the
C                    derivative is too small for the discrete estimate
C                    to have any validity.
C
                     NSKIP  =  NSKIP + 1
                     TSTDRV = .FALSE.
 
                  END IF
 
               END IF
C
C              TOL has been set for the aberration correction cases.
C
 
            END IF
C
C           TOL has been set.
C
 
            IF ( TSTDRV ) THEN
 
               TOL = MAX ( TOL, 1.D-7/DMAG )
 
C               WRITE (*,*) 'USING TOL = ', TOL
               CALL CHCKSD ( 'DRLERR', DRLERR, '~', 0.D0, TOL, OK )
 
 
 
C
C                   These statements may be useful for debugging.
C
C               IF ( .NOT. OK ) THEN
C
C                  WRITE (*,*) '=================================== '
C                  WRITE (*,*) 'CASE = ', CASE
C                  WRITE (*,*) COORDS
C                  WRITE (*,*) 'PRIFRM  = ', PRIFRM
C                  WRITE (*,*) 'PRIJ    = ', PRIJ
C                  WRITE (*,*) 'PRIVDF  = ', PRIVDF
C                  WRITE (*,*) 'PRIOBS  = ', PRIOBS
C                  WRITE (*,*) 'PRITRG  = ', PRITRG
C                  WRITE (*,*) 'PRIVF   = ', PRIVF
C                  WRITE (*,*) 'VDF     = ', VDF
C                  WRITE (*,*) 'SECVDF  = ', SECVDF
C                  WRITE (*,*) 'SECOBS  = ', SECOBS
C                  WRITE (*,*) 'SECTRG  = ', SECTRG
C                  WRITE (*,*) 'SECVF   = ', SECVF
C                  WRITE (*,*) 'VFRAME  = ', VFRAME
C               IF ( ISVEL(1) .AND. ISVEL(2) ) THEN
C                  WRITE (*,*) 'DRVERR, DRLERR  = ', DRVERR, DRLERR
C               END IF
C                  WRITE (*,*) 'PRIVEC  = ', PRIVEC
C                  WRITE (*,*) 'SECVEC  = ', SECVEC
C                   WRITE (*,*) 'DMAG    = ', DMAG
C                   WRITE (*,*) '1.D-7/DMAG    = ', 1.D-7/DMAG
C                   WRITE (*,*) '1.D-8/DMAG    = ', 1.D-8/DMAG
C                  WRITE (*,*) 'DM16  /DMAG    = ', DM16  /DMAG
C
C                  CALL M33 ( 'DRDIFF', DRDIFF )
C                  CALL M66 ( 'XFORM',  XFORM  )
 
C                  CALL XF2RAV ( XFORM, R, V )
 
C                  WRITE (*,*) 'Frame angular velocity: ', V
C                  WRITE (*,*) '||V|| = ', VNORM(V)
C
 
C                WRITE (*,*) 'F_DYN01:'
C                WRITE (*,*) 'STATE = ', STATE
C               WRITE (*,*) 'SECVF = ', SECVF
C               WRITE (*,*) 'ET    = ', ET
C               CALL VHAT (STATE(4), UVEL )
C               WRITE (*, * ) 'UVEL = '
C               WRITE (*,'(1X,3E25.17)' ) UVEL
 
C                 DO I = 1, DX-1
C                    WRITE (*,*) DEFTXT(I)
C                 END DO
C
C                 WRITE (*,*) '=================================== '
C
C              END IF
C
 
 
            END IF
 
 
         END IF
 
      END DO
 
C      WRITE (*,*) 'Number of cases rejected: ', NOGO
C      WRITE (*,*) 'MAXDER = ', MAXDER
C      WRITE (*,*) 'NSKIP  = ', NSKIP
 
C      WRITE (*,*) 'NNORML = ', NNORML
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
