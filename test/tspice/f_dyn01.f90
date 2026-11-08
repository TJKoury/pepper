!$Procedure F_DYN01 ( Dynamic Frame Test Family 01 )

      SUBROUTINE F_DYN01 ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the first
!     subset of dynamic frame routines.  Only two-vector frames
!     are used in these tests.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzdyn.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!
!     Tests done here include, but are not necessarily limited to:
!
!        - Test state transformation from SXFORM:
!
!            o Make sure upper left and lower right blocks match.
!            o Test determinant of rotation blocks
!            o Test norms of rows and columns
!
!        - Compare SXFORM and PXFORM results.
!
!        - Compare SXFORM derivative to discrete derivative
!          computed using PXFORM.  Check absolute and relative
!          derivative errors.
!
!        - Compute state transformation via a local computation.
!          Compare rotation portion of transformation matrix
!          from SXFORM to locally computed result.
!
!        - Make sure that when rotation state is inertial, when a
!          matrix mapping from base frame to an inertial frame is
!          right-multiplied by matrix produced by SXFORM, resulting
!          matrix has zero derivative.
!
!        - Make sure that when rotation state is frozen, when a
!          matrix produced by SXFORM has zero derivative.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 05-JUN-2021 (BVS)
!
!        Added missing 'D' to literal DP constants.
!
!        Added SAVE statements for ABPAIR whose values are
!        initialized using DATA.
!
!        Re-ordered header sections.
!
!-    TSPICE Version 2.0.0, 09-NOV-2005 (NJB)
!
!        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
!        Tolerance was increase for GSE-to-J2000 test from 5.D-8 to
!        6.D-8.
!
!-    TSPICE Version 1.0.0, 10-JAN-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORMG

      INTEGER               ISRCHC
      INTEGER               PRODAI

      LOGICAL               EQSTR

!
!     Local Parameters
!
      CHARACTER*(*)         OTP
      PARAMETER           ( OTP    = 'OBSERVER_TARGET_POSITION' )

      CHARACTER*(*)         OTV
      PARAMETER           ( OTV    = 'OBSERVER_TARGET_VELOCITY' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'test_dyn.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'test_dyn.bsp' )

!
!     Tolerance levels for various tests.
!
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

!
!     Parameters controlling frame definitions:
!

!
!     Number of dimensions of the test parameter space:
!
      INTEGER               NDIMS
      PARAMETER           ( NDIMS  = 5 )

!
!     Number of aberration correction settings:
!
      INTEGER               NABCRR
      PARAMETER           ( NABCRR = 11 )

!
!     Number of aberration correction settings used for the
!     full cartesian product test:
!
      INTEGER               NABPRS
      PARAMETER           ( NABPRS = 5 )
!      PARAMETER           ( NABPRS = 1 )

!
!     The constant ABCIDX refers to the ordinal position of the
!     dimension corresponding to aberration correction in the parameter
!     space.
!
      INTEGER               ABCIDX
      PARAMETER           ( ABCIDX = 1 )

!
!     Number of axis definition cases:
!
      INTEGER               NAXDEF
      PARAMETER           ( NAXDEF = 6 )

!
!     Number of base frame cases:
!
      INTEGER               NBASEF
      PARAMETER           ( NBASEF = 2 )

!
!     The constant BFRIDX refers to the ordinal position of the
!     dimension corresponding to base frame in the parameter
!     space.
!
      INTEGER               BFRIDX
      PARAMETER           ( BFRIDX = ABCIDX + 1 )

!
!     Number of bodies:
!
      INTEGER               NBODS
      PARAMETER           ( NBODS   = 5 )

!
!     Number of observer-target body pairs:
!
      INTEGER               NOTPRS
      PARAMETER           ( NOTPRS  = 5 )

      INTEGER               OTPIDX
      PARAMETER           ( OTPIDX  = BFRIDX + 1 )

!
!     Number of vector definitions:
!
      INTEGER               NVECDF
      PARAMETER           ( NVECDF = 4 )

!
!     Number of vector frames:
!
      INTEGER               NVECFR
      PARAMETER           ( NVECFR = 5 )

!
!     Number of extended vector definitions.  These definitions
!     include every combination of frame-dependent definition type
!     and frame (for example, velocity and observer frame, constant
!     and J2000 frame, etc.)
!
      INTEGER               NXVCDF
      PARAMETER           ( NXVCDF = NVECDF - 2 + 2*NVECFR )

      INTEGER               PRIIDX
      PARAMETER           ( PRIIDX = OTPIDX + 1 )

      INTEGER               SECIDX
      PARAMETER           ( SECIDX = PRIIDX + 1 )

      INTEGER               ORDLEN
      PARAMETER           ( ORDLEN = 4 )

!
!     Other parameters
!
!     Number of coordinate systems used to represent constant vectors:
!
      INTEGER               NCOSYS
      PARAMETER           ( NCOSYS = 3 )

      INTEGER               NUNITS
      PARAMETER           ( NUNITS = 2 )

!
!     Local Variables
!
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

!
!     We'll use the kernel variable name "stem"
!
!        FRAME_<ID code>_
!
!     The length declared below (18) is the length of such a string
!     where the ID code contains 11 digits.
!
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

!
!     This variable is used for debugging.
!
      INTEGER               NOGO


!
!     OTPAIR is used to store observer-target pairs used to define
!     all vector types other than constant vectors.  The first
!     element of the ith column indicates the observer (the value
!     is an index into the BODIES array) for the primary vector;
!     the second element indicates the target for the primary vector.
!     The third and fourth elements play the same role for the
!     secondary vector.
!
      INTEGER               OTPAIR ( 4, NOTPRS )

      LOGICAL               BIGERR
      LOGICAL               FOUND
      LOGICAL               GO
      LOGICAL               ISCON  ( 2 )
      LOGICAL               ISVEL  ( 2 )
      LOGICAL               PRIBLK ( NABCOR )
      LOGICAL               TSTDRV
      LOGICAL               SECBLK ( NABCOR )

!
!     Saved variables
!
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

!
!     Initial values
!
      DATA                  ABCORR                                    / &
                            'NONE', &
                            'LT', &
                            'LT+S', &
                            'XLT', &
                            'XLT+S', &
                            'CN', &
                            'CN+S', &
                            'XCN', &
                            'XCN+S', &
                            'S', &
                            'XS'                                      /

!
!     The elements of ABPAIR are indices into the ABCORR array. ABPAIR
!     has dimensions 2 x NABPRS. Each column of ABPAIR indicates the
!     corrections to be used for the primary and secondary vectors.
!
!     For the special case of constant vectors, the correction
!     'LT+S' is replaced with the correction 'LT', the correction
!     'XLT' is replaced with the correction 'S', and the correction
!     'XLT+S' is replaced with the correction 'XS'.  These replacements
!     are done in-line.
!
      DATA                  ABPAIR                                    / &
                            1, 1, &
                            1, 5, &
                            3, 1, &
                            5, 3, &
                            4, 4                                      /

!
!     When defining axes, we vary the spacing and case to test the
!     algorithm used to parse axis specifications.
!
      DATA                  AXDEF                                     / &
                            'Z',      'X', &
                            '-x',     ' Y ', &
                            ' Y ',    '-z', &
                            ' -  y ', '  X', &
                            ' z',     ' - x ', &
                            '- Z',    ' - y '                         /

!
!     We use both inertial and dynamic base frames.
!
      DATA                  BASEFR                                    / &
                            'J2000', &
                            'GSE'                                     /

!
!     PHOBOS_RADIAL is an alternative, or additional, dynamic base
!     frame.  At least one dynamic base frame must be used.
!
!
!     .                      'PHOBOS_RADIAL'                          /

!
!     Bodies acting as observers and targets.
!
      DATA                  BODIES                                    / &
                            'PHOBOS', &
                            'MARS', &
                            'EARTH', &
                            'MOON', &
                            'SUN'                                     /


!
!     Coordinate systems used to represent constant vectors:
!
      DATA                  CORSYS                                    / &
                            '''RECTANGULAR''', &
                            '''LATITUDINAL''', &
                            '''RA/DEC'''                              /

!
!     Euclidean basis vectors.
!
      DATA                  E                                         / &
                            1.D0,  0.D0,  0.D0, &
                            0.D0,  1.D0,  0.D0, &
                            0.D0,  0.D0,  1.D0                        /

!
!     The primary vector is always defined using Phobos as the
!     observer and Mars as the target.  The secondary vector
!     may have the same observer and target (used when the second
!     vector is a velocity vector), the same observer and a different
!     target, or different observer and target.
!
      DATA                  OTPAIR                                    / &
                            1,  2,  1,  2, &
                            1,  2,  1,  3, &
                            1,  2,  3,  4, &
                            3,  4,  3,  4, &
                            3,  5,  3,  4                             /

!
!     VECDEF contains the possible vector definitions.
!
      DATA                  VECDEF                                    / &
                            'OBSERVER_TARGET_POSITION', &
                            'TARGET_NEAR_POINT', &
                            'OBSERVER_TARGET_VELOCITY', &
                            'CONSTANT'                                /

!
!     VECFRM contains frames associated with vectors.  Only velocity
!     and constant vectors actually use frame definitions.
!
      DATA                  VECFRM                                    / &
                            'J2000', &
                            'GSE', &
                            'PHOBOS_RADIAL', &
                            'TARGET_FRAME', &
                            'OBSERVER_FRAME'                          /


!
!     When constant vectors are used and the coordinate system
!     is rectangular, the vectors are picked from this set.
!
      DATA                  VECSTR                                    / &
                            '( 1,  0,  1 )', &
                            '( 1,  1,  0 )', &
                            '( 0,  1,  1 )'                           /

!
!     These strings are used for latitudinal coordinates.
!
      DATA                  ANGSTR                                    / &
                            '-69.761', &
                            ' 78.565', &
                            '-1', &
                            ' 1'                                      /

      DATA                  UNITS                                     / &
                            '''DEGREES''', &
                            '''RADIANS'''                             /

!
!     DIMS defines the dimensions of the cartesian product of the
!     input parameters.  The cardinality of the set comprising the
!     Nth "factor" of the cartesian product is DIMS(N).  The cardinality
!     of the product itself is the product of the elements of DIMS.
!
      DATA                  DIMS                                      / &
                            NABPRS, &
                            NBASEF, &
                            NOTPRS, &
                            NXVCDF, &
                            NXVCDF                                    /


      DATA                  ZR   / 9 * 0.D0 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_DYN01' )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create test inputs for comprehensive two-vector ' // &
                   'test.'                                             )

!
!     Create and load kernels.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( SPK, .TRUE., HANDLE  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Modify the prime meridian constants for Phobos to
!     make the rotation period deviate more from the orbital
!     period.
!
      CALL VPACK  ( 35.06D0,      100.D0,   0.D0,   V )
      CALL PDPOOL ( 'BODY401_PM', 3,                V )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We'll need to create two non-inertial frames:  one to
!     use as a base frame and one to use as a frame associated
!     with velocity and constant vectors.  We'll the GSE
!     frame as one and a Mars radial frame as the other.
!
      DEFTXT(1) = &
      'FRAME_GSE                        =  2399000'
      DEFTXT(2) = &
      'FRAME_2399000_NAME               = ''GSE'''
      DEFTXT(3) = &
      'FRAME_2399000_CLASS              =  5'
      DEFTXT(4) = &
      'FRAME_2399000_CLASS_ID           =  2399000'
      DEFTXT(5) = &
      'FRAME_2399000_CENTER             =  399'
      DEFTXT(6) = &
      'FRAME_2399000_RELATIVE           = ''J2000'''
      DEFTXT(7) = &
      'FRAME_2399000_'//KWSTYL//'       = '''// KVPARM //''''
      DEFTXT(8) = &
      'FRAME_2399000_'//KWFFAM//'             = '''// KV2VEC //''''
      DEFTXT(9) = &
      'FRAME_2399000_'//KWPRI//KWVAXI//'       = '''//KVX//''''
      DEFTXT(10) = &
      'FRAME_2399000_'//KWPRI//KWVCDF//'       = '''//KVPOSV//''''
      DEFTXT(11) = &
      'FRAME_2399000_'//KWPRI//KWVOBS//'       = ''EARTH'''
      DEFTXT(12) = &
      'FRAME_2399000_'//KWPRI//KWVTRG//'         = ''SUN'''
      DEFTXT(13) = &
      'FRAME_2399000_'//KWPRI//KWVABC//'         = ''NONE'''
      DEFTXT(14) = &
      'FRAME_2399000_'//KWSEC//KWVAXI//'       = ''-'//KVY//''''
      DEFTXT(15) = &
      'FRAME_2399000_'//KWSEC//KWVCDF//'       =  '''// KVVELV //''''
      DEFTXT(16) = &
      'FRAME_2399000_'//KWSEC//KWVOBS//'       = ''SUN'''
      DEFTXT(17) = &
      'FRAME_2399000_'//KWSEC//KWVTRG//'       = ''EARTH'''
      DEFTXT(18) = &
      'FRAME_2399000_'//KWSEC//KWVABC//'         = ''NONE'''
      DEFTXT(19) = &
      'FRAME_2399000_'//KWSEC//KWVFRM//'          = ''J2000'''
      DEFTXT(20) = &
      'FRAME_2399000_'//KWRSTA//'       =  '''// KVROTG //''''

!
!     Load the GSE frame definition.
!
      CALL LMPOOL ( DEFTXT, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Define the PHOBOS_RADIAL frame.
!
      DEFTXT(1) = &
      'FRAME_PHOBOS_RADIAL              =  2401000'
      DEFTXT(2) = &
      'FRAME_2401000_NAME               = ''PHOBOS_RADIAL'''
      DEFTXT(3) = &
      'FRAME_2401000_CLASS              =  5'
      DEFTXT(4) = &
      'FRAME_2401000_CLASS_ID           =  2401000'
      DEFTXT(5) = &
      'FRAME_2401000_CENTER             =  401'
      DEFTXT(6) = &
      'FRAME_2401000_RELATIVE           = ''IAU_MARS'''
      DEFTXT(7) = &
      'FRAME_2401000_'//KWSTYL//'       = '''// KVPARM //''''
      DEFTXT(8) = &
      'FRAME_2401000_'//KWFFAM//'             = '''// KV2VEC //''''
      DEFTXT(9) = &
      'FRAME_2401000_'//KWPRI//KWVAXI//'       = '''//KVZ//''''
      DEFTXT(10) = &
      'FRAME_2401000_'//KWPRI//KWVCDF//'       = '''//KVPOSV//''''
      DEFTXT(11) = &
      'FRAME_2401000_'//KWPRI//KWVOBS//'       = ''PHOBOS'''
      DEFTXT(12) = &
      'FRAME_2401000_'//KWPRI//KWVTRG//'         = ''MARS'''
      DEFTXT(13) = &
      'FRAME_2401000_'//KWPRI//KWVABC//'         = ''NONE'''
      DEFTXT(14) = &
      'FRAME_2401000_'//KWSEC//KWVAXI//'       = ''-'//KVX//''''
      DEFTXT(15) = &
      'FRAME_2401000_'//KWSEC//KWVCDF//'       =  '''// KVVELV //''''
      DEFTXT(16) = &
      'FRAME_2401000_'//KWSEC//KWVOBS//'       = ''PHOBOS'''
      DEFTXT(17) = &
      'FRAME_2401000_'//KWSEC//KWVTRG//'       = ''MARS'''
      DEFTXT(18) = &
      'FRAME_2401000_'//KWSEC//KWVABC//'         = ''NONE'''
      DEFTXT(19) = &
      'FRAME_2401000_'//KWSEC//KWVFRM//'          = ''J2000'''
      DEFTXT(20) = &
      'FRAME_2401000_'//KWRSTA//'       =  '''// KVROTG //''''

!
!     Load the PHOBOS_RADIAL frame definition.
!
      CALL LMPOOL ( DEFTXT, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!--- Case: ------------------------------------------------------
!
!
!     First test:  examine the GSE frame.
!
      CALL TCASE ( 'Check GSE-to-J2000 frame transformation.' )

      ET = 1.D7

      CALL SXFORM ( 'GSE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Perform "sanity checks" on the returned matrix. Make sure the
!     diagonal blocks are identical rotations, compute a discrete
!     derivative and compare to the lower left block, and make sure the
!     upper right block is a zero matrix.
!
      DELTA = 1.D0

      CALL PXFORM ( 'GSE', 'J2000', ET-DELTA, RMINUS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PXFORM ( 'GSE', 'J2000', ET+DELTA, RPLUS  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA, &
                     NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )

!
!     Check the error measurements we've made. First up is the
!     determinant error.
!
      TOL = DM14
      CALL CHCKSD ( 'DETERR', DETERR, '~', 0.D0, TOL, OK )

!
!     Check norms of rows and columns in the rotation blocks.
!
      CALL CHCKSD ( 'NRMERR', NRMERR, '~', 0.D0, TOL, OK )

!
!     Check the absolute derivative error.
!
      TOL = 8*DM14
      CALL CHCKSD ( 'DRVERR', DRVERR, '~', 0.D0, TOL, OK )

!
!     Check the relative derivative error.
!
      TOL = 0.6D0 * DM7
      CALL CHCKSD ( 'DRLERR', DRLERR, '~', 0.D0, TOL, OK )



!
!--- Case: ------------------------------------------------------
!
!
!     Second test:  examine the PHOBOS_RADIAL frame.
!
      CALL TCASE ( 'Check PHOBOS_RADIAL-to-IAU_MARS ' // &
                   'frame transformation.'            )

      ET = 1.D7

      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Perform "sanity checks" on the returned matrix. Make sure the
!     diagonal blocks are identical rotations, compute a discrete
!     derivative and compare to the lower left block, and make sure the
!     upper right block is a zero matrix.
!
      DELTA = 1.D0

      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET-DELTA, RMINUS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET+DELTA, RPLUS  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA, &
                     NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )

!
!     Check the error measurements we've made. First up is the
!     determinant error.
!
      TOL = DM14
      CALL CHCKSD ( 'DETERR', DETERR, '~', 0.D0, TOL, OK )

!
!     Check norms of rows and columns in the rotation blocks.
!
      CALL CHCKSD ( 'NRMERR', NRMERR, '~', 0.D0, TOL, OK )

!
!     Check the absolute derivative error.
!
      TOL = DM9
      CALL CHCKSD ( 'DRVERR', DRVERR, '~', 0.D0, TOL, OK )

!
!     Check the relative derivative error.
!
      TOL = 5 * DM7
      CALL CHCKSD ( 'DRLERR', DRLERR, '~', 0.D0, TOL, OK )


!
!--- Case: ------------------------------------------------------
!
!
!     Create an inertial version of the PHOBOS_RADIAL frame.
!
      CALL TCASE  ( 'Test INERTIAL version of PHOBOS_RADIAL frame.' )

      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )

      CALL REPMC  ( DEFTX2(20), KVROTG, KVINRT, DEFTX2(20) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTX2, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, XF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the PHOBOS_RADIAL to IAU_MARS transformation
!     defined using this frame to that using the standard
!     PHOBOS_RADIAL frame.  Just compare the rotation
!     blocks.
!
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

!
!     Check results from PXFORM.
!
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, R3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'PXFORM rotation', R3, '~', R2, 9, DM14, OK )

!
!     Check the derivative matrix:  when we compose the transformation
!     XF2 with the IAU_MARS-to-J2000 transformation, we should
!     get a result with derivative block zero.
!
      CALL SXFORM ( 'IAU_MARS', 'J2000', ET, XF2000 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXMG   ( XF2000, XF2, 6, 6, 6,    XFB2J  )

      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XFB2J(3+I,J)
         END DO
      END DO

      CALL CHCKAD ( 'Derivative block', R, '~', ZR, 9, DM16, OK )



!
!--- Case: ------------------------------------------------------
!
!
!     Create a frozen version of the PHOBOS_RADIAL frame.
!
      CALL TCASE  ( 'Test FROZEN version of PHOBOS_RADIAL frame.' )

!
!     Expunge the optional ROTATN_STATE keyword from the kernel pool.
!
      CALL DVPOOL ( 'FRAME_2401000_'//KWRSTA  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the frame definition.
!
      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )
!
!     Blank out the ROTATN_STATE assignment for this frame.
!
      DEFTX2(20) = ' '

!
!     Set the freeze epoch equal to 1.E7 seconds past J2000 TDB.
!
      DEFTX2(21) = &
      'FRAME_2401000_'//KWFREZ//'       = @2000-117/05:46:40'

      CALL LMPOOL ( DEFTX2, 21 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Although we look up XF2 at ET 0.D0, we should get the
!     same rotation we obtained from the standard PHOBOS_RADIAL
!     frame at ET 1.E7, except that the derivative block has
!     been zeroed out.
!
      CALL CLEARD ( 36, XF2 )
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, XF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Compare the PHOBOS_RADIAL to IAU_MARS transformation
!     defined using this frame to that using the standard
!     PHOBOS_RADIAL frame.  Just compare the rotation
!     blocks.
!
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

!
!     Check results from PXFORM.
!
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, R3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'PXFORM rotation', R3, '~', R2, 9, DM14, OK )

!
!     Check the derivative matrix:  we should have derivative block
!     zero.
!
      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XF2(3+I,J)
         END DO
      END DO

      CALL CHCKAD ( 'Derivative block', R, '~', ZR, 9, DM16, OK )


!
!--- Case: ------------------------------------------------------
!
!
!     Create a new frozen frame that doesn't use a velocity
!     vector in its definition.  The secondary vector will be the
!     Mars-Sun vector. Start with the PHOBOS_RADIAL frame as a
!     template.
!
      CALL TCASE  ( 'Test FROZEN two-position vector frame.' )

!
!     Initialize the frame definition.
!
      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )
!
!     Blank out the ROTATN_STATE assignment for this frame.
!
      DEFTX2(20) = ' '

      CALL REPMC  ( DEFTX2(15), KVVELV,     KVPOSV,     DEFTX2(15) )
      CALL REPMC  ( DEFTX2(17), 'MARS',     'SUN',      DEFTX2(17) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the freeze epoch equal to 1.E7 seconds past J2000 TDB.
!
      DEFTX2(21) = &
      'FRAME_2401000_'//KWFREZ//'       = @2000-117/05:46:40'

      CALL LMPOOL ( DEFTX2, 21 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Although we look up XF2 at ET 0.D0, we should get the
!     same rotation we obtained from the standard PHOBOS_RADIAL
!     frame at ET 1.E7, except that the derivative block has
!     been zeroed out.
!
      CALL CLEARD ( 36, XF2 )
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, XF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the upper left rotation block from XF2.
!
      DO I = 1, 3
         DO J = 1, 3
            R2(I,J) = XF2 (I,J)
         END DO
      END DO

!
!     Check results from PXFORM.
!
      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', ET, R3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'PXFORM rotation', R3, '~', R2, 9, DM14, OK )

!
!     Check the derivative matrix:  we should have derivative block
!     zero.
!
      DO I = 1, 3
         DO J = 1, 3
            R (I,J) = XF2(3+I,J)
         END DO
      END DO

      CALL CHCKAD ( 'Derivative block', R, '~', ZR, 9, DM16, OK )


!
!--- Case: ------------------------------------------------------
!
!
!     Create a new frozen frame with linearly dependent defining
!     vectors.  Both primary and secondary vectors will be the
!     Phobos-Mars position vector. Start with the PHOBOS_RADIAL frame
!     as a template.
!
      CALL TCASE  ( 'Test linearly dependent two-position ' // &
                    'vector frame.' )

!
!     Initialize the frame definition.
!
      CALL MOVEC  ( DEFTXT, 20, DEFTX2 )
!
!     Blank out the ROTATN_STATE assignment for this frame.
!
      DEFTX2(20) = ' '

      CALL REPMC  ( DEFTX2(15), KVVELV,     KVPOSV,     DEFTX2(15) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTX2, 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure error is caught by both SXFORM and PXFORM.
!
      CALL SXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, XF2 )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      CALL PXFORM ( 'PHOBOS_RADIAL', 'IAU_MARS', 0.D0, R )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!     Expunge the optional FREEZE_EPOCH keyword from the kernel pool.
!
      CALL DVPOOL ( 'FRAME_2401000_'//KWFREZ  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore the original PHOBOS_RADIAL frame definition.
!
      DEFTXT(21) = ' '
      CALL LMPOOL ( DEFTXT, 21 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Proceed to the general test cases.
!
!
!     Because there are so many options that may be used to define
!     a two-vector frame, we can't conveniently loop over every
!     set of possibilities using nested loops:  the loops would be
!     nested too deeply.  Instead, we define a mapping that allows
!     us to map a test case number to a set of input parameters.
!
!     First compute the number of test cases in the cartesian product
!     test.
!

      NCART = PRODAI ( DIMS, NDIMS )



!
!     All of the variables initialized here are used for debugging.
!
      NOGO   = 0
      NPR    = 0
!     NNORML = 0
      NCOR   = 0
      NSKIP  = 0
      MAXDER = 0.D0


      DO CASE = 1, NCART
!
!        Find the multi-dimensional coordinates of the current
!        case in the cartesian product of the test input sets.
!
         CALL MULTIX ( 1, NDIMS, DIMS, CASE, COORDS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Set frame definition parameters defined by COORDS.
!        Start out with the base frame.
!
         BFRAME = BASEFR ( COORDS(BFRIDX) )

!
!        The second dimension refers to the observer-target pairs
!        for the primary and secondary vectors.  (For constant
!        vectors, only the observer is used, and this is only if
!        aberration corrections are used.)
!
         I      = OTPAIR ( 1, COORDS(OTPIDX) )
         J      = OTPAIR ( 2, COORDS(OTPIDX) )
         PRIOBS = BODIES ( I )
         PRITRG = BODIES ( J )

         I      = OTPAIR ( 3, COORDS(OTPIDX) )
         J      = OTPAIR ( 4, COORDS(OTPIDX) )
         SECOBS = BODIES ( I )
         SECTRG = BODIES ( J )

!
!        The third dimension refers to the extended vector
!        definition for the primary vector.  Extended definitions
!        include frame selections for velocity vectors and
!        constant vectors (those vectors requiring frames as
!        part of their definitions).  The ordering of extended
!        frame definitions is:
!
!            1           )  Observer-target position
!            2           )  Observer-near point position
!            3           )  Velocity vector, frame definition 1
!                             ...
!            2 + NVECFR  )  Velocity vector, frame definition NVECFR
!            3 + NVECFR  )  Constant vector, frame definition 1
!                             ...
!            2 + 2*NVECFR)  Constant vector, frame definition NVECFR
!
!        The fourth dimension provides the analogous information
!        for the secondary vector.
!
!        We'll process the definitions for both vectors in the loop
!        below.
!

         DO I = 1, 2
!
!           Indicate that we don't have a velocity vector or
!           constant vector until we know otherwise.
!
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
!
!              The vector is an observer-target position.
!
               VDF = VECDEF(1)
               VFR = ' '

            ELSE IF ( J .EQ. 2 ) THEN

!
!              The vector is an observer-near point position.
!
               VDF = VECDEF ( 2 )
               VFR = ' '

            ELSE IF (       ( J .GE. 3             ) &
                      .AND. ( J .LE. 2 + NVECFR )  ) THEN
!
!              The vector is an observer-target velocity.
!              The quantity J-2 is an index into the vector
!              frame array.
!
               VDF      = VECDEF ( 3   )
               VFR      = VECFRM ( J-2 )
               ISVEL(I) = .TRUE.

            ELSE
!
!              The primary is constant in the specified frame.
!              The quantity J-2-NVECFR is an index into the vector
!              frame array.
!
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

!
!           Set the aberration correction for the current vector.
!
            J = ABPAIR ( I, COORDS(ABCIDX) )

!
!           If the current vector is constant, adjust the
!           aberration correction to make it valid for constant
!           vectors.
!
!           For the special case of constant vectors, the correction
!           'LT+S' is replaced with the correction 'LT', the correction
!           'XLT' is replaced with the correction 'S', and the
!           correction 'XLT+S' is replaced with the correction 'XS'.
!
            IF ( ISCON(I) ) THEN

               IF ( ABCORR(J) .EQ. 'LT+S' ) THEN
!
!                 Set the correction for this vector to 'LT'
!
                  J = ISRCHC ( 'LT', NABCRR, ABCORR )

               ELSE IF ( ABCORR(J) .EQ. 'XLT' ) THEN
!
!                 Set the correction for this vector to 'S'
!
                  J = ISRCHC ( 'S', NABCRR, ABCORR )


               ELSE IF ( ABCORR(J) .EQ. 'XLT+S' ) THEN
!
!                 Set the correction for this vector to 'XS'
!
                  J = ISRCHC ( 'XS', NABCRR, ABCORR )

               ELSE IF ( ABCORR(J) .EQ. 'NONE' ) THEN
!
!                 Set the correction for this vector to 'NONE'
!
                  J = ISRCHC ( 'NONE', NABCRR, ABCORR )

               ELSE

                  CALL SETMSG ( 'Unexpected aberration correction # '// &
                                'was encountered when mapping '      // &
                                'correction for constant vector #.'  // &
                                'Case is #.'                         )
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

!
!        Decide whether we need to execute the current case.
!
!        There are certain cases we can skip:
!
!           1) Neither vector is a velocity vector and both
!              vectors have the same observer/target sets.
!              Such vector pairs will be too close to linearly
!              dependent to work with.
!
!           2) Both vectors are velocity vectors.
!
         GO = .TRUE.


!
!        Use this code to disable testing of frames defined by
!        a pair of velocity vectors.
!
!         IF ( ISVEL(1) .AND. ISVEL(2) ) THEN
!            GO = .FALSE.
!         END IF

         J  =  COORDS( PRIIDX )
         K  =  COORDS( SECIDX )



         IF (     (   ISVEL(1)   .AND.   ISVEL(2)    ) &
             .OR. ( ( J .LE. 2 ) .AND. ( K .LE. 2 )  )   )  THEN
!
!           The primary and secondary vectors are either both
!
!              - velocity vectors
!
!              - in the set
!
!                  { OBSERVER_TARGET_POSITION, TARGET_NEAR_POINT }
!
!           In this case we don't want both vectors to be defined
!           by the same pair of objects, because the vectors will
!           be linearly dependent or close to it.
!
            IF (      (      ( PRIOBS .EQ. SECOBS ) &
                        .OR. ( PRIOBS .EQ. SECTRG ) ) &
                .AND. (      ( PRITRG .EQ. SECOBS ) &
                        .OR. ( PRITRG .EQ. SECTRG ) )  ) THEN

               GO = .FALSE.

            END IF

         END IF

!
!        Another linear dependency problem:  if the vector frame
!        is PHOBOS_RADIAL, then we don't want cases where the observer
!        and target are from the set { Phobos, Mars } for both
!        vectors.
!
         IF (      ( PRIFRM .EQ. 'PHOBOS_RADIAL' ) &
              .OR. ( SECFRM .EQ. 'PHOBOS_RADIAL' ) ) THEN

!
!           Note: this code must change if the observer-target pairs
!           are changed!
!
            IF ( COORDS(OTPIDX) .EQ. 1 ) THEN

               GO  = .FALSE.

            END IF

         END IF

!
!        Similar problem for the earth and sun when the velocity
!        frame is GSE:
!
         IF (      ( PRIFRM .EQ. 'GSE' ) &
              .OR. ( SECFRM .EQ. 'GSE' ) ) THEN

!
!           Note: this code must change if the observer-target pairs
!           are changed!
!
            IF ( COORDS(OTPIDX) .EQ. 4 ) THEN

               GO  = .FALSE.

            END IF

         END IF


!
!         Bits used for debugging
!
         IF ( .NOT. GO ) THEN
            NOGO = NOGO + 1
         END IF


         IF ( GO ) THEN
!
!           Cycle over the vector-axis associations.  There's
!           no interaction between these parameters and the
!           other parameters, so we don't need to try every
!           possible combination of these values with the other
!           inputs.  We just need to use each association.
!
            AXPAIR =  MOD ( CASE, NAXDEF ) + 1

!
!           We're ready to create a frame definition.
!
!
!           Create a text buffer containing a frame definition that
!           uses the current frame definition
!
!
!           First comes the frame name-to-frame code assignment.
!
            FRNAME     = 'TWOVEC_DYN_FRAME'
            FRCODE     = -2000000000

            DEFTXT(1)  = 'FRAME_#          = #'
            CALL REPMC  ( DEFTXT(1), '#', FRNAME, DEFTXT(1) )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI  ( DEFTXT(1), '#', FRCODE, DEFTXT(1) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           We'll use the kernel variable name "stem"
!
!              FRAME_<ID code>_
!
!           repeatedly, so instead of making thousands of REPMI
!           calls, we just create this stem here.
!
            CALL REPMI ( 'FRAME_#_', '#', FRCODE, FRSTEM )
            CALL CHCKXC ( .FALSE., ' ', OK )


            DEFTXT(2)  =  FRSTEM // 'NAME             = ''#'''
            CALL REPMC  ( DEFTXT(2), '#', FRNAME, DEFTXT(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Frame class and class ID come next.
!
            DEFTXT(3)   =  FRSTEM // 'CLASS            = 5'

            DEFTXT(4)   =  FRSTEM // 'CLASS_ID         = #'
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI   ( DEFTXT(4), '#', FRCODE, DEFTXT(4) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           The frame center is always the primary vector's
!           observer, for now.  The center must be specified
!           via a code.
!
            DEFTXT(5)   =  FRSTEM // 'CENTER           = #'

            CALL BODN2C ( PRIOBS, J, FOUND )

            IF ( .NOT. FOUND ) THEN
               CALL SETMSG ( 'No ID code for #.' )
               CALL ERRCH  ( '#', PRIOBS         )
               CALL SIGERR ( 'SPICE(BUG)'        )
            END IF

            CALL REPMI  ( DEFTXT(5), '#',  J, DEFTXT(5) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Set the base frame.
!
            DEFTXT(6)   =  FRSTEM // 'RELATIVE         = ''#'''
            CALL REPMC  ( DEFTXT(6), '#', BFRAME, DEFTXT(6) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Set the frame definition style and family.
!
            DEFTXT(7)  = &
            FRSTEM // KWSTYL//'       = '''// KVPARM //''''
            CALL REPMI  ( DEFTXT(7), '#', FRCODE,    DEFTXT(7) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DEFTXT(8)  = &
            FRSTEM // KWFFAM//'             = '''// KV2VEC //''''
            CALL REPMI  ( DEFTXT(8), '#', FRCODE,    DEFTXT(8) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DEFTXT(9)  =  FRSTEM//KWRSTA//'       =  '''// KVROTG //''''
            CALL REPMI  ( DEFTXT(9), '#', FRCODE,    DEFTXT(9) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Create definition assignments for the primary and secondary
!           axes.
!
!           At this point, we'll have to abandon hard-coded array
!           indices for the DEFTXT array and instead use the variable
!           DX to represent the current index.
!
            DX = 10
!
!           Blank out the variables containing associated frame names,
!           so we don't end up with residual values from other test
!           cases in these names.
!
            PRIVF  = ' '
            SECVF  = ' '
            VFRAME = ' '


            DO I = 1, 2

               IF ( I .EQ. 1 ) THEN
!
!                 On the first pass, set up definitions for the
!                 primary vector.
!
                  ORD  = 'PRI_'
                  VDF  = PRIVDF
                  VFR  = PRIFRM
                  COR  = PRICOR
                  OBS  = PRIOBS
                  TARG = PRITRG

               ELSE
!
!                 On the second pass, set up definitions for the
!                 secondary vector.
!
                  ORD  = 'SEC_'
                  VDF  = SECVDF
                  VFR  = SECFRM
                  COR  = SECCOR
                  OBS  = SECOBS
                  TARG = SECTRG

               END IF

!
!              Identify the axis { +/-X, +/-Y, +/-Z } associated
!              with the current vector.
!
               DEFTXT(DX) =  FRSTEM // ORD // 'AXIS         = ''#'''

               CALL REPMC  ( DEFTXT(DX),      '#', &
                             AXDEF(I,AXPAIR), DEFTXT(DX) )


               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1

!
!              Set the current vector definition.
!
               DEFTXT(DX) =  FRSTEM // ORD // KWVCDF // '     = ''#'''
               CALL REPMC  ( DEFTXT(DX), '#', VDF, DEFTXT(DX) )
               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1

!
!              Set the current vector observer.
!
               DEFTXT(DX) =  FRSTEM // ORD // KWVOBS // '     = ''#'''
               CALL REPMC  ( DEFTXT(DX), '#', OBS, DEFTXT(DX) )
               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1

!
!              Set the current vector target.  Constant vectors
!              don't have targets.
!
               IF ( VDF .NE. 'CONSTANT' ) THEN

                  DEFTXT(DX) =  FRSTEM // ORD// KWVTRG // '     = ''#'''
                  CALL REPMC  ( DEFTXT(DX), '#', TARG, DEFTXT(DX) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  DX         =  DX + 1

               END IF
!
!              Set the current vector's aberration correction.
!
               DEFTXT(DX) =  FRSTEM // ORD// KWVABC // '     = ''#'''

               CALL REPMC  ( DEFTXT(DX), '#', COR, DEFTXT(DX) )
               CALL CHCKXC ( .FALSE., ' ', OK )
               DX         =  DX + 1

!
!              Set the current vector's frame, if necessary.
!              Velocity vectors and constant vectors have associated
!              frames.
!
               IF (     ( VDF .EQ. 'OBSERVER_TARGET_VELOCITY' ) &
                   .OR. ( VDF .EQ. 'CONSTANT'                 ) ) THEN

                  IF (      ( VFR .EQ. 'J2000'         ) &
                       .OR. ( VFR .EQ. 'IAU_JUPITER'   ) &
                       .OR. ( VFR .EQ. 'GSE'           ) &
                       .OR. ( VFR .EQ. 'IAU_PHOBOS'    ) &
                       .OR. ( VFR .EQ. 'PHOBOS_RADIAL' ) ) THEN

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

!
!                 Save the resolved frame names for later testing.
!
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

!
!              If the vector is constant, define it.  The vector
!              definitions have no interaction with the other defining
!              parameters other than the vector order, so all we need
!              do is make sure all definitions are covered for the
!              primary and secondary vectors.  We'll use the following
!              definitions:
!
!                 ... SPEC      = 'RECTANGULAR'
!                 ... VECTOR    = ( <number1>, <number2>, <number3> )
!
!                 ... SPEC      = 'RECTANGULAR'
!                 ... VECTOR    = ( <number4>, <number5>, <number6> )
!
!                 ... SPEC      = 'RECTANGULAR'
!                 ... VECTOR    = ( <number7>, <number8>, <number9> )
!
!                 ... SPEC      = 'LATITUDINAL'
!                 ... UNITS     = 'DEGREES'
!                 ... LONGITUDE = <number>
!                 ... LATITUDE  = <number>
!
!                 ... SPEC      = 'LATITUDINAL'
!                 ... UNITS     = 'RADIANS'
!                 ... LONGITUDE = <number>
!                 ... LATITUDE  = <number>
!
!
!
               IF ( VDF .EQ. 'CONSTANT' ) THEN

                  IF ( I .EQ. 1 ) THEN
                     NCOR = NCOR + 1
                  END IF

!
!                 Make the vector selection depend on the case
!                 and whether the axis is primary or secondary.
!
                  J    = MOD ( NCOR + I, 5 ) + 1

                  IF ( J .LE. 3 ) THEN
!
!                    This is the rectangular coordinate case.
!
                     SYSIDX     =  1

                     DEFTXT(DX) =  FRSTEM // ORD // KWVSPC // &
                                   '         = ' // CORSYS(SYSIDX)
                     DX         =  DX + 1


                     DEFTXT(DX) =  FRSTEM // ORD // KWVECT // &
                                   '       = '

                     CALL SUFFIX ( VECSTR(J), 1,  DEFTXT(DX) )
                     DX         =  DX + 1

!
!                    Produce the d.p. constant vector; we'll use this
!                    in later testing.  Start by parsing the string
!                    representing the vector.  The first token will
!                    be a blank (representing the null token to the
!                    left of the left parenthesis), so start with
!                    token #2.
!
                     CALL LPARSM ( VECSTR(J), '(),', 7, N, TOKENS )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     DO L = 2, 4
                        CALL PRSDP ( TOKENS(L), CONVEC(L-1) )
                        CALL CHCKXC ( .FALSE., ' ', OK )
                     END DO

                  ELSE
!
!                    This is one of the latitudinal or RA/DEC
!                    coordinate cases.
!
                     IF ( J .EQ. 4 ) THEN
!
!                       Use latitudinal coordinates.
!
                        SYSIDX =  2
                     ELSE
!
!                       Use RA/DEC coordinates.
!
                        SYSIDX =  3
                     END IF

                     DEFTXT(DX) =  FRSTEM // ORD // KWVSPC // &
                                   '         = ' // CORSYS(SYSIDX)
                     DX         =  DX + 1

!
!                    Let K be the index of the units.
!
                     K          = J - 3

                     DEFTXT(DX) =  FRSTEM // ORD // KWUNIT // &
                                   '        = ' // UNITS(K)
                     DX         =  DX + 1

                     IF ( J .EQ. 4 ) THEN
                        DEFTXT(DX) =  FRSTEM // ORD // KWLON // &
                                      '    = '
                     ELSE
                        DEFTXT(DX) =  FRSTEM // ORD // KWRA // &
                                      '           = '
                     END IF

                     CALL SUFFIX ( ANGSTR(1,K), 1,  DEFTXT(DX) )
                     CALL CHCKXC ( .FALSE., ' ', OK )
!
!                    Save the longitude as a d.p. number.
!
                     CALL PRSDP ( ANGSTR(1,K), LON0 )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     DX         =  DX + 1

                     IF ( J .EQ. 4 ) THEN
                        DEFTXT(DX) =  FRSTEM // ORD // KWLAT // &
                                      '     = '
                     ELSE
                        DEFTXT(DX) =  FRSTEM // ORD // KWDEC // &
                                      '          = '
                     END IF

                     CALL SUFFIX ( ANGSTR(2,K),   1,  DEFTXT(DX) )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     DX         =  DX + 1
!
!                    Save the latitude as a d.p. number.
!
                     CALL PRSDP ( ANGSTR(2,K), LAT0 )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Convert the numeric lat/lon values to radians.
!
                     CALL CMPRSS ( '''', 0, UNITS(K), TMPSTR )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CONVRT ( LAT0, TMPSTR, KVRADN, LAT )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL CONVRT ( LON0, TMPSTR, KVRADN, LON )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Produce a d.p. constant vector.
!
                     CALL LATREC ( 1.D0, LON, LAT, CONVEC )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF
!
!                 Save the constant vector as one of the defining
!                 vectors.
!
                  IF ( I .EQ. 1 ) THEN
                     CALL VEQU ( CONVEC, PRIVEC )
                  ELSE
                     CALL VEQU ( CONVEC, SECVEC )
                  END IF

               END IF

            END DO



!
!           Enter the frame definition into the kernel pool.
!
            CALL LMPOOL ( DEFTXT, DX-1 )
            CALL CHCKXC ( .FALSE., ' ', OK )





!
!           Here's where the testing begins.
!
!
!--- Case: ------------------------------------------------------
!
            CALL REPMI  ( 'SXFORM test number #.  Test results ' // &
                          'against those from PXFORM.', &
                          '#', CASE, CASMSG )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )

!
!           Pick a time value.
!
            ET = 1.D3 * ( CASE - 5000 )

!            WRITE (*,*) 'F_DYN01:  SXFORM CALL <<<<<<<<<<<<<<<<<<'

            CALL SXFORM ( FRNAME, BFRAME, ET, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )


!
!           IF ( .NOT. OK ) THEN
!              DO I = 1, DX-1
!                 WRITE (*,*) DEFTXT(I)
!              END DO
!
!              WRITE (*,*) '=================================== '
!           END IF
!

!            WRITE (*,*) 'F_DYN01:  DONE <<<<<<<<<<<<<<<<<<'

            CALL PXFORM ( FRNAME, BFRAME, ET, R )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Extract the derivative block from XFORM into DRVBLK.
!           We'll use this block later; we're extracting it here
!           to keep this code close to the associated SXFORM call.
!
            DO I = 1, 3
               DO J = 1, 3
                  DRVBLK(J,I) = XFORM(J+3,I)
               END DO
            END DO

            DMAG = VNORMG( DRVBLK, 9 )

!
!           Extract the rotation block from XFORM into R2.
!
            DO I = 1, 3
               DO J = 1, 3
                  R2(J,I) = XFORM(J,I)
               END DO
            END DO

            CALL CHCKAD ( 'SXFORM vs PXFORM', R2, '~', R, 9, DM12, OK )

!
!               These statements may be useful for debugging.
!
!
!               IF ( .NOT. OK ) THEN
!
!                  WRITE (*,*) '=================================== '
!                  WRITE (*,*) 'CASE = ', CASE
!                  WRITE (*,*) COORDS
!                  WRITE (*,*) 'PRIFRM  = ', PRIFRM
!                  WRITE (*,*) 'PRIJ    = ', PRIJ
!                  WRITE (*,*) 'PRIVF   = ', PRIVF
!                  WRITE (*,*) 'VDF     = ', VDF
!                  WRITE (*,*) 'VFRAME  = ', VFRAME
!                  WRITE (*,*) 'DRVERR, DRLERR  = ', DRVERR, DRLERR
!                  WRITE (*,*) 'DMAG    = ', DMAG
!                  WRITE (*,*) 'DM16  /DMAG    = ', DM16  /DMAG
!
!                 DO I = 1, DX-1
!                    WRITE (*,*) DEFTXT(I)
!                 END DO
!
!                 WRITE (*,*) '=================================== '
!
!               END IF
!


!
!--- Case: ------------------------------------------------------
!
            CALL REPMI  ( 'PXFORM test number #.  ' // &
                          'Primary vector def = #.', &
                                  '#', CASE,   CASMSG )
            CALL REPMC  ( CASMSG, '#', PRIVDF, CASMSG )

            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )

!
!           Find the primary axis name and sign.
!
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

!
!           Create the primary vector, specified relative to the
!           base frame.  Map this vector to the current dynamic
!           frame.  The vector should match the current axis
!           specification for the primary defining vector.
!
            IF ( PRIVDF .EQ. 'OBSERVER_TARGET_POSITION' ) THEN

               CALL SPKPOS ( PRITRG, ET, 'J2000', PRICOR, &
                             PRIOBS, V,  LT              )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL PXFORM ( 'J2000', FRNAME, ET, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV  ( R2,      V,     VTEMP )
               CALL VSCL ( AXSIGN, VTEMP,  V     )
               CALL VHAT ( V,      PRIVEC        )

!
!              PRIVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~', &
                             E(1,AXINDX),  3,       DM12 ,  OK )


            ELSE IF ( PRIVDF .EQ. 'TARGET_NEAR_POINT' ) THEN
!
!              Find the specified near point.
!
               CALL SUBPT ( 'NEAR POINT', PRITRG, ET, PRICOR, &
                            PRIOBS,       SPOINT, ALT         )

!
!              We actually need to use the position transformation
!              from PRIVF to FRMNAM for this one.  NOTE:  the
!              epoch of this transformation is critical.  We must
!              use the same epoch as was used by SUBPT.
!
!              Get the light time LT from the observer to target
!              center.
!
               CALL SPKPOS ( PRITRG, ET,    'J2000', PRICOR, &
                             PRIOBS, POS,   LT               )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Find the epoch associated with the target by
!              adjusting ET by LT.
!
               CALL ZZCOREPC ( PRICOR, ET, LT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Find the position transformation from the target
!              body-fixed frame at ETCORR to J2000.  Convert
!              the body-fixed sub-point position to the J2000
!              frame.
!
               CALL CNMFRM ( PRITRG, J, TRGFRM, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'TARGET FRAME FOUND', FOUND, .TRUE., OK )

               CALL PXFORM ( TRGFRM,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV    ( R2, SPOINT, V )

!
!              Compute the observer-to-near point vector in the J2000
!              frame.
!
               CALL VADD ( V, POS, VTEMP )

!
!              Find the position transformation J2000 the current frame
!              at ET.  Rotate the observer-near point vector into this
!              frame.
!
               CALL PXFORM ( 'J2000', FRNAME, ET, R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV    ( R3,      VTEMP,  V )

!
!              Negate the vector if indicated, then unitize.
!
               CALL VSCL ( AXSIGN,    V,      VTEMP  )
               CALL VHAT ( VTEMP,     PRIVEC         )

!
!              PRIVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~', &
                             E(1,AXINDX),  3,       DM12,  OK )


            ELSE IF ( PRIVDF .EQ. 'OBSERVER_TARGET_VELOCITY' ) THEN
!
!              Look up the velocity vector in the specified frame.
!              Note this choice is critical:  any other frame,
!              unless related by a constant offset to the correct
!              frame, yields an invalid result.
!
               CALL SPKEZR ( PRITRG, ET,    PRIVF, PRICOR, &
                             PRIOBS, STATE, LT            )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              We actually need to use the position transformation
!              from PRIVF to FRMNAM for this one.  NOTE:  the
!              epoch of this transformation is critical.  We must
!              use the same epoch as was used by SPKEZR.  If
!              PRIVF is non-inertial, we need to adjust ET by
!              the light time from the observer to the center of PRIVF.
!              We can actually finesse the issue of whether the frame
!              is non-inertial, since performing the correction for
!              an inertial frame has no effect.
!
               CALL NAMFRM ( PRIVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )

               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )

!
!              Get the light time CLT from the observer to the center
!              of the frame in which we looked up the velocity.
!
               CALL SPKEZR ( CENTER, ET,     PRIVF, PRICOR, &
                             PRIOBS, CSTATE, CLT            )
               CALL CHCKXC ( .FALSE., ' ', OK )


!
!              Find the epoch associated with the center of the
!              frame by adjusting ET by CLT.
!
               CALL ZZCOREPC ( PRICOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Find the position transformation from PRIVF at
!              ETCORR to the current frame at ET.  We must do
!              this in two steps, passing through J2000.
!
               CALL PXFORM ( PRIVF,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXM    ( R3,      R2,     R4         )
!
!              Extract the velocity vector.  Rotate to the current
!              frame, negate if indicated, then unitize.
!
               CALL MXV  ( R4,       STATE(4),  V      )
               CALL VSCL ( AXSIGN,   V,         VTEMP  )
               CALL VHAT ( VTEMP,    PRIVEC            )

!
!              PRIVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~', &
                             E(1,AXINDX),  3,       DM12,  OK )



            ELSE IF ( PRIVDF .EQ. 'CONSTANT' ) THEN
!
!              The primary vector is a constant; the vector is
!              stored in PRIVEC.
!
!              We need to use the position transformation from PRIVF
!              to FRMNAM for this one.  NOTE:  the epoch of this
!              transformation is critical. If PRIVF is non-inertial, we
!              need to adjust ET by the light time from the observer to
!              the center of PRIVF. We can actually finesse the issue
!              of whether the frame is non-inertial, since performing
!              the correction for an inertial frame has no effect.
!
               CALL NAMFRM ( PRIVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )

               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )

!
!              Parse the aberration correction for the primary
!              vector.  Get the light time to the frame center if
!              light time corrections are used.
!
               CALL ZZPRSCOR ( PRICOR, PRIBLK )

               IF ( PRIBLK(LTIDX) ) THEN
!
!                 Get the light time CLT from the observer to the
!                 center of the frame in which the constant vector is
!                 defined.
!
                  CALL SPKEZR ( CENTER, ET,     PRIVF, PRICOR, &
                                PRIOBS, CSTATE, CLT            )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE
!
!                 The frame is not corrected for light time:
!                 the light time to the frame center is treated as
!                 zero.
!
                  CLT = 0.D0

               END IF

!
!              Find the epoch associated with the center of the
!              frame by adjusting ET by CLT.
!
               CALL ZZCOREPC ( PRICOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              We now want to express the constant vector in
!              the current dynamic frame at ET.  We must
!              transform the constant vector in two steps,
!              since the epoch associated with the frame in
!              which the constant vector is specified is ETCORR.
!              We first map the constant vector to the J2000
!              frame, then to the current frame.
!
!              When the constant vector is expressed relative
!              to the J2000 frame, we apply the stellar aberration
!              correction if necessary.
!
!              Find the position transformation from PRIVF at
!              ETCORR to J2000.  Apply to PRIVEC.
!
               CALL PXFORM ( PRIVF,   'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ',     OK )

               CALL MXV    ( R2,      PRIVEC,  V  )

               IF ( PRIBLK(STLIDX) ) THEN
!
!                 Stellar aberration correction is required.
!
!                 Find the state of the observer relative to the
!                 solar system barycenter at ET, in the J2000 frame.
!
                  CALL BODN2C ( PRIOBS,  J,    FOUND )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )

                  CALL SPKSSB ( J,       ET,  'J2000', STOBS )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )

                  IF ( PRIBLK(XMTIDX) ) THEN
!
!                    Apply transmission stellar aberration correction.
!
                     CALL STLABX ( V, STOBS(4), VTEMP )

                  ELSE
!
!                    Apply transmission stellar aberration correction.
!
                     CALL STELAB ( V, STOBS(4), VTEMP )

                  END IF

                  CALL VEQU ( VTEMP, V )

               END IF
!
!              The vector V contains our corrected (if necessary)
!              constant vector relative to J2000.
!
!              Look up the rotation from J2000 to the current frame
!              at ET.  Apply to our constant vector.
!
               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ',    OK         )
               CALL MXV    ( R3,      V,      PRIVEC     )

!
!              Negate the constant vector if indicated, then unitize.
!
               CALL VSCL ( AXSIGN,   PRIVEC,  VTEMP  )
               CALL VHAT ( VTEMP,    PRIVEC          )

!
!              PRIVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'PRIVEC',     PRIVEC,  '~', &
                             E(1,AXINDX),  3,       DM12 ,  OK )


            ELSE
!
!              We have an unrecognized vector definition.
!
               CALL SETMSG ( 'F_DYN01 encountered vector ' // &
                             'definition #.'               )
               CALL ERRCH  ( '#',  PRIVDF                  )
               CALL SIGERR ( 'SPICE(BUG)'                  )

               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF



!
!
!           We're now going to test the secondary vector.  We could
!           have combined this code in a loop together with the tests
!           for the primary vector, but "unrolling" the loop makes
!           the test code a bit easier to look at.
!
!
!--- Case: ------------------------------------------------------
!
            CALL REPMI  ( 'PXFORM test number #.  ' // &
                          'Secondary vector def = #.', &
                                  '#', CASE,   CASMSG )
            CALL REPMC  ( CASMSG, '#', SECVDF, CASMSG )

            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )


!
!           Find the secondary axis name and sign.
!
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

!
!           Create the secondary vector, specified relative to the base
!           frame.  Map this vector to the current dynamic frame.  The
!           component of this vector orthogonal to the primary vector
!           should match the current axis specification for the
!           secondary defining vector.
!
            IF ( SECVDF .EQ. 'OBSERVER_TARGET_POSITION' ) THEN

               CALL SPKPOS ( SECTRG, ET, 'J2000', SECCOR, &
                             SECOBS, V,  LT              )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL PXFORM ( 'J2000', FRNAME, ET, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV  ( R2,      V,     VTEMP )
               CALL VSCL ( AXSIGN, VTEMP,  V     )

!
!              PRIVEC is left over from the tests on the primary
!              axis.  Replace SECVEC with its component orthogonal
!              to PRIVEC.  Unitize this component.
!
               CALL VPERP ( V,     PRIVEC,  VTEMP  )
               CALL VHAT  ( VTEMP,          SECVEC )

!
!              SECVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~', &
                             E(1,AXINDX),  3,       DM12 ,  OK )


            ELSE IF ( SECVDF .EQ. 'TARGET_NEAR_POINT' ) THEN
!
!              Find the specified near point.
!
               CALL SUBPT ( 'NEAR POINT', SECTRG, ET, SECCOR, &
                            SECOBS,       SPOINT, ALT         )

!
!              We actually need to use the position transformation
!              from SECVF to FRMNAM for this one.  NOTE:  the
!              epoch of this transformation is critical.  We must
!              use the same epoch as was used by SUBPT.
!
!              Get the light time LT from the observer to target
!              center.
!
               CALL SPKPOS ( SECTRG, ET,    'J2000', SECCOR, &
                             SECOBS, POS,   LT               )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Find the epoch associated with the target by
!              adjusting ET by LT.
!
               CALL ZZCOREPC ( SECCOR, ET, LT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Find the position transformation from the target
!              body-fixed frame at ETCORR to J2000.  Convert
!              the body-fixed sub-point position to the J2000
!              frame.
!
               CALL CNMFRM ( SECTRG, J, TRGFRM, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'TARGET FRAME FOUND', FOUND, .TRUE., OK )

               CALL PXFORM ( TRGFRM,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV    ( R2, SPOINT, V )

!
!              Compute the observer-to-near point vector in the J2000
!              frame.
!
               CALL VADD ( V, POS, VTEMP )

!
!              Find the position transformation J2000 the current frame
!              at ET.  Rotate the observer-near point vector into this
!              frame.
!
               CALL PXFORM ( 'J2000', FRNAME, ET, R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV    ( R3,      VTEMP,  V )

!
!              Negate the vector if indicated, then unitize.
!
               CALL VSCL ( AXSIGN,    V,      VTEMP  )

!
!              PRIVEC is left over from the tests on the primary
!              axis.  Replace SECVEC with its component orthogonal
!              to PRIVEC.  Unitize this component.
!
               CALL VPERP ( VTEMP, PRIVEC,  V      )
               CALL VHAT  ( V,              SECVEC )

!
!              SECVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~', &
                             E(1,AXINDX),  3,       DM12 ,  OK )



            ELSE IF ( SECVDF .EQ. 'OBSERVER_TARGET_VELOCITY' ) THEN
!
!              Look up the velocity vector in the specified frame.
!              Note this choice is critical:  any other frame,
!              unless related by a constant offset to the correct
!              frame, yields an invalid result.
!
               CALL SPKEZR ( SECTRG, ET,    SECVF, SECCOR, &
                             SECOBS, STATE, LT            )

               CALL CHCKXC ( .FALSE., ' ', OK )


!               WRITE (*,*) 'F_DYN01:'
!               WRITE (*,*) 'STATE = ', STATE
!               WRITE (*,*) 'SECVF = ', SECVF
!               WRITE (*,*) 'ET    = ', ET
!               CALL VHAT (STATE(4), UVEL )
!               WRITE (*, * ) 'UVEL = '
!               WRITE (*,'(1X,3E25.17)' ) UVEL


!
!              We actually need to use the position transformation
!              from SECVF to FRMNAM for this one.  NOTE:  the
!              epoch of this transformation is critical.  We must
!              use the same epoch as was used by SPKEZR.  If
!              SECVF is non-inertial, we need to adjust ET by
!              the light time from the observer to the center of SECVF.
!              We can actually finesse the issue of whether the frame
!              is non-inertial, since performing the correction for
!              an inertial frame has no effect.
!
               CALL NAMFRM ( SECVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )

               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )

!
!              Get the light time CLT from the observer to the center
!              of the frame in which we looked up the velocity.
!
               CALL SPKEZR ( CENTER, ET,     SECVF, SECCOR, &
                             SECOBS, CSTATE, CLT            )
               CALL CHCKXC ( .FALSE., ' ', OK )


!
!              Find the epoch associated with the center of the
!              frame by adjusting ET by CLT.
!
               CALL ZZCOREPC ( SECCOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Find the position transformation from SECVF at
!              ETCORR to the current frame at ET.  We must do
!              this in two steps, passing through J2000.
!
               CALL PXFORM ( SECVF,  'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXM    ( R3,      R2,     R4         )
!
!              Extract the velocity vector.  Rotate to the current
!              frame, negate if indicated, then unitize.
!
               CALL MXV  ( R4,       STATE(4),  VTEMP     )
               CALL VSCL ( AXSIGN,   VTEMP,            V  )

!
!              PRIVEC is left over from the tests on the primary
!              axis.  Replace SECVEC with its component orthogonal
!              to PRIVEC.  Unitize this component.
!
               CALL VPERP ( V,     PRIVEC,  VTEMP  )
               CALL VHAT  ( VTEMP,          SECVEC )

!
!              PRIVEC is left over from the tests on the primary
!              axis.  Replace SECVEC with its component orthogonal
!              to PRIVEC.
!
               CALL VPERP ( SECVEC, PRIVEC, VTEMP  )
               CALL VEQU  ( VTEMP,          SECVEC )

!
!              SECVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~', &
                             E(1,AXINDX),  3,       DM12 ,  OK )



            ELSE IF ( SECVDF .EQ. 'CONSTANT' ) THEN
!
!              The secondary vector is a constant; the vector is
!              stored in SECVEC.
!
!              We need to use the position transformation from SECVF
!              to FRMNAM for this one.  NOTE:  the epoch of this
!              transformation is critical. If SECVF is non-inertial, we
!              need to adjust ET by the light time from the observer to
!              the center of SECVF. We can actually finesse the issue
!              of whether the frame is non-inertial, since performing
!              the correction for an inertial frame has no effect.
!
               CALL NAMFRM ( SECVF, J )
               CALL FRINFO ( J, CENT, CLASS, CLSSID, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRAME CENTER FOUND', FOUND, .TRUE., OK )

               CALL BODC2N ( CENT, CENTER, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'CENTER NAME FOUND', FOUND, .TRUE., OK )

!
!              Parse the aberration correction for the secondary
!              vector.  Get the light time to the frame center if
!              light time corrections are used.
!
               CALL ZZPRSCOR ( SECCOR, SECBLK )

               IF ( SECBLK(LTIDX) ) THEN
!
!                 Get the light time CLT from the observer to the
!                 center of the frame in which the constant vector is
!                 defined.
!
                  CALL SPKEZR ( CENTER, ET,     SECVF, SECCOR, &
                                SECOBS, CSTATE, CLT            )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE
!
!                 The frame is not corrected for light time:
!                 the light time to the frame center is treated as
!                 zero.
!
                  CLT = 0.D0

               END IF

!
!              Find the epoch associated with the center of the
!              frame by adjusting ET by CLT.
!
               CALL ZZCOREPC ( SECCOR, ET, CLT, ETCORR )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              We now want to express the constant vector in
!              the current dynamic frame at ET.  We must
!              transform the constant vector in two steps,
!              since the epoch associated with the frame in
!              which the constant vector is specified is ETCORR.
!              We first map the constant vector to the J2000
!              frame, then to the current frame.
!
!              When the constant vector is expressed relative
!              to the J2000 frame, we apply the stellar aberration
!              correction if necessary.
!
!              Find the position transformation from SECVF at
!              ETCORR to J2000.  Apply to SECVEC.
!
               CALL PXFORM ( SECVF,   'J2000', ETCORR, R2 )
               CALL CHCKXC ( .FALSE., ' ',     OK )

               CALL MXV    ( R2,      SECVEC,  V  )

               IF ( SECBLK(STLIDX) ) THEN
!
!                 Stellar aberration correction is required.
!
!                 Find the state of the observer relative to the
!                 solar system barycenter at ET, in the J2000 frame.
!
                  CALL BODN2C ( SECOBS,  J,    FOUND )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )

                  CALL SPKSSB ( J,       ET,  'J2000', STOBS )
                  CALL CHCKXC ( .FALSE., ' ',  OK    )

                  IF ( SECBLK(XMTIDX) ) THEN
!
!                    Apply transmission stellar aberration correction.
!
                     CALL STLABX ( V, STOBS(4), VTEMP )

                  ELSE
!
!                    Apply transmission stellar aberration correction.
!
                     CALL STELAB ( V, STOBS(4), VTEMP )

                  END IF

                  CALL VEQU ( VTEMP, V )

               END IF

!
!              The vector V contains our (corrected if necessary)
!              constant vector relative to J2000.
!
!              Look up the rotation from J2000 to the current frame
!              at ET.  Apply to our constant vector.
!
               CALL PXFORM ( 'J2000', FRNAME, ET,     R3 )
               CALL CHCKXC ( .FALSE., ' ',    OK         )
               CALL MXV    ( R3,      V,      SECVEC     )

!
!              Negate the constant vector if indicated.
!
               CALL VSCL ( AXSIGN,   SECVEC,  VTEMP  )

!
!              PRIVEC is left over from the tests on the primary
!              axis.  Replace SECVEC with its component orthogonal
!              to PRIVEC.  Unitize this component.
!
               CALL VPERP ( VTEMP, PRIVEC,  V      )
               CALL VHAT  ( V,              SECVEC )

!
!              SECVEC should match the axis indicated by AXINDX.
!
               CALL CHCKAD ( 'SECVEC',     SECVEC,  '~', &
                             E(1,AXINDX),  3,       DM12 ,  OK )


            ELSE
!
!              We have an unrecognized vector definition.
!
               CALL SETMSG ( 'F_DYN01 encountered vector ' // &
                             'definition #.'               )
               CALL ERRCH  ( '#',  SECVDF                  )
               CALL SIGERR ( 'SPICE(BUG)'                  )

               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF





!
!--- Case: ------------------------------------------------------
!
            CALL REPMI  ( 'SXFORM test number #.  Test derivative ' // &
                          'block.', &
                          '#', CASE, CASMSG )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( CASMSG )

!
!           Perform "sanity checks" on the returned matrix.
!           Make sure the diagonal blocks are identical rotations,
!           compute a discrete derivative and compare to the
!           lower left block, and make sure the upper right
!           block is a zero matrix.
!
            DELTA = 1.D0

            CALL PXFORM ( FRNAME, BFRAME, ET-DELTA, RMINUS )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL PXFORM ( FRNAME, BFRAME, ET+DELTA, RPLUS  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA, &
                           NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the error measurements we've made.  First
!           up is the determinant error.
!
            TOL = DM14
            CALL CHCKSD ( 'DETERR', DETERR, '~', 0.D0, TOL, OK )

!
!           Check norms of rows and columns in the rotation
!           blocks.
!
            CALL CHCKSD ( 'NRMERR', NRMERR, '~', 0.D0, TOL, OK )

!
!           Check the derivative error when measured against
!           the discrete derivative.
!
            MAXDER = MAX ( MAXDER, DRVERR )

            IF (      ( INDEX(PRICOR, 'S') .GT. 0 ) &
                 .OR. ( INDEX(SECCOR, 'S') .GT. 0 ) ) THEN

               TOL = DM4
!               TOL = DM5
            ELSE
!               TOL = DM5
               TOL = 5.D-7
            END IF


            CALL CHCKSD ( 'DRVERR', DRVERR, '~', 0.D0, TOL, OK )


!
!                 These statements may be useful for debugging.
!
!               IF ( .NOT. OK ) THEN
!
!                  WRITE (*,*) '=================================== '
!                  WRITE (*,*) 'CASE = ', CASE
!                  WRITE (*,*) COORDS
!                  WRITE (*,*) 'PRIFRM  = ', PRIFRM
!                  WRITE (*,*) 'PRIJ    = ', PRIJ
!                  WRITE (*,*) 'PRIVF   = ', PRIVF
!                  WRITE (*,*) 'VDF     = ', VDF
!                  WRITE (*,*) 'SECVDF  = ', SECVDF
!                  WRITE (*,*) 'SECOBS  = ', SECOBS
!                  WRITE (*,*) 'SECTRG  = ', SECTRG
!                  WRITE (*,*) 'SECVF   = ', SECVF
!                  WRITE (*,*) 'VFRAME  = ', VFRAME
!                  WRITE (*,*) 'DRVERR  = ', DRVERR
!                  WRITE (*,*) 'PRIVEC  = ', PRIVEC
!                  WRITE (*,*) 'SECVEC  = ', SECVEC
!                  WRITE (*,*) 'DMAG    = ', DMAG
!                  WRITE (*,*) 'DM16  /DMAG    = ', DM16  /DMAG
!
!                  CALL M33 ( 'DRDIFF', DRDIFF )
!
!                 DO I = 1, DX-1
!                    WRITE (*,*) DEFTXT(I)
!                 END DO
!
!                 WRITE (*,*) '=================================== '
!
!              END IF

!
!           Check the derivative relative error.  Perform this
!           test only if both aberration corrections are 'NONE'
!           and if the derivative error is not to close to
!           machine epsilon.
!
!

!
!           Decide whether we'll do a derivative test.
!
            TSTDRV = .TRUE.
            TOL    =  0.D0

            IF (       ( PRICOR .EQ. 'NONE'  ) &
                 .AND. ( SECCOR .EQ. 'NONE'  )  ) THEN
!
!              No aberration corrections are used.
!
!              There are a few special cases where we must expect
!              quite inaccurate answers.  Set the tolerances for
!              these first.
!
!              The Phobos-earth and earth-sun velocity vectors in the
!              IAU_PHOBOS and PHOBOS_RADIAL frames are particularly
!              problematic.
!
               IF  ( DRVERR .LT. DM15 ) THEN
!
!                 The relative error is not meaningful; skip
!                 the test.
!
                  NSKIP  =  NSKIP + 1
                  TSTDRV = .FALSE.

               ELSE IF  ( PRIVF  .EQ. 'PHOBOS_RADIAL' ) THEN

!                  TOL =  DM5
                  TOL =  DM4

               ELSE IF  (       ( PRIOBS .EQ. 'PHOBOS'        ) &
                          .AND. ( PRITRG .EQ. 'MARS'          ) &
                          .AND. ( PRIVF  .EQ. 'IAU_PHOBOS'    ) &
                          .AND. ( PRIVDF .EQ.  OTV            ) ) THEN

!                  TOL =  DM4
                  TOL =  DM6

                  NPR = NPR + 1

!                  WRITE (*,*) 'IAU_PHOBOS, NPR = ', NPR

               END IF


               IF  (       ( SECOBS .EQ. 'PHOBOS'        ) &
                     .AND. ( SECTRG .EQ. 'EARTH'         ) &
                     .AND. ( SECVF  .EQ. 'IAU_PHOBOS'    ) &
                     .AND. ( SECVDF .EQ.  OTV            ) ) THEN

                  TOL =  MAX ( TOL, DM4 )

               ELSE IF (       ( SECOBS .EQ. 'EARTH'         ) &
                         .AND. ( SECTRG .EQ. 'SUN'           ) &
                         .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' ) &
                         .AND. ( SECVDF .EQ.  OTV            ) ) THEN

                  TOL =  MAX ( TOL, DM3 )

               ELSE IF (       ( SECOBS .EQ. 'EARTH'         ) &
                         .AND. ( SECTRG .EQ. 'SUN'           ) &
                         .AND. ( SECVF  .EQ. 'IAU-EARTH'     ) &
                         .AND. ( SECVDF .EQ.  OTV            ) ) THEN

                  TOL = MAX ( TOL, DM3 )

               ELSE IF (       ( SECOBS .EQ. 'PHOBOS'        ) &
                         .AND. ( SECTRG .EQ. 'EARTH'         ) &
                         .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' ) &
                         .AND. ( SECVDF .EQ.  OTV            ) ) THEN

                  TOL =  MAX ( TOL, DM2 )


               END IF


               IF ( TOL .EQ. 0.D0 ) THEN

                  IF ( DMAG .GT. DM15 ) THEN
!
!                    This is the normal case.
!
!                    NNORML = NNORML + 1

!                     TOL = MAX ( 10.D0**( -16 - LOG10(DMAG) ),  DM6 )
                     TOL = MAX ( 10.D0**( -16 - LOG10(DMAG) ), DM5 )


                  ELSE IF ( DMAG .EQ. 0.D0 ) THEN
!
!                    This should happen only for constant frames,
!                    in which case estimated and actual derivatives
!                     should---we think---be identically zero.
!
                     TOL = 0.D0

                  ELSE
!
!                    No point in testing the relative error---the
!                    derivative is too small for the discrete estimate
!                    to have any validity.
!
                     NSKIP  =  NSKIP + 1
                     TSTDRV = .FALSE.

                  END IF

               END IF
!
!              The tolerance has been set for the pure geometric
!              cases.
!

            ELSE IF (     ( INDEX(PRICOR, 'LT') .GT. 0  ) &
                     .OR. ( INDEX(SECCOR, 'LT') .GT. 0  ) &
                     .OR. ( INDEX(PRICOR, 'CN') .GT. 0  ) &
                     .OR. ( INDEX(SECCOR, 'CN') .GT. 0  ) &
                     .OR. ( INDEX(PRICOR, 'S' ) .GT. 0  ) &
                     .OR. ( INDEX(SECCOR, 'S' ) .GT. 0  )  ) THEN
!
!              We're using light time corrections possibly
!              stellar aberration corrections).
!
!              There are several special cases where we must expect
!              quite inaccurate answers.  Set the tolerances for
!              these first.
!
!              The phobos-earth velocity vector in the IAU_PHOBOS
!              frame is particularly problematic.
!
               BIGERR = .FALSE.

               IF  (       ( SECOBS .EQ. 'PHOBOS'        ) &
                     .AND. ( SECTRG .EQ. 'EARTH'         ) &
                     .AND. ( SECVF  .EQ. 'IAU_PHOBOS'    ) &
                     .AND. ( SECVDF .EQ.  OTV            ) )  THEN

                  BIGERR = .TRUE.
                  TOL    =  DM4


               ELSE IF (       ( PRIVDF .EQ. 'CONSTANT'      ) &
                         .AND. ( PRIVF  .EQ. 'GSE'           ) &
                         .AND. ( SECOBS .EQ. 'EARTH'         ) &
                         .AND. ( SECTRG .EQ. 'SUN'           ) ) THEN
!
!                 The velocity is just noise for many of these
!                 cases, so the relative error is meaningless.
!
                  BIGERR = .TRUE.
                  TOL    =  0.D0
                  TSTDRV = .FALSE.


               ELSE IF (       ( SECOBS .EQ. 'EARTH'         ) &
                         .AND. ( SECTRG .EQ. 'SUN'           ) &
                         .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' ) &
                         .AND. ( SECVDF .EQ.  OTV            ) ) THEN

                  BIGERR = .TRUE.
                  TOL    =  DM2


               ELSE IF (       ( SECOBS .EQ. 'PHOBOS'        ) &
                         .AND. ( SECTRG .EQ. 'EARTH'         ) &
                         .AND. ( SECVF  .EQ. 'PHOBOS_RADIAL' ) &
                         .AND. ( SECVDF .EQ.  OTV            ) ) THEN

                  BIGERR = .TRUE.
                  TOL    =  DM2


               ELSE IF (       ( PRIVDF .EQ. 'CONSTANT' ) &
                         .AND. ( SECVDF .EQ. 'CONSTANT' ) ) THEN
!
!                 The velocity is just noise for many of these
!                 cases, so the relative error is meaningless.
!
                  BIGERR = .TRUE.
                  TOL    =  0.D0
                  TSTDRV = .FALSE.

               END IF


!
!              Further tolerance adjustments may be needed if we're
!              using stellar aberration.
!
               IF ( TSTDRV ) THEN

                  IF ( INDEX(PRICOR, 'S') .GT. 0 ) THEN

                     IF  (       ( PRIOBS .EQ. 'PHOBOS' ) &
                           .AND. ( PRIVF  .EQ. 'GSE'    ) )  THEN

                        BIGERR = .TRUE.
                        TOL    =  DM1

                     ELSE IF (      (PRIOBS .EQ. 'PHOBOS'       ) &
                              .AND. (PRIVF  .EQ. 'PHOBOS_RADIAL') ) THEN

                        BIGERR = .TRUE.
                        TOL    =  DM3

                     ELSE IF (      (PRIOBS .EQ. 'PHOBOS'  ) &
                              .AND. (PRIVDF .EQ. OTP       ) ) THEN

                        BIGERR = .TRUE.
                        TOL    =  DM3

                     END IF

                     IF (       ( PRIOBS .EQ. 'EARTH'  ) &
                          .AND. ( PRITRG .EQ. 'SUN'    ) &
                          .AND. ( PRIVDF .EQ.  OTV     ) ) THEN

                        BIGERR = .TRUE.

                        IF ( PRIVF .EQ. 'PHOBOS_RADIAL' ) THEN
!
!                          The derivative in this case suffers
!                          from total loss of precision.
!

                           TSTDRV =  .FALSE.
                           TOL    =  0.D0

                        ELSE
!
!                          We have about one digit of information here.
!
                           TOL    =  DM1

                        END IF

                     END IF

                  END IF

                  IF ( INDEX(SECCOR, 'S') .GT. 0 ) THEN

                     IF (       ( SECOBS .EQ. 'EARTH'  ) &
                          .AND. ( SECTRG .EQ. 'SUN'    ) &
                          .AND. ( SECVDF .EQ.  OTV     ) ) THEN

                        BIGERR = .TRUE.

                        IF ( SECVF .EQ. 'PHOBOS_RADIAL' ) THEN
!
!                          The derivative in this case suffers
!                          from total loss of precision.
!

                           TSTDRV =  .FALSE.
                           TOL    =  0.D0

                        ELSE
!
!                          We have about one digit of information here.
!
                           TOL    =  DM1

                        END IF


                     ELSE IF (       ( SECOBS .EQ. 'PHOBOS') &
                               .AND. ( SECTRG .EQ. 'EARTH' ) ) THEN

                        BIGERR =  .TRUE.
                        TOL    =  DM1

                     ELSE IF (      (SECOBS .EQ. 'EARTH'        ) &
                              .AND. (SECVF  .EQ. 'PHOBOS_RADIAL') ) THEN

                        BIGERR =  .TRUE.
                        TOL    =  MAX ( TOL, DM3 )

                     END IF

                  END IF

               END IF

!
!              Set the tolerance for the "normal" cases.
!
               IF ( TSTDRV .AND. ( .NOT. BIGERR ) ) THEN

                  IF ( DMAG .GT. DM10 ) THEN
!
!                    The derivative magnitude is large enough to
!                    squeeze out about four digits of precision.
!
                     TOL = DM4

                  ELSE IF ( DMAG .GT. DM12 ) THEN

!
!                    We may expect about two digits of agreement
!                    between the computed as estimated derivative.
!
                     TOL = DM2

                  ELSE IF ( DMAG .EQ. 0.D0 ) THEN

!
!                    This should happen only for constant frames,
!                    in which case estimated and actual derivatives
!                    should---we think---be identically zero.
!
                     TOL = 0.D0

                  ELSE
!
!                    No point in testing the relative error---the
!                    derivative is too small for the discrete estimate
!                    to have any validity.
!
                     NSKIP  =  NSKIP + 1
                     TSTDRV = .FALSE.

                  END IF

               END IF
!
!              TOL has been set for the aberration correction cases.
!

            END IF
!
!           TOL has been set.
!

            IF ( TSTDRV ) THEN

               TOL = MAX ( TOL, 1.D-7/DMAG )

!               WRITE (*,*) 'USING TOL = ', TOL
               CALL CHCKSD ( 'DRLERR', DRLERR, '~', 0.D0, TOL, OK )



!
!                   These statements may be useful for debugging.
!
!               IF ( .NOT. OK ) THEN
!
!                  WRITE (*,*) '=================================== '
!                  WRITE (*,*) 'CASE = ', CASE
!                  WRITE (*,*) COORDS
!                  WRITE (*,*) 'PRIFRM  = ', PRIFRM
!                  WRITE (*,*) 'PRIJ    = ', PRIJ
!                  WRITE (*,*) 'PRIVDF  = ', PRIVDF
!                  WRITE (*,*) 'PRIOBS  = ', PRIOBS
!                  WRITE (*,*) 'PRITRG  = ', PRITRG
!                  WRITE (*,*) 'PRIVF   = ', PRIVF
!                  WRITE (*,*) 'VDF     = ', VDF
!                  WRITE (*,*) 'SECVDF  = ', SECVDF
!                  WRITE (*,*) 'SECOBS  = ', SECOBS
!                  WRITE (*,*) 'SECTRG  = ', SECTRG
!                  WRITE (*,*) 'SECVF   = ', SECVF
!                  WRITE (*,*) 'VFRAME  = ', VFRAME
!               IF ( ISVEL(1) .AND. ISVEL(2) ) THEN
!                  WRITE (*,*) 'DRVERR, DRLERR  = ', DRVERR, DRLERR
!               END IF
!                  WRITE (*,*) 'PRIVEC  = ', PRIVEC
!                  WRITE (*,*) 'SECVEC  = ', SECVEC
!                   WRITE (*,*) 'DMAG    = ', DMAG
!                   WRITE (*,*) '1.D-7/DMAG    = ', 1.D-7/DMAG
!                   WRITE (*,*) '1.D-8/DMAG    = ', 1.D-8/DMAG
!                  WRITE (*,*) 'DM16  /DMAG    = ', DM16  /DMAG
!
!                  CALL M33 ( 'DRDIFF', DRDIFF )
!                  CALL M66 ( 'XFORM',  XFORM  )

!                  CALL XF2RAV ( XFORM, R, V )

!                  WRITE (*,*) 'Frame angular velocity: ', V
!                  WRITE (*,*) '||V|| = ', VNORM(V)
!

!                WRITE (*,*) 'F_DYN01:'
!                WRITE (*,*) 'STATE = ', STATE
!               WRITE (*,*) 'SECVF = ', SECVF
!               WRITE (*,*) 'ET    = ', ET
!               CALL VHAT (STATE(4), UVEL )
!               WRITE (*, * ) 'UVEL = '
!               WRITE (*,'(1X,3E25.17)' ) UVEL

!                 DO I = 1, DX-1
!                    WRITE (*,*) DEFTXT(I)
!                 END DO
!
!                 WRITE (*,*) '=================================== '
!
!              END IF
!


            END IF


         END IF

      END DO

!      WRITE (*,*) 'Number of cases rejected: ', NOGO
!      WRITE (*,*) 'MAXDER = ', MAXDER
!      WRITE (*,*) 'NSKIP  = ', NSKIP

!      WRITE (*,*) 'NNORML = ', NNORML
!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
