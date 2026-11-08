C$Procedure      F_GFILUM ( GFILUM family tests )

      SUBROUTINE F_GFILUM ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFILUM
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

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

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
C     This routine tests the higher-level SPICELIB
C     geometry routine GFILUM.
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
C
C$ Version
C
C-    TSPICE Version 1.1.0, 28-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change 
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.0.0, 02-APR-2014 (NJB)(EDW)
C
C        Previous version was 29-NOV-2012 (NJB)(EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD

      INTEGER               CARDD
      INTEGER               WNCARD

      LOGICAL               EQSTR

C
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'nat.tpc' )

      CHARACTER*(*)         PCK2
      PARAMETER           ( PCK2   = 'generic.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2   = 'generic.bsp' )

      CHARACTER*(*)         OCTSPK
      PARAMETER           ( OCTSPK = 'octl_test.bsp' )

      CHARACTER*(*)         OCTL
      PARAMETER           ( OCTL   = 'OCTL' )

      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 1.D-9 )


      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 1.0D-6 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.0D-5 )
      
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C      INTEGER               NAMLEN
C      PARAMETER           ( NAMLEN = 32 )

C      INTEGER               NREF
C      PARAMETER           ( NREF = 3 )


      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 100 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2*MAXIVL )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               OPLEN
      PARAMETER           ( OPLEN  = 6 )

      INTEGER               NREL
      PARAMETER           ( NREL   = 9 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 5 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 25 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 25 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )

      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 25 )

      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 25 )

C
C     Local variables
C
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(TYPLEN)    ANGTYP
      CHARACTER*(CRDLEN)    COORD
      CHARACTER*(CORLEN)    CORRS  ( NCORR )
      CHARACTER*(SYSLEN)    CORSYS
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(BDNMLN)    ILLUM
      CHARACTER*(LNSIZE)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(OPLEN)     RELATE
      CHARACTER*(OPLEN)     RELOPS ( NREL )
      CHARACTER*(FRNMLN)    SRFREF
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(TIMLEN)    TIME0
      CHARACTER*(LNSIZE)    TXT    ( TXTSIZ )

      DOUBLE PRECISION      ADJ    ( NREL )
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MODCFN ( LBCELL : MAXWIN )
      DOUBLE PRECISION      MODRES ( LBCELL : MAXWIN )
      DOUBLE PRECISION      NP     (3)
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      VALS   ( NREL )
      DOUBLE PRECISION      XRSULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      XTIME

C
C     The workspace array has to handle the largest workspace
C     we'll use, which is that required by GFPOSC.
C
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN,  NWMAX )


      INTEGER               COUNT
      INTEGER               CORIDX
      INTEGER               HANDLE
      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               I
      INTEGER               N
      INTEGER               OCTID
      INTEGER               OPIDX
      INTEGER               XN

      LOGICAL               FOUND

C
C     Saved everything.
C
      SAVE

C
C     Initial values
C
      DATA                  CORRS  / 'NONE', 'LT', 'CN', 'LT+S', 'CN+S'/
     .

C
C     Note: the absolute extremum operators are repeated because
C     they're used with both zero and non-zero adjustment values.
C
      DATA                  RELOPS / '=',      '<',      '>',
     .                               'LOCMIN', 'LOCMAX', 'ABSMIN',
     .                               'ABSMIN', 'ABSMAX', 'ABSMAX'  /

C
C     Reference values to be used with binary operators:
C     These values are in degrees; they must be converted to
C     radians before use.
C
      DATA                  VALS  /  65.D0,  70.D0,  60.D0,  6*0.D0 /

C
C     Ajustment values to be used with absolute extremum operators:
C
      DATA                  ADJ   /  5 * 0.D0, 0.D0, 5.D-2,
     .                                         0.D0, 5.D-2 /



C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFILUM' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTSPK ( SPK2, .TRUE., HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL NATPCK ( PCK,  .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTPCK ( PCK2, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Set up a confinement window. Initialize this and
C     the result window.
C
      TIME0 = '2000 JAN 1  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Create a confinement window with 4 intervals.
C
      DO I = 1, 4

         LEFT  = ET0  + (I-1)*SPD() + 3600.D0
         RIGHT = LEFT +               3600.D0 * 22

         CALL WNINSD ( LEFT, RIGHT, CNFINE )

      END DO




C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Non-positive step size' )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'Ellipsoid'
      ANGTYP = 'INCIDENCE'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'
      RELATE = 'LOCMAX'
      REFVAL =  0.D0
      ADJUST =  0.D0
      STEP   =  1.D3

      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      STEP = 0.D0


      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


      STEP = -1.D0

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Workspace window size too small.' )

      CALL SSIZED ( 1, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'Ellipsoid'
      ANGTYP = 'INCIDENCE'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'
      RELATE = 'LOCMAX'
      REFVAL =  0.D0
      ADJUST =  0.D0
      STEP   =  1.D3

      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, 1,      NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Result window too small (detected before search)' )

      CALL SSIZED ( 1, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'Ellipsoid'
      ANGTYP = 'INCIDENCE'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'
      RELATE = 'LOCMAX'
      REFVAL =  0.D0
      ADJUST =  0.D0
      STEP   =  1.D3

      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Result window too small (detected during search)' )

      METHOD = 'Ellipsoid'
      ANGTYP = 'INCIDENCE'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'
      RELATE = 'LOCMAX'
      REFVAL =  0.D0
      ADJUST =  0.D0
      STEP   =  1.D3

      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 5 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      STEP = 300.D0

      CALL SSIZED ( 2, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(OUTOFROOM)', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized value of FIXREF.' )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              'XYZ',  ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'FIXREF is not centered on the target body.' )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              'IAU_MARS',  ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad computation method.' )

      STEP = 1.D0

      ANGTYP = 'EMISSION'

      METHOD = 'DSK'

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)',   OK )

      METHOD = 'ELLIPSOID'

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad illumination angle type' )

      STEP = 1.D0

      ANGTYP = 'XYZ'

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad relational operator' )

      STEP = 1.D0

      ANGTYP = 'INCIDENCE'

      RELATE = '>='

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction values' )

      STEP   = 1.D0

      RELATE = 'LOCMAX'


      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, 'S',    OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, 'XS',   OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, 'RLT',    OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, 'XRLT',   OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, 'Z',   OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Transmission aberration correction values.' )

      STEP   = 1.D0

      RELATE = 'LOCMAX'


      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, 'XLT',  OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, 'XCN',  OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


      CALL GFILUM ( METHOD, ANGTYP,  TARGET, ILLUM,
     .              FIXREF, 'XLT+S', OBSRVR, SPOINT,
     .              RELATE, REFVAL,  ADJUST, STEP,
     .              CNFINE, MAXWIN,  NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

      CALL GFILUM ( METHOD, ANGTYP,  TARGET, ILLUM,
     .              FIXREF, 'XCN+S', OBSRVR, SPOINT,
     .              RELATE, REFVAL,  ADJUST, STEP,
     .              CNFINE, MAXWIN,  NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad value of ADJUST.' )

      ADJUST = -1.D0

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized target, observer, or illumination '
     .//           'source.'                                        )

      ADJUST = 0.D0

      CALL GFILUM ( METHOD, ANGTYP, 'X',    ILLUM,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL GFILUM ( METHOD, ANGTYP, TARGET, 'X',
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, 'X',    SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Target and observer are identical.' )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, ILLUM,
     .              FIXREF, ABCORR, TARGET, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Target and illumination source are identical.' )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET, TARGET,
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for observer' )

      STEP = 1.D0

      CALL GFILUM ( METHOD, ANGTYP, TARGET,   ILLUM,
     .              FIXREF, ABCORR, 'GASPRA', SPOINT,
     .              RELATE, REFVAL, ADJUST,   STEP,
     .              CNFINE, MAXWIN, NWILUM,   WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for target' )

      STEP = 1.D0

      FIXREF = 'IAU_GASPRA'

      CALL GFILUM ( METHOD, ANGTYP, 'GASPRA', ILLUM,
     .              FIXREF, ABCORR, OBSRVR,   SPOINT,
     .              RELATE, REFVAL, ADJUST,   STEP,
     .              CNFINE, MAXWIN, NWILUM,   WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for illumination source' )

      STEP = 1.D0

      FIXREF = 'IAU_EARTH'

      CALL GFILUM ( METHOD, ANGTYP, TARGET, 'GASPRA',
     .              FIXREF, ABCORR, OBSRVR, SPOINT,
     .              RELATE, REFVAL, ADJUST, STEP,
     .              CNFINE, MAXWIN, NWILUM, WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for target.'  )

      STEP = 1.D3

      CALL GFILUM ( METHOD,       ANGTYP, TARGET,  ILLUM,
     .              'ITRF93',     ABCORR, OBSRVR,  SPOINT,
     .              RELATE,       REFVAL, ADJUST,  STEP,
     .              CNFINE,       MAXWIN, NWILUM,  WORK,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************



C*********************************************************************
C*
C*    Simple cases using Nat's solar system
C*
C*********************************************************************

C
C     The cases below all involve finding times when local extrema are
C     attained. In each case we know the correct answers. All of these
C     tests are done with aberration corrections set to 'NONE'.
C
C     Following these tests, a series of comparisons is performed using
C     results produced by alternate methods. The second set of tests is
C     comprehensive: those tests use all combinations of operators and
C     aberration corrections.
C

C
C---- Case -------------------------------------------------------------
C

C
C     All expected event times are based on Nat's solar system.
C
      CALL TCASE ( 'Local minimum of emission angle at north '
     .//           'pole of ALPHA; observer is SUN; abcorr = NONE'  )


      ANGTYP = 'EMISSION'
      RELATE = 'locmin'
      REFVAL = 0.D0
      ADJUST = 0.D0

      ABCORR = 'NONE'

      TARGET = ' ALPHA'
      FIXREF = ' ALPHAFIXED'
      OBSRVR = ' SUN'
      ILLUM  = ' SUN'

      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL VPACK  ( 0.D0, 0.D0, RADII(3), SPOINT )

      TIME0 = '2000 JAN 1 12:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )
C
C     Check sun position for debugging:
C     CALL SPKPOS ( OBSRVR, ET0, FIXREF, 'NONE', TARGET, POS, LT )
C     WRITE (*,*) 'Position of sun wrt alpha at J2000:'
C     WRITE (*,*) POS
C

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     The period of ALPHA's orbit is 7 days. This can be derived
C     from the relative angular velocities of ALPHA and BETA
C     and the period of the occultation starts.
C
C     A minimum of the emission angle should occur whenever ALPHA's
C     north pole points toward the sun. This should happen at the
C     J2000 epoch and every 7 days before or after.
C
C     Create a confinement window with 4 intervals.
C
      DO I = 1, 4

         LEFT  = ET0  + (I-1)*SPD()*7   -      3600.D0
         RIGHT = LEFT                   +  2 * 3600.D0

         CALL WNINSD ( LEFT, RIGHT, CNFINE )

      END DO

C
C     We can use a large step. The extrema should be 12 hours
C     apart.
C
      STEP = 10 * 3600.D0

      METHOD = 'Ellipsoid'

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )


      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )


      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check event start time.
C
         TITLE = 'Event # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = ET0 +  ( DBLE(I-1) * SPD() * 7 )

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check event end time; this should equal the begin time
C        since we're searching for an extremum.
C
         TITLE = 'Event # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

C
C        Dump event times for debugging:
C
C         CALL TIMOUT ( LEFT,
C     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
C     .                 TIMSTR )
C         WRITE (*,*) I, ' ',  TIMSTR
C
C         CALL TIMOUT ( RIGHT,
C     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
C     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR


      END DO

C
C---- Case -------------------------------------------------------------
C

C
C     All expected event times are based on Nat's solar system.
C
      CALL TCASE ( 'Local minimum of solar incidence angle at north '
     .//           'pole of ALPHA; observer is SUN; abcorr = NONE'   )

C
C     This is a repeat of the previous case, with only the angle
C     type changed.
C
      ANGTYP = ' Incidence'

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )


      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )


      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check event start time.
C
         TITLE = 'Event # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = ET0 +  ( DBLE(I-1) * SPD() * 7 )

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )
C
C        Check event end time; this should equal the begin time
C        since we're searching for an extremum.
C
         TITLE = 'Event # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

      END DO



C
C---- Case -------------------------------------------------------------
C

C
C     All expected event times are based on Nat's solar system.
C
      CALL TCASE ( 'Local maximum of emission angle at north '
     .//           'pole of ALPHA; observer is SUN; abcorr = NONE'  )


      RELATE = 'LOCMAX'

C
C     ALPHA orbits with constant angular velocity on a circular path,
C     so local maxima occur 1/2 period apart from local minima.
C

C
C     Create a confinement window with 4 intervals.
C
      DO I = 1, 4

         LEFT  = ET0 + (3.5D0 * SPD() ) + (I-1)*SPD()*7   -      3600.D0
         RIGHT = LEFT                                     +  2 * 3600.D0

         CALL WNINSD ( LEFT, RIGHT, CNFINE )

      END DO

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )


      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )


      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check event start time.
C
         TITLE = 'Event # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = ET0 + (3.5D0 * SPD() ) + (I-1)*SPD()*7

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check event end time; this should equal the begin time
C        since we're searching for an extremum.
C
         TITLE = 'Event # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

C
C        Dump event times for debugging:
C
C         CALL TIMOUT ( LEFT,
C     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
C     .                 TIMSTR )
C         WRITE (*,*) I, ' ',  TIMSTR
C
C         CALL TIMOUT ( RIGHT,
C     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
C     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR


      END DO



C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'Local maximum of solar incidence angle at north '
     .//           'pole of ALPHA; observer is SUN; abcorr = NONE'   )


C
C     This is a repeat of the previous case, with only the angle
C     type changed.
C
      ANGTYP = '  Incidence'

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )


      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check event start time.
C
         TITLE = 'Event # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = ET0 + (3.5D0 * SPD() ) + (I-1)*SPD()*7

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )
C
C        Check event end time; this should equal the begin time
C        since we're searching for an extremum.
C
         TITLE = 'Event # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

      END DO



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Local minimum of phase angle at -X axis of '
     .//           'ALPHA; observer is BETA; abcorr = NONE' )


C
C     This is a phase angle search.
C
      ANGTYP = 'PHASE'

C
C     This time we want to use Beta as the observer. Note that
C     the surface point on Alpha's north pole won't do for this
C     computation.
C
C     We're going to work around this problem by changing the
C     orientation of Alpha. We'll use the ALPHA_VIEW_XY frame as
C     Alpha's body-fixed frame. This is a two-vector dynamic frame
C     in which the +X axis points from the sun to Alpha at all times.
C     We'll create a sun-facing surface point on Alpha at the tip
C     of Alpha's -X axis.
C
      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL VPACK  ( -RADII(1), 0.D0, 0.D0, SPOINT )

      FIXREF = 'ALPHA_VIEW_XY'

C
C     Set up the participants.
C
      OBSRVR = 'BETA'
      TARGET = 'ALPHA'
      ILLUM  = 'SUN'

      RELATE = 'LOCMIN'
      STEP   = 3600.D0

C
C     Set the confinement window to cover a single 4-day time interval.
C
      ET0 = 0.D0

      ET1 = ET0 +  ( 4 * SPD() )

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 8, 0,
     .              OK                                              )


      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check event start time.
C
         TITLE = 'Event # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        The phase angle is minimized at the central time of
C        each occultation and midway between these times.
C
         XTIME = 300.D0 + (I-1)*SPD()/2

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )
C
C        Check event end time; this should equal the begin time
C        since we're searching for an extremum.
C
         TITLE = 'Event # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

      END DO




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Repeat the previous test with the illumination '
     .//           'source set to BETA and the observer set to SUN.' )

C
C     We need at least one test in which the sun is not the illumination
C     source. The event times should be unchanged.
C

C
C     This is a phase angle search.
C
      ANGTYP = 'PHASE'

C
C     This time we want to use Beta as the observer. Note that
C     the surface point on Alpha's north pole won't do for this
C     computation.
C
C     We're going to work around this problem by changing the
C     orientation of Alpha. We'll use the ALPHA_VIEW_XY frame as
C     Alpha's body-fixed frame. This is a two-vector dynamic frame
C     in which the +X axis points from the sun to Alpha at all times.
C     We'll create a sun-facing surface point on Alpha at the tip
C     of Alpha's -X axis.
C
      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL VPACK  ( -RADII(1), 0.D0, 0.D0, SPOINT )

      FIXREF = 'ALPHA_VIEW_XY'

C
C     Set up the participants.
C
      OBSRVR = 'SUN'
      TARGET = 'ALPHA'
      ILLUM  = 'BETA'

      RELATE = 'LOCMIN'
      STEP   = 3600.D0

C
C     Set the confinement window to cover a single 4-day time interval.
C
      ET0 = 0.D0

      ET1 = ET0 +  ( 4 * SPD() )

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 8, 0,
     .              OK                                              )


      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check event start time.
C
         TITLE = 'Event # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

C
C        The phase angle is minimized at the central time of
C        each occultation and midway between these times.
C
         XTIME = 300.D0 + (I-1)*SPD()/2

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )
C
C        Check event end time; this should equal the begin time
C        since we're searching for an extremum.
C
         TITLE = 'Event # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

      END DO


C*********************************************************************
C*
C*    Comprehensive cases using comparisons against alternate
C*    computations
C*
C*********************************************************************


C*********************************************************************
C*
C*    PHASE angle tests
C*
C*********************************************************************

C
C     We'use use the earth, moon, and sun as the three participating
C     bodies. The surface point will be the OCTL telescope; we'll
C     create an SPK file for this point. We also need an FK for a
C     topocentric frame centered at the point.
C
C     Though it's not strictly necessary, we'll use real data for
C     these kernels, with one exception: we'll use the terrestrial
C     reference frame IAU_EARTH rather than ITRF93.
C
C     The original reference frame specifications follow:
C
C
C        Topocentric frame OCTL_TOPO
C
C           The Z axis of this frame points toward the zenith.
C           The X axis of this frame points North.
C
C           Topocentric frame OCTL_TOPO is centered at the site OCTL
C           which has Cartesian coordinates
C
C              X (km):                 -0.2448937761729E+04
C              Y (km):                 -0.4667935793438E+04
C              Z (km):                  0.3582748499430E+04
C
C           and planetodetic coordinates
C
C              Longitude (deg):      -117.6828380000000
C              Latitude  (deg):        34.3817491000000
C              Altitude   (km):         0.2259489999999E+01
C
C           These planetodetic coordinates are expressed relative to
C           a reference spheroid having the dimensions
C
C              Equatorial radius (km):  6.3781400000000E+03
C              Polar radius      (km):  6.3567523100000E+03
C
C           All of the above coordinates are relative to the frame
C           ITRF93.
C

C
C---- Case -------------------------------------------------------------
C

C
C     This isn't a test, but we'll call it that so we'll have
C     a meaningful label in any error messages that arise.
C
      CALL TCASE ( 'Create OCTL kernels.' )

C
C     As mentioned, we go with a frame that's more convenient than
C     ITRF93:
C
      FIXREF = 'IAU_EARTH'

C
C     Prepare a frame kernel in a string array.
C
      TXT( 1) = 'FRAME_OCTL_TOPO            =  1398962'
      TXT( 2) = 'FRAME_1398962_NAME         =  ''OCTL_TOPO'' '
      TXT( 3) = 'FRAME_1398962_CLASS        =  4'
      TXT( 4) = 'FRAME_1398962_CLASS_ID     =  1398962'
      TXT( 5) = 'FRAME_1398962_CENTER       =  398962'

      TXT( 6) = 'OBJECT_398962_FRAME        =  ''OCTL_TOPO'' '

      TXT( 7) = 'TKFRAME_1398962_RELATIVE   =  ''IAU_EARTH'' '
      TXT( 8) = 'TKFRAME_1398962_SPEC       =  ''ANGLES'' '
      TXT( 9) = 'TKFRAME_1398962_UNITS      =  ''DEGREES'' '
      TXT(10) = 'TKFRAME_1398962_AXES       =  ( 3, 2, 3 )'
      TXT(11) = 'TKFRAME_1398962_ANGLES     =  ( -242.3171620000000,'
      TXT(12) = '                                 -55.6182509000000,'
      TXT(13) = '                                 180.0000000000000  )'
      TXT(14) = 'NAIF_BODY_NAME            +=  ''OCTL'' '
      TXT(15) = 'NAIF_BODY_CODE            +=  398962'

C
C     It will be convenient to have a version of this frame that
C     has the +Z axis pointed down instead of up.
C
      TXT(16) = 'FRAME_OCTL_FLIP            =  2398962'
      TXT(17) = 'FRAME_2398962_NAME         =  ''OCTL_FLIP'' '
      TXT(18) = 'FRAME_2398962_CLASS        =  4'
      TXT(19) = 'FRAME_2398962_CLASS_ID     =  2398962'
      TXT(20) = 'FRAME_2398962_CENTER       =  398962'

      TXT(21) = 'TKFRAME_2398962_RELATIVE   =  ''OCTL_TOPO'' '
      TXT(22) = 'TKFRAME_2398962_SPEC       =  ''ANGLES'' '
      TXT(23) = 'TKFRAME_2398962_UNITS      =  ''DEGREES'' '
      TXT(24) = 'TKFRAME_2398962_AXES       =  ( 3, 2, 3 )'
      TXT(25) = 'TKFRAME_2398962_ANGLES     =  ( 0, 180.0, 0 ) '

      CALL LMPOOL ( TXT, TXTSIZ )
      CALL CHCKXC ( .FALSE., ' ',   OK )



C
C     Now create an SPK file containing a type 8 segment for OCTL.
C
      CALL SPKOPN ( OCTSPK, OCTSPK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize both states to zero.
C
      CALL CLEARD ( 12, STATES )

C
C     The first position component:
C
      SPOINT(1) =   -0.2448937761729D4
      SPOINT(2) =   -0.4667935793438D4
      SPOINT(3) =    0.3582748499430D4

C
C     We're going to use a version of the OCTL position
C     that has zero altitude relative to the earth's
C     reference ellipsoid. This is done to enable
C     consistency checks to be done using GFPOSC.
C
C     For compatibility with the topocentric frame
C     specification above, we'll use the following
C     earth radii:
C
      RADII(1)  = 6.3781400000000D+03
      RADII(2)  = 6.3781400000000D+03
      RADII(3)  = 6.3567523100000D+03

      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )

      CALL NEARPT ( SPOINT, RADII(1), RADII(2), RADII(3), NP, ALT )

      CALL VEQU ( NP, SPOINT )

      CALL VEQU ( SPOINT, STATES(1,1) )

C
C     The second position matches the first: we don't model
C     plate motion.
C
      CALL VEQU ( SPOINT, STATES(1,2) )

C
C     Time bounds for the segment:
C

      FIRST = ( -50 ) * SPD() * 365.25D0
      STEP  = ( 100 ) * SPD() * 365.25D0

      LAST  = FIRST + STEP - 1.D-6

C
C     Get the OCTL ID code from the kernel we just
C     loaded.
C
      CALL BODN2C ( OCTL, OCTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

C
C     Write the segment.
C
      CALL SPKW08 ( HANDLE, OCTID,  399,     FIXREF,
     .              FIRST,  LAST,   'octl',  1,
     .              2,      STATES, FIRST,   STEP   )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Now load the OCTL SPK file.
C
      CALL FURNSH ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Phase angle tests: we'll compare results from GFILUM to those
C     obtained using GFPA. Note that the surface point must be
C     an ephemeris object in order to carry out these tests.
C
      ANGTYP = ' Phase'

      ILLUM  = 'Sun'
      TARGET = ' 399'
      OBSRVR = ' moon'

C
C     Note that FIXREF has already been set.
C



C
C     Set up the confinement window.
C
      TIME0 = '2011 JAN 1'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Search over approximately two months.
C
      ET1 = ET0 + ( 60 * SPD() )

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )

C
C     Use a 10 day step. We expect the extrema to
C     be about 14 days apart.
C
      STEP = 10 * SPD()

C
C     Loop over all operators and aberration corrections.
C

      DO OPIDX = 1, NREL
C
C        Convert the reference value to radians.
C
         RELATE = RELOPS ( OPIDX )
         REFVAL = VALS   ( OPIDX ) * RPD()
         ADJUST = ADJ    ( OPIDX )


         DO CORIDX = 1, NCORR

            ABCORR = CORRS( CORIDX )
C
C---- Case -------------------------------------------------------------
C
            TITLE = 'Phase angle search: RELATE = #; '
     .      //      'REFVAL (deg) = #; ADJUST = #; ABCORR = #; '
     .      //      'observer = #; target = #; illum source = #; '
     .      //      'FIXREF = #; SPOINT = ( # # # ).'

            CALL REPMC ( TITLE, '#', RELATE,          TITLE )
            CALL REPMD ( TITLE, '#', VALS(OPIDX), 14, TITLE )
            CALL REPMD ( TITLE, '#', ADJUST,      14, TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,          TITLE )
            CALL REPMC ( TITLE, '#', OBSRVR,          TITLE )
            CALL REPMC ( TITLE, '#', TARGET,          TITLE )
            CALL REPMC ( TITLE, '#', ILLUM,           TITLE )
            CALL REPMC ( TITLE, '#', FIXREF,          TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(1),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(2),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(3),   14, TITLE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL TCASE ( TITLE )

C
C           Initialize the result windows.
C
            CALL SSIZED ( MAXWIN, RESULT )
            CALL SSIZED ( MAXWIN, XRSULT )

C
C           Find the expected result window. Note that the
C           target is OCTL in this case.
C
            CALL GFPA ( OCTL,   ILLUM,  ABCORR, OBSRVR,
     .                  RELATE, REFVAL, ADJUST, STEP,
     .                  CNFINE, MAXWIN, NWPA,   WORK,
     .                  XRSULT                         )

            CALL CHCKXC ( .FALSE., ' ',   OK )


            IF ( OK ) THEN
C
C              Search for the condition of interest using GFILUM.
C
               CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .                       FIXREF, ABCORR, OBSRVR,  SPOINT,
     .                       RELATE, REFVAL, ADJUST,  STEP,
     .                       CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )

               CALL CHCKXC ( .FALSE., ' ',   OK )


               IF ( OK ) THEN
C
C                 Compare result window cardinalities first.
C
                  XN = WNCARD( XRSULT )
C
C                 Test family validity check: we're not supposed to
C                 have any cases where the results are empty.
C
                  CALL CHCKSI ( 'XN', XN, '>', 0, 0, OK )

                  N  = WNCARD( RESULT )

                  CALL CHCKSI ( 'N', N, '=', XN, 0, OK )


                  IF ( OK ) THEN
C
C                    Compare result window interval bounds.
C
                     CALL CHCKAD( 'RESULT', RESULT(1), '~',
     .                                      XRSULT(1), 2*N, LOOSE, OK )

                     IF ( OK ) THEN
C
C                       The result window found by GFILUM agrees
C                       with that found by GFPA. Check the actual
C                       angular values at the window endpoints
C                       for the cases where the operator is
C                       binary.
C
                        IF ( RELATE .EQ. '=' ) THEN
C
C                          Check the phase angle at each interval
C                          endpoint.
C
                           DO I = 1, N

                              CALL WNFETD ( RESULT, I, START, FINISH )
                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              TITLE = 'Angle at start of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', START,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              CALL CHCKSD ( TITLE,  PHASE,  '~',
     .                                      REFVAL, ANGTOL, OK  )


                              TITLE = 'Angle at end of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', FINISH,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              CALL CHCKSD ( TITLE,  PHASE,  '~',
     .                                      REFVAL, ANGTOL, OK  )
                           END DO


                        ELSE IF (      ( RELATE .EQ. '<' )
     .                            .OR. ( RELATE .EQ. '>' )  ) THEN
C
C                          Perform checks only at interval endpoints
C                          contained in the interior of the confinement
C                          window. At the confinement window boundaries,
C                          the inequality may be satisfied without the
C                          value being equal to the reference value.
C
                           DO I = 1, N

                              CALL WNFETD ( RESULT, I, START, FINISH )
                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              TITLE = 'Angle at start of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', START,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              IF ( START .GT. ET0 ) THEN

                                 CALL CHCKSD ( TITLE, PHASE,  '~',
     .                                         REFVAL,  ANGTOL, OK  )
                              END IF

                              TITLE = 'Angle at end of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', FINISH,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              IF ( FINISH .LT. ET1 ) THEN

                                 CALL CHCKSD ( TITLE, PHASE,  '~',
     .                                         REFVAL,  ANGTOL, OK  )
                              END IF

                           END DO
C
C                          End of phase angle value check loop.
C
                        END IF
C
C                       End of phase angle checks.
C
                     END IF
C
C                    End of IF block for successful RESULT check.
C
                  END IF
C
C                 End of interval endpoint checks.
C
               END IF
C
C              End of IF block for successful GFILUM call.
C
            END IF
C
C           End of IF block for successful GFPA call.
C
         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of operator loop.
C


C*********************************************************************
C*
C*    EMISSION angle tests
C*
C*********************************************************************


C
C     Emission angle tests: we'll compare results from GFILUM to those
C     obtained using GFPOSC. Note that the surface point must be
C     an ephemeris object having an associated topocentric frame
C     in order to carry out these tests.
C

C
C     The emission angle is the supplement of the colatitude,
C     measured in the surface point topocentric frame, of
C     the observer-surface point vector. Equivalently, the emission
C     angle is the colatitude of the observer-surface point vector
C     in the "flip" frame, which has its +Z axis pointing downward.
C
C     We can use GFPOSC to find times when this colatitude, measured
C     in the flip frame, meets conditions on the emission angle.
C
      OBSRVR = 'MOON'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      SRFREF = 'OCTL_FLIP'
      CORSYS = 'SPHERICAL'
      COORD  = 'COLATITUDE'

      ANGTYP = 'EMISSION'

C
C     For the emission angle test, we don't need to use
C     a month-long confinement window. A few days is enough.
C
      ET1 = ET0 + ( 3 * SPD() )

C
C     Use a 6 hour step. We expect the extrema to
C     be 12+delta hours apart, where delta may be
C     a few hours.
C
      STEP   = 6.D0 * 3600.D0


      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Loop over all operators and aberration corrections.
C
      DO OPIDX = 1, NREL
C
C        Convert the reference value to radians.
C
         RELATE = RELOPS ( OPIDX )
         REFVAL = VALS   ( OPIDX ) * RPD()
         ADJUST = ADJ    ( OPIDX )


         DO CORIDX = 1, NCORR

            ABCORR = CORRS( CORIDX )
C
C---- Case -------------------------------------------------------------
C
            TITLE = 'Emission angle search: RELATE = #; '
     .      //      'REFVAL (deg) = #; ADJUST = #; ABCORR = #; '
     .      //      'observer = #; target = #; illum source = #; '
     .      //      'FIXREF = #; SPOINT = ( # # # ); '
     .      //      'SRFREF = #.'

            CALL REPMC ( TITLE, '#', RELATE,          TITLE )
            CALL REPMD ( TITLE, '#', VALS(OPIDX), 14, TITLE )
            CALL REPMD ( TITLE, '#', ADJUST,      14, TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,          TITLE )
            CALL REPMC ( TITLE, '#', OBSRVR,          TITLE )
            CALL REPMC ( TITLE, '#', TARGET,          TITLE )
            CALL REPMC ( TITLE, '#', ILLUM,           TITLE )
            CALL REPMC ( TITLE, '#', FIXREF,          TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(1),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(2),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(3),   14, TITLE )
            CALL REPMC ( TITLE, '#', SRFREF,          TITLE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL TCASE ( TITLE )

C
C           Initialize the result windows.
C
            CALL SSIZED ( MAXWIN, RESULT )
            CALL SSIZED ( MAXWIN, XRSULT )

C
C           Find the expected result window. Note that the
C           target is OCTL in this case.
C
            CALL GFPOSC ( OCTL,   SRFREF,  ABCORR, OBSRVR,
     .                    CORSYS, COORD,   RELATE, REFVAL,
     .                    ADJUST, STEP,    CNFINE, MAXWIN,
     .                    NWMAX,  WORK,    XRSULT          )

            CALL CHCKXC ( .FALSE., ' ',   OK )


            IF ( OK ) THEN
C
C              Search for the condition of interest using GFILUM.
C
               CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .                       FIXREF, ABCORR, OBSRVR,  SPOINT,
     .                       RELATE, REFVAL, ADJUST,  STEP,
     .                       CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )

               CALL CHCKXC ( .FALSE., ' ',   OK )


               IF ( OK ) THEN
C
C                 Compare result window cardinalities first.
C
                  XN = WNCARD( XRSULT )
C
C                 Test family validity check: we're not supposed to
C                 have any cases where the results are empty.
C
                  CALL CHCKSI ( 'XN', XN, '>', 0, 0, OK )


C                  WRITE (*,*) RELATE, '  ', 'XN = ', XN

                  N  = WNCARD( RESULT )

                  CALL CHCKSI ( 'N', N, '=', XN, 0, OK )


                  IF ( OK ) THEN
C
C                    Compare result window interval bounds.
C
                     CALL CHCKAD( 'RESULT', RESULT(1), '~',
     .                                      XRSULT(1), 2*N, LOOSE, OK )

                     IF ( OK ) THEN
C
C                       The result window found by GFILUM agrees
C                       with that found by GFPOSC. Check the actual
C                       angular values at the window endpoints
C                       for the cases where the operator is
C                       binary.
C
                        IF ( RELATE .EQ. '=' ) THEN
C
C                          Check the emission angle at each interval
C                          endpoint.
C
                           DO I = 1, N

                              CALL WNFETD ( RESULT, I, START, FINISH )
                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              TITLE = 'Angle at start of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', START,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              CALL CHCKSD ( TITLE,  EMISSN, '~',
     .                                      REFVAL, ANGTOL, OK  )


                              TITLE = 'Angle at end of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', FINISH,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKSD ( TITLE,  EMISSN,  '~',
     .                                      REFVAL, ANGTOL, OK  )
                           END DO


                        ELSE IF (      ( RELATE .EQ. '<' )
     .                            .OR. ( RELATE .EQ. '>' )  ) THEN
C
C                          Perform checks only at interval endpoints
C                          contained in the interior of the confinement
C                          window. At the confinement window boundaries,
C                          the inequality may be satisfied without the
C                          value being equal to the reference value.
C
                           DO I = 1, N

                              CALL WNFETD ( RESULT, I, START, FINISH )
                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              TITLE = 'Angle at start of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', START,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              IF ( START .GT. ET0 ) THEN

                                 CALL CHCKSD ( TITLE,  EMISSN,  '~',
     .                                         REFVAL, ANGTOL, OK  )
                              END IF

                              TITLE = 'Angle at end of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', FINISH,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              IF ( FINISH .LT. ET1 ) THEN

                                 CALL CHCKSD ( TITLE,  EMISSN,  '~',
     .                                         REFVAL, ANGTOL, OK  )
                              END IF


                           END DO
C
C                          End of emission angle value check loop.
C
                        END IF
C
C                       End of emission angle checks.
C
                     END IF
C
C                    End of IF block for successful RESULT check.
C
                  END IF
C
C                 End of interval endpoint checks.
C
               END IF
C
C              End of IF block for successful GFILUM call.
C
            END IF
C
C           End of IF block for successful GFPOSC call.
C
         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of operator loop.
C




C*********************************************************************
C*
C*    INCIDENCE angle tests
C*
C*********************************************************************


C
C     Incidence angle tests: we'll compare results from GFILUM to
C     those obtained using GFPOSC, where in the latter case, the
C     surface point is treated as the observer. Results obtained using
C     GFILUM must have one-way observer-surface point light time
C     subtracted in order to be comparable to those from GFPOSC.
C
C     Note that the surface point must be an ephemeris object having an
C     associate topocentric frame in order to carry out these tests.
C
C     In the geometric case, the solar incidence angle is the
C     colatitude, measured in the surface point topocentric frame, of
C     the surface point-sun vector. When aberration corrections are
C     used, the surface point-sun vector must be computed at an epoch
C     corrected for observer-surface point light time.
C
C     We can use GFPOSC to find times when this colatitude, measured in
C     the OCTL topocentric frame, meets conditions on the solar
C     incidence angle.
C
      OBSRVR = 'MOON'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'

      SRFREF = 'OCTL_TOPO'
      CORSYS = 'SPHERICAL'
      COORD  = 'COLATITUDE'

      ANGTYP = 'INCIDENCE'

C
C     For the solar incidence angle test, we don't need to use
C     a month-long confinement window. A few days is enough.
C

C
C     Set up the confinement window.
C
      TIME0 = '2011 JAN 1'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET1 = ET0 + ( 3 * SPD() )

C
C     Use a 6 hour step. We expect the extrema to
C     be 12+delta hours apart, where delta may be
C     a few hours.
C
      STEP   = 6.D0 * 3600.D0


      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Loop over all operators and aberration corrections.
C
      DO OPIDX = 1, NREL
C
C        Convert the reference value to radians.
C
         RELATE = RELOPS ( OPIDX )
         REFVAL = VALS   ( OPIDX ) * RPD()
         ADJUST = ADJ    ( OPIDX )


         DO CORIDX = 1, NCORR

            ABCORR = CORRS( CORIDX )
C
C---- Case -------------------------------------------------------------
C
            TITLE = 'Solar incidence angle search: RELATE = #; '
     .      //      'REFVAL (deg) = #; ADJUST = #; ABCORR = #; '
     .      //      'observer = #; target = #; illum source = #; '
     .      //      'FIXREF = #; SPOINT = ( # # # ); '
     .      //      'SRFREF = #.'

            CALL REPMC ( TITLE, '#', RELATE,          TITLE )
            CALL REPMD ( TITLE, '#', VALS(OPIDX), 14, TITLE )
            CALL REPMD ( TITLE, '#', ADJUST,      14, TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,          TITLE )
            CALL REPMC ( TITLE, '#', OBSRVR,          TITLE )
            CALL REPMC ( TITLE, '#', TARGET,          TITLE )
            CALL REPMC ( TITLE, '#', ILLUM,           TITLE )
            CALL REPMC ( TITLE, '#', FIXREF,          TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(1),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(2),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(3),   14, TITLE )
            CALL REPMC ( TITLE, '#', SRFREF,          TITLE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL TCASE ( TITLE )

C
C           Initialize the result windows.
C
            CALL SSIZED ( MAXWIN, RESULT )
            CALL SSIZED ( MAXWIN, XRSULT )
            CALL SSIZED ( MAXWIN, MODCFN )
            CALL SSIZED ( MAXWIN, MODRES )

            IF ( EQSTR( ABCORR, 'NONE' )  ) THEN

               CALL COPYD ( CNFINE, MODCFN )

            ELSE
C
C              Create a modified confinement window: this is the
C              original confinement window, adjusted for one-way light
C              time between the observer and the surface point.
C
               DO I = 1, CARDD( CNFINE )

                  ET = CNFINE(I)

                  CALL SPKPOS( OCTL,   ET,     'J2000',
     .                         ABCORR, OBSRVR, POS, LT )
                  CALL CHCKXC ( .FALSE., ' ',   OK )

                  MODCFN(I) = ET - LT

               END DO

               CALL SCARDD ( CARDD(CNFINE), MODCFN )

            END IF

C
C           Find the expected result window. Note that the
C           target is the sun and the observer is OCTL in this case.
C
            CALL GFPOSC ( 'SUN',  SRFREF,  ABCORR, OCTL,
     .                    CORSYS, COORD,   RELATE, REFVAL,
     .                    ADJUST, STEP,    MODCFN, MAXWIN,
     .                    NWMAX,  WORK,    XRSULT          )

            CALL CHCKXC ( .FALSE., ' ',   OK )


            IF ( OK ) THEN
C
C              Search for the condition of interest using GFILUM.
C
               CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .                       FIXREF, ABCORR, OBSRVR,  SPOINT,
     .                       RELATE, REFVAL, ADJUST,  STEP,
     .                       CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )

               CALL CHCKXC ( .FALSE., ' ',   OK )


               IF ( OK ) THEN
C
C                 Compare result window cardinalities first.
C
                  XN = WNCARD( XRSULT )
C
C                 Test family validity check: we're not supposed to
C                 have any cases where the results are empty.
C
                  CALL CHCKSI ( 'XN', XN, '>', 0, 0, OK )

                  N  = WNCARD( RESULT )

                  CALL CHCKSI ( 'N', N, '=', XN, 0, OK )


                  IF ( OK ) THEN
C
C                    Compare result window interval bounds.
C
C                    Here's where things get a bit tricky. We can't
C                    just compare RESULT against XRSULT, because
C                    these windows contain event times for different
C                    locations. They're directly comparable only
C                    when aberration corrections are turned off.
C
                     IF ( EQSTR( ABCORR, 'NONE' )  ) THEN
C
C                       No problem: this is the geometric case.
C
                        CALL CHCKAD ( 'RESULT',    RESULT(1),
     .                                '~',         XRSULT(1),
     .                                2*N, LOOSE, OK         )
                     ELSE
C
C                       Modify the window of event times by subtracting
C                       the applicable one-way light time from each
C                       element.
C
                        CALL SSIZED ( MAXWIN, MODRES )

                        DO I = 1, CARDD( RESULT )

                           ET = RESULT( I )
C
C                          Find the light time from the surface point
C                          to the observer, where the reception time
C                          is ET.
C
                           CALL SPKPOS ( OCTL,   ET, 'J2000', ABCORR,
     .                                   OBSRVR, POS, LT              )
                           CALL CHCKXC ( .FALSE., ' ',   OK )

                           MODRES(I) = RESULT(I) - LT

                        END DO

                        IF ( OK ) THEN
C
C                          Compare the transformed result window to
C                          the expected window.
C
                           CALL CHCKAD ( 'RESULT (modified)', MODRES(1),
     .                                   '~',                 XRSULT(1),
     .                                   2*N, LOOSE, OK               )
                        END IF


                     END IF

                     IF ( OK ) THEN
C
C                       The result window found by GFILUM agrees
C                       with that found by GFPOSC. Check the actual
C                       angular values at the window endpoints
C                       for the cases where the operator is
C                       binary.
C
                        IF ( RELATE .EQ. '=' ) THEN
C
C                          Check the solar incidence angle at each
C                          interval endpoint.
C
                           DO I = 1, N

                              CALL WNFETD ( RESULT, I, START, FINISH )
                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              TITLE = 'Angle at start of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', START,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              CALL CHCKSD ( TITLE,  SOLAR, '~',
     .                                      REFVAL, ANGTOL, OK  )


                              TITLE = 'Angle at end of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', FINISH,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              CALL CHCKSD ( TITLE,  SOLAR,  '~',
     .                                      REFVAL, ANGTOL, OK  )
                           END DO


                        ELSE IF (      ( RELATE .EQ. '<' )
     .                            .OR. ( RELATE .EQ. '>' )  ) THEN
C
C                          Perform checks only at interval endpoints
C                          contained in the interior of the confinement
C                          window. At the confinement window boundaries,
C                          the inequality may be satisfied without the
C                          value being equal to the reference value.
C
                           DO I = 1, N

                              CALL WNFETD ( RESULT, I, START, FINISH )
                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              TITLE = 'Angle at start of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', START,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              IF ( START .GT. ET0 ) THEN

                                 CALL CHCKSD ( TITLE,  SOLAR,  '~',
     .                                         REFVAL, ANGTOL, OK  )
                              END IF

                              TITLE = 'Angle at end of interval #'
                              CALL REPMI ( TITLE, '#', I, TITLE )

                              CALL ILUMIN ( METHOD, 'EARTH', FINISH,
     .                                      FIXREF, ABCORR,  OBSRVR,
     .                                      SPOINT, TRGEPC,  SRFVEC,
     .                                      PHASE,  SOLAR,   EMISSN )

                              CALL CHCKXC ( .FALSE., ' ',   OK )

                              IF ( FINISH .LT. ET1 ) THEN

                                 CALL CHCKSD ( TITLE,  SOLAR,  '~',
     .                                         REFVAL, ANGTOL, OK  )
                              END IF

                           END DO
C
C                          End of solar incidence angle value check
C                          loop.
C
                        END IF
C
C                       End of solar incidence angle checks.
C
                     END IF
C
C                    End of IF block for successful RESULT check.
C
                  END IF
C
C                 End of interval endpoint checks.
C
               END IF
C
C              End of IF block for successful GFILUM call.
C
            END IF
C
C           End of IF block for successful GFPOSC call.
C
         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of operator loop.
C





C
C     Case
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C

      ANGTYP = 'EMISSION'
      RELATE = 'locmin'
      REFVAL = 0.D0
      ADJUST = 0.D0
      ABCORR = 'NONE'
      TARGET = ' ALPHA'
      FIXREF = ' ALPHAFIXED'
      OBSRVR = ' SUN'
      ILLUM  = ' SUN'
      METHOD = 'Ellipsoid'
      STEP = 3600.D0

  
      LEFT  = 0.D0
      RIGHT = 10.D0 * SPD()

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL WNINSD ( LEFT,   RIGHT, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFILUM ( METHOD, ANGTYP, TARGET,  ILLUM,
     .              FIXREF, ABCORR, OBSRVR,  SPOINT,
     .              RELATE, REFVAL, ADJUST,  STEP,
     .              CNFINE, MAXWIN, NWILUM,  WORK,  RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     The values in the time window should not match
C     as the search used different tolerances. Check
C     the first value in the first interval.
C
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )
  

C
C     Reset the convergence tolerance.
C
      CALL GFSTOL ( CNVTOL )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

C
C     Clean up SPK files.
C
      CALL SPKUEF ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL UNLOAD ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END
