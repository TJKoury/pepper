C$Procedure      F_ZZGFCSLV ( Test GF coordinate solver )
  
      SUBROUTINE F_ZZGFCSLV ( OK )

C$ Abstract
C
C     Test the GF private coordinate search routine ZZGFCSLV.
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
C     This routine tests the private SPICELIB GF
C     coordinate search routine ZZGFCSLV.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TSPICE Version 2.2.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 2.1.0, 15-APR-2014 (EDW)
C
C        Added explicit declaration of GFBAIL type.
C
C-    TSPICE Version 2.0.0, 13-MAY-2009 (NJB)
C
C        Updated to use strings representing integers
C        as body "names" for at least one test case
C        for each of the vector definitions POSDEF,
C        SOBDEF, and SINDEF.
C
C-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      INTEGER               WNCARD

      LOGICAL               GFBAIL

C
C     EXTERNAL declarations
C
      EXTERNAL              GFSTEP
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFBAIL
      


C
C     Local parameters
C
C     
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgfcslv.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfcslv.bsp' )
  

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-8 )
 
      DOUBLE PRECISION      MEDABS
      PARAMETER           ( MEDABS = 1.D-6 )

      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 1.D-5 )

      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 120.D0 )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

 

C
C     Maximum length of a coordinate system name.
C
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )
      
C
C     Maximum length of a coordinate name.
C
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )
 
      INTEGER               VDFLEN
      PARAMETER           ( VDFLEN = 32 )
 
      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 80 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2*1000 )
      
      INTEGER               MW
      PARAMETER           ( MW     = MAXWIN )

      INTEGER               NW
      PARAMETER           ( NW     = NWMAX  )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               OPLEN
      PARAMETER           ( OPLEN  = 40 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(CRDLEN)    CRDNAM
      CHARACTER*(SYSLEN)    CRDSYS
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(OPLEN)     RELATE
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
C      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(VDFLEN)    VECDEF      
      
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MW )
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ET2
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      LATS   ( 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAXADJ
      DOUBLE PRECISION      MAXEPC
      DOUBLE PRECISION      MINADJ
      DOUBLE PRECISION      MINEPC
      DOUBLE PRECISION      QTOL
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RA0
      DOUBLE PRECISION      RA1
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MW )
      DOUBLE PRECISION      RESLT2 ( LBCELL : MW )
      DOUBLE PRECISION      RESLT3 ( LBCELL : MW )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      WORK   ( MW, NW )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      XTIME

      INTEGER               HAN2
      INTEGER               HANDLE      
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               OBSID
      INTEGER               TRGID
      INTEGER               XN

      LOGICAL               BAIL
      LOGICAL               FOUND
      LOGICAL               RPT

C
C     Save everything.
C
      SAVE
 
C
C     Initial values 
C
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFCSLV' )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load a PCK.
C     
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

C
C     Load an SPK file as well.
C     
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create and load Nat's solar system SPK and PCK/FK
C     kernels.
C     
      CALL NATPCK ( NPCK, .TRUE., .FALSE. )
      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      
C
C     Actual DE-based ephemerides yield better comparisons,
C     since these ephemerides have less noise than do those
C     produced by TSTSPK.
C     
C      CALL FURNSH ( 'de421.bsp' )
C      CALL FURNSH ( 'jup230.bsp' )
C 


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C     The following error cases involve invalid initialization
C     values or missing data discovered at search time.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad observer or target name.' )

      ABCORR = 'CN+S'
      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      DREF   = REF
      CRDSYS = 'LATITUDINAL'
      CRDNAM = 'LATITUDE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      TIMSTR = '2008 MAY 1' 

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      TARGET = 'X'

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
           
      ET0 = ET - SPD()
      ET1 = ET + SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RPT  = .FALSE.
      BAIL = .FALSE.

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      TARGET = 'EARTH'
      OBSRVR = 'Y'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad window count' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     0,        WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     14,        WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     -1,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad window size' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, 0,      NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWSTOOSMALL)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, 1,      NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWSTOOSMALL)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, -1,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWSTOOSMALL)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Negative adjustment value' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ADJUST = -1.D0

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 

C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-positive tolerance value' )

C
C     Try both zero and negative tolerance values.
C
      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ADJUST = 0.D0
      TOL    = 0.D0

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 

      TOL    = -1.D0

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 

C
C     Reset TOL to something valid.
C
      TOL = 1.D-3

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad vector definition' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'None'
      METHOD = 'Near point'

      VECDEF = 'ACCELERATION'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )
      
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad computation method' )

C
C     This error is detected post-initialization.
C
      VECDEF = SINDEF
      ABCORR = 'None'
      METHOD = 'Near point'


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )
      
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction' )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ABCORR = 'None.'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate system' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'GAUSSIAN'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate name' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'LATITUDE'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad reference frame REF' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'Z'

      REF    = 'EME2000'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SINDEF: Reference frame REF not centered on target '
     .//           'when VECDEF requires it.'                   )

      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'
      TARGET = 'Mars'
      REF    = 'IAU_EARTH'
      
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )
     
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SINDEF: Non-existent reference frame DREF' )


      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'Z'
      
      REF    = 'IAU_EARTH'
      DREF   = 'EME2000'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad vector DVEC' )


      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'RECTANGULAR'
      CRDNAM = 'Z'
      
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'

      CALL CLEARD ( 3, DVEC )

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad target radii count' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      VECDEF = SOBDEF
      CRDSYS = 'LATITUDINAL'
      CRDNAM = 'LATITUDE'

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      ABCORR = 'None'
      DREF   = 'J2000'

      CALL GDPOOL ( 'BODY399_RADII', 1, 1, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 2, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     Restore all three radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target orientation data available' )

C
C     This error is detected post-initialization.
C
      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      REF    = 'ITRF93'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target ephemeris data available' )
C
C     This error is detected post-initialization.
C

      TARGET = 'GASPRA'
      TRGID  = 9511010
      REF = 'IAU_GASPRA'
 
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No observer ephemeris data available' )
C
C     This error is detected post-initialization.
C

      TARGET = 'MOON'
      TRGID  = 301
      OBSRVR = 'GASPRA'
      OBSID  = 9511010
      REF = 'IAU_MOON'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No DREF ephemeris data available' )
C
C     This error is detected post-initialization.
C

      DREF   = 'IAU_GASPRA'
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )
 
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No DREF orientation data available' )
C
C     This error is detected post-initialization.
C

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      TRGID  = 399
      OBSRVR = 'MOON'
      OBSID  = 301
      REF = 'IAU_EARTH'
      DREF   = 'ITRF93'
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )





C*********************************************************************
C*
C*    Normal cases --- Nat's solar system
C*
C*********************************************************************



C
C --- Case: ------------------------------------------------------
C

C
C     We're going to find times when the Alpha-Gamma vector crosses the
C     J2000 X-Y plane. This should occur every 12 hours, starting
C     at the J2000 epoch.
C
      CALL TCASE ( 'Find times when Gamma crosses '
     .//           'the J2000 x-y plane.'          )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 3:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 3 3:00 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'NONE'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = 'GAMMA'
      OBSRVR = 'ALPHA'
      REF    = 'J2000'
      DREF   = ' '
      CRDSYS = 'rectangular'
      CRDNAM = 'z'

      CALL CLEARD  ( 3, DVEC )     

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size.
C
      CALL GFSSTP ( 10000.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found 4 roots.
C
      N = WNCARD ( RESULT )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )

C
C     Make sure that the start and finish times are
C     those we expect.
C
      DO I = 1, N

         XTIME = 12.D0 * 3600.D0 * (I-1)

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START,  '~', XTIME, CNVTOL, OK )
         CALL CHCKSD ( QNAME, FINISH, '~', XTIME, CNVTOL, OK )

      END DO

C
C     Check the Z coordinate at the crossing time. 
C
C     In order to decide how accurate the coordinate should be,
C     we'll use the speed of Gamma together with the time 
C     tolerance.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKEZR ( TARGET, START, REF, ABCORR, OBSRVR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .EQ. 1 ) THEN
            QTOL = VNORM( STATE(4) ) * CNVTOL
         END IF

         QNAME = 'Z # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, STATE(3), '~', 0.D0, QTOL, OK )
 
      END DO







C*********************************************************************
C*
C*    Normal cases --- "real" solar system
C*
C*********************************************************************



C
C --- Case: ------------------------------------------------------
C

C
C     We're going to find times when the Earth-Sun vector crosses the
C     Earth equator (the x-y plane of the IAU_EARTH frame). We'll then
C     check the planetocentric latitude of the vector at the result
C     times. We'll also perform the search looking for 0 position
C     Z-component and compare the results of that search.
C

      CALL TCASE ( 'Find times when the Sun crosses '
     .//           'the IAU_EARTH x-y plane.'         )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2001 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = '10'
      OBSRVR = '399'
      REF    = 'IAU_EARTH'
      DREF   = ' '
      CRDSYS = 'latitudinal'
      CRDNAM = 'latitude'

      CALL CLEARD  ( 3, DVEC )     

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size.
C
      CALL GFSSTP ( 100*SPD() )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at least one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Check the planetocentric latitude at the crossing time.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Latitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LAT, '~', 0.D0, MEDABS, OK )
 
      END DO


C
C --- Case: ------------------------------------------------------
C

C
C     We're going to find times when the Earth-Sun vector attains
C     local minima and maxima.
C
      CALL TCASE ( 'Find local min/max of Solar latitude as '
     .//           'seen from Earth.'                         )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2005 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = 'SUN'
      OBSRVR = 'EARTH'
      REF    = 'IAU_EARTH'
      DREF   = ' '
      CRDSYS = 'latitudinal'
      CRDNAM = 'latitude'

      CALL CLEARD  ( 3, DVEC )     

      RELATE = 'LOCMAX'
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size.
C
      CALL GFSSTP ( 100*SPD() )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found five roots.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 5, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Check the planetocentric latitude at the crossing time. We
C     should find the latitude is greater than that at +/-
C     TDELTA seconds from each interval start time.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            ET = START + (J-2)*TDELTA

            CALL SPKPOS ( TARGET, ET,     REF, 
     .                    ABCORR, OBSRVR, POS, LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECLAT ( POS, R, LON, LATS(J) )

         END DO

C         WRITE (*, '(I10,3E24.16)'  ) I, LATS

         QNAME = 'Max: Left side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(1), '<', LATS(2), MEDABS, OK )


         QNAME = 'Max: Right side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(3), '<', LATS(2), MEDABS, OK )
 
      END DO


C
C     Now test using local minima.
C
      RELATE = 'locmin'

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found five roots.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 5, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Check the planetocentric latitude at the crossing time. We
C     should find the latitude is greater than that at +/-
C     TDELTA seconds from each interval start time.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            ET = START + (J-2)*TDELTA

            CALL SPKPOS ( TARGET, ET,     REF, 
     .                    ABCORR, OBSRVR, POS, LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECLAT ( POS, R, LON, LATS(J) )

         END DO

C         WRITE (*, '(I10,3E24.16)'  ) I, LATS

         QNAME = 'Min: Left side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(1), '>', LATS(2), MEDABS, OK )


         QNAME = 'Min: Right side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(3), '>', LATS(2), MEDABS, OK )
 
      END DO






C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Find times when the Sun crosses '
     .//           'the J2000 prime meridian.'        )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL SSIZED ( MAXWIN, RESLT3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2001 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'NONE'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = 'SUN'
      OBSRVR = 'EARTH'
      REF    = 'J2000'
      DREF   = ' '
      CRDSYS = 'Latitudinal'
      CRDNAM = 'LONGITUDE'

      CALL CLEARD  ( 3, DVEC )     

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size.
C
      CALL GFSSTP ( 100*SPD() )
      CALL GFSSTP ( 10*SPD() )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at least one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Check the longitude at the crossing time.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Longitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LON, '~', 0.D0, MEDABS, OK )
 
      END DO

C
C     Now find times when the Sun crosses the J2000 x-z plane.
C     Delete times when the X coordinate is negative. The
C     result should match that from the longitude search.
C
      CRDSYS = 'rectangular'
      CRDNAM = 'Y'
      REFVAL = 0.D0

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Empty the window RESLT3.
C
      CALL SCARDD ( 0, RESLT3 )

C
C     Copy the times corresponding to positive X-values to RESLT3.
C
      DO I = 1, WNCARD(RESLT2)

         CALL WNFETD ( RESLT2, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )

         IF ( POS(1) .GT. 0.D0 ) THEN

            CALL WNINSD ( START, FINISH, RESLT3 )

         END IF

      END DO

C
C     Make sure we found at as many roots as
C     we did on the previous search.
C
      XN = WNCARD ( RESULT )
      N  = WNCARD ( RESLT3 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

C
C     Check the solution times against those from the
C     Z-component search.
C
      CALL CHCKAD ( 'RESLT3', RESLT3, '~', RESULT, N, TIMTOL, OK )




C
C --- Case: ------------------------------------------------------
C

C
C     We're going to find times when the sub-solar point
C     crosses the Earth equator (the x-y plane of the 
C     IAU_EARTH frame). We'll then check the Z component
C     of the vector at the result times. We'll also
C     perform the search looking for 0 geodetic latitude
C     and compare the results of that search.
C

      CALL TCASE ( 'Find times when the sub-solar point '
     .//           'on the Earth (near point definition) '
     .//           'crosses the IAU_EARTH x-y plane.'     )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2001 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = SOBDEF
      METHOD = 'Near point: Ellipsoid'
      TARGET = '399'
      OBSRVR = '10'
      REF    = 'IAU_EARTH'
      DREF   = ' '
      CRDSYS = 'Geodetic'
      CRDNAM = 'latitude'

      CALL CLEARD  ( 3, DVEC )     

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size.
C
      CALL GFSSTP ( 100*SPD() )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at least one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Check the latitude at the crossing time.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Latitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LAT, '~', 0.D0, MEDABS, OK )
 
      END DO

C
C     Repeat the search using Cartesian coordinates.
C
      CRDSYS = 'Rectangular'
      CRDNAM = 'z'

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at as many roots as
C     we did on the previous search.
C
      XN = N
      N  = WNCARD ( RESLT2 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

C
C     Check the solution times against those from the
C     Z-component search.
C
      CALL CHCKAD ( 'RESLT2', RESLT2, '~', RESULT, N, TIMTOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find times when the sub-solar point '
     .//           'on the Earth (near point definition) '
     .//           'crosses the IAU_EARTH prime meridian.'  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL SSIZED ( MAXWIN, RESLT3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We use a time period of a few days, not a year.
C
      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 5', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'lt+s'
      VECDEF = SOBDEF
      METHOD = 'near point: ellipsoid'
      TARGET = 'Earth'
      OBSRVR = 'Sun'
      REF    = 'Iau_Earth'
      DREF   = ' '
      CRDSYS = 'Latitudinal'
      CRDNAM = 'Longitude'

      CALL CLEARD  ( 3, DVEC )     

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size. We need a size smaller than
C     one half day.
C
      CALL GFSSTP ( 8 * 3600.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at least one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Check the longitude at the crossing time.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SUBPNT ( METHOD, TARGET, START, REF, ABCORR, OBSRVR,
     .                 POS,    TRGEPC, SRFVEC                     )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Longitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LON, '~', 0.D0, MEDABS, OK )
 
      END DO

C
C     Now find times when the sub-solar point crosses the IAU_EARTH x-z
C     plane. Delete times when the X coordinate is negative. The result
C     should match that from the longitude search.
C
      CRDSYS = 'rectangular'
      CRDNAM = 'y'

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Empty the window RESLT3.
C
      CALL SCARDD ( 0, RESLT3 )

C
C     Copy the times corresponding to positive X-values to RESLT3.
C
      DO I = 1, WNCARD(RESLT2)

         CALL WNFETD ( RESLT2, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )

         IF ( POS(1) .GT. 0.D0 ) THEN

            CALL WNINSD ( START, FINISH, RESLT3 )

         END IF

      END DO

C
C     Make sure we found at as many roots as
C     we did on the previous search.
C
      XN = WNCARD ( RESULT )
      N  = WNCARD ( RESLT3 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

C
C     Check the solution times against those from the
C     Z-component search.
C
      CALL CHCKAD ( 'RESLT3', RESLT3, '~', RESULT, N, TIMTOL, OK )




C
C --- Case: ------------------------------------------------------
C

C
C     We're going to find times when a selected Sun-Earth ray crosses
C     the Earth equator (the x-y plane of the IAU_EARTH frame). We'll
C     perform the search looking for 0 geodetic latitude and compare
C     the results of that search. We'll then check the Z component of
C     the vector at the result times.
C

      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 '
     .//           'crosses the IAU_EARTH x-y plane.'         )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = '399'
      OBSRVR = '10'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'Geodetic'
      CRDNAM = 'latitude'

C
C     Set DVEC.
C
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size. We need a step small enough
C     to capture the existence window, which we expect to be
C     about 7 minutes long, based on the Earth's orbital speed
C     and the Earth's ellipsoidal radii.
C
      CALL GFSSTP ( 200.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at least one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Check the latitude at the crossing time.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SINCPT ( METHOD, TARGET, START, REF, ABCORR,
     .                 OBSRVR, DREF,   DVEC,  POS, TRGEPC,
     .                 SRFVEC, FOUND                      )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

C
C        Although we're working with geodetic coordinates, we
C        can use RECLAT for latitudes close to zero.
C
         CALL RECLAT ( POS, R, LON, LAT )


         CALL SINCPT ( METHOD, TARGET, FINISH, REF, ABCORR,
     .                 OBSRVR, DREF,   DVEC,   POS, TRGEPC,
     .                 SRFVEC, FOUND                      )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'Intercept FOUND (1)', FOUND, .TRUE., OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Latitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LAT, '~', 0.D0, MEDABS, OK )
 
      END DO

C
C     Repeat the search using Cartesian coordinates.
C
      CRDSYS = 'Rectangular'
      CRDNAM = 'z'

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at as many roots as
C     we did on the previous search.
C
      XN = N
      N  = WNCARD ( RESLT2 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

C
C     Check the solution times against those from the
C     Z-component search.
C
      CALL CHCKAD ( 'RESLT2', RESLT2, '~', RESULT, N, TIMTOL, OK )



C
C --- Case: ------------------------------------------------------
C

C
C     Using the same observer, target, ray, and time period from
C     the previous search, we'll find the times when the absolute
C     minimum and maximum intercept of right ascension occur.
C     Since orbital motion is a larger effect than the Earth's
C     spin in this case, the RA of the intercept is actually
C     increasing with time.
C
C     Note: since the intercept longitude passes through 180 degrees
C     during the time span of this test, the absolute minimum and
C     absolute maximum longitude are both 180 degrees.
C
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 '
     .//           'attains extreme RA values.'               )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

C
C     Set DVEC.
C
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = 'ABSMIN'
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size. We need a step small enough
C     to capture the existence window, which we expect to be
C     about 7 minutes long, based on the Earth's orbital speed
C     and the Earth's ellipsoidal radii.
C
      CALL GFSSTP ( 200.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found exactly one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Save the epoch of the minimum.
C
      MINEPC = RESULT(1)


      RELATE = 'ABSMAX'

C
C     Now search for an absolute maximum.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found exactly one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

C
C     Save the epoch of the maximum.
C
      MAXEPC = RESULT(1)

C
C     Compare RA at the epochs of minimum and 
C     maximum values.
C
      CALL SINCPT ( METHOD, TARGET, MINEPC, REF, ABCORR,
     .              OBSRVR, DREF,   DVEC,   POS, TRGEPC,
     .              SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA0, DEC )

      CALL SINCPT ( METHOD, TARGET, MAXEPC, REF, ABCORR,
     .              OBSRVR, DREF,   DVEC,   POS, TRGEPC,
     .              SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA1, DEC )

C
C     Verify that MINEPC < MAXPEC and RA0 < RA1.
C
      CALL CHCKSD ( 'epoch order', MINEPC, '<', MAXEPC, 0.D0, OK )
      CALL CHCKSD ( 'RA order',    RA0,    '<', RA1,    0.D0, OK )
 



C
C --- Case: ------------------------------------------------------
C

C
C     Repeat the previous searches using an adjustment value of
C     1 degree.
C
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 '
     .//           'attains extreme RA values; adjust = 1 deg.' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

C
C     Set DVEC.
C
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = 'ABSMIN'
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 1.D0 * RPD()
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size. We need a step small enough
C     to capture the existence window, which we expect to be
C     about 7 minutes long, based on the Earth's orbital speed
C     and the Earth's ellipsoidal radii.
C
      CALL GFSSTP ( 200.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found exactly one root. Note that the
C     expectation that there's one root is based on
C     the geometry of this case.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

C
C     Save the final epoch of the solution interval.
C
      MINADJ = RESULT(2)

C
C     Now search for an absolute maximum.
C
      RELATE = 'ABSMAX'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found exactly one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

C
C     Save the initial epoch of the solution interval.
C
      MAXADJ = RESULT(1)

C
C     Check RA at the epochs of where the adjusted minimum and 
C     maximum values occur. 
C
      CALL SINCPT ( METHOD, TARGET, MINADJ, REF, ABCORR,
     .              OBSRVR, DREF,   DVEC,   POS, TRGEPC,
     .              SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA, DEC )

      CALL CHCKSD ( 'Adjusted min RA', RA, '~', RA0+ADJUST, TIGHT, OK )



      CALL SINCPT ( METHOD, TARGET, MAXADJ, REF, ABCORR,
     .              OBSRVR, DREF,   DVEC,   POS, TRGEPC,
     .              SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA, DEC )

      CALL CHCKSD ( 'Adjusted max RA', RA, '~', RA1-ADJUST, TIGHT, OK )








 
C
C --- Case: ------------------------------------------------------
C

C
C     Using the same observer, target, ray, and time period from
C     the previous search, we'll find the times when the right
C     ascension of the intercept is 180 degrees.
C
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 '
     .//           'attains RA 180 degrees.'                  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

C
C     Set DVEC.
C
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '='
      REFVAL = PI()
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size. We need a step small enough
C     to capture the existence window, which we expect to be
C     about 7 minutes long, based on the Earth's orbital speed
C     and the Earth's ellipsoidal radii.
C
      CALL GFSSTP ( 200.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found exactly one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

C
C     Make sure that the start and finish times are
C     close together.
C
      CALL WNFETD ( RESULT, 1, START, FINISH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      QNAME = 'Interval start # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

C
C     Save the epoch of this root for re-use.
C
      ET2 = START

C
C     Check the RA of the intercept at the start time.
C
      CALL SINCPT ( METHOD, TARGET, ET2,   REF, ABCORR,
     .              OBSRVR, DREF,   DVEC,  POS, TRGEPC,
     .              SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA0, DEC )

      CALL CHCKSD ( 'RA0', RA0, '~', PI(), MEDABS, OK )


C
C     Repeat the search using Cartesian coordinates. We're looking
C     for a time when the Y-coordinate is zero. For this case, we
C     happen not to have any roots for positive X, so we should
C     find the same epoch as we did in the RA search.
C     
C
      CRDSYS = 'Rectangular'
      CRDNAM = 'Y'
      REFVAL = 0.D0

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found at as many roots as
C     we did on the previous search.
C
      XN = N
      N  = WNCARD ( RESLT2 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

C
C     Check the solution times against those from the
C     Z-component search.
C
      CALL CHCKAD ( 'RESLT2', RESLT2, '~', RESULT, N, TIMTOL, OK )


C
C --- Case: ------------------------------------------------------
C

C
C     Using the same observer, target, ray, and time period from
C     the previous search, we'll find the times when the right
C     ascension of the intercept is less than 180 degrees.
C
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 '
     .//           'has RA < 180 degrees.'                    )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

C
C     Set DVEC.
C
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '<'
      REFVAL = PI()
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size. We need a step small enough
C     to capture the existence window, which we expect to be
C     about 7 minutes long, based on the Earth's orbital speed
C     and the Earth's ellipsoidal radii.
C
      CALL GFSSTP ( 200.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found exactly one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

C
C     We expect the start and finish times to match MINPEC and ET2,
C     within a reasonable tolerance.
C
      CALL WNFETD ( RESULT, 1, START, FINISH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      QNAME = 'Interval start # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, START, '~', MINEPC, TIMTOL, OK )

      QNAME = 'Interval end # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, FINISH, '~', ET2, TIMTOL, OK )


C
C --- Case: ------------------------------------------------------
C

C
C     Using the same observer, target, ray, and time period from
C     the previous search, we'll find the times when the right
C     ascension of the intercept is greater than 180 degrees.
C
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 '
     .//           'has RA > 180 degrees.'                    )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

C
C     Set DVEC.
C
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '>'
      REFVAL = PI()
      TOL    = CNVTOL
      ADJUST = 0.D0
        
      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Set the search step size. We need a step small enough
C     to capture the existence window, which we expect to be
C     about 7 minutes long, based on the Earth's orbital speed
C     and the Earth's ellipsoidal radii.
C
      CALL GFSSTP ( 200.D0 )

C
C     Perform the search.
C
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure we found exactly one root.
C
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

C
C     We expect the start and finish times to match ET2 and MAXEPC,
C     within a reasonable tolerance.
C
      CALL WNFETD ( RESULT, 1, START, FINISH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      QNAME = 'Interval start # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, START, '~', ET2, TIMTOL, OK )

      QNAME = 'Interval end # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, FINISH, '~', MAXEPC, TIMTOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END



