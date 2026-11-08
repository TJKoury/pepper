C$Procedure      F_ZZGFLNG1 ( Test GF longitude solver, part 1 )
  
      SUBROUTINE F_ZZGFLNG1 ( OK )

C$ Abstract
C
C     Test the GF private longitude search routine ZZGFLONG.
C     This family tests only exception handling.
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
C     This routine does not generate any exceptions. Routines in its
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
C     This routine tests exception handling performed by the private
C     SPICELIB GF coordinate search routine ZZGFLONG.
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    TSPICE Version 1.2.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 1.1.0, 15-APR-2014 (EDW)
C
C        Added explicit declaration of GFBAIL type.
C
C-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB) (EDW)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD

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
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgflong.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgflong.bsp' )
  

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )

 

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
      PARAMETER           ( MAXWIN = 2*20000 )
      
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
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(VDFLEN)    VECDEF      
      
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MW )
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MW )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      WORK   ( MW, NW )

      INTEGER               HAN2
      INTEGER               HANDLE      
      INTEGER               N
      INTEGER               OBSID
      INTEGER               TRGID

      LOGICAL               BAIL
      LOGICAL               FOUND
      LOGICAL               RPT

C
C     Saved all.
C
      SAVE

C 
C
C     Initial values 
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFLNG1' )


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
      CALL TCASE ( 'Bad window count' )

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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     14,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     0,      WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )



      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     0,      WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     -1,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad operator' )


      RELATE = '=='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      TARGET = 'EARTH'
      OBSRVR = 'Y'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 

      TOL    = -1.D0

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-longitude/RA system' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'RECTANGULAR'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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
      
      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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
 
      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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
 
      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )






C*********************************************************************
C*
C*    Non-error exceptional cases
C*
C*********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Empty confinement window' )

C
C     Start with an empty confinement window and non-empty
C     result window; make sure the result window is empty
C     on output.
C
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


      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
           
      ET0 = ET
      ET1 = ET + 1.D0

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RPT  = .FALSE.
      BAIL = .FALSE.

C
C     Endow RESULT with a single interval.
C
      CALL SCARDD ( 0, RESULT )
      CALL WNINSD ( ET0, ET1, RESULT )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                BAIL,   GFBAIL, MW,     NW,     WORK,
     .                CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT), '=', 0, 0, OK )




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



