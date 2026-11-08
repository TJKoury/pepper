C$Procedure      F_GFOCCE ( GFOCCE family tests )

      SUBROUTINE F_GFOCCE ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        GFOCCE
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

      INCLUDE               'gf.inc'

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
C     This routine partially tests the mid-level SPICELIB geometry
C     routine GFOCCE. Complete testing of GFOCCE requires execution of
C     the test families
C
C        F_GFOCLT
C        F_ZZGFOCU
C
C     as well.
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
C-    TSPICE Version 2.0.0, 22-FEB-2016 (NJB)
C
C        Updated expected short error messages.
C
C-    TSPICE Version 1.1.0, 15-APR-2014 (EDW)
C
C        Added explicit declaration of GFBAIL type.
C
C-    TSPICE Version 1.0.0, 17-FEB-2009 (NJB)
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
C
C     SPICELIB default functions for
C
C        - Interrupt handling (no-op function):   GFBAIL
C        - Search refinement:                     GFREFN
C        - Progress report termination:           GFREPF
C        - Progress report initialization:        GFREPI
C        - Progress report update:                GFREPU
C        - Search step size "get" function:       GFSTEP
C     
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPF
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP


C     
C     Local parameters
C
      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK  = 'nat.tpc' )

      CHARACTER*(*)         PCK2 
      PARAMETER           ( PCK2 = 'generic.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1 = 'nat.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2 = 'generic.bsp' )


      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = CNVTOL )
 

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )


      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(TIMLEN)    UTC0
      CHARACTER*(TIMLEN)    UTC1

      DOUBLE PRECISION      CNFINE ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RESULT ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XTIME

      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               I

      LOGICAL               BAIL
      LOGICAL               RPT

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFOCCE' )

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
      UTC0 = '2000 JAN 1  00:00:00 TDB'
      
      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

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
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Result window too small (detected before search)' )

      CALL SSIZED ( 1, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 5 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BAIL = .FALSE.
      RPT  = .FALSE.


      STEP = 300.D0
      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL  = CNVTOL


      CALL GFOCCE ( 'ANY',
     .              'MOON',   'ellipsoid',  'IAU_MOON',
     .              'SUN',    'ellipsoid',  'IAU_SUN',
     .              'LT',     'EARTH',      CNVTOL,
     .              GFSTEP,   GFREFN,       RPT,
     .              GFREPI,   GFREPU,       GFREPF,
     .              BAIL,     GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

C
C     Restore original result window size.
C
      CALL SSIZED ( 2*MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad convergence tolerance.' )

      CALL STR2ET ( '2000 JAN 1 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 5 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      BAIL = .FALSE.
      RPT  = .FALSE.

      STEP = 300.D0
      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL  = 0.D0

      CALL GFOCCE ( 'ANY',
     .              'MOON',   'ellipsoid',  'IAU_MOON',
     .              'SUN',    'ellipsoid',  'IAU_SUN',
     .              'LT',     'EARTH',      TOL,
     .              GFSTEP,   GFREFN,       RPT,
     .              GFREPI,   GFREPU,       GFREPF,
     .              BAIL,     GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )

      CALL SSIZED ( 2*MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized shape for front target' )
 
      TOL = CNVTOL

      CALL GFOCCE ( 'ANY',
     .              'MOON',   'ray',        'IAU_MOON',
     .              'SUN',    'ellipsoid',  'IAU_SUN',
     .              'LT',     'EARTH',      TOL,
     .              GFSTEP,   GFREFN,       RPT,
     .              GFREPI,   GFREPU,       GFREPF,
     .              BAIL,     GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized shape for back target' )

      TOL = CNVTOL

      CALL GFOCCE ( 'ANY',
     .              'MOON',   'point',      'IAU_MOON',
     .              'SUN',    'ray',        'IAU_SUN',
     .              'LT',     'EARTH',      TOL,
     .              GFSTEP,   GFREFN,       RPT,
     .              GFREPI,   GFREPU,       GFREPF,
     .              BAIL,     GFBAIL,       CNFINE,  RESULT )
 

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )






C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************
 


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Any occultation of ALPHA by BETA, abcorr = LT+S' )

C
C     Note: the stellar aberration correction spec should be ignored.
C
      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'
      
      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      BAIL = .FALSE.
      RPT  = .FALSE.


      STEP = 300.D0
      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL  = CNVTOL

      CALL GFOCCE ( 'ANY',
     .              'beta',   'ellipsoid',  'betafixed',
     .              'alpha',  'ellipsoid',  'alphafixed',
     .              'LT+S',   'sun',        TOL,
     .              GFSTEP,   GFREFN,       RPT,
     .              GFREPI,   GFREPU,       GFREPF,
     .              BAIL,     GFBAIL,       CNFINE,  RESULT )
 
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0, 
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )

         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )
C
C        Check occultation start time.
C
         XTIME = DBLE(I-1)*SPD() + 1.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 10*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT, 
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
         CALL TIMOUT ( RIGHT, 
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
      END DO


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


      CALL T_SUCCESS ( OK )

      RETURN
      END
