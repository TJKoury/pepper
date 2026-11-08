!$Procedure      F_GFOCCE ( GFOCCE family tests )

      SUBROUTINE F_GFOCCE ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFOCCE
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

      INCLUDE               'gf.inc'

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
!     This routine partially tests the mid-level SPICELIB geometry
!     routine GFOCCE. Complete testing of GFOCCE requires execution of
!     the test families
!
!        F_GFOCLT
!        F_ZZGFOCU
!
!     as well.
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
!
! $ Version
!
!-    TSPICE Version 2.0.0, 22-FEB-2016 (NJB)
!
!        Updated expected short error messages.
!
!-    TSPICE Version 1.1.0, 15-APR-2014 (EDW)
!
!        Added explicit declaration of GFBAIL type.
!
!-    TSPICE Version 1.0.0, 17-FEB-2009 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD

      INTEGER               WNCARD

      LOGICAL               GFBAIL


!
!     EXTERNAL declarations
!
!
!     SPICELIB default functions for
!
!        - Interrupt handling (no-op function):   GFBAIL
!        - Search refinement:                     GFREFN
!        - Progress report termination:           GFREPF
!        - Progress report initialization:        GFREPI
!        - Progress report update:                GFREPU
!        - Search step size "get" function:       GFSTEP
!
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPF
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP


!
!     Local parameters
!
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

!
!     Local variables
!
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

!
!     Saved variables
!
      SAVE

!
!     Initial values
!

!
!     Begin every test family with an open call.
!
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


!
!     Set up a confinement window. Initialize this and
!     the result window.
!
      UTC0 = '2000 JAN 1  00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

!
!     Create a confinement window with 4 intervals.
!
      DO I = 1, 4

         LEFT  = ET0  + (I-1)*SPD() + 3600.D0
         RIGHT = LEFT +               3600.D0 * 22

         CALL WNINSD ( LEFT, RIGHT, CNFINE )

      END DO




!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
! --- Case: ------------------------------------------------------
!
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


      CALL GFOCCE ( 'ANY', &
                    'MOON',   'ellipsoid',  'IAU_MOON', &
                    'SUN',    'ellipsoid',  'IAU_SUN', &
                    'LT',     'EARTH',      CNVTOL, &
                    GFSTEP,   GFREFN,       RPT, &
                    GFREPI,   GFREPU,       GFREPF, &
                    BAIL,     GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

!
!     Restore original result window size.
!
      CALL SSIZED ( 2*MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
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

      CALL GFOCCE ( 'ANY', &
                    'MOON',   'ellipsoid',  'IAU_MOON', &
                    'SUN',    'ellipsoid',  'IAU_SUN', &
                    'LT',     'EARTH',      TOL, &
                    GFSTEP,   GFREFN,       RPT, &
                    GFREPI,   GFREPU,       GFREPF, &
                    BAIL,     GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )

      CALL SSIZED ( 2*MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized shape for front target' )

      TOL = CNVTOL

      CALL GFOCCE ( 'ANY', &
                    'MOON',   'ray',        'IAU_MOON', &
                    'SUN',    'ellipsoid',  'IAU_SUN', &
                    'LT',     'EARTH',      TOL, &
                    GFSTEP,   GFREFN,       RPT, &
                    GFREPI,   GFREPU,       GFREPF, &
                    BAIL,     GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized shape for back target' )

      TOL = CNVTOL

      CALL GFOCCE ( 'ANY', &
                    'MOON',   'point',      'IAU_MOON', &
                    'SUN',    'ray',        'IAU_SUN', &
                    'LT',     'EARTH',      TOL, &
                    GFSTEP,   GFREFN,       RPT, &
                    GFREPI,   GFREPU,       GFREPF, &
                    BAIL,     GFBAIL,       CNFINE,  RESULT )


      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )






!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Any occultation of ALPHA by BETA, abcorr = LT+S' )

!
!     Note: the stellar aberration correction spec should be ignored.
!
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

      CALL GFOCCE ( 'ANY', &
                    'beta',   'ellipsoid',  'betafixed', &
                    'alpha',  'ellipsoid',  'alphafixed', &
                    'LT+S',   'sun',        TOL, &
                    GFSTEP,   GFREFN,       RPT, &
                    GFREPI,   GFREPU,       GFREPF, &
                    BAIL,     GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0, &
                    OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )

         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )
!
!        Check occultation start time.
!
         XTIME = DBLE(I-1)*SPD() + 1.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

!
!        Check occultation end time.
!
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 10*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT, &
                       'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)', &
                       TIMSTR )
         CALL TIMOUT ( RIGHT, &
                       'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)', &
                       TIMSTR )
      END DO


!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

!
!     Clean up SPK files.
!
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
