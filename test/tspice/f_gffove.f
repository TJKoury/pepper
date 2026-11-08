C$Procedure      F_GFFOVE ( Test GFFOVE )
  
      SUBROUTINE F_GFFOVE ( OK )

C$ Abstract
C
C     Test the GF FOV search API GFFOVE. 
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
C     This family partially tests the GF API routine GFFOVE. Complete
C     testing of GFFOVE requires execution of the test families
C
C        F_GFTFOV
C        F_GFRFOV
C        F_ZZGFFVU
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
      PARAMETER           ( PCK    = 'zzgffvu.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         NIK
      PARAMETER           ( NIK    = 'nat.ti' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgffvu.bsp' )
  
      
      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 2*CNVTOL )

 
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )
 
C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LBLSIZ)    QNAME
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TFRAME
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(SHPLEN)    TSHAPE
      
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      ENDPT  ( 2 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XTIME

      INTEGER               HAN2
      INTEGER               HANDLE      
      INTEGER               I
      INTEGER               N

      LOGICAL               BAIL
      LOGICAL               RPT
  
C
C     Saved Variables
C
      SAVE

C
C     Initial values 
C
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFFOVE' )


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
C     Create and load Nat's solar system SPK, PCK/FK, and IK
C     files.
C     
      CALL NATPCK ( NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATIK  ( NIK,  NSPK,   NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
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
C     We exercise only those error checks that are actually
C     performed in GFOCCE.
C



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Result window too small' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( 1,      RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      TSHAPE = 'ELLIPSOID'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL

      RPT    = .FALSE.
      BAIL   = .FALSE.

      CALL GFSSTP ( STEP )

      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR,     
     .              TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL,    
     .              GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU,   
     .              GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

C
C     Restore window sizes.
C
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad tolerance value' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 0.D0
      TOL    = 0.D0

      RPT    = .FALSE.
      BAIL   = .FALSE.

      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR,     
     .              TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL,    
     .              GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU,   
     .              GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )






C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C     We'll start out with cases using an ellipsoidal target and
C     geometric states. We'll use the instrument defined
C     in nat.ti:
C
C        ALPHA_ELLIPSE_NONE
C
C     This instrument track's body Alpha so body Beta's FOV entry and
C     exit times match the start and stop times of Beta's transit
C     across Alpha.
C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

C
C     Use a 3-day span that covers 3 events.
C
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TARGET = 'BETA'
      TSHAPE = 'ELLIPSOID'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL
      ABCORR = 'NONE'

      RPT    = .FALSE.
      BAIL   = .FALSE.


C
C --- Case: ------------------------------------------------------
C
C

C
C     Set up the TCASE call.
C
      TITLE = 'Target #; target shape #; Inst. #; #; '
     .//      'target frame #'

      CALL REPMC ( TITLE, '#', TARGET, TITLE )
      CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
      CALL REPMC ( TITLE, '#', INST,   TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
      CALL REPMC ( TITLE, '#', TFRAME, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE ( TITLE )

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR,     
     .              TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL,    
     .              GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU,   
     .              GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = WNCARD( RESULT )

C
C     We're expecting 3 events.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      IF ( N .GT. 0 ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the interval endpoints. The event should
C           start at noon and stop 10 minutes later.

            QNAME = 'Start time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD()

            CALL CHCKSD ( QNAME, ENDPT(1), '~', 
     .                    XTIME, TIMTOL,    OK )

               
            QNAME = 'Stop time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD() + 600.D0

            CALL CHCKSD ( QNAME, ENDPT(2), '~', 
     .                   XTIME, TIMTOL,    OK )

         END DO
C
C        End of result interval check loop.
C
      END IF
C
C     End of non-empty result window block.
C



C
C     The second case uses a ray as the target. The ray will
C     point along the +X axis of the BETA_VIEW_XY frame, which
C     tracks body BETA.
C
C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

C
C     Use a 3-day span that covers 3 events.
C
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      INST   = 'ALPHA_RECTANGLE_NONE'
      TARGET = '<none>'
      TSHAPE = 'RAY'
      TFRAME = 'BETA_VIEW_XY'

      CALL VPACK ( 1.D0, 0.0D0, 0.D0, RAYDIR )

      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL
      ABCORR = 'NONE'

      RPT    = .FALSE.
      BAIL   = .FALSE.


C
C --- Case: ------------------------------------------------------
C
C

C
C     Set up the TCASE call.
C
      TITLE = 'Target #; target shape #; Inst. #; #; '
     .//      'target frame #'

      CALL REPMC ( TITLE, '#', TARGET, TITLE )
      CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
      CALL REPMC ( TITLE, '#', INST,   TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
      CALL REPMC ( TITLE, '#', TFRAME, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      TITLE = 'Target #; target shape #; Inst. #; #; '

      CALL TCASE ( TITLE )

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL GFFOVE ( INST,    TSHAPE,  RAYDIR,     
     .              TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL,    
     .              GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU,   
     .              GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = WNCARD( RESULT )

C
C     We're expecting 3 events.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      IF ( N .GT. 0 ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the interval endpoints. The event should start at one
C           minute past noon and stop 8 minutes later.
C
            QNAME = 'Start time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD() + 60.D0

            CALL CHCKSD ( QNAME, ENDPT(1), '~', 
     .                    XTIME, TIMTOL,    OK )

               
            QNAME = 'Stop time @'
            CALL REPMI ( QNAME, '@', I, QNAME )

            XTIME = (I-1)*SPD() + 540.D0

            CALL CHCKSD ( QNAME, ENDPT(2), '~', 
     .                   XTIME, TIMTOL,    OK )

         END DO
C
C        End of result interval check loop.
C
      END IF
C
C     End of non-empty result window block.
C


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



