C$Procedure F_ZZTANSLV ( ZZTANSLV tests )
 
      SUBROUTINE F_ZZTANSLV ( OK )
 
C$ Abstract
C
C     Exercise the routine ZZTANSLV.
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
C     This routine tests the SPICELIB routine ZZTANSLV.
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
C-    TSPICE Version 1.0.0, 30-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               WNCARD

C
C     EXTERNAL routines
C
      EXTERNAL              GFSTEP
      EXTERNAL              GFREFN
      EXTERNAL              T_TANSTA
      
C
C     Local Parameters 
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 1000 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2 * MAXIVL )

C
C     Local Variables 
C   
      DOUBLE PRECISION      CNVTOL
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      POINTS ( 3, MAXWIN )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      XRESLT ( MAXWIN )
      DOUBLE PRECISION      XPNTS  ( 3, MAXWIN )
    
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               XN

      LOGICAL               CSTEP
      LOGICAL               ENDFLG ( 2 )


C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZTANSLV' )



C**********************************************************************
C
C     ZZTANSLV normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find all state transitions on interval [1,10]. ' 
     .//           'Use constant step of 0.5.'                      )

  
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 1.D0
      FINISH = 10.D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found.
C     
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )
      
C
C     Check the interval endpoints.
C      
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

C
C     Check the endpoint flags. The first and last endpoints are
C     not considered to be state transitions.
C
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

C
C     Check the returned points. The values are defined only for
C     transitions, so the first and last points in the array are
C     not checked.
C
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1) 
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~', 
     .                        XPNTS (1,2), 3*(2*XN-2), TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find all state transitions on interval [1,10]. ' 
     .//           'Use a step of 0.5, but use the GF step callback.' )

  
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .FALSE.

      CALL GFSSTP ( 0.5D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      STEP   = 0.5D0
      START  = 1.D0
      FINISH = 10.D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found.
C     
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )
      
C
C     Check the interval endpoints.
C      
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

C
C     Check the endpoint flags. The first and last endpoints are
C     not considered to be state transitions.
C
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

C
C     Check the returned points. The values are defined only for
C     transitions, so the first and last points in the array are
C     not checked.
C
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1) 
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~', 
     .                        XPNTS (1,2), 3*(2*XN-2), TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find all state transitions on interval [1,10]. ' 
     .//           'Use constant step of 0.99.'                      )

  
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.99D0
      START  = 1.D0
      FINISH = 10.D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found.
C     
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )
      
C
C     Check the interval endpoints.
C      
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

C
C     Check the endpoint flags. The first and last endpoints are
C     not considered to be state transitions.
C
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

C
C     Check the returned points. The values are defined only for
C     transitions, so the first and last points in the array are
C     not checked.
C
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1) 
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~', 
     .                        XPNTS (1,2), 3*(2*XN-2), TOL, OK )


 

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find all state transitions on interval [0.5,10.5]. '
     .//           'Use constant step of 0.5.'                        )

  
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found.
C     
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )
      
C
C     Check the interval endpoints.
C      
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

C
C     Check the endpoint flags. The first and last endpoints ARE
C     not considered to be state transitions.
C
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

C
C     Check the returned points. All of the points should be valid.
C
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1) 
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS, '~', XPNTS, 6*XN, TOL, OK )
 
 

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find all state transitions on interval [0.5,10.5]. '
     .//           'Use a step of 0.5, but use the GF step callback.' )


  
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .FALSE.

      CALL GFSSTP ( 0.5D0 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


      STEP   = 0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found.
C     
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )
      
C
C     Check the interval endpoints.
C      
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

C
C     Check the endpoint flags. The first and last endpoints ARE
C     not considered to be state transitions.
C
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

C
C     Check the returned points. All of the points should be valid.
C
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1) 
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS, '~', XPNTS, 6*XN, TOL, OK )
 

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find all state transitions on interval [0.5,10.5]. '
     .//           'Use constant step of 0.99.'                        )

  
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.99D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found.
C     
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )
      
C
C     Check the interval endpoints.
C      
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

C
C     Check the endpoint flags. The first and last endpoints ARE
C     not considered to be state transitions.
C
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

C
C     Check the returned points. All of the points should be valid.
C
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1) 
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS, '~', XPNTS, 6*XN, TOL, OK )
 

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Look for state transition on interval [2.0,2.5]. ' 
     .//           'Use constant step of 0.5. A transition should '
     .//           'be found. The first interval endpoint should '
     .//           'be considered a non-transition endpoint, while '
     .//           'the second one should be considered a transition.' )

C
C     In this case the state function is .TRUE. at X = 1.0 and
C     nowhere else.
C

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.0D0
      FINISH = 2.5D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found. We should have a transition
C     at the right endpoint of the search interval.
C     
      XN = 1
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )

C
C     The last endpoint flag should indicate a transition.
C
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

C
C     Check the result window. The second endpoint is determined
C     by the convergence algorithm. We expect the actual result
C     only to be close to the expected value.
C
      XRESLT(1) = START
      XRESLT(2) = START + CNVTOL/2
      

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2, TOL, OK )

C
C     The point value at X = 2 is the same as at X = 1.
C
      CALL VPACK ( 1.D0, 2.D0, 3.D0, XPNTS )

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~~', XPNTS, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Look for state transition on interval [2.5,3.0]. ' 
     .//           'Use constant step of 0.5. A transition should '
     .//           'be found. The first interval endpoint should '
     .//           'be considered a transition endpoint, while '
     .//           'the second one should NOT be considered a '
     .//           'transition.'                               )

C
C     In this case the state function is .TRUE. at X = 1.0 and
C     nowhere else.
C

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intervals found. We should have a transition
C     at the right endpoint of the search interval.
C     
      XN = 1
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )

C
C     The last endpoint flag should indicate a transition.
C
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

C
C     Check the result window. The second endpoint is determined
C     by the convergence algorithm. We expect the actual result
C     only to be close to the expected value.
C
      XRESLT(1) = FINISH - CNVTOL/2
      XRESLT(2) = FINISH
      

      TOL = 2 * CNVTOL
     
      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2, TOL, OK )
 
C
C     The point value should correspond to the left endpoint.
C
      CALL VPACK ( 3.D0, 4.D0, 5.D0, XPNTS )

      CALL CHCKAD ( 'POINTS', POINTS, '~~', XPNTS, 3, TOL, OK )

 

C**********************************************************************
C
C     ZZTANSLV Error cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Non-positive tolerance.' )

     

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0
      CNVTOL = 0.D0

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )

      CNVTOL   = -1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Tolerance too small compared to START.' )

     

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0

      CNVTOL = 1.D-20 * START

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Tolerance too small compared to FINISH.' )

     

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0

      CNVTOL = 1.D-20 * FINISH

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Non-positive constant step.' )
     

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.D0
      START  = 2.5D0
      FINISH = 3.0D0

      CNVTOL = 1.D-3 * FINISH

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCONSTSTEP)', OK )

      STEP = -1.D0

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCONSTSTEP)', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Constant step is positive but too small.' )
     

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      START  = 2.5D0
      FINISH = 3.0D0

      STEP   = 1.D-19 * FINISH

      CNVTOL = 1.D-3 * FINISH

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCONSTSTEP)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'START > FINISH.' )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = START - 1.D0

      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'START = FINISH.' )
C
C     This is a non-error exception.
C

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = START 

      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Points array too small.' )

     

      CALL SSIZED ( 8, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   =  0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Result window too small.' )

     

      CALL SSIZED ( 9, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   =  0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, 
     .                CSTEP,    STEP,   START,  FINISH,
     .                CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)', OK )
 
C
C     End of error cases.
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END



C
C     State function for use by F_ZZTANSLV.
C
C     Let LOWER be defined as
C
C        LOWER = floor( X ).           
C
C     The function returns .TRUE. if 
C
C        LOWER 
C
C     is odd and
C
C        0  <  LOWER  <  X  <  LOWER+1  
C           -         -     -
C
C     For all other X, the function returns .FALSE.
C
C     When the function returns .TRUE., the returned
C     value of POINT is
C
C        ( LOWER, LOWER+1, LOWER+2 )
C
C     When the function returns .FALSE., the returned
C     value of POINT is
C
C        ( 0, 0, 0 )
C
C
      SUBROUTINE T_TANSTA ( X, STATE, POINT ) 

      IMPLICIT NONE

      DOUBLE PRECISION      X 
      LOGICAL               STATE
      DOUBLE PRECISION      POINT ( 3 )

C
C     SPICELIB functions
C
      LOGICAL               ODD

C
C     Local variables
C     
      DOUBLE PRECISION      DVAL
      DOUBLE PRECISION      FLOOR

      INTEGER               IFLOOR


      IF ( X .LT. 1.D0 ) THEN

         STATE = .FALSE.
         CALL CLEARD ( 3, POINT )

      ELSE

         FLOOR  = AINT ( X ) 
         IFLOOR = INT ( FLOOR )

         IF ( ODD(IFLOOR) ) THEN

            STATE = .TRUE.
            DVAL  = FLOOR

         ELSE IF ( X .EQ. FLOOR ) THEN
C
C           IFLOOR is even.
C
            STATE = .TRUE.
            DVAL  = FLOOR - 1.D0

         ELSE

            STATE = .FALSE.

         END IF

      END IF

      
      IF ( STATE ) THEN
         
         CALL VPACK ( DVAL, DVAL+1.D0, DVAL+2.D0, POINT )

      ELSE

         CALL CLEARD ( 3, POINT )

      END IF

      RETURN
      END 
