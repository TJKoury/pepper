C$Procedure F_ZZBQUAD ( ZZBQUAD tests )
 
      SUBROUTINE F_ZZBQUAD ( OK )
 
C$ Abstract
C
C     Exercise the bounded quadratic solution routine ZZBQUAD.
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
C     This routine tests the bounded quadratic solution
C     routine ZZBQUAD. 
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
C-    TSPICE Version 1.0.0, 20-JAN-2017 (NJB)
C
C        Previous version 20-SEP-2014 (NJB)
C
C-&
 
C
C     SPICELIB functions
C


C
C     Local Parameters 
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

C
C     Local Variables 
C  
      CHARACTER*(LNSIZE)    TITLE
  

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      UB
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XR1
      DOUBLE PRECISION      XR2

      INTEGER               N
      INTEGER               NX
      INTEGER               XN
      INTEGER               XNX
     
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
      CALL TOPEN ( 'F_ZZBQUAD' )




C**********************************************************************
C
C     Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Coefficients have absolute values that are too large.'

      CALL TCASE ( TITLE )


      A  =   1.D160
      B  =   1.D0
      C  =   1.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      A  =  -1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      A  =   1.D0
      B  =   1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      B  =  -1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      B  =   1.D0
      C  =   1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      B  =   1.D0
      C  =   1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      C  =  -1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      A  =   1.D160
      B  =   1.D0
      C  =   1.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Non-postive upper bound.'
      CALL TCASE ( TITLE )

      A  =   1.D0
      B  =   1.D0
      C  =  -1.D0

      UB = 0.D0 

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      UB = -1.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Bound has absolute values that are too large.'
      CALL TCASE ( TITLE )

      A  =   1.D0
      B  =   1.D0
      C  =   1.D0
      UB =   1.D160

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )





C**********************************************************************
C
C     Non-error exceptions
C
C**********************************************************************



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Complex roots'

      CALL TCASE ( TITLE )


C
C     The equation is
C
C        ( X + 2i ) * ( X - 2i ) = 0
C
C     or
C
C         2
C        X  + 4 = 0
C     
C
C     Use an upper bound of 10.
C

      A  =   1.D0
      B  =   0.D0
      C  =   4.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'First order equation, no roots excluded'

      CALL TCASE ( TITLE )


C
C     The equation is
C
C         
C        X + 4 = 0
C     
C
C     Use an upper bound of 10.
C

      A  =   0.D0
      B  =   1.D0
      C  =   4.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = -4.D0
      XR2 = -4.D0

      TOL = VTIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'First order equation, both roots excluded'

      CALL TCASE ( TITLE )


C
C     The equation is
C
C         
C        X + 4 = 0
C     
C
C     Use an upper bound of 4 - 1e-12.
C

      A  =   0.D0
      B  =   1.D0
      C  =   4.D0
      UB =   4.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate equation; form is 0 = 0. N should '
     .//       'be set to -1.'

      CALL TCASE ( TITLE )


C
C     The equation is
C
C           2
C        0*X  + 0*X + 0 = 0
C     
C
C     Use an upper bound of 10.
C

      A  =   0.D0
      B  =   0.D0
      C  =   0.D0
      UB =   10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )




C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate equation; form is 1 = 0. N should '
     .//       'be set to -2.'

      CALL TCASE ( TITLE )


C
C     The equation is
C
C           2
C        0*X  + 0*X + 1 = 0
C     
C
C     Use an upper bound of 10.
C

      A  =   0.D0
      B  =   0.D0
      C  =   1.D0
      UB =   10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Single root of multiplicity 2; root is too '
     .//       'large to compute.' 

      CALL TCASE ( TITLE )

C
C     Set coefficients to produce the root -4, having multiplicity 2.
C
      A  =    4.D0
      B  =   32.D0
      C  =   64.D0
      UB =    3.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: equation with two small real roots. '
     .//       'The upper bound does not exclude roots.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( X - 1 ) * ( X - 2 ) = 0
C
C     or
C
C         2
C        X  - 3X + 2 = 0
C     
C
C     Use an upper bound of 10.
C
      A  =   1.D0
      B  =  -3.D0
      C  =   2.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D0
      XR2 = 2.D0

      TOL = VTIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with two small real roots; the root '
     .//       'of larger magnitude is positive. '
     .//       'The upper bound excludes the larger root.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( X - 1 ) * ( X - 200 ) = 0
C
C     or
C
C         2
C        X  - 201X + 200 = 0
C     
C
C     Use an upper bound of 200 - 1e-12
C
      A  =     1.D0
      B  =  -201.D0
      C  =   200.D0
      UB =   200.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D0
      XR2 = 0.D0

      TOL = VTIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with two small real roots; the root '
     .//       'of larger magnitude is positive. '
     .//       'The upper bound excludes both roots.'

      CALL TCASE ( TITLE )

C
C     Use the equation above but change UB.
C
      UB = 1.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 2
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with two small real roots; the root '
     .//       'of larger magnitude is negative. '
     .//       'No roots are excluded.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( X - 1 ) * ( X + 200 ) = 0
C
C     or
C
C         2
C        X  + 199 X - 200 = 0
C     
C
C     Use an upper bound of 200 + 1e-12
C
      A  =     1.D0
      B  =   199.D0
      C  =  -200.D0
      UB =   200.D0 + 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 =    1.D0
      XR2 = -200.D0

      TOL = VTIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with two small real roots; the root '
     .//       'of larger magnitude is negative. '
     .//       'The upper bound excludes the root of '
     .//       'larger magnitude.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( X - 1 ) * ( X + 200 ) = 0
C
C     or
C
C         2
C        X  + 199 X - 200 = 0
C     
C
C     Use an upper bound of 200 - 1e-12
C
      A  =     1.D0
      B  =   199.D0
      C  =  -200.D0
      UB =   200.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with two small real roots; the root '
     .//       'of larger magnitude is negative. '
     .//       'The upper bound excludes both roots.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( X - 1 ) * ( X + 200 ) = 0
C
C     or
C
C         2
C        X  + 199 X - 200 = 0
C     
C
C     Use an upper bound of 1 - 1e-12
C
      A  =     1.D0
      B  =   199.D0
      C  =  -200.D0
      UB =     1.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 2
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=',  XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with two large real roots. '
     .//       'The upper bound excludes the larger root.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( X - 1.D70 ) * ( X - 2.D70 ) = 0
C
C     or
C
C         2
C        X  - 3.D70 X + 2.D140 = 0
C     
C
C     Use an upper bound of 1.5D70
C
      A  =   1.D0
      B  =  -3.D70
      C  =   2.D140
      UB =   1.5D70

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D70
      XR2 = 0.D0

      TOL = TIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with two large real roots. '
     .//       'The upper bound excludes both roots.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( X - 1.D70 ) * ( X - 2.D70 ) = 0
C
C     or
C
C         2
C        X  - 3.D70 X + 2.D140 = 0
C     
C
C     Use an upper bound of 1.D70 - 1.D-56
C
      A  =   1.D0
      B  =  -3.D70
      C  =   2.D140
      UB =   1.0D70 - 1.0D56

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 2
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=',  XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )





C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


