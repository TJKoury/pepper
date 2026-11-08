C$Procedure F_ZZCNQUAD ( ZZCNQUAD tests )
 
      SUBROUTINE F_ZZCNQUAD ( OK )
 
C$ Abstract
C
C     Exercise the cone intercept quadratic solution routine ZZCNQUAD.
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
C     This routine tests the cone intercept quadratic solution
C     routine ZZCNQUAD. 
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
C-    TSPICE Version 1.0.0, 07-FEB-2017 (NJB)
C     
C        Previous version 23-SEP-2014 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      TOUCHD

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
      DOUBLE PRECISION      C1 ( 2 )
      DOUBLE PRECISION      C2 ( 2 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      UB
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XR1
      DOUBLE PRECISION      XR2
      DOUBLE PRECISION      XVAL1
      DOUBLE PRECISION      XVAL2

      INTEGER               N
      INTEGER               XN
     
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
      CALL TOPEN ( 'F_ZZCNQUAD' )




C**********************************************************************
C
C     Error cases
C
C**********************************************************************


 

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Bound has absolute values that are too large.'
      CALL TCASE ( TITLE )

      A  =   1.D0
      B  =   1.D0
      C  =   1.D0
      UB =   1.D160

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      UB =   -1.D160

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = -4.D0
      XR2 = -4.D0

      TOL = VTIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )

C
C     Make sure the root of larger magnitude is the second
C     one.
C
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )


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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )



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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D0
      XR2 = 2.D0

      TOL = VTIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )

C
C     Make sure the root of larger magnitude is the second
C     one.
C
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 =    1.D0
      XR2 = -200.D0

      TOL = VTIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )

C
C     Make sure the root of larger magnitude is the second
C     one.
C
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D0
      XR2 = 0.D0

      TOL = TIGHT
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )

      TOL = 0.D0
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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D70
      XR2 = 0.D0

      TOL = TIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with one small real root, one root '
     .//       'having magnitude near 1. '
     .//       'The upper bound excludes the larger root. '
     .//       'The quadratic and zero-order coefficients '
     .//       'are small; the linear coefficient has '
     .//       'magnitude near 1.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( 1.D-5 X - 1.D-10 ) * ( 1.D-5 X - 1.D5 ) = 0
C
C     or
C
C                2
C        1.D-10 X  -(1+1.D-15) X + 1.D-5 =  0
C     
C
C     Use an upper bound of 1.D9
C
      A  =   1.D-10
      B  =   - ( 1.D0 + 1.D-15 )
      C  =   1.D-5
      UB =   1.D9

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D-5
      XR2 = 0.D0

      TOL = TIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation with one small real root, one root '
     .//       'having magnitude near 1. '
     .//       'The upper bound excludes both roots. '
     .//       'The quadratic and zero-order coefficients '
     .//       'are small; the linear coefficient has '
     .//       'magnitude near 1.'

      CALL TCASE ( TITLE )

C
C     The equation is
C
C        ( 1.D-5 X - 1.D-10 ) * ( 1.D-5 X - 1.D5 ) = 0
C
C     or
C
C                2
C        1.D-10 X  -(1+1.D-15) X + 1.D-5 =  0
C     
C
C     Use an upper bound of 1.D9
C
      A  =   1.D-10
      B  =   - ( 1.D0 + 1.D-15 )
      C  =   1.D-5
      UB =   1.D-6

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = TIGHT
      
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Reciprocal solution case: negative discriminant.'

      CALL TCASE ( TITLE )

      A  =   1.D-10
      B  =   1.D0
      C  =   1.D11
      UB =   1.D9

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )
 

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Reciprocal solution case: zero discriminant. '
     .//       'Root has multiplicity 2. Root magnitude is '
     .//       'less than bound.'

      CALL TCASE ( TITLE )

      A  =   2.D0 ** (-37.D0 )
      B  =   1.D0
      C  =   2.D0 **  35.D0
      UB =   1.D15

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Reciprocal solution case: zero discriminant. '
     .//       'Root has multiplicity 2. Root is too large '
     .//       'to accept.'                             

      CALL TCASE ( TITLE )

      A  =   2.D0 ** (-37.D0 )
      B  =   1.D0
      C  =   2.D0 **  35.D0
      UB =   1.D9

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )
 




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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0
      
      CALL CHCKSD ( 'R1', R1, '=',  XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation has very large B compared to both A and C.'
      CALL TCASE ( TITLE )


      A  =   3.D0
      B  =   4.D11
      C  =   5.D2
      UB =   1.D20

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


      F  = TOUCHD( A * R1**2  +  B * R1  +  C )

      TOL = 1.D-6

      CALL CHCKSD ( 'F(R1)', F, '~', 0.D0, TOL, OK )

C      WRITE (*,*) 'f(R1): ', F

C
C     Compare R1 and R2 to results from RQUAD:
C
      CALL RQUAD ( A, B, C, C1, C2 )
      
      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', C1(1), TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', C2(1), TOL, OK )


C
C     Make sure the root of larger magnitude is the second
C     one.
C
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )



       
C     F  = TOUCHD( A * R2**2  +  B * R2  +  C )

C      WRITE (*,*) 'f(R2): ', F

C      WRITE (*,*) 'C1 = ', C1
C      WRITE (*,*) 'C2 = ', C2

C      WRITE (*,*) 'C1(1) - R1 = ', C1(1) - R1
C      WRITE (*,*) 'C2(1) - R2 = ', C2(1) - R2
      



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation has very large B compared to both A and C; '
     .//       'B is negative.'
      CALL TCASE ( TITLE )


      A  =    3.D0
      B  =   -4.D11
      C  =    5.D2
      UB =   1.D20

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


      F  = TOUCHD( A * R1**2  +  B * R1  +  C )

      TOL = 1.D-6

      CALL CHCKSD ( 'F(R1)', F, '~', 0.D0, TOL, OK )

C      WRITE (*,*) 'f(R1): ', F

C
C     Compare R1 and R2 to results from RQUAD:
C
      CALL RQUAD ( A, B, C, C1, C2 )
      
      TOL = VTIGHT

      IF (  ABS( C1(1) ) .LT. ABS( C2(1) )  ) THEN

         XVAL1 = C1(1)
         XVAL2 = C2(1)

      ELSE

         XVAL2 = C1(1)
         XVAL1 = C2(1)

      END IF

      CALL CHCKSD ( 'R1', R1, '~/', XVAL1, TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', XVAL2, TOL, OK )

C
C     Make sure the root of larger magnitude is the second
C     one.
C
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )


       
      F  = TOUCHD( A * R2**2  +  B * R2  +  C )

C      WRITE (*,*) 'f(R2): ', F

C      WRITE (*,*) 'C1 = ', C1
C      WRITE (*,*) 'C2 = ', C2

C      WRITE (*,*) 'C1(1) - R1 = ', C1(1) - R1
C      WRITE (*,*) 'C2(1) - R2 = ', C2(1) - R2
      


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation has very large B compared to A; '
     .//       'C has the same scale as B.'
      CALL TCASE ( TITLE )

      A  =   2.D0
      B  =   3.D12
      C  =   4.D12
      UB =   1.D40

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

C
C     Compare R1 and R2 to results from RQUAD:
C
      CALL RQUAD ( A, B, C, C1, C2 )
      
      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', C1(1), TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', C2(1), TOL, OK )


C
C     Make sure the root of larger magnitude is the second
C     one.
C
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Equation has very large C compared to A,B.'

      CALL TCASE ( TITLE )

C
C     This is a case that led to severe loss of precision
C     in ZZBQUAD:
C
      A =  -1.11022302462515654D-016   
      B =   518.74106559784411D0       
      C =  -920626.76641559054D0

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


      F  = TOUCHD( A * R1**2  +  B * R1  +  C )

      TOL = 1.D-6

      CALL CHCKSD ( 'F(R1)', F, '~', 0.D0, TOL, OK )

 
C
C     Compare R1 and R2 to results from RQUAD:
C
      CALL RQUAD ( A, B, C, C1, C2 )
      
      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', C1(1), TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', C2(1), TOL, OK )

C
C     Make sure the root of larger magnitude is the second
C     one.
C
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


