C$Procedure F_ZZELNAXX ( ZZELNAXX tests )
 
      SUBROUTINE F_ZZELNAXX ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZELNAXX.
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
C     This routine tests the private SPICELIB routine ZZELNAXX.
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
C-    TSPICE Version 1.0.0, 19-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      PI

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )
 
C
C     Local Variables
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      M
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XX
      DOUBLE PRECISION      XXPT   
      DOUBLE PRECISION      XY
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      YXPT   
      
C
C     Saved values
C
      SAVE

C
C     Initial values
C
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZELNAXX' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Oblate case, LAT = pi/6' )
 
      A   = 2.D0
      B   = 1.D0
      LAT = PI()/6

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     M is the slope of the normal vector at (x,y).
C     This vector is the gradient of
C
C                   2   2      2
C        f(x,y) =  x / a   +  y / b
C
C
      M = TAN( LAT )

C
C     Let T = (y/x) (the tangent of the latitudinal latitude)
C
      T = (B**2)/(A**2) * M

C
C     Since 
C
C        y = T*x
C
C     we have
C
C         2   2       2 2   2
C        x / a    +  T x / b    =   1
C
C     So
C 
C         2        2     2   2  -1       2    2 2
C        x  = ( 1/a  +  T / b  )  ,     y  = T x
C
C     Both x, y are >= 0.
C
C
C     XX is the expected X-axis intercept, which is
C
C                 2 2
C         ( 1 - (b/a ) ) * x
C     
C
      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

C
C     XY is the expected Y-axis intercept, which is 
C
C                2   2
C        ( 1 - (a / b ) ) y
C

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Oblate case, LAT = pi/3' )
 
      A   = 2.D0
      B   = 1.D0
      LAT = PI()/3

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     See the pi/6 case for a discussion of what follows.
C
      M = TAN( LAT )
C
      T = (B**2)/(A**2) * M

      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Oblate case, LAT << 1' )
 
      A   = 2.D0
      B   = 1.D0
      LAT = 1.D-13

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     See the pi/3 case for a discussion of what follows.
C
      M = TAN( LAT )
C
      T = (B**2)/(A**2) * M

      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Oblate case, LAT = 0' )
 
      A   = 2.D0
      B   = 1.D0
      LAT = 0.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     See the pi/3 case for a discussion of what follows.
C
      M = TAN( LAT )
C
      T = (B**2)/(A**2) * M

      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Oblate case, LAT = pi/2' )
 
      A   = 2.D0
      B   = 1.D0
      LAT = PI()/2

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     See the pi/3 case for a discussion of what follows.
C
      X   = 0.D0

      Y   = B

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )


 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Oblate case, LAT = (pi/2) - EPS, EPS << 1' )
 
      A   = 2.D0
      B   = 1.D0
      EPS = 1.D-13
      LAT = ( PI()/2 ) - EPS

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In principle, tan(LAT) can be very large. We want to
C     work with cot(LAT) and x/y instead. Let T = x/y.
C 
C                     2   2
C        tan(LAT) = (a / b ) * y/x
C
C     so
C 
C              2   2
C        T = (a / b ) * cot(LAT)
C
C
      T = ( (A**2)/(B**2) ) * ( COS(LAT)/SIN(LAT) )

C
C     x = T*y, so
C
C              2        2
C        (Ty/a)  + (y/b)  = 1
C
C     Then
C
C          2      2  2       2  -1
C         y  = ( T /a  +  1/b  ) 
C
C
C
      Y   = SQRT(  1.D0 / ( (T**2/A**2) + (1.D0/B**2) )  )

      X   = T * Y

      XX  = ( 1.D0 - (B**2/A**2) ) * X

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Prolate case, LAT = pi/6' )
 
      A   = 1.D0
      B   = 2.D0
      LAT = PI()/6

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The prolate geometry is just a reflection of an oblate case
C     in which the latitude is complemented and the axis lengths
C     are swapped. The X intercept in the reflected case is the
C     Y intercept of the original case; the relation is the same
C     for the other intercepts.
C     
      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Prolate case, LAT = pi/3' )
 
      A   = 1.D0
      B   = 2.D0
      LAT = PI()/3

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Prolate case, LAT << 1' )


      A   = 1.D0
      B   = 2.D0
      EPS = 1.D-13
      LAT = EPS

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Prolate case, LAT = 0' )


      A   = 1.D0
      B   = 2.D0
      LAT = 0.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Prolate case, LAT = pi/2' )


      A   = 1.D0
      B   = 2.D0
      LAT = PI()/2

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Prolate case, LAT = (pi/2) - EPS, EPS << 1' )


      A   = 1.D0
      B   = 2.D0
      EPS = 1.D-13
      LAT = ( PI()/2 ) - EPS

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )



C***********************************************************************
C
C     Error cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid axis length.' )
      
 
      A   = 0.D0
      B   = 1.D0
      LAT = PI()/2

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )

      A   = -1.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )


      A   = 1.D0
      B   = 0.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )

      B   = -1.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )

C
C ---------------------------------------------------------
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


