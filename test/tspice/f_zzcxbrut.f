C$Procedure      F_ZZCXBRUT ( Test brute force cone-segment intercept )
 
      SUBROUTINE F_ZZCXBRUT ( OK )
      IMPLICIT NONE

C$ Abstract
C
C     Test the SPICELIB brute force line segment-cone intersection
C     routine ZZCXBRUT.
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
C     This routine tests the routine ZZCXBRUT.
C        
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
C        Previous version 30-SEP-2016 (NJB)
C
C-&
 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

C
C     Local parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

C      DOUBLE PRECISION      TIGHT
C      PARAMETER           ( TIGHT  = 1.D-12 )

C
C     Local Variables
C       
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      LOGICAL               ISBRCK
      

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZCXBRUT' )

 


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is '
     .//           'parallel to X-Y plane. First endpoint ' 
     .//           'is outside cone.'                      )
 
 
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 2.D0, 0.D0, 1.D0, ENDPT1 )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 1.D0, 0.D0, 1.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is '
     .//           'parallel to X-Y plane. First endpoint '
     .//           'is inside cone.'                       )
 
 
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 2.D0, 0.D0, 1.D0, ENDPT2 )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, ENDPT1 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL VPACK ( 1.D0, 0.D0, 1.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is '
     .//           'parallel to Z axis. First endpoint '
     .//           'is outside cone.'                    )
 
 
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 2.D0, 0.D0, 0.D0, ENDPT1 )
      CALL VPACK ( 2.D0, 0.D0, 3.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 2.D0, 0.D0, 2.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is '
     .//           'parallel to Z axis. First endpoint '
     .//           'is inside cone.'                    )
 
 
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 2.D0, 0.D0, 0.D0, ENDPT2 )
      CALL VPACK ( 2.D0, 0.D0, 3.D0, ENDPT1 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 2.D0, 0.D0, 2.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is '
     .//           'parallel to Z axis; first endpoint is far '
     .//           'below X-Y plane.'                          )
 
 
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 2.D0, 0.D0, -1.D10, ENDPT1 )
      CALL VPACK ( 2.D0, 0.D0,  3.D0,  ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 2.D0, 0.D0, 2.D0, XXPT )

      TOL = 1.D-5

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is on the Z axis;'
     .//           'first endpoint is below X-Y plane.' )
 
 
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 0.D0, 0.D0, -1.D0, ENDPT1 )
      CALL VPACK ( 0.D0, 0.D0,  3.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 0.D0, XXPT )

      TOL = VTIGHT

C
C     Use absolute test since the expected result is the
C     zero vector.
C
      CALL CHCKAD ( 'XPT', XPT, '~~', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is '
     .//           'parallel to X-Y plane. First endpoint ' 
     .//           'is outside cone. Angle > pi/2.'         )
 
 
      CALL VPACK ( 0.D0, 0.D0,  1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0,  0.D0, APEX )
      
      ANGLE = 3*PI()/4

      
      CALL VPACK ( 2.D0, 0.D0, -1.D0, ENDPT1 )
      CALL VPACK ( 0.D0, 0.D0, -1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 1.D0, 0.D0, -1.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is +Z; line segment is '
     .//           'parallel to X-Y plane. First endpoint ' 
     .//           'is outside cone. Angle > pi/2. '
     .//           'APEX is non-zero.'                    )
 
 
      CALL VPACK ( 0.D0, 0.D0,  1.D0, AXIS )
      CALL VPACK ( 3.D0, 4.D0,  5.D0, APEX )
      
      ANGLE = 3*PI()/4

      
      CALL VPACK ( 5.D0, 4.D0,  4.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 4.D0,  4.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 4.D0, 4.D0, 4.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AXIS is -Z; line segment is '
     .//           'parallel to X-Y plane. First endpoint ' 
     .//           'is outside cone. Angle > pi/2. '
     .//           'APEX is non-zero.'                    )
 
 
      CALL VPACK ( 0.D0, 0.D0, -1.D0, AXIS )
      CALL VPACK ( 3.D0, 4.D0,  5.D0, APEX )
      
      ANGLE = 3*PI()/4

      
      CALL VPACK ( 5.D0, 4.D0,  6.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 4.D0,  6.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 4.D0, 4.D0, 6.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Root is not bracketed.'  )
 

      CALL VPACK ( 0.D0, 0.D0,  1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0,  0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 2.D0, 0.D0,  1.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 0.D0,  1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .FALSE., OK )




C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error: axis is zero vector.' )


      CALL VPACK ( 0.D0, 0.D0,  0.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0,  0.D0, APEX )
      
      ANGLE = PI()/4

      
      CALL VPACK ( 2.D0, 0.D0,  1.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 0.D0,  1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
