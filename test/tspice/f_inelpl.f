C$Procedure F_INELPL ( Ellipse/plane intersection routine tests )
 
      SUBROUTINE F_INELPL ( OK )
 
C$ Abstract
C
C     Exercise INELPL, the SPICELIB routine that slices ellipses
C     with planes.
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
C     This routine tests the SPICELIB routine INELPL.
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
C-    TSPICE Version 2.0.0, 22-FEB-2008 (NJB)
C
C        Added test cases to exercise new exception-handling
C        logic of INELPL. Loosened tolerances in test function
C        T_ISPXEL.
C
C-    TSPICE Version 1.0.0, 15-NOV-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISPXEL

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )
      
      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 2000 )
      
      INTEGER               UBEL
      PARAMETER           ( UBEL   =  9 )
      
      INTEGER               UBPL
      PARAMETER           ( UBPL   =  4 )

C
C     Local Variables
C      
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONST  
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PPT    ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      XPT    ( 3, 2 )
      DOUBLE PRECISION      XPTS   ( 3, 2 )

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               SEED

      LOGICAL               VALXPT   

C
C     Saved values
C

C
C     Initial values
C

C
C     In this test family, we encapsulate some of the geometric
C     tests within functions defined in this file. The function
C     
C        T_ISPXEL
C
C     test whether a set of points constitute the intersection of a
C     specified plane and ellipse.  It does this by testing the points
C     to see whether they are
C   
C        - On the ellipse:  a 2-d point P = (x,y) is "on" the
C          ellipse with semi-axis lengths a, b if 
C          the "level surface parameter" 
C
C             lambda = x**2/a**2 + y**2/b**2
C
C          is sufficiently close to 1.  In three dimensions, the 
C          point must lie in the plane of the ellipse and must 
C          satisfy the above equation when mapped to a basis 
C          spanned by the ellipse's semi-axes.
C
C            
C        - On the plane:  a point P = (x,y,z) is "on" the
C          plane with normal vector N and constant C if
C          the dot product
C
C             < N, P >
C
C          is sufficiently close to C.
C          
C 




C
C     Open the test family.
C
      CALL TOPEN ( 'F_INELPL' )



C
C     Perform a number of simple INELPL tests for which we can compute
C     the intersection points in advance.
C
C --- Case: ------------------------------------------------------
C
C
      TITLE = 'Ellipse and plane are parallel and disjoint.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 2.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

C
C     Create the ellipse.
C
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 1.D0

C
C     Create the plane.
C
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the intersection.
C
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intersection points.
C
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

C
C --- Case: ------------------------------------------------------
C
C
      TITLE = 'Ellipse and plane are orthogonal and disjoint.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 2.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

C
C     Create the ellipse.
C
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, NORMAL )
      CONST = 3.D0

C
C     Create the plane.
C
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the intersection.
C
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intersection points.
C
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C
C
      TITLE = 'Plane contains ellipse.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 2.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

C
C     Create the ellipse.
C
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

C
C     Create the plane.
C
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the intersection.
C
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intersection points.
C
      CALL CHCKSI ( 'N', N, '=', -1, 0, OK )
      

C
C --- Case: ------------------------------------------------------
C
C
      TITLE = 'Plane and ellipse intersect in a single point.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

C
C     Create the ellipse.
C
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, NORMAL )
      CONST = 1.D0

C
C     Create the plane.
C
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the intersection.
C
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intersection points.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
      
C
C     Check the intersection points.
C     
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPT1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )

      CALL CHCKAD ( 'XPT2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
C
      TITLE = 'Plane and ellipse intersect along the line x = 0.5.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

C
C     Create the ellipse.
C
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, NORMAL )
      CONST = 5.D-1

C
C     Create the plane.
C
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the intersection.
C
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of intersection points.
C
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )
      
C
C     Check the intersection points.
C     
      IF ( XPTS(2,1) .LT. 0.D0 ) THEN
         S = -1.D0
      ELSE
         S =  1.D0
      END IF

      CALL VPACK  ( 5.D-1,  S*SQRT(3.D0)/2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPT1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )


      CALL VPACK  ( 5.D-1, -S*SQRT(3.D0)/2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPT2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )



C
C     Now for some more difficult cases.  We'll generate the ellipses
C     and planes using random numbers.  There are four components to
C     generate:
C     
C        - random plane normal vectors
C        - random ellipse axes
C        - random plane constants
C        - random scale factors for the ellipse and plane; these are
C          used to create a wide range of scales
C

      
      SEED = -1

      DO I = 1, NRANDM
C
C --- Case: ------------------------------------------------------
C

C
C        Get a scale factor.
C            
         SFACTR  =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )
            
C
C        Make up ellipse vectors.
C   
         DO J = 1, 3

            CENTER(J) =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )
            V1(J)     =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )
            V2(J)     =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )

         END DO

C
C        Pack the ellipse.
C
         CALL CGV2EL ( CENTER, V1, V2, ELLIPS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the semi-axes of this ellipse.
C
         CALL SAELGV ( V1, V2, SMAJOR, SMINOR )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Generate a random point in the rectangle spanned by
C        the linear combinations
C
C           CENTER + alpha*SMAJOR + beta*SMINOR, 
C
C               -1 <  alpha, beta  <  1
C                  -               -
C
         ALPHA  =  T_RANDD (  -1.D0,  1.D0,  SEED )
         BETA   =  T_RANDD (  -1.D0,  1.D0,  SEED )
       

         CALL VLCOM3 ( 1.D0, CENTER, ALPHA, SMAJOR, BETA, SMINOR, PPT )

C
C        We gotta have a plane normal vector.
C
         NORMAL(1) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(2) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(3) =  T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( NORMAL )

         TITLE = 'INELPL Random case #.  CENTER = (#, #, #); ' //
     .           'V1 = (#, #, #); V2 = (#, #, #); PPT = '      //
     .           '(#, #, #); NORMAL = (#, #, #)'

         CALL REPMI ( TITLE, '#', I,             TITLE )
         CALL REPMD ( TITLE, '#', CENTER(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', CENTER(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', CENTER(3), 14, TITLE )
         CALL REPMD ( TITLE, '#', V1(1),     14, TITLE )
         CALL REPMD ( TITLE, '#', V1(2),     14, TITLE )
         CALL REPMD ( TITLE, '#', V1(3),     14, TITLE )
         CALL REPMD ( TITLE, '#', V2(1),     14, TITLE )
         CALL REPMD ( TITLE, '#', V2(2),     14, TITLE )
         CALL REPMD ( TITLE, '#', V2(3),     14, TITLE )
         CALL REPMD ( TITLE, '#', PPT(1),    14, TITLE )
         CALL REPMD ( TITLE, '#', PPT(2),    14, TITLE )
         CALL REPMD ( TITLE, '#', PPT(3),    14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(3), 14, TITLE )

         CALL TCASE ( TITLE )

C
C        Form the plane.
C
         CALL NVP2PL ( NORMAL, PPT, PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )
                  
C
C        Cross our fingers and toes and let 'er rip.    
C            
         CALL INELPL ( ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )         
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         IF ( N .GT. 0 ) THEN
C
C           Make sure the intersection points we found are
C           contained in both the plane and the the
C           ellipse.
C
            VALXPT  = T_ISPXEL ( ELLIPS, PLANE, N, XPTS, OK )

            CALL CHCKSL ( 'Is intersection valid?', 
     .                     VALXPT, .TRUE., OK       )

         END IF  


      END DO


C
C     INELPL non-error exception cases:
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. '
     .//           'and intersects plane in one point.'        )

C
C     Create the plane.
C
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the ellipse.
C
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, CENTER )
      CALL VPACK  ( 0.D0, 0.D0, 4.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL (ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 1, 0, OK )
C
C     Check intersection points directly (T_ISPXEL isn't
C     designed to work with degenerate ellipses).
C
      CALL VPACK ( 1.D0, 2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPTS 1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )
      CALL CHCKAD ( 'XPTS 2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. '
     .//           'and lies in plane.'                        )

C
C     Create the plane.
C
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the ellipse.
C
C     The ellipse and plane are chosen so that round-off
C     errors should not prevent INELPL from deciding that
C     the ellipse lies in the plane.
C
      CALL VPACK  ( 1.D0, 2.D0, 0.D0, CENTER )
      CALL VPACK  ( 4.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', -1, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. '
     .//           'and is disjoint from, but parallel to, '
     .//           'the plane.'                                )

C
C     Create the plane.
C
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the ellipse.
C
      CALL VPACK  ( 1.D0, 2.D0, -5.D0, CENTER )
      CALL VPACK  ( 4.D0, 0.D0,  0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0,  0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 0, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. '
     .//           'and is disjoint from, and not parallel to, '
     .//           'the plane.'                                )

C
C     Create the plane.
C
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the ellipse.
C
      CALL VPACK  ( 1.D0, 2.D0, -5.D0, CENTER )
      CALL VPACK  ( 4.D0, 0.D0,  1.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0,  0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 0, 0, OK )





C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INELPL: ellipse is a single point '
     .//           'and intersects plane in one point.'        )

C
C     Create the plane.
C
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the ellipse.
C
      CALL VPACK  ( 1.D0, 2.D0, 0.D0, CENTER )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 1, 0, OK )
C
C     Check intersection points directly (T_ISPXEL isn't
C     designed to work with degenerate ellipses).
C
      CALL VPACK ( 1.D0, 2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPTS 1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )
      CALL CHCKAD ( 'XPTS 2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INELPL: ellipse is a single point '
     .//           'and does not intersect plane.'        )

C
C     Create the plane.
C
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the ellipse.
C
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, CENTER )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', 0, '=', 0, 0, OK )

C
C     INELPL error cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'INELPL: invalid plane' )

      CALL CLEARD ( 4, PLANE )

      CALL INELPL ( ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDPLANE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'INELPL: invalid ellipse having '
     .//            'non-orthogonal axes.'           )

C
C     Create the plane.
C
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the ellipse. Note that we can't use
C     CGV2EL for this purpose, since it will create
C     proper, orthogonal semi-axis vectors.
C
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, ELLIPS(1) )
      CALL VPACK  ( 0.D0, 1.D0, 1.D0, ELLIPS(4) )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, ELLIPS(7) )

      CALL INELPL ( ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDELLIPSE)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END





C
C     Supporting function T_ISPXEL
C
      LOGICAL FUNCTION T_ISPXEL ( ELLIPS, PLANE, N, XPTS, OK )
      IMPLICIT NONE
C
C     Test whether a set of points constitute the intersection of a
C     specified plane and ellipse.
C      
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )
      
      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )
      
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      PLANE  ( UBPL )
      INTEGER               N
      DOUBLE PRECISION      XPTS   ( 3, 2 )
      LOGICAL               OK

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

C
C     Local parameters 
C      
      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 1.D-12 )
      
      DOUBLE PRECISION      PLNTOL
      PARAMETER           ( PLNTOL = 1.D-11 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

C
C     Local variables      
C      
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      LEVEL  
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OFFSET ( 3, 2 )
      DOUBLE PRECISION      PC
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      SCALE      
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLX   ( 3, 2 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      XAX    ( 3, 2 )

      INTEGER               I
      

C
C     Give the function an initial return value.
C
      T_ISPXEL = .FALSE.

C
C     Scale ellipse and plane.
C
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A     =  VNORM(SMAJOR)
      B     =  VNORM(SMINOR)
      C     =  VNORM(CENTER)

      CALL PL2NVC ( PLANE, NORMAL, CONST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

C
C     Scale the ellipse and plane constant for safer computation.
C
      SCALE = MAX ( ABS(A), ABS(B), ABS(C), ABS(CONST) )

      CALL VSCLIP ( 1.D0 / SCALE, CENTER )
      CALL VSCLIP ( 1.D0 / SCALE, SMAJOR )
      CALL VSCLIP ( 1.D0 / SCALE, SMINOR )

      SCLA  = A     / SCALE
      SCLB  = B     / SCALE

      CONST = CONST / SCALE

C
C     Scale the input points.
C
      DO I = 1, N
         CALL VSCL (  1.D0 / SCALE, XPTS(1,I), SCLX(1,I) )
      END DO

C
C     Subtract the scaled ellipse center from the intersection
C     points.
C
      DO I = 1, N
         CALL VSUB ( SCLX(1,I), CENTER, OFFSET(1,I) )
      END DO

C
C     Transform the input points into the semi-axis frame.
C
      CALL TWOVEC ( SMAJOR, 1, SMINOR, 2, RMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N
         CALL MXV ( RMAT, OFFSET(1,I), XAX(1,I) )
      END DO
     
C
C     Check the `level' of each point. 
C      
      DO I = 1, N
      
         LEVEL  =    ( XAX(1,I)**2 / SCLA**2 )
     .             + ( XAX(2,I)**2 / SCLB**2 )
     
         TITLE = 'Level of point #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSD ( TITLE, LEVEL, '~', 1.D0, LVLTOL, OK )

         IF  ( .NOT. OK ) THEN
         
            T_ISPXEL = .FALSE.
            RETURN
         
         END IF
     
      END DO
      
C
C     Check each point to see if it's in the plane.
C      
      CALL PL2NVC ( PLANE, NORMAL, CONST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CONST = CONST / SCALE
      
      DO I = 1, N
      
         PC = VDOT ( SCLX(1,I), NORMAL )
         
         TITLE = 'Plane constant of point #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, PC, '~/', CONST, PLNTOL, OK )

         IF ( .NOT. OK  ) THEN
         
            T_ISPXEL = .FALSE.
            RETURN
         
         END IF
         
      END DO
      
      T_ISPXEL = .TRUE.      
      
      RETURN
      END
