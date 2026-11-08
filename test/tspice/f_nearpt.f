C$Procedure F_NEARPT ( NEARPT tests )
 
      SUBROUTINE F_NEARPT ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine NEARPT. NEARPT finds the nearest
C     point on the surface of an ellipsoid to a specified point.
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
C     This routine tests the SPICELIB routine NEARPT.
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
C-    TSPICE Version 2.0.0, 26-JUN-2014 (NJB)
C
C        Removed duplicate test case for input point (1,1,1)
C        and sphere. Added case for input point at center of 
C        sphere. Removed declaration of unused parameter NSIMPL.
C     
C     Last update was 08-SEP-2010 (NJB)
C
C        Added simple test cases, in particular to 
C        check prolate case with on-long-axis view point.
C
C-    TSPICE Version 1.1.0, 24-FEB-2008 (NJB)
C
C        Loosened tolerance for initial NEARPT interior 
C        random cases. Changed ATOL from 1.D-15 to 1.D-14.
C
C-    TSPICE Version 1.0.0, 14-NOV-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISNPPT
      LOGICAL               T_ISEDPT


C
C     Local parameters
C
C
C     SPICELIB ellipse upper bound.
C
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

C
C     Altitude tolerance:  this is applied to the sum of
C     altitude of the view point and the maximum of the 
C     ellipsoid's axis lengths.  The reason the tolerance is not 
C     applied to the altitude directly is that the altitude may have
C     large relative errors, but still be accurate, when the view point
C     is close to the surface.
C
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL   = 1.D-14 )

C
C     Angular separation tolerance:  this applies to the angular
C     separation between the near point-to-view point vector and
C     the properly signed normal vector at the near point.
C
      DOUBLE PRECISION      STOL
      PARAMETER           ( STOL   = 1.D-11 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-9 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 400 )

      INTEGER               NXRAND
      PARAMETER           ( NXRAND = 4000 )

C
C     Local Variables
C      
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      GV1    ( 3 )
      DOUBLE PRECISION      GV2    ( 3 )
      DOUBLE PRECISION      IPALT
      DOUBLE PRECISION      IPNEAR ( 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SRFX   ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               PINDEX
      INTEGER               SEED

      LOGICAL               FOUND
      LOGICAL               ISNEAR
      LOGICAL               VALPT  

C
C     Saved values
C
      SAVE                  ORIGIN
      
C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /



 

C
C     In this test family, we encapsulate some of the geometric
C     tests within functions defined in this file. The function
C     
C        T_ISEDPT
C
C     tests a point is on a specified ellipsoid:
C
C        - A point P = (x,y,z) is "on" the ellipsoid with semi-axis
C          lengths a, b, c if the "level surface parameter"
C
C             lambda = x**2/a**2 + y**2/b**2 + z**2/c**2 
C
C          is sufficiently close to 1.
C          
C
C     The function
C     
C        T_ISNPPT 
C
C     test whether the near point found by NEARPT has the property
C     that the outward normal at the near point can be extended to
C     touch the input point.  
C
C     T_ISNPPT also checks the distance of the near point from the
C     point: the distance is checked against the value obtained from
C     NPLNPT.

C




C
C     Open the test family.
C
      CALL TOPEN ( 'F_NEARPT' )


C
C     NEARPT simple cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid is sphere; point is at origin.' )

      CALL CLEARD ( 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D0, ATOL, OK )

      CALL CHCKSD ( 'VNORM(PNEAR)', VNORM(PNEAR), '~', 1.D0, ATOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid is sphere; point is exterior '
     .//            'on +Z axis.'                                   )

      CALL VPACK ( 0.D0, 0.D0, 5.D0, VIEWPT )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XPT    )


      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', XPT, 3, ATOL, OK )

      CALL CHCKSD ( 'ALT', ALT, '~', 4.D0, ATOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid is sphere; point is interior '
     .//            'on ray generated by (1,1,1)'                    )

      S = 1/SQRT(3.D0)

      CALL VPACK ( S/2.D0, S/2.D0, S/2.D0, VIEWPT )


      CALL VPACK ( S, S, S, XPT    )


      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', XPT, 3, ATOL, OK )

      CALL CHCKSD ( 'ALT',   ALT,   '~',  -0.5D0, ATOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid is triaxial; point is on ' 
     .//            'selected outward normal.'                   )
C
C     Start with the unit sphere and a point on the ray (1,1,1);
C     multiply all participants by diag[3,2,1].
C
      A = 3.D0
      B = 2.D0
      C = 1.D0
      
      S = 1/SQRT(3.D0)

      CALL VPACK ( A*S, B*S, C*S, SRFX )

C
C     Create the outward unit normal at SRFX.
C
      CALL VPACK  ( SRFX(1)/A**2, SRFX(2)/B**2, SRFX(3)/C**2, NORMAL )

      CALL VHATIP ( NORMAL )

C
C     Create a view point at altitude 1 along the normal.
C
      CALL VADD   ( SRFX, NORMAL, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', 1.D0, ATOL, OK )

C
C     The near point should be the surface point at which the
C     normal was generated.
C
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', SRFX, 3, ATOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid is triaxial; point is on ' 
     .//            'selected inward normal.'                   )
C
C     Start with the unit sphere and a point on the ray (1,1,1);
C     multiply all participants by diag[3,2,1].
C
      A = 3.D0
      B = 2.D0
      C = 1.D0
      
      S = 1/SQRT(3.D0)

      CALL VPACK ( A*S, B*S, C*S, SRFX )

C
C     Create the outward unit normal at SRFX.
C
      CALL VPACK  ( SRFX(1)/A**2, SRFX(2)/B**2, SRFX(3)/C**2, NORMAL )

      CALL VHATIP ( NORMAL )

C
C     Create a view point at altitude 1 along the normal.
C
      CALL VLCOM   ( 1.D0, SRFX, -1.D-1, NORMAL, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D-1, ATOL, OK )

C
C     The near point should be the surface point at which the
C     normal was generated.
C
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', SRFX, 3, ATOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid is bi-axial; point is at ' //
     .              'origin.'                                     )

      CALL CLEARD ( 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 2.D0, 2.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D0, ATOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid is tri-axial; point is at ' //
     .              'origin.'                                     )

      CALL CLEARD ( 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 3.D0, 2.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D0, ATOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: interior prolate case. Viewpoint is '
     .//            'on the long axis; solution is off-axis.'      )

      
      A = 10.D0
      B =  5.D0
      C =  5.D0

      CALL VPACK  ( 7.D0, 0.D0, 0.D0, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the solution is on the ellipsoid.
C
      VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

      CALL CHCKSL ( 'Is on ellipsoid', VALPT, .TRUE., OK )

C
C     Make sure the solution satisfies the orthogonality 
C     condition.
C
      ISNEAR =  T_ISNPPT ( VIEWPT, A,   B,    C, 
     .                     PNEAR,  ALT, ATOL, STOL, OK )

      CALL CHCKSL ( 'Is critical point', ISNEAR, .TRUE., OK )

C
C     Make sure altitude is greater than -2.95.
C
      CALL CHCKSD ( 'ALT', ALT, '>', -2.95D0, ATOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: interior prolate case. Repeat previous '
     .//            'case with point closer to the origin.'          )

      
      A = 10.D0
      B =  5.D0
      C =  5.D0

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the solution is on the ellipsoid.
C
      VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

      CALL CHCKSL ( 'Is on ellipsoid', VALPT, .TRUE., OK )

C
C     Make sure the solution satisfies the orthogonality 
C     condition.
C
      ISNEAR =  T_ISNPPT ( VIEWPT, A,   B,    C, 
     .                     PNEAR,  ALT, ATOL, STOL, OK )

      CALL CHCKSL ( 'Is critical point', ISNEAR, .TRUE., OK )

C
C     Make sure altitude is greater than -5.
C
      CALL CHCKSD ( 'ALT', ALT, '>', -5.D0, ATOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: interior prolate case. Repeat previous '
     .//            'case with point near axis endpoint.'           )

      
      A = 10.D0
      B =  5.D0
      C =  5.D0

      CALL VPACK  ( 9.D0, 0.D0, 0.D0, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the solution is on the ellipsoid.
C
      VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

      CALL CHCKSL ( 'Is on ellipsoid', VALPT, .TRUE., OK )

C
C     Make sure the solution satisfies the orthogonality 
C     condition.
C
      ISNEAR =  T_ISNPPT ( VIEWPT, A,   B,    C, 
     .                     PNEAR,  ALT, ATOL, STOL, OK )

      CALL CHCKSL ( 'Is critical point', ISNEAR, .TRUE., OK )

C
C     Make sure near point is axis endpoint.
C
      CALL CHCKSD ( 'ALT', ALT, '>', -1.D0, ATOL, OK )

      CALL VPACK  ( A, 0.D0, 0.D0, XPT )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', XPT, 3, ATOL, OK )
 



C
C --- Case: ------------------------------------------------------
C



C
C     First set of random test cases:  random ellipsoids and random
C     *exterior* viewing points.
C      
      SEED = -1

      DO I = 1, NXRAND
C
C --- Case: ------------------------------------------------------
C            
C
C        Make up ellipsoid axis lengths.
C            
         A =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         B =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         C =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
            
C
C        Create viewing point.
C
         VIEWPT(1) = 10.D0 ** T_RANDD ( -3.D0,  100.D0, SEED )  
         VIEWPT(2) = 10.D0 ** T_RANDD ( -3.D0,  100.D0, SEED ) 
         VIEWPT(3) = 10.D0 ** T_RANDD ( -3.D0,  100.D0, SEED ) 

C
C        Find the level surface parameter of the view point.
C
         LEVEL =    (VIEWPT(1)/A)**2.D0 
     .            + (VIEWPT(2)/B)**2.D0 
     .            + (VIEWPT(3)/C)**2.D0

C
C        If the viewing point is inside the ellipsoid, replace it
C        with the surface point found by scaling up the viewing
C        point.
C        
         IF ( LEVEL .LT. 1.D0 ) THEN
C
C           FOUND will always be .TRUE. in this case.
C
            CALL SURFPT ( ORIGIN, VIEWPT, A, B, C, SRFX, FOUND )
C
C           Scale the point up a tad for safety.
C
            CALL VSCL ( (1.D0 + 1.D-15),   SRFX,   VIEWPT )

         END IF


         TITLE = 'Initial NEARPT exterior random case #. '   //
     .           'For this set of cases, we don''t use '     //
     .           'extreme scale differences for the axis '   //
     .           'lengths:  range is [0.1 : 10]. VIEWPT '    //
     .           'components are in range [0.001 : 1.e150]. '//
     .           'A = #; B = #; C = #; VIEWPT = (#,#,#).'

         CALL REPMI ( TITLE,  '#', I,             TITLE )
         CALL REPMD ( TITLE,  '#', A,         14, TITLE )
         CALL REPMD ( TITLE,  '#', B,         14, TITLE )
         CALL REPMD ( TITLE,  '#', C,         14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(1), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(2), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(3), 14, TITLE )

         CALL TCASE ( TITLE )

C
C        Cross our fingers and toes and let 'er rip.    
C            
         CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )
      

         IF ( OK ) THEN
C
C           Make sure the near point we found belongs to
C           the surface of the ellipsoid.
C
            VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

            CALL CHCKSL ( 'Is near point on surface?', 
     .                    VALPT, .TRUE., OK           )

C
C           The outward normal at the near point should point towards
C           the view point.
C
            ISNEAR = T_ISNPPT ( VIEWPT, A,   B,    C,  
     .                          PNEAR,  ALT, ATOL, STOL, OK  )

            CALL CHCKSL ( 'Is signed outward normal parallel to  ' //
     .                    'near point-view point vector?  Is '     //
     .                    'distance correct?',
     .                    ISNEAR, .TRUE., OK )

         END IF

      END DO




C
C     Second set of random test cases:  random ellipsoids and random
C     *interior* viewing points.  At this point, we don't verify
C     that the solutions are absolute minima, but we do verify 
C     that they are critical points.
C
      SEED = -1

      DO I = 1, NXRAND
C
C --- Case: ------------------------------------------------------
C
C
C        Make up ellipsoid axis lengths.
C            
         A =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         B =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         C =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
            
C
C        Create viewing point.
C
         VIEWPT(1) = T_RANDD ( -1.D0,  1.D0, SEED )  
         VIEWPT(2) = T_RANDD ( -1.D0,  1.D0, SEED ) 
         VIEWPT(3) = T_RANDD ( -1.D0,  1.D0, SEED ) 

C
C        Find the level surface parameter of the view point.
C
         LEVEL =    (VIEWPT(1)/A)**2.D0 
     .            + (VIEWPT(2)/B)**2.D0 
     .            + (VIEWPT(3)/C)**2.D0

C
C        If the viewing point is outside the ellipsoid, replace it
C        with an interior point found by scaling down the surface
C        point corresponding to the viewing point.
C        
         IF ( LEVEL .GE. 1.D0 ) THEN
C
C           FOUND will always be .TRUE. in this case.
C
            CALL SURFPT ( ORIGIN, VIEWPT, A, B, C, SRFX, FOUND )
C
C           Scale the point down to make it usable.
C
            SFACTR = T_RANDD ( 0.D0,  1.D0-1.D-6, SEED ) 

            CALL VSCL ( SFACTR, SRFX, VIEWPT )

         END IF


         TITLE = 'Initial NEARPT interior random case #. '   //
     .           'For this set of cases, we don''t use '     //
     .           'extreme scale differences for the axis '   //
     .           'lengths:  range is [0.1 : 10]. VIEWPT '    //
     .           'components are in range [0.001 : 1.e150]. '//
     .           'A = #; B = #; C = #; VIEWPT = (#,#,#).'

         CALL REPMI ( TITLE,  '#', I,             TITLE )
         CALL REPMD ( TITLE,  '#', A,         14, TITLE )
         CALL REPMD ( TITLE,  '#', B,         14, TITLE )
         CALL REPMD ( TITLE,  '#', C,         14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(1), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(2), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(3), 14, TITLE )

         CALL TCASE ( TITLE )

C
C        Cross our fingers and toes and let 'er rip.    
C            
         CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C        Make sure the near point we found is contained in
C        both the plane and the surface of the ellipsoid.
C
         IF ( OK ) THEN
C
C           Make sure the near point we found belongs to
C           the surface of the ellipsoid.
C
            VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

            CALL CHCKSL ( 'Is near point on surface?', 
     .                    VALPT, .TRUE., OK           )

C
C           The inward normal at the near point should point towards
C           the view point.
C
            ISNEAR = T_ISNPPT ( VIEWPT, A,   B,    C,  
     .                          PNEAR,  ALT, ATOL, STOL, OK  )

            CALL CHCKSL ( 'Is signed normal parallel to  ' //
     .                    'near point-view point vector?  Is '     //
     .                    'distance correct?',
     .                    ISNEAR, .TRUE., OK )

         END IF

      END DO



C
C     Third set of random test cases:  random ellipsoids and random
C     viewing points lying on principal planes. 
C
      SEED = -1

      DO I = 1, NXRAND
C
C --- Case: ------------------------------------------------------
C
C
C        Make up ellipsoid axis lengths.
C            
         A =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         B =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         C =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )

C
C        Do a few tests to check for problems in the special
C        cases where the axes are not distinct.
C
         IF ( I .LT. 200 ) THEN
C
C           Turn the ellipsoid into a sphere.
C
            B = A
            C = A
            
         ELSE IF ( I .LT. 400 ) THEN
C
C           The ellipsoid is symmetric about X.
C           
            B = C

         ELSE IF ( I .LT. 600 ) THEN
C
C           The ellipsoid is symmetric about Y.
C           
            C = A

         ELSE IF ( I .LT. 800 ) THEN
C
C           The ellipsoid is symmetric about Z.
C           
            B = A

         END IF

C
C        Create viewing point.  We pick a plane index, a scale factor
C        to give us the radius of the point, and an angular argument.
C
         PINDEX = NINT (  T_RANDD ( 1.D0,  3.D0, SEED )  )
         SCALE  =         T_RANDD ( 0.D0,  1.D0, SEED )
         THETA  =         T_RANDD ( -PI(), PI(), SEED )


         IF ( PINDEX .EQ. 1 ) THEN
C
C           The point lies in the y-z plane.
C
            VIEWPT(1) = 0.D0
            VIEWPT(2) = SCALE * B * COS(THETA)
            VIEWPT(3) = SCALE * C * SIN(THETA)

C
C           In addition to creating a viewing point, create generating
C           vectors for the intersection ellipse defined by the
C           ellipsoid and the y-z plane.  We'll use this ellipse later.
C
            CALL VPACK  ( 0.D0, B,    0.D0, GV1 )
            CALL VPACK  ( 0.D0, 0.D0, C,    GV2 )
           

         ELSE IF ( PINDEX .EQ. 2 ) THEN
C
C           The point lies in the x-z plane.
C
            VIEWPT(1) = SCALE * A * COS(THETA)
            VIEWPT(2) = 0.D0
            VIEWPT(3) = SCALE * C * SIN(THETA)

            CALL VPACK  ( A,    0.D0, 0.D0, GV1 )
            CALL VPACK  ( 0.D0, 0.D0, C,    GV2 )

         ELSE
C
C           The point lies in the x-Y plane.
C
            VIEWPT(1) = SCALE * A * COS(THETA)
            VIEWPT(2) = SCALE * B * SIN(THETA)
            VIEWPT(3) = 0.D0

            CALL VPACK  ( A,    0.D0, 0.D0, GV1 )
            CALL VPACK  ( 0.D0, B,    0.D0, GV2 )

         END IF

C
C        Create the ellipse from the generating vectors.
C
         CALL CGV2EL ( ORIGIN, GV1, GV2, ELLIPS )


C
C        If the viewing point is outside the ellipsoid, replace it
C        with an interior point found by scaling down the surface
C        point corresponding to the viewing point.
C        
C        Find the level surface parameter of the view point.
C
         LEVEL =    (VIEWPT(1)/A)**2.D0 
     .            + (VIEWPT(2)/B)**2.D0 
     .            + (VIEWPT(3)/C)**2.D0

         IF ( LEVEL .GE. 1.D0 ) THEN

            SFACTR = 1.D0 - 1.D-6

            CALL VSCLIP ( SFACTR, VIEWPT )
            
         END IF


         TITLE = 'NEARPT interior principal plane random '      //
     .           'case #.  For this set of cases, we don''t '   //
     .           'use extreme scale differences for the axis '  //
     .           'lengths:  range is [0.1 : 10]. VIEWPT '       //
     .           'components are in range [0.001 : 1.e150]. '   //
     .           'A = #; B = #; C = #; VIEWPT = (#,#,#).'

         CALL REPMI ( TITLE,  '#', I,             TITLE )
         CALL REPMD ( TITLE,  '#', A,         14, TITLE )
         CALL REPMD ( TITLE,  '#', B,         14, TITLE )
         CALL REPMD ( TITLE,  '#', C,         14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(1), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(2), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(3), 14, TITLE )

         CALL TCASE ( TITLE )

C
C        Cross our fingers and toes and let 'er rip.    
C            
         CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C        Make sure the near point we found is contained in
C        both the plane and the surface of the ellipsoid.
C
         IF ( OK ) THEN
C
C           Make sure the near point we found belongs to
C           the surface of the ellipsoid.
C
            VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

            CALL CHCKSL ( 'Is near point on surface?', 
     .                    VALPT, .TRUE., OK           )

C
C           The inward normal at the near point should point towards
C           the view point.  For these cases we use a more lenient
C           tolerance for the angular separation.  We also use a 
C           slightly looser altitude tolerance.
C
            ISNEAR = T_ISNPPT ( VIEWPT, A,   B,     C,  
     .                          PNEAR,  ALT, TIGHT, MEDIUM, OK  )

            CALL CHCKSL ( 'Is signed normal parallel to  ' //
     .                    'near point-view point vector?  Is '     //
     .                    'distance correct?',
     .                    ISNEAR, .TRUE., OK )

            IF ( PNEAR(PINDEX) .NE. 0.D0 ) THEN
C
C              We have a solution that doesn't lie in the same plane
C              as the viewing point.  This may well be correct, but
C              we'll check it.  Find the in-plane solution and 
C              compare altitudes.
C
               CALL NPELPT ( VIEWPT, ELLIPS, IPNEAR, IPALT )

C
C              We expect the in-plane "altitude" to be greater than
C              or equal to that of the out-of-plane near point.
C
               CALL CHCKSD ( 'IPALT', IPALT, '>=', ALT, 0.D0, OK )

C               WRITE (*,*) 'IPALT - ALT = ', IPALT - ALT

C               WRITE (*,*) 'Out-of-plane solution'
C               WRITE (*,*) 'view point = ', VIEWPT
C               WRITE (*,*) 'near point = ', PNEAR
C               WRITE (*,*) 'altitude   = ', ALT
C               WRITE (*,*) '======================='

            END IF

         END IF

      END DO







C
C     NEARPT error cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid has one zero-length axis.' )

      CALL FILLD (   1.D1, 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 0.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT, 1.D0, 0.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 0.D0, PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ellipsoid has one negative-length axis.' )

      CALL NEARPT ( VIEWPT, -1.D0, 1.D0,   1.D0,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  1.D0, -1.D0,  1.D0,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  1.D0,  1.D0, -1.D0,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: ratio of longest to shortest axes ' //
     .              'is too large.'                              )

      A = 1.D-6
      B = 1.D150
      C = 1.D0

      CALL NEARPT ( VIEWPT,  A, B, C,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  C, A, B,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  B, C, A,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NEARPT: product of scaled axis magnitude ' //
     .              'and scaled POSITN component is too large.' )

      A = 1.D-6
      B = 1.D75
      C = 1.D0

      CALL VPACK  ( A, B**3.D0, C, VIEWPT )
      CALL NEARPT ( VIEWPT,  A, B, C,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INPUTSTOOLARGE)', OK )

      CALL VPACK  ( C, A, B**3.D0, VIEWPT ) 
      CALL NEARPT ( VIEWPT,  C, A, B,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INPUTSTOOLARGE)', OK )

      CALL VPACK  ( B**3.D0, C, A, VIEWPT ) 
      CALL NEARPT ( VIEWPT,  B, C, A,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INPUTSTOOLARGE)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END





C
C     The utility function T_ISNPPT tests whether the "near point"
C     found by NEARPT satisfies the criterion that the appropriately
C     signed surface normal at the near point has small angular
C     separation from the vector from the near point to view point.
C
      LOGICAL FUNCTION T_ISNPPT ( V, A, B, C, X, ALT, ATOL, STOL, OK )
      IMPLICIT NONE

      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      ATOL
      DOUBLE PRECISION      STOL
      LOGICAL               OK 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

C
C     Local parameters
C

C
C     Local variables
C
      DOUBLE PRECISION      ALTSGN
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      MAXAX
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PVEC   ( 3 ) 
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      XALT
    
C
C     Executable code.
C

C
C     Find the level surface parameter of the view point.
C
      LEVEL = (V(1)/A)**2.D0 + (V(2)/B)**2.D0 + (V(3)/C)**2.D0

C
C     Set the sign of the altitude of the near point.
C
      IF ( LEVEL .GE. 1.D0 ) THEN
         ALTSGN =  1.D0
      ELSE
         ALTSGN = -1.D0
      END IF

C
C     Find the vector from the near point to the view point.
C
      CALL VSUB ( V, X, PVEC )

C
C     Checking the altitude can be problematic, since small altitudes
C     can be accurate but have a large relative error.  Instead,
C     check the relative error in the sum of the altitude and
C     the maximum axis.
C
      MAXAX = MAX ( A, B, C )

      XALT  = MAXAX + ALTSGN * VNORM( PVEC )

      CALL CHCKSD ( 'MAXAX+ALT', 
     .               MAXAX+ALT,   '~/',  XALT, ATOL,   OK )

      IF ( .NOT. OK ) THEN

         T_ISNPPT = .FALSE.
         RETURN

      END IF

C
C     Obtain outward ellipsoid surface unit normal at the near point.
C     
      CALL SURFNM ( A, B, C, X, NORMAL )

C
C     Adjust sign of normal vector according to whether view point is
C     inside or outside the ellipsoid.
C
      CALL VSCLIP ( ALTSGN, NORMAL )

C
C     Find angular separation of sign-adjusted normal and PVEC.
C
      SEP = VSEP ( NORMAL, PVEC )

C
C     Check the angular separation.
C
      CALL CHCKSD ( 'SEP', SEP, '~', 0.D0 , STOL, OK )

      T_ISNPPT = OK 

      RETURN
      END




C
C     Supporting function T_ISEDPT
C
C     This function is defined in the test family F_NPEDLN.
C     The calling sequence is
C
C        LOGICAL FUNCTION T_ISEDPT ( PT, A, B, C, OK )
C
C
C        Test whether a point is on a specified ellipsoid.
C      
C        DOUBLE PRECISION      PT ( 3 )
C        DOUBLE PRECISION      A
C        DOUBLE PRECISION      B
C        DOUBLE PRECISION      C
C        LOGICAL               OK
C
