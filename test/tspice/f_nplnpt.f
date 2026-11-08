C$Procedure F_NPLNPT ( NPLNPT tests )
 
      SUBROUTINE F_NPLNPT ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine NPLNPT.  NPLNPT find the nearest
C     point on a specified line to a specified point.
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
C$ Files
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
C$ Particulars
C
C     This routine tests the SPICELIB routine NPLNPT.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 13-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDIST

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )
      
      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 1000 )
      

C
C     Local Variables
C      
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      DIST  
      DOUBLE PRECISION      LINEDR ( 3 )
      DOUBLE PRECISION      LINEPT ( 3 )
      DOUBLE PRECISION      LTRANS ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PTRANS ( 3 )
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XTRANS ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               SEED


C
C     Saved values
C

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_NPLNPT' )


C
C     Run some simple tests where the correct results can be 
C     determined by inspection.
C
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'NPLNPT simple case:  line is x = y; z = 0.  POINT ' //  
     .        'is  (-1, 0, 0 ).'
      CALL TCASE ( TITLE )


      CALL VPACK (  1.D0,  1.D0, 0.D0, LINEDR )
      CALL VPACK (  0.D0,  0.D0, 0.D0, LINEPT )
      CALL VPACK ( -1.D0,  0.D0, 0.D0, POINT  )

      A = SQRT(2.D0)/2

      CALL VPACK ( -0.5D0, -0.5D0, 0.D0, XPT  )
      
      CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

C
C     Test the near point and distance.
C
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT, 3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   A,      TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'NPLNPT non-error exception case:  POINT ' //  
     .        'is on the line.'
      CALL TCASE ( TITLE )


      CALL VPACK (  1.D0,  1.D0, 0.D0, LINEDR )
      CALL VPACK (  0.D0,  1.D0, 0.D0, LINEPT )
      CALL VPACK ( -1.D0,  0.D0, 0.D0, POINT  )

      CALL VPACK ( -1.D0, 0.D0, 0.D0, XPT  )
      
      CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

C
C     Test the near point and distance.
C
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT,   3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   0.D0,     TIGHT, OK )


C
C     Now for some more difficult cases.  We'll generate the points
C     and lines using random numbers.  There are ten components to
C     generate:
C     
C        - random line direction vectors
C        - random line points
C        - random points off the line
C        - random scale factors for the line and point; these are
C          used to create a wide range of scales
C
      
      SEED = -1

      DO I = 1, NRANDM
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'NPLNPT random case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )

C
C        Get a scale factor.
C            
         SFACTR    =  10.D0 ** T_RANDD ( -306.D0, 306.D0, SEED )
            
C
C        We gotta have a line direction vector. 
C
         LINEDR(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( LINEDR )
            
C
C        We need a point on the line.  
C
         LINEPT(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

C
C        We also need a point off the line.  Scale the point up to
C        increase the likelihood of a non-intercept case.
C
         POINT(1)  =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(2)  =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(3)  =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

C
C        Find the expected near point.  First define a frame whose
C        x-axis is parallel to LINEDR.  Then transform POINT
C        into this frame. Also transform LINEPT into the same frame. 
C
         CALL VEQU   ( LINEDR, X )

         CALL FRAME  ( X, Y, Z )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3
            TRANS(1,J) = X(J)
            TRANS(2,J) = Y(J)
            TRANS(3,J) = Z(J)
         END DO

         CALL MXV   ( TRANS, POINT,     PTRANS )
         CALL MXV   ( TRANS, LINEPT,    LTRANS )

C
C        The y-z projection of the resulting point is the closest point
C        on the line to the origin. The expected near point has the
C        same x-component as PTRANS.
C
         CALL VPACK ( PTRANS(1), LTRANS(2), LTRANS(3),  XTRANS )

C
C        Map XTRANS back to the base frame.
C
         CALL MTXV ( TRANS, XTRANS, XPT )

C
C        The distance between XPT and POINT is the expected
C        distance from the near point.
C
         XDIST = VDIST ( XPT, POINT )

C
C        Cross our fingers and toes and let 'er rip.    
C            
         CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Test the near point and distance.
C
         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/',  XPT,  3, TIGHT, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',   XDIST,   TIGHT, OK )

      END DO


C
C     NPLNPT error cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NPLNPT: zero direction vector,' )

      CALL FILLD  ( 1.D1, 3,  LINEPT )
      CALL CLEARD (       3,  LINEDR )

      CALL VPACK  ( 1.D0, 1.D0, 1.D0, POINT )

      CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END



