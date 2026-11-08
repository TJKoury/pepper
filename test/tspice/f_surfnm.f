C$Procedure F_SURFNM ( SURFNM tests )
 
      SUBROUTINE F_SURFNM ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine SURFNM.  SURFNM find the unit
C     outward normal at a specfied point on an ellipsoid.
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
C     This routine tests the SPICELIB routine SURFNM.
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
C-    TSPICE Version 1.0.0, 21-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C

C
C     Other functions
C

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

     
C
C     Local Variables
C      
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      SRFPT  ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      XNORML ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y

      LOGICAL               FOUND

C
C     Saved values
C
      SAVE                  ORIGIN

C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_SURFNM' )


C
C     Run some simple tests where the correct results can be 
C     determined by inspection.
C
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'SURFNM simple case:  point is at top of unit sphere'
      CALL TCASE  ( TITLE )

      A = 1.D0
      B = 1.D0
      C = 1.D0

      CALL VPACK ( 0.D0, 0.D0, 1.D0, SRFPT  )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'SURFNM simple case:  point is (sqrt(3)/3,sqrt(3)/3,' //
     .        'sqrt(3)/3) on unit sphere'
      CALL TCASE  ( TITLE )

      A = 1.D0
      B = 1.D0
      C = 1.D0

      X = SQRT( 3.D0 ) / 3.D0

      CALL VPACK ( X, X, X, SRFPT  )
      CALL VPACK ( X, X, X, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'SURFNM simple case: repeat previous case, but scale ' //
     .        'down sphere by 1.D-200'
      CALL TCASE  ( TITLE )

      SCALE = 1.D-200

      A = SCALE
      B = SCALE
      C = SCALE

      X = SQRT( 3.D0 ) / 3.D0

      Y = X * SCALE

      CALL VPACK ( Y, Y, Y, SRFPT  )
      CALL VPACK ( X, X, X, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'SURFNM simple case: repeat previous case, but scale ' //
     .        'up sphere by 1.D200'
      CALL TCASE  ( TITLE )

      SCALE = 1.D200

      A = SCALE
      B = SCALE
      C = SCALE

      X = SQRT( 3.D0 ) / 3.D0

      Y = X * SCALE

      CALL VPACK ( Y, Y, Y, SRFPT  )
      CALL VPACK ( X, X, X, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Triaxial case: axis lengths are 1, 2, 3.  Point ' //
     .        'lies along direction vector (1, 0.3, 0.5)'

      CALL TCASE  ( TITLE )

      A  =  1.D0
      B  =  2.D0
      C  =  3.D0

      CALL VPACK ( 1.D0, 0.3D0, 0.5D0, U )
  
C
C     Find the surface point on the specified line.
C    
      CALL SURFPT ( ORIGIN, U, A, B, C, SRFPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compute the normal direction; unitize the expected normal vector.
C
      CALL VPACK ( SRFPT(1)/A**2, SRFPT(2)/B**2, SRFPT(3)/C**2, XNORML )

      CALL VHATIP ( XNORML )

C
C     See whether SURFNM agrees.
C
      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )




C
C     SURFNM error cases:
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'INEDPL: ellipsoid has one zero-length axis.' )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, SRFPT )

      CALL SURFNM (  0.D0, 1.D0,   1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0,  0.D0,  1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0,  1.D0,  0.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SURFNM: ellipsoid has one negative-length axis.' )

      CALL SURFNM ( -1.D0, 1.D0,   1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0, -1.D0,  1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0,  1.D0, -1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


