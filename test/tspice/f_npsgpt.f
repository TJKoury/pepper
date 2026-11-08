C$Procedure F_NPSGPT ( NPSGPT tests )
 
      SUBROUTINE F_NPSGPT ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine NPSGPT.
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
C     This routine tests the SPICELIB routine NPSGPT.
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
C-    TSPICE Version 1.0.0, 18-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )
 

      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 160 )

C
C     Local Variables
C
      CHARACTER*(TITLEN)    TITLE

      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      EP1    ( 3 )
      DOUBLE PRECISION      EP2    ( 3 )
      DOUBLE PRECISION      MAXSCL
      DOUBLE PRECISION      MINSCL
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      PERP   ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SMALL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XPNEAR ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )
            
      INTEGER               I
      INTEGER               NSCALE


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
      CALL TOPEN ( 'F_NPSGPT' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. '
     .//           'Point has negative X.'                           )
 

      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK ( -1.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP1, XPNEAR )

      XDIST = 2.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. '
     .//           'Point has X = 1.'                               )
 

      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK (  1.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP1, XPNEAR )

      XDIST = 0.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. '
     .//           'Point has X = 2.'                               )
 

      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK (  2.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP2, XPNEAR )

      XDIST = 0.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. '
     .//           'Point has X > 2.'                               )  
 

      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK (  3.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP2, XPNEAR )

      XDIST = 1.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )


C
C --- Case: ------------------------------------------------------
C


C
C     Now try some more general cases.
C
      MINSCL = -100.D0
      MAXSCL =  100.D0
      NSCALE =  30
      DSCALE =  ( MAXSCL - MINSCL ) / NSCALE


      DO I = 0, NSCALE
C
C        Generate endpoints of the segment.
C
         SCALE = 10.D0 ** ( MINSCL + (I*DSCALE) )

         CALL VPACK ( 1.D0,   3.D0, -7.D0,  EP1 )
         CALL VPACK ( 5.D0, -13.D0, -0.5D0, EP2 )

         CALL VSCLIP ( SCALE, EP1 )
         CALL VSCLIP ( SCALE, EP2 )

C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Point projection is off EP1 end; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

C
C        Let DIR be the vector from EP1 to EP2. 
C        
         CALL VSUB ( EP2, EP1, DIR  )
         CALL VHAT ( DIR,      UDIR )
         CALL VEQU ( UDIR,     X    )

C
C        Get some vectors orthogonal to DIR; generate
C        a perpendicular component vector for P.
C
         CALL FRAME ( X, Y, Z )
         CALL CHCKXC( .FALSE., ' ', OK )
         
         CALL VLCOM ( 2.D0*SCALE, Y, 5.D0*SCALE, Z, PERP )

C
C        Generate an input point.
C
         SMALL = 1.D-13

         CALL VLCOM3 ( 1.D0, EP1, -SMALL, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP1, XPNEAR )

         XDIST = VDIST ( P, EP1 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Point projection is EP1; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

C
C        Generate an input point.
C
         CALL VLCOM3 ( 1.D0, EP1, 0.0D0, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP1, XPNEAR )

         XDIST = VDIST ( P, EP1 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Point projection is between EP1 and EP2; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

C
C        Generate an input point.
C
         CALL VLCOM3 ( 0.5D0, EP1, 0.5D0, EP2, 1.D0, PERP, P )
 
         TOL = VTIGHT

         CALL VLCOM ( 0.5D0, EP1, 0.5D0, EP2, XPNEAR )

         XDIST = VNORM( PERP )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Point projection is EP2; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

         
C
C        Generate an input point.
C
         CALL VLCOM3 ( 1.D0, EP2, 0.0D0, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP2, XPNEAR )

         XDIST = VDIST ( P, EP2 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Point projection is off EP2 end; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )
 
C
C        Generate an input point.
C
         SMALL = 1.D-13

         CALL VLCOM3 ( 1.D0, EP2, SMALL, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP2, XPNEAR )

         XDIST = VDIST ( P, EP2 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )

      END DO


C*********************************************************************
C*
C*    Non-error exceptional case
C*
C*********************************************************************

      CALL TCASE ( 'EP1 == EP2' )

      
      CALL VLCOM ( 1.D0, EP2, 1.D0, DIR, P )

      CALL NPSGPT ( EP1, EP1, P, PNEAR, DIST )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VEQU ( EP1, XPNEAR )

      XDIST = VDIST ( P, EP1 )

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )




C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C     None.
C


C
C ---------------------------------------------------------
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


