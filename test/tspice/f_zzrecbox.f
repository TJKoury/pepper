C$Procedure F_ZZRECBOX ( ZZRECBOX tests )
 
      SUBROUTINE F_ZZRECBOX ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZRECBOX.
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

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

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
C     This routine tests the private SPICELIB routine ZZRECBOX.
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
C-    TSPICE Version 1.0.0, 04-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

C
C     Local Variables 
C
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LX
      DOUBLE PRECISION      LY
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XR
      DOUBLE PRECISION      XLX
      DOUBLE PRECISION      XLY
      DOUBLE PRECISION      XLZ

      INTEGER               I
 
           
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
      CALL TOPEN ( 'F_ZZRECBOX' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Box includes origin.' )
 
      BOUNDS(1,1) =  -1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) =  4.D0

      BOUNDS(1,3) = -6.D0
      BOUNDS(2,3) =  8.D0


      DO I = 1, 3
         XCTR(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2
      END DO
    
      XLX = BOUNDS(2,1) - BOUNDS(1,1) 
      XLY = BOUNDS(2,2) - BOUNDS(1,2) 
      XLZ = BOUNDS(2,3) - BOUNDS(1,3) 

      XR  = SQRT( XLX**2 + XLY**2 + XLZ**2 ) / 2


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      TOL = VTIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~~/', XCTR, 3, TOL, OK )

      CALL CHCKSD ( 'LX', LX, '~', XLX, TOL, OK )
      CALL CHCKSD ( 'LY', LY, '~', XLY, TOL, OK )
      CALL CHCKSD ( 'LZ', LZ, '~', XLZ, TOL, OK )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XR, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Box excludes origin.' )
 
      BOUNDS(1,1) =  3.D0
      BOUNDS(2,1) =  5.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) = -6.D0
      BOUNDS(2,3) = -5.D0


      DO I = 1, 3
         XCTR(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2
      END DO
    
      XLX = BOUNDS(2,1) - BOUNDS(1,1) 
      XLY = BOUNDS(2,2) - BOUNDS(1,2) 
      XLZ = BOUNDS(2,3) - BOUNDS(1,3) 

      XR  = SQRT( XLX**2 + XLY**2 + XLZ**2 ) / 2


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      TOL = VTIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~~/', XCTR, 3, TOL, OK )

      CALL CHCKSD ( 'LX', LX, '~', XLX, TOL, OK )
      CALL CHCKSD ( 'LY', LY, '~', XLY, TOL, OK )
      CALL CHCKSD ( 'LZ', LZ, '~', XLZ, TOL, OK )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XR, TOL, OK )




C***********************************************************************
C
C     Error cases
C
C***********************************************************************
 
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: invalid rectangular coordinate bounds.' )

 
      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  -3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      
      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )  

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


