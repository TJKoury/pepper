C$Procedure      F_ZZDSKSGR ( Test ZZDSKSGR )
 
      SUBROUTINE F_ZZDSKSGR ( OK )
      IMPLICIT NONE

C$ Abstract
C
C     Test the SPICELIB bounding ellipsoid routine ZZDSKSGR.
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
 
      INCLUDE 'dskdsc.inc'

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
C     This routine tests the routine ZZDSKSGR.
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
C-    TSPICE Version 1.0.0, 13-FEB-2017 (NJB)
C
C-&

 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      ZZDSKSGR

C
C     Local parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

C
C     Local Variables
C        
      DOUBLE PRECISION      BDS    ( 2, 3 ) 
      DOUBLE PRECISION      CORNER ( 3 )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XR

      INTEGER               I
      INTEGER               J
      INTEGER               K
      
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZDSKSGR' )

C
C     ZZDSKSGR error cases:
C           

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error case: unrecognized coordinate system code.' )
 

      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX) = DBLE( -1 )

      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad flattening coefficient.' )
 

      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( PDTSYS )
 
      RE               = 3.D3
      F                = 1.D0 + 1.D-13
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F

      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error case: negative equatorial radius.' )
 

      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( PDTSYS )
 
      RE               = -3.D3
      F                = 0.D0
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F

      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error case: negative minimum radius.' )
 

      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( LATSYS )

      DSKDSC(MN3IDX)   = -1.D0
      DSKDSC(MX3IDX)   =  1.D0


      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



C
C     ZZDSKSGR normal cases:
C           

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Find radius for rectangular boundary.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX) = DBLE( RECSYS )

      DSKDSC(MN1IDX) = -7.D0
      DSKDSC(MX1IDX) =  3.D0
      DSKDSC(MN2IDX) =  5.D0
      DSKDSC(MX2IDX) =  6.D0
      DSKDSC(MN3IDX) = -1.D0
      DSKDSC(MX3IDX) =  3.D0

C
C     Compute expected radius. This is the magnitude of the longest
C     vector from the origin to a corner of the bounding box.
C
C     Use the dumbest possible brute force approach. This is 
C     done to have a different algorithm from that in the 
C     SPICELIB routine.
C      
      CALL MOVED ( DSKDSC(MN1IDX), 6, BDS )

      XR = 0.D0

      DO I = 1, 2

         CORNER(1) = BDS(I,1)

         DO J = 1, 2

            CORNER(2) = BDS(J,2)

            DO K = 1, 2

               CORNER(3) = BDS(K,3)

               XR = MAX( XR, VNORM(CORNER) )

            END DO

         END DO

      END DO


      R   = ZZDSKSGR ( DSKDSC ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKSD ( 'R', R, '~/', XR, TOL, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Find radius for latitudinal boundary.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX) = DBLE( LATSYS )

      DSKDSC(MN1IDX) = -PI()/4
      DSKDSC(MX1IDX) =  PI()/2
      DSKDSC(MN2IDX) =  PI()/4
      DSKDSC(MX2IDX) =  PI()/3
      DSKDSC(MN3IDX) =  1.D3
      DSKDSC(MX3IDX) =  2.D3

      XR  = DSKDSC( MX3IDX )

      R   = ZZDSKSGR( DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 0.D0 

      CALL CHCKSD ( 'R', R, '~/', XR, TOL, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Find radius for oblate planetodetic boundary.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( PDTSYS )
 
      RE               = 3.D3
      RP               = 2.D3
      F                = ( RE - RP ) / RE
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F


      DSKDSC(MN1IDX) = -PI()/4
      DSKDSC(MX1IDX) =  PI()/2
      DSKDSC(MN2IDX) =  PI()/4
      DSKDSC(MX2IDX) =  PI()/3
      DSKDSC(MN3IDX) =  -1.D1
      DSKDSC(MX3IDX) =   2.D1

C
C     The largest distance from the origin is on the equator
C     at max altitude.
C
      XR  = RE + DSKDSC(MX3IDX)

      R   = ZZDSKSGR( DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKSD ( 'R', R, '~/', XR, TOL, OK )





      CALL T_SUCCESS ( OK )
      RETURN
      END
