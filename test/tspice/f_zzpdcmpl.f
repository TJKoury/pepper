C$Procedure F_ZZPDCMPL ( ZZPDCMPL tests )
 
      SUBROUTINE F_ZZPDCMPL ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB geometry routine ZZPDCMPL.
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
C     This routine tests the private SPICELIB 
C     geometry routine ZZPDCMPL.
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
C-    TSPICE Version 1.0.0, 19-JAN-2017 (NJB)
C
C        Original version 09-MAR-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      RPD

C
C     Local Parameters
C
      INTEGER               LT
      PARAMETER           ( LT = -1 )
      
      INTEGER               EQ
      PARAMETER           ( EQ =  0 )

      INTEGER               GT
      PARAMETER           ( GT =  1 )

C
C     Local Variables
C
C

C      CHARACTER*(LNSIZE)    LABEL
C      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      P     ( 3 )
      DOUBLE PRECISION      PLAT
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
 
C      INTEGER               I
C      INTEGER               J
      INTEGER               REL
      INTEGER               XREL

C
C     Saved values
C      
C     Initial values
C
 

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZPDCMPL' )





C***********************************************************************
C
C     Special cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Point is at origin, LAT > 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  = 0.D0 
      P(2)  = 0.D0 
      P(3)  = 0.D0 

C
C     The latitude of P is considered to be zero.
C
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Point is at origin, LAT < 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4
      
      P(1)  = 0.D0 
      P(2)  = 0.D0 
      P(3)  = 0.D0 

C
C     The latitude of P is considered to be zero.
C
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Point is at origin, LAT = 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  = 0.D0 
      P(2)  = 0.D0 
      P(3)  = 0.D0 

C
C     The latitude of P is considered to be zero.
C
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = EQ

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Point is on -Z axis, LAT > 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  =  0.D0 
      P(2)  =  0.D0 
      P(3)  = -1.D0 

C
C     The latitude of P is -pi/2.
C
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Point is on -Z axis, -pi/2 < LAT < 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4
      
      P(1)  =  0.D0 
      P(2)  =  0.D0 
      P(3)  = -1.D0 

C
C     The latitude of P is -pi/2.
C
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Point is on -Z axis, LAT = 0.' )

      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  =  0.D0 
      P(2)  =  0.D0 
      P(3)  = -1.D0 

C
C     The latitude of P is -pi/2.
C
      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




C***********************************************************************
C
C     LAT = 0 tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = 0 deg; P(3) > 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = 0 deg; P(3) = 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 0.D0 

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = 0 deg; P(3) < 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  = -1.D0 

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = RE/2; LAT = 0 deg; P(3) > 0' )


      RE    = 2.D0
      RP    = 1.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = RE/2; LAT = 0 deg; P(3) = 0' )


      RE    = 2.D0
      RP    = 1.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 0.D0 

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = RE/2; LAT = 0 deg; P(3) < 0' )


      RE    = 2.D0
      RP    = 1.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  = -1.D0 

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




C***********************************************************************
C
C     LAT = pi/2, -pi/2 tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = 90 deg; P(3) > 0, P(1)=P(2)=0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = HALFPI()
      
      P(1)  = 0.D0 
      P(2)  = 0.D0 
      P(3)  = 1.D0 

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = 90 deg; P(3) > 0, P(1) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = HALFPI()
      
      P(1)  = 1.D0 
      P(2)  = 0.D0 
      P(3)  = 1.D0 

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = 90 deg; P(3) > 0, P(2) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = HALFPI()
      
      P(1)  =  0.D0 
      P(2)  =  1.D0 
      P(3)  =  1.D0 

      XREL  = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )




C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = -90 deg; P(3) < 0, P(1)=P(2)=0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -HALFPI()
      
      P(1)  =  0.D0 
      P(2)  =  0.D0 
      P(3)  = -1.D0 

      XREL  = EQ

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = -90 deg; P(3) < 0, P(1) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -HALFPI()
      
      P(1)  =  1.D0 
      P(2)  =  0.D0 
      P(3)  = -1.D0 

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'RP = 2*RE; LAT = -90 deg; P(3) < 0, P(2) != 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -HALFPI()
      
      P(1)  =   0.D0 
      P(2)  =   1.D0 
      P(3)  =  -1.D0 

      XREL  = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )





C***********************************************************************
C
C     Basic tests for prolate spheroids
C
C***********************************************************************



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 70 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 50 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = -50 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 60+1e-13 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = 60-1e-13 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

 
      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = -70 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = -50 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = 50 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  =  50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = -60 deg; lat(P) = -60-1e-13 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = 2*RE; LAT = 60 deg; lat(P) = -60+1e-13 deg' )
 

      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

 
      
C***********************************************************************
C
C     Basic tests for oblate spheroids
C
C***********************************************************************



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 70 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 50 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = 50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = -50 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 60+1e-13 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = 60-1e-13 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = 60.D0 * RPD()
      PLAT  = ( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

 
      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = -70 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -70.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = -50 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = 50 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  =  50.D0 * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = -60 deg; lat(P) = -60-1e-13 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 + 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = LT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )



C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RP = RE/2; LAT = 60 deg; lat(P) = -60+1e-13 deg' )
 

      RE    = 8.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = -60.D0 * RPD()
      PLAT  = -( 60.D0 - 1.D-13 ) * RPD()
      LON   = 30.D0 * RPD()
      ALT   =  1.D0

      CALL GEOREC ( LON, PLAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XREL = GT

      CALL ZZPDCMPL ( RE, F, P, LAT, REL )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'REL', REL, '=', XREL, 0, OK )

 
      
 



  
C
C --- Case: ------------------------------------------------------
C
 

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


