C$Procedure F_ZZRYTPDT ( ZZRYTPDT tests )
 
      SUBROUTINE F_ZZRYTPDT ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZRYTPDT.
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
C     This routine tests the private SPICELIB routine ZZRYTPDT.
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
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 18-JAN-2017 (NJB) (EDW)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )
 

C
C     Local Variables 
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      COLAT
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      EMAX
      DOUBLE PRECISION      EMIN
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      H
      DOUBLE PRECISION      H2
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      MAXALT
      DOUBLE PRECISION      MAXD
      DOUBLE PRECISION      MINALT
      DOUBLE PRECISION      NEGDIR ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      PMAX
      DOUBLE PRECISION      PMIN
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      S
      DOUBLE PRECISION      TARGET ( 3 )
      DOUBLE PRECISION      TARGT2 ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UPLNML ( 3 )
      DOUBLE PRECISION      VEAST  ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VWEST  ( 3 )
      DOUBLE PRECISION      XINCPT
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )
      DOUBLE PRECISION      XYX    ( 3 )
      DOUBLE PRECISION      YINCPT
      DOUBLE PRECISION      Z      ( 3 )     
      
      INTEGER               NXPTS
      INTEGER               XNXPTS

      LOGICAL               FOUND

C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /
      DATA                  Z      / 0.D0, 0.D0, 1.D0 /

C
C     Saved variables
C     
      SAVE

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZRYTPDT' )




C***********************************************************************
C
C     Error cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: negative margin.' )

      RE = 3000.D0
      F  =    0.1D0      

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      
      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) =  PI()/2

      BOUNDS(1,3) = -RE/2.D0
      BOUNDS(2,3) =  RE/2.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      CALL VPACK ( 3010.D0, 3020.D0, 2800.D0, VERTEX )

      MARGIN = -1.D-6

      CALL VMINUS ( VERTEX, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: zero direction vector.' )


      MARGIN = 1.D-6


      CALL CLEARD ( 3, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC  ( .TRUE., 'SPICE(ZEROVECTOR)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: longitude bound out of range.' )


      CALL VMINUS ( VERTEX, RAYDIR )


      BOUNDS(1,1) = -3*PI()

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      BOUNDS(1,1) = -2*PI()
      BOUNDS(2,1) =  3*PI()

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 


C***********************************************************************
C
C
C     OBLATE CASES 
C
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup for simple oblate cases.' )
C
C
C     The following simple cases all use the same volume element:
C
C        RE:      3000.0
C        F:          0.5
C
C        Lon:     -PI()/4 : PI()/4
C        Lat:     -PI()/6 : PI()/3
C        Alt:     -100.0  : 100.0
C

      RE = 3000.D0
      F  =    0.5D0  

      BOUNDS(1,1) = -PI()/4
      BOUNDS(2,1) =  PI()/4
      
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F
      
C
C     Use a positive margin.
C
      MARGIN = 1.D-5

C
C     Get the radii of the outer and inner bounding ellipsoids.
C     Take margin into account.
C
C     This is the oblate case. 
C      
      RP     = RE * ( 1.D0 - F )      

      MINALT = BOUNDS(1,3) - MARGIN*ABS( BOUNDS(1,3) )
      MAXALT = BOUNDS(2,3) + MARGIN*ABS( BOUNDS(2,3) )

      CALL ZZELLBDS ( RE,   RP,   MAXALT, MINALT, 
     .                EMAX, PMAX, EMIN,   PMIN   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray''s vertex is inside element.' )


C
C     Create a vertex at maximum altitude. This point will be
C     inside the outer bounding ellipsoid, so it will be
C     considered to be inside the volume element.
C
      LON = 0.D0
      LAT = 0.D0
      ALT = BOUNDS(2,3)
      
      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      CALL VMINUS ( VERTEX, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', VERTEX, 3, TOL, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray misses outer bounding spheroid.' )

      CALL VPACK ( RE,    0.D0, RE,   VERTEX )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray hits outer bounding spheroid but '
     .//           'misses element.'                      )

      CALL VPACK ( RE,    0.D0, RP*.99D0,   VERTEX )
      CALL VPACK ( -1.D0, 0.D0, 0.D0,       RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray hits element on outer bounding spheroid.' )

      CALL VPACK ( 2*RE,    0.D0, RP/2, VERTEX )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, RAYDIR )


      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept.
C
      CALL SURFPT ( VERTEX, RAYDIR, EMAX, EMAX, PMAX, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT, 3, TOL, OK )

 

C
C --- Case: ------------------------------------------------------
C

 


      CALL TCASE ( 'Ray hits element on inner bounding spheroid. ' 
     .//           'Vertex is outside outer spheroid.'           )

      CALL VPACK ( -4*RE, 0.D0, RP/2, VERTEX )
      CALL VPACK (  1.D0, 0.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. We need to approach the
C     inner ellipsoid from the opposite side, since the vertex
C     and element are on opposite sides of the Z axis.
C
      CALL VEQU ( VERTEX, ENDPT2 )

      ENDPT2(1) = - ENDPT2(1)

      CALL VMINUS ( RAYDIR, NEGDIR )

      CALL SURFPT ( ENDPT2, NEGDIR, EMIN, EMIN, PMIN, XXPT, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

 


C
C --- Case: ------------------------------------------------------
C

 


      CALL TCASE ( 'Ray hits element on inner bounding spheroid. ' 
     .//           'Vertex is inside inner spheroid.'           )

      CALL VPACK (  0.D0, 0.D0, RP/2, VERTEX )
      CALL VPACK (  1.D0, 0.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C 
      CALL SURFPT ( VERTEX, RAYDIR, EMIN, EMIN, PMIN, XXPT, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )
 
C
C --- Case: ------------------------------------------------------
C


      CALL TCASE ( 'Ray hits element from west side.'  )


      S = SQRT(2.D0)/2

      CALL VPACK (  S*RE - 10.D0, -2*RE, 0.D0, VERTEX )
      CALL VPACK (  0.D0,  1.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C 
C
C     Create the normal vector for the western boundary.
C
      CALL LATREC ( 1.D0,  BOUNDS(1,1), 0.D0, VWEST  )
      CALL UCRSS  ( VWEST, Z,                 UPLNML )

C
C     The western plane contains the origin.
C
      CONST = 0.D0
      
      CALL VHAT ( RAYDIR, UDIR )


      MAXD  = 4 * RE

      
      CALL ZZINRYPL ( VERTEX, UDIR, UPLNML,
     .                CONST,  MAXD, XNXPTS, XXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'ZZINRYPL XNXTPS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )
 
       

C
C --- Case: ------------------------------------------------------
C


      CALL TCASE ( 'Ray hits element from east side.'  )


      S = SQRT(2.D0)/2

      CALL VPACK (  S*RE - 10.D0, 2*RE, 0.D0, VERTEX )
      CALL VPACK (  0.D0, -1.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C 
C
C     Create the normal vector for the eastern boundary.
C
      CALL LATREC ( 1.D0,  BOUNDS(2,1), 0.D0, VEAST  )
      CALL UCRSS  ( Z,     VEAST,             UPLNML )

C
C     The eastern plane contains the origin.
C
      CONST = 0.D0
      
      CALL VHAT ( RAYDIR, UDIR )


      MAXD  = 4 * RE

      
      CALL ZZINRYPL ( VERTEX, UDIR, UPLNML,
     .                CONST,  MAXD, XNXPTS, XXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'ZZINRYPL XNXTPS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )       





C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'Ray hits element from north side.'  )

 
       CALL GEOREC ( 0.D0, BOUNDS(2,2), 0.D0, RE, F, VERTEX )
 

 
      CALL RECGEO ( VERTEX, RE, F, LON, LAT, ALT )
 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     raise it above the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = 2*RE
      
 
      CALL VPACK (  0.D0,  0.D0, -1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the upper latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone is not the origin.
C     It lies on the -Z axis. Compute the intercept.
C
      ANGLE = HALFPI()-BOUNDS(2,2)


 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
  


C
C --- Case: ------------------------------------------------------
C

 
      CALL TCASE ( 'Ray hits element from south side.'  )

 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )


C
C     Shift the vertex toward the origin in the X direction and
C     lower it below the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = -2*RE
      

      CALL VPACK (  0.D0,  0.D0,  1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the lower latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone is not the origin.
C     It lies on the +Z axis. Compute the intercept.
C
      ANGLE = HALFPI()-BOUNDS(1,2)
      
      LAT   = BOUNDS(1,2)

      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT


      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )
 

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
  

C
C     In the following cases, we change the latitude bounds of
C     the element.
C

C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'North element boundary is at 0 latitude. '
     .//           'Ray hits element from north side.'  )

 
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  0.D0

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     raise it above the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = 2*RE
      
 
      CALL VPACK (  0.D0,  0.D0, -1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the "upper latitude cone."
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the -Z axis. However, in this case the apex
C     should be the origin. Compute the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(2,2)

      LAT   = BOUNDS(2,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       



C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'North element boundary is at negative latitude. '
     .//           'Ray hits element from north side.'  )

 
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/6

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     raise it above the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = 2*RE
      
 
      CALL VPACK (  0.D0,  0.D0, -1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the "upper latitude cone."
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the -Z axis. However, in this case the apex
C     should be the origin. Compute the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(2,2)

      LAT = BOUNDS(2,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       


C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'South element boundary is at 0 latitude. '
     .//           'Ray hits element from south side.'  )

 
      BOUNDS(1,2) = 0.D0  
      BOUNDS(2,2) = PI()/3

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and minimum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     lower it below the outer bounding sphere in the -Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = -2*RE
     
 
      CALL VPACK (  0.D0,  0.D0,  1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


C
C     Compute the expected intercept. 
C
C     The intercept should be on the "lower latitude cone."
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the +Z axis (for cones of negative latitude). 
C     However, in this case the apex should be the origin. Compute 
C     the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(1,2)

      LAT   = BOUNDS(1,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
    

C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'South element boundary is at negative latitude. '
     .//           'Ray hits element from south side.'  )

 
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and minimum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     lower it below the outer bounding sphere in the -Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = -2*RE
     
 
      CALL VPACK (  0.D0,  0.D0,  1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


C
C     Compute the expected intercept. 
C
C     The intercept should be on the lower latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the +Z axis (for cones of negative latitude). 
C     However, in this case the apex should be the origin. Compute 
C     the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(1,2)

      LAT   = BOUNDS(1,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT
 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
    

C
C     The following latitude boundary intercept cases test the handling
C     of multiple roots in the ray-cone intercept computation. In these
C     cases, the second root should be chosen.
C 
    
C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'North element boundary is at positive latitude. '
     .//           'Ray hits element from the -X direction.'  )
      
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3



C
C     Find the point on the reference ellipsoid at maximum altitude
C     and maximum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 

C
C     Shift the vertex in the -X direction. and lower it below the outer
C     bounding sphere in the -Z direction.
C
      VERTEX(1) = VERTEX(1) - 10*RE
      VERTEX(3) = VERTEX(3) - 10.D0

C
C     This ray should hit the upper latitude cone twice. The second
C     intercept is the one we want.
C 
      CALL VPACK (  1.D0,  0.D0,  0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the upper latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(1) = -ENDPT2(1)

      ANGLE = HALFPI()-BOUNDS(2,2)

      LAT   = BOUNDS(2,2)
 
C
C     The cone's apex should be on the -Z axis.
C
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )

C
C     Note: we expect two roots. The second one is the correct one.
C
      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 2, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT2, 3, TOL, OK )       
    


    
C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'South element boundary is at negative latitude. '
     .//           'Ray hits element from the -X direction.'  )
      
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3



C
C     Find the point on the reference ellipsoid at maximum altitude
C     and minimum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )
 

C
C     Shift the vertex in the -X direction. and raise it below the outer
C     bounding sphere in the +Z direction.
C
      VERTEX(1) = VERTEX(1) - 10*RE
      VERTEX(3) = VERTEX(3) + 10.D0

C
C     This ray should hit the lower latitude cone twice. The second
C     intercept is the one we want.
C 
      CALL VPACK (  1.D0,  0.D0,  0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the lower latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(1) = -ENDPT2(1)

      ANGLE = HALFPI()-BOUNDS(1,2)

      LAT   = BOUNDS(1,2)
 
C
C     The cone's apex should be on the +Z axis.
C
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )

C
C     Note: we expect two roots. The second one is the correct one.
C
      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 2, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT2, 3, TOL, OK )       
    








C***********************************************************************
C
C
C     PROLATE CASES 
C
C
C***********************************************************************


C
C     Repeat the cases above using a prolate reference ellipsoid.
C



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup for simple prolate cases.' )
C
C
C     The following simple cases all use the same volume element:
C
C        RE:      3000.0
C        F:         -1.0
C
C        Lon:     -PI()/4 : PI()/4
C        Lat:     -PI()/6 : PI()/3
C        Alt:     -100.0  : 100.0
C

      RE = 3000.D0
      F  =   -1.D0  

      BOUNDS(1,1) = -PI()/4
      BOUNDS(2,1) =  PI()/4
      
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F
      
C
C     Use a positive margin.
C
      MARGIN = 1.D-5

C
C     Get the radii of the outer and inner bounding ellipsoids.
C     Take margin into account.
C
C     This is the prolate case. 
C      
      RP     = RE * ( 1.D0 - F )      

      MINALT = BOUNDS(1,3) - MARGIN*ABS( BOUNDS(1,3) )
      MAXALT = BOUNDS(2,3) + MARGIN*ABS( BOUNDS(2,3) )

C
C     Careful...see the header of ZZELLBDS for info on use
C     with prolate ellipsoids.
C
      CALL ZZELLBDS ( RP,   RE,   MAXALT, MINALT, 
     .                PMAX, EMAX, PMIN,   EMIN   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray''s vertex is inside element.' )


C
C     Create a vertex at maximum altitude. This point will be
C     inside the outer bounding ellipsoid, so it will be
C     considered to be inside the volume element.
C
      LON = 0.D0
      LAT = 0.D0
      ALT = BOUNDS(2,3)
      
      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      CALL VMINUS ( VERTEX, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', VERTEX, 3, TOL, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray misses outer bounding spheroid.' )

      CALL VPACK ( RP,    0.D0, RP,   VERTEX )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray hits outer bounding spheroid but '
     .//           'misses element.'                      )

      CALL VPACK ( RP*.99D0, 0.D0, RE,    VERTEX )
      CALL VPACK ( 0.D0,     0.D0, -1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ray hits element on outer bounding spheroid.' )

      CALL VPACK ( 2*RE,    0.D0, RP/2, VERTEX )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, RAYDIR )


      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept.
C
      CALL SURFPT ( VERTEX, RAYDIR, EMAX, EMAX, PMAX, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT, 3, TOL, OK )

 
       

C
C --- Case: ------------------------------------------------------
C

 


      CALL TCASE ( 'Ray hits element on inner bounding spheroid. ' 
     .//           'Vertex is outside outer spheroid.'           )

      CALL VPACK ( -4*RE, 0.D0, RP/2, VERTEX )
      CALL VPACK (  1.D0, 0.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. We need to approach the
C     inner ellipsoid from the opposite side, since the vertex
C     and element are on opposite sides of the Z axis.
C
      CALL VEQU ( VERTEX, ENDPT2 )

      ENDPT2(1) = - ENDPT2(1)

      CALL VMINUS ( RAYDIR, NEGDIR )

      CALL SURFPT ( ENDPT2, NEGDIR, EMIN, EMIN, PMIN, XXPT, FOUND )
C      CALL SURFPT ( VERTEX, RAYDIR, EMAX, EMAX, PMAX, XXPT, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

 


C
C --- Case: ------------------------------------------------------
C

 


      CALL TCASE ( 'Ray hits element on inner bounding spheroid. ' 
     .//           'Vertex is inside inner spheroid.'           )

      CALL VPACK (  0.D0, 0.D0, RP/2, VERTEX )
      CALL VPACK (  1.D0, 0.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C       
      CALL SURFPT ( VERTEX, RAYDIR, EMIN, EMIN, PMIN, XXPT, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )
 
C
C --- Case: ------------------------------------------------------
C


      CALL TCASE ( 'Ray hits element from west side.'  )


      S = SQRT(2.D0)/2

      CALL VPACK (  S*RE - 10.D0, -2*RE, 0.D0, VERTEX )
      CALL VPACK (  0.D0,  1.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C 
C
C     Create the normal vector for the western boundary.
C
      CALL LATREC ( 1.D0,  BOUNDS(1,1), 0.D0, VWEST  )
      CALL UCRSS  ( VWEST, Z,                 UPLNML )

C
C     The western plane contains the origin.
C
      CONST = 0.D0
      
      CALL VHAT ( RAYDIR, UDIR )


      MAXD  = 4 * RE

      
      CALL ZZINRYPL ( VERTEX, UDIR, UPLNML,
     .                CONST,  MAXD, XNXPTS, XXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'ZZINRYPL XNXTPS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )
 
       

C
C --- Case: ------------------------------------------------------
C


      CALL TCASE ( 'Ray hits element from east side.'  )


      S = SQRT(2.D0)/2

      CALL VPACK (  S*RE - 10.D0, 2*RE, 0.D0, VERTEX )
      CALL VPACK (  0.D0, -1.D0, 0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C 
C
C     Create the normal vector for the eastern boundary.
C
      CALL LATREC ( 1.D0,  BOUNDS(2,1), 0.D0, VEAST  )
      CALL UCRSS  ( Z,     VEAST,             UPLNML )

C
C     The eastern plane contains the origin.
C
      CONST = 0.D0
      
      CALL VHAT ( RAYDIR, UDIR )


      MAXD  = 4 * RE

      
      CALL ZZINRYPL ( VERTEX, UDIR, UPLNML,
     .                CONST,  MAXD, XNXPTS, XXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'ZZINRYPL XNXTPS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )       





C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'Ray hits element from north side.'  )

 
       CALL GEOREC ( 0.D0, BOUNDS(2,2), 0.D0, RE, F, VERTEX )
 

 
      CALL RECGEO ( VERTEX, RE, F, LON, LAT, ALT )
 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     raise it above the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = 2*RE
      
 
      CALL VPACK (  0.D0,  0.D0, -1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the upper latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone is not the origin.
C     It lies on the -Z axis. Compute the intercept.
C
      ANGLE = HALFPI()-BOUNDS(2,2)


 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
  

C
C --- Case: ------------------------------------------------------
C

 
      CALL TCASE ( 'Ray hits element from south side.'  )

 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )


C
C     Shift the vertex toward the origin in the X direction and
C     lower it below the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = -2*RE
      

      CALL VPACK (  0.D0,  0.D0,  1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the lower latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone is not the origin.
C     It lies on the +Z axis. Compute the intercept.
C
      ANGLE = HALFPI()-BOUNDS(1,2)
      
      LAT   = BOUNDS(1,2)

      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT


      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )
 

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
  

C
C     In the following cases, we change the latitude bounds of
C     the element.
C

C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'North element boundary is at 0 latitude. '
     .//           'Ray hits element from north side.'  )

 
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  0.D0

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     raise it above the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = 2*RE
      
 
      CALL VPACK (  0.D0,  0.D0, -1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the "upper latitude cone."
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the -Z axis. However, in this case the apex
C     should be the origin. Compute the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(2,2)

      LAT   = BOUNDS(2,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       



C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'North element boundary is at negative latitude. '
     .//           'Ray hits element from north side.'  )

 
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/6

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     raise it above the outer bounding sphere in the Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = 2*RE
      
 
      CALL VPACK (  0.D0,  0.D0, -1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the "upper latitude cone."
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the -Z axis. However, in this case the apex
C     should be the origin. Compute the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(2,2)

      LAT = BOUNDS(2,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       


C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'South element boundary is at 0 latitude. '
     .//           'Ray hits element from south side.'  )

 
      BOUNDS(1,2) = 0.D0  
      BOUNDS(2,2) = PI()/3

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and minimum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     lower it below the outer bounding sphere in the -Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = -2*RE
     
 
      CALL VPACK (  0.D0,  0.D0,  1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


C
C     Compute the expected intercept. 
C
C     The intercept should be on the "lower latitude cone."
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the +Z axis (for cones of negative latitude). 
C     However, in this case the apex should be the origin. Compute 
C     the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(1,2)

      LAT   = BOUNDS(1,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
    

C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'South element boundary is at negative latitude. '
     .//           'Ray hits element from south side.'  )

 
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3

C
C     Find the point on the reference ellipsoid at maximum altitude
C     and minimum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )
 
C
C     Shift the vertex toward the origin in the X direction and
C     lower it below the outer bounding sphere in the -Z direction.

      VERTEX(1) = VERTEX(1) - 100.D0
      VERTEX(3) = -2*RE
     
 
      CALL VPACK (  0.D0,  0.D0,  1.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


C
C     Compute the expected intercept. 
C
C     The intercept should be on the lower latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(3) = -ENDPT2(3)

C
C     Caution! The apex of the latitude cone usually is not the origin.
C     It usually lies on the +Z axis (for cones of negative latitude). 
C     However, in this case the apex should be the origin. Compute 
C     the intercept by the usual means.
C
      ANGLE = HALFPI()-BOUNDS(1,2)

      LAT   = BOUNDS(1,2)
 
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )


      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 1, 0, OK )

      TOL = TIGHT
 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT1, 3, TOL, OK )       
    

C
C     The following latitude boundary intercept cases test the handling
C     of multiple roots in the ray-cone intercept computation. In these
C     cases, the second root should be chosen.
C 
    
C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'North element boundary is at positive latitude. '
     .//           'Ray hits element from the -X direction.'  )
      
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3



C
C     Find the point on the reference ellipsoid at maximum altitude
C     and maximum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(2,2), BOUNDS(2,3), RE, F, VERTEX )
 

C
C     Shift the vertex in the -X direction. and lower it below the outer
C     bounding sphere in the -Z direction.
C
      VERTEX(1) = VERTEX(1) - 10*RE
      VERTEX(3) = VERTEX(3) - 10.D0

C
C     This ray should hit the upper latitude cone twice. The second
C     intercept is the one we want.
C 
      CALL VPACK (  1.D0,  0.D0,  0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the upper latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(1) = -ENDPT2(1)

      ANGLE = HALFPI()-BOUNDS(2,2)

      LAT   = BOUNDS(2,2)
 
C
C     The cone's apex should be on the -Z axis.
C
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )

C
C     Note: we expect two roots. The second one is the correct one.
C
      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 2, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT2, 3, TOL, OK )       
    


    
C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'South element boundary is at negative latitude. '
     .//           'Ray hits element from the -X direction.'  )
      
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) =  PI()/3



C
C     Find the point on the reference ellipsoid at maximum altitude
C     and minimum latitude, longitude zero.
C 
      CALL GEOREC ( 0.D0, BOUNDS(1,2), BOUNDS(2,3), RE, F, VERTEX )
 

C
C     Shift the vertex in the -X direction. and raise it below the outer
C     bounding sphere in the +Z direction.
C
      VERTEX(1) = VERTEX(1) - 10*RE
      VERTEX(3) = VERTEX(3) + 10.D0

C
C     This ray should hit the lower latitude cone twice. The second
C     intercept is the one we want.
C 
      CALL VPACK (  1.D0,  0.D0,  0.D0, RAYDIR )

      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     Compute the expected intercept. 
C
C     The intercept should be on the lower latitude cone.
C 
      CALL VEQU ( VERTEX, ENDPT1 )
      CALL VEQU ( ENDPT1, ENDPT2 )
      
      ENDPT2(1) = -ENDPT2(1)

      ANGLE = HALFPI()-BOUNDS(1,2)

      LAT   = BOUNDS(1,2)
 
C
C     The cone's apex should be on the +Z axis.
C
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( ORIGIN, APEX )
      APEX(3) = YINCPT

 
      CALL INCNSG ( APEX,   Z,      ANGLE, ENDPT1,
     .              ENDPT2, XNXPTS, XPT1,  XPT2   )

C
C     Note: we expect two roots. The second one is the correct one.
C
      CALL CHCKSI ( 'INCNSG XNXPTS', XNXPTS, '=', 2, 0, OK )

      TOL = TIGHT 

      CALL CHCKAD ( 'XPT', XPT, '~~/', XPT2, 3, TOL, OK )       
    

 

    
C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'Oblate case: ray hits positive latitude cone '
     .//           'below X-Y plane. No valid roots.'             )
 
C
C     In the following case, the ray hits a latitude cone at
C     a point having longitude and altitude within the element's
C     bounds, but the intercept is nevertheless outside the element. 
C
C     This type of case should not arise if we have a planetodetic
C     volume element boundary created using DSKW02, because that
C     routine limits the lower altitude. However, it is possible to
C     pass to ZZRYTPDT inputs that define such a case, and 
C     we want to make sure that it's handled properly.
C
C     Use a highly eccentric spheroid.
C

      RE = 3000.D0
      F  =    0.8D0      

      CORPAR(1) = RE
      CORPAR(2) = F

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      
      BOUNDS(1,2) =  0.D0
      BOUNDS(2,2) =  PI()/8
C
C     Create a point on the spheroid having maximum latitude.
C
      LAT = BOUNDS(2,2)

      CALL GEOREC ( 0.D0, LAT, 0.D0, RE, F, P )


      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELNAXX ( A, B, LAT, XINCPT, YINCPT )

      CALL VPACK ( 0.D0, 0.D0, YINCPT, APEX )

      CALL VPACK ( XINCPT, 0.D0, 0.D0, XYX )

C
C     Pick a lower altitude bound that will get past ZZELLBDS.
C
      BOUNDS(1,3) = (-B**2/A) + 1.D0
      BOUNDS(2,3) =  0.D0

C
C     Consider the vector from the cone's apex to P. We'll pick a
C     target point on this vector, such that the target has negative Z
C     component. The target's altitude is within the element's altitude
C     bounds, because the altitude is measured from the surface on the 
C     -Z side.
C
      COLAT = HALFPI() - LAT

      H     = -B - ( BOUNDS(1,3) / 2 )

      CALL VPACK ( -TAN(COLAT)*(YINCPT-H), 0.D0, H, TARGET )

     
      
C
C     Check target's off-axis angle: the target should lie on the cone.
C
      CALL VSUB ( TARGET, APEX, OFFSET )

C
C     Create a horizontal ray that contacts the cone at TARGET.
C
      CALL VPACK ( 2*A,   0.D0, H,    VERTEX )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, RAYDIR )

C
C     Find the intercept of the ray with the boundary of the 
C     volume element. The ray will hit the upper latitude cone
C     below the X-Y plane. This should be considered a non-intercept
C     case.
C
      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )



C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'Oblate case: ray hits positive latitude cone '
     .//           'below X-Y plane, then above X-Y plane. First '
     .//           'root is invalid; second root is valid.'       )

 
C
C     In the following case, the ray hits a latitude cone at
C     a point having longitude and altitude within the element's
C     bounds, but the intercept is nevertheless outside the element. 
C
C     This type of case should not arise if we have a planetodetic
C     volume element boundary created using DSKW02, because that
C     routine limits the lower altitude. However, it is possible to
C     pass to ZZRYTPDT inputs that define such a case, and 
C     we want to make sure that it's handled properly.
C
C     Use a highly eccentric spheroid.
C

      RE = 3000.D0
      F  =    0.8D0      

      CORPAR(1) = RE
      CORPAR(2) = F

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      
      BOUNDS(1,2) =  0.D0
      BOUNDS(2,2) =  PI()/8
C
C     Create a point on the spheroid having maximum latitude.
C
      LAT = BOUNDS(2,2)

      CALL GEOREC ( 0.D0, LAT, 0.D0, RE, F, P )


      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELNAXX ( A, B, LAT, XINCPT, YINCPT )

      CALL VPACK ( 0.D0, 0.D0, YINCPT, APEX )

      CALL VPACK ( XINCPT, 0.D0, 0.D0, XYX )

C
C     Pick a lower altitude bound that will get past ZZELLBDS.
C
      BOUNDS(1,3) = (-B**2/A) + 1.D0
      BOUNDS(2,3) =  100.D0

C
C     Consider the vector from the cone's apex to P. We'll pick a
C     target point on this vector, such that the target has negative Z
C     component. The target's altitude is within the element's altitude
C     bounds, because the altitude is measured from the surface on the 
C     -Z side.
C
      COLAT = HALFPI() - LAT

      H     = -B - ( BOUNDS(1,3) / 2 )

      CALL VPACK ( -TAN(COLAT)*(YINCPT-H), 0.D0, H, TARGET )
 

C
C     Create a second target point that does have latitude within
C     bounds. 
C
      H2 = B/8

      CALL VPACK( -TAN(COLAT)*(H2-YINCPT), 0.D0, H2, TARGT2 )

C
C     Check second target's off-axis angle: the target should lie on
C     the cone.
C
      CALL VSUB ( TARGT2, APEX, OFFSET )

C
C     Create a ray that contacts the cone first at TARGET, then at
C     TARGT2.
C
      CALL VSUB ( TARGT2, TARGET, RAYDIR )

 

      CALL VSUB ( TARGET, RAYDIR, VERTEX )
      CALL VHATIP ( RAYDIR ) 

C
C     Find the intercept of the ray with the boundary of the 
C     volume element. The ray will hit the upper latitude cone
C     below the X-Y plane. This should be considered a non-intercept
C     case.
C
      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     The intercept that was found should be the second target.
C     

      IF ( OK ) THEN
         CALL CHCKAD ( 'XPT', XPT, '~~/', TARGT2, 3, TIGHT, OK )
      END IF




C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'Oblate case: ray hits negative latitude cone '
     .//           'above X-Y plane. No valid roots.'             )
 
C
C     In the following case, the ray hits a latitude cone at
C     a point having longitude and altitude within the element's
C     bounds, but the intercept is nevertheless outside the element. 
C
C     This type of case should not arise if we have a planetodetic
C     volume element boundary created using DSKW02, because that
C     routine limits the lower altitude. However, it is possible to
C     pass to ZZRYTPDT inputs that define such a case, and 
C     we want to make sure that it's handled properly.
C
C     Use a highly eccentric spheroid.
C

      RE = 3000.D0
      F  =    0.8D0      

      CORPAR(1) = RE
      CORPAR(2) = F

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      
      BOUNDS(1,2) =  -PI()/8
      BOUNDS(2,2) =  0.D0

C
C     Create a point on the spheroid having minimum latitude. 
C
      LAT = BOUNDS(1,2)

      CALL GEOREC ( 0.D0, LAT, 0.D0, RE, F, P )


      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELNAXX ( A, B, LAT, XINCPT, YINCPT )

      CALL VPACK ( 0.D0, 0.D0, YINCPT, APEX )

      CALL VPACK ( XINCPT, 0.D0, 0.D0, XYX )

C
C     Pick a lower altitude bound that will get past ZZELLBDS.
C
      BOUNDS(1,3) = (-B**2/A) + 1.D0
      BOUNDS(2,3) =  100.D0

C
C     Consider the vector from the cone's apex to P. We'll pick a
C     target point on this vector, such that the target has positive Z
C     component. The target's altitude is within the element's altitude
C     bounds, because the altitude is measured from the surface on the 
C     +Z side.
C
      COLAT = HALFPI() - LAT

      H     = B - ( BOUNDS(1,3) / 2 )

      CALL VPACK ( -TAN(COLAT)*(YINCPT-H), 0.D0, H, TARGET )
 
C
C     Check target's off-axis angle: the target should lie on the cone.
C
      CALL VSUB ( TARGET, APEX, OFFSET )

C
C     Create a horizontal ray that contacts the cone at TARGET.
C
      CALL VPACK ( 2*A,   0.D0, H,    VERTEX )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, RAYDIR )

C
C     Find the intercept of the ray with the boundary of the 
C     volume element. The ray will hit the upper latitude cone
C     below the X-Y plane. This should be considered a non-intercept
C     case.
C
      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )




C
C --- Case: ------------------------------------------------------
C
  
      CALL TCASE ( 'Oblate case: ray hits negative latitude cone '
     .//           'above X-Y plane, then below X-Y plane. First '
     .//           'root is invalid; second root is valid.'       )

 
C
C     In the following case, the ray hits a latitude cone at
C     a point having longitude and altitude within the element's
C     bounds, but the intercept is nevertheless outside the element. 
C
C     This type of case should not arise if we have a planetodetic
C     volume element boundary created using DSKW02, because that
C     routine limits the lower altitude. However, it is possible to
C     pass to ZZRYTPDT inputs that define such a case, and 
C     we want to make sure that it's handled properly.
C
C     Use a highly eccentric spheroid.
C

      RE = 3000.D0
      F  =    0.8D0      

      CORPAR(1) = RE
      CORPAR(2) = F

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      
      BOUNDS(1,2) =  -PI()/8
      BOUNDS(2,2) =  0.D0
C
C     Create a point on the spheroid having minimum latitude.
C
      LAT = BOUNDS(1,2)

      CALL GEOREC ( 0.D0, LAT, 0.D0, RE, F, P )


      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELNAXX ( A, B, LAT, XINCPT, YINCPT )

      CALL VPACK ( 0.D0, 0.D0, YINCPT, APEX )

      CALL VPACK ( XINCPT, 0.D0, 0.D0, XYX )

C
C     Pick a lower altitude bound that will get past ZZELLBDS.
C
      BOUNDS(1,3) = (-B**2/A) + 1.D0
      BOUNDS(2,3) =  100.D0

C
C     Consider the vector from the cone's apex to P. We'll pick a
C     target point on this vector, such that the target has negative Z
C     component. The target's altitude is within the element's altitude
C     bounds, because the altitude is measured from the surface on the 
C     -Z side.
C
      COLAT = HALFPI() - LAT

      H     = B - ( BOUNDS(1,3) / 2 )

      CALL VPACK ( -TAN(COLAT)*(YINCPT-H), 0.D0, H, TARGET )


C
C     Create a second target point that does have latitude within
C     bounds. 
C
      H2 = -B/8

      CALL VPACK( TAN(COLAT)*(YINCPT-H2), 0.D0, H2, TARGT2 )

C
C     Check second target's off-axis angle: the target should lie on
C     the cone.
C
      CALL VSUB ( TARGT2, APEX, OFFSET )

    
C
C     Create a ray that contacts the cone first at TARGET, then at
C     TARGT2.
C
      CALL VSUB ( TARGT2, TARGET, RAYDIR ) 
      CALL VSUB ( TARGET, RAYDIR, VERTEX )

      CALL VHATIP ( RAYDIR )

C
C     Find the intercept of the ray with the boundary of the 
C     volume element. The ray will hit the upper latitude cone
C     below the X-Y plane. This should be considered a non-intercept
C     case.
C
      CALL ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, 
     .                CORPAR, MARGIN, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

C
C     The intercept that was found should be the second target.
C     

      IF ( OK ) THEN
         CALL CHCKAD ( 'XPT', XPT, '~~/', TARGT2, 3, TIGHT, OK )
      END IF

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END
