C$Procedure F_ZZRYTELT ( ZZRYTELT tests )
 
      SUBROUTINE F_ZZRYTELT ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZRYTELT.
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
C     This routine tests the private SPICELIB routine ZZRYTELT.
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
C-    TSPICE Version 1.0.0, 20-JUL-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      PI

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

C
C     Local Variables 
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      AMAX
      DOUBLE PRECISION      AMIN
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BMAX
      DOUBLE PRECISION      BMIN
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      H
      DOUBLE PRECISION      L
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      W
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               NXPTS
      INTEGER               XNXPTS

      LOGICAL               FOUND

           
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
      CALL TOPEN ( 'F_ZZRYTELT' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

 
C***********************************************************************
C
C
C     Rectangular coordinates
C
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Rectangular coordinate hit case. Zero MARGIN.' )
 
C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )

      L =  40.D0
      W =  20.D0
      H =  10.D0

      BOUNDS(1,1) = -L/2
      BOUNDS(2,1) =  L/2
      BOUNDS(1,2) = -W/2
      BOUNDS(2,2) =  W/2
      BOUNDS(1,3) = -H/2
      BOUNDS(2,3) =  H/2

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX) = RECSYS

      CALL VPACK ( 0.D0, 0.D0, 2*H, VERTEX )
      CALL VMINUS( VERTEX,          RAYDIR )

      MARGIN = 0.D0

      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an intercept.
C      
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C     Create the expected intercept point.
C
      CALL VPACK ( 0.D0, 0.D0, H/2, XXPT )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Rectangular coordinate miss case. Zero '
     .//           'MARGIN.' )
 
C
C     Use the descriptor from the previous case.
C
C
C     Shift the vertex so the ray just misses the volume element on
C     the +X side.
C
      DELTA = 1.D-12

      CALL VPACK ( (L/2) + DELTA, 0.D0, 2*H, VERTEX )

      CALL VPACK ( 0.D0, 0.D0, -1.D0, RAYDIR )

      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect not to find an intercept.
C      
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Rectangular coordinate hit case. Positive '
     .//           'MARGIN.' )
 
C
C     Use the descriptor from the previous case.
C
C
C     Shift the vertex so the ray just misses the volume element on
C     the +X side, but hits the extended element.
C
      DELTA = 1.D-12 

      CALL VPACK ( (L/2) + DELTA, 0.D0, 2*H, VERTEX )

      CALL VPACK ( 0.D0, 0.D0, -1.D0, RAYDIR )

      MARGIN = 2 * DELTA
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an intercept.
C      
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C     Create the expected intercept point.
C
      CALL VPACK ( VERTEX(1), 0.D0, (H/2)+(H*MARGIN), XXPT )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Rectangular coordinate miss case. Positive '
     .//           'MARGIN.' )
 
C
C     Use the descriptor from the previous case.
C
C
C     Shift the vertex so the ray just misses the volume element on
C     the +X side, but hits the extended element.
C
      DELTA = 1.D-12 

      CALL VPACK ( (L/2) + DELTA*L, 0.D0, 2*H, VERTEX )

      CALL VPACK ( 0.D0, 0.D0, -1.D0, RAYDIR )

      MARGIN = DELTA / 2
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We don't expect an intercept.
C      
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

 

C***********************************************************************
C
C
C     Latitudinal coordinates
C
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Latitudinal coordinate hit case. Zero MARGIN.' )
 
C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) = -PI()/6
      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/3
      BOUNDS(1,3) =  1.D3
      BOUNDS(2,3) =  2.D3

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX) = LATSYS

      CALL LATREC ( 1.D6, -PI()/4, PI()/4, VERTEX )
      CALL VMINUS ( VERTEX,                RAYDIR )

      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an intercept.
C      
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C     Create the expected intercept point.
C
      CALL LATREC ( BOUNDS(2,3), -PI()/4, PI()/4,  XXPT )
      
      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Latitudinal coordinate miss case. Zero MARGIN.' )
 
C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) = -PI()/6
      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/3
      BOUNDS(1,3) =  1.D3
      BOUNDS(2,3) =  2.D3

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX) = LATSYS

      CALL LATREC ( 1.D6, 0.D0, PI()/4, VERTEX )
      CALL VMINUS ( VERTEX,             RAYDIR )

      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We don't expect an intercept.
C      
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Latitudinal coordinate hit case. Positive MARGIN.' )
 

      MARGIN = 1.D-12

C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) = -PI()/6
      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/3
      BOUNDS(1,3) =  1.D3
      BOUNDS(2,3) =  2.D3

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX) = LATSYS

      DELTA = MARGIN/2

      CALL LATREC ( 1.D6, -PI()/4, (PI()/4) + DELTA, VERTEX )
      CALL VMINUS ( VERTEX,                          RAYDIR )

      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an intercept.
C      
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C     Create the expected intercept point.
C
      CALL LATREC ( BOUNDS(2,3), -PI()/4, PI()/4,  XXPT )
      
      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Latitudinal coordinate miss case. '
     .//           'Positive MARGIN.' )
 

      MARGIN = 1.D-12

C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) = -PI()/6
      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/3
      BOUNDS(1,3) =  1.D3
      BOUNDS(2,3) =  2.D3

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX) = LATSYS

      DELTA = MARGIN*2

      CALL LATREC ( 1.D6, -PI()/4, BOUNDS(2,2) + DELTA, VERTEX )
      CALL VMINUS ( VERTEX,                              RAYDIR )

      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We don't expect an intercept.
C      
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )




C***********************************************************************
C
C
C     Planetodetic coordinates
C
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Planetodetic coordinate hit case. Zero MARGIN.' )
 
C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CLEARD ( NSYPAR, CORPAR )

      RE        = 2.D3
      F         = 0.2D0
      
      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) = -PI()/6
      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/3
      BOUNDS(1,3) =  -10.D0
      BOUNDS(2,3) =   10.D0

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX)   = PDTSYS
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F

      LON = -PI()/4
      LAT =  PI()/4
      ALT =  1.D4

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LATREC (  1.D0, LON+PI(), -LAT, RAYDIR )

      
      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an intercept.
C      
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C     Create the expected intercept point. We must first get
C     the radii of the outer bounding ellipsoid.
C
      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELLBDS ( A,    B,    BOUNDS(2,3), BOUNDS(1,3), 
     .                AMAX, BMAX, AMIN,        BMIN        )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SURFPT ( VERTEX, RAYDIR, AMAX, AMAX, BMAX, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL CHCKSL ( 'SURFPT FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Planetodetic coordinate miss case. Zero MARGIN.' )
 

      LON = -PI()/4
      LAT =  0.D0
      ALT = 1.D6

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATREC (  1.D0, LON+PI(), -LAT, RAYDIR )

      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We don't expect an intercept.
C      
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Planetodetic coordinate hit case. '
     .//           'Positive MARGIN.'                  )
 
C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CLEARD ( NSYPAR, CORPAR )

      RE        = 2.D3
      F         = 0.2D0
      
      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) = -PI()/6
      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/3
      BOUNDS(1,3) =  -10.D0
      BOUNDS(2,3) =   10.D0

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX)   = PDTSYS
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F


      MARGIN = 1.D-12

      DELTA  = MARGIN/2

      LON    = -PI()/4
      LAT    = (PI()/3) + DELTA
      ALT    = 1.D6

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATREC (  1.D0, LON+PI(), -LAT, RAYDIR )

      
      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an intercept.
C      
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C     Create the expected intercept point. We must first get
C     the radii of the outer bounding ellipsoid.
C
      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELLBDS ( A,    B,    BOUNDS(2,3), BOUNDS(1,3), 
     .                AMAX, BMAX, AMIN,        BMIN        )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFPT ( VERTEX, RAYDIR, AMAX, AMAX, BMAX, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL CHCKSL ( 'SURFPT FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Planetodetic coordinate miss case. '
     .//           'Positive MARGIN.'                  )
 
C
C     Set the coordinate system and bounds in the DSK descriptor.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CLEARD ( NSYPAR, CORPAR )

      RE        = 2.D3
      F         = 0.2D0
      
      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) = -PI()/6
      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/3
      BOUNDS(1,3) =  -10.D0
      BOUNDS(2,3) =   10.D0

      CALL MOVED ( BOUNDS, 6, DSKDSC(MN1IDX) )

      DSKDSC(SYSIDX)   = PDTSYS
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F


      MARGIN = 1.D-12

      DELTA  = MARGIN * 2

      LON    = -PI()/4
      LAT    = (PI()/3) + DELTA
      ALT    = 1.D6

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATREC (  1.D0, LON+PI(), -LAT, RAYDIR )

      
      MARGIN = 0.D0
      
      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We don't expect an intercept.
C      
      XNXPTS = 0


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate system.'  )


      DSKDSC( SYSIDX ) = -1

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC,
     .                MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .TRUE., 'SPICE(BADCOORDSYS)', OK )



C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


