!$Procedure F_ZZRYTELT ( ZZRYTELT tests )

      SUBROUTINE F_ZZRYTELT ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZRYTELT.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB routine ZZRYTELT.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 20-JUL-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!
!     Local Variables
!
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


!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZRYTELT' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!***********************************************************************
!
!
!     Rectangular coordinates
!
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Rectangular coordinate hit case. Zero MARGIN.' )

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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


      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an intercept.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!     Create the expected intercept point.
!
      CALL VPACK ( 0.D0, 0.D0, H/2, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Rectangular coordinate miss case. Zero ' &
      //           'MARGIN.' )

!
!     Use the descriptor from the previous case.
!
!
!     Shift the vertex so the ray just misses the volume element on
!     the +X side.
!
      DELTA = 1.D-12

      CALL VPACK ( (L/2) + DELTA, 0.D0, 2*H, VERTEX )

      CALL VPACK ( 0.D0, 0.D0, -1.D0, RAYDIR )

      MARGIN = 0.D0

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect not to find an intercept.
!
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Rectangular coordinate hit case. Positive ' &
      //           'MARGIN.' )

!
!     Use the descriptor from the previous case.
!
!
!     Shift the vertex so the ray just misses the volume element on
!     the +X side, but hits the extended element.
!
      DELTA = 1.D-12

      CALL VPACK ( (L/2) + DELTA, 0.D0, 2*H, VERTEX )

      CALL VPACK ( 0.D0, 0.D0, -1.D0, RAYDIR )

      MARGIN = 2 * DELTA

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an intercept.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!     Create the expected intercept point.
!
      CALL VPACK ( VERTEX(1), 0.D0, (H/2)+(H*MARGIN), XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Rectangular coordinate miss case. Positive ' &
      //           'MARGIN.' )

!
!     Use the descriptor from the previous case.
!
!
!     Shift the vertex so the ray just misses the volume element on
!     the +X side, but hits the extended element.
!
      DELTA = 1.D-12

      CALL VPACK ( (L/2) + DELTA*L, 0.D0, 2*H, VERTEX )

      CALL VPACK ( 0.D0, 0.D0, -1.D0, RAYDIR )

      MARGIN = DELTA / 2

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We don't expect an intercept.
!
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )



!***********************************************************************
!
!
!     Latitudinal coordinates
!
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Latitudinal coordinate hit case. Zero MARGIN.' )

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an intercept.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!     Create the expected intercept point.
!
      CALL LATREC ( BOUNDS(2,3), -PI()/4, PI()/4,  XXPT )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Latitudinal coordinate miss case. Zero MARGIN.' )

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We don't expect an intercept.
!
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Latitudinal coordinate hit case. Positive MARGIN.' )


      MARGIN = 1.D-12

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an intercept.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!     Create the expected intercept point.
!
      CALL LATREC ( BOUNDS(2,3), -PI()/4, PI()/4,  XXPT )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Latitudinal coordinate miss case. ' &
      //           'Positive MARGIN.' )


      MARGIN = 1.D-12

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We don't expect an intercept.
!
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )




!***********************************************************************
!
!
!     Planetodetic coordinates
!
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Planetodetic coordinate hit case. Zero MARGIN.' )

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an intercept.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!     Create the expected intercept point. We must first get
!     the radii of the outer bounding ellipsoid.
!
      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELLBDS ( A,    B,    BOUNDS(2,3), BOUNDS(1,3), &
                      AMAX, BMAX, AMIN,        BMIN        )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SURFPT ( VERTEX, RAYDIR, AMAX, AMAX, BMAX, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Planetodetic coordinate miss case. Zero MARGIN.' )


      LON = -PI()/4
      LAT =  0.D0
      ALT = 1.D6

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATREC (  1.D0, LON+PI(), -LAT, RAYDIR )

      MARGIN = 0.D0

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We don't expect an intercept.
!
      XNXPTS = 0

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Planetodetic coordinate hit case. ' &
      //           'Positive MARGIN.'                  )

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an intercept.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!     Create the expected intercept point. We must first get
!     the radii of the outer bounding ellipsoid.
!
      A = RE
      B = RE * ( 1.D0 - F )

      CALL ZZELLBDS ( A,    B,    BOUNDS(2,3), BOUNDS(1,3), &
                      AMAX, BMAX, AMIN,        BMIN        )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFPT ( VERTEX, RAYDIR, AMAX, AMAX, BMAX, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Planetodetic coordinate miss case. ' &
      //           'Positive MARGIN.'                  )

!
!     Set the coordinate system and bounds in the DSK descriptor.
!
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

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We don't expect an intercept.
!
      XNXPTS = 0


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate system.'  )


      DSKDSC( SYSIDX ) = -1

      CALL ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                      MARGIN, NXPTS,  XPT    )
      CALL CHCKXC ( .TRUE., 'SPICE(BADCOORDSYS)', OK )



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


