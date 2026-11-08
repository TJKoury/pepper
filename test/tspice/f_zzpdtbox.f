C$Procedure F_ZZPDTBOX ( ZZPDTBOX tests )

      SUBROUTINE F_ZZPDTBOX ( OK )

C$ Abstract
C
C     Exercise the planetodetic volume element bounding
C     box routine ZZPDTBOX.
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
C     This routine tests the DSK support routine ZZPDTBOX.
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
C-    TSPICE Version 1.1.0, 22-SEP-2021 (NJB)
C
C        Loosened TIGHT tolerance to 1.D-11.
C
C-    TSPICE Version 1.0.0, 04-JUN-2016 (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )


C
C     Local Variables
C
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      PMAX   ( 3 )
      DOUBLE PRECISION      PMAXXY ( 3 )
      DOUBLE PRECISION      PMIN   ( 3 )
      DOUBLE PRECISION      PMINXY ( 3 )
      DOUBLE PRECISION      PXY    ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XLR
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XLZ
      DOUBLE PRECISION      XRAD

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZPDTBOX' )


C**********************************************************************
C
C     Error cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad longitude range: max is less than min '
     .//           'by more than 2*pi.'                        )


      CALL CLEARD ( NSYPAR, CORPAR )

      RE = 3.D3
      F  = 0.5D0

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = -TWOPI() - 1.D-15

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLONGITUDERANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Latitude bounds out of order' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()
      BOUNDS(2,2) = -HALFPI()

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDEBOUNDS)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Minimum latitude is too small' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI() - 1.D-10
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDERANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Maximum latitude is too large' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI() + 1.D-10

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDERANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid shape parameters.' )



      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = -10.D0
      BOUNDS(2,3) =  20.D0


      RE = 0.D3
      F  = 0.D0

      CORPAR(1) = RE
      CORPAR(2) = F

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      CORPAR(2) = 1.5D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




C**********************************************************************
C
C     Normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Trivial case: element is spherical shell.' )

      RE = 3.D3
      F  = 0.D0

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = -10.D0
      BOUNDS(2,3) =  20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XRAD = ( RE + BOUNDS(2,3) ) * SQRT(3.D0)

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

      XLR = 2 * ( RE + BOUNDS(2,3) )
      XLT = XLR
      XLZ = XLR

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is north polar cap with zero thickness.' )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = HALFPI()/2
      BOUNDS(2,2) = HALFPI()

      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0

C
C     Find the coordinates of the bound of minimum longitude,
C     minimum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(1,2), BOUNDS(2,3), RE, F, P )

      CALL VEQU ( P, PXY )
      PXY(3) = 0.D0


      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )

      XCTR(3) = ( P(3) + (RP + BOUNDS(2,3)) ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = VNORM(PXY) * 2
      XLT = XLR
      XLZ = ( RP + BOUNDS(2,3) ) - P(3)

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is north polar cap with zero thickness. '
     .//           'Prolate case.'                                  )


      RE =  3.0D3
      F  = -0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = HALFPI()/2
      BOUNDS(2,2) = HALFPI()

      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0

C
C     Find the coordinates of the bound of minimum longitude,
C     minimum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(1,2), BOUNDS(2,3), RE, F, P )

      CALL VEQU ( P, PXY )
      PXY(3) = 0.D0


      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )

      XCTR(3) = ( P(3) + (RP + BOUNDS(2,3)) ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = VNORM(PXY) * 2
      XLT = XLR
      XLZ = ( RP + BOUNDS(2,3) ) - P(3)

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is north polar cap with non-zero '
     .//           'thickness.' )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()/2
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

C
C     Find the coordinates of the bound of minimum longitude,
C     minimum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(1,2), BOUNDS(2,3), RE, F, PMAX )

      CALL VEQU ( PMAX, PMAXXY )
      PMAXXY(3) = 0.D0


C
C     Find the coordinates of the bound of minimum longitude,
C     minimum latitude, and minimum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(1,2), BOUNDS(1,3), RE, F, PMIN)

      CALL VEQU ( PMIN, PMINXY )
      PMINXY(3) = 0.D0


      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CLEARD ( 3, XCTR )
      XCTR(3) = ( PMIN(3) +  ( RP + BOUNDS(2,3) ) ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = VNORM(PMAXXY) * 2
      XLT = XLR
      XLZ = ( RP + BOUNDS(2,3) ) - PMIN(3)

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is north polar cap with non-zero '
     .//           'thickness. Prolate case.' )


      RE =  3.0D3
      F  = -0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()/2
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

C
C     Find the coordinates of the bound of minimum longitude,
C     minimum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(1,2), BOUNDS(2,3), RE, F, PMAX )

      CALL VEQU ( PMAX, PMAXXY )
      PMAXXY(3) = 0.D0


C
C     Find the coordinates of the bound of minimum longitude,
C     minimum latitude, and minimum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(1,2), BOUNDS(1,3), RE, F, PMIN)

      CALL VEQU ( PMIN, PMINXY )
      PMINXY(3) = 0.D0


      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CLEARD ( 3, XCTR )
      XCTR(3) = ( PMIN(3) +  ( RP + BOUNDS(2,3) ) ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = VNORM(PMAXXY) * 2
      XLT = XLR
      XLZ = ( RP + BOUNDS(2,3) ) - PMIN(3)

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is south polar cap with zero thickness.' )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2

      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0


C
C     Find the coordinates of the bound of minimum longitude,
C     maximum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(2,2), BOUNDS(2,3), RE, F, P )

      CALL VEQU ( P, PXY )
      PXY(3) = 0.D0


      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = ( P(3) + (-RP - BOUNDS(2,3)) ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )



      XLR = 2 * VNORM(PXY)
      XLT = XLR
      XLZ = 2 * ABS( XCTR(3) - P(3) )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is south polar cap with zero thickness. '
     .//           'Prolate case.'                                   )


      RE =  3.0D3
      F  = -0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2

      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0


C
C     Find the coordinates of the bound of minimum longitude,
C     maximum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(2,2), BOUNDS(2,3), RE, F, P )

      CALL VEQU ( P, PXY )
      PXY(3) = 0.D0


      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = ( P(3) + (-RP - BOUNDS(2,3)) ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )



      XLR = 2 * VNORM(PXY)
      XLT = XLR
      XLZ = 2 * ABS( XCTR(3) - P(3) )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is south polar cap with non-zero '
     .//           'thickness.' )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2

      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

C
C     Find the coordinates of the bound of minimum longitude,
C     maximum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(2,2), BOUNDS(2,3), RE, F, PMAX )

      CALL VEQU ( PMAX, PMAXXY )
      PMAXXY(3) = 0.D0


C
C     Find the coordinates of the bound of minimum longitude,
C     maximum latitude, and minimum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(2,2), BOUNDS(1,3), RE, F, PMIN )

      CALL VEQU ( PMIN, PMINXY )
      PMINXY(3) = 0.D0



      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )

      XCTR(3) =  ( PMIN(3) + ( -RP -BOUNDS(2,3) )  ) / 2

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = 2 * VNORM( PMAXXY )
      XLT = XLR
      XLZ = PMIN(3) - ( -RP - BOUNDS(2,3) )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is south polar cap with non-zero '
     .//           'thickness. Prolate case.' )


      RE =  3.0D3
      F  = -0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2

      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

C
C     Find the coordinates of the bound of minimum longitude,
C     maximum latitude, and maximum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(2,2), BOUNDS(2,3), RE, F, PMAX )

      CALL VEQU ( PMAX, PMAXXY )
      PMAXXY(3) = 0.D0


C
C     Find the coordinates of the bound of minimum longitude,
C     maximum latitude, and minimum altitude.
C
      CALL GEOREC ( BOUNDS(1,1), BOUNDS(2,2), BOUNDS(1,3), RE, F, PMIN )

      CALL VEQU ( PMIN, PMINXY )
      PMINXY(3) = 0.D0



      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )

      XCTR(3) =  ( PMIN(3) + ( -RP -BOUNDS(2,3) )  ) / 2

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = 2 * VNORM( PMAXXY )
      XLT = XLR
      XLZ = PMIN(3) - ( -RP - BOUNDS(2,3) )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is chunk above the X-Y plane, '
     .//           'lying over the +X axis.'               )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) =  PI()/4
      BOUNDS(2,2) =  PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is chunk above the X-Y plane, '
     .//           'lying over the +X axis. Prolate case.'  )


      RE =  3.0D3
      F  = -0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) =  PI()/4
      BOUNDS(2,2) =  PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is chunk below the X-Y plane, '
     .//           'lying under the +X axis.'               )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is chunk below the X-Y plane, '
     .//           'lying under the +X axis. Prolate case.' )


      RE =  3.0D3
      F  = -0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is chunk above the X-Y plane, '
     .//           'lying over the -X axis. Longitude extent '
     .//           'is > pi.'                                 )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) =  PI()/3
      BOUNDS(2,1) = -PI()/3

      BOUNDS(1,2) =  PI()/4
      BOUNDS(2,2) =  PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is chunk below the X-Y plane, '
     .//           'lying under the -X axis. Longitude extent '
     .//           'is > pi.'                                 )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) =  PI()/3
      BOUNDS(2,1) = -PI()/3

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =  -PI()/4

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is an equatorial belt that is '
     .//           'symmetric about the X-Y plane.'        )



      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =   PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is an equatorial belt that is '
     .//           'asymmetric about the X-Y plane.'        )


      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is a slice ranging from the north '
     .//           'to the south pole. Longitude extent is < pi.' )



      RE = 3.0D3
      F  = 0.5D0
      RP = RE * ( 1.D0 - F )

      CORPAR(1) = RE
      CORPAR(2) = F


      BOUNDS(1,1) =   3*PI()/4
      BOUNDS(2,1) =  -3*PI()/4

      BOUNDS(1,2) =  -PI()/2
      BOUNDS(2,2) =   PI()/2

      BOUNDS(1,3) =    1.D0
      BOUNDS(2,3) =   20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Element is a slice ranging from the north '
     .//           'to the south pole. Longitude extent is > pi.' )

      BOUNDS(1,1) =    PI()/4
      BOUNDS(2,1) =  - PI()/4

      BOUNDS(1,2) =  -PI()/2
      BOUNDS(2,2) =   PI()/2

      BOUNDS(1,3) =    1.D0
      BOUNDS(2,3) =   20.D0

      CALL ZZPDTBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C
      CALL T_TSTPBX ( BOUNDS, CORPAR, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END


C
C     Utility routine T_TSTPBX: create planetodetic bounding
C     box for testing. The bounds of segment may span the
C     X-Y plane.
C
      SUBROUTINE T_TSTPBX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( * )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'T_TSTPBX' )

C
C     Local variables
C
      DOUBLE PRECISION      CTR    ( 3, 2 )
      DOUBLE PRECISION      DIAG   ( 3 )
      DOUBLE PRECISION      EXTENT ( 3, 2 )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      LOCBDS ( 2, 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXRC
      DOUBLE PRECISION      MAXZ
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINRC
      DOUBLE PRECISION      MINZ
      DOUBLE PRECISION      R
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2

      INTEGER               I



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )

      MINLAT = BOUNDS(1,2)
      MAXLAT = BOUNDS(2,2)

      IF (      ( MINLAT .GE. 0.D0 )
     .     .OR. ( MAXLAT .LT. 0.D0 ) ) THEN
C
C        The volume element doesn't cross the X-Y plane.
C        We can delegate the job.
C
         CALL T_MKPBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )

         CALL CHKOUT ( RNAME )
         RETURN

      END IF

C
C     The volume element crosses the X-Y plane. Divide the
C     element into upper and lower parts; find bounding boxes
C     for each.
C
      DO I = 1, 2

         CALL MOVED ( BOUNDS, 6, LOCBDS )
         LOCBDS(I,2) = 0.D0

         CALL T_MKPBOX ( LOCBDS,      CORPAR,      CTR(1,I),
     .                   EXTENT(1,I), EXTENT(2,I), EXTENT(3,I), R )

      END DO

C
C     At this point the longitude of the center and the tangential
C     extent of the box are known. We'll need to recompute the
C     radius and Z component of the center and the radial and Z
C     extents of the box.
C
      LT  =  EXTENT(2,1)

C
C     Start with the Z values. Recall that the two boxes
C     are bounded by the X-Y plane, which simplifies our
C     calculations. The box at index 1 is the upper box.
C
      MAXZ      =  EXTENT(3,1)
      MINZ      = -EXTENT(3,2)
      LZ        =  MAX ( 0.D0,  MAXZ - MINZ )
      CENTER(3) =  MAXZ - (LZ/2)

C
C     Both boxes have the same maximum radial component, so we can
C     compute this component using the first box alone. The minimum
C     radial component must be derived using both boxes.
C
C     Note the radial component of the box center is always positive,
C     and the longitude of the box center is always the same as that of
C     the midpoint of the center of any arc of constant latitude on
C     the outer bounding surface.
C
      CALL RECCYL ( CTR(1,1), R1, LON, H )

      MAXRC     = R1  +  EXTENT(1,1)/2

      CALL RECCYL ( CTR(1,2), R2, LON, H )


      MINRC     = MIN(  R1 - EXTENT(1,1)/2,
     .                  R2 - EXTENT(1,2)/2  )

      LR        = MAX( 0.D0,  MAXRC - MINRC )

      H         = CENTER(3)

      CALL CYLREC ( MAXRC - LR/2,  LON,  H,  CENTER )

      CALL VPACK ( LT/2, LR/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      CALL CHKOUT ( RNAME )
      RETURN
      END



C
C     Utility routine T_MKPBOX: create box using coordinates
C     of the corners of a volume element. This technique
C     is valid only for "small" elements, for which the
C     bounding box is not tangent at interior points of
C     the element's surface. Note that this routine cannot
C     be used for elements that cross the X-Y plane.
C
      SUBROUTINE T_MKPBOX ( BOUNDS, CORPAR, CENTER, LR, LT, LZ, RADIUS )
      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( * )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'T_MKPBOX' )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

C
C     Local variables
C
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      CORNER ( 3, 2, 2, 2 )
      DOUBLE PRECISION      CPLANE ( UBPL )
      DOUBLE PRECISION      DIAG   ( 3 )
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXRC
      DOUBLE PRECISION      MAXZ
      DOUBLE PRECISION      MIDARC ( 3 )
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDPT  ( 3 )
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINRC
      DOUBLE PRECISION      MINZ
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADVEC ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               K

      SAVE                  Z

      DATA                  Z / 0.D0, 0.D0, 1.D0 /

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )

      RE     = CORPAR(1)
      F      = CORPAR(2)

      MINLAT = BOUNDS(1,2)

      MINLON = BOUNDS(1,1)
      MAXLON = BOUNDS(2,1)

      IF ( MAXLON .LE. MINLON ) THEN
         MAXLON = MAXLON + TWOPI()
      END IF

      DLON   = MAXLON - MINLON
      MIDLON = MINLON + DLON/2

C
C     Compute Cartesian coordinates of the box corners.
C
      DO I = 1, 2

         LON = BOUNDS(I,1)

         DO J = 1, 2

            LAT = BOUNDS(J,2)

            DO K = 1, 2

               ALT = BOUNDS(K,3)

               CALL GEOREC ( LON, LAT, ALT, RE, F, CORNER(1,I,J,K) )

            END DO

         END DO

      END DO

C
C     Pick the latitude of the segment so that its length
C     is the transverse length of the box.
C
      IF ( MINLAT .GE. 0.D0 ) THEN
C
C        Use the lower pair of corners on the surface of maximum
C        radius.
C
         I = 1
      ELSE
         I = 2
      END IF

C
C     Find the midpoint of the latitude boundary of maximum
C     altitude that is closest to the X-Y plane. This is the
C     point with the greatest component in the radial direction.
C
      CALL GEOREC ( MIDLON, BOUNDS(I,2), BOUNDS(2,3), RE, F, MIDARC )

C
C     Create a central plane for the box: this plane contains
C     the midpoint of each line segment connecting a pair of
C     corners at the same latitude and altitude, and is normal
C     to each such line segment.

      CALL VSUB ( CORNER(1,2,I,2), CORNER(1,1,I,2), NORMAL )

      CALL VLCOM ( 0.5D0, CORNER(1,2,1,2),
     .             0.5D0, CORNER(1,1,1,2), MIDPT )

      CALL NVP2PL ( NORMAL, MIDPT, CPLANE )

C
C     Compute the radius of one of the corners at latitude index I.
C     We'll use this later.
C
      CALL RECLAT ( CORNER(1,2,I,2), R, LON, LAT )


      IF ( DLON .LE. PI() ) THEN
C
C        The segment connecting the two corners selected above
C        has length equal to the tangential extent of the element.
C
         LT = VNORM( NORMAL )

      ELSE
C
C        The widest part of the element is at the longitudes 90 degrees
C        away from MIDLON. The width is just the width of the outer
C        circle formed by projecting the element orthogonally onto the
C        X-Y plane.
C
         CALL VEQU ( CORNER(1,2,I,2), P )
         P(3) = 0.D0


         LT = 2.D0 * VNORM(P)

      END IF

C
C     For each corner on the volume face of minimum longitude,
C     compute the minimum and maximum Z values of the corners.
C
      MINZ = DPMAX()
      MAXZ = DPMIN()

      DO J = 1, 2

         DO K = 1, 2

            MINZ = MIN( MINZ, CORNER(3,1,J,K) )
            MAXZ = MAX( MAXZ, CORNER(3,1,J,K) )

         END DO

      END DO

C
C     Compute the Z extent of the element.
C
      LZ = MAX( MAXZ - MINZ,  0.D0 )

C
C     Let RADVEC be a unit vector in the radial direction.
C
      CALL UCRSS ( NORMAL, Z, RADVEC )

C
C     Find the component of MIDARC in the RADVEC (radial) direction.
C
      MAXRC = VDOT ( MIDARC, RADVEC )


      IF ( DLON .LT. PI() ) THEN
C
C        For the corners on the intersection of the volume face of
C        minimum longitude and surface of minimum altitude, compute the
C        corners' components in the radial direction. Select the
C        minimum component.
C
         MINRC = MIN(  VDOT( CORNER(1,1,1,1), RADVEC ),
     .                 VDOT( CORNER(1,1,2,1), RADVEC )  )

      ELSE
C
C        The element "wraps" around the Z axis; the points having
C        minimum tangential components lie on the surface of
C        maximum altitude.
C
         MINRC = MIN(  VDOT( CORNER(1,1,1,2), RADVEC ),
     .                 VDOT( CORNER(1,1,2,2), RADVEC )  )

      END IF

C
C     We now have the extent of the element in the radial direction.
C
      LR = MAX( MAXRC - MINRC,  0.D0 )

C
C     MIDARC lies on the central, vertical plane of the box, at the end
C     of maximum tangential component. We can derive the coordinates of
C     the center of the box from MIDARC, MINZ, and the box extents.
C
      CALL CYLREC( MAXRC - (LR/2),   MIDLON,   MINZ + (LZ/2), CENTER )


      CALL VPACK ( LT/2, LR/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      CALL CHKOUT ( RNAME )
      RETURN
      END
