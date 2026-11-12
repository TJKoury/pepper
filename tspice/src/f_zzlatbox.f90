!$Procedure F_ZZLATBOX ( ZZLATBOX tests )

      SUBROUTINE F_ZZLATBOX ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the latitudinal volume element bounding
!     box routine ZZLATBOX.
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
!     This routine tests the DSK support routine ZZLATBOX.
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
!-    TSPICE Version 1.0.0, 03-FEB-2015 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


!
!     Local Variables
!
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XLR
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XLZ
      DOUBLE PRECISION      XRAD

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZLATBOX' )


!**********************************************************************
!
!     Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad longitude range: max is less than min ' &
      //           'by more than 2*pi.'                        )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = -TWOPI() - 1.D-15

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLONGITUDERANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Latitude bounds out of order' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()
      BOUNDS(2,2) = -HALFPI()

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDEBOUNDS)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Minimum latitude is too small' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI() - 1.D-10
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDERANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Maximum latitude is too large' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI() + 1.D-10

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDERANGE)', OK )




!**********************************************************************
!
!     Normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Trivial case: element is sphere.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XRAD = BOUNDS(2,3) * SQRT(3.D0)

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

      XLR = 2 * BOUNDS(2,3)
      XLT = XLR
      XLZ = XLR

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is north polar cap with zero thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()/2
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = BOUNDS(2,3) * (( 1.D0 + SQRT(2.D0)/2 ) / 2 )


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) * ( 1.D0 - SQRT(2.D0)/2 )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is north polar cap with non-zero ' &
      //           'thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()/2
      BOUNDS(2,2) =  HALFPI()

      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = (  BOUNDS(2,3) + ( BOUNDS(1,3)*SQRT(2.D0)/2 )  ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) - ( BOUNDS(1,3)*SQRT(2.D0)/2 )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is south polar cap with zero thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2

      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = -BOUNDS(2,3) * (( 1.D0 + SQRT(2.D0)/2 ) / 2 )


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) * ( 1.D0 - SQRT(2.D0)/2 )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is south polar cap with non-zero ' &
      //           'thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2

      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = -(  BOUNDS(2,3) + ( BOUNDS(1,3)*SQRT(2.D0)/2 )  ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) - ( BOUNDS(1,3)*SQRT(2.D0)/2 )

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is chunk above the X-Y plane, ' &
      //           'lying over the +X axis.'               )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) =  PI()/4
      BOUNDS(2,2) =  PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is chunk below the X-Y plane, ' &
      //           'lying under the +X axis.'               )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is chunk above the X-Y plane, ' &
      //           'lying over the -X axis. Longitude extent ' &
      //           'is > pi.'                                 )


      BOUNDS(1,1) =  PI()/3
      BOUNDS(2,1) = -PI()/3

      BOUNDS(1,2) =  PI()/4
      BOUNDS(2,2) =  PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is chunk below the X-Y plane, ' &
      //           'lying under the -X axis. Longitude extent ' &
      //           'is > pi.'                                 )


      BOUNDS(1,1) =  PI()/3
      BOUNDS(2,1) = -PI()/3

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =  -PI()/4

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is an equatorial belt that is ' &
      //           'symmetric about the X-Y plane.'        )


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =   PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is an equatorial belt that is ' &
      //           'asymmetric about the X-Y plane.'        )


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/3

      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is a slice ranging from the north ' &
      //           'to the south pole. Longitude extent is < pi.' )

      BOUNDS(1,1) =   3*PI()/4
      BOUNDS(2,1) =  -3*PI()/4

      BOUNDS(1,2) =  -PI()/2
      BOUNDS(2,2) =   PI()/2

      BOUNDS(1,3) =    1.D0
      BOUNDS(2,3) =   20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is a slice ranging from the north ' &
      //           'to the south pole. Longitude extent is > pi.' )

      BOUNDS(1,1) =    PI()/4
      BOUNDS(2,1) =  - PI()/4

      BOUNDS(1,2) =  -PI()/2
      BOUNDS(2,2) =   PI()/2

      BOUNDS(1,3) =    1.D0
      BOUNDS(2,3) =   20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected outputs.
!
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


!
!     Utility routine T_TSTBOX: create latitude bounding
!     box for testing. The bounds of segment may span the
!     X-Y plane.
!
      SUBROUTINE T_TSTBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      use pepper_legacy
      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

!
!     SPICELIB Functions
!


!
!     Local parameters
!
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'T_TSTBOX' )

!
!     Local variables
!
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

      IF (      ( MINLAT .GE. 0.D0 ) &
           .OR. ( MAXLAT .LT. 0.D0 ) ) THEN
!
!        The volume element doesn't cross the X-Y plane.
!        We can delegate the job.
!
         CALL T_MKBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

         CALL CHKOUT ( RNAME )
         RETURN

      END IF

!
!     The volume element crosses the X-Y plane. Divide the
!     element into upper and lower parts; find bounding boxes
!     for each.
!
      DO I = 1, 2

         CALL MOVED ( BOUNDS, 6, LOCBDS )

         LOCBDS(I,2) = 0.D0

         CALL T_MKBOX ( LOCBDS,      CTR(1,I),    EXTENT(1,I), &
                        EXTENT(2,I), EXTENT(3,I), R           )

      END DO

!
!     At this point the longitude of the center and the tangential
!     extent of the box are known. We'll need to recompute the
!     radius and Z component of the center and the radial and Z
!     extents of the box.
!
      LT  =  EXTENT(2,1)

!
!     Start with the Z values. Recall that the two boxes
!     are bounded by the X-Y plane, which simplifies our
!     calculations. The box at index 1 is the upper box.
!
      MAXZ      =  EXTENT(3,1)
      MINZ      = -EXTENT(3,2)
      LZ        =  MAX ( 0.D0,  MAXZ - MINZ )
      CENTER(3) =  MAXZ - (LZ/2)

!
!     Both boxes have the maximum radial component, so we can
!     compute this component using the first box alone. The
!     minimum radial component must be derived using both boxes.
!
!     Note the radial component of the box center is always
!     positive, and the longitude of the box center is always
!     the same as that of the midpoint of the centers of any
!     arc of constant latitude on the outer bounding sphere.
!
      CALL RECCYL ( CTR(1,1), R1, LON, H )

      MAXRC     = R1  +  EXTENT(1,1)/2

      CALL RECCYL ( CTR(1,2), R2, LON, H )


      MINRC     = MIN(  R1 - EXTENT(1,1)/2, &
                        R2 - EXTENT(1,2)/2  )

      LR        = MAX( 0.D0,  MAXRC - MINRC )

      H         = CENTER(3)

      CALL CYLREC ( MAXRC - LR/2,  LON,  H,  CENTER )

      CALL VPACK ( LT/2, LR/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      CALL CHKOUT ( RNAME )
      RETURN
      END



!
!     Utility routine T_MKBOX: create box using coordinates
!     of the corners of a volume element. This technique
!     is valid only for "small" elements, for which the
!     bounding box is not tangent at interior points of
!     the elements' surface. Note that this routine cannot
!     be used for elements that cross the X-Y plane.
!
      SUBROUTINE T_MKBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      use pepper_legacy
      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

!
!     SPICELIB Functions
!


!
!     Local parameters
!
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'T_MKBOX' )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

!
!     Local variables
!
      DOUBLE PRECISION      CORNER ( 3, 2, 2, 2 )
      DOUBLE PRECISION      CPLANE ( UBPL )
      DOUBLE PRECISION      DIAG   ( 3 )
      DOUBLE PRECISION      DLON
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
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADVEC ( 3 )
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

      MINLAT = BOUNDS(1,2)

      MINLON = BOUNDS(1,1)
      MAXLON = BOUNDS(2,1)

      IF ( MAXLON .LE. MINLON ) THEN
         MAXLON = MAXLON + TWOPI()
      END IF

      DLON   = MAXLON - MINLON
      MIDLON = MINLON + DLON/2

!
!     Compute Cartesian coordinates of the box corners.
!
      DO I = 1, 2

         LON = BOUNDS(I,1)

         DO J = 1, 2

            LAT = BOUNDS(J,2)

            DO K = 1, 2

               R = BOUNDS(K,3)

               CALL LATREC ( R, LON, LAT, CORNER(1,I,J,K) )

            END DO

         END DO

      END DO

!
!     Pick the latitude of the segment so that its length
!     is the transverse length of the box.
!
      IF ( MINLAT .GE. 0.D0 ) THEN
!
!        Use the lower pair of corners on the sphere of maximum
!        radius.
!
         I = 1
      ELSE
         I = 2
      END IF

!
!     Find the midpoint of the latitude boundary of maximum
!     radius that is closest to the X-Y plane. This is the
!     point with the greatest component in the radial direction.
!
      CALL LATREC ( BOUNDS(2,3), MIDLON, BOUNDS(I,2), MIDARC )

!
!     Create a central plane for the box: this plane contains
!     the midpoint of a line segment connecting a pair of
!     corners at the same latitude and radius, and is normal
!     to the segment.

      CALL VSUB ( CORNER(1,2,I,2), CORNER(1,1,I,2), NORMAL )

      CALL VLCOM ( 0.5D0, CORNER(1,2,1,2), &
                   0.5D0, CORNER(1,1,1,2), MIDPT )

      CALL NVP2PL ( NORMAL, MIDPT, CPLANE )


      IF ( DLON .LE. PI() ) THEN
!
!        The segment connecting the two corners selected above
!        has length equal to the tangential extent of the element.
!
         LT = VNORM( NORMAL )

      ELSE
!
!        The widest part of the element is at the longitudes 90 degrees
!        away from MIDLON. The width is just the width of the outer
!        circle formed by projecting the element orthogonally onto the
!        X-Y plane.
!
         LT = 2.D0 * BOUNDS(2,3) * COS( BOUNDS(I,2) )

      END IF

!
!     For each corner on the volume face of minimum longitude,
!     compute the minimum and maximum Z values of the corners.
!
      MINZ = DPMAX()
      MAXZ = DPMIN()

      DO J = 1, 2

         DO K = 1, 2

            MINZ = MIN( MINZ, CORNER(3,1,J,K) )
            MAXZ = MAX( MAXZ, CORNER(3,1,J,K) )

         END DO

      END DO

!
!     Compute the Z extent of the element.
!
      LZ = MAX( MAXZ - MINZ,  0.D0 )

!
!     Let RADVEC be a unit vector in the radial direction.
!
      CALL UCRSS ( NORMAL, Z, RADVEC )

!
!     Find the component of MIDARC in the RADVEC (radial) direction.
!
      MAXRC = VDOT ( MIDARC, RADVEC )


      IF ( DLON .LT. PI() ) THEN
!
!        For the corners on the intersection of the volume face of
!        minimum longitude and sphere of minimum radius, compute the
!        corners' components in the radial direction. Select the
!        minimum component.
!
         MINRC = MIN(  VDOT( CORNER(1,1,1,1), RADVEC ), &
                       VDOT( CORNER(1,1,2,1), RADVEC )  )

      ELSE
!
!        The element "wraps" around the Z axis; the points having
!        minimum tangential components lie on the outer sphere.
!
         MINRC = MIN(  VDOT( CORNER(1,1,1,2), RADVEC ), &
                       VDOT( CORNER(1,1,2,2), RADVEC )  )

      END IF

!
!     We now have the extent of the element in the radial direction.
!
      LR = MAX( MAXRC - MINRC,  0.D0 )

!
!     MIDARC lies on the central, vertical plane of the box, at the end
!     of maximum tangential component. We can derive the coordinates of
!     the center of the box from MIDARC, MINZ, and the box extents.
!
      CALL CYLREC( MAXRC - (LR/2),   MIDLON,   MINZ + (LZ/2), CENTER )


      CALL VPACK ( LT/2, LR/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      CALL CHKOUT ( RNAME )
      RETURN
      END


