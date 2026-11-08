!$Procedure F_ZZSEGBOX ( ZZSEGBOX tests )

      SUBROUTINE F_ZZSEGBOX ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine ZZSEGBOX.
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
      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'

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
!     This routine tests the SPICELIB routine ZZSEGBOX.
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
!-    TSPICE Version 1.0.0, 11-FEB-2017 (NJB)
!
!        11-JUL-2016 (NJB)
!
!          Original version.
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI

      LOGICAL               EXISTS

!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               NDSKS
      PARAMETER           ( NDSKS  = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

!
!     Local Variables
!
      CHARACTER*(FILSIZ)    DSKS   ( NDSKS )
      CHARACTER*(NAMLEN)    FRNAME

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      BOXCTR ( 3 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      MAXV   ( 3 )
      DOUBLE PRECISION      MINV   ( 3 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XRAD

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NV
      INTEGER               SURFID

      LOGICAL               FOUND
      LOGICAL               MAKVTL
      LOGICAL               USEPAD

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
      CALL TOPEN ( 'F_ZZSEGBOX' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check bounding sphere for segment using the ' &
      //           'Latitudinal coordinate system.'               )


!
!     Strictly speaking, we don't need an actual DSK file; we just
!     need a DSK descriptor. However, we'll create a file in order
!     to work with a realistic descriptor.
!

      BODYID = 499
      SURFID = 1
      FRNAME = 'IAU_MARS'
      FIRST  = -100*JYEAR()
      LAST   =  100*JYEAR()
      CORSYS = LATSYS

      A       = 3400.D0
      B       = A
      C       = 3375.D0

      CALL CLEARD ( NSYPAR, CORPAR )

!
!     Longitude bounds:
!
      BOUNDS(1,1) = PI()/6
      BOUNDS(2,1) = PI()/3

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

!
!     Radius bounds are not used by T_SECDS2.
!
      BOUNDS(1,3) = 0.D0
      BOUNDS(2,3) = 0.D0


      NLON    = 100
      NLAT    =  50

      MAKVTL  = .FALSE.
      USEPAD  = .TRUE.

      DSKS(1) = 'zzsegbox_lat.bds'

      IF ( EXISTS(DSKS(1)) ) THEN

         CALL DELFIL( DSKS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

!
!     Create the DSK file.
!
      CALL T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, USEPAD, &
                      DSKS(1)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Fetch the DSK descriptor for the segment we just created.
!
      CALL DASOPR ( DSKS(1), HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA segment found', FOUND, .TRUE., OK )

      CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the segment's radius bounds from the DSK descriptor.
!
      BOUNDS(1,3) = DSKDSC(MN3IDX)
      BOUNDS(2,3) = DSKDSC(MX3IDX)

!
!     Create a bounding box and sphere for the segment using both
!     ZZSEGBOX and ZZLATBOX. The results should match.
!
      CALL ZZSEGBOX ( DSKDSC, BOXCTR, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZLATBOX ( BOUNDS, XCTR, LR, LT, LZ, XRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'RADIUS', RADIUS, '~/',  XRAD,    TOL, OK )
      CALL CHCKAD ( 'BOXCTR', BOXCTR, '~~/', XCTR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check bounding sphere for segment using the ' &
      //           'Planetodetic coordinate system.'               )


!
!     Strictly speaking, we don't need an actual DSK file; we just
!     need a DSK descriptor. However, we'll create a file in order
!     to work with a realistic descriptor.
!

      BODYID = 599
      SURFID = 1
      FRNAME = 'IAU_JUPITER'
      FIRST  = -100*JYEAR()
      LAST   =  100*JYEAR()
      CORSYS = PDTSYS

      A       = 71492.D0
      B       = A
      C       = 66854.D0

      F       = (A-C) / A


      CORPAR(1) = A
      CORPAR(2) = F

      CALL CLEARD ( NSYPAR-2, CORPAR(3) )

!
!     Longitude bounds:
!
      BOUNDS(1,1) = PI()/6
      BOUNDS(2,1) = PI()/3

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

!
!     Altitude bounds are not used by T_SECDS2.
!
      BOUNDS(1,3) = 0.D0
      BOUNDS(2,3) = 0.D0



      NLON    = 100
      NLAT    =  50

      MAKVTL  = .FALSE.
      USEPAD  = .FALSE.

      DSKS(2) = 'zzsegbox_pdt.bds'

      IF ( EXISTS(DSKS(2)) ) THEN

         CALL DELFIL( DSKS(2) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

!
!     Create the DSK file.
!
      CALL T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, USEPAD, &
                      DSKS(2)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Fetch the DSK descriptor for the segment we just created.
!
      CALL DASOPR ( DSKS(2), HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA segment found', FOUND, .TRUE., OK )

      CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the segment's altitude bounds from the DSK descriptor.
!
      BOUNDS(1,3) = DSKDSC(MN3IDX)
      BOUNDS(2,3) = DSKDSC(MX3IDX)

!
!     Create a bounding box and sphere for the segment using both
!     ZZSEGBOX and ZZLATBOX. The results should match.
!
      CALL ZZSEGBOX ( DSKDSC, BOXCTR, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZPDTBOX ( BOUNDS, CORPAR, XCTR, LR, LT, LZ, XRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'RADIUS', RADIUS, '~/',  XRAD,    TOL, OK )
      CALL CHCKAD ( 'BOXCTR', BOXCTR, '~~/', XCTR, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check bounding sphere for segment using the ' &
      //           'Rectangular coordinate system.'               )


!
!     Strictly speaking, we don't need an actual DSK file; we just
!     need a DSK descriptor. However, we'll create a file in order
!     to work with a realistic descriptor.
!
!     We're going to start out as though we're using the latitudinal
!     system, since the plate set generation utilities work with a
!     lon/lat coverage specification.
!
      BODYID = 699
      SURFID = 1
      FRNAME = 'IAU_SATURN'
      FIRST  = -100*JYEAR()
      LAST   =  100*JYEAR()
      CORSYS = LATSYS

      A       = 60268.D0
      B       = A
      C       = 54364.D0

      CALL CLEARD ( NSYPAR, CORPAR )

!
!     Longitude bounds:
!
      BOUNDS(1,1) = PI()/6
      BOUNDS(2,1) = PI()/3

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

!
!     Radius bounds are not used by T_SECDS2.
!
      BOUNDS(1,3) = 0.D0
      BOUNDS(2,3) = 0.D0


      NLON    = 100
      NLAT    =  50

      MAKVTL  = .FALSE.
      USEPAD  = .TRUE.

      DSKS(3) = 'zzsegbox_rec.bds'

      IF ( EXISTS(DSKS(3)) ) THEN

         CALL DELFIL( DSKS(3) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

!
!     Create the DSK file.
!
      CALL T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, USEPAD, &
                      DSKS(1)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Fetch the DSK descriptor for the segment we just created.
!
      CALL DASOPR ( DSKS(1), HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA segment found', FOUND, .TRUE., OK )

      CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     Here's where we deviate from the latitudinal case. We're going
!     to convert the descriptor to one using rectangular coordinates.
!
!     Before closing the file, fetch the vertices of the file
!     and obtain the extents of the plate set.
!
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Let MINV(I) and MAXV(I) store the minimum and maximum values of
!     the Ith components of the vertices.
!
      DO I = 1, 3

         MINV(I) = DPMAX()
         MAXV(I) = DPMIN()

      END DO

      DO I = 1, NV

         CALL DSKV02 ( HANDLE, DLADSC, I, 1, N, VERTEX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            MINV(J) = MIN( VERTEX(J), MINV(J) )
            MAXV(J) = MAX( VERTEX(J), MAXV(J) )

         END DO

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Turn the descriptor into one for rectangular coordinates.
!
      DSKDSC( SYSIDX ) = RECSYS

      DSKDSC( MN1IDX ) = MINV(1)
      DSKDSC( MX1IDX ) = MAXV(1)
      DSKDSC( MN2IDX ) = MINV(2)
      DSKDSC( MX2IDX ) = MAXV(2)
      DSKDSC( MN3IDX ) = MINV(3)
      DSKDSC( MX3IDX ) = MAXV(3)

!
!     Create a bounding box and sphere for the segment using both
!     ZZSEGBOX and ZZLATBOX. The results should match.
!
      CALL ZZSEGBOX ( DSKDSC, BOXCTR, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, 3
         BOUNDS(1,I) = MINV(I)
         BOUNDS(2,I) = MAXV(I)
      END DO

      CALL ZZRECBOX ( BOUNDS, XCTR, LR, LT, LZ, XRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'RADIUS', RADIUS, '~/',  XRAD,    TOL, OK )
      CALL CHCKAD ( 'BOXCTR', BOXCTR, '~~/', XCTR, 3, TOL, OK )



!***********************************************************************
!
!     Error cases
!
!***********************************************************************



!
! ---------------------------------------------------------
!
      CALL TCASE ( 'Error: try segment using cylindrical coordinates.' )

!
!     Use descriptor from last case; change the coordinate system
!     parameter to indicate cylindrical coordinates.
!
      DSKDSC( SYSIDX ) = DBLE( CYLSYS )

      CALL ZZSEGBOX ( DSKDSC, BOXCTR, RADIUS )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
!     Clean up.
!

!
! ---------------------------------------------------------
!
      CALL TCASE ( 'Clean up' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NDSKS

         IF ( EXISTS(DSKS(I)) ) THEN

            CALL DELFIL( DSKS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


