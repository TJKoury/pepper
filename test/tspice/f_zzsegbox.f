C$Procedure F_ZZSEGBOX ( ZZSEGBOX tests )
 
      SUBROUTINE F_ZZSEGBOX ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine ZZSEGBOX.
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
      INCLUDE 'dla.inc'
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
C     This routine tests the SPICELIB routine ZZSEGBOX.
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
C-    TSPICE Version 1.0.0, 11-FEB-2017 (NJB)
C
C        11-JUL-2016 (NJB)
C
C          Original version.
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI

      LOGICAL               EXISTS

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )
  
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               NDSKS
      PARAMETER           ( NDSKS  = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C
C     Local Variables
C
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
      CALL TOPEN ( 'F_ZZSEGBOX' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check bounding sphere for segment using the ' 
     .//           'Latitudinal coordinate system.'               )


C
C     Strictly speaking, we don't need an actual DSK file; we just
C     need a DSK descriptor. However, we'll create a file in order
C     to work with a realistic descriptor.
C

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

C
C     Longitude bounds:
C
      BOUNDS(1,1) = PI()/6
      BOUNDS(2,1) = PI()/3

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

C
C     Radius bounds are not used by T_SECDS2.
C
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

C
C     Create the DSK file.
C
      CALL T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSKS(1)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Fetch the DSK descriptor for the segment we just created.
C
      CALL DASOPR ( DSKS(1), HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA segment found', FOUND, .TRUE., OK )

      CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the segment's radius bounds from the DSK descriptor.
C
      BOUNDS(1,3) = DSKDSC(MN3IDX)
      BOUNDS(2,3) = DSKDSC(MX3IDX)

C
C     Create a bounding box and sphere for the segment using both
C     ZZSEGBOX and ZZLATBOX. The results should match.
C
      CALL ZZSEGBOX ( DSKDSC, BOXCTR, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZLATBOX ( BOUNDS, XCTR, LR, LT, LZ, XRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'RADIUS', RADIUS, '~/',  XRAD,    TOL, OK )
      CALL CHCKAD ( 'BOXCTR', BOXCTR, '~~/', XCTR, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check bounding sphere for segment using the ' 
     .//           'Planetodetic coordinate system.'               )


C
C     Strictly speaking, we don't need an actual DSK file; we just
C     need a DSK descriptor. However, we'll create a file in order
C     to work with a realistic descriptor.
C

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
     
C
C     Longitude bounds:
C
      BOUNDS(1,1) = PI()/6
      BOUNDS(2,1) = PI()/3

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

C
C     Altitude bounds are not used by T_SECDS2.
C
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

C
C     Create the DSK file.
C
      CALL T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSKS(2)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Fetch the DSK descriptor for the segment we just created.
C
      CALL DASOPR ( DSKS(2), HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA segment found', FOUND, .TRUE., OK )

      CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the segment's altitude bounds from the DSK descriptor.
C
      BOUNDS(1,3) = DSKDSC(MN3IDX)
      BOUNDS(2,3) = DSKDSC(MX3IDX)

C
C     Create a bounding box and sphere for the segment using both
C     ZZSEGBOX and ZZLATBOX. The results should match.
C
      CALL ZZSEGBOX ( DSKDSC, BOXCTR, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZPDTBOX ( BOUNDS, CORPAR, XCTR, LR, LT, LZ, XRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'RADIUS', RADIUS, '~/',  XRAD,    TOL, OK )
      CALL CHCKAD ( 'BOXCTR', BOXCTR, '~~/', XCTR, 3, TOL, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check bounding sphere for segment using the ' 
     .//           'Rectangular coordinate system.'               )


C
C     Strictly speaking, we don't need an actual DSK file; we just
C     need a DSK descriptor. However, we'll create a file in order
C     to work with a realistic descriptor.
C
C     We're going to start out as though we're using the latitudinal
C     system, since the plate set generation utilities work with a 
C     lon/lat coverage specification.
C
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

C
C     Longitude bounds:
C
      BOUNDS(1,1) = PI()/6
      BOUNDS(2,1) = PI()/3

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

C
C     Radius bounds are not used by T_SECDS2.
C
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

C
C     Create the DSK file.
C
      CALL T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSKS(1)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Fetch the DSK descriptor for the segment we just created.
C
      CALL DASOPR ( DSKS(1), HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA segment found', FOUND, .TRUE., OK )

      CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     Here's where we deviate from the latitudinal case. We're going
C     to convert the descriptor to one using rectangular coordinates.
C
C     Before closing the file, fetch the vertices of the file
C     and obtain the extents of the plate set.
C
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Let MINV(I) and MAXV(I) store the minimum and maximum values of
C     the Ith components of the vertices.
C
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

C
C     Turn the descriptor into one for rectangular coordinates.
C
      DSKDSC( SYSIDX ) = RECSYS

      DSKDSC( MN1IDX ) = MINV(1)
      DSKDSC( MX1IDX ) = MAXV(1)
      DSKDSC( MN2IDX ) = MINV(2)
      DSKDSC( MX2IDX ) = MAXV(2)
      DSKDSC( MN3IDX ) = MINV(3)
      DSKDSC( MX3IDX ) = MAXV(3)
     
C
C     Create a bounding box and sphere for the segment using both
C     ZZSEGBOX and ZZLATBOX. The results should match.
C
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



C***********************************************************************
C
C     Error cases
C
C***********************************************************************



C
C ---------------------------------------------------------
C
      CALL TCASE ( 'Error: try segment using cylindrical coordinates.' )

C
C     Use descriptor from last case; change the coordinate system
C     parameter to indicate cylindrical coordinates.
C      
      DSKDSC( SYSIDX ) = DBLE( CYLSYS )

      CALL ZZSEGBOX ( DSKDSC, BOXCTR, RADIUS )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C     Clean up.
C

C
C ---------------------------------------------------------
C
      CALL TCASE ( 'Clean up' )

      CALL KCLEAR 
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NDSKS

         IF ( EXISTS(DSKS(I)) ) THEN

            CALL DELFIL( DSKS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


