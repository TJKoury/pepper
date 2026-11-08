!$Procedure ZZRECBOX (Bounding box for rectangular volume element)

      SUBROUTINE ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Compute a bounding box center and radius for a rectangular volume
!     element.
!
!     The outputs are the box's center, dimensions in the X, Y, and Z
!     directions, and the box's radius. The box itself coincides with
!     the input box.
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
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LX
      DOUBLE PRECISION      LY
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BOUNDS     I   Array of coordinate bounds for segment.
!     CENTER     O   Center of bounding box.
!     LX         O   Extent of box in the X direction.
!     LY         O   Extent of box in the Y direction.
!     LZ         O   Extent of box in the Z direction.
!     RADIUS     O   Radius of box.
!
! $ Detailed_Input
!
!     BOUNDS     is a 2 x 3 double precision array containing bounds
!                on the rectangular coordinates of the spatial region
!                (volume element) covered by a DSK segment. The
!                contents of BOUNDS are:
!
!                   BOUNDS(*,1)              X bounds
!                   BOUNDS(*,2)              Y bounds
!                   BOUNDS(*,3)              Z bounds
!
!                Elements (1,*) are lower bounds; elements (2,*) are
!                upper bounds.
!
! $ Detailed_Output
!
!     CENTER     is a double precision 3-vector representing the center
!                of the volume specified by BOUNDS.
!
!     LR,
!     LT,
!     LZ         are, respectively, the extents (edge lengths) of the
!                bounding box in the radial, tangential, and Z
!                directions.
!
!     RADIUS     is the radius of the sphere that circumscribes the
!                box. RADIUS is equal to the length of a line segment
!                connecting the center of the box to any corner.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the minimum value of of any coordinate exceeds the maximum,
!        the error SPICE(BOUNDSOUTOFORDER) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports the DSK ray-surface intercept segment
!     selection algorithms.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 04-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     bounding box radius for rectangular volume element
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN

!
!     Local parameters
!
!
!     Element boundary indices:
!
      INTEGER               LOWER
      PARAMETER           ( LOWER  = 1 )

      INTEGER               UPPER
      PARAMETER           ( UPPER  = 2 )

!
!     Local variables
!
      DOUBLE PRECISION      DIAG   ( 3 )
      DOUBLE PRECISION      L      ( 3 )
      DOUBLE PRECISION      MAXCOR ( 3 )
      DOUBLE PRECISION      MINCOR ( 3 )

      INTEGER               I

!
!     This routine uses discovery check-in. We check RETURN in order to
!     avoid performing math operations using invalid operands.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Get local copies of the bounds of the volume element.
!
      DO I = 1, 3

         MINCOR(I) = BOUNDS(LOWER,I)
         MAXCOR(I) = BOUNDS(UPPER,I)
         L(I)      = MAXCOR(I) - MINCOR(I)

         IF ( L(I) .LE. 0.D0 ) THEN

            CALL CHKIN  ( 'ZZRECBOX'                               )
            CALL SETMSG ( 'Coordinate # bounds were #:#; bounds ' &
            //            'must be strictly increasing.'           )
            CALL ERRINT ( '#',  I                                  )
            CALL ERRDP  ( '#',  MINCOR(I)                          )
            CALL ERRDP  ( '#',  MAXCOR(I)                          )
            CALL SIGERR ( 'SPICE(BOUNDSOUTOFORDER)'                )
            CALL CHKOUT ( 'ZZRECBOX'                               )
            RETURN

         END IF

      END DO

!
!     Set output box dimensions.
!
      LX = L(1)
      LY = L(2)
      LZ = L(3)

!
!     Compute the coordinates of the center of the box.
!
      DO I = 1, 3
         CENTER(I) = MINCOR(I) + ( L(I)/2 )
      END DO

!
!     The radius is the distance from the center of the box
!     to any corner.
!
      CALL VPACK ( LX/2, LY/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      RETURN
      END
