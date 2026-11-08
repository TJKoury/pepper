!$Procedure PLTNP ( Nearest point on triangular plate )

      subroutine pltnp ( point, v1, v2, v3, pnear, dist )

! $ Abstract
!
!     Find the nearest point on a triangular plate to a given point.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!
! $ Declarations

      implicit none

      double precision      point ( 3 )
      double precision      v1    ( 3 )
      double precision      v2    ( 3 )
      double precision      v3    ( 3 )
      double precision      pnear ( 3 )
      double precision      dist

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POINT      I   A point in 3-dimensional space.
!     V1,
!     V2,
!     V3         I   Vertices of a triangular plate.
!     PNEAR      O   Nearest point on the plate to POINT.
!     DIST       O   Distance between PNEAR and POINT.
!
! $ Detailed_Input
!
!     POINT    is an arbitrary point in 3-dimensional space.
!
!     V1,
!     V2,
!     V3       are 3-vectors constituting the vertices of
!              a triangular plate.
!
!              The plate is allowed to be degenerate: it may
!              consist of a line segment or of a single point.
!
! $ Detailed_Output
!
!     PNEAR    is the closest point on the plate to POINT.
!              PNEAR is unique, since the plate is convex.
!
!     DIST     is the distance between POINT and PNEAR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The input plate is allowed to be degenerate: it may be
!         a line segment or a single point.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input
!     (if any), the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1) Find the nearest point to the point (2,2,2) on a plate having
!        vertices at the unit basis vectors that lie along the positive
!        X, Y, and Z coordinate axes.
!
!
!        Example code begins here.
!
!
!              PROGRAM PLTNP_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,3E15.7)' )
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      DIST
!              DOUBLE PRECISION      POINT  ( 3 )
!              DOUBLE PRECISION      PNEAR  ( 3 )
!              DOUBLE PRECISION      V1     ( 3 )
!              DOUBLE PRECISION      V2     ( 3 )
!              DOUBLE PRECISION      V3     ( 3 )
!
!        C
!        C     POINT is the input point.
!        C
!              CALL VPACK ( 2.D0, 2.D0, 2.D0, POINT )
!        C
!        C     V1, V2, V3 are the vertices of a plate.
!        C
!              CALL VPACK ( 1.D0, 0.D0, 0.D0, V1 )
!              CALL VPACK ( 0.D0, 1.D0, 0.D0, V2 )
!              CALL VPACK ( 0.D0, 0.D0, 1.D0, V3 )
!        C
!        C     Find the near point on the plate.
!        C
!              CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
!
!              WRITE (*,*) ' '
!              WRITE (*,FMT1) 'Plate vertex 1 = ', V1
!              WRITE (*,FMT1) 'Plate vertex 2 = ', V2
!              WRITE (*,FMT1) 'Plate vertex 3 = ', V3
!              WRITE (*,FMT1) 'Input point    = ', POINT
!              WRITE (*,*)    ' '
!              WRITE (*,FMT1) 'Near point     = ', PNEAR
!              WRITE (*,FMT1) 'Distance       = ', DIST
!              WRITE (*,*) ' '
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Plate vertex 1 =   0.1000000E+01  0.0000000E+00  0.0000000E+00
!        Plate vertex 2 =   0.0000000E+00  0.1000000E+01  0.0000000E+00
!        Plate vertex 3 =   0.0000000E+00  0.0000000E+00  0.1000000E+01
!        Input point    =   0.2000000E+01  0.2000000E+01  0.2000000E+01
!
!        Near point     =   0.3333333E+00  0.3333333E+00  0.3333333E+00
!        Distance       =   0.2886751E+01
!
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.3, 04-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Edited code example output format for the solution to fit
!        within the $Examples section without modifications. Added
!        DSK to $Required_Readings.
!
!-    SPICELIB Version 1.1.2, 01-FEB-2016 (NJB)
!
!        Added code example to header.
!
!     DSKLIB Version 1.1.1, 19-MAR-2015 (NJB)
!
!        Fixed spelling error in header.
!
!     DSKLIB Version 1.1.0, 31-DEC-2014 (NJB)
!
!        Bug fix: vertex indices for outside case, near
!        point on 3rd edge were corrected.
!
!     DSKLIB Version 1.0.0, 29-SEP-2014 (NJB)
!
!-&


! $ Index_Entries
!
!     nearest point on triangular plate
!
!-&


!
!     SPICELIB functions
!
      double precision      vdist
      double precision      vdot
      double precision      vnorm

      logical               return
      logical               vzero

!
!     Local variables
!
      double precision      d1
      double precision      d2
      double precision      d3
      double precision      e1     ( 3 )
      double precision      e2     ( 3 )
      double precision      e3     ( 3 )
      double precision      enorm1 ( 3 )
      double precision      enorm2 ( 3 )
      double precision      enorm3 ( 3 )
      double precision      l1
      double precision      l2
      double precision      l3
      double precision      normal ( 3 )
      double precision      np1    ( 3 )
      double precision      np2    ( 3 )
      double precision      pdiff  ( 3 )
      double precision      perp   ( 3 )

      logical               degen
      logical               in1
      logical               in2
      logical               in3

      if ( return() ) then
         return
      end if

!
!     Use discovery check-in.
!
!
!     Compute the plate's edges.
!
      call vsub ( v2, v1, e1 )
      call vsub ( v3, v2, e2 )
      call vsub ( v1, v3, e3 )

!
!     Compute a normal vector for the plate, if possible.
!     If the plate is degenerate, we'll find out at this point.
!
      call vcrss ( e1, e2, normal )

!
!     Compute the outward normals of the plate's edges in the
!     plate containing the plate.
!
      call vcrss ( e1, normal, enorm1 )
      call vcrss ( e2, normal, enorm2 )
      call vcrss ( e3, normal, enorm3 )

      degen =      vzero(normal) .or. vzero(enorm1) &
              .or. vzero(enorm2) .or. vzero(enorm3)

      if ( degen ) then
!
!        The "plate" is a line segment or point. Determine
!        which case we have.
!
         l1 = vnorm(e1)
         l2 = vnorm(e2)
         l3 = vnorm(e3)

         if ( ( l1 .eq. 0.d0 ) .and. ( l2 .eq. 0.d0 ) ) then
!
!           Up to round-off error, the vertices coincide.
!           The vertex V1 for practical purposes is the plate.
!
            call vequ ( v1, pnear )

            dist = vdist( pnear, point )

         else
!
!           The plate is a line segment having positive length.
!           One of the edges will coincide with the segment.
!           Determine which vertices are the endpoints.
!
            if ( l1 .gt. max(l2, l3) ) then
!
!              The segment is bounded by V1 and V2.
!
               call npsgpt( v1, v2, point, pnear, dist )

            else if ( l2 .gt. max(l3, l1) ) then
!
!              The segment is bounded by V2 and V3.
!
               call npsgpt( v2, v3, point, pnear, dist )

            else
!
!              The segment is bounded by V3 and V1.
!
               call npsgpt( v3, v1, point, pnear, dist )

            end if

         end if
!
!        The outputs are set for the degenerate cases.
!
         return

      end if

!
!     We have a non-degenerate plate. NORMAL has unit length.
!
!     We'll treat V1 as an origin in the plane containing
!     the plate. Find the offset of the POINT from V1, and
!     find the component of this offset orthogonal to NORMAL.
!
      call vsub  ( point, v1,     pdiff )
      call vperp ( pdiff, normal, perp  )

!
!     Determine whether V1+PERP is inside the plate.
!
!     Note that the "line constants" for edges 1 and 3
!     are zero, since these edges contain V1. The line
!     constant for edge 2 is that of the offset of V2
!     from V1; this offset is edge 1.
!
      in1 = vdot( perp, enorm1 ) .le. 0.d0
      in2 = vdot( perp, enorm2 ) .le. vdot(e1,enorm2)
      in3 = vdot( perp, enorm3 ) .le. 0.d0


      if ( in1 .and. in2 .and. in3 ) then
!
!        V1+PERP is inside the plate. It is the closest
!        point on the plate to POINT.
!
         call vadd ( v1, perp, pnear )
!
!        We have the near point; set the distance.
!
         dist = vdist( pnear, point )

      else
!
!        PERP is outside the plate. The nearest point
!        on the plate to POINT is on one of the edges.
!
!        We'll use the "in" flags to reduce the number
!        of point-edge distance computations.
!
         if ( ( .not. in1 ) .and. ( in2 .and. in3 ) ) then
!
!           The solution must be on the first edge.
!
            call npsgpt ( v1, v2, point, pnear, dist )

         else if ( ( .not. in2 ) .and. ( in3 .and. in1 ) ) then

!
!           The solution must be on the second edge.
!
            call npsgpt ( v2, v3, point, pnear, dist )

         else if ( ( .not. in3 ) .and. ( in1 .and. in2 ) ) then
!
!           The solution must be on the third edge.
!
            call npsgpt ( v3, v1, point, pnear, dist )

         else
!
!           Compute solutions on all three edges and pick
!           the best one.
!
            call npsgpt ( v1, v2, point, np1,   d1 )
            call npsgpt ( v2, v3, point, np2,   d2 )
            call npsgpt ( v3, v1, point, pnear, d3 )

            if ( d1 .le. min(d2, d3) ) then

               call vequ ( np1, pnear )
               dist = d1

            else if ( d2 .le. min(d3, d1) ) then

               call vequ ( np2, pnear )
               dist = d2

            else
!
!              PNEAR is already set.
!
               dist = d3

            end if

         end if

      end if

      return
      end
