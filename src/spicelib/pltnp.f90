!$Procedure PLTNP ( Nearest point on triangular plate )

      SUBROUTINE PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )

! $ Abstract
!
!     Find the nearest point on a triangular plate to a given point.
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
!     DSK
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      POINT ( 3 )
      DOUBLE PRECISION      V1    ( 3 )
      DOUBLE PRECISION      V2    ( 3 )
      DOUBLE PRECISION      V3    ( 3 )
      DOUBLE PRECISION      PNEAR ( 3 )
      DOUBLE PRECISION      DIST

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
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local variables
!
      DOUBLE PRECISION      D1
      DOUBLE PRECISION      D2
      DOUBLE PRECISION      D3
      DOUBLE PRECISION      E1     ( 3 )
      DOUBLE PRECISION      E2     ( 3 )
      DOUBLE PRECISION      E3     ( 3 )
      DOUBLE PRECISION      ENORM1 ( 3 )
      DOUBLE PRECISION      ENORM2 ( 3 )
      DOUBLE PRECISION      ENORM3 ( 3 )
      DOUBLE PRECISION      L1
      DOUBLE PRECISION      L2
      DOUBLE PRECISION      L3
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      NP1    ( 3 )
      DOUBLE PRECISION      NP2    ( 3 )
      DOUBLE PRECISION      PDIFF  ( 3 )
      DOUBLE PRECISION      PERP   ( 3 )

      LOGICAL               DEGEN
      LOGICAL               IN1
      LOGICAL               IN2
      LOGICAL               IN3

      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Use discovery check-in.
!
!
!     Compute the plate's edges.
!
      CALL VSUB ( V2, V1, E1 )
      CALL VSUB ( V3, V2, E2 )
      CALL VSUB ( V1, V3, E3 )

!
!     Compute a normal vector for the plate, if possible.
!     If the plate is degenerate, we'll find out at this point.
!
      CALL VCRSS ( E1, E2, NORMAL )

!
!     Compute the outward normals of the plate's edges in the
!     plate containing the plate.
!
      CALL VCRSS ( E1, NORMAL, ENORM1 )
      CALL VCRSS ( E2, NORMAL, ENORM2 )
      CALL VCRSS ( E3, NORMAL, ENORM3 )

      DEGEN =      VZERO(NORMAL) .OR. VZERO(ENORM1) &
              .OR. VZERO(ENORM2) .OR. VZERO(ENORM3)

      IF ( DEGEN ) THEN
!
!        The "plate" is a line segment or point. Determine
!        which case we have.
!
         L1 = VNORM(E1)
         L2 = VNORM(E2)
         L3 = VNORM(E3)

         IF ( ( L1 .EQ. 0.D0 ) .AND. ( L2 .EQ. 0.D0 ) ) THEN
!
!           Up to round-off error, the vertices coincide.
!           The vertex V1 for practical purposes is the plate.
!
            CALL VEQU ( V1, PNEAR )

            DIST = VDIST( PNEAR, POINT )

         ELSE
!
!           The plate is a line segment having positive length.
!           One of the edges will coincide with the segment.
!           Determine which vertices are the endpoints.
!
            IF ( L1 .GT. MAX(L2, L3) ) THEN
!
!              The segment is bounded by V1 and V2.
!
               CALL NPSGPT( V1, V2, POINT, PNEAR, DIST )

            ELSE IF ( L2 .GT. MAX(L3, L1) ) THEN
!
!              The segment is bounded by V2 and V3.
!
               CALL NPSGPT( V2, V3, POINT, PNEAR, DIST )

            ELSE
!
!              The segment is bounded by V3 and V1.
!
               CALL NPSGPT( V3, V1, POINT, PNEAR, DIST )

            END IF

         END IF
!
!        The outputs are set for the degenerate cases.
!
         RETURN

      END IF

!
!     We have a non-degenerate plate. NORMAL has unit length.
!
!     We'll treat V1 as an origin in the plane containing
!     the plate. Find the offset of the POINT from V1, and
!     find the component of this offset orthogonal to NORMAL.
!
      CALL VSUB  ( POINT, V1,     PDIFF )
      CALL VPERP ( PDIFF, NORMAL, PERP  )

!
!     Determine whether V1+PERP is inside the plate.
!
!     Note that the "line constants" for edges 1 and 3
!     are zero, since these edges contain V1. The line
!     constant for edge 2 is that of the offset of V2
!     from V1; this offset is edge 1.
!
      IN1 = VDOT( PERP, ENORM1 ) .LE. 0.D0
      IN2 = VDOT( PERP, ENORM2 ) .LE. VDOT(E1,ENORM2)
      IN3 = VDOT( PERP, ENORM3 ) .LE. 0.D0


      IF ( IN1 .AND. IN2 .AND. IN3 ) THEN
!
!        V1+PERP is inside the plate. It is the closest
!        point on the plate to POINT.
!
         CALL VADD ( V1, PERP, PNEAR )
!
!        We have the near point; set the distance.
!
         DIST = VDIST( PNEAR, POINT )

      ELSE
!
!        PERP is outside the plate. The nearest point
!        on the plate to POINT is on one of the edges.
!
!        We'll use the "in" flags to reduce the number
!        of point-edge distance computations.
!
         IF ( ( .NOT. IN1 ) .AND. ( IN2 .AND. IN3 ) ) THEN
!
!           The solution must be on the first edge.
!
            CALL NPSGPT ( V1, V2, POINT, PNEAR, DIST )

         ELSE IF ( ( .NOT. IN2 ) .AND. ( IN3 .AND. IN1 ) ) THEN

!
!           The solution must be on the second edge.
!
            CALL NPSGPT ( V2, V3, POINT, PNEAR, DIST )

         ELSE IF ( ( .NOT. IN3 ) .AND. ( IN1 .AND. IN2 ) ) THEN
!
!           The solution must be on the third edge.
!
            CALL NPSGPT ( V3, V1, POINT, PNEAR, DIST )

         ELSE
!
!           Compute solutions on all three edges and pick
!           the best one.
!
            CALL NPSGPT ( V1, V2, POINT, NP1,   D1 )
            CALL NPSGPT ( V2, V3, POINT, NP2,   D2 )
            CALL NPSGPT ( V3, V1, POINT, PNEAR, D3 )

            IF ( D1 .LE. MIN(D2, D3) ) THEN

               CALL VEQU ( NP1, PNEAR )
               DIST = D1

            ELSE IF ( D2 .LE. MIN(D3, D1) ) THEN

               CALL VEQU ( NP2, PNEAR )
               DIST = D2

            ELSE
!
!              PNEAR is already set.
!
               DIST = D3

            END IF

         END IF

      END IF

      RETURN
      END
