!$Procedure ZZRYTREC ( DSK, ray touches rectangular element )

      SUBROUTINE ZZRYTREC ( VERTEX, RAYDIR, BOUNDS, &
                            MARGIN, NXPTS,  XPT    )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find nearest intersection to ray's vertex of ray and
!     rectangular volume element. If the vertex is inside
!     the element, the vertex is considered to be the solution.
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
!     INTERCEPT
!     INTERSECTION
!     RAY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      MARGIN
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT    ( 3 )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     BOUNDS     I   Bounds of rectangular volume element.
!     MARGIN     I   Margin used for element expansion.
!     NXPTS      O   Number of intercept points.
!     XPT        O   Intercept.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray direction must be represented
!                in the reference frame of the segment to which the
!                volume element boundaries correspond. The vertex is
!                considered to be an offset from the center of the
!                reference frame associated with the segment.
!
!     BOUNDS     is a 2x3 array containing the bounds of a rectangular
!                volume element. Normally this is the coverage boundary
!                of a DSK segment. In the element
!
!                   BOUNDS(I,J)
!
!                J is the coordinate index. J is one of
!
!                   { 1, 2, 3 }
!
!                I is the bound index.
!
!                   I = 1   ->   lower bound
!                   I = 2   ->   upper bound
!
! $ Detailed_Output
!
!     XPT        is the intercept of the ray on the boundary of the
!                input volume element, if such an intercept exists. If
!                the ray's vertex is inside the element, XPT is set
!                equal to the vertex. XPT is valid if and only if FOUND
!                is .TRUE.

!                XPT is expressed in the reference frame associated
!                with the inputs VERTEX and RAYDIR. XPT represents
!                an offset from the origin of the coordinate system.
!
!                XPT is valid only if NXPTS is set to 1.
!
!
!     NXPTS      is the number of intercept points of the ray and
!                the volume element.
!
!                Currently there are only two possible values for
!                NXPTS:
!
!                   1 for an intersection
!                   0 for no intersection
!
!                If the vertex is inside the element, NXPTS is
!                set to 1.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the minimum value of any coordinate exceeds the maximum,
!        the error will be signaled by a routine in the call tree of
!        this routine.
!
! $ Files
!
!     None. However, the input segment boundaries normally have
!     been obtained from a loaded DSK file.
!
! $ Particulars
!
!     This routine sits on top of data DSK type-specific ray-segment
!     intercept routines such as DSKX02.
!
! $ Examples
!
!     See usage in ZZDSKBUX.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by the DSK
!     subsystem.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 01-MAR-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray on rectangular volume element
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

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
      DOUBLE PRECISION      BOXORI ( 3 )
      DOUBLE PRECISION      EXTENT ( 3 )
      DOUBLE PRECISION      DELTA  ( 3 )
      DOUBLE PRECISION      L      ( 3 )

      INTEGER               I

      LOGICAL               FOUND
      LOGICAL               INSIDE


      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Compute the original volume edge lengths from the coordinate
!     bounds.
!
      DO I = 1, 3

         L(I) = BOUNDS(UPPER, I) - BOUNDS(LOWER, I)

         IF ( L(I) .LE. 0.D0 ) THEN

            CALL CHKIN  ( 'ZZRYTREC'                               )
            CALL SETMSG ( 'Coordinate # bounds were #:#; bounds ' &
            //            'must be strictly increasing.'           )
            CALL ERRINT ( '#',  I                                  )
            CALL ERRDP  ( '#',  BOUNDS(LOWER,I)                    )
            CALL ERRDP  ( '#',  BOUNDS(UPPER,I)                    )
            CALL SIGERR ( 'SPICE(BADCOORDBOUNDS)'                  )
            CALL CHKOUT ( 'ZZRYTREC'                               )
            RETURN

         END IF

      END DO

!
!     Determine whether the vertex is inside the element.
!     Use double the margin for this test, since we don't
!     want to have false negative tests for rays having
!     vertices lying on the expanded element boundary.
!
      NXPTS = 0

      CALL ZZINREC ( VERTEX, BOUNDS, 2*MARGIN, NONE, INSIDE )

      IF ( INSIDE ) THEN
!
!        We know the answer.
!
         NXPTS = 1

         CALL VEQU ( VERTEX, XPT )

         RETURN

      END IF

!
!     Expand the box using the specified margin.
!
      DO I = 1, 3

         DELTA(I)  = MARGIN * ABS( L(I) )

         BOXORI(I) = BOUNDS(LOWER,I) - DELTA(I)

         EXTENT(I) = L(I) + ( 2*DELTA(I) )

      END DO

!
!     Find the ray-surface intercept on the expanded element,
!     if the intercept exists.
!
      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )

      IF ( FOUND ) THEN
         NXPTS = 1
      END IF

      RETURN
      END
