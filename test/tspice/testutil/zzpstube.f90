!$Procedure ZZPSTUBE ( Plate set, create tube )

      SUBROUTINE ZZPSTUBE ( N,     VRTCES, CRVSUB, &
                            NSAMP, CLOSED, VOUT,   POUT )

! $ Abstract
!
!     Create a plate set representing a tube with specified 3-D
!     polygonal cross section. A user-defined callback subroutine is
!     supplied to define the central curve of the tube, its derivative
!     vector, and twist.
!
!     An input logical flag indicates whether the tube is closed.
!
!     Polygon vertices are required to be ordered in the positive
!     sense about the initial central curve derivative.
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
!     CELLS
!     DSK
!
! $ Keywords
!
!     DSK
!     PLATE
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               N
      DOUBLE PRECISION      VRTCES ( 3, * )
      EXTERNAL              CRVSUB
      INTEGER               NSAMP
      LOGICAL               CLOSED
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of vertices in the base polygon.
!     VRTCES     I   3-D vertices of the base polygon.
!     CRVSUB     I   Subroutine defining central curve.
!     NSAMP      I   Number of samples to use to define cross sections.
!     CLOSED     I   Flag indicating whether tube is closed.
!     VOUT       O   Vertex cell of the plate set.
!     POUT       O   Plate cell of the plate set.
!
! $ Detailed_Input
!
!     N              is the number of vertices in the polygonal cross
!                    section of the tube.
!
!
!     VRTCES         is an array of 3-D vertices representing the
!                    initial cross section of the tube. This is the
!                    cross section of the tube evaluated at the
!                    parameter value T = 0 (see the description of
!                    CRVSUB below).
!
!                    The vertices must be ordered in the positive
!                    (counterclockwise) sense about the curve's
!                    derivative vector at the parameter value T = 0
!                    (see the description of CRVSUB below).
!
!
!     CRVSUB         is the name of a subroutine to be called to
!                    generate a curve that runs through the tube shape
!                    created by this routine. CRVSUB maps the interval
!
!                       [0, 1]
!
!                    to a curve in 3-dimensional space. For each value
!                    T in the interval [0, 1], CRVSUB returns a 3-D
!                    vector representing the curve at that value of T,
!                    a 3-D derivative vector giving the curve's
!                    direction at T, and a twist angle that defines the
!                    orientation of the tube's cross section at T.
!
!                    The subroutine CRVSUB has the calling sequence
!
!                       CRVSUB ( T, CURVE, DERIV, TWIST )
!
!                       DOUBLE PRECISION      T
!                       DOUBLE PRECISION      CURVE ( 3 )
!                       DOUBLE PRECISION      DERIV ( 3 )
!                       DOUBLE PRECISION      TWIST
!
!                    The range of the input T is 0.D0 : 1.D0.
!
!                    The TWIST angle has units of radians. The TWIST
!                    angle must be either monotonically increasing or
!                    monotonically decreasing; it must not have branch
!                    discontinuities.
!
!                    The vertices of the surface of the tube are
!                    generated by sampling the curve at evenly spaced
!                    values of T and adding (suitably rotated) cross
!                    section vectors to the curve at each sample.
!
!                    The orientation of cross sections is determined as
!                    follows: if the curve's derivatives at samples
!                    T(I+1) and T(I) have cross product zero, the cross
!                    section vectors for sample T(I+1) are copied from
!                    the those at sample T(I). If the twist angle
!                    changes by DELTA between the two samples, the
!                    cross section vectors are rotated by DELTA about
!                    the derivative at T(I+1). For the cross section
!                    at T = 0, the value of TWIST at T = 0 is applied.
!
!                    If the cross product is non-zero, the cross
!                    section at T(I+1) is created by rotating the cross
!                    section vectors at T(I) about the cross product
!                    vector by the angle between the curve's
!                    derivatives at the two values of T. This creates a
!                    set of cross section vectors lying in a plane
!                    orthogonal to the derivative at T(I+1). Next,
!                    twist is applied to the cross section at T(I+1) as
!                    described above.
!
!
!     NSAMP          is the number of "samples" taken to define the
!                    tube's cross-sectional shape. For each sample, a
!                    call to CRVSUB is made. Samples are taken at T = 0,
!                    T = 1, and at NSAMP-2 equally spaced points in
!                    between. The samples are taken at increasing values
!                    of T.
!
!
!     CLOSED         is a logical flag indicating whether the tube is
!                    "closed": that is, the boundary of the tube at T =
!                    1 matches that at T = 0. If CLOSED is .TRUE., the
!                    cross section vectors at T = 1 are simply copied
!                    from those at T = 0. Otherwise, the cross section
!                    vectors at T = 1 are computed as for the other
!                    cross sections.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    vertices of a plate set representing the surface
!                    of the tube.
!
!     POUT           is a SPICELIB integer cell containing the plates
!                    of the tube. The vertices of each plate are
!                    ordered so that they define an outward normal
!                    vector pointing away from the central curve of the
!                    tube.
!
! $ Parameters
!
!    LBCELL is the lower bound of the SPICELIB cell data structure.
!
! $ Exceptions
!
!     1) If the size of VOUT is too small to contain the output
!        plate set's vertices, the error SPICE(VERTARRAYTOOSMALL) is
!        signaled.
!
!     2) If the size of POUT is too small to contain the output plate
!        set's plates, the error SPICE(PLTARRAYTOOSMALL) is signaled.
!
!     3) Any other errors that occur while accessing SPICE cells
!        will be diagnosed by routines in the call tree of this
!        routine.
!
!     4) If the vertices are not ordered in the positive sense
!        about the +Z axis, the error SPICE(BADVERTEXORDER) will be
!        signaled.
!
!     5) The number of cross section samples must be sufficient to
!        allow the output plate set to follow the shape of the curve.
!        There is no check for this.
!
!     6) The number of cross section samples must be sufficient to
!        allow the output plate set to model the twist angle.
!        There is no check for this.
!
!     7) The twist angle returned by CRVSUB must be either monotonically
!        increasing or monotonically decreasing. There is no check for
!        this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine can be used to generate plate sets representing
!     simple planetary ring models.
!
! $ Examples
!
!      1)  Make a plate set representing a twisted ring with triangular
!          cross section. The ring twists by 2*pi radians from start to
!          end and is closed.
!
!          Write the plate set to a MKDSK input file.
!
!
!     Example code begins here.
!
!
!           PROGRAM MKTRIRG
!           IMPLICIT NONE
!
!           INCLUDE 'dsk02.inc'
!
!     C
!     C     SPICELIB functions
!     C
!           DOUBLE PRECISION      PI
!
!     C
!     C     EXTERNAL functions
!     C
!           EXTERNAL              CRVSUB
!
!     C
!     C     Local parameters
!     C
!           INTEGER               LBCELL
!           PARAMETER           ( LBCELL = -5 )
!
!           INTEGER               FILSIZ
!           PARAMETER           ( FILSIZ = 255 )
!
!           INTEGER               NV
!           PARAMETER           ( NV = 100 )
!
!           INTEGER               PSIZE
!           PARAMETER           ( PSIZE  = 3 * MAXPLT )
!
!           INTEGER               VSIZE
!           PARAMETER           ( VSIZE  = 3 * MAXVRT )
!
!     C
!     C     Local variables
!     C
!           CHARACTER*(FILSIZ)    INPFIL
!
!           DOUBLE PRECISION      POLVRT ( 3, NV )
!           DOUBLE PRECISION      R
!           DOUBLE PRECISION      THETA
!           DOUBLE PRECISION      VERTS  ( LBCELL : VSIZE )
!
!           INTEGER               I
!           INTEGER               N
!           INTEGER               NSAMP
!           INTEGER               PLATES ( LBCELL : PSIZE )
!           INTEGER               UNIT
!
!           LOGICAL               CLOSED
!
!           INPFIL = 'triring.inp'
!
!           CALL TXTOPN ( INPFIL, UNIT )
!
!           CALL SSIZEI ( PSIZE, PLATES )
!           CALL SSIZED ( VSIZE, VERTS  )
!
!           N     = 3
!           THETA = 2 * PI() / N
!
!           R     = 1.D0
!
!           DO I = 1, N
!
!              POLVRT(1,I) = 0.D0
!              POLVRT(2,I) = R * COS( (I-1)*THETA )
!              POLVRT(3,I) = R * SIN( (I-1)*THETA )
!
!           END DO
!
!           NSAMP  = 50
!           CLOSED = .TRUE.
!
!           CALL ZZPSTUBE ( N,     POLVRT, CRVSUB, NSAMP, CLOSED,
!          .                VERTS, PLATES )
!
!           CALL ZZPSWRIT ( UNIT, VERTS, PLATES )
!
!           CLOSE( UNIT )
!
!           END
!
!
!           SUBROUTINE CRVSUB ( T, CURVE, DERIV, TWIST )
!           IMPLICIT NONE
!
!           DOUBLE PRECISION      T
!           DOUBLE PRECISION      CURVE ( 3 )
!           DOUBLE PRECISION      DERIV ( 3 )
!           DOUBLE PRECISION      TWIST
!
!
!           DOUBLE PRECISION      PI
!
!           DOUBLE PRECISION      R
!           DOUBLE PRECISION      THETA
!
!           THETA = ( 2 * PI() * T ) - PI()/2
!           R     = 3.D0
!
!           CURVE(1) = R * COS(THETA)
!           CURVE(2) = R * SIN(THETA)
!           CURVE(3) = 0.D0
!
!           DERIV(1) = -R * SIN(THETA)
!           DERIV(2) =  R * COS(THETA)
!           DERIV(3) = 0.D0
!
!           TWIST    = 2 * PI() * T
!
!           END
!
!
!
! $ Restrictions
!
!     1) Various input errors cannot be checked by this routine. See
!        the Exceptions section above.
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
!-    DSKLIB Version 1.0.0, 10-SEP-2014 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      INTEGER               SIZED
      INTEGER               SIZEI

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      CP     ( 3 )
      DOUBLE PRECISION      CURVE  ( 3 )
      DOUBLE PRECISION      DERIV  ( 3 )
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      DTWIST
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PRVCRV ( 3 )
      DOUBLE PRECISION      PRVDIR ( 3 )
      DOUBLE PRECISION      PRVTWS
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TWIST
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VTEMP2 ( 3 )

      INTEGER               CPOUT
      INTEGER               CVOUT
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NEXT
      INTEGER               NP
      INTEGER               NV
      INTEGER               P1
      INTEGER               P2
      INTEGER               P3
      INTEGER               PSTART
      INTEGER               VSTART


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZPSTUBE' )

!
!     Check the sample count.
!
      IF ( NSAMP .LT. 2 ) THEN

         CALL SETMSG ( 'Cross section sample count was #; count ' &
         //            'must be at least 2.'                     )
         CALL ERRINT ( '#',  NSAMP                               )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                  )
         CALL CHKOUT ( 'ZZPSTUBE'                                )
         RETURN

      END IF

!
!     Set the independent variable step size.
!
      STEP = 1.D0 / (NSAMP-1)

!
!     Check the space in the output cells.
!
      NV    = N * NSAMP
      CVOUT = 3 * NV

      NP    = 2 * N * (NSAMP-1)
      CPOUT = 3 * NP

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSTUBE'                                )
         RETURN

      END IF

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSTUBE'                                )
         RETURN

      END IF

!
!     Look up the initial polygon center and curve direction.
!
      CALL CRVSUB ( 0.D0, PRVCRV, PRVDIR, PRVTWS )

      CALL VHATIP ( PRVDIR )

!
!     Check the order of the cross section vectors.
!
      DO I = 2, N

         CALL VCRSS ( VRTCES(1,I-1), VRTCES(1,I), CP )

         DP = VDOT ( CP, PRVDIR )

         IF ( DP .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Cross section vertices are not in strictly ' &
            //            'increasing order: rotation angle from ' &
            //            'vertex # to vertex # about the initial ' &
            //            'curve derivative vector is not positive.'   )
            CALL ERRINT ( '#', I                                       )
            CALL ERRINT ( '#', I+1                                     )
            CALL SIGERR ( 'SPICE(BADVERTEXORDER)'                      )
            CALL CHKOUT ( 'ZZPSTUBE'                                   )
            RETURN

         END IF

      END DO

!
!     Store the input vertices. Each vertex is an offset from
!     CURVE.
!
      DO I = 1, N

         CALL VROTV ( VRTCES(1,I), PRVDIR, PRVTWS, VTEMP )

         J = 3*(I-1) + 1

         CALL VADD  ( VTEMP, PRVCRV, VOUT(J) )

      END DO

!
!     Add vertices and plates for each sample.
!
      VSTART = 3*N + 1
      PSTART = 1

      DO I = 2, NSAMP

         IF ( ( I .LT. NSAMP ) .OR. ( .NOT. CLOSED ) ) THEN

            T = (I-1) * STEP

            CALL CRVSUB ( T, CURVE, DERIV, TWIST )

            CALL VHATIP ( DERIV )

            DTWIST = TWIST - PRVTWS

!
!           Compute the cross product of the previous and current
!           curve direction.
!
            CALL VCRSS ( PRVDIR, DERIV, NORMAL )

            ANGLE = ASIN ( VNORM(NORMAL) )

            IF ( ANGLE .EQ. 0.D0 ) THEN
!
!              Just apply twist to the previous cross section.
!
               K = (3*N*(I-2)) + 1

               DO J = 1, N

                  CALL VSUB  ( VOUT(K), PRVCRV, VTEMP2 )

                  CALL VROTV ( VTEMP2,  DERIV,  DTWIST, VTEMP )

                  CALL VADD  ( VTEMP,   CURVE,  VOUT(VSTART) )

                  VSTART = VSTART + 3
                  K      = K      + 3

               END DO

            ELSE
!
!              Rotate the cross section by ANGLE about NORMAL.
!              Apply twist to each rotated vertex.
!
               K = (3*N*(I-2)) + 1

               DO J = 1, N

                  CALL VSUB  ( VOUT(K), PRVCRV, VTEMP2 )

                  CALL VROTV ( VTEMP2,  NORMAL, ANGLE, VTEMP )

                  CALL VROTV ( VTEMP,  DERIV,   DTWIST, VTEMP2 )

                  CALL VADD  ( VTEMP2, CURVE,   VOUT(VSTART) )

                  VSTART = VSTART + 3
                  K      = K      + 3

               END DO

            END IF

            CALL VEQU ( CURVE, PRVCRV )
            CALL VEQU ( DERIV, PRVDIR )

            PRVTWS = TWIST


         ELSE
!
!           The tube is closed and I is equal to NSAMP. We
!           simply copy the input cross section to obtain
!           the current cross section.
!
            K =  3 * N * (NSAMP-1)

            CALL MOVED ( VOUT(1), 3*N, VOUT(K+1) )

         END IF

!
!        Create plates.
!
         DO J = 1, N

            IF ( J .LT. N ) THEN
               NEXT = J + 1
            ELSE
               NEXT = 1
            END IF

            P1 = ( (I-2)*N ) + J
            P2 = ( (I-2)*N ) + NEXT
            P3 =    P1 + N

            POUT(PSTART  ) = P1
            POUT(PSTART+1) = P2
            POUT(PSTART+2) = P3

            P1 = ( (I-2)*N ) + NEXT
            P2 =    P1 + N
            P3 = ( (I-1)*N ) + J

            POUT(PSTART+3) = P1
            POUT(PSTART+4) = P2
            POUT(PSTART+5) = P3

            PSTART = PSTART + 6

         END DO

      END DO


      CALL SCARDD ( CVOUT, VOUT )
      CALL SCARDI ( CPOUT, POUT )

      CALL CHKOUT ( 'ZZPSTUBE' )
      RETURN
      END


