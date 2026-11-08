C$Procedure ZZPSTUBE ( Plate set, create tube )
 
      SUBROUTINE ZZPSTUBE ( N,     VRTCES, CRVSUB, 
     .                      NSAMP, CLOSED, VOUT,   POUT )
 
C$ Abstract
C
C     Create a plate set representing a tube with specified 3-D
C     polygonal cross section. A user-defined callback subroutine is
C     supplied to define the central curve of the tube, its derivative
C     vector, and twist.
C
C     An input logical flag indicates whether the tube is closed.
C
C     Polygon vertices are required to be ordered in the positive
C     sense about the initial central curve derivative.
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
C     CELLS
C     DSK
C
C$ Keywords
C
C     DSK
C     PLATE
C
C$ Declarations

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

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of vertices in the base polygon.
C     VRTCES     I   3-D vertices of the base polygon.
C     CRVSUB     I   Subroutine defining central curve.
C     NSAMP      I   Number of samples to use to define cross sections.
C     CLOSED     I   Flag indicating whether tube is closed.
C     VOUT       O   Vertex cell of the plate set.
C     POUT       O   Plate cell of the plate set.
C
C$ Detailed_Input
C
C     N              is the number of vertices in the polygonal cross
C                    section of the tube.
C
C
C     VRTCES         is an array of 3-D vertices representing the
C                    initial cross section of the tube. This is the
C                    cross section of the tube evaluated at the
C                    parameter value T = 0 (see the description of
C                    CRVSUB below).
C
C                    The vertices must be ordered in the positive
C                    (counterclockwise) sense about the curve's
C                    derivative vector at the parameter value T = 0
C                    (see the description of CRVSUB below).
C
C
C     CRVSUB         is the name of a subroutine to be called to
C                    generate a curve that runs through the tube shape
C                    created by this routine. CRVSUB maps the interval
C
C                       [0, 1]
C
C                    to a curve in 3-dimensional space. For each value
C                    T in the interval [0, 1], CRVSUB returns a 3-D
C                    vector representing the curve at that value of T,
C                    a 3-D derivative vector giving the curve's
C                    direction at T, and a twist angle that defines the
C                    orientation of the tube's cross section at T.
C
C                    The subroutine CRVSUB has the calling sequence
C
C                       CRVSUB ( T, CURVE, DERIV, TWIST )
C
C                       DOUBLE PRECISION      T
C                       DOUBLE PRECISION      CURVE ( 3 )
C                       DOUBLE PRECISION      DERIV ( 3 )
C                       DOUBLE PRECISION      TWIST
C
C                    The range of the input T is 0.D0 : 1.D0.
C 
C                    The TWIST angle has units of radians. The TWIST
C                    angle must be either monotonically increasing or
C                    monotonically decreasing; it must not have branch
C                    discontinuities. 
C
C                    The vertices of the surface of the tube are
C                    generated by sampling the curve at evenly spaced
C                    values of T and adding (suitably rotated) cross
C                    section vectors to the curve at each sample.
C
C                    The orientation of cross sections is determined as
C                    follows: if the curve's derivatives at samples
C                    T(I+1) and T(I) have cross product zero, the cross
C                    section vectors for sample T(I+1) are copied from
C                    the those at sample T(I). If the twist angle
C                    changes by DELTA between the two samples, the
C                    cross section vectors are rotated by DELTA about
C                    the derivative at T(I+1). For the cross section
C                    at T = 0, the value of TWIST at T = 0 is applied.
C
C                    If the cross product is non-zero, the cross
C                    section at T(I+1) is created by rotating the cross
C                    section vectors at T(I) about the cross product
C                    vector by the angle between the curve's
C                    derivatives at the two values of T. This creates a
C                    set of cross section vectors lying in a plane
C                    orthogonal to the derivative at T(I+1). Next,
C                    twist is applied to the cross section at T(I+1) as
C                    described above.
C
C
C     NSAMP          is the number of "samples" taken to define the
C                    tube's cross-sectional shape. For each sample, a
C                    call to CRVSUB is made. Samples are taken at T = 0,
C                    T = 1, and at NSAMP-2 equally spaced points in
C                    between. The samples are taken at increasing values
C                    of T.
C
C
C     CLOSED         is a logical flag indicating whether the tube is
C                    "closed": that is, the boundary of the tube at T =
C                    1 matches that at T = 0. If CLOSED is .TRUE., the
C                    cross section vectors at T = 1 are simply copied
C                    from those at T = 0. Otherwise, the cross section
C                    vectors at T = 1 are computed as for the other
C                    cross sections.
C                         
C$ Detailed_Output
C
C     VOUT           is a SPICELIB double precision cell containing the
C                    vertices of a plate set representing the surface
C                    of the tube. 
C
C     POUT           is a SPICELIB integer cell containing the plates
C                    of the tube. The vertices of each plate are
C                    ordered so that they define an outward normal
C                    vector pointing away from the central curve of the
C                    tube.
C                  
C$ Parameters
C
C    LBCELL is the lower bound of the SPICELIB cell data structure.
C
C$ Exceptions
C
C     1) If the size of VOUT is too small to contain the output
C        plate set's vertices, the error SPICE(VERTARRAYTOOSMALL) is
C        signaled.
C
C     2) If the size of POUT is too small to contain the output plate
C        set's plates, the error SPICE(PLTARRAYTOOSMALL) is signaled.
C
C     3) Any other errors that occur while accessing SPICE cells
C        will be diagnosed by routines in the call tree of this
C        routine.
C
C     4) If the vertices are not ordered in the positive sense 
C        about the +Z axis, the error SPICE(BADVERTEXORDER) will be
C        signaled.
C
C     5) The number of cross section samples must be sufficient to
C        allow the output plate set to follow the shape of the curve.
C        There is no check for this.
C
C     6) The number of cross section samples must be sufficient to
C        allow the output plate set to model the twist angle.
C        There is no check for this.
C
C     7) The twist angle returned by CRVSUB must be either monotonically
C        increasing or monotonically decreasing. There is no check for
C        this. 
C 
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine can be used to generate plate sets representing
C     simple planetary ring models. 
C
C$ Examples
C
C      1)  Make a plate set representing a twisted ring with triangular
C          cross section. The ring twists by 2*pi radians from start to
C          end and is closed.
C
C          Write the plate set to a MKDSK input file.
C 
C
C     Example code begins here.
C
C
C           PROGRAM MKTRIRG
C           IMPLICIT NONE
C
C           INCLUDE 'dsk02.inc'
C
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      PI
C
C     C
C     C     EXTERNAL functions
C     C
C           EXTERNAL              CRVSUB
C
C     C
C     C     Local parameters
C     C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C
C           INTEGER               FILSIZ
C           PARAMETER           ( FILSIZ = 255 )
C
C           INTEGER               NV
C           PARAMETER           ( NV = 100 )
C
C           INTEGER               PSIZE
C           PARAMETER           ( PSIZE  = 3 * MAXPLT )
C
C           INTEGER               VSIZE
C           PARAMETER           ( VSIZE  = 3 * MAXVRT )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(FILSIZ)    INPFIL
C
C           DOUBLE PRECISION      POLVRT ( 3, NV )
C           DOUBLE PRECISION      R
C           DOUBLE PRECISION      THETA
C           DOUBLE PRECISION      VERTS  ( LBCELL : VSIZE )
C
C           INTEGER               I
C           INTEGER               N
C           INTEGER               NSAMP
C           INTEGER               PLATES ( LBCELL : PSIZE )
C           INTEGER               UNIT
C
C           LOGICAL               CLOSED
C
C           INPFIL = 'triring.inp'
C
C           CALL TXTOPN ( INPFIL, UNIT )
C
C           CALL SSIZEI ( PSIZE, PLATES )
C           CALL SSIZED ( VSIZE, VERTS  )
C
C           N     = 3
C           THETA = 2 * PI() / N
C
C           R     = 1.D0
C
C           DO I = 1, N
C
C              POLVRT(1,I) = 0.D0
C              POLVRT(2,I) = R * COS( (I-1)*THETA )
C              POLVRT(3,I) = R * SIN( (I-1)*THETA )
C
C           END DO
C
C           NSAMP  = 50
C           CLOSED = .TRUE.
C
C           CALL ZZPSTUBE ( N,     POLVRT, CRVSUB, NSAMP, CLOSED,
C          .                VERTS, PLATES )
C
C           CALL ZZPSWRIT ( UNIT, VERTS, PLATES )
C
C           CLOSE( UNIT )
C
C           END
C
C
C           SUBROUTINE CRVSUB ( T, CURVE, DERIV, TWIST )
C           IMPLICIT NONE
C
C           DOUBLE PRECISION      T
C           DOUBLE PRECISION      CURVE ( 3 )
C           DOUBLE PRECISION      DERIV ( 3 )
C           DOUBLE PRECISION      TWIST
C
C
C           DOUBLE PRECISION      PI
C
C           DOUBLE PRECISION      R
C           DOUBLE PRECISION      THETA
C
C           THETA = ( 2 * PI() * T ) - PI()/2
C           R     = 3.D0
C
C           CURVE(1) = R * COS(THETA)
C           CURVE(2) = R * SIN(THETA)
C           CURVE(3) = 0.D0
C
C           DERIV(1) = -R * SIN(THETA)
C           DERIV(2) =  R * COS(THETA)
C           DERIV(3) = 0.D0
C
C           TWIST    = 2 * PI() * T
C
C           END
C
C
C     
C$ Restrictions
C
C     1) Various input errors cannot be checked by this routine. See
C        the Exceptions section above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    DSKLIB Version 1.0.0, 10-SEP-2014 (NJB)
C
C-&
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      INTEGER               SIZED
      INTEGER               SIZEI

      LOGICAL               RETURN

C
C     Local variables
C
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

C
C     Check the sample count.
C
      IF ( NSAMP .LT. 2 ) THEN

         CALL SETMSG ( 'Cross section sample count was #; count '
     .   //            'must be at least 2.'                     )
         CALL ERRINT ( '#',  NSAMP                               )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                  )
         CALL CHKOUT ( 'ZZPSTUBE'                                )
         RETURN

      END IF

C
C     Set the independent variable step size.
C
      STEP = 1.D0 / (NSAMP-1)

C
C     Check the space in the output cells.
C     
      NV    = N * NSAMP
      CVOUT = 3 * NV

      NP    = 2 * N * (NSAMP-1)
      CPOUT = 3 * NP

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSTUBE'                                )
         RETURN

      END IF

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSTUBE'                                )
         RETURN

      END IF 
 
C
C     Look up the initial polygon center and curve direction.
C
      CALL CRVSUB ( 0.D0, PRVCRV, PRVDIR, PRVTWS )

      CALL VHATIP ( PRVDIR )

C
C     Check the order of the cross section vectors.
C
      DO I = 2, N

         CALL VCRSS ( VRTCES(1,I-1), VRTCES(1,I), CP )

         DP = VDOT ( CP, PRVDIR )

         IF ( DP .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Cross section vertices are not in strictly '
     .      //            'increasing order: rotation angle from '
     .      //            'vertex # to vertex # about the initial '
     .      //            'curve derivative vector is not positive.'   )
            CALL ERRINT ( '#', I                                       )
            CALL ERRINT ( '#', I+1                                     )
            CALL SIGERR ( 'SPICE(BADVERTEXORDER)'                      )
            CALL CHKOUT ( 'ZZPSTUBE'                                   )
            RETURN

         END IF

      END DO

C
C     Store the input vertices. Each vertex is an offset from
C     CURVE.
C 
      DO I = 1, N

         CALL VROTV ( VRTCES(1,I), PRVDIR, PRVTWS, VTEMP )

         J = 3*(I-1) + 1

         CALL VADD  ( VTEMP, PRVCRV, VOUT(J) )

      END DO
      
C
C     Add vertices and plates for each sample.
C
      VSTART = 3*N + 1
      PSTART = 1

      DO I = 2, NSAMP
         
         IF ( ( I .LT. NSAMP ) .OR. ( .NOT. CLOSED ) ) THEN

            T = (I-1) * STEP

            CALL CRVSUB ( T, CURVE, DERIV, TWIST )

            CALL VHATIP ( DERIV )

            DTWIST = TWIST - PRVTWS

C        
C           Compute the cross product of the previous and current
C           curve direction.
C         
            CALL VCRSS ( PRVDIR, DERIV, NORMAL )

            ANGLE = ASIN ( VNORM(NORMAL) )

            IF ( ANGLE .EQ. 0.D0 ) THEN
C
C              Just apply twist to the previous cross section.
C   
               K = (3*N*(I-2)) + 1

               DO J = 1, N

                  CALL VSUB  ( VOUT(K), PRVCRV, VTEMP2 )

                  CALL VROTV ( VTEMP2,  DERIV,  DTWIST, VTEMP )

                  CALL VADD  ( VTEMP,   CURVE,  VOUT(VSTART) )

                  VSTART = VSTART + 3
                  K      = K      + 3

               END DO

            ELSE
C
C              Rotate the cross section by ANGLE about NORMAL.
C              Apply twist to each rotated vertex.
C
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
C
C           The tube is closed and I is equal to NSAMP. We
C           simply copy the input cross section to obtain
C           the current cross section.
C
            K =  3 * N * (NSAMP-1)

            CALL MOVED ( VOUT(1), 3*N, VOUT(K+1) )

         END IF

C
C        Create plates.
C 
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


