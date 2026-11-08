!$Procedure PLTEXP ( Plate expander )

      subroutine pltexp ( iverts, delta, overts )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Expand a triangular plate by a specified amount. The expanded
!     plate is co-planar with, and has the same orientation as, the
!     original. The centroids of the two plates coincide.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      real(wp)              iverts ( 3, 3 )
      real(wp)              delta
      real(wp)              overts ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IVERTS     I   Vertices of the plate to be expanded.
!     DELTA      I   Fraction by which the plate is to be expanded.
!     OVERTS     O   Vertices of the expanded plate.
!
! $ Detailed_Input
!
!     IVERTS   is an array containing three vertices of a triangular
!              plate. Each vertex is a three-dimensional vector. The
!              elements
!
!                 IVERTS(J,I), J = 1, 3
!
!              are, respectively, the X, Y, and Z components of the
!              Ith vertex.
!
!
!     DELTA    is a fraction by which the plate is to be scaled.
!              Scaling is done so that the scaled plate has the
!              following properties:
!
!                 -  it is co-planar with the input plate
!
!                 -  its centroid coincides with that of the input
!                    plate
!
!                 -  its sides remain parallel to the corresponding
!                    sides of the input plate
!
!                 -  the distance of each vertex from the centroid is
!                    (1+DELTA) times the corresponding distance for
!                    the input plate
!
! $ Detailed_Output
!
!     OVERTS   is an array containing three vertices of the triangular
!              plate resulting from scaling the input plate.
!
!              If CTROID is the centroid (the average of the vertices)
!              of the input plate, then the Ith vertex of OVERTS
!
!                 OVERTS(J,I), J = 1, 3
!
!              is equal to
!
!                 CTROID(J) + (1+DELTA)*( IVERTS(J,I) - CTROID(J) ),
!
!                 J = 1, 3
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports "greedy" ray-plate intercept algorithms.
!     Such algorithms attempt to ensure that false negatives---in which
!     an intersection is not found due to round-off error---do not
!     occur. In such an algorithm, the plate of interest is expanded
!     slightly before the intersection test is performed.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input
!     (if any), the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Expand an equilateral triangle that lies in the plane
!
!           { (x,y,z) : z = 7 }
!
!        Use an expansion fraction of 1.D0; this doubles the size of
!        the plate.
!
!        Example code begins here.
!
!
!              PROGRAM PLTEXP_EX1
!              IMPLICIT NONE
!
!              DOUBLE PRECISION      DELTA
!              DOUBLE PRECISION      IVERTS ( 3, 3 )
!              DOUBLE PRECISION      OVERTS ( 3, 3 )
!              DOUBLE PRECISION      S
!
!              INTEGER               I
!
!              S = SQRT(3.D0)/2
!
!              CALL VPACK (    S,  -0.5D0,  7.D0, IVERTS(1,1) )
!              CALL VPACK ( 0.D0,    1.D0,  7.D0, IVERTS(1,2) )
!              CALL VPACK (   -S,  -0.5D0,  7.D0, IVERTS(1,3) )
!
!              DELTA = 1.D0
!
!              CALL PLTEXP ( IVERTS, DELTA, OVERTS )
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Vertices of input plate: '
!
!              WRITE (*, '(1X,A,3E18.10)' ) ' I1 = ',
!             .          (IVERTS(I,1), I = 1, 3)
!              WRITE (*, '(1X,A,3E18.10)' ) ' I2 = ',
!             .          (IVERTS(I,2), I = 1, 3)
!              WRITE (*, '(1X,A,3E18.10)' ) ' I3 = ',
!             .          (IVERTS(I,3), I = 1, 3)
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Vertices of output plate: '
!
!              WRITE (*, '(1X,A,3E18.10)' ) ' O1 = ',
!             .          (OVERTS(I,1), I = 1, 3)
!              WRITE (*, '(1X,A,3E18.10)' ) ' O2 = ',
!             .          (OVERTS(I,2), I = 1, 3)
!              WRITE (*, '(1X,A,3E18.10)' ) ' O3 = ',
!             .          (OVERTS(I,3), I = 1, 3)
!              WRITE (*,*) ' '
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Vertices of input plate:
!          I1 =   0.8660254038E+00 -0.5000000000E+00  0.7000000000E+01
!          I2 =   0.0000000000E+00  0.1000000000E+01  0.7000000000E+01
!          I3 =  -0.8660254038E+00 -0.5000000000E+00  0.7000000000E+01
!
!         Vertices of output plate:
!          O1 =   0.1732050808E+01 -0.1000000000E+01  0.7000000000E+01
!          O2 =   0.0000000000E+00  0.2000000000E+01  0.7000000000E+01
!          O3 =  -0.1732050808E+01 -0.1000000000E+01  0.7000000000E+01
!
!
!        Note that the height of the plate is unchanged, but the vectors
!        from the centroid to the vertices have doubled in length.
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
!-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Edits to code example output format for the solution to fit
!        within the $Examples section without modifications.
!
!-    SPICELIB Version 1.0.0, 29-FEB-2016 (NJB)
!
!        Based on original version 28-MAY-2014 (NJB)
!
!-&


! $ Index_Entries
!
!     expand triangular plate
!
!-&


!
!     Local variables
!
      real(wp)              sclctr ( 3 )
      real(wp)              d
      real(wp)              s

!
!     Compute the centroid of the input vertices. Scale the centroid
!     by DELTA, since we'll only use the scaled form.
!
!     Unroll all loops to avoid loop overhead.
!
      d = delta / 3.0_wp

      sclctr(1) = d * ( iverts(1,1) + iverts(1,2) + iverts(1,3) )
      sclctr(2) = d * ( iverts(2,1) + iverts(2,2) + iverts(2,3) )
      sclctr(3) = d * ( iverts(3,1) + iverts(3,2) + iverts(3,3) )

!
!     Compute the offsets of the vertices from the centroid CTROID;
!     scale each offset by (1+DELTA). The Ith expanded vertex is
!
!        CTROID + (1+DELTA) * ( IVERTS(*,I) - CTROID )
!
!     which can be re-written as
!
!        ( (1+DELTA) * IVERTS(*,I) )  -  ( DELTA * CTROID )
!
!     or
!
!        ( (1+DELTA) * IVERTS(*,I) )  -  SCLCTR
!
!
!
      s = 1.0_wp + delta


      overts(1,1) = ( s * iverts(1,1) ) - sclctr(1)
      overts(2,1) = ( s * iverts(2,1) ) - sclctr(2)
      overts(3,1) = ( s * iverts(3,1) ) - sclctr(3)

      overts(1,2) = ( s * iverts(1,2) ) - sclctr(1)
      overts(2,2) = ( s * iverts(2,2) ) - sclctr(2)
      overts(3,2) = ( s * iverts(3,2) ) - sclctr(3)

      overts(1,3) = ( s * iverts(1,3) ) - sclctr(1)
      overts(2,3) = ( s * iverts(2,3) ) - sclctr(2)
      overts(3,3) = ( s * iverts(3,3) ) - sclctr(3)

      end
