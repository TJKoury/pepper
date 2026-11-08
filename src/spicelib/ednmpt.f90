!$Procedure EDNMPT ( Ellipsoid normal vector to surface point )

      SUBROUTINE EDNMPT ( A, B, C, NORMAL, POINT )

! $ Abstract
!
!     Return the unique point on an ellipsoid's surface where the
!     outward normal direction is a given vector.
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
!     ELLIPSOID
!     GEOMETRY
!     NORMAL
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Length of the ellipsoid semi-axis along the X-axis.
!     B          I   Length of the ellipsoid semi-axis along the Y-axis.
!     C          I   Length of the ellipsoid semi-axis along the Z-axis.
!     NORMAL     I   Outward normal direction.
!     POINT      O   Point where outward normal is parallel to NORMAL.
!
! $ Detailed_Input
!
!     A        is the length of the semi-axis of the ellipsoid
!              that is parallel to the X-axis of the body-fixed
!              coordinate system.
!
!     B        is the length of the semi-axis of the ellipsoid
!              that is parallel to the Y-axis of the body-fixed
!              coordinate system.
!
!     C        is the length of the semi-axis of the ellipsoid
!              that is parallel to the Z-axis of the body-fixed
!              coordinate system.
!
!     NORMAL   is a non-zero vector. The unique point on the
!              ellipsoid at which NORMAL is an outward normal vector
!              is sought.
!
! $ Detailed_Output
!
!     POINT    is the unique point on the ellipsoid at which NORMAL
!              is an outward normal vector.
!
!              POINT is a 3-vector giving the body-fixed coordinates
!              of a point on the ellipsoid. In body-fixed
!              coordinates, the semi-axes of the ellipsoid are
!              aligned with the X, Y, and Z-axes of the coordinate
!              system.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the semi-axis lengths is non-positive, the error
!         SPICE(BADAXISLENGTH) is signaled.
!
!     2)  If any of the semi-axis lengths underflows to zero when
!         divided by the largest semi-axis length, the error
!         SPICE(AXISUNDERFLOW) is signaled.
!
!     3)  If NORMAL is the zero vector, the error SPICE(ZEROVECTOR)
!         is signaled.
!
!     4)  If the input pass the above checks but lead to a
!         divide-by-zero error or to an computing an invalid argument
!         of a fractional exponential expression, the error
!         SPICE(DEGENERATECASE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine can be used to determine the distance between an
!     ellipsoid and a non-intersecting plane. This distance computation
!     supports computation of terminator points on an ellipsoid.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Choose a triaxial ellipsoid with three unequal semi-axis
!        lengths. Pick several vectors; find the points on the
!        ellipsoid where the respective outward normals are parallel to
!        those vectors.
!
!        Check the results: at each point, a computed outward normal
!        vector should have very small angular separation from the
!        input vector. Also, the point should be on the surface of the
!        ellipsoid. The ellipsoid can be thought of as a level surface
!        of the function
!
!                             2        2         2
!           f(x, y, z) = (x/A)  + (y/B)  +  (z/C)
!
!        where A, B, C are the semi-axis lengths of the ellipsoid.
!        Specifically, the ellipsoid is the set
!
!           { (x, y, z) : f(x, y, z)  =  1 }
!
!        We can evaluate F at a point to determine whether that point
!        is close to the ellipsoid's surface.
!
!
!        Example code begins here.
!
!
!              PROGRAM EDNMPT_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      VSEP
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,F14.8)'  )
!
!              CHARACTER*(*)         FMT3
!              PARAMETER           ( FMT3 = '(A,3F14.8)' )
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      A
!              DOUBLE PRECISION      B
!              DOUBLE PRECISION      C
!              DOUBLE PRECISION      NORMAL ( 3 )
!              DOUBLE PRECISION      POINT  ( 3 )
!              DOUBLE PRECISION      XNORML ( 3 )
!
!        C
!        C     Initialize the ellipsoid semi-axes.
!        C
!              A = 10.D0
!              B =  5.D0
!              C =  2.D0
!
!        C
!        C     Pick several vectors; find the points
!        C     on the ellipsoid where the respective
!        C     outward normals are parallel to those
!        C     vectors; check the results.
!        C
!              CALL VPACK  ( 0.D0, 0.D0, 3.D0, XNORML )
!              CALL EDNMPT ( A,    B,    C,    XNORML, POINT  )
!              CALL SURFNM ( A,    B,    C,    POINT,  NORMAL )
!
!              WRITE (*,*   ) ' '
!              WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
!              WRITE (*,FMT3) 'Input vector:        ', XNORML
!              WRITE (*,FMT3) 'Point:               ', POINT
!              WRITE (*,FMT3) 'Outward normal:      ', NORMAL
!              WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
!             .                                          XNORML )
!              WRITE (*,FMT1) 'Off-surface error:   ',
!             .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
!             .               + (POINT(3)/C)**2 - 1
!              WRITE (*,*) ' '
!
!
!              CALL VPACK  ( 15.D0, -7.D0, 3.D0, XNORML )
!              CALL EDNMPT ( A,      B,    C,    XNORML, POINT  )
!              CALL SURFNM ( A,      B,    C,    POINT,  NORMAL )
!
!              WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
!              WRITE (*,FMT3) 'Input vector:        ', XNORML
!              WRITE (*,FMT3) 'Point:               ', POINT
!              WRITE (*,FMT3) 'Outward normal:      ', NORMAL
!              WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
!             .                                             XNORML )
!              WRITE (*,FMT1) 'Off-surface error:   ',
!             .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
!             .               + (POINT(3)/C)**2 - 1
!              WRITE (*,*) ' '
!
!              CALL VPACK  ( 15.D0, -7.D0, 3.D0, XNORML )
!              CALL EDNMPT ( A,      B,    C,    XNORML, POINT  )
!              CALL SURFNM ( A,      B,    C,    POINT,  NORMAL )
!
!              WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
!              WRITE (*,FMT3) 'Input vector:        ', XNORML
!              WRITE (*,FMT3) 'Point:               ', POINT
!              WRITE (*,FMT3) 'Outward normal:      ', NORMAL
!              WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
!             .                                             XNORML )
!              WRITE (*,FMT1) 'Off-surface error:   ',
!             .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
!             .               + (POINT(3)/C)**2 - 1
!              WRITE (*,*) ' '
!
!              CALL VPACK  ( A/2, B/2,  C/2, XNORML )
!              CALL EDNMPT ( A,   B,    C,   XNORML, POINT  )
!              CALL SURFNM ( A,   B,    C,   POINT,  NORMAL )
!
!              WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
!              WRITE (*,FMT3) 'Input vector:        ', XNORML
!              WRITE (*,FMT3) 'Point:               ', POINT
!              WRITE (*,FMT3) 'Outward normal:      ', NORMAL
!              WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
!             .                                             XNORML )
!              WRITE (*,FMT1) 'Off-surface error:   ',
!             .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
!             .               + (POINT(3)/C)**2 - 1
!              WRITE (*,*) ' '
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
!        Input vector:            0.00000000    0.00000000    3.00000000
!        Point:                   0.00000000    0.00000000    2.00000000
!        Outward normal:          0.00000000    0.00000000    1.00000000
!        Angular error (rad):     0.00000000
!        Off-surface error:       0.00000000
!
!        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
!        Input vector:           15.00000000   -7.00000000    3.00000000
!        Point:                   9.73103203   -1.13528707    0.07784826
!        Outward normal:          0.89165745   -0.41610681    0.17833149
!        Angular error (rad):     0.00000000
!        Off-surface error:       0.00000000
!
!        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
!        Input vector:           15.00000000   -7.00000000    3.00000000
!        Point:                   9.73103203   -1.13528707    0.07784826
!        Outward normal:          0.89165745   -0.41610681    0.17833149
!        Angular error (rad):     0.00000000
!        Off-surface error:       0.00000000
!
!        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
!        Input vector:            5.00000000    2.50000000    1.00000000
!        Point:                   9.69412864    1.21176608    0.07755303
!        Outward normal:          0.88045091    0.44022545    0.17609018
!        Angular error (rad):     0.00000000
!        Off-surface error:       0.00000000
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Modified the output format FMT1 in the code example for the
!        output to fit in the $Examples section without modifications.
!
!-    SPICELIB Version 1.0.0, 17-MAY-2016 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     point on an ellipsoid having given surface normal
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      TOUCHD

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local variables
!
      DOUBLE PRECISION      ARG
      DOUBLE PRECISION      LAMBDA
      DOUBLE PRECISION      NA2
      DOUBLE PRECISION      NB2
      DOUBLE PRECISION      NC2
      DOUBLE PRECISION      SA
      DOUBLE PRECISION      SB
      DOUBLE PRECISION      SC
      DOUBLE PRECISION      SCALE

!
!     Use discovery check-in. We will test RETURN though, since
!     we could have invalid inputs to our arithmetic computations
!     if a SPICE error condition exists upon entry.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Perform some preliminary checks.
!
!     We need a non-degenerate ellipsoid to start with.
!
      IF (      ( A .LE. 0.D0 ) &
           .OR. ( B .LE. 0.D0 ) &
           .OR. ( C .LE. 0.D0 )  ) THEN

         CALL CHKIN  ( 'EDNMPT'                               )
         CALL SETMSG ( 'All ellipsoid semi-axis lengths must ' &
         //            'be strictly positive. Lengths were: ' &
         //            'A = #; B = #; C = #'                  )
         CALL ERRDP  ( '#',  A                                )
         CALL ERRDP  ( '#',  B                                )
         CALL ERRDP  ( '#',  C                                )
         CALL SIGERR ( 'SPICE(BADAXISLENGTH)'                 )
         CALL CHKOUT ( 'EDNMPT'                               )
         RETURN

      END IF

!
!     We'll work with scaled copies of the input semi-axis
!     lengths.
!
      SCALE = MAX ( A, B, C )

      SA    = TOUCHD( A / SCALE )
      SB    = TOUCHD( B / SCALE )
      SC    = TOUCHD( C / SCALE )
!
!     If any of the scaled-semi axes underflowed to zero,
!     we can't continue.
!
      IF (      ( SA .LE. 0.D0 ) &
           .OR. ( SB .LE. 0.D0 ) &
           .OR. ( SC .LE. 0.D0 )  ) THEN

         CALL CHKIN  ( 'EDNMPT'                                   )
         CALL SETMSG ( 'Scaled semi-axis lengths must be ' &
         //            'strictly positive. Scaled lengths were: ' &
         //            'SA = #; SB = #; SC = #'                   )
         CALL ERRDP  ( '#',  SA                                   )
         CALL ERRDP  ( '#',  SB                                   )
         CALL ERRDP  ( '#',  SC                                   )
         CALL SIGERR ( 'SPICE(AXISUNDERFLOW)'                     )
         CALL CHKOUT ( 'EDNMPT'                                   )
         RETURN

      END IF

!
!     The normal vector can't be the zero vector.
!
      IF ( VZERO(NORMAL) ) THEN

         CALL CHKIN  ( 'EDNMPT'                                       )
         CALL SETMSG ( 'The input normal vector was the zero vector. ' &
         //            'There is no solution.'                        )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                            )
         CALL CHKOUT ( 'EDNMPT'                                       )
         RETURN

      END IF

!
!     In general, given an ellipsoid E with axes a, b, c,
!     and given a normal vector
!
!        n = ( n1, n2, n3 )
!
!     this problem is equivalent to finding a point (x,y,z)
!     on E such that
!
!             2     2     2
!        ( x/a,  y/b,  z/c ) = lambda * n
!
!     which implies
!                                           2     2     2
!        ( x, y, z )         = lambda ( n1*a, n2*b, n3*c )
!
!
!     Then since the vector on the left side is on the surface
!     of E, we must have
!
!              2       2   2        2   2       2   2
!        lambda  ( ( n1 * a  ) + (n2 * b ) + (n3 + c ) ) = 1
!
!
!     Requiring lambda to be positive, we have
!
!                               2   2        2   2       2   2
!        lambda = 1 / sqrt( ( n1 * a  ) + (n2 * b ) + (n3 + c ) )
!
!
      NA2 = NORMAL(1) * SA * SA
      NB2 = NORMAL(2) * SB * SB
      NC2 = NORMAL(3) * SC * SC

      ARG = TOUCHD(   ( NA2 * NORMAL(1) ) + ( NB2 * NORMAL(2) ) &
                    + ( NC2 * NORMAL(3) )                        )

      IF ( ARG .LE. 0.D0 ) THEN

         CALL CHKIN  ( 'EDNMPT'                                   )
         CALL SETMSG ( 'Scale factor LAMBDA must be positive, ' &
         //            'but reciprocal of square of LAMBDA is #.' )
         CALL ERRDP  ( '#', ARG                                   )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                    )
         CALL CHKOUT ( 'EDNMPT'                                   )
         RETURN

      END IF

!
!     Compute LAMBDA as above, and scale it too. This will place
!     POINT on the original ellipsoid.
!
      LAMBDA   = ( ARG ** (-0.5D0) ) * SCALE

      POINT(1) = LAMBDA * NA2
      POINT(2) = LAMBDA * NB2
      POINT(3) = LAMBDA * NC2

      END
