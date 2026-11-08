!$Procedure EDPNT ( Ellipsoid point  )

      subroutine edpnt ( p, a, b, c, ep )

! $ Abstract
!
!     Scale a point so that it lies on the surface of a specified
!     triaxial ellipsoid that is centered at the origin and aligned
!     with the Cartesian coordinate axes.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ELLIPSOID
!     GEOMETRY
!     MATH
!
! $ Declarations

      implicit none

      double precision      p  ( 3 )
      double precision      a
      double precision      b
      double precision      c
      double precision      ep ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   A point in three-dimensional space.
!     A          I   Semi-axis length in the X direction.
!     B          I   Semi-axis length in the Y direction.
!     C          I   Semi-axis length in the Z direction.
!     EP         O   Point on ellipsoid.
!
! $ Detailed_Input
!
!     P        is a non-zero point in three-dimensional space.
!
!     A,
!     B,
!     C        are, respectively, the semi-axis lengths of a triaxial
!              ellipsoid in the X, Y, and Z directions. The axes of
!              the ellipsoid are aligned with the axes of the
!              Cartesian coordinate system.
!
! $ Detailed_Output
!
!     EP       is the result of scaling the input point P so that
!              it lies on the surface of the triaxial ellipsoid
!              defined by the input semi-axis lengths.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the target ellipsoid's semi-axis lengths is
!         non-positive, the error SPICE(INVALIDAXES) is signaled.
!
!     2)  If P is the zero vector, the error SPICE(ZEROVECTOR) is
!         signaled.
!
!     3)  If the level surface parameter of the input point
!         underflows, the error SPICE(POINTTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine efficiently computes the ellipsoid surface point
!     corresponding to a specified ray emanating from the origin.
!     Practical examples of this computation occur in the SPICELIB
!     routines LATSRF and SRFREC.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1) Find the surface intercept point on an ellipsoid having radii
!
!            ( 3, 2, 1 )
!
!        of the ray emanating from the origin and having direction
!        vector
!
!            ( 1, 1, 1 )
!
!
!        Example code begins here.
!
!
!              PROGRAM EDPNT_EX1
!              IMPLICIT NONE
!
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,F18.14)'  )
!
!              CHARACTER*(*)         FMT3
!              PARAMETER           ( FMT3 = '(A,3F18.14)' )
!
!              DOUBLE PRECISION      A
!              DOUBLE PRECISION      B
!              DOUBLE PRECISION      C
!              DOUBLE PRECISION      V      ( 3 )
!              DOUBLE PRECISION      EP     ( 3 )
!              DOUBLE PRECISION      LEVEL
!
!              A = 3.D0
!              B = 2.D0
!              C = 1.D0
!
!              CALL VPACK ( 1.D0, 1.D0, 1.D0, V )
!
!              CALL EDPNT ( V, A, B, C, EP )
!
!              WRITE (*,FMT3) 'EP    = ', EP
!
!        C
!        C     Verify that EP is on the ellipsoid.
!        C
!              LEVEL = (EP(1)/A)**2 + (EP(2)/B)**2 + (EP(3)/C)**2
!
!              WRITE (*,FMT1) 'LEVEL = ', LEVEL
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        EP    =   0.85714285714286  0.85714285714286  0.85714285714286
!        LEVEL =   1.00000000000000
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 09-JUL-2020 (JDR)
!
!        Minor edits to the header and code example.
!
!-    SPICELIB Version 2.0.0, 19-APR-2016 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     scale point to lie on ellipsoid
!
!-&


!
!     SPICELIB functions
!
      double precision      touchd

      logical               failed
      logical               vzero

!
!     Local variables
!
      double precision      level
      double precision      sq

!
!     Use discovery check-in.
!
      if ( failed() ) then
         return
      end if

      if (      ( a .le. 0.d0 ) &
           .or. ( b .le. 0.d0 ) &
           .or. ( c .le. 0.d0 ) ) then

         call chkin  ( 'EDPNT'                            )
         call setmsg ( 'Ellipsoid radii must be strictly ' &
         //            'positive but are (#, #, #).'      )
         call errdp  ( '#', a                             )
         call errdp  ( '#', b                             )
         call errdp  ( '#', c                             )
         call sigerr ( 'SPICE(INVALIDRADII)'              )
         call chkout ( 'EDPNT'                            )
         return

      end if

!
!     The input point must be non-zero, or we can't scale it
!     to the ellipsoid.
!
      if ( vzero(p) ) then

         call chkin  ( 'EDPNT'                            )
         call setmsg ( 'Input point was the zero vector. ' &
         //            'A non-zero vector is required.'   )
         call sigerr ( 'SPICE(ZEROVECTOR)'                )
         call chkout ( 'EDPNT'                            )
         return

      end if

!
!     Find the level surface parameter of the input point with respect
!     to the scaled ellipsoid.
!
      level = touchd( ( p(1)/a )**2 + ( p(2)/b )**2 + ( p(3)/c )**2  )

      if ( level .le. 0.d0 ) then
!
!        We expect that LEVEL will be non-negative, but it could
!        be zero. We check for negative values as a precaution.
!
         call chkin  ( 'EDPNT'                                    )
         call setmsg ( 'Input point''s level surface parameter ' &
         //            'was non-positive. The point is too close ' &
         //            'to the origin to be scaled to the ' &
         //            'ellipsoid. The point was (#, #, #).'      )
         call errdp  ( '#', p(1)                                  )
         call errdp  ( '#', p(2)                                  )
         call errdp  ( '#', p(3)                                  )
         call sigerr ( 'SPICE(POINTTOOSMALL)'                     )
         call chkout ( 'EDPNT'                                    )
         return

      end if

!
!     Scale the point to one for which the level surface parameter is 1.
!
      sq    = sqrt(level)

      ep(1) = p(1) / sq
      ep(2) = p(2) / sq
      ep(3) = p(3) / sq

      return
      end
