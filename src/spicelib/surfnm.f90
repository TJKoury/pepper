!$Procedure SURFNM ( Surface normal vector on an ellipsoid )

      subroutine surfnm ( a, b, c, point, normal )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Compute the outward-pointing, unit normal vector at a point on
!     the surface of an ellipsoid.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ELLIPSOID
!     GEOMETRY
!
! $ Declarations

      implicit none

      real(wp)           a
      real(wp)           b
      real(wp)           c
      real(wp)           point  ( 3 )
      real(wp)           normal ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Length of the ellipsoid semi-axis along the X-axis.
!     B          I   Length of the ellipsoid semi-axis along the Y-axis.
!     C          I   Length of the ellipsoid semi-axis along the Z-axis.
!     POINT      I   Body-fixed coordinates of a point on the ellipsoid.
!     NORMAL     O   Outward pointing unit normal to ellipsoid at POINT.
!
! $ Detailed_Input
!
!     A        is the length of the semi-axis of the ellipsoid that is
!              parallel to the X-axis of the body-fixed reference frame.
!
!     B        is the length of the semi-axis of the ellipsoid that is
!              parallel to the Y-axis of the body-fixed reference frame.
!
!     C        is the length of the semi-axis of the ellipsoid that is
!              parallel to the Z-axis of the body-fixed reference frame.
!
!     POINT    is a 3-vector giving the bodyfixed coordinates of a point
!              on the ellipsoid. In bodyfixed coordinates, the semi-axes
!              of the ellipsoid are aligned with the X, Y, and Z-axes of
!              the reference frame.
!
! $ Detailed_Output
!
!     NORMAL   is the unit vector pointing away from the ellipsoid and
!              normal to the ellipsoid at POINT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the axes are non-positive, the error
!         SPICE(BADAXISLENGTH) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes the outward pointing unit normal vector to
!     the ellipsoid having semi-axes of length A, B, and C from the
!     point POINT.
!
! $ Examples
!
!     A typical use of SURFNM would be to find the angle of incidence
!     of the light from the sun at a point on the surface of an
!     ellipsoid.
!
!     Let Q be a 3-vector representing the rectangular body-fixed
!     coordinates of a point on the ellipsoid (we are assuming that
!     the axes of the ellipsoid are aligned with the axes of the
!     body fixed frame.) Let V be the vector from Q to the sun in
!     bodyfixed coordinates. Then the following code fragment could
!     be used to compute angle of incidence of sunlight at Q.
!
!        CALL SURFNM   ( A, B, C, Q, NRML )
!
!        INCIDN = VSEP ( V,          NRML )
!
! $ Restrictions
!
!     1)  It is assumed that the input POINT is indeed on the ellipsoid.
!         No checking for this is done.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Updated the documentation to refer to "reference frame" instead
!        of "coordinate system" as per NAIF conventions.
!
!-    SPICELIB Version 1.3.2, 23-FEB-2016 (NJB)
!
!        Corrected some typos in the header.
!
!-    SPICELIB Version 1.3.1, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    SPICELIB Version 1.3.0, 02-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VHAT call.
!
!-    SPICELIB Version 1.2.0, 07-AUG-1996 (WLT)
!
!        Added a SAVE statement so that the error message will
!        not be lost between separate invocations of the routine.
!
!-    SPICELIB Version 1.1.0, 21-JUL-1995 (WLT)
!
!        A typo in the $Examples section was corrected
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     surface normal vector on an ellipsoid
!
!-&


!
!     SPICELIB Functions
!
      logical               return

!
!     Local Variables
!
      real(wp)              m
      real(wp)              a1
      real(wp)              b1
      real(wp)              c1

      character(32)         mssg ( 7 )
      integer(ip)           bad
      save

!
!     Initial values
!
      data                  mssg  / 'Axis A was nonpositive.', &
                                    'Axis B was nonpositive.', &
                                    'Axes A and B were nonpositive.', &
                                    'Axis C was nonpositive.', &
                                    'Axes A and C were nonpositive.', &
                                    'Axes B and C were nonpositive.', &
                                    'All three axes were nonpositive.' /

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Check the axes to make sure that none of them is less than or
!     equal to zero. If one is, signal an error and return.
!
      bad = 0

      if ( a <= 0 ) &
         bad = bad + 1

      if ( b <= 0 ) &
         bad = bad + 2

      if ( c <= 0 ) &
         bad = bad + 4


      if ( bad > 0 ) then
         call setmsg ( mssg (bad)                                     // &
                       ' ? '                                           )
         call errch  ( ' ? ', 'The A,B, and C axes were '             // &
                              '#, #, and # respectively.'              )

         call errdp  ( '#', a )
         call errdp  ( '#', b )
         call errdp  ( '#', c )

         call sigerr ( 'SPICE(BADAXISLENGTH)' )
         return
      end if

!
!     Mathematically we want to compute (Px/a**2, Py/b**2, Pz/c**2)
!     and then convert this to a unit vector. However, computationally
!     this can blow up in our faces.  But note that only the ratios
!     a/b, b/c and a/c are important in computing the unit normal.
!     We can use the trick below to avoid the unpleasantness of
!     multiplication and division overflows.
!
      m         = min ( a, b, c )

!
!     M can be divided by A,B or C without fear of an overflow
!     occurring.
!
      a1        = m/a
      b1        = m/b
      c1        = m/c

!
!     All of the terms A1,B1,C1 are less than 1. Thus no overflows
!     can occur.
!
      normal(1) = point(1) * (a1*a1)
      normal(2) = point(2) * (b1*b1)
      normal(3) = point(3) * (c1*c1)

      call vhatip ( normal )

      return
      end
