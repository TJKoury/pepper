!$Procedure SURFNM ( Surface normal vector on an ellipsoid )

      SUBROUTINE SURFNM ( A, B, C, POINT, NORMAL )

! $ Abstract
!
!     Compute the outward-pointing, unit normal vector at a point on
!     the surface of an ellipsoid.
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
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   A
      DOUBLE PRECISION   B
      DOUBLE PRECISION   C
      DOUBLE PRECISION   POINT  ( 3 )
      DOUBLE PRECISION   NORMAL ( 3 )

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
      LOGICAL               RETURN

!
!     Local Variables
!
      DOUBLE PRECISION      M
      DOUBLE PRECISION      A1
      DOUBLE PRECISION      B1
      DOUBLE PRECISION      C1

      CHARACTER*32          MSSG ( 7 )
      INTEGER               BAD
      SAVE

!
!     Initial values
!
      DATA                  MSSG  / 'Axis A was nonpositive.', &
                                    'Axis B was nonpositive.', &
                                    'Axes A and B were nonpositive.', &
                                    'Axis C was nonpositive.', &
                                    'Axes A and C were nonpositive.', &
                                    'Axes B and C were nonpositive.', &
                                    'All three axes were nonpositive.' /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SURFNM' )
      END IF


!
!     Check the axes to make sure that none of them is less than or
!     equal to zero. If one is, signal an error and return.
!
      BAD = 0

      IF ( A .LE. 0 ) &
         BAD = BAD + 1

      IF ( B .LE. 0 ) &
         BAD = BAD + 2

      IF ( C .LE. 0 ) &
         BAD = BAD + 4


      IF ( BAD .GT. 0 ) THEN
         CALL SETMSG ( MSSG (BAD)                                     // &
                       ' ? '                                           )
         CALL ERRCH  ( ' ? ', 'The A,B, and C axes were '             // &
                              '#, #, and # respectively.'              )

         CALL ERRDP  ( '#', A )
         CALL ERRDP  ( '#', B )
         CALL ERRDP  ( '#', C )

         CALL SIGERR ( 'SPICE(BADAXISLENGTH)' )
         CALL CHKOUT ( 'SURFNM' )
         RETURN
      END IF

!
!     Mathematically we want to compute (Px/a**2, Py/b**2, Pz/c**2)
!     and then convert this to a unit vector. However, computationally
!     this can blow up in our faces.  But note that only the ratios
!     a/b, b/c and a/c are important in computing the unit normal.
!     We can use the trick below to avoid the unpleasantness of
!     multiplication and division overflows.
!
      M         = MIN ( A, B, C )

!
!     M can be divided by A,B or C without fear of an overflow
!     occurring.
!
      A1        = M/A
      B1        = M/B
      C1        = M/C

!
!     All of the terms A1,B1,C1 are less than 1. Thus no overflows
!     can occur.
!
      NORMAL(1) = POINT(1) * (A1*A1)
      NORMAL(2) = POINT(2) * (B1*B1)
      NORMAL(3) = POINT(3) * (C1*C1)

      CALL VHATIP ( NORMAL )

      CALL CHKOUT ( 'SURFNM' )
      RETURN
      END
