!$Procedure NPSGPT ( Nearest point on line segment )

      SUBROUTINE NPSGPT ( EP1, EP2, POINT, PNEAR, DIST )

! $ Abstract
!
!     Find the nearest point on a line segment to a given point.
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
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      EP1   ( 3 )
      DOUBLE PRECISION      EP2   ( 3 )
      DOUBLE PRECISION      POINT ( 3 )
      DOUBLE PRECISION      PNEAR ( 3 )
      DOUBLE PRECISION      DIST

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     EP1,
!     EP2        I   Endpoints of a line segment.
!     POINT      I   A point in 3-dimensional space.
!     PNEAR      O   Nearest point on the line segment to POINT.
!     DIST       O   Distance between PNEAR and POINT.
!
! $ Detailed_Input
!
!     EP1,
!     EP2      are the endpoints of a line segment in 3-dimensional
!              space. EP1 and EP2 need not be distinct.
!
!     POINT    is an arbitrary point in 3-dimensional space.
!
! $ Detailed_Output
!
!     PNEAR    is the closest point on the line segment to POINT.
!
!     DIST     is the distance between POINT and PNEAR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The input segment is allowed to be degenerate: it may be
!         a single point.
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
!     1) Compute the nearest point on a line segment to a given
!        point in a simple case for which the results can easily be
!        checked.
!
!
!        Example code begins here.
!
!
!              PROGRAM NPSGPT_EX1
!              IMPLICIT NONE
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,3F13.8)' )
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      DIST
!              DOUBLE PRECISION      ENDPT1 ( 3 )
!              DOUBLE PRECISION      ENDPT2 ( 3 )
!              DOUBLE PRECISION      PNEAR  ( 3 )
!              DOUBLE PRECISION      POINT  ( 3 )
!
!        C
!        C     Initialize the line segment's endpoints.
!        C
!              CALL VPACK ( 1.D0, -2.D0, 3.D0, ENDPT1 )
!              CALL VPACK ( 1.D0,  2.D0, 3.D0, ENDPT2 )
!        C
!        C     Set the input point.
!        C
!              CALL VPACK ( 1.D0,  0.D0, 0.D0, POINT )
!        C
!        C     Find the near point on the segment.
!        C
!              CALL NPSGPT ( ENDPT1, ENDPT2, POINT, PNEAR, DIST )
!
!              WRITE (*,*) ' '
!              WRITE (*,FMT1) 'Endpoint 1:  ', ENDPT1
!              WRITE (*,FMT1) 'Endpoint 2:  ', ENDPT2
!              WRITE (*,FMT1) 'Point:       ', POINT
!              WRITE (*,*) ' '
!              WRITE (*,FMT1) 'Near point:  ', PNEAR
!              WRITE (*,FMT1) 'Distance:    ', DIST
!              WRITE (*,*) ' '
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Endpoint 1:     1.00000000  -2.00000000   3.00000000
!        Endpoint 2:     1.00000000   2.00000000   3.00000000
!        Point:          1.00000000   0.00000000   0.00000000
!
!        Near point:     1.00000000   0.00000000   3.00000000
!        Distance:       3.00000000
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
!-    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 02-FEB-2016 (NJB)
!
!        Updated from DSKLIB Version 1.0.0, 20-MAR-2015 (NJB)
!
!-&


! $ Index_Entries
!
!     nearest point on line segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local variables
!
      DOUBLE PRECISION      SEG    ( 3 )
      DOUBLE PRECISION      SEGDOT
      DOUBLE PRECISION      LNEAR  ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      OFFDOT

!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Find a direction vector defined by the endpoints.
!
      CALL VSUB ( EP2, EP1, SEG )

      IF ( VZERO(SEG) ) THEN
!
!        The endpoints coincide, and both coincide with the
!        near point.
!
         CALL VEQU ( EP1, PNEAR )

         DIST = VDIST( EP1, POINT )

         RETURN

      END IF

!
!     Find the nearest point to POINT on the line defined by
!     EP1 and SEG.
!
      CALL NPLNPT ( EP1, SEG, POINT, LNEAR, DIST )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Determine whether LNEAR is on the segment, "before" EP1, or
!     "after" EP2, where SEG points in the "increasing" direction.
!
      CALL VSUB ( LNEAR, EP1, OFFSET )

      OFFDOT = VDOT( OFFSET, SEG )

      IF ( OFFDOT .LT. 0.D0 ) THEN
!
!        The nearest point on the line precedes the first endpoint.
!        The closest point on the segment is the first endpoint.
!
         CALL VEQU ( EP1, PNEAR )

         DIST = VDIST ( EP1, POINT )

      ELSE
!
!        See whether OFFSET is past the second endpoint. Compare
!        the dot product of OFFSET with SEG to that of SEG with
!        itself, since SEG is the offset of EP2 from EP1.
!
         SEGDOT = VDOT( SEG, SEG )

         IF ( OFFDOT .GT. SEGDOT ) THEN
!
!           The nearest point on the line follows the last endpoint.
!           The closest point on the segment is the last endpoint.
!
            CALL VEQU ( EP2, PNEAR )

            DIST = VDIST ( EP2, POINT )

         ELSE
!
!           The near point is on the segment. LNEAR is actually the
!           solution.
!
            CALL VEQU ( LNEAR, PNEAR )

!
!           DIST was correctly set by the call to NPLNPT.
!
         END IF

      END IF

      RETURN
      END
