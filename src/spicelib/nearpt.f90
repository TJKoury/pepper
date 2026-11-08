!$Procedure NEARPT ( Nearest point on an ellipsoid )

      SUBROUTINE NEARPT ( POSITN, A, B, C, NPOINT, ALT )

! $ Abstract
!
!     Locate the point on the surface of an ellipsoid that is nearest
!     to a specified position. Also return the altitude of the position
!     above the ellipsoid.
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
!     ALTITUDE
!     ELLIPSOID
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      POSITN ( 3 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      NPOINT ( 3 )
      DOUBLE PRECISION      ALT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POSITN     I   Position of a point in body-fixed frame.
!     A          I   Length of semi-axis parallel to x-axis.
!     B          I   Length of semi-axis parallel to y-axis.
!     C          I   Length on semi-axis parallel to z-axis.
!     NPOINT     O   Point on the ellipsoid closest to POSITN.
!     ALT        O   Altitude of POSITN above the ellipsoid.
!
! $ Detailed_Input
!
!     POSITN   is a 3-vector giving the position of a point with respect
!              to the center of an ellipsoid. The vector is expressed
!              in a body-fixed reference frame. The semi-axes of the
!              ellipsoid are aligned with the x, y, and z-axes of the
!              body-fixed frame.
!
!     A        is the length of the semi-axis of the ellipsoid that is
!              parallel to the x-axis of the body-fixed reference frame.
!
!     B        is the length of the semi-axis of the ellipsoid that is
!              parallel to the y-axis of the body-fixed reference frame.
!
!     C        is the length of the semi-axis of the ellipsoid that is
!              parallel to the z-axis of the body-fixed reference frame.
!
! $ Detailed_Output
!
!     NPOINT   is the nearest point on the ellipsoid to POSITN.
!              NPOINT is a 3-vector expressed in the body-fixed
!              reference frame.
!
!     ALT      is the altitude of POSITN above the ellipsoid. If
!              POSITN is inside the ellipsoid, ALT will be negative
!              and have magnitude equal to the distance between
!              NPOINT and POSITN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the axis lengths A, B or C are non-positive, the
!         error SPICE(BADAXISLENGTH) is signaled.
!
!     2)  If the ratio of the longest to the shortest ellipsoid axis
!         is large enough so that arithmetic expressions involving its
!         squared value may overflow, the error SPICE(BADAXISLENGTH)
!         is signaled.
!
!     3)  If any of the expressions
!
!            A * ABS( POSITN(1) ) / m**2
!            B * ABS( POSITN(2) ) / m**2
!            C * ABS( POSITN(3) ) / m**2
!
!         where `m' is the minimum of { A, B, C }, is large enough so
!         that arithmetic expressions involving these sub-expressions
!         may overflow, the error SPICE(INPUTSTOOLARGE) is signaled.
!
!     4)  If the axes of the ellipsoid have radically different
!         magnitudes, for example if the ratios of the axis lengths vary
!         by 10 orders of magnitude, the results may have poor
!         precision. No error checks are done to identify this problem.
!
!     5)  If the axes of the ellipsoid and the input point POSITN have
!         radically different magnitudes, for example if the ratio of
!         the magnitude of POSITN to the length of the shortest axis is
!         1.E25, the results may have poor precision. No error checks
!         are done to identify this problem.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Many applications of this routine are more easily performed
!     using the higher-level SPICELIB routine SUBPNT. This routine
!     is the mathematical workhorse on which SUBPNT relies.
!
!     This routine solves for the location, N, on the surface of an
!     ellipsoid nearest to an arbitrary location, P, relative to that
!     ellipsoid.
!
! $ Examples
!
!     Example 1.
!
!     The code fragment below illustrates how you can use SPICELIB to
!     compute the apparent sub-earth point on the moon.
!
!     C
!     C     Load the ephemeris, leapseconds and physical constants
!     C     files first. We assume the names of these files are
!     C     stored in the character variables SPK, LSK and
!     C     PCK.
!     C
!           CALL FURNSH ( SPK )
!           CALL FURNSH ( LSK )
!           CALL FURNSH ( PCK )
!
!     C
!     C     Get the apparent position of the moon as seen from the
!     C     earth. Look up this position vector in the moon
!     C     body-fixed frame IAU_MOON. The orientation of the
!     C     IAU_MOON frame will be computed at epoch ET-LT.
!     C
!           CALL SPKPOS ( 'moon',  ET,    'IAU_MOON', 'LT+S',
!          .              'earth', TRGPOS, LT                 )
!
!     C
!     C     Negate the moon's apparent position to obtain the
!     C     position of the earth in the moon's body-fixed frame.
!     C
!           CALL VMINUS ( TRGPOS, EVEC )
!
!     C
!     C     Get the lengths of the principal axes of the moon.
!     C     Transfer the elements of the array RADII to the
!     C     variables A, B, C to enhance readability.
!     C
!           CALL BODVRD (  'MOON',    'RADII', DIM, RADII )
!           CALL VUPACK (  RADII,  A,       B,   C     )
!
!     C
!     C     Finally get the point SUBPNT on the surface of the
!     C     moon closest to the earth --- the sub-earth point.
!     C     SUBPNT is expressed in the IAU_MOON reference frame.
!     C
!           CALL NEARPT ( EVEC, A, B, C, SUBPNT, ALT )
!
!
!     Example 2.
!
!     One can use this routine to define a generalization of GEODETIC
!     coordinates called GAUSSIAN coordinates of a triaxial body. (The
!     name is derived from the famous Gauss-map of classical
!     differential geometry).  The coordinates are longitude,
!     latitude, and altitude.
!
!     We let the x-axis of the body fixed coordinate system point
!     along the longest axis of the triaxial body. The y-axis points
!     along the middle axis and the z-axis points along the shortest
!     axis.
!
!     Given a point P, there is a point on the ellipsoid that is
!     closest to P, call it Q. The latitude and longitude of P
!     are determined by constructing the outward pointing unit normal
!     to the ellipsoid at Q. Latitude of P is the latitude that the
!     normal points toward in the body-fixed frame. Longitude is
!     the longitude the normal points to in the body-fixed frame.
!     The altitude is the signed distance from P to Q, positive if P
!     is outside the ellipsoid, negative if P is inside.
!     (the mapping of the point Q to the unit normal at Q is the
!     Gauss-map of Q).
!
!     To obtain the Gaussian coordinates of a point whose position
!     in body-fixed rectangular coordinates is given by a vector P,
!     the code fragment below will suffice.
!
!        CALL NEARPT ( P,    A, B, C, Q, ALT  )
!        CALL SURFNM (       A, B, C  Q, NRML )
!
!        CALL RECLAT ( NRML, R, LONG, LAT     )
!
!     The Gaussian coordinates are LONG, LAT, and ALT.
!
! $ Restrictions
!
!     See $Exceptions section.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 26-OCT-2021 (NJB) (JDR) (EDW)
!
!        Edit to logic to reduce unneeded operations when
!        error or projection vectors equal zero. Addition
!        of details concerning the "ellipsoid near point"
!        problem and solution.
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.4.0, 27-JUN-2013 (NJB)
!
!        Updated in-line comments.
!
!     Last update was 04-MAR-2013 (NJB)
!
!        Bug fix: now correctly computes off-axis solution for
!        the case of a prolate ellipsoid and a viewing point
!        on the interior long axis.
!
!-    SPICELIB Version 1.3.1, 07-FEB-2008 (NJB)
!
!        Header update: header now refers to SUBPNT rather
!        than deprecated routine SUBPT.
!
!-    SPICELIB Version 1.3.0, 07-AUG-2006 (NJB)
!
!        Bug fix: added initialization of variable SNGLPT to support
!                  operation under the Macintosh Intel Fortran
!                  compiler. Note that this bug did not affect
!                  operation of this routine on other platforms.
!
!-    SPICELIB Version 1.2.0, 15-NOV-2005 (EDW) (NJB)
!
!        Various changes were made to ensure that all loops terminate.
!
!        Bug fix: scale of transverse component of error vector
!        was corrected for the exterior point case.
!
!        Bug fix: non-standard use of duplicate arguments in VSCL
!        calls was corrected.
!
!        Error checking was added to screen out inputs that might
!        cause numeric overflow.
!
!        Replaced BODVAR call in examples to BODVRD.
!
!-    SPICELIB Version 1.1.1, 28-JUL-2003 (NJB) (CHA)
!
!        Various header corrections were made.
!
!-    SPICELIB Version 1.1.0, 27-NOV-1990 (WLT)
!
!        The routine was substantially rewritten to achieve
!        more robust behavior and document the mathematics
!        of the routine.
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
!     distance from point to ellipsoid
!     nearest point on an ellipsoid
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 27-NOV-1990 (NJB)
!
!        Bug fix: added initialization of variable SNGLPT to support
!                  operation under the Macintosh Intel Fortran
!                  compiler. Note that this bug did not affect
!                  operation of this routine on other platforms. The
!                  statement referencing the uninitialized variable
!                  was:
!
!           IF ( INSIDE  .AND. (      SNGLPT .EQ. 2
!     .                          .OR. SNGLPT .EQ. 3 ) ) THEN
!
!        SNGLPT is uninitialized only if INSIDE is .FALSE.,
!        so the  value of the logical expression is not affected by
!        the uninitialized value of SNGLPT.
!
!        However, the Intel Fortran compiler for the Mac flags a runtime
!        error when the above code is exercised. So SNGLPT is now
!        initialized prior to the above IF statement.
!
!
!-    SPICELIB Version 1.2.0, 15-NOV-2005 (EDW) (NJB)
!
!        Bug fix: scale of transverse component of error vector
!        was corrected for the exterior point case.
!        Replaced BODVAR call in examples to BODVRD.
!
!        Bug fix: non-standard use of duplicate arguments in VSCL
!        calls was corrected.
!
!        Various changes were made to ensure that all loops terminate.
!
!        Error checking was added to screen out inputs that might
!        cause numeric overflow.
!
!        Removed secant solution branch from root-finding loop.
!        Although the secant solution sped up some root searches,
!        it caused large numbers of unnecessary iterations in others.
!
!        Changed the expression:
!
!           IF (       LAMBDA .EQ. LOWER
!     .          .OR.  LAMBDA .EQ. UPPER ) THEN
!
!        to
!
!           IF (      APPROX( LAMBDA, LOWER, CNVTOL )
!     .          .OR. APPROX( LAMBDA, UPPER, CNVTOL )  ) THEN
!
!        Use of APPROX eliminates the possibility of an infinite loop
!        when LAMBDA approaches to within epsilon of, but does not
!        equate to UPPER or LOWER. Infinite loops occurred under some
!        compiler's optimizations.
!
!        The loop also includes a check on number of iterations,
!        signaling an error if the bisection loop uses more than
!        MAXITR passes.
!
!        TOUCHD is now used to defeat extended-register usage in
!        cases where such usage may cause logic problems.
!
!        Some minor code changes were made to ensure that various
!        variables remain in their expected ranges.
!
!        A few code changes were made to enhance clarity.
!
!
!-    SPICELIB Version 1.1.0, 27-NOV-1990 (WLT)
!
!        The routine was nearly rewritten so that points
!        near the coordinate planes in the interior of the ellipsoid
!        could be handled without fear of floating point overflow
!        or divide by zero.
!
!        While the mathematical ideas involved in the original routine
!        are retained, the code is for the most part new. In addition,
!        the new code has been documented far more extensively than was
!        NEARPT 1.0.0.
!
!
!-    Beta Version 2.0.0, 9-JAN-1989 (WLT)
!
!        Error handling added has been added for bad axes values.
!
!        The algorithm did not work correctly for some points inside
!        the ellipsoid lying on the plane orthogonal to the shortest
!        axis of the ellipsoid. The problem was corrected.
!
!        Finally the algorithm was made slightly more robust and clearer
!        by use of SPICELIB routines and by normalizing the inputs.
!
!        Add an example to the header section.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM

      LOGICAL               APPROX
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Parameters
!
!
!     The convergence tolerance CNVTOL is used to terminate the
!     bisection loop when the solution interval is very small but
!     hasn't converged to length zero.  This situation can occur when
!     the root is extremely close to zero.
!
      DOUBLE PRECISION      CNVTOL
      PARAMETER           ( CNVTOL = 1.D-16 )


!
!     Various potentially large numbers we'll compute must not
!     exceed DPMAX()/MARGIN:
!
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D2 )

!
!     The parameter MAXSOL determines the maximum number of
!     iterations that will be performed in locating the
!     near point.  This must be at least 3.  To get strong
!     robustness in the routine, MAXSOL should be at least 4.
!
      INTEGER               MAXSOL
      PARAMETER           ( MAXSOL  = 6 )

!
!     MAXITR defines the maximum number of iterations allowed in
!     the bisection loop used to find LAMBDA.  If this loop requires
!     more than MAXITR iterations to achieve convergence, NEARPT
!     will signal an error.
!
!     On a PC/Linux/g77 platform, it has been observed that each
!     bisection loop normally completes in fewer than 70 iterations.
!     MAXITR is used as a "backstop" to prevent infinite looping in
!     case the normal loop termination conditions don't take effect.
!     The value selected is based on the range of exponents for IEEE
!     double precision floating point numbers.
!
      INTEGER               MAXITR
      PARAMETER           ( MAXITR  = 2048 )

!
!     Length of lines in message buffer.
!
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 80 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    MSSG   ( 7 )

      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      AXISQR ( 3 )
      DOUBLE PRECISION      BESTHT
      DOUBLE PRECISION      BESTPT ( 3 )
      DOUBLE PRECISION      COPY   ( 3 )
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DENOM2
      DOUBLE PRECISION      DENOM3
      DOUBLE PRECISION      EPOINT ( 3 )
      DOUBLE PRECISION      ERR    ( 3 )
      DOUBLE PRECISION      ERRP   ( 3 )
      DOUBLE PRECISION      FACTOR
      DOUBLE PRECISION      HEIGHT
      DOUBLE PRECISION      LAMBDA
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      NEWERR
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OLDERR
      DOUBLE PRECISION      ORIGNL ( 3 )
      DOUBLE PRECISION      PNORM
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PRODCT
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      QLOWER
      DOUBLE PRECISION      QUPPER
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SIGN
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TEMP
      DOUBLE PRECISION      TERM   ( 3 )
      DOUBLE PRECISION      TOOBIG
      DOUBLE PRECISION      TLAMBD ( 3 )
      DOUBLE PRECISION      UPPER

      INTEGER               BAD
      INTEGER               I
      INTEGER               IORDER ( 3 )
      INTEGER               ITR
      INTEGER               SNGLPT
      INTEGER               SOLUTN

      LOGICAL               EXTRA
      LOGICAL               INSIDE
      LOGICAL               SOLVNG
      LOGICAL               TRIM

!
!     Saved variables
!
      SAVE                  MSSG

!
!     Initial values
!
      DATA                  MSSG / 'Axis A was nonpositive. ?', &
                                   'Axis B was nonpositive. ?', &
                                   'Axes A and B were nonpositive. ?', &
                                   'Axis C was nonpositive. ?', &
                                   'Axes A and C were nonpositive. ?', &
                                   'Axes B and C were nonpositive. ?', &
                                   'All three axes were nonpositive. ?'/


!
!     Here's what you can expect to find in the routine below.
!
!        Chapter 1.  Error and Exception Handling.
!
!        Chapter 2.  Mathematical background for the solution---the
!                    lambda equation.
!
!        Chapter 3.  Initializations for the main processing loop.
!
!        Chapter 4.  Mathematical Solution of the lambda equation.
!
!                    Section 4.1  Avoiding numerical difficulties.
!                    Section 4.2  Bracketing the root of the lambda
!                                 equation.
!                    Section 4.3  Refining the estimate of lambda.
!                    Section 4.4  Handling points on the central plane.
!
!        Chapter 5.  Decisions and initializations for sharpening
!                    the solution.
!
!        Chapter 6.  Clean up.
!

!
!     Chapter 1
!
!     Error and Exception Handling.
!     ================================================================
!     ----------------------------------------------------------------
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NEARPT' )
      END IF

!
!     Check the axes to make sure that none of them is less than or
!     equal to zero. If one is, signal an error and return.
!
      BAD = 0

      IF ( A .LE. 0.0D0 ) THEN
         BAD = BAD + 1
      END IF

      IF ( B .LE. 0.0D0 ) THEN
         BAD = BAD + 2
      END IF

      IF ( C .LE. 0.0D0 ) THEN
         BAD = BAD + 4
      END IF


      IF ( BAD .GT. 0 ) THEN

         CALL SETMSG ( MSSG (BAD)                       )
         CALL ERRCH  ( '?', 'The A,B, and C axes were ' // &
                            '#, #, and # respectively.' )

         CALL ERRDP  ( '#', A )
         CALL ERRDP  ( '#', B )
         CALL ERRDP  ( '#', C )

         CALL SIGERR ( 'SPICE(BADAXISLENGTH)' )
         CALL CHKOUT ( 'NEARPT' )
         RETURN

      END IF



!
!     Chapter 2
!
!     Mathematical background for the solution---the lambda equation.
!     ================================================================
!     ----------------------------------------------------------------
!
!     You can describe this problem in two ways, either using a
!     geometric description of the required conditions for the
!     solution, or a functional description based on minimization.
!
!     Geometric description:
!
!     Here is the background and general outline of how this problem is
!     going to be solved.
!
!     We want to find a point on the ellipsoid
!
!
!           X**2       Y**2       Z**2
!          ------  +  ------  +  ------  =  1                       [1]
!           A**2       B**2       C**2
!
!     that is closest to the input point POSITN.
!
!         If one cares about the gory details, we know that such a
!         point must exist because the ellipsoid is a compact subset of
!         Euclidean 3-space and the distance function between the input
!         point and the ellipsoid is continuous. Since any continuous
!         function on a compact set actually achieves its minimum at
!         some point of the compact set, we are guaranteed that a
!         closest point exists. The closest point may not be unique if
!         the input point is inside the ellipsoid.
!
!     If we let NPOINT be a closest point to POSITN, then the line
!     segment joining POSITN to NPOINT is parallel to the normal to the
!     ellipsoid at NPOINT.  Moreover, suppose we let SEGMENT(P) be the
!     line segment that connects an arbitrary point P with POSITN.  It
!     can be shown that there is only one point P on the ellipsoid in
!     the same octant at POSITN such that the normal at P is parallel
!     to SEGMENT(P).
!
!
!         More gory details: A normal to a point (X,Y,Z)
!         on the ellipsoid is given by
!
!               X      Y      Z
!           ( -----, -----, ----- )                                 [2]
!              A**2   B**2   C**2
!
!         Given a fixed lambda, and allowing (X,Y,Z) to
!         range over all points on the ellipsoid, the set
!         of points
!
!
!                 lambda*X      lambda*Y      lambda*Z
!           ( X + --------, Y + --------, Z + -------- )            [3]
!                   A**2          B**2          C**2
!
!         describes another ellipsoid with axes having lengths
!
!                  lambda         lambda        lambda
!              A + ------ ,   B + ------ ,  C + ------  .           [4]
!                    A              B             C
!
!         Moreover, as long as lambda > - MIN( A**2, B**2, C**2 )
!         none of these ellipsoids intersect.  Thus, as long as
!         the normal lines are not allowed to cross the coordinate plane
!         orthogonal to the smallest axis (called the central plane)
!         they do not intersect.
!
!
!         Finally every point that does not lie on the central plane
!         lies on one of the "lambda" ellipsoids described above.
!
!         Consequently, for each point, P, not on the central plane
!         there is a unique point, NPOINT, on the ellipsoid, such that
!         the normal line at NPOINT also contains P and does not cross
!         the central plane.
!
!
!     From the above discussion we see that we can mathematically
!     solve the near point problem by finding a point NPOINT
!     on the ellipsoid given by the equation:
!
!           X**2       Y**2       Z**2
!          ------  +  ------  +  ------  =  1                       [5]
!           A**2       B**2       C**2
!
!
!     such that for some value of lambda
!
!          POSITN = NPOINT + LAMBDA * NORMAL(NPOINT).
!
!     Moreover, if POSITN = (Px,Py,Pz) then lambda must satisfy
!     the equation:
!
!              2                 2                2
!           Px                 Py               Pz
!      -------------   +  -------------  +  ------------  = 1       [6]
!                  2                2               2
!      (A + lambda)       (B + lambda)      (C + lambda)
!           ------             ------            ------
!              A                 B                 C
!
!     and lambda must be greater than -MIN(A**2,B**2,C**2)
!
!
!     Once lambda is known, NPOINT can be computed from the equation:
!
!          POSITN = NPOINT + LAMBDA*NORMAL(NPOINT).
!
!
!     The process of solving for lambda can be viewed as selecting
!     that ellipsoid
!
!                2                 2               2
!               x                 y               z
!      --------------- + ---------------- + ---------------  = 1    [7]
!                     2                  2                 2
!       (a + lambda/a)    ( b + lambda/b)    (c + lambda/c)
!
!     that contains the input point POSITN.  For lambda = 0, this
!     ellipsoid is just the input ellipsoid.  When we increase
!     lambda we get a larger "inflated" ellipsoid.  When we
!     decrease lambda we get a smaller "deflated" ellipsoid.  Thus,
!     the search for lambda can be viewed as inflating or deflating
!     the input ellipsoid (in a specially prescribed manner) until
!     the resulting ellipsoid contains the input point POSITN.
!
!     The mathematical solution laid out above, has some numeric
!     flaws.  However, it is robust enough so that if it is applied
!     repeatedly, we can converge to a good solution of the near point
!     problem.
!
!     In the code that follows, we will first solve the lambda equation
!     using the original input point.  However, for points near the
!     central plane the solution we obtain may not lie on the
!     ellipsoid.  But, it should lie along the correct normal line.
!
!     Using this first candidate solution, we find the closest point
!     to it on the ellipsoid.  This second iteration always produces
!     a point that is as close as you can get to the ellipsoid.
!     However, the normal at this second solution may not come as close
!     as desired to pointing toward the input position.  To overcome
!     this deficiency we sharpen the second solution.
!
!     To sharpen a solution we use the computed near point, the
!     computed altitude of POSITN and the normal at the near point to
!     approximate POSITN. The difference between the approximated
!     position of POSITN and the input value of POSITN is called the
!     error vector. To get a sharpened solution we translate the
!     computed near point in the direction of the component of the
!     error vector orthogonal to the normal. We then find the
!     mathematical near point to our translated solution.
!
!     The sharpening process is repeated until it no longer produces
!     an "improved" near point.
!
!     At each step of this procedure, we must compute a solution to
!     the "lambda" equation in order to produce our next estimate of
!     the near point.  If it were possible to create a "private"
!     routine in FORTRAN that only this routine could access, we
!     would do it.  However, things being what they are, we have to
!     compute the lambda solution in a loop.  We keep track of which
!     refinement we are working on by counting the number of
!     lambda solutions that are computed.
!
!     Functional description:
!
!     The problem also defines a minimization condition solvable with
!     the technique of Lagrange Multipliers. In this case,
!     minimize the distance between the ellipsoid surface (set) and the
!     location P subject to the ellipsoid constraint, the solution,
!     NPOINT, an element of the ellipsoid set.
!
!     Define the Lagrange expression, L, as
!
!        L = f - lambda * g                                         [8]
!
!     with f the function to minimize, and g the constraint on f.
!
!     Solve
!
!        D*L = 0                                                    [9]
!
!     with
!
!        g = 0                                                     [10]
!
!     which gives us the expressions
!
!        D*f = lambda * D*g                                        [11]
!        g = 0
!
!     where D = (Dx, Dy, Dz).
!                                     2
!      Choose f(x,y,z) as ||P-NPOINT|| rather than ||P-NPOINT||
!      to simplify the resulting derivatives. Both functions have the
!      same minimum.
!
!        f(x,y,z) = (Px - X)**2 + (Py-Y)**2 + (Pz-Z)**2            [12]
!
!       The constrain equation
!
!                    X**2       Y**2       Z**2
!        g(x,y,z) = ------  +  ------  +  ------  - 1              [13]
!                    A**2       B**2       C**2
!
!      The resulting equations from [11]
!
!         (Px-X) = lambda *   X                                    [14]
!                            ---
!                            A**2
!
!         (Py-Y) = lambda *   Y                                    [15]
!                            ---
!                            B**2
!
!         (Pz-Z) = lambda *   Z                                    [16]
!                            ---
!                            C**2
!
!          X**2       Y**2       Z**2
!          -----   +  -----  +   -----  - 1 = 0                    [17]
!          A**2       B**2       C**2
!
!     Solve [14], [15], [16] for X, Y, Z, then substitute into [5]
!
!              2                 2                2
!           Px                 Py               Pz
!      -------------   +  -------------  +  ------------  - 1 = 0  [18]
!                  2                  2                 2
!      (A + lambda)       (B + lambda)      (C + lambda)
!           ------             ------            ------
!              A                 B                 C
!
!     We see expression [18] identical to expression [6].
!



!
!     Chapter 3
!
!     Initializations for the main processing loop
!     ================================================================
!     ----------------------------------------------------------------
!
!     The solution as implemented uses the geometric description
!     of the problem.
!
!     Let the game begin!
!
!     First order the axes of the ellipsoid and corresponding
!     component of POSITN by the size of lengths of axes.  Knowing
!     which axes are smallest will simplify our task of computing
!     lambda when the time comes.
!
      AXIS(1)  = A
      AXIS(2)  = B
      AXIS(3)  = C

      CALL VEQU   ( POSITN,    POINT  )

      CALL ORDERD ( AXIS,   3, IORDER )

      CALL REORDD ( IORDER, 3, AXIS   )
      CALL REORDD ( IORDER, 3, POINT  )

!
!     Rescale everything so as to avoid underflows when squaring
!     quantities and copy the original starting point.
!
!     Be sure that this is UNDONE at the end of the routine.
!
      SCALE = 1.0D0/AXIS(1)

      CALL VSCLIP ( SCALE, AXIS   )
      CALL VSCLIP ( SCALE, POINT  )
      CALL VEQU   ( POINT, ORIGNL )

!
!     Save the norm of the scaled input point.
!
      PNORM = VNORM ( POINT )

!
!     The scaled axis lengths must be small enough so they can
!     be squared.
!
      TOOBIG = SQRT ( DPMAX() / MARGIN )

!
!     Note the first axis has length 1.D0, so we don't check it.
!
      DO I = 2, 3

         IF ( AXIS(I) .GT. TOOBIG ) THEN

            CALL SETMSG ( 'Ratio of length of axis #* to length of ' // &
                          'axis #* is *; this value may cause '      // &
                          'numeric overflow.'                        )
            CALL ERRINT ( '*', IORDER(I)                             )
            CALL ERRINT ( '*', IORDER(1)                             )
            CALL ERRDP  ( '*', AXIS(I)                               )
            CALL SIGERR ( 'SPICE(BADAXISLENGTH)'                     )
            CALL CHKOUT ( 'NEARPT'                                   )
            RETURN

         END IF

      END DO

!
!     We also must limit the size of the products
!
!        AXIS(I)*POINT(I), I = 1, 3
!
!     We can safely check these by comparing the products of
!     the square roots of the factors to TOOBIG.
!
      DO I = 1, 3

         PRODCT = SQRT( AXIS(I) ) * SQRT ( ABS(POINT(I)) )

         IF ( PRODCT .GT. TOOBIG ) THEN

            CALL SETMSG ( 'Product of length of scaled axis #* and ' // &
                          'size of corresponding scaled component '  // &
                          'of POSITN is > *; these values '          // &
                          'may cause numeric overflow.'              )
            CALL ERRINT ( '*', IORDER(I)                             )
            CALL ERRDP  ( '*', TOOBIG**2.D0                          )
            CALL SIGERR ( 'SPICE(INPUTSTOOLARGE)'                    )
            CALL CHKOUT ( 'NEARPT'                                   )
            RETURN

         END IF

      END DO

!
!     Compute the squared lengths of the scaled axes.
!
      AXISQR(1) = AXIS(1)*AXIS(1)
      AXISQR(2) = AXIS(2)*AXIS(2)
      AXISQR(3) = AXIS(3)*AXIS(3)

!
!     We will need to "solve" for the NEARPT at least 3 times.
!     SOLUTN is the counter that keeps track of how many times
!     we have actually solved for a near point.  SOLVNG indicates
!     whether we should continue solving for NEARPT. The logic relies
!     on the initial value of SOLVNG as TRUE, maintaining that
!     value unless explicitly changed.
!
      SNGLPT =  4
      SOLUTN =  1
      SOLVNG = .TRUE.

      DO WHILE ( SOLVNG )


!
!       Chapter 4
!
!       Mathematical solution of the lambda equation.
!       ================================================================
!       ----------------------------------------------------------------
!
!
!        Make a stab at solving the mathematical problem of finding the
!        near point.  In other words, solve the lambda equation.
!
!
!        Avoiding Numerical difficulties
!        -------------------------------
!
!        First make a copy of POINT, then to avoid numerical
!        difficulties later on, we will assume that any component of
!        POINT that is not sufficiently different from zero to
!        contribute to an addition to the corresponding component
!        of AXIS, is in fact zero.
!
         CALL VEQU ( POINT, COPY )

         DO I = 1, 3

            IF (      ( (0.5D0*POINT(I) + AXIS(I)) .EQ.  AXIS(I) ) &
                 .OR. ( (0.5D0*POINT(I) - AXIS(I)) .EQ. -AXIS(I) ) ) &
            THEN
               POINT(I) = 0.0D0
            END IF

         END DO

!
!        OK. Next we set up the logical that indicates whether
!        the current point is inside the ellipsoid.
!
         INSIDE = .FALSE.

!
!        Bracketing the root of the lambda equation.
!        -------------------------------------------
!
!        Let (x,y,z) stand for (POINT(1), POINT(2), POINT(3)) and
!        let (a,b,c) stand for (AXIS(1),  AXIS(2),   AXIS(3)).
!
!        The main step in finding the near point is to find the
!        root of the lambda equation:
!
!                 2                     2               2
!                x                     y               z
!       0 = --------------- + ---------------- + ---------------  - 1
!                         2                  2                 2
!           (a + lambda/a)    ( b + lambda/b)    (c + lambda/c)
!
!
!        We let Q(lambda) be the right hand side of this equation.
!        To find the roots of the equation we determine
!        values of lambda that make Q greater than 0 and less than 0.
!        An obvious value to check is lambda = 0.
!
         Q = (POINT(1)/AXIS(1))**2 &
           + (POINT(2)/AXIS(2))**2 &
           + (POINT(3)/AXIS(3))**2 &
           - 1.0D0

!
!        On the first solution pass, we will determine the sign of
!        the altitude of the input POSITN
!
         IF ( SOLUTN .EQ. 1 ) THEN

            IF       ( Q .GE. 0 ) THEN
               SIGN =  1.0D0
            ELSE
               SIGN = -1.0D0
            END IF

         END IF

!
!        OK. Now for the stuff we will have to do on every solution
!        pass.
!
!        Below, LOWER and UPPER are the bounds on our independent
!        variable LAMBDA.  QLOWER and QUPPER are the values of Q
!        evaluated at LOWER and UPPER, respectively.  The root we
!        seek lies in the interval
!
!           [ LOWER, UPPER ]
!
!        At all points in the algorithm, we have, since Q is a
!        decreasing function to the right of the first non-removable
!        singularity,
!
!           QLOWER > 0
!                  -
!
!           QUPPER < 0
!                  -
!
!        We'll use bracketing to ensure that round-off errors don't
!        violate these inequalities.
!
!        The logical flag INSIDE indicates whether the point is
!        strictly inside the interior of the ellipsoid.  Points on the
!        surface are not considered to be inside.
!
         IF ( Q .EQ. 0.0D0 ) THEN
!
!           In this case the point is already on the ellipsoid
!           (pretty lucky eh?)  We simply set our bracketing values,
!           QLOWER and QUPPER, to zero so that that bisection
!           loop won't ever get executed.
!
            QLOWER = 0.0D0
            QUPPER = 0.0D0
            LOWER  = 0.0D0
            UPPER  = 0.0D0
            LAMBDA = 0.0D0

            INSIDE = .FALSE.


         ELSE IF ( Q .GT. 0.0D0 ) THEN
!
!           The input point is outside the ellipsoid (we expect that
!           this is the usual case).  We want to choose our lower
!           bracketing value so that the bracketing values for lambda
!           aren't too far apart.  So we just make sure that the largest
!           term of the expression for Q isn't bigger than 4.
!
            DO I = 1, 3
               TLAMBD(I) = ( 0.5D0*DABS(POINT(I)) - AXIS(I) ) * AXIS(I)
            END DO

            LOWER = MAX ( 0.0D0, TLAMBD(1), TLAMBD(2), TLAMBD(3) )

!
!           Choose the next value of lambda so that the largest term
!           of Q will be no more than 1/4. (?)
!
            UPPER  = 2.0D0 * MAX ( DABS( AXIS(1)*POINT(1) ), &
                                   DABS( AXIS(2)*POINT(2) ), &
                                   DABS( AXIS(3)*POINT(3) ) )
            LAMBDA = UPPER


            INSIDE = .FALSE.

         ELSE
!
!           In this case the point POSITN is inside the ellipsoid.
!
            INSIDE = .TRUE.

!
!           This case is a bit of a nuisance. To solve the lambda
!           equation we have to find upper and lower bounds on
!           lambda such that one makes Q greater than 0, the other
!           makes Q less than 0.  Once the root has been bracketed
!           in this way it is a straightforward problem to find
!           the value of LAMBDA that is closest to the root we
!           seek.  We already know that for LAMBDA = 0, Q is negative.
!           So we only need to find a value of LAMBDA that makes
!           Q positive.  But... the expression for Q has singularities
!           at LAMBDA = -a**2, -b**2, and -c**2.
!
!           These singularities are not necessarily to be avoided.
!           If the numerator of one of the terms for Q is zero, we
!           can simply compute Q ignoring that particular term.  We
!           say that a singularity corresponding to a term whose
!           numerator is zero is a viable singularity.  By being
!           careful in our computation of Q, we can assign LAMBDA to
!           the value of the singularity. A singularity that is not
!           viable is called a true singularity.
!
!           By choosing LAMBDA just slightly greater than the largest
!           true singularity, we can bracket the root we seek.
!
!           First we must decide which singularity is the first true
!           one.
!
            SNGLPT = 4

            DO I = 3, 1, -1

               IF ( POINT(I) .NE. 0 ) THEN
                  SNGLPT = I
               END IF

            END DO

!
!           If there is a singular point, compute LAMBDA so that the
!           largest term of Q is equal to 4.
!
            IF ( SNGLPT .LE. 3 ) THEN

               DO I = 1, 3

                  IF ( POINT(I) .EQ. 0 ) THEN
                     TLAMBD(I) = -AXISQR(3)
                  ELSE
                     TLAMBD(I) = AXIS(I) * (   0.5D0*DABS(POINT(I)) &
                                             - AXIS(I)              )
                  END IF

               END DO

               LAMBDA = MAX ( TLAMBD(1), TLAMBD(2), TLAMBD(3) )
               LOWER  = LAMBDA
               UPPER  = MAX ( LOWER, 0.0D0 )


            ELSE
!
!              The point must be at the origin.  In this case
!              we know where the closest point is. WE DON'T have
!              to compute anything. It's just at the end of the
!              shortest semi-major axis.  However, since we
!              may have done some rounding off, we will make
!              sure that we pick the side of the shortest axis
!              that has the same sign as COPY(1).
!
!              We are going to be a bit sneaky here.  We know
!              where the closest point is so we are going to
!              simply make POINT and COPY equal to that point
!              and set the upper and lower bracketing bounds
!              to zero so that we won't have to deal with any
!              special cases later on.
!
               IF ( COPY(1) .LT. 0 ) THEN
                  POINT(1) = -AXIS(1)
                  COPY(1)  = -AXIS(1)
               ELSE
                  POINT(1) =  AXIS(1)
                  COPY(1)  =  AXIS(1)
               END IF

               COPY(2) = 0.0D0
               COPY(3) = 0.0D0

               UPPER   = 0.0D0
               LOWER   = 0.0D0
               LAMBDA  = 0.0D0
               Q       = 0.0D0

               INSIDE  = .FALSE.

            END IF

         END IF


!
!        OK. Now compute the value of Q at the two bracketing
!        values of LAMBDA.
!
         DO I = 1, 3

            IF ( POINT(I) .EQ. 0 ) THEN

               TERM(I) = 0.0D0

            ELSE
!
!              We have to be a bit careful for points inside the
!              ellipsoid. The denominator of the factor we are going to
!              compute is ( AXIS + LAMBDA/AXIS ). Numerically this may
!              be too close to zero for us to actually divide POINT by
!              it.  However, since our solution algorithm for lambda
!              does not depend upon the differentiability of Q---in
!              fact it depends only on Q having the correct sign---we
!              can simply truncate its individual terms when we are in
!              danger of division overflows.

               DENOM = AXIS(I) + LAMBDA/AXIS(I)

               TRIM  = 0.5D0 * DABS( POINT(I) )   .GT.   DENOM

               IF ( INSIDE .AND. TRIM ) THEN

                 FACTOR = 2.0D0

               ELSE
!
!                 We don't expect DENOM to be zero here, but we'll
!                 check anyway.
!
                  IF ( DENOM .EQ. 0.D0 ) THEN

                     CALL SETMSG ( 'AXIS(#) + LAMBDA/AXIS(#) is zero.' )
                     CALL ERRINT ( '#',  I                             )
                     CALL ERRINT ( '#',  I                             )
                     CALL SIGERR ( 'SPICE(BUG)'                        )
                     CALL CHKOUT ( 'NEARPT'                            )
                     RETURN

                  END IF

                  FACTOR =  POINT(I) / DENOM

               END IF

               TERM(I) = FACTOR*FACTOR

            END IF

         END DO


         IF ( .NOT. INSIDE ) THEN
            QLOWER = Q
            QUPPER = TERM(1) + TERM(2) + TERM(3) - 1.0D0
         ELSE
            QUPPER = Q
            QLOWER = TERM(1) + TERM(2) + TERM(3) - 1.0D0
         END IF

!
!        Bracket QLOWER and QUPPER.
!
         QLOWER = MAX ( 0.D0,  QLOWER )
         QUPPER = MIN ( 0.D0,  QUPPER )


         LAMBDA = UPPER
         Q      = QUPPER

!
!        Refining the estimate of lambda
!        -------------------------------
!
!        Now find the root of Q by bisection.
!
         ITR = 0
!
!        Throughout this loop we'll use TOUCHD to avoid logic problems
!        that may be caused by extended precision register usage by
!        some compilers.
!
         DO WHILE ( TOUCHD(UPPER-LOWER) .GT. 0.D0 )

            ITR = ITR + 1

            IF ( ITR .GT. MAXITR ) THEN

               CALL SETMSG ('Iteration limit # exceeded in NEARPT. ' // &
                            'A, B, C = # # #; POSITN = ( #, #, # ). '// &
                            'LOWER = #; UPPER = #; UPPER-LOWER = #. '// &
                            'Solution pass number = #.  This event ' // &
                            'should never occur. Contact NAIF.'      )
               CALL ERRINT ( '#',  MAXITR                            )
               CALL ERRDP  ( '#',  A                                 )
               CALL ERRDP  ( '#',  B                                 )
               CALL ERRDP  ( '#',  C                                 )
               CALL ERRDP  ( '#',  POSITN(1)                         )
               CALL ERRDP  ( '#',  POSITN(2)                         )
               CALL ERRDP  ( '#',  POSITN(3)                         )
               CALL ERRDP  ( '#',  LOWER                             )
               CALL ERRDP  ( '#',  UPPER                             )
               CALL ERRDP  ( '#',  UPPER - LOWER                     )
               CALL SIGERR ( 'SPICE(BUG)'                            )
               CALL CHKOUT ( 'NEARPT'                                )
               RETURN

            END IF

!
!           Bracket LOWER, QLOWER, and QUPPER.
!
            LOWER  = MIN ( LOWER, UPPER  )
            QLOWER = MAX ( 0.D0,  QLOWER )
            QUPPER = MIN ( 0.D0,  QUPPER )

!
!           Depending upon how Q compares with Q at the
!           bracketing endpoints we adjust the endpoints
!           of the bracketing interval
!
            IF ( Q .EQ. 0 ) THEN
!
!              We've found the root.
!
               LOWER = LAMBDA
               UPPER = LAMBDA

            ELSE

               IF ( Q .LT. 0.D0 ) THEN

                  UPPER  = LAMBDA
                  QUPPER = Q

               ELSE
!
!                 We have Q > 0
!
                  LOWER  = LAMBDA
                  QLOWER = Q

               END IF
!
!              Update LAMBDA.
!
               LAMBDA = 0.5D0*LOWER + 0.5D0*UPPER

!
!              It's quite possible as we get close to the root for Q
!              that round off errors in the computation of the next
!              value of LAMBDA will push it outside of the current
!              bracketing interval.  Force it back in to the current
!              interval.
!
               LAMBDA = BRCKTD ( LAMBDA, LOWER, UPPER )

            END IF
!
!           At this point, it's guaranteed that
!
!              LOWER  <  LAMBDA  <  UPPER
!                     -          -
!
!           If we didn't find a root, we've set LAMBDA to the midpoint
!           of the previous values of LOWER and UPPER, and we've moved
!           either LOWER or UPPER to the old value of LAMBDA, thereby
!           halving the distance between LOWER and UPPER.
!
!           If we are still at an endpoint, we might as well cash in
!           our chips.  We aren't going to be able to get away from the
!           endpoints.  Set LOWER and UPPER equal so that the loop will
!           finally terminate.
!
            IF (      APPROX( LAMBDA, LOWER, CNVTOL ) &
                 .OR. APPROX( LAMBDA, UPPER, CNVTOL )  ) THEN
!
!              Make the decision as to which way to push
!              the boundaries, by selecting that endpoint
!              at which Q is closest to zero.

               IF ( DABS(QLOWER) .LT. DABS(QUPPER) ) THEN
                  UPPER = LOWER
               ELSE
                  LOWER = UPPER
               END IF
!
!              Since LOWER is equal to UPPER, the loop will terminate.
!
            END IF

!
!           If LOWER and UPPER aren't the same, we compute the
!           value of Q at our new guess for LAMBDA.
!
            IF ( TOUCHD(UPPER-LOWER) .GT. 0 ) THEN

               DO I = 1, 3

                  IF ( POINT(I) .EQ. 0 ) THEN

                     TERM(I) = 0.0D0

                  ELSE

                     DENOM =  AXIS(I) + LAMBDA/AXIS(I)

                     TRIM  =  0.5D0 * DABS( POINT(I) )   .GT.   DENOM

                     IF ( INSIDE .AND. TRIM ) THEN

                        FACTOR = 2.0D0

                     ELSE
!
!                       We don't expect DENOM to be zero here, but we'll
!                       check anyway.
!
                        IF ( DENOM .EQ. 0.D0 ) THEN

                           CALL SETMSG ( 'AXIS(#) + LAMBDA/AXIS(#) ' // &
                                         'is zero.'                  )
                           CALL ERRINT ( '#',  I                     )
                           CALL ERRINT ( '#',  I                     )
                           CALL SIGERR ( 'SPICE(BUG)'                )
                           CALL CHKOUT ( 'NEARPT'                    )
                           RETURN

                        END IF

                        FACTOR =  POINT(I) / DENOM

                     END IF

                     TERM(I) = FACTOR*FACTOR

                  END IF

               END DO

               Q  =  TOUCHD( TERM(1) + TERM(2) + TERM(3) - 1.0D0 )

            END IF
!
!           Q(LAMBDA) has been set unless we've already found
!           a solution.
!
!           Loop back through the bracketing refinement code.
!
        END DO

!
!       Now we have LAMBDA, compute the nearest point based upon
!       this value.
!
        LAMBDA = LOWER


        DO I = 1, 3

           IF ( POINT(I) .EQ. 0.0D0 ) THEN

              SPOINT(I) = 0.0D0

           ELSE

              DENOM = 1.0D0 +  LAMBDA/AXISQR(I)
!
!             We don't expect that DENOM will be non-positive, but we
!             check for this case anyway.
!
              IF ( DENOM .LE. 0.D0 ) THEN

                 CALL SETMSG ( 'Denominator in expression for ' // &
                               'SPOINT(#) is #.'                )
                 CALL ERRINT ( '#', I                           )
                 CALL ERRDP  ( '#', DENOM                       )
                 CALL SIGERR ( 'SPICE(BUG)'                     )
                 CALL CHKOUT ( 'NEARPT'                         )
                 RETURN

              END IF

              SPOINT(I) = COPY(I) / DENOM

           END IF

        END DO

!
!       Handling points on the central plane.
!       -------------------------------------
!
!       I suppose you thought you were done at this point. Not
!       necessarily.  If POINT is INSIDE the ellipsoid and happens to
!       lie in the Y-Z plane, there is a possibility (perhaps even
!       likelihood) that the nearest point on the ellipsoid is NOT in
!       the Y-Z plane. we must consider this possibility next.
!
        IF ( INSIDE  .AND. (      (SNGLPT .EQ. 2) &
                             .OR. (SNGLPT .EQ. 3) ) ) THEN
!
!          There are two ways to get here. SNGLPT = 2 or SNGLPT = 3.
!          Fortunately these two cases can be handled simultaneously by
!          code. However, they are most easily understood if explained
!          separately.
!
!          Case 1.  SNGLPT = 2
!
!          The input to the lambda solution POINT lies in the Y-Z plane.
!          We have already detected one critical point of the
!          distance function to POINT restricted to the ellipsoid.
!          This point also lies in the Y-Z plane.  However, when
!          POINT lies on the Y-Z plane close to the center of the
!          ellipsoid, there may be a point that is nearest that does
!          not lie in the Y-Z plane. Assuming the existence of such a
!          point, (x,y,z) it must satisfy the equations
!
!                   lambda*x
!             x  +  --------  = POINT(1)  = 0
!                      a*a
!
!
!                    lambda*y
!             y  +  --------  = POINT(2)
!                      b*b
!
!
!                   lambda*z
!             z  +  --------  = POINT(3)
!                      c*c
!
!
!          Since we are assuming that this undetected solution (x,y,z)
!          does not have x equal to 0, it must be the case that
!
!          lambda = -a*a.
!
!          Because of this, we must have
!
!             y    =  POINT(2) / ( 1 - (a**2/b**2) )
!             z    =  POINT(3) / ( 1 - (a**2/c**2) )
!
!          The value of x is obtained by forcing
!
!             (x/a)**2 + (y/b)**2 + (z/c)**2 = 1.
!
!          This assumes of course that a and b are not equal. If a and
!          b are the same, then since POINT(2) is not zero, the
!          solution we found above by deflating the original ellipsoid
!          will find the near point.
!
!             (If a and b are equal, the ellipsoid deflates to a
!              segment on the z-axis when lambda = -a**2. Since
!              POINT(2) is not zero, the deflating ellipsoid must pass
!              through POINT before it collapses to a segment.)
!
!
!          Case 2.  SNGLPT = 3
!
!          The input to the lambda solution POINT lies on the Z-axis.
!          The solution obtained in the generic case above will
!          locate the critical point of the distance function
!          that lies on the Z.  However, there will also be
!          critical points in the X-Z plane and Y-Z plane.  The point
!          in the X-Z plane is the one to examine.  Why?  We are looking
!          for the point on the ellipsoid closest to POINT.  It must
!          lie in one of these two planes. But the ellipse of
!          intersection with the X-Z plane fits inside the ellipse
!          of intersection with the Y-Z plane.  Therefore the closest
!          point on the Y-Z ellipse must be at a greater distance than
!          the closest point on the X-Z ellipse.  Thus, in solving
!          the equations
!
!
!                   lambda*x
!             x  +  --------  = POINT(1)  = 0
!                      a*a
!
!
!                    lambda*y
!             y  +  --------  = POINT(2)  = 0
!                      b*b
!
!
!                   lambda*z
!             z  +  --------  = POINT(3)
!                      c*c
!
!
!          We have
!
!             lambda = -a*a,
!
!             y      =  0,
!
!             z      =  POINT(3) / ( 1 - (a**2/c**2) )
!
!          The value of x is obtained by forcing
!
!             (x/a)**2 + (y/b)**2 + (z/c)**2 = 1.
!
!          This assumes that a and c are not equal.  If
!          a and c are the same, then the solution we found above
!          by deflating the original ellipsoid, will find the
!          near point.
!
!             ( If a = c, then the input ellipsoid is a sphere.
!               The ellipsoid will deflate to the center of the
!               sphere.  Since our point is NOT at the center,
!               the deflating sphere will cross through
!               (x,y,z) before it collapses to a point )
!
!          We begin by assuming this extra point doesn't exist.
!
           EXTRA = .FALSE.

!
!          Next let's note a few simple tests we can apply to
!          eliminate searching for an extra point.
!
!          First of all the smallest axis must be different from
!          the axis associated with the first true singularity.
!
!
!          Next, whatever point we find, it must be true that
!
!               |y| < b, |z| < c
!
!          because of the condition on the absolute values, we must
!          have:
!
!               | POINT(2) | <= b - a*(a/b)
!
!               | POINT(3) | <= c - a*(a/c)
!

           IF (     (     AXIS (1)  .NE. AXIS(SNGLPT)                ) &
               .AND.(DABS(POINT(2)) .LE. AXIS(2) - AXISQR(1)/AXIS(2) ) &
               .AND.(DABS(POINT(3)) .LE. AXIS(3) - AXISQR(1)/AXIS(3) )) &
           THEN
!
!             What happens next depends on whether the ellipsoid is
!             prolate or triaxial.
!
              IF ( AXIS(1) .EQ. AXIS(2) ) THEN
!
!                This is the prolate case; we need to compute the
!                z component of the solution.
!
                 DENOM3 = 1.0D0 - AXISQR(1)/AXISQR(3)

                 IF ( DENOM3 .GT. 0.D0 ) THEN

                    EPOINT(2) = 0.D0
!
!                   Concerning the safety of the following division:
!
!                      - A divide-by-zero check has been done above.
!
!                      - The numerator can be squared without exceeding
!                        DPMAX(). This was checked near the start of the
!                        routine.
!
!                      - The denominator was computed as the difference
!                        of 1.D0 and another number. The smallest
!                        possible magnitude of a non-zero value of the
!                        denominator is roughly 1.D-16, assuming IEEE
!                        double precision numeric representation.
!
!
                    EPOINT(3) = POINT(3) / DENOM3

!
!                   See if these components can even be on the
!                   ellipsoid...
!
                    TEMP      = 1.0D0  - ( EPOINT(2) / AXIS(2) )**2 &
                                       - ( EPOINT(3) / AXIS(3) )**2

                    IF ( TEMP .GT. 0 ) THEN
!
!                      ...and compute the x component of the point.
!
                       EPOINT(1) = AXIS(1) * DSQRT( MAX(0.D0,TEMP) )
                       EXTRA     = .TRUE.

                    END IF

                 END IF

              ELSE
!
!                This is the triaxial case.
!
!                Compute the y and z components (2 and 3) of the extra
!                point.
!
                 DENOM2 = 1.0D0 - (AXISQR(1)/AXISQR(2))
                 DENOM3 = 1.0D0 - (AXISQR(1)/AXISQR(3))

!
!                We expect DENOM2 and DENOM3 will always be positive.
!                Nonetheless, we check to make sure this is the case.
!                If not, we don't compute the extra point.
!
                 IF (       ( DENOM2 .GT. 0.D0 ) &
                      .AND. ( DENOM3 .GT. 0.D0 ) ) THEN

!
!                   Concerning the safety of the following divisions:
!
!                      - Divide-by-zero checks have been done above.
!
!                      - The numerators can be squared without exceeding
!                        DPMAX(). This was checked near the start of the
!                        routine.
!
!                      - Each denominator was computed as the difference
!                        of 1.D0 and another number. The smallest
!                        possible magnitude of a non-zero value of
!                        either denominator is roughly 1.D-16, assuming
!                        IEEE double precision numeric representation.
!

                    EPOINT(2) = POINT(2) / DENOM2
                    EPOINT(3) = POINT(3) / DENOM3
!
!                   See if these components can even be on the
!                   ellipsoid...
!
                    TEMP      = 1.0D0  - ( EPOINT(2) / AXIS(2) )**2 &
                                       - ( EPOINT(3) / AXIS(3) )**2

                    IF ( TEMP .GT. 0 ) THEN
!
!                      ...and compute the x component of the point.
!
                       EPOINT(1) = AXIS(1) * DSQRT(TEMP)
                       EXTRA     = .TRUE.

                    END IF

                 END IF

              END IF

           END IF

!
!          Ok.  If an extra point is possible, check and see if it
!          is the one we are searching for.
!
           IF ( EXTRA ) THEN

              IF (      VDIST(EPOINT, POINT) &
                   .LT. VDIST(SPOINT, POINT) ) THEN

                 CALL VEQU ( EPOINT, SPOINT )

              END IF

           END IF

        END IF


!
!       Chapter 5
!
!       Decisions and initializations for sharpening the solution.
!       ================================================================
!       ----------------------------------------------------------------
!
        IF ( SOLUTN .EQ. 1 ) THEN

!
!           The first solution for the nearest point may not be
!           very close to being on the ellipsoid.  To
!           take care of this case, we next find the point on the
!           ellipsoid, closest to our first solution point.
!           (Ideally the normal line at this second point should
!           contain both the current solution point and the
!           original point).
!
            CALL   VEQU ( SPOINT, POINT  )
            CALL   VEQU ( SPOINT, BESTPT )

            BESTHT = VDIST ( BESTPT, ORIGNL )


         ELSE IF ( SOLUTN .EQ. 2 ) THEN

!
!           The current solution point will be very close to lying
!           on the ellipsoid.  However, the normal at this solution
!           may not actually point toward the input point.
!
!           With the current solution we can predict
!           the location of the input point.  The difference between
!           this predicted point and the actual point can be used
!           to sharpen our estimate of the solution.
!
!           The sharpening is performed by
!
!              1) Compute the vector ERR that gives the difference
!                 between the input point (POSITN) and the point
!                 computed using the solution point, normal and
!                 altitude.
!
!              2) Find the component of ERR that is orthogonal to the
!                 normal at the current solution point. If the point
!                 is outside the ellipsoid, scale this component to
!                 the approximate scale of the near point.  We use
!                 the scale factor
!
!                     ||near point|| / ||input point||
!
!                  Call this scaled component ERRP.
!
!              3) Translate the solution point by ERRP to get POINT.
!
!              4) Find the point on the ellipsoid closest to POINT.
!                 (step 4 is handled by the solution loop above).
!
!
!           First we need to compute the altitude
!
            HEIGHT  = SIGN * VDIST( SPOINT, ORIGNL )

!
!           Next compute the difference between the input point and
!           the one we get by moving out along the normal at our
!           solution point by the computed altitude.
!
            CALL SURFNM ( AXIS(1), AXIS(2), AXIS(3), SPOINT, NORMAL )

            DO I=1, 3

               ERR(I) = ORIGNL(I) - (SPOINT(I) + HEIGHT*NORMAL(I))

            END DO


!
!           Find the component of the error vector that is
!           perpendicular to the normal, and shift our solution
!           point by this component.
!
            CALL VPERP ( ERR, NORMAL, ERRP )

!
!           Check for a zero projection. If so, set iteration flag to
!           prevent an iteration pass after the current finishes.
!
            IF ( VZERO(ERRP) ) THEN
               SOLVNG = .FALSE.
            END IF


!
!           The sign of the original point's altitude tells
!           us whether the point is outside the ellipsoid.
!
            IF ( SIGN .GE. 0.D0 ) THEN
!
!              Scale the transverse component down to the local radius
!              of the surface point.
!
               IF ( PNORM .EQ. 0.D0 ) THEN
!
!                 Since the point is outside of the scaled ellipsoid,
!                 we don't expect to arrive here. This is a backstop
!                 check.
!
                  CALL SETMSG ( 'Norm of scaled point is 0. ' // &
                                'POSITN = ( #, #, # )'        )
                  CALL ERRDP  ( '#',  POSITN(1)               )
                  CALL ERRDP  ( '#',  POSITN(2)               )
                  CALL ERRDP  ( '#',  POSITN(3)               )
                  CALL SIGERR ( 'SPICE(BUG)'                  )
                  CALL CHKOUT ( 'NEARPT'                      )
                  RETURN

               END IF

               IF ( SOLVNG ) THEN
                  CALL VSCLIP ( VNORM(SPOINT)/PNORM, ERRP )
               END IF

            END IF

            CALL VADD ( SPOINT, ERRP, POINT )

            IF (  (SPOINT(1) .EQ. POINT(1)) .AND. &
                  (SPOINT(2) .EQ. POINT(2)) .AND. &
                  (SPOINT(3) .EQ. POINT(3))       ) THEN
               SOLVNG = .FALSE.
            END IF


            OLDERR = VNORM ( ERR )
            BESTHT = HEIGHT

!
!           Finally store the current solution point, so that if
!           this sharpening doesn't improve our estimate of the
!           near point, we can just return our current best guess.
!
            CALL VEQU ( SPOINT, BESTPT )


         ELSE IF ( SOLUTN .GT. 2 ) THEN

!
!           This branch exists for the purpose of testing our
!           "sharpened" solution and setting up for another sharpening
!           pass.
!
!           We have already stored our best guess so far in BESTPT and
!           the vector ERR is the difference
!
!              ORIGNL - ( BESTPT + BESTHT*NORMAL )
!
!           We have just computed a new candidate "best" near point.
!           SPOINT.
!
!           If the error vector
!
!              ORIGNL - ( SPOINT + HEIGHT*NORMAL)
!
!           is shorter than our previous error, we will make SPOINT
!           our best guess and try to sharpen our estimate again.
!
!           If our sharpening method hasn't improved things, we just
!           call it quits and go with our current best guess.
!

!
!           First compute the altitude,
!
            HEIGHT  = SIGN * VDIST( SPOINT, ORIGNL )

!
!           ... compute the difference
!
!              ORIGNL - SPOINT - HEIGHT*NORMAL,
!
            CALL SURFNM ( AXIS(1), AXIS(2), AXIS(3), SPOINT, NORMAL )

            DO I = 1, 3

               ERR(I) = ORIGNL(I) - (SPOINT(I) + HEIGHT*NORMAL(I))

            END DO

!
!           Check for a zero difference. If so, set iteration flag to
!           prevent an iteration pass after the current finishes.
!
            IF ( VZERO(ERR) ) THEN
               SOLVNG = .FALSE.
               NEWERR = 0.D0
            ELSE

!              Determine the magnitude of the error due to our
!              sharpened estimate, if error non zero.
!
               NEWERR = VNORM ( ERR )
            END IF

!
!           If the sharpened estimate yields a smaller error ...
!
            IF ( NEWERR .LT. OLDERR ) THEN
!
!              ...our current value of SPOINT becomes our new
!              best point and the current altitude becomes our
!              new altitude.
!
               OLDERR  =  NEWERR
               BESTHT  =  HEIGHT

               CALL  VEQU ( SPOINT,  BESTPT )

!
!              Next, if we haven't passed the limit on the number of
!              iterations allowed we prepare the initial point for our
!              "sharpening" estimate.
!
               IF ( SOLUTN .LE. MAXSOL ) THEN


!                 Find the component of the error vector that is
!                 perpendicular to the normal, and shift our solution
!                 point by this component.

                  CALL VPERP ( ERR, NORMAL, ERRP )

!
!                 Check for a zero projection. If so, set iteration
!                 flag to prevent an iteration pass after the current
!                 finishes.
!
                  IF ( VZERO(ERRP) ) THEN
                     SOLVNG = .FALSE.
                  END IF

!
!                 The sign of the original point's altitude tells
!                 us whether the point is outside the ellipsoid.
!
                  IF ( SIGN .GE. 0.D0 ) THEN
!
!                    Scale the transverse component down to the local
!                    radius of the surface point.
!
                     IF ( PNORM .EQ. 0.D0 ) THEN
!
!                       Since the point is outside of the scaled
!                       ellipsoid, we don't expect to arrive here.
!                       This is a backstop check.
!
                        CALL SETMSG ( 'Norm of scaled point is 0. ' // &
                                      'POSITN = ( #, #, # )'        )
                        CALL ERRDP  ( '#',  POSITN(1)               )
                        CALL ERRDP  ( '#',  POSITN(2)               )
                        CALL ERRDP  ( '#',  POSITN(3)               )
                        CALL SIGERR ( 'SPICE(BUG)'                  )
                        CALL CHKOUT ( 'NEARPT'                      )
                        RETURN

                     END IF

                     IF ( SOLVNG ) THEN
                        CALL VSCLIP ( VNORM(SPOINT)/PNORM, ERRP )
                     END IF

                  END IF

                  CALL VADD  ( SPOINT, ERRP, POINT )

                  IF (  (SPOINT(1) .EQ. POINT(1)) .AND. &
                        (SPOINT(2) .EQ. POINT(2)) .AND. &
                        (SPOINT(3) .EQ. POINT(3))       ) THEN
                     SOLVNG = .FALSE.
                  END IF

               END IF

            ELSE
!
!              If things didn't get better, there is no point in
!              going on.  Just set the SOLVNG flag to .FALSE. to
!              terminate the outer loop.
!
               SOLVNG = .FALSE.

            END IF

         END IF

!
!        Increment the solution counter so that eventually this
!        loop will terminate.
!
         SOLUTN = SOLUTN + 1

         SOLVNG = SOLVNG .AND. ( SOLUTN .LE. MAXSOL )

      END DO

!
!     Chapter 6
!
!     Clean up.
!     ==================================================================
!     ------------------------------------------------------------------
!
!     Re-scale and re-order the components of our solution point. Scale
!     and copy the value of BESTHT into the output argument.
!
      CALL VSCLIP( 1.0D0/SCALE, BESTPT )

      DO I = 1,3
         NPOINT( IORDER(I) ) = BESTPT(I)
      END DO

      ALT = BESTHT/SCALE

      CALL CHKOUT ( 'NEARPT' )

      RETURN
      END
