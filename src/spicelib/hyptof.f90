!$Procedure HYPTOF ( Hyperbolic time of flight )

      SUBROUTINE HYPTOF ( MA, ECC, F )

! $ Abstract
!
!     Solve the time of flight equation MA = e sinh(F) - F for the
!     hyperbolic eccentric anomaly F, given the mean anomaly, MA,
!     and the eccentricity, e.
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
!     CONIC
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      MA
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      F

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MA         I   Mean anomaly at epoch.
!     ECC        I   Eccentricity.
!     F          O   Hyperbolic eccentric anomaly.
!
! $ Detailed_Input
!
!     MA       is the hyperbolic mean anomaly of an orbiting body at
!              some epoch t,
!
!                                   3 1/2
!                 MA = (t-T)(mu/(-a) )
!
!              where T is the time of periapsis passage, a is
!              the semi-major axis of the orbit, and mu is the
!              gravitational parameter of the primary body.
!
!     ECC      is the eccentricity of the orbit.
!
! $ Detailed_Output
!
!     F        is the corresponding eccentric anomaly. This is the
!              solution to the time of flight equation
!
!                 MA = e sinh(F) - F
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity (ECC) is less than one, the error
!         SPICE(WRONGCONIC) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Iterate to solve
!
!        f(F,MA,e) = e sinh(F) - F - MA = 0
!
! $ Examples
!
!     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
!          Astrodynamics," Dover Publications Inc., 1971.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.2.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.1.0, 13-JUL-2007 (NJB)
!
!        Bug fix: MAXLOG is now saved.
!
!-    SPICELIB Version 3.0.0, 14-DEC-1994 (WLT)
!
!        A counter was placed in the loop which bisects to a
!        solution to the hyperbolic version of Kepler's equation.
!        This addition forces the loop to terminate. On some platforms
!        the loop would not terminate without this additional
!        check. This was due to the compiler performing tests on
!        extended precision registers.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 19-APR-1990 (WLT)
!
!        A bad initial guess at bracketing the solution to the
!        hyperbolic time of flight equation was corrected so that
!        floating point overflows are now avoided. In addition, the
!        Newton's method used before has been replaced by simply
!        bisection.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     hyperbolic time of flight
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
!
!        The routine now verifies that the eccentricity is in the
!        proper range---(1,+infinity)---before proceeding.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DCBRT

      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               COUNT
      INTEGER               MCOUNT

      DOUBLE PRECISION      MAXLOG

      DOUBLE PRECISION      M

      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      MIDDLE
      DOUBLE PRECISION      MIDVAL
      DOUBLE PRECISION      UPPER

      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      LASTDF

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  MAXLOG

!
!     Initial values
!
      DATA                  FIRST   / .TRUE. /



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'HYPTOF' )
      END IF

      IF ( FIRST ) THEN
         FIRST  = .FALSE.
         MAXLOG = LOG ( DPMAX() )
      END IF

      IF ( ECC .LT. 1.D0 ) THEN
         CALL SIGERR ( 'SPICE(WRONGCONIC)' )
         CALL CHKOUT ( 'HYPTOF'            )
         RETURN
      END IF

!
!     For reasons of numerical stability, we have to intercept cases
!     where the mean anomaly is zero or negative (since log x is not
!     defined for non-positive x). If the mean anomaly is zero, the
!     eccentric anomaly is also zero (by inspection).
!
!     Since the function e sinh(F) - F is an odd function, we can
!     solve the equation ABS(MA) = e sinh(F) - F for F and get
!     the solution to MA = e sinh(F) - F by negating F if MA is
!     less than 0.
!
      IF ( MA .EQ. 0.D0 ) THEN
         F = 0.D0
         CALL CHKOUT ( 'HYPTOF' )
         RETURN
      ELSE
         M = ABS ( MA )
      END IF


!
!     The initial bounds for the eccentric anomaly F are determined
!     as follows:
!
!     For the value of F we seek,
!
!        M = e sinh F - F
!
!     Thus
!
!        M < e sinh F =    (e/2) { Exp(F) - Exp(-F)}
!
!     Hence
!
!        2 M                1
!        ---   < Exp(F) - -----
!         e               Exp(F)
!
!     which yields
!
!
!        2 M Exp(F)
!        ----------   < Exp(F)**2 - 1
!            e
!
!     and
!
!           M**2                2M Exp(F)     M**2
!       1 + ---- <  Exp(F)**2 - ---------  +  ---- = {Exp(F) - (M/e)}**2
!           e**2                   e          e**2
!
!
!     Therefore we must have one of the following be true.
!
!
!      SQRT( 1 + (M/e)**2 )  <  Exp(F) - (M/e)
!
!    or
!
!     - SQRT( 1 + (M/e)**2 )  >  Exp(F) - (M/e)
!
!    The second case implies that
!
!     0 > (M/e) - SQRT( 1 + (M/e)**2 ) > Exp(F)
!
!    but since Exp(F) > 0 for all F it must be the case that
!
!      (M/e) + SQRT( 1 + (M/e)**2 ) < Exp(F)
!
!
!    Hence
!
!       Log ( (M/e) + SQRT(1 + (M/e)**2) )  < F
!
!
!
!     Returning to our initial equation:
!
!        M = e sinh F - F
!
!                         3        5
!                        F        F
!          =  e ( F  +  ---   +  --- + ...   )  -  F
!                        3!       5!
!
!               3
!          >  eF / 6
!
!     Thus
!
!
!             3 __________
!        F <  \/  6M / e
!
!
!     Thus our solution must satisfy the inequalities
!
!
!                                                      3 __________
!      LOG ( (M/e) + SQRT(1 + (M/e)**2) )  <   F   <   \/  6M/e
!
!
!     In addition we know that the solution must lie somewhere
!     in the region between 0 and the maximum value of F for which
!     (e sinh F - F) can be computed.  This turns out to be
!     approximately LOG( DPMAX() / e ) = LOG(DPMAX()) - LOG(e) .
!
!
      LOWER = LOG ( (M/ECC)  +  DSQRT( 1.0D0 + (M/ECC)**2 ) )
      UPPER = MIN ( DCBRT(6.0D0*M/ECC), MAXLOG - LOG (ECC ) )
      UPPER = MAX ( LOWER, UPPER )
!
!     Perform some simple checks first to avoid problems with
!     convergence of the loop below.  If LOWER is zero, then
!     M/ECC is so small that when added to 1 it doesn't make
!     any difference ( dLOG/dt = 1 at 1 after all).  So in this
!     case we will just solve the linear portion of the
!     expansion of e SINH(F) - F = M
!
!
!     Now we simply perform bisection to locate the root.
!
      MIDDLE = MAX( LOWER, MIN( UPPER, 0.5D0*UPPER + 0.5D0*LOWER ) )
      MIDVAL = ECC * SINH(MIDDLE) - MIDDLE - M

      DIFF   = UPPER - LOWER
!
!     Finally pick a reasonable upper bound on the number of loop
!     iterations we shall need to perform.
!
      MCOUNT = 100
      COUNT  = 0
      DO WHILE ( DIFF .GT. 0.0D0 .AND. COUNT .LT. MCOUNT )

!
!        Move one of the endpoints to the middle.
!
         IF ( MIDVAL .GT. 0.0D0 ) THEN

            UPPER = MIDDLE

         ELSE IF ( MIDVAL .LT. 0.0D0 ) THEN

            LOWER = MIDDLE

         ELSE

            LOWER = MIDDLE
            UPPER = MIDDLE

         END IF

!
!        Compute the next middle point.
!
         MIDDLE = MAX( LOWER, MIN( UPPER, 0.5D0*UPPER + 0.5D0*LOWER ) )
         LASTDF = DIFF
         COUNT  = COUNT + 1

!
!        If we are on an endpoint, we are ready to call it quits.
!
         IF (      ( MIDDLE .EQ. LOWER ) &
              .OR. ( MIDDLE .EQ. UPPER ) ) THEN

            DIFF   = 0.0D0

         ELSE

            DIFF   = UPPER - LOWER
            MIDVAL = ECC * SINH ( MIDDLE ) - MIDDLE - M

         END IF


      END DO

!
!     Restore the proper sign, if necessary.
!
      IF ( MA .LT. 0 ) THEN
         F = - MIDDLE
      ELSE
         F =   MIDDLE
      END IF


      CALL CHKOUT ( 'HYPTOF' )
      RETURN
      END
