!$Procedure KPSOLV ( Solve Kepler's Equation --- Vector Form )

      DOUBLE PRECISION FUNCTION KPSOLV ( EVEC )

! $ Abstract
!
!     Solve the equation X = < EVEC, U(X) > where U(X) is the unit
!     vector [ COS(X), SIN(X) ] and  < , > denotes the two-dimensional
!     dot product.
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
!     ROOTS
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      EVEC ( 2 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     EVEC       I   A 2-vector whose magnitude is less than 1.
!
!     The function returns the solution to X = < EVEC, U(X) >
!
! $ Detailed_Input
!
!     EVEC     is any two dimensional vector whose magnitude is
!              less than 1.
!
! $ Detailed_Output
!
!     The function returns the value X such that the equation
!
!        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the magnitude of EVEC is greater than or equal to 1,
!         the error SPICE(EVECOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses bisection and Newton's method to find
!     the root of the equation
!
!        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
!
!     This equation is just a "vector form" of Kepler's equation.
!
! $ Examples
!
!     Suppose you need to solve the equation
!
!         M = E - e SIN(E)                           [ 1 ]
!
!     for E. If we let X = E - M the equation is transformed to
!
!        0 = X - e SIN( X + M )
!
!          = X - e SIN( M ) COS(X) - e COS(M) SIN ( X )
!
!     Thus if we solve the equation
!
!        X = e SIN(M) COS(X) + e COS(M) SIN(X)
!
!     we can find the value of X we can compute E.
!
!     The code fragment below illustrates how this routine can
!     be used to solve equation [1].
!
!         EVEC(1) = ECC * DSIN(M)
!         EVEC(2) = ECC * DCOS(M)
!         E       = M   + KPSOLV( EVEC )
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT)
!
!        KPSOLV is now given an initial value of zero so that
!        if an error condition is detected, KPSOLV will have
!        a return value.
!
!-    SPICELIB Version 1.0.0, 03-JAN-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     Solve the vector form of the Kepler equation
!
!-&


!
!     MXNEWT is the number of iterations we will perform
!     in the Newtons method for finding the solution to
!     the vector form of Kepler's equation.  It has been
!     empirically determined that 5 iterations is always
!     sufficient on computers have 64 bit double precision
!     numbers.
!
      INTEGER               MXNEWT
      PARAMETER           ( MXNEWT = 5 )


      DOUBLE PRECISION      COSX
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      ECC2
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      SINX
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XL
      DOUBLE PRECISION      XM
      DOUBLE PRECISION      XU
      DOUBLE PRECISION      Y0
      DOUBLE PRECISION      YPX
      DOUBLE PRECISION      YX
      DOUBLE PRECISION      YXM

      INTEGER               I
      INTEGER               MAXIT

!
!     We give the function an initial value, just in case
!     we exit without solving Kepler's equation.
!
      KPSOLV = 0.0D0

      H      = EVEC(1)
      K      = EVEC(2)

      ECC2   = H*H + K*K

      IF ( ECC2 .GE. 1.0D0 ) THEN

         CALL CHKIN  ( 'KPSOLV' )
         CALL SETMSG ( 'The magnitude of the vector EVEC = ( #, ' &
         //            '# ) must be less than 1.  However, the ' &
         //            'magnitude of this vector is #.' )

         CALL ERRDP  ( '#', H           )
         CALL ERRDP  ( '#', K           )
         CALL ERRDP  ( '#', DSQRT(ECC2) )

         CALL SIGERR ( 'SPICE(EVECOUTOFRANGE)'  )
         CALL CHKOUT ( 'KPSOLV' )
         RETURN

      END IF

!
!     We first approximate the equation 0 = X - H * COS(X) - K * SIN(X)
!     using bisection.  If we let Y(X) = X - H * COS(X) - K * SIN(X)
!
!        Y( ECC) =  ECC - <EVEC,U(X)>  =   ECC - ECC*COS(ANGLE_X) > 0
!        Y(-ECC) = -ECC - <EVEC,U(X)>  =  -ECC - ECC*COS(ANGLE_X) < 0
!
!     where ANGLE_X is the angle between U(X) and EVEC. Thus -ECC
!     and ECC necessarily bracket the root of the equation Y(X) = 0.
!
!     Also note that Y'(X) = 1 - < EVEC, V(X) > where V(X) is the
!     unit vector given by U'(X).  Thus Y is an increasing function
!     over the interval from -ECC to ECC.
!
!     The mid point of ECC and -ECC is 0 and Y(0) = -H.  Thus
!     we can do the first bisection step without doing
!     much in the way of computations.
!

      Y0  = -H
      XM  =  0.0D0
      ECC =  DSQRT(ECC2)

      IF      ( Y0 .GT. 0.0D0 ) THEN

         XU  =  0.0D0
         XL  = -ECC

      ELSE IF ( Y0 .LT. 0.0D0 ) THEN

         XU =  ECC
         XL =  0.0D0

      ELSE

         KPSOLV = 0.0D0
         RETURN

      END IF
!
!     Iterate until we are assured of being in a region where
!     Newton's method will converge quickly.  The formula
!     below was empirically determined to give good results.
!
      MAXIT = MIN(32, MAX( 1, NINT(1.0D0/(1.0D0-ECC)) ) )

      DO I = 1, MAXIT

!
!        Compute the next midpoint.  We bracket XM by XL and XU just in
!        case some kind of strange rounding occurs in the computation
!        of the midpoint.
!
         XM     = MAX( XL, MIN( XU, 0.5D0*(XL + XU) ) )

!
!        Compute Y at the midpoint of XU and XL
!
         YXM = XM - H*COS(XM) - K*SIN(XM)
!
!        Determine the new upper and lower bounds.
!
         IF ( YXM .GT. 0.0D0 ) THEN
            XU = XM
         ELSE
            XL = XM
         END IF


      END DO

!
!     We've bisected into a region where we can now get rapid
!     convergence using Newton's method.
!
      X   = XM

      DO I = 1, MXNEWT

         COSX = DCOS(X)
         SINX = DSIN(X)
!
!        Compute Y and Y' at X.  Use these to get the next
!        iteration for X.
!
!        For those of you who might be wondering, "Why not put
!        in a check for YX .EQ. 0 and return early if we get
!        an exact solution?"  Here's why.  An empirical check
!        of those cases where you can actually escape from the
!        Do-loop  showed that the test YX .EQ. 0 is true
!        only about once in every 10000 case of random inputs
!        of EVEC.  Thus on average the check is a waste of
!        time and we don't bother with it.
!
         YX   = X     - H*COSX - K*SINX
         YPX  = 1.0D0 + H*SINX - K*COSX
         X    = X     - YX/YPX

      END DO

      KPSOLV = X

      RETURN
      END
