!$Procedure      CHBFIT ( Chebyshev fit )

      SUBROUTINE CHBFIT ( FUNC, LEFT, RIGHT, N, WORK, COEFFS )
      IMPLICIT NONE

! $ Abstract
!
!     Return the Chebyshev coefficients for a Chebyshev expansion
!     of a specified function.
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
!     INTERPOLATION
!     MATH
!     POLYNOMIAL
!
! $ Declarations

      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 25 )

      DOUBLE PRECISION      FUNC
      EXTERNAL              FUNC

      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      INTEGER               N
      DOUBLE PRECISION      WORK   ( * )
      DOUBLE PRECISION      COEFFS ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     MAXSIZ     P   Maximum number of terms in expansion.
!     FUNC       I   Function to be approximated.
!     LEFT       I   Left endpoint of approximation interval.
!     RIGHT      I   Right endpoint of approximation interval.
!     N          I   Number of terms in Chebyshev expansion.
!     WORK       I   Work space array of dimension N.
!     COEFFS     O   Coefficients of Chebyshev expansion.
!
! $ Detailed_Input
!
!     FUNC           is the function to be approximated.  FUNC must
!                    accept a single, double precision input argument
!                    and must return a double precision value.  FUNC
!                    should be declared EXTERNAL in the caller of this
!                    routine.
!
!     LEFT,
!     RIGHT          are, respectively, the left and right endpoints
!                    of the interval on which the input function is
!                    to be approximated.
!
!     N              is the number of terms in the desired Chebyshev
!                    expansion.  The degree of the highest-order
!                    Chebyshev polynomial in the expansion is N-1.
!
!     WORK           is a work space array of dimension N.
!
!
! $ Detailed_Output
!
!     COEFFS         is an array containing the coefficients of
!                    the N-term Chebyshev expansion of the input
!                    function.
!
!                    Let
!
!                       T (x)   =  cos ( j arccos(x) )
!                        j
!
!                    be the Chebyshev polynomial of degree j; then
!                    COEFFS are computed such that the expansion
!
!                        N
!                       ___
!                       \    COEFFS(j)  T   (x)
!                       /__              j-1
!
!                       j=1
!
!                    is the Chebyshev expansion of F(Y) on the
!                    interval [-1,1], where
!
!                       F(Y) =  FUNC(X)
!
!                    and
!
!                               X  -  (LEFT+RIGHT)/2
!                       Y    =  ---------------------
!                                 (LEFT-RIGHT) / 2
!
!                    The coefficients computed by this routine are
!                    compatible with the SPICELIB routines CHBINT,
!                    CHBVAL, and CHBDER.
!
!                    See the $Particulars section for further details
!                    on the specification of this routine.
!
! $ Parameters
!
!     MAXSIZ         is the maximum number of terms in the Chebyshev
!                    expansion.  This is the maximum allowed value of
!                    N.
!
! $ Exceptions
!
!     1)  If N is less than 1, the error SPICE(INVALIDSIZE) is
!         signaled.  The function will return the value 0.D0.
!
!     2)  If N is greater than MAXSIZ, the error SPICE(INVALIDSIZE) is
!         signaled.  The function will return the value 0.D0.
!
!     3)  This routine does not attempt to ward off or diagnose
!         arithmetic overflows.
!
!     4)  If the endpoints LEFT and RIGHT are not in strictly
!         increasing order, the error SPICE(INVALIDENDPTS)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The coefficient set produced by this routine is described below:
!
!        Let
!
!           x ,    k = 1, ... , N
!            k
!
!        be the roots of the Chebyshev polynomial
!
!           T (x)   =  cos ( N arccos(x) )
!            N
!
!        These roots are
!
!           cos ( (k-1/2)*PI/N ),    k = 1, ..., N.
!
!
!        For a function f(x) defined on the closed
!        interval [-1,1], the N-term Chebyshev expansion
!        is
!
!            N
!           ___
!           \    C  T   (x)
!           /__   j  j-1
!
!           j=1
!
!        where
!                         N
!                        ___
!           C  =  (2/N)  \   f(x ) T   (x ),  j = 2, ...,N,
!            j           /__    k   j-1  k
!
!                        k=1
!
!                         N
!                        ___
!           C  =  (1/N)  \   f(x )
!            1           /__    k
!
!                        k=1
!
!
!        The definition of
!
!           C
!            1
!
!        used differs from that used in reference [1];
!        our value is half theirs, and yields the simpler
!        expression for the expansion of f(x) shown above.
!
!        When the function f(x) to be approximated is
!        defined on the interval [LEFT,RIGHT], the mapping
!
!                     x  -  (LEFT+RIGHT)/2
!           y(x)  =  ---------------------
!                       (LEFT-RIGHT) / 2
!
!        can be used to define a new function F such that
!        F(y) = f(x).  F has domain [-1,1] and hence admits
!        a Chebyshev expansion.
!
!        In this routine, the above mapping is used to
!        transform the domain of the input function to the
!        interval [-1,1].
!
!
! $ Examples
!
!     1)  Recover coefficients from a function whose Chebyshev
!         expansion is known.  Suppose
!
!            f(x) = 1*T (x) + 2*T (x) + 3*T (x) + 4*T (x).
!                      0         1         2         3
!
!         The following small program produces the Chebyshev
!         coefficients of f:
!
!
!                  PROGRAM TSTCHB
!                  IMPLICIT NONE
!            C
!            C     Test Chebyshev fitting for a simple function.
!            C
!                  INTEGER               NCOEFF
!                  PARAMETER           ( NCOEFF = 4 )
!
!                  DOUBLE PRECISION      FUNC
!                  EXTERNAL              FUNC
!
!                  DOUBLE PRECISION      COEFFS ( NCOEFF )
!                  DOUBLE PRECISION      WORK   ( NCOEFF )
!                  INTEGER               I
!
!
!                  CALL CHBFIT ( FUNC,   -1.D0,  1.D0,
!                 .              NCOEFF,  WORK,  COEFFS )
!
!                  WRITE (*,*) 'Coefficients follow:'
!
!                  DO I = 1, NCOEFF
!                     WRITE (*,*) 'DEGREE: ', I-1, ' = ', COEFFS(I)
!                  END DO
!
!                  END
!
!
!                  DOUBLE PRECISION FUNCTION FUNC ( X )
!                  IMPLICIT NONE
!            C
!            C     Return
!            C
!            C        f(x) = 1*T (x) + 2*T (x) + 3*T (x) + 4*T (x).
!            C                  0         1         2         3
!            C
!                  DOUBLE PRECISION      X
!
!                  INTEGER               NCOEFF
!                  PARAMETER           ( NCOEFF = 4 )
!
!                  DOUBLE PRECISION      CP  ( NCOEFF )
!                  DOUBLE PRECISION      X2S ( 2 )
!                  INTEGER               I
!
!                  DO I = 1, NCOEFF
!                     CP(I) =  DBLE(I)
!                  END DO
!
!                  X2S(1) = 0.D0
!                  X2S(2) = 1.D0
!
!                  CALL CHBVAL ( CP, NCOEFF-1, X2S, X, FUNC )
!                  END
!
! $ Restrictions
!
!     1)  Maximum number of terms in the expansion is limited by the
!         parameter MAXSIZ.
!
! $ Literature_References
!
!     [1]  "Numerical Recipes---The Art of Scientific Computing" by
!           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
!           William T. Vetterling (see section 5.6).
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SUPPORT Version 2.0.0, 14-SEP-2007 (NJB)
!
!        Now pre-computes Chebyvshev polynomial values.  Maximum
!        number of terms in the expansion is limited by the
!        parameter MAXSIZ.
!
!-    SUPPORT Version 1.0.0, 16-JUN-1996 (NJB)
!
!-&

! $ Index_Entries
!
!     fit Chebyshev expansion to a function
!     determine Chebyshev coefficients of a function
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               MAX2
      PARAMETER           ( MAX2    = MAXSIZ * MAXSIZ )

      INTEGER               MAX3
      PARAMETER           ( MAX3    = MAXSIZ * MAX2   )

!
!     Local variables
!
      DOUBLE PRECISION      ARG
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RTAB   ( MAXSIZ, MAXSIZ )
      DOUBLE PRECISION      TTAB   ( MAXSIZ, MAXSIZ, MAXSIZ )
      DOUBLE PRECISION      X

      INTEGER               I
      INTEGER               J
      INTEGER               K

      LOGICAL               PASS1

!
!     Saved variables
!
      SAVE                  PASS1
      SAVE                  RTAB
      SAVE                  TTAB

!
!     Initial values
!
      DATA                  PASS1 / .TRUE. /


!
!     Check in only if an error is detected.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Make sure the requested expansion order is not too large.
!
      IF ( N .GT. MAXSIZ ) THEN

         CALL CHKIN  ( 'CHBFIT'                                   )
         CALL SETMSG ( 'The requested expansion order # exceeds ' // &
                       'the maximum supported order #.'           )
         CALL ERRINT ( '#',  N                                    )
         CALL ERRINT ( '#',  MAXSIZ                               )
         CALL ERRINT ( '#',  N                                    )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                       )
         CALL CHKOUT ( 'CHBFIT'                                   )
         RETURN

      END IF

!
!     No data, no interpolation.
!
      IF ( N .LT. 1 ) THEN

         CALL CHKIN  ( 'CHBFIT'                              )
         CALL SETMSG ( 'Array size must be positive; was #.' )
         CALL ERRINT ( '#',  N                               )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                  )
         CALL CHKOUT ( 'CHBFIT'                              )
         RETURN

      END IF

!
!     Make sure the input interval is OK.
!
      IF ( LEFT .GE. RIGHT ) THEN

         CALL CHKIN  ( 'CHBFIT'                                 )
         CALL SETMSG ( 'Left endpoint = #; right endpoint = #.' )
         CALL ERRDP  ( '#',  LEFT                               )
         CALL ERRDP  ( '#',  RIGHT                              )
         CALL SIGERR ( 'SPICE(INVALIDENDPTS)'                   )
         CALL CHKOUT ( 'CHBFIT'                                 )
         RETURN

      END IF


      IF ( PASS1 ) THEN
!
!        On the first pass, compute a table of roots of all
!        Cheby polynomials from degree 1 to degree N.  The Ith
!        column of the table contains roots of the Ith polynomial.
!
         CALL CLEARD ( MAX2, RTAB )

         DO I = 1, MAXSIZ

            DO K = 1, I

               RTAB(K,I) =  DCOS (  PI() * ( K - .5D0 ) / I  )

            END DO

         END DO

!
!        Also compute a table of Chebyshev function values.  For
!        each expansion size J from 1 to N, we compute the values
!        of
!
!           T   (x ) ... T   ( x )
!            0    1       0     J
!
!                    .
!                    .
!                    .
!
!           T   (x ) ... T   ( x )
!            J-1  1       J-1   J
!
!        where
!
!           x
!            K
!
!        is the Kth root of
!
!           T
!            J
!
!        In our 3-dimensional table, the (K,I,J) entry is the value
!        of
!
!           T    ( x  )
!            I-1    K
!
!        where
!
!           x
!            K
!
!        is the Kth root of
!
!           T
!            J
!

         CALL CLEARD ( MAX3, TTAB )


         DO J = 1, MAXSIZ
!
!           Compute Cheby values needed to implement an expansion
!           of size J.
!
            DO I = 1, J
!
!              Compute values of
!
!                 T
!                  I-1
!
!              on the roots of
!
!                 T
!                  J
!
!
               DO K = 1, J
!
!                 Evaluate
!
!                    T
!                     I-1
!
!                 at the Kth root of
!
!                    T
!                     J
!
                  ARG          =  PI() * ( K - .5D0 ) / J

                  TTAB(K,I,J)  =  DCOS ( (I-1) * ARG )

               END DO

            END DO

         END DO

         PASS1 = .FALSE.

      END IF





!
!     Find the transformation parameters.
!
      MIDPT  =  ( RIGHT + LEFT ) / 2.D0
      RADIUS =  ( RIGHT - LEFT ) / 2.D0

!
!     Compute the input function values at the transformed Chebyshev
!     roots.
!
      DO K = 1, N

         X       =  RADIUS * RTAB(K,N)   +   MIDPT
         WORK(K) =  FUNC(X)

      END DO


!
!     Compute the coefficients.
!
      DO J = 1, N

         COEFFS(J) =  0.D0

         DO K = 1, N

            COEFFS(J) =  WORK(K) * TTAB(K,J,N)   +  COEFFS(J)

         END DO

         COEFFS(J) = 2.D0 * COEFFS(J) / N

      END DO

!
!     Scale the zero-order coefficient to simplify the form of the
!     Chebyshev expansion.
!
      COEFFS(1) = COEFFS(1) * 0.5D0

      RETURN
      END

