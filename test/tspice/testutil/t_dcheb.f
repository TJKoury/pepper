C$Procedure      T_DCHEB ( Numeric derivative from Chebyshev fit )
 
      SUBROUTINE T_DCHEB ( FUNC, X, DELTA, NTERMS, NRTAIN, F, DF )
      IMPLICIT NONE
 
C$ Abstract
C
C     Numerically estimate the derivative of a scalar function
C     at a specified point by fitting a Chebyshev expansion
C     to the function and differentiating the expansion.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     INTERPOLATION
C     MATH
C     POLYNOMIAL
C
C$ Declarations
 
      DOUBLE PRECISION      FUNC
      EXTERNAL              FUNC
      DOUBLE PRECISION      X
      DOUBLE PRECISION      DELTA
      INTEGER               NTERMS
      INTEGER               NRTAIN
      DOUBLE PRECISION      F
      DOUBLE PRECISION      DF

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAXSIZ     P   Maximum number of terms in expansion.
C     FUNC       I   Function to be approximated.
C     X          I   Point at which to compute derivative.
C     DELTA      I   Half-width of domain of fit.
C     NTERMS     I   Number of terms in fitted expansion.
C     NRTAIN     I   Number of retained terms used for derivative.
C     F          O   Value of expansion at X.
C     DF         O   Derivative estimate at X.
C
C$ Detailed_Input
C
C     FUNC           is the function to be differentiated.  FUNC must
C                    accept a single, double precision input argument
C                    and must return a double precision value.  FUNC
C                    should be declared EXTERNAL in the caller of this
C                    routine.
C
C
C     X              is the point in the domain of FUNC at which the
C                    derivative of FUNC is to be estimated.
C
C
C     DELTA          is the half width of the interval over which
C                    FUNC is to be fitted by a Chebyshev expansion.
C                    The interval used is
C
C                       [ X - DELTA,  X + DELTA ]
C
C
C     NTERMS         is the number of terms in the Chebyshev expansion
C                    used to approximate FUNC. The degree of the 
C                    expansion is NTERMS-1.                    
C
C     NRTAIN         is the number of retained terms in the Chebyshev
C                    expansion. These terms are used to compute the
C                    derivative estimate. NRTAIN should be at least 1
C                    and cannot exceed NTERMS.
C
C$ Detailed_Output
C
C     F              is the result of evaluating the retained Chebyshev
C                    expansion at X. 
C
C
C     DF             is the result of evaluating the derivative of the 
C                    retained Chebyshev expansion at X. 
C
C
C$ Parameters
C
C     MAXSIZ         is the maximum number of terms in the Chebyshev
C                    expansion. This is the maximum allowed value of
C                    NTERMS.
C
C$ Exceptions
C
C     1)  If NTERMS is less than 1 or greater than MAXSIZ, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     2)  If NRTAIN is less than 1 or greater than NTERMS, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     3)  If DELTA is not strictly positive the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     4)  This routine does not attempt to ward off or diagnose
C         arithmetic overflows.
C
C     5)  See the routines CHBFIT and CHBINT for exceptions specific
C         to those routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides a numeric estimate of the derivative of
C     a function of one variable. The derivative is computed by 
C
C        1) Fitting to the input function, over the interval
C
C              X - DELTA  :  X + DELTA
C
C           a Chebyshev expansion of a degree NTERMS-1.
C
C        2) Estimating the derivative of the function at X by 
C           differentiating the first NRTAIN terms of the 
C           expansion.
C
C     A heuristic comment:
C
C        It can be advantageous to use a large value of NTERMS together
C        with a smaller value of NRTAIN. The number of function samples
C        used to compute each Chebyshev coefficient is determined by
C        NTERMS (see the SUPPORT library routine CHBFIT for details).
C        Throwing away the higher-order terms of the expansion before
C        differentiating can remove noise from the function and improve
C        accuracy of the derivative estimate.
C     
C
C$ Examples
C
C     See usage in the TSPICE routine T_DZZILU.
C
C$ Restrictions
C
C     1)  Maximum number of terms in the expansion is limited by the
C         parameter MAXSIZ.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 06-APR-2012 (NJB)
C
C-&
 
C$ Index_Entries
C
C     differentiate a Chebyshev expansion of a function
C     discrete derivative from Chebyshev fit
C     numeric derivative from Chebyshev fit
C
C-&
 
 

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 25 )

C
C     Local variables
C     
      DOUBLE PRECISION      COEFFS ( MAXSIZ )
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      WORK   ( MAXSIZ )
      DOUBLE PRECISION      X2S    ( 2 )

      INTEGER               DEGP
 

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'T_DCHEB' )

C
C     Check NTERMS.
C
      IF ( ( NTERMS .LT. 1 ) .OR. ( NTERMS .GT. MAXSIZ ) ) THEN

         CALL SETMSG ( 'NTERMS must be in the range 1:# but was #.' )
         CALL ERRINT ( '#',  MAXSIZ                                 )
         CALL ERRINT ( '#',  NTERMS                                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                     )
         CALL CHKOUT ( 'T_DCHEB'                                    )
         RETURN

      END IF

C
C     Check NRTAIN.
C
      IF ( ( NRTAIN .LT. 1 ) .OR. ( NRTAIN .GT. NTERMS ) ) THEN

         CALL SETMSG ( 'NRTAIN must be in the range 1:# but was #.' )
         CALL ERRINT ( '#',  NTERMS                                 )
         CALL ERRINT ( '#',  NRTAIN                                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                     )
         CALL CHKOUT ( 'T_DCHEB'                                    )
         RETURN

      END IF

C
C     Check DELTA.
C
      IF ( DELTA .LE. 0D0 )  THEN

         CALL SETMSG ( 'DELTA must be positive but was #.' )
         CALL ERRDP  ( '#',  DELTA                         )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'            )
         CALL CHKOUT ( 'T_DCHEB'                           )
         RETURN

      END IF
      
C
C     Set domain bounds for Chebyshev fit:
C
      LEFT  = X - DELTA
      RIGHT = X + DELTA

C
C     Set transformation parameters:
C
      X2S( 1 ) = X
      X2S( 2 ) = DELTA

C
C     Fit N-term Cheby expansion to the input function on [LEFT,
C     RIGHT].
C
      CALL CHBFIT ( FUNC, LEFT, RIGHT, NTERMS, WORK, COEFFS )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'T_DCHEB' )
         RETURN
      END IF

C
C     Find the first derivative of the expansion at X.
C
      DEGP = NRTAIN - 1
      CALL CHBINT ( COEFFS, DEGP, X2S, X, F, DF )
  

      CALL CHKOUT ( 'T_DCHEB' )
      RETURN
      END
