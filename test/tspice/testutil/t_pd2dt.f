C$Procedure  T_PD2DT ( Polynomial derivatives to difference table )

      SUBROUTINE T_PD2DT ( DEGP, PDERIV, WORK, DIFTAB )
      IMPLICIT NONE

C$ Abstract
C
C     Convert a polynomial and its derivatives, evaluated at a point,
C     to an MDA "difference table."
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
C     SPK
C
C$ Declarations

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 35 )

      INTEGER               DEGP
      DOUBLE PRECISION      PDERIV ( 0 : DEGP                )
      DOUBLE PRECISION      WORK   ( 0 : DEGP-1,  0 : DEGP-1 )
      DOUBLE PRECISION      DIFTAB ( 0 : DEGP                )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAXDEG     I   Maximum degree of internal work space buffer.
C     DEGP       I   Polynomial degree.
C     PDERIV    I-O  Array of polynomial derivatives.
C     WORK      I-O  Work space array.
C     DIFTAB     O   Difference table.
C
C$ Detailed_Input
C
C     DEGP           is the degree of a polynomial P(x) to be mapped to
C                    a difference table.
C
C     PDERIV         is an array of derivatives of P(x), evaluated at
C                    some point.  Some computations are done in place
C                    in this array, so this is an in-out argument.
C                    The element of PDERIV at index 0 is the function
C                    value itself.
C
C                    In normal use within SPKNIO, PDERIV will be the
C                    derivatives of a polynomial representing one
C                    component (x-, y-, or z-) of the acceleration of
C                    an ephemeris object.
C
C     WORK           is a work space array.
C
C$ Detailed_Output
C
C     PDERIV         is an array of derivatives of P(x), evaluated at
C                    some point.  Some computations are done in place
C                    in this array, so this is an in-out argument.
C                    On output, the jth element of PDERIV has been
C                    divided by (j+1)!.
C
C     WORK           is the work space array, filled in with a matrix
C                    used to produce the difference table.
C
C     DIFTAB         is a difference table.  This is a vector that
C                    represents the input polynomial and its
C                    derivatives. It is in a form such that, when the
C                    input polynomial derivative vector represents a
C                    component of the acceleration of some object,
C                    DIFTAB is amenable to integration by the SPICELIB
C                    type 1 SPK evaluator SPKE01.
C
C                    The relationship of DIFTAB to the input polynomial
C                    derivative vector is given by the equations below.
C                    The matrix labeled M below is upper-triangular,
C                    with zeros below the main diagonal.
C
C
C     DIFTAB(0)       =                             PDERIV(0)
C
C
C   +-            -+     +-                    -+ +-                 -+
C   | DIFTAB(1)    |     | \      k-j   k!      | | PDERIV(1)   /1!   |
C   | DIFTAB(2)    |     |  \ (-1)   ---------  | | PDERIV(2)   /2!   |
C   |       .      |  =  |   \       (k-j)! j!  | |       .           |
C   |       .      |     |    \                 | |       .           |
C   |       .      |     | 0   \                | |       .           |
C   | DIFTAB(PDEG) |     |      \               | | PDERIV(PDEG)/PDEG!|
C   +-            -+     +-                    -+ +-                 -+
C
C                                   (M)
C
C
C
C$ Parameters
C
C     MAXDEG        is the maximum polynomial degree supported by
C                   the internal work space buffer.  Difference tables
C                   having higher degree than MAXDEG are handled by
C                   this routine, but the run-time efficiency will be
C                   lower, because the matrix M**-1 will have to
C                   be re-computed on each call.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) This routine is intended for use only within the SPKNIO
C        program.
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
C-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
C
C        This routine has been moved to the TESTUTIL library.
C
C-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
C
C        MAXDEG was increased to 35. Routine was renamed for
C        use in TSPICE. Call to CHOOSE was replaced with call
C        to T_CHOOSE.
C
C-    SPKNIO Version 1.1.0, 02-MAR-2003 (NJB)
C
C        Now buffers the matrix
C
C            -1
C           M
C
C        and re-uses this matrix when the difference table degree
C        matches that from the previous call.
C
C-    SPKNIO Version 1.0.0, 11-APR-2000 (NJB)
C
C-&

C

C
C     Non-SPICELIB functions
C
      INTEGER               T_CHOOSE

C
C     Local variables
C
      DOUBLE PRECISION      FACT
      DOUBLE PRECISION      MINV   ( MAXDEG, MAXDEG )

      INTEGER               DEGPSQ
      INTEGER               J
      INTEGER               K
      INTEGER               PRVDEG
      INTEGER               SIGN

      LOGICAL               USEMNV

C
C     Saved variables
C
      SAVE                  MINV
      SAVE                  PRVDEG

C
C     Initial values
C
      DATA                  PRVDEG  / -1 /

C
C     The first element of the difference table is just the
C     constant term from the polynomial.
C
      DIFTAB(0) = PDERIV(0)

C
C     Use the work space array to build the mystery matrix
C
C         -1
C        M
C
C
C     The (j,k) entry of the mystery matrix is
C
C
C            k-j
C        (-1)    * ( <k choose j> )
C
C
C     for k >= j.  The lower triangular portion of the matrix is
C     identically zero.
C
      IF (       ( DEGP .EQ. PRVDEG )
     .     .AND. ( DEGP .LE. MAXDEG )  ) THEN
C
C        We can use the previously computed matrix MINV.
C
         USEMNV = .TRUE.

      ELSE

         DEGPSQ = DEGP * DEGP

         CALL CLEARD ( DEGPSQ, WORK )


         DO J = 0, DEGP-1

            SIGN   =   1

            DO K = J, DEGP-1

               WORK(J,K) = SIGN * T_CHOOSE( K, J )

               SIGN = -SIGN

            END DO

         END DO

         IF ( DEGP .LE. MAXDEG ) THEN
            CALL MOVED ( WORK, DEGPSQ, MINV )
         END IF


         USEMNV = .FALSE.

      END IF


C
C     Divide the jth element of PDERIV by j!.
C
      FACT = 1

      DO J = 1, DEGP

         FACT = J*FACT

         PDERIV(J) = PDERIV(J) / FACT

      END DO

C
C     Now left-multiply our vector of polynomial derivatives by
C     the mystery matrix.
C
      IF ( USEMNV ) THEN

         CALL MXVG ( MINV, PDERIV(1), DEGP, DEGP, DIFTAB(1) )

      ELSE

         CALL MXVG ( WORK, PDERIV(1), DEGP, DEGP, DIFTAB(1) )

      END IF

      PRVDEG = DEGP

      END
