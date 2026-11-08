C$Procedure      T_T13XMD ( Type 13 SPK record to extended MDA )

      SUBROUTINE T_T13XMD ( T13REC, MDABEG, MDAEND, T21REC, FOUND )
      IMPLICIT NONE

C$ Abstract
C
C     Convert a type 13 SPK record to a type 21 (extended MDA) SPK
C     record.
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

      INCLUDE 'spk21.inc'


      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 300 )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 23 )

      INTEGER               MAXDIM
      PARAMETER           ( MAXDIM = MAXTRM )


      DOUBLE PRECISION      T13REC ( * )
      DOUBLE PRECISION      MDABEG
      DOUBLE PRECISION      MDAEND
      DOUBLE PRECISION      T21REC ( * )
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     T13REC     I   Type 13 SPK record.
C     MDABEG     I   Start time of expansion coverage interval.
C     MDAEND     I   End time of expansion coverage interval.
C     T21REC     O   Type 21 SPK record.
C     FOUND      O   Flag indicating whether solution was found.
C     MAXDEG     P   Maximum type 13 polynomial degree.
C     MAXDIM     P   Maximum difference line table size.
C
C$ Detailed_Input
C
C     T13REC         is a type 13 SPK record.  Each such record is
C                    structured as follows:
C
C                       +----------------------+
C                       | number of states (n) |
C                       +----------------------+
C                       | state 1 (6 elts.)    |
C                       +----------------------+
C                       | state 2 (6 elts.)    |
C                       +----------------------+
C                                   .
C                                   .
C                                   .
C                       +----------------------+
C                       | state n (6 elts.)    |
C                       +----------------------+
C                       | epochs 1--n          |
C                       +----------------------+
C
C
C     MDABEG         is the start time of the coverage interval of
C                    the MDA created by this routine.
C
C     MDAEND         is the end time of the coverage interval of the
C                    MDA created by this routine. This is also the time
C                    value about which the Taylor expansions of each
C                    position component are centered.
C
C
C$ Detailed_Output
C
C     T21REC         is a type 21 SPK record. Each such record contains
C                    DLSIZE+1 double precision numbers, where DLSIZE is
C
C                       ( 4 * MAXDIM ) + 11
C
C                    and MAXDIM is in the range 15:MAXTRM.
C
C                    The layout of data in the record is as follows:
C
C                        T21REC( 1 ):             MAXDIM
C                        T21REC( 2 : DLSIZE ):    an extended difference
C                                                 line.
C
C                    The contents of an extended difference line are as
C                    shown below:
C
C                       Dimension  Description
C                       ---------  ----------------------------------
C                       1          Reference epoch of difference line
C                       MAXDIM     Stepsize function vector
C                       1          Reference position vector,  x
C                       1          Reference velocity vector,  x
C                       1          Reference position vector,  y
C                       1          Reference velocity vector,  y
C                       1          Reference position vector,  z
C                       1          Reference velocity vector,  z
C                       MAXDIM,3   Modified divided difference
C                                  arrays (MDAs)
C                       1          Maximum integration order plus 1
C                       3          Integration order array
C
C                    The reference position and velocity are those of
C                    BODY relative to CENTER at the reference epoch.
C                    (A difference line is essentially a polynomial
C                    expansion of acceleration about the reference
C                    epoch.)
C
C                    See the TSPICE routine T_PD2DT for details on how
C                    the MDAs are derived.
C
C
C     FOUND          is a logical flag indicating whether the desired
C                    output record was computable.  If the solution
C                    process gives rise to a singular coefficient
C                    matrix, no solution is found.
C
C$ Parameters
C
C     MAXDEG        is the maximum allowed degree of the polynomials
C                   defined by the type 13 input record.
C
C     MAXDIM        is the maximum extended difference line table size
C                   per component.
C
C$ Exceptions
C
C     1) If the linear algebraic system defining the Taylor
C        coefficients of the trajectory polynomials is singular,
C        FOUND will be returned .FALSE.
C
C     2) If the input MDAEND is not within the closed interval
C        defined by the time tags in the input record, the
C        error SPICE(TIMEOUTOFRANGE) is signaled.
C
C     3) If the polynomial degree associated with the input record
C        is outside of the range [1:MAXDEG], the error
C        SPICE(VALUEOUTOFRANGE) is signaled.
C
C     4) If the interpolation window size of the input type 13
C        record is odd, the error SPICE(INVALIDSIZE) is signaled.
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
C     1) This routine is intended for use only within the TSPICE
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
C-    TSPICE Version 2.0.0, 20-OCT-2012 (NJB)
C
C        This routine was renamed for use in TSPICE. Calls to SPKNIO
C        utilities TAYMAT, TAYHRM, PD2DT were replaced by calls to
C        routines T_TAYMAT, T_TAYHRM, and T_PD2DT. Unused variable
C        RIDX was removed. Several typos in comments were corrected.
C
C-    SPKNIO Version 1.0.0, 24-MAR-2003 (NJB)
C
C-&

C
C     SPICELIB functions
C
      INTEGER               LSTLTD

      LOGICAL               ODD
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               MDAOFF
      PARAMETER           ( MDAOFF = MAXTRM + 7 )

      INTEGER               MDASIZ
      PARAMETER           ( MDASIZ = MAXTRM )

      INTEGER               MAXCOF
      PARAMETER           ( MAXCOF = MAXDEG + 1 )

      INTEGER               MSIZE
      PARAMETER           ( MSIZE  = MAXCOF * MAXCOF )

C
C     Local variables
C
      DOUBLE PRECISION      ACCDER ( MDASIZ )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DBUFF  ( MAXCOF * 3 )
      DOUBLE PRECISION      DERIVS ( MAXCOF )
      DOUBLE PRECISION      DT     ( MDASIZ )
      DOUBLE PRECISION      MBUFF  ( MSIZE )
      DOUBLE PRECISION      RSYS   ( MAXCOF * 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SPAN
      DOUBLE PRECISION      SHRINK
      DOUBLE PRECISION      TVALS  ( MAXCOF )
      DOUBLE PRECISION      WORK   ( MAXREC, MAXREC )
      DOUBLE PRECISION      X

      INTEGER               DEGP
      INTEGER               DLSIZE
      INTEGER               FROM
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LIDX
      INTEGER               MDALOC
      INTEGER               N
      INTEGER               TO
      INTEGER               TSTART


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_T13XMD' )

C
C     Nothing found yet.
C
      FOUND = .FALSE.

C
C     For each position component, find the Taylor expansion of its
C     polynomial representation at X. Convert the derivative set to a
C     difference line. Store the difference line in the type 21 record.
C
C     Get the type 13 polynomial degree from the state count.
C
      N  =  NINT(  T13REC(1)  )

      IF ( ODD(N) ) THEN

         CALL SETMSG ( 'Window size is #; must be even' )
         CALL ERRINT ( '#', N                           )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'             )
         CALL CHKOUT ( 'T_T13XMD'                       )
         RETURN

      END IF

      DEGP  =  2*N - 1

      IF (  ( DEGP .LT. 1 ) .OR. ( DEGP .GT. MAXDEG )  ) THEN

         CALL SETMSG ( 'Polynomial degree is #; must be in range 1:#.' )
         CALL ERRINT ( '#', DEGP                                       )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                        )
         CALL CHKOUT ( 'T_T13XMD'                                      )
         RETURN

      END IF

C
C     Let TSTART be the index in T13REC where the time values start.
C
      TSTART = 2 + 6*N

C
C     Find the span of the time values in the record.
C
      SPAN = T13REC(TSTART+N-1) - T13REC(TSTART)

C
C     Ensure that at least one time tag in the input record
C     strictly precedes MDAEND.
C
      LIDX = LSTLTD ( MDAEND, N, T13REC(TSTART) )

      IF ( LIDX .EQ. 0 ) THEN

         CALL SETMSG ( 'Input time MDAEND = #; is not greater '
     .   //            'than first epoch in type 13 record #.'  )
         CALL ERRDP  ( '#', MDAEND                              )
         CALL ERRDP  ( '#', T13REC(TSTART)                      )
         CALL SIGERR ( 'SPICE(TIMEOUTOFRANGE)'                  )
         CALL CHKOUT ( 'T_T13XMD'                               )
         RETURN

      END IF

      IF ( MDAEND .GT. T13REC(TSTART+N-1) ) THEN

         CALL SETMSG ( 'Input time MDAEND = #; exceeds last ' //
     .                 'epoch in type 13 record #.'           )
         CALL ERRDP  ( '#', MDAEND                            )
         CALL ERRDP  ( '#', T13REC(TSTART+N-1)                )
         CALL SIGERR ( 'SPICE(TIMEOUTOFRANGE)'                )
         CALL CHKOUT ( 'T_T13XMD'                             )
         RETURN

      END IF

C
C     Transform the time tags by scaling their distance from
C     the central point of the expansion.  We pick the scale to
C     make the width of the interval from first to last
C     transformed tag equal to 1.
C
      DO I = 1, N

         TVALS(I) = ( T13REC(TSTART-1+I) - MDAEND ) / SPAN

      END DO

C
C     Let X be the transformed center of the expansion.
C
      X = 0.D0

C
C     Let D be the step size for the difference line.
C     D will be equal to the length of the interval
C     over which the polynomial expansion is applicable.
C
      D  =  MDAEND - MDABEG

C
C     Rather than evaluating the derivatives of our expansion, we'll
C     evaluate the derivatives of a related expansion that has support
C     on a shorter interval.
C
C        This shrinking operation was originally done to work around a
C        numeric overflow problem in separate software run on a VAX and
C        subject to configuration control restrictions that prevented
C        correction of the problem.
C
C        Since the algorithm works and since there is no strong
C        reason to change it, it is left in its original form.
C
C
      SHRINK = MIN ( 1.D0, 1.D0/SPAN )

C
C     Pack the function and derivative values.
C
      DO I = 1, 3

         TO =  1  +  2 * N * (I-1)

         DO J = 1, N

            FROM          =   1  +  (J-1)*6  +  I
            RSYS(TO)      =   T13REC( FROM )

            FROM          =   1  +  (J-1)*6  +  I  + 3
            RSYS(TO + N)  =   T13REC( FROM ) *  SPAN

            TO            =   TO +  1

         END DO

      END DO

C
C
C     Construct a coefficient matrix.
C
      CALL T_TAYMAT ( N, TVALS, X, MBUFF )

C
C     Solve for the Taylor coefficients at X, and compute derivatives
C     for all three vector components.
C
      CALL T_TAYHRM ( N,  3,  MBUFF,  RSYS,  DBUFF,  FOUND )

      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'T_T13XMD' )
         RETURN
      END IF

      DO I = 1, 3
C
C        Unpack the derivatives for the Ith component.
C
         FROM = 1 + ( 2*N * (I-1) )

         CALL MOVED ( DBUFF(FROM), 2*N, DERIVS )

C
C        Now that we have the derivatives, obtain the corresponding
C        difference line and write it into the type 21 output record.
C        The record description from SPKE21 is copied below.
C
C        Name     Dimension  Description
C        ------   ---------  -------------------------------
C        TL               1  Final epoch of record
C        G           MAXDIM  Stepsize function vector
C        REFPOS           3  Reference position vector
C        REFVEL           3  Reference velocity vector
C        DT      MAXDIM,NTE  Modified divided difference arrays
C        KQMAX1           1  Maximum integration order plus 1
C        KQ             NTE  Integration order array
C
C
C        Scale velocity and acceleration to compensate for the
C        shrinkage factor. The higher derivatives are scaled
C        separately.
C
         DERIVS(2) = DERIVS(2) * SHRINK
         DERIVS(3) = DERIVS(3) * SHRINK**2

C
C        Compute the scaled step size S.
C
         S  = D * SHRINK

C
C        We're going to scale the Jth element of the acceleration
C        portion (starting with the derivative of acceleration) of our
C        derivatives array by SHRINK**2 * S**J.  Since S can be large,
C        we must do this carefully.  We arrange our multiplications so
C        that we never compute a large power of S directly.
C
         CALL CLEARD ( MDASIZ, ACCDER )

         CALL MOVED ( DERIVS(4), DEGP-2, ACCDER )

         DO J = 1, DEGP-2

            ACCDER(J) = ACCDER(J) * S * SHRINK**2

            DO K = 1, J-1
               ACCDER(J)  =  ACCDER(J) * S
            END DO

         END DO


         CALL MOVED ( ACCDER, DEGP-2, DERIVS(4) )

         CALL CLEARD ( MDASIZ, DT )

C
C        Compute the difference table for the current polynomial:
C
         CALL T_PD2DT ( DEGP-2, DERIVS(3), WORK, DT )

         MDALOC = MDAOFF + 1 + (I-1)*MDASIZ

         CALL MOVED ( DT, MDASIZ, T21REC(MDALOC) )

C
C        Put into the output array the reference position and velocity
C        values for the current state component.
C
         MDALOC = 2 + MDASIZ + 2*(I-1)

         T21REC ( MDALOC ) = DERIVS(1)

         MDALOC = MDALOC + 1

         T21REC ( MDALOC ) = DERIVS(2)

      END DO
C
C     Fill in the rest of the type 21 record.
C
C     Reference epoch.  This is also the final epoch covered by
C     the record:
C
      T21REC(1) = MDAEND

C
C     Stepsize values:
C
      CALL FILLD ( D, MDASIZ, T21REC(2) )

C
C     Maximum integration order plus 1:
C
      MDALOC            =  1 + MDASIZ + 2*3 + 3*MDASIZ + 1

      T21REC ( MDALOC ) =  MDASIZ + 1

C
C     Integration orders for each component:
C
      MDALOC = MDALOC + 1

      CALL FILLD ( DBLE(MDASIZ), 3, T21REC(MDALOC) )

C
C     Shift the record to make room for the initial size value.
C
      DLSIZE = ( 4 * MDASIZ ) + 11

      DO I = DLSIZE, 1, -1

         T21REC(I+1) = T21REC(I)

      END DO

      T21REC(1) = DBLE( MAXDIM )

C
C     FOUND is set; it was set by TAYHRM.
C
      CALL CHKOUT ( 'T_T13XMD' )
      RETURN
      END
