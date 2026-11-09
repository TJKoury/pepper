!$Procedure      T_T13XMD ( Type 13 SPK record to extended MDA )

      SUBROUTINE T_T13XMD ( T13REC, MDABEG, MDAEND, T21REC, FOUND )
      use pepper_inc_spk21
      IMPLICIT NONE

! $ Abstract
!
!     Convert a type 13 SPK record to a type 21 (extended MDA) SPK
!     record.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SPK
!
! $ Declarations



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

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     T13REC     I   Type 13 SPK record.
!     MDABEG     I   Start time of expansion coverage interval.
!     MDAEND     I   End time of expansion coverage interval.
!     T21REC     O   Type 21 SPK record.
!     FOUND      O   Flag indicating whether solution was found.
!     MAXDEG     P   Maximum type 13 polynomial degree.
!     MAXDIM     P   Maximum difference line table size.
!
! $ Detailed_Input
!
!     T13REC         is a type 13 SPK record.  Each such record is
!                    structured as follows:
!
!                       +----------------------+
!                       | number of states (n) |
!                       +----------------------+
!                       | state 1 (6 elts.)    |
!                       +----------------------+
!                       | state 2 (6 elts.)    |
!                       +----------------------+
!                                   .
!                                   .
!                                   .
!                       +----------------------+
!                       | state n (6 elts.)    |
!                       +----------------------+
!                       | epochs 1--n          |
!                       +----------------------+
!
!
!     MDABEG         is the start time of the coverage interval of
!                    the MDA created by this routine.
!
!     MDAEND         is the end time of the coverage interval of the
!                    MDA created by this routine. This is also the time
!                    value about which the Taylor expansions of each
!                    position component are centered.
!
!
! $ Detailed_Output
!
!     T21REC         is a type 21 SPK record. Each such record contains
!                    DLSIZE+1 double precision numbers, where DLSIZE is
!
!                       ( 4 * MAXDIM ) + 11
!
!                    and MAXDIM is in the range 15:MAXTRM.
!
!                    The layout of data in the record is as follows:
!
!                        T21REC( 1 ):             MAXDIM
!                        T21REC( 2 : DLSIZE ):    an extended difference
!                                                 line.
!
!                    The contents of an extended difference line are as
!                    shown below:
!
!                       Dimension  Description
!                       ---------  ----------------------------------
!                       1          Reference epoch of difference line
!                       MAXDIM     Stepsize function vector
!                       1          Reference position vector,  x
!                       1          Reference velocity vector,  x
!                       1          Reference position vector,  y
!                       1          Reference velocity vector,  y
!                       1          Reference position vector,  z
!                       1          Reference velocity vector,  z
!                       MAXDIM,3   Modified divided difference
!                                  arrays (MDAs)
!                       1          Maximum integration order plus 1
!                       3          Integration order array
!
!                    The reference position and velocity are those of
!                    BODY relative to CENTER at the reference epoch.
!                    (A difference line is essentially a polynomial
!                    expansion of acceleration about the reference
!                    epoch.)
!
!                    See the TSPICE routine T_PD2DT for details on how
!                    the MDAs are derived.
!
!
!     FOUND          is a logical flag indicating whether the desired
!                    output record was computable.  If the solution
!                    process gives rise to a singular coefficient
!                    matrix, no solution is found.
!
! $ Parameters
!
!     MAXDEG        is the maximum allowed degree of the polynomials
!                   defined by the type 13 input record.
!
!     MAXDIM        is the maximum extended difference line table size
!                   per component.
!
! $ Exceptions
!
!     1) If the linear algebraic system defining the Taylor
!        coefficients of the trajectory polynomials is singular,
!        FOUND will be returned .FALSE.
!
!     2) If the input MDAEND is not within the closed interval
!        defined by the time tags in the input record, the
!        error SPICE(TIMEOUTOFRANGE) is signaled.
!
!     3) If the polynomial degree associated with the input record
!        is outside of the range [1:MAXDEG], the error
!        SPICE(VALUEOUTOFRANGE) is signaled.
!
!     4) If the interpolation window size of the input type 13
!        record is odd, the error SPICE(INVALIDSIZE) is signaled.
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
!     None.
!
! $ Restrictions
!
!     1) This routine is intended for use only within the TSPICE
!        program.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
!
!        This routine has been moved to the TESTUTIL library.
!
!-    TSPICE Version 2.0.0, 20-OCT-2012 (NJB)
!
!        This routine was renamed for use in TSPICE. Calls to SPKNIO
!        utilities TAYMAT, TAYHRM, PD2DT were replaced by calls to
!        routines T_TAYMAT, T_TAYHRM, and T_PD2DT. Unused variable
!        RIDX was removed. Several typos in comments were corrected.
!
!-    SPKNIO Version 1.0.0, 24-MAR-2003 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               LSTLTD

      LOGICAL               ODD
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               MDAOFF
      PARAMETER           ( MDAOFF = MAXTRM + 7 )

      INTEGER               MDASIZ
      PARAMETER           ( MDASIZ = MAXTRM )

      INTEGER               MAXCOF
      PARAMETER           ( MAXCOF = MAXDEG + 1 )

      INTEGER               MSIZE
      PARAMETER           ( MSIZE  = MAXCOF * MAXCOF )

!
!     Local variables
!
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

!
!     Nothing found yet.
!
      FOUND = .FALSE.

!
!     For each position component, find the Taylor expansion of its
!     polynomial representation at X. Convert the derivative set to a
!     difference line. Store the difference line in the type 21 record.
!
!     Get the type 13 polynomial degree from the state count.
!
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

!
!     Let TSTART be the index in T13REC where the time values start.
!
      TSTART = 2 + 6*N

!
!     Find the span of the time values in the record.
!
      SPAN = T13REC(TSTART+N-1) - T13REC(TSTART)

!
!     Ensure that at least one time tag in the input record
!     strictly precedes MDAEND.
!
      LIDX = LSTLTD ( MDAEND, N, T13REC(TSTART) )

      IF ( LIDX .EQ. 0 ) THEN

         CALL SETMSG ( 'Input time MDAEND = #; is not greater ' &
         //            'than first epoch in type 13 record #.'  )
         CALL ERRDP  ( '#', MDAEND                              )
         CALL ERRDP  ( '#', T13REC(TSTART)                      )
         CALL SIGERR ( 'SPICE(TIMEOUTOFRANGE)'                  )
         CALL CHKOUT ( 'T_T13XMD'                               )
         RETURN

      END IF

      IF ( MDAEND .GT. T13REC(TSTART+N-1) ) THEN

         CALL SETMSG ( 'Input time MDAEND = #; exceeds last ' // &
                       'epoch in type 13 record #.'           )
         CALL ERRDP  ( '#', MDAEND                            )
         CALL ERRDP  ( '#', T13REC(TSTART+N-1)                )
         CALL SIGERR ( 'SPICE(TIMEOUTOFRANGE)'                )
         CALL CHKOUT ( 'T_T13XMD'                             )
         RETURN

      END IF

!
!     Transform the time tags by scaling their distance from
!     the central point of the expansion.  We pick the scale to
!     make the width of the interval from first to last
!     transformed tag equal to 1.
!
      DO I = 1, N

         TVALS(I) = ( T13REC(TSTART-1+I) - MDAEND ) / SPAN

      END DO

!
!     Let X be the transformed center of the expansion.
!
      X = 0.D0

!
!     Let D be the step size for the difference line.
!     D will be equal to the length of the interval
!     over which the polynomial expansion is applicable.
!
      D  =  MDAEND - MDABEG

!
!     Rather than evaluating the derivatives of our expansion, we'll
!     evaluate the derivatives of a related expansion that has support
!     on a shorter interval.
!
!        This shrinking operation was originally done to work around a
!        numeric overflow problem in separate software run on a VAX and
!        subject to configuration control restrictions that prevented
!        correction of the problem.
!
!        Since the algorithm works and since there is no strong
!        reason to change it, it is left in its original form.
!
!
      SHRINK = MIN ( 1.D0, 1.D0/SPAN )

!
!     Pack the function and derivative values.
!
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

!
!
!     Construct a coefficient matrix.
!
      CALL T_TAYMAT ( N, TVALS, X, MBUFF )

!
!     Solve for the Taylor coefficients at X, and compute derivatives
!     for all three vector components.
!
      CALL T_TAYHRM ( N,  3,  MBUFF,  RSYS,  DBUFF,  FOUND )

      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'T_T13XMD' )
         RETURN
      END IF

      DO I = 1, 3
!
!        Unpack the derivatives for the Ith component.
!
         FROM = 1 + ( 2*N * (I-1) )

         CALL MOVED ( DBUFF(FROM), 2*N, DERIVS )

!
!        Now that we have the derivatives, obtain the corresponding
!        difference line and write it into the type 21 output record.
!        The record description from SPKE21 is copied below.
!
!        Name     Dimension  Description
!        ------   ---------  -------------------------------
!        TL               1  Final epoch of record
!        G           MAXDIM  Stepsize function vector
!        REFPOS           3  Reference position vector
!        REFVEL           3  Reference velocity vector
!        DT      MAXDIM,NTE  Modified divided difference arrays
!        KQMAX1           1  Maximum integration order plus 1
!        KQ             NTE  Integration order array
!
!
!        Scale velocity and acceleration to compensate for the
!        shrinkage factor. The higher derivatives are scaled
!        separately.
!
         DERIVS(2) = DERIVS(2) * SHRINK
         DERIVS(3) = DERIVS(3) * SHRINK**2

!
!        Compute the scaled step size S.
!
         S  = D * SHRINK

!
!        We're going to scale the Jth element of the acceleration
!        portion (starting with the derivative of acceleration) of our
!        derivatives array by SHRINK**2 * S**J.  Since S can be large,
!        we must do this carefully.  We arrange our multiplications so
!        that we never compute a large power of S directly.
!
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

!
!        Compute the difference table for the current polynomial:
!
         CALL T_PD2DT ( DEGP-2, DERIVS(3), WORK, DT )

         MDALOC = MDAOFF + 1 + (I-1)*MDASIZ

         CALL MOVED ( DT, MDASIZ, T21REC(MDALOC) )

!
!        Put into the output array the reference position and velocity
!        values for the current state component.
!
         MDALOC = 2 + MDASIZ + 2*(I-1)

         T21REC ( MDALOC ) = DERIVS(1)

         MDALOC = MDALOC + 1

         T21REC ( MDALOC ) = DERIVS(2)

      END DO
!
!     Fill in the rest of the type 21 record.
!
!     Reference epoch.  This is also the final epoch covered by
!     the record:
!
      T21REC(1) = MDAEND

!
!     Stepsize values:
!
      CALL FILLD ( D, MDASIZ, T21REC(2) )

!
!     Maximum integration order plus 1:
!
      MDALOC            =  1 + MDASIZ + 2*3 + 3*MDASIZ + 1

      T21REC ( MDALOC ) =  MDASIZ + 1

!
!     Integration orders for each component:
!
      MDALOC = MDALOC + 1

      CALL FILLD ( DBLE(MDASIZ), 3, T21REC(MDALOC) )

!
!     Shift the record to make room for the initial size value.
!
      DLSIZE = ( 4 * MDASIZ ) + 11

      DO I = DLSIZE, 1, -1

         T21REC(I+1) = T21REC(I)

      END DO

      T21REC(1) = DBLE( MAXDIM )

!
!     FOUND is set; it was set by TAYHRM.
!
      CALL CHKOUT ( 'T_T13XMD' )
      RETURN
      END
