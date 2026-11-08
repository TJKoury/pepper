!$Procedure SPKE21 ( S/P Kernel, evaluate, type 21 )

      SUBROUTINE SPKE21 ( ET, RECORD, STATE )

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 21
!     (Extended Difference Lines).
!
! $ Required_Reading
!
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'spk21.inc'

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Evaluation epoch.
!     RECORD     I   Data record.
!     STATE      O   State (position and velocity).
!     MAXTRM     P   Maximum number of terms per difference table
!                    component.
!
! $ Detailed_Input
!
!     ET       is an epoch at which a state vector is to be
!              computed. The epoch is represented as seconds past
!              J2000 TDB.
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of an
!              ephemeris object, relative to its center of motion,
!              in an inertial reference frame.
!
!              The contents of RECORD are as follows:
!
!                 RECORD(1):         The difference table size per
!                                    Cartesian component. Call this
!                                    size MAXDIM; then the difference
!                                    line (MDA) size DLSIZE is
!
!                                      ( 4 * MAXDIM ) + 11
!
!                 RECORD(2)
!                    ...
!                 RECORD(1+DLSIZE):  An extended difference line.
!                                    The contents are:
!
!                    Dimension  Description
!                    ---------  ----------------------------------
!                    1          Reference epoch of difference line
!                    MAXDIM     Stepsize function vector
!                    1          Reference position vector,  x
!                    1          Reference velocity vector,  x
!                    1          Reference position vector,  y
!                    1          Reference velocity vector,  y
!                    1          Reference position vector,  z
!                    1          Reference velocity vector,  z
!                    MAXDIM,3   Modified divided difference
!                               arrays (MDAs)
!                    1          Maximum integration order plus 1
!                    3          Integration order array
!
! $ Detailed_Output
!
!     STATE    is the state resulting from evaluation of the input
!              record at ET. Units are km and km/sec.
!
! $ Parameters
!
!     MAXTRM   is the maximum number of terms allowed in
!              each component of the difference table
!              contained in the input argument RECORD.
!              See the INCLUDE file spk21.inc for the value
!              of MAXTRM.
!
! $ Exceptions
!
!     1)  If the maximum table size of the input record exceeds
!         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of type 21 (difference lines)
!     segments are described in the SPK Required Reading file.
!
!     SPKE21 is a modified version of SPKE01. The routine has been
!     generalized to support variable size difference lines.
!
! $ Examples
!
!     None.
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
!     F.T. Krogh         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB) (FTK) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     evaluate type_21 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Local variables
!
!     The names below are original to the routine. They correspond
!     roughly to the original memos written by Fred Krogh to explain
!     how all this stuff really works.
!
      DOUBLE PRECISION      TL
      DOUBLE PRECISION      G        ( MAXTRM )

      DOUBLE PRECISION      REFPOS   (    3 )
      DOUBLE PRECISION      REFVEL   (    3 )

      DOUBLE PRECISION      DT       ( MAXTRM, 3 )
      INTEGER               KQMAX1
      INTEGER               KQ       (    3 )

      DOUBLE PRECISION      FC       ( MAXTRM )


      DOUBLE PRECISION      SUM
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      TP
      DOUBLE PRECISION      WC       ( MAXTRM-1 )
      DOUBLE PRECISION      W        ( MAXTRM+2 )

      INTEGER               MAXDIM
      INTEGER               MQ2
      INTEGER               KS1
      INTEGER               KS
      INTEGER               KQQ
      INTEGER               I
      INTEGER               J
      INTEGER               JX

!
!     Save everything between calls.
!
      SAVE

!
!     Initial values
!
      DATA                  FC (  1 )    / 1.D0 /

!
!     Use discovery check-in.
!
!     If the RETURN function is set, don't even bother with this.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF
!
!     The first element of the input record is the dimension
!     of the difference table MAXDIM.
!
      MAXDIM = NINT ( RECORD(1) )

      IF ( MAXDIM .GT. MAXTRM ) THEN

         CALL CHKIN  ( 'SPKE21'                                )
         CALL SETMSG ( 'The input record has a maximum table ' &
         //            'dimension of #, while the maximum ' &
         //            'supported by this routine is #. It is ' &
         //            'possible that this problem is due to ' &
         //            'your SPICE Toolkit being out of date.' )
         CALL ERRINT ( '#',  MAXDIM                            )
         CALL ERRINT ( '#',  MAXTRM                            )
         CALL SIGERR ( 'SPICE(DIFFLINETOOLARGE)'               )
         CALL CHKOUT ( 'SPKE21'                                )
         RETURN

      END IF

!
!     Unpack the contents of the MDA array.
!
!        Name     Dimension  Description
!        ------   ---------  -------------------------------
!        TL               1  Reference epoch of record
!        G           MAXDIM  Stepsize function vector
!        REFPOS           3  Reference position vector
!        REFVEL           3  Reference velocity vector
!        DT      MAXDIM,NTE  Modified divided difference arrays
!        KQMAX1           1  Maximum integration order plus 1
!        KQ             NTE  Integration order array
!
!     For our purposes, NTE is always 3.
!

      CALL MOVED ( RECORD( 2),      1, TL )
      CALL MOVED ( RECORD( 3), MAXDIM, G  )

!
!     Collect the reference position and velocity.
!
      REFPOS(1) =  RECORD(MAXDIM+3)
      REFVEL(1) =  RECORD(MAXDIM+4)

      REFPOS(2) =  RECORD(MAXDIM+5)
      REFVEL(2) =  RECORD(MAXDIM+6)

      REFPOS(3) =  RECORD(MAXDIM+7)
      REFVEL(3) =  RECORD(MAXDIM+8)

!
!     Initializing the difference table is one aspect of this routine
!     that's a bit different from SPKE01. Here the first dimension of
!     the table in the input record can be smaller than MAXTRM. So, we
!     must transfer separately the portions of the table corresponding
!     to each component.
!
      DO I = 1, 3
         CALL MOVED ( RECORD(I*MAXDIM+9), MAXDIM, DT(1,I) )
      END DO

      KQMAX1    = INT ( RECORD(4*MAXDIM + 9 ) )
      KQ(1)     = INT ( RECORD(4*MAXDIM + 10) )
      KQ(2)     = INT ( RECORD(4*MAXDIM + 11) )
      KQ(3)     = INT ( RECORD(4*MAXDIM + 12) )

!
!     Next we set up for the computation of the various differences
!
      DELTA     = ET     - TL
      TP        = DELTA
      MQ2       = KQMAX1 - 2
      KS        = KQMAX1 - 1

!
!     This is clearly collecting some kind of coefficients.
!     The problem is that we have no idea what they are...
!
!     The G coefficients are supposed to be some kind of step size
!     vector.
!
!     TP starts out as the delta t between the request time and the
!     difference line's reference epoch. We then change it from DELTA
!     by the components of the stepsize vector G.
!
      DO J = 1, MQ2
!
!        Make sure we're not about to attempt division by zero.
!
         IF ( G(J) .EQ. 0.D0 ) THEN

            CALL CHKIN  ( 'SPKE21'                             )
            CALL SETMSG ( 'A  value of zero was found at ' &
            //            'index # of the step size vector.'   )
            CALL ERRINT ( '#',  J                              )
            CALL SIGERR ( 'SPICE(ZEROSTEP)'                    )
            CALL CHKOUT ( 'SPKE21'                             )
            RETURN

         END IF

         FC(J+1) = TP    / G(J)
         WC(J)   = DELTA / G(J)
         TP      = DELTA + G(J)

      END DO

!
!     Collect KQMAX1 reciprocals.
!
      DO J = 1, KQMAX1
         W(J) = 1.0D0/DBLE(J)
      END DO

!
!     Compute the W(K) terms needed for the position interpolation
!     (Note,  it is assumed throughout this routine that KS, which
!     starts out as KQMAX1-1 (the ``maximum integration'')
!     is at least 2.
!
      JX  = 0
      KS1 = KS - 1

      DO WHILE ( KS .GE. 2 )

         JX  = JX+1

         DO  J = 1, JX
            W(J+KS) = FC(J+1)*W(J+KS1) - WC(J)*W(J+KS)
         END DO

         KS  = KS1
         KS1 = KS1 - 1

      END DO

!
!     Perform position interpolation: (Note that KS = 1 right now.
!     We don't know much more than that.)
!
      DO  I = 1, 3

         KQQ = KQ(I)
         SUM = 0.0D0

         DO J = KQQ, 1, -1
            SUM = SUM + DT(J,I)*W(J+KS)
         END DO

         STATE(I) = REFPOS(I)  +  DELTA * ( REFVEL(I) + DELTA*SUM )

      END DO

!
!     Again we need to compute the W(K) coefficients that are
!     going to be used in the velocity interpolation.
!     (Note, at this point, KS = 1, KS1 = 0.)
!
      DO  J = 1, JX
         W(J+KS) = FC(J+1)*W(J+KS1) - WC(J)*W(J+KS)
      END DO

      KS = KS - 1

!
!     Perform velocity interpolation:
!
      DO  I = 1, 3

         KQQ = KQ(I)
         SUM = 0.0D0

         DO J = KQQ, 1, -1
            SUM = SUM + DT(J,I)*W(J+KS)
         END DO

         STATE(I+3) = REFVEL(I) + DELTA*SUM

      END DO

      RETURN
      END
