!$Procedure T_XFORM ( Test state transformation )

      SUBROUTINE T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA, &
                           NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Perform basic tests on a state transformation matrix:  verify
!     that the diagonal blocks contain matching rotations; check
!     the derivative block against a discrete derivative obtained
!     from input rotations.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     UTILITY
!
! $ Declarations

      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      RMINUS ( 3, 3 )
      DOUBLE PRECISION      RPLUS  ( 3, 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      NRMERR
      DOUBLE PRECISION      DETERR
      DOUBLE PRECISION      DRVERR
      DOUBLE PRECISION      DRLERR
      DOUBLE PRECISION      DRDIFF ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     XFORM      I   State transformation matrix.
!     RMINUS     I   Left side rotation for discrete derivative.
!     RPLUS      I   Right side rotation for discrete derivative.
!     DELTA      I   Abscissa separation for discrete derivative.
!     NRMERR     O   Maximum row and column norm error.
!     DETERR     O   Determinant error.
!     DRVERR     O   Derivative L2 (RMS) error.
!     DRLERR     O   Derivative relative error.
!     DRDIFF     O   Derivative difference.
!
! $ Detailed_Input
!
!     XFORM          is a time-dependent state transformation matrix to
!                    be tested. For the purposes of discussing the
!                    discrete derivative used in tests done by this
!                    routine, we'll consider XFORM to correspond to the
!                    epoch T0 second past J2000 TDB.
!
!     RMINUS         is a rotation matrix representing the value of
!                    XFORM's rotation blocks at time T0-DELTA.
!
!     RPLUS          is a rotation matrix representing the value of
!                    XFORM's rotation blocks at time T0+DELTA.
!
!     DELTA          is an interval separating abscissa values used
!                    for discrete differentiation.  DELTA is expressed
!                    in TDB seconds.
!
! $ Detailed_Output
!
!     NRMERR         is the maximum error of the norm of any row or
!                    column of the rotation blocks of XFORM.  Each
!                    such norm should be 1.
!
!     DETERR         is the error of the determinant of the upper
!                    left block of XFORM.  The determinant should be 1.
!
!     DRVERR         is the error of the derivative block of XFORM.
!                    This error is the L2 norm (RMS error) of the
!                    difference between the derivative block of XFORM
!                    and a discrete derivative computed from RPLUS,
!                    RMINUS, and DELTA.
!
!                    The discrete derivative is computed as follows:
!
!                       1) The derivative each component of the
!                          upper left block of XFORM is computed
!                          using QDERIV.
!
!                       2) Each row of the discrete derivative is
!                          "sharpened" by removing its radial component
!                          relative to the corresponding row of XFORM.
!                          This is done by making each of row the upper
!                          left block and its corresponding discrete
!                          derivative into a state vector and passing
!                          this state through DVHAT.  The velocity
!                          portion of the state returned by DVHAT
!                          becomes a row of the sharpened discrete
!                          derivative.
!
!     DRLERR         is the relative error between the derivative
!                    block of XFORM and the discrete derivative.
!                    This is computed using VRELG.
!
!     DRDIFF         is the matrix difference between the derivative
!                    block of XFORM and the discrete derivative.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!    1) If the rotation (main diagonal) blocks of XFORM don't match
!       exactly, the error SPICE(INVALIDMATRIX) is signaled.
!
!    2) If a routine called by this routine signals an error,
!       this routine simply returns.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used to perform sanity checks on state
!     transformations.
!
! $ Examples
!
!     See use in test family F_DYN01.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 21-DEC-2004 (NJB)
!
!-&

! $ Index_Entries
!
!       Compute multidimensional array index from offset
!
!-&


!
!     SPICELIB functions
!


!
!     Local variables
!
      DOUBLE PRECISION      BLERR
      DOUBLE PRECISION      DRBLCK ( 3, 3 )
      DOUBLE PRECISION      DSCRET ( 3, 3 )
      DOUBLE PRECISION      DSCRTR ( 3, 3 )
      DOUBLE PRECISION      DSHARP ( 6 )
      DOUBLE PRECISION      LRBLCK ( 3, 3 )
      DOUBLE PRECISION      RSTATE ( 6 )
      DOUBLE PRECISION      ULBLCK ( 3, 3 )
      DOUBLE PRECISION      URBLCK ( 3, 3 )
      DOUBLE PRECISION      XPBLCK ( 3, 3 )
      DOUBLE PRECISION      XPD    ( 3, 3 )

      INTEGER               I


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_XFORM' )

!
!     Extract the 3x3 blocks of the input state
!     transformation matrix.
!
      CALL MOVED ( XFORM(1,1), 3, ULBLCK(1,1) )
      CALL MOVED ( XFORM(1,2), 3, ULBLCK(1,2) )
      CALL MOVED ( XFORM(1,3), 3, ULBLCK(1,3) )

      CALL MOVED ( XFORM(1,4), 3, URBLCK(1,1) )
      CALL MOVED ( XFORM(1,5), 3, URBLCK(1,2) )
      CALL MOVED ( XFORM(1,6), 3, URBLCK(1,3) )

      CALL MOVED ( XFORM(4,1), 3, DRBLCK(1,1) )
      CALL MOVED ( XFORM(4,2), 3, DRBLCK(1,2) )
      CALL MOVED ( XFORM(4,3), 3, DRBLCK(1,3) )

      CALL MOVED ( XFORM(4,4), 3, LRBLCK(1,1) )
      CALL MOVED ( XFORM(4,5), 3, LRBLCK(1,2) )
      CALL MOVED ( XFORM(4,6), 3, LRBLCK(1,3) )

!
!     The upper left and lower right blocks should be identical.
!
      BLERR  =  VDISTG( ULBLCK, LRBLCK, 9 )

      IF ( BLERR .NE. 0.D0 ) THEN

         CALL SETMSG ( 'L2 distance between blocks on main diagonal ' // &
                       'is #; this distance must be zero.'            )
         CALL ERRDP  ( '#', BLERR                                     )
         CALL SIGERR ( 'SPICE(INVALIDMATRIX)'                         )
         CALL CHKOUT ( 'T_XFORM'                                      )
         RETURN

      END IF

!
!     Start out by finding the norms of rows and columns in
!     the upper left block.
!
      NRMERR = 0.D0

      DO I = 1, 3
         NRMERR = MAX (  NRMERR,  ABS( 1.D0 - VNORM( ULBLCK(1,I) ) )  )
      END DO

      CALL XPOSE ( ULBLCK, XPBLCK )

      DO I = 1, 3
         NRMERR = MAX (  NRMERR,  ABS( 1.D0 - VNORM( XPBLCK(1,I) ) )  )
      END DO

!
!     Find the determinant error of the upper left block.
!
      DETERR = ABS (  1.D0 - DET ( ULBLCK )  )

!
!     Compute a discrete derivative of the upper left block using
!     the matrices corresponding to time offsets of +/- DELTA.
!
      CALL QDERIV ( 9, RMINUS, RPLUS, DELTA, DSCRET )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_XFORM' )
         RETURN
      END IF
!
!     "Sharpen" the discrete derivative:  we know the rows of the
!     discrete derivative matrix should be orthogonal to those of the
!     corresponding rows of the rotation ULBLCK.  We'll find it
!     easier to work with columns of matrices, so let XPD be the
!     transpose of the discrete derivative.
!
      CALL XPOSE ( DSCRET, XPD )

      DO I = 1, 3
!
!        XPBLCK contains the transpose of XFORM's upper left rotation
!        block.  Make a state vector out of the Ith column of XPBLCK
!        and the corresponding discrete derivative.  Find the state of
!        corresponding unit vector; store the velocity of the unit
!        vector in the Ith column of DSCRTR.
!
         CALL VEQU  ( XPBLCK(1,I),  RSTATE      )
         CALL VEQU  ( XPD   (1,I),  RSTATE(4)   )
         CALL DVHAT ( RSTATE,       DSHARP      )
         CALL VEQU  ( DSHARP(4),    DSCRTR(1,I) )

      END DO

!
!     Replace the discrete derivative with the sharpened version
!     we just computed.
!
      CALL XPOSE ( DSCRTR, DSCRET )

!
!     Find the L2 error between the discrete derivative and the
!     derivative block of XFORM.  Also find the relative error.
!
      DRVERR = VDISTG ( DSCRET, DRBLCK, 9 )
      DRLERR = VRELG  ( DSCRET, DRBLCK, 9 )

!
!     Find the matrix difference of the derivatives.
!
      CALL VSUBG ( DSCRET, DRBLCK, 9, DRDIFF )

      CALL CHKOUT ( 'T_XFORM' )
      RETURN
      END




