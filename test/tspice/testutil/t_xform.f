C$Procedure T_XFORM ( Test state transformation )

      SUBROUTINE T_XFORM ( XFORM,  RMINUS, RPLUS,  DELTA, 
     .                     NRMERR, DETERR, DRVERR, DRLERR, DRDIFF )
      IMPLICIT NONE

C$ Abstract
C
C     Perform basic tests on a state transformation matrix:  verify
C     that the diagonal blocks contain matching rotations; check
C     the derivative block against a discrete derivative obtained
C     from input rotations.
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
C     MATH
C     UTILITY
C
C$ Declarations

      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      RMINUS ( 3, 3 )
      DOUBLE PRECISION      RPLUS  ( 3, 3 ) 
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      NRMERR
      DOUBLE PRECISION      DETERR
      DOUBLE PRECISION      DRVERR
      DOUBLE PRECISION      DRLERR
      DOUBLE PRECISION      DRDIFF ( 3, 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     XFORM      I   State transformation matrix.
C     RMINUS     I   Left side rotation for discrete derivative.
C     RPLUS      I   Right side rotation for discrete derivative.
C     DELTA      I   Abscissa separation for discrete derivative.
C     NRMERR     O   Maximum row and column norm error.
C     DETERR     O   Determinant error.
C     DRVERR     O   Derivative L2 (RMS) error.
C     DRLERR     O   Derivative relative error.
C     DRDIFF     O   Derivative difference.
C
C$ Detailed_Input
C
C     XFORM          is a time-dependent state transformation matrix to
C                    be tested. For the purposes of discussing the
C                    discrete derivative used in tests done by this
C                    routine, we'll consider XFORM to correspond to the
C                    epoch T0 second past J2000 TDB.
C                
C     RMINUS         is a rotation matrix representing the value of
C                    XFORM's rotation blocks at time T0-DELTA.
C                  
C     RPLUS          is a rotation matrix representing the value of
C                    XFORM's rotation blocks at time T0+DELTA.
C
C     DELTA          is an interval separating abscissa values used
C                    for discrete differentiation.  DELTA is expressed
C                    in TDB seconds.
C
C$ Detailed_Output
C
C     NRMERR         is the maximum error of the norm of any row or
C                    column of the rotation blocks of XFORM.  Each
C                    such norm should be 1.
C
C     DETERR         is the error of the determinant of the upper
C                    left block of XFORM.  The determinant should be 1.
C
C     DRVERR         is the error of the derivative block of XFORM.
C                    This error is the L2 norm (RMS error) of the
C                    difference between the derivative block of XFORM
C                    and a discrete derivative computed from RPLUS,
C                    RMINUS, and DELTA.
C
C                    The discrete derivative is computed as follows:
C
C                       1) The derivative each component of the
C                          upper left block of XFORM is computed
C                          using QDERIV.
C
C                       2) Each row of the discrete derivative is
C                          "sharpened" by removing its radial component
C                          relative to the corresponding row of XFORM.
C                          This is done by making each of row the upper
C                          left block and its corresponding discrete
C                          derivative into a state vector and passing
C                          this state through DVHAT.  The velocity
C                          portion of the state returned by DVHAT
C                          becomes a row of the sharpened discrete
C                          derivative.
C
C     DRLERR         is the relative error between the derivative
C                    block of XFORM and the discrete derivative.
C                    This is computed using VRELG.
C
C     DRDIFF         is the matrix difference between the derivative
C                    block of XFORM and the discrete derivative.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C    1) If the rotation (main diagonal) blocks of XFORM don't match
C       exactly, the error SPICE(INVALIDMATRIX) is signaled.
C
C    2) If a routine called by this routine signals an error,
C       this routine simply returns.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is used to perform sanity checks on state 
C     transformations.
C
C$ Examples
C
C     See use in test family F_DYN01.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 21-DEC-2004 (NJB) 
C
C-&

C$ Index_Entries
C
C       Compute multidimensional array index from offset
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DET 
      DOUBLE PRECISION      VDISTG
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VRELG

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local variables
C
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

C
C     Extract the 3x3 blocks of the input state
C     transformation matrix.
C
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

C
C     The upper left and lower right blocks should be identical.
C
      BLERR  =  VDISTG( ULBLCK, LRBLCK, 9 )

      IF ( BLERR .NE. 0.D0 ) THEN

         CALL SETMSG ( 'L2 distance between blocks on main diagonal ' //
     .                 'is #; this distance must be zero.'            )
         CALL ERRDP  ( '#', BLERR                                     )
         CALL SIGERR ( 'SPICE(INVALIDMATRIX)'                         )
         CALL CHKOUT ( 'T_XFORM'                                      )
         RETURN

      END IF

C
C     Start out by finding the norms of rows and columns in
C     the upper left block.
C
      NRMERR = 0.D0

      DO I = 1, 3
         NRMERR = MAX (  NRMERR,  ABS( 1.D0 - VNORM( ULBLCK(1,I) ) )  )
      END DO

      CALL XPOSE ( ULBLCK, XPBLCK )

      DO I = 1, 3
         NRMERR = MAX (  NRMERR,  ABS( 1.D0 - VNORM( XPBLCK(1,I) ) )  )
      END DO

C
C     Find the determinant error of the upper left block.
C
      DETERR = ABS (  1.D0 - DET ( ULBLCK )  )

C
C     Compute a discrete derivative of the upper left block using
C     the matrices corresponding to time offsets of +/- DELTA.
C
      CALL QDERIV ( 9, RMINUS, RPLUS, DELTA, DSCRET )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_XFORM' )
         RETURN
      END IF
C
C     "Sharpen" the discrete derivative:  we know the rows of the
C     discrete derivative matrix should be orthogonal to those of the
C     corresponding rows of the rotation ULBLCK.  We'll find it
C     easier to work with columns of matrices, so let XPD be the
C     transpose of the discrete derivative.
C
      CALL XPOSE ( DSCRET, XPD )

      DO I = 1, 3 
C
C        XPBLCK contains the transpose of XFORM's upper left rotation
C        block.  Make a state vector out of the Ith column of XPBLCK
C        and the corresponding discrete derivative.  Find the state of
C        corresponding unit vector; store the velocity of the unit
C        vector in the Ith column of DSCRTR.
C         
         CALL VEQU  ( XPBLCK(1,I),  RSTATE      )
         CALL VEQU  ( XPD   (1,I),  RSTATE(4)   )
         CALL DVHAT ( RSTATE,       DSHARP      )
         CALL VEQU  ( DSHARP(4),    DSCRTR(1,I) )

      END DO

C
C     Replace the discrete derivative with the sharpened version
C     we just computed.
C     
      CALL XPOSE ( DSCRTR, DSCRET )

C
C     Find the L2 error between the discrete derivative and the
C     derivative block of XFORM.  Also find the relative error.
C
      DRVERR = VDISTG ( DSCRET, DRBLCK, 9 )
      DRLERR = VRELG  ( DSCRET, DRBLCK, 9 )
 
C
C     Find the matrix difference of the derivatives.
C
      CALL VSUBG ( DSCRET, DRBLCK, 9, DRDIFF )

      CALL CHKOUT ( 'T_XFORM' )
      RETURN
      END




