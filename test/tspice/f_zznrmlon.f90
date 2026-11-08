!$Procedure F_ZZNRMLON ( ZZNRMLON tests )

      SUBROUTINE F_ZZNRMLON ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZNRMLON.
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
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB routine ZZNRMLON.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 10-OCT-2016 (NJB)
!
!        Updated to treat as errors cases where
!
!           - the longitude bounds are equal
!
!           - the maximum bound is less than the minimum
!             bound and the bounds differ by an integer
!             multiple of 2*pi.
!
!-    TSPICE Version 1.0.0, 16-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


!     Local Variables
!
!
      DOUBLE PRECISION      ATOL
      DOUBLE PRECISION      INMAX
      DOUBLE PRECISION      INMIN
      DOUBLE PRECISION      OUTMAX
      DOUBLE PRECISION      OUTMIN
      DOUBLE PRECISION      SMALL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XOMAX
      DOUBLE PRECISION      XOMIN

!
!     Saved values
!
      SAVE

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZNRMLON' )





!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMIN = INMAX + <small>  < 0; ATOL = 0' )

      SMALL = 1.D-14

      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = INMIN - SMALL

      XOMIN = INMIN
      XOMAX = INMAX - SMALL + TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '-2 pi < INMIN < INMAX < -1.5 pi; ATOL = 0' )

      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = -1.6D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect equality.
!
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '-2 pi < INMIN < INMAX < -1.5 pi; ATOL > 0' )

      ATOL  =  1.D-13
      INMIN = -1.7D0 * PI()
      INMAX = -1.6D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect equality.
!
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '1.5 pi < INMIN, INMAX < 2 pi; ATOL = 0' )

      ATOL  = 0.D0
      INMIN = 1.6D0 * PI()
      INMAX = 1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect equality.
!
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '1.5 pi < INMIN, INMAX < 2 pi; ATOL > 0' )

      ATOL  = 0.D0
      INMIN = 1.6D0 * PI()
      INMAX = 1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect equality.
!
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '1.5 pi < INMIN = INMAX - <small> ' &
      //           ' < 2 pi; ATOL = 0' )

      SMALL = 1.D-14

      ATOL  = 0.D0
      INMIN = 1.7D0 * PI()
      INMAX = INMIN - SMALL

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '-2 pi < INMIN < INMAX < INMIN + ATOL < ' &
      //           '-1.5 pi; ATOL > 0' )
!
!     The upper bound exceeds the lower bound by less than
!     the input tolerance. The upper bound is negative.
!     The upper bound should be shifted.
!
      ATOL  = 1.D-13
      INMIN = -1.7D0 * PI()
      INMAX = INMIN + ( ATOL / 10 )

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '0 < INMIN < INMAX < INMIN + ATOL < ' &
      //           '1.5 pi; ATOL > 0' )
!
!     The upper bound exceeds the lower bound by less than
!     the input tolerance. The lower bound is positive.
!     The lower bound should be shifted.
!
      ATOL  = 1.D-13
      INMIN = 1.D-13
      INMAX = INMIN + ( ATOL / 10 )

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '-2 pi < INMIN < INMAX < INMIN + ATOL < ' &
      //           '-1.5 pi; ATOL > 0' )
!
!     The upper bound exceeds the lower bound by a little more than
!     the input tolerance.
!
      ATOL  = 1.D-13
      INMIN = -1.7D0 * PI()
      INMAX = INMIN + ( 1.1D0*ATOL  )

      XOMIN = INMIN
      XOMAX = INMAX

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact agreement.
!
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX < INMIN; INMAX < 0; |INMAX-INMIN| < 2pi; ' &
      //           'ATOL = 0'                                       )
!
!     The bounds are out of order. The magnitude of the difference
!     is less than 2*pi. Tolerance is 0.
!
      ATOL  =  0.D0
      INMIN =  0.D0
      INMAX =  -1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX < INMIN; INMAX < 0; |INMAX-INMIN| < 2pi; ' &
      //           'ATOL > 0'                                       )
!
!     The bounds are out of order. The upper bound is negative. The
!     magnitude of the difference is less than 2*pi. Tolerance is > 0.
!

      ATOL  =  1.D-13
      INMIN =  0.D0
      INMAX =  -1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX < INMIN; INMAX > 0; |INMAX-INMIN| < 2pi; ' &
      //           'ATOL > 0'                                       )
!
!     The bounds are out of order. The upper bound is positive. The
!     magnitude of the difference is less than 2*pi. Tolerance is > 0.
!
!     The lower bound should be shifted left.
!
      ATOL  =  1.D-13
      INMIN =  1.8D0 * PI()
      INMAX =  0.1D0 * PI()

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX < INMIN; INMAX < 0; |INMAX-INMIN| > 2pi; ' &
      //           'ATOL = 0.'                                      )

!
!     This case requires both bounds to be shifted.
!
      ATOL  =   0.D0
      INMIN =   1.D0  * PI()
      INMAX =  -1.7D0 * PI()

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMIN + 2*pi < INMAX; ATOL = 0' )

!
!     This case requires the upper bound to be shifted.
!
      ATOL  =   0.D0
      INMIN =  -1.D0  * PI()
      INMAX =   1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX - TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMIN + 2*pi + ATOL < INMAX; ATOL > 0' )

!
!     This case requires the upper bound to be shifted.
!
      ATOL  =   1.D-13
      INMIN =  -1.D0  * PI()
      INMAX =   INMIN + TWOPI() + 1.1D0*ATOL

      XOMIN = INMIN
      XOMAX = INMAX - TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMIN + 2*pi < INMAX < INMIN + 2*pi + ' &
      //           'ATOL; ATOL > 0'                        )

!
!     This case requires the upper bound to be left as is.
!
      ATOL  =   1.D-13
      INMIN =  -1.D0  * PI()
      INMAX =   INMIN + TWOPI() + 0.5D0*ATOL

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '-2pi - ATOL < INMIN; ATOL > 0.' )

!
!     This is a ridiculously large tolerance, but it is supposed
!     to be accepted.
!
      ATOL  =   1.D0

      INMIN =  -TWOPI() - ATOL + 1.D-13

      INMAX =   INMIN + TWOPI()

      XOMIN = -TWOPI()
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX < 2pi + ATOL; ATOL > 0.' )

!
!     This is a ridiculously large tolerance, but it is supposed
!     to be accepted.
!
      ATOL  =   1.D0

      INMAX =   TWOPI() + ATOL -1.D-13

      INMIN =   INMAX - TWOPI()

      XOMIN = INMIN
      XOMAX = TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '-2pi - ATOL < INMIN; INMAX < INMIN; ATOL > 0.' )

!
!     This is a ridiculously large tolerance, but it is supposed
!     to be accepted.
!
      ATOL  =   1.D0

      INMIN =  -TWOPI() - ATOL + 1.D-13

      INMAX =   INMIN - 1.D-14

      XOMIN = -TWOPI()
      XOMAX = XOMIN + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX < 2pi + ATOL; ATOL > 0.' )

!
!     This is a ridiculously large tolerance, but it is supposed
!     to be accepted.
!
      ATOL  =   1.D0

      INMAX =   TWOPI() + ATOL -1.D-13

      INMIN =   INMAX + 1.D-14

      XOMAX = TWOPI()
      XOMIN = XOMAX - TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect tight agreement.
!
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


!***********************************************************************
!
!     Error cases
!
!***********************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Negative tolerance.' )

      ATOL  =  -1.D-13
      INMIN =  -1.D0  * PI()
      INMAX =   INMIN + TWOPI()

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMIN < -2pi - ATOL' )

      ATOL  =   1.D-13
      INMIN =  -TWOPI() - 1.1D0*ATOL
      INMAX =   INMIN + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMIN >  2pi + ATOL' )

      ATOL  =   1.D-13
      INMIN =   TWOPI() +1.1D0*ATOL
      INMAX =   TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX < -2pi - ATOL' )

      ATOL  =   1.D-13
      INMAX =  -TWOPI() - 1.1D0*ATOL
      INMAX =   INMIN + TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMAX >  2pi + ATOL' )

      ATOL  =   1.D-13
      INMAX =   TWOPI() +1.1D0*ATOL
      INMIN =   TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INMIN = INMAX < 0; ATOL = 0' )

      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = INMIN

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( '-2 pi < INMIN = INMAX < -1.5 pi; ATOL = 0' )

      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = INMIN

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )

!
! --- Case: ------------------------------------------------------
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


