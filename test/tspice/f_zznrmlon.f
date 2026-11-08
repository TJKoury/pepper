C$Procedure F_ZZNRMLON ( ZZNRMLON tests )
 
      SUBROUTINE F_ZZNRMLON ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZNRMLON.
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
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB routine ZZNRMLON.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 10-OCT-2016 (NJB)
C
C        Updated to treat as errors cases where
C
C           - the longitude bounds are equal
C
C           - the maximum bound is less than the minimum
C             bound and the bounds differ by an integer
C             multiple of 2*pi.
C
C-    TSPICE Version 1.0.0, 16-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


C     Local Variables
C
C
      DOUBLE PRECISION      ATOL
      DOUBLE PRECISION      INMAX
      DOUBLE PRECISION      INMIN
      DOUBLE PRECISION      OUTMAX
      DOUBLE PRECISION      OUTMIN
      DOUBLE PRECISION      SMALL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XOMAX
      DOUBLE PRECISION      XOMIN
 
C
C     Saved values
C
      SAVE

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZNRMLON' )





C***********************************************************************
C
C     Normal cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMIN = INMAX + <small>  < 0; ATOL = 0' )
 
      SMALL = 1.D-14

      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = INMIN - SMALL

      XOMIN = INMIN
      XOMAX = INMAX - SMALL + TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '-2 pi < INMIN < INMAX < -1.5 pi; ATOL = 0' )
 
      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = -1.6D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect equality.
C
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '-2 pi < INMIN < INMAX < -1.5 pi; ATOL > 0' )
 
      ATOL  =  1.D-13
      INMIN = -1.7D0 * PI()
      INMAX = -1.6D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect equality.
C
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '1.5 pi < INMIN, INMAX < 2 pi; ATOL = 0' )
 
      ATOL  = 0.D0
      INMIN = 1.6D0 * PI()
      INMAX = 1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect equality.
C
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '1.5 pi < INMIN, INMAX < 2 pi; ATOL > 0' )
 
      ATOL  = 0.D0
      INMIN = 1.6D0 * PI()
      INMAX = 1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect equality.
C
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '1.5 pi < INMIN = INMAX - <small> '
     .//           ' < 2 pi; ATOL = 0' )
 
      SMALL = 1.D-14

      ATOL  = 0.D0
      INMIN = 1.7D0 * PI()
      INMAX = INMIN - SMALL

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX 


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '-2 pi < INMIN < INMAX < INMIN + ATOL < '
     .//           '-1.5 pi; ATOL > 0' )
C
C     The upper bound exceeds the lower bound by less than
C     the input tolerance. The upper bound is negative.
C     The upper bound should be shifted.
C
      ATOL  = 1.D-13
      INMIN = -1.7D0 * PI()
      INMAX = INMIN + ( ATOL / 10 )

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '0 < INMIN < INMAX < INMIN + ATOL < '
     .//           '1.5 pi; ATOL > 0' )
C
C     The upper bound exceeds the lower bound by less than
C     the input tolerance. The lower bound is positive.
C     The lower bound should be shifted.
C
      ATOL  = 1.D-13
      INMIN = 1.D-13
      INMAX = INMIN + ( ATOL / 10 )

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX 

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '-2 pi < INMIN < INMAX < INMIN + ATOL < '
     .//           '-1.5 pi; ATOL > 0' )
C
C     The upper bound exceeds the lower bound by a little more than
C     the input tolerance.
C
      ATOL  = 1.D-13
      INMIN = -1.7D0 * PI()
      INMAX = INMIN + ( 1.1D0*ATOL  )

      XOMIN = INMIN
      XOMAX = INMAX 

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact agreement.
C
      TOL   = 0.D0

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '=', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '=', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX < INMIN; INMAX < 0; |INMAX-INMIN| < 2pi; '
     .//           'ATOL = 0'                                       )
C
C     The bounds are out of order. The magnitude of the difference 
C     is less than 2*pi. Tolerance is 0.
C 
      ATOL  =  0.D0
      INMIN =  0.D0
      INMAX =  -1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX < INMIN; INMAX < 0; |INMAX-INMIN| < 2pi; '
     .//           'ATOL > 0'                                       )
C
C     The bounds are out of order. The upper bound is negative. The
C     magnitude of the difference is less than 2*pi. Tolerance is > 0.
C 
 
      ATOL  =  1.D-13
      INMIN =  0.D0
      INMAX =  -1.7D0 * PI()

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX < INMIN; INMAX > 0; |INMAX-INMIN| < 2pi; '
     .//           'ATOL > 0'                                       )
C
C     The bounds are out of order. The upper bound is positive. The
C     magnitude of the difference is less than 2*pi. Tolerance is > 0.
C 
C     The lower bound should be shifted left.
C
      ATOL  =  1.D-13
      INMIN =  1.8D0 * PI()
      INMAX =  0.1D0 * PI()

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX 


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX < INMIN; INMAX < 0; |INMAX-INMIN| > 2pi; '
     .//           'ATOL = 0.'                                      )
 
C
C     This case requires both bounds to be shifted.
C
      ATOL  =   0.D0
      INMIN =   1.D0  * PI()
      INMAX =  -1.7D0 * PI()

      XOMIN = INMIN - TWOPI()
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMIN + 2*pi < INMAX; ATOL = 0' )
 
C
C     This case requires the upper bound to be shifted.
C
      ATOL  =   0.D0
      INMIN =  -1.D0  * PI()
      INMAX =   1.7D0 * PI()

      XOMIN = INMIN 
      XOMAX = INMAX - TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMIN + 2*pi + ATOL < INMAX; ATOL > 0' )
 
C
C     This case requires the upper bound to be shifted.
C
      ATOL  =   1.D-13
      INMIN =  -1.D0  * PI()
      INMAX =   INMIN + TWOPI() + 1.1D0*ATOL

      XOMIN = INMIN 
      XOMAX = INMAX - TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMIN + 2*pi < INMAX < INMIN + 2*pi + '
     .//           'ATOL; ATOL > 0'                        )
 
C
C     This case requires the upper bound to be left as is.
C
      ATOL  =   1.D-13
      INMIN =  -1.D0  * PI()
      INMAX =   INMIN + TWOPI() + 0.5D0*ATOL

      XOMIN = INMIN 
      XOMAX = INMAX 


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '-2pi - ATOL < INMIN; ATOL > 0.' )

C
C     This is a ridiculously large tolerance, but it is supposed
C     to be accepted.
C
      ATOL  =   1.D0

      INMIN =  -TWOPI() - ATOL + 1.D-13

      INMAX =   INMIN + TWOPI()

      XOMIN = -TWOPI()
      XOMAX = INMAX 


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX < 2pi + ATOL; ATOL > 0.' )

C
C     This is a ridiculously large tolerance, but it is supposed
C     to be accepted.
C
      ATOL  =   1.D0

      INMAX =   TWOPI() + ATOL -1.D-13

      INMIN =   INMAX - TWOPI()

      XOMIN = INMIN
      XOMAX = TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '-2pi - ATOL < INMIN; INMAX < INMIN; ATOL > 0.' )

C
C     This is a ridiculously large tolerance, but it is supposed
C     to be accepted.
C
      ATOL  =   1.D0

      INMIN =  -TWOPI() - ATOL + 1.D-13

      INMAX =   INMIN - 1.D-14

      XOMIN = -TWOPI()
      XOMAX = XOMIN + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX < 2pi + ATOL; ATOL > 0.' )

C
C     This is a ridiculously large tolerance, but it is supposed
C     to be accepted.
C
      ATOL  =   1.D0

      INMAX =   TWOPI() + ATOL -1.D-13

      INMIN =   INMAX + 1.D-14

      XOMAX = TWOPI()
      XOMIN = XOMAX - TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect tight agreement.
C
      TOL   = VTIGHT

      CALL CHCKSD ( 'OUTMIN', OUTMIN, '~', XOMIN, TOL, OK )
      CALL CHCKSD ( 'OUTMAX', OUTMAX, '~', XOMAX, TOL, OK )


C***********************************************************************
C
C     Error cases
C
C***********************************************************************



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Negative tolerance.' )

      ATOL  =  -1.D-13
      INMIN =  -1.D0  * PI()
      INMAX =   INMIN + TWOPI() 

      XOMIN = INMIN 
      XOMAX = INMAX 


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMIN < -2pi - ATOL' )

      ATOL  =   1.D-13
      INMIN =  -TWOPI() - 1.1D0*ATOL
      INMAX =   INMIN + TWOPI() 


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMIN >  2pi + ATOL' )

      ATOL  =   1.D-13
      INMIN =   TWOPI() +1.1D0*ATOL
      INMAX =   TWOPI() 
 
      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX < -2pi - ATOL' )

      ATOL  =   1.D-13
      INMAX =  -TWOPI() - 1.1D0*ATOL
      INMAX =   INMIN + TWOPI() 
 
      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMAX >  2pi + ATOL' )

      ATOL  =   1.D-13
      INMAX =   TWOPI() +1.1D0*ATOL
      INMIN =   TWOPI() 
 
      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INMIN = INMAX < 0; ATOL = 0' )
 
      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = INMIN

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()

      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( '-2 pi < INMIN = INMAX < -1.5 pi; ATOL = 0' )
 
      ATOL  = 0.D0
      INMIN = -1.7D0 * PI()
      INMAX = INMIN

      XOMIN = INMIN
      XOMAX = INMAX + TWOPI()


      CALL ZZNRMLON ( INMIN, INMAX, ATOL, OUTMIN, OUTMAX )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )

C
C --- Case: ------------------------------------------------------
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


