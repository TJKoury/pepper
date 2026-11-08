C$Procedure      F_ZZCKE06 ( Family of tests for ZZCKE06 )

      SUBROUTINE F_ZZCKE06 ( OK )

C$ Abstract
C
C     This test family exercises ZZCKE06.
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

      INCLUDE 'ckparam.inc'
      INCLUDE 'ck06.inc'

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
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the CK type 6 quaternion state
C     evaluator ZZCKE06.
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
C-    TSPICE Version 1.1.0, 11-OCT-2021 (NJB)
C
C        Initialized CLKBUF in all divide-by-zero error test cases.
C
C-    TSPICE Version 1.0.0, 07-AUG-2015 (NJB)
C
C-&



C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VDOTG

C
C     Local parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 240 )

      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 10000 )

      INTEGER               MXPKSZ
      PARAMETER           ( MXPKSZ = C06MXZ )

      INTEGER               RECSIZ
      PARAMETER           ( RECSIZ = CKMRSZ )

      INTEGER               PBUFSZ
      PARAMETER           ( PBUFSZ = MXPKSZ * MAXREC )

C
C     Local Variables
C
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      ANGLE0
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CLKBUF ( MAXREC )
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      DAV    ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DQ     ( 0 : 3 )
      DOUBLE PRECISION      PACKTS ( PBUFSZ )
      DOUBLE PRECISION      PREVQ  ( 0 : 3 )
      DOUBLE PRECISION      QANGLE
      DOUBLE PRECISION      QAXIS  ( 3 )
      DOUBLE PRECISION      QMAT   ( 3, 3 )
      DOUBLE PRECISION      QQANGL
      DOUBLE PRECISION      QSTATE ( 8 )
      DOUBLE PRECISION      QUAT   ( 0 : 3 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      REC2   ( RECSIZ )
      DOUBLE PRECISION      RECORD ( RECSIZ )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      SPAN
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      W
      DOUBLE PRECISION      XAV    ( 3 )
      DOUBLE PRECISION      XSCLK
      DOUBLE PRECISION      XCMAT  ( 3, 3 )
      DOUBLE PRECISION      XQUAT  ( 0 : 3 )

      INTEGER               DEGREE
      INTEGER               I
      INTEGER               NSAMP
      INTEGER               PKTSIZ
      INTEGER               PTR
      INTEGER               SIZE
      INTEGER               SUBTYP
      INTEGER               WINSIZ

C
C     Saved variables
C
C     Save all variables to avoid CSPICE stack overflow
C     problems.
C
      SAVE

C
C     Initial values
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN  ( 'F_ZZCKE06' )




C**********************************************************************
C
C
C     Error test cases
C
C
C**********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Invalid subtype' )

      CALL CLEARD ( RECSIZ, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SUBTYP = -1
      WINSIZ = 11
      SCLKDP = 0.D0
      RATE   = 1.D0

      RECORD(1) = SCLKDP
      RECORD(2) = DBLE( SUBTYP )
      RECORD(3) = DBLE( WINSIZ )
      RECORD(4) = RATE

      CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Hermite subtype with invalid quaternion sequence' )


      SCLKDP = 0.D0
      TDELTA = 1.D4
      RATE   = 1.D-4

      SUBTYP = 0
      DEGREE = 11
      WINSIZ = ( DEGREE + 1 ) / 2
      PKTSIZ = C06PS0


      RECORD(1) = SCLKDP
      RECORD(2) = DBLE( SUBTYP )
      RECORD(3) = DBLE( WINSIZ )
      RECORD(4) = RATE

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2

         CALL AXISAR ( AXIS, -ANGLE, CMAT )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )


         IF ( I .EQ. 3 ) THEN
            QUAT(0) = -C
         ELSE
            QUAT(0) =  C
         END IF

         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

C
C        Compute the quaternion derivative with respect
C        to TDB; store it after the quaternion.
C
C        As in the previous case, the angular rate
C        in radians/second is
C
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

      END DO


      CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

      PTR = PKTSIZ*WINSIZ + 5

      CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

      CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Subtype 0 divide-by zero case' )


      SCLKDP = 0.D0
      TDELTA = 1.D4
      RATE   = 1.D-4

      SUBTYP = 0
      DEGREE = 11
      WINSIZ = ( DEGREE + 1 ) / 2
      PKTSIZ = C06PS0

      CALL CLEARD ( RECSIZ, RECORD )
      CALL CLEARD ( PBUFSZ, PACKTS )

      RECORD(1) = SCLKDP
      RECORD(2) = DBLE( SUBTYP )
      RECORD(3) = DBLE( WINSIZ )
      RECORD(4) = RATE

      CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

      PTR = PKTSIZ*WINSIZ + 5

      DO I = 1, WINSIZ
         CLKBUF(I) = I * TDELTA
      END DO

      CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

      CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Subtype 1 divide-by zero case' )


      SCLKDP = 0.D0
      TDELTA = 1.D4
      RATE   = 1.D-4

      SUBTYP = 1
      DEGREE = 11
      WINSIZ = DEGREE + 1
      PKTSIZ = C06PS1

      CALL CLEARD ( RECSIZ, RECORD )
      CALL CLEARD ( PBUFSZ, PACKTS )

      RECORD(1) = SCLKDP
      RECORD(2) = DBLE( SUBTYP )
      RECORD(3) = DBLE( WINSIZ )
      RECORD(4) = RATE

      CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

      PTR = PKTSIZ*WINSIZ + 5

      DO I = 1, WINSIZ
         CLKBUF(I) = I * TDELTA
      END DO

      CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

      CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Subtype 2 divide-by zero case' )


      SCLKDP = 0.D0
      TDELTA = 1.D4
      RATE   = 1.D-4

      SUBTYP = 2
      DEGREE = 11
      WINSIZ = ( DEGREE + 1 ) / 2
      PKTSIZ = C06PS2

      CALL CLEARD ( RECSIZ, RECORD )
      CALL CLEARD ( PBUFSZ, PACKTS )

      RECORD(1) = SCLKDP
      RECORD(2) = DBLE( SUBTYP )
      RECORD(3) = DBLE( WINSIZ )
      RECORD(4) = RATE


      CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

      PTR = PKTSIZ*WINSIZ + 5

      DO I = 1, WINSIZ
         CLKBUF(I) = I * TDELTA
      END DO

      CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

      CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Subtype 3 divide-by zero case' )


      SCLKDP = 0.D0
      TDELTA = 1.D4
      RATE   = 1.D-4

      SUBTYP = 3
      DEGREE = 11
      WINSIZ = DEGREE + 1
      PKTSIZ = C06PS3

      CALL CLEARD ( RECSIZ, RECORD )
      CALL CLEARD ( PBUFSZ, PACKTS )

      RECORD(1) = SCLKDP
      RECORD(2) = DBLE( SUBTYP )
      RECORD(3) = DBLE( WINSIZ )
      RECORD(4) = RATE

      CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

      PTR = PKTSIZ*WINSIZ + 5

      DO I = 1, WINSIZ
         CLKBUF(I) = I * TDELTA
      END DO

      CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )


      CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )


C**********************************************************************
C
C
C     Normal test cases
C
C
C**********************************************************************



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test using type 6, subtype 1 attitude record.' )

C
C     We'll use a fixed axis and a rotation angle that
C     increases quadratically with time.
C
C     Our time step will be 1 second (10000 ticks )
C
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, AXIS )
      CALL VHATIP ( AXIS )

      SCLKDP = 0.D0
      TDELTA = 1.D4
      RATE   = 1.D-4

      SUBTYP = 1
      DEGREE = 11
      WINSIZ = DEGREE + 1
      PKTSIZ = C06PS1

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2

         CALL AXISAR ( AXIS, -ANGLE, CMAT )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

      END DO




      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 1 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test using type 6, subtype 0 attitude record.' )

C
C     We use the packet set from the previous test.
C
      SUBTYP = 0

      DEGREE = 11
      WINSIZ = ( DEGREE + 1 ) / 2
      PKTSIZ = C06PS0

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2

         CALL AXISAR ( AXIS, -ANGLE, CMAT )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

C
C        Compute the quaternion derivative with respect
C        to TDB; store it after the quaternion.
C
C        As in the previous case, the angular rate
C        in radians/second is
C
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 0 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test using type 6, subtype 2 attitude record.' )

C
C     We use the packet set from the previous test.
C
      SUBTYP = 2

      DEGREE = 11
      WINSIZ = ( DEGREE + 1 ) / 2
      PKTSIZ = C06PS2

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2

         CALL AXISAR ( AXIS, -ANGLE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

C
C        Compute the quaternion derivative with respect
C        to TDB; store it after the quaternion.
C
C        As in the previous case, the angular rate
C        in radians/second is
C
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

C
C        Next up is angular velocity.
C
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+C06PS0) )

C
C        We need angular acceleration. The angular velocity
C        direction is fixed; we just need the acceleration
C        of the rotation angle. We already have
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C
C        so
C
C
C                d(ANGLE)
C              ------------ =   2 * ALPHA / TDELTA**2
C              d**2(SCLKDP)
C
C
C        and
C
C              d(ANGLE)
C              ---------    =  ALPHA / (RATE*TDELTA)**2
C              d**2(TDB)
C
C
         CALL VSCL ( ALPHA/(RATE*TDELTA)**2, AXIS, DAV )

         CALL VEQU ( DAV, PACKTS(PTR+11) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 2 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test using type 6, subtype 3 attitude record.' )

C
C     We use the packet set from the previous test.
C
      SUBTYP = 3

      DEGREE = 11
      WINSIZ = DEGREE + 1
      PKTSIZ = C06PS3

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2

         CALL AXISAR ( AXIS, -ANGLE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

C
C        Compute the quaternion derivative with respect
C        to TDB; store it after the quaternion.
C
C        As in the previous case, the angular rate
C        in radians/second is
C
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

C
C        Next up is angular velocity.
C
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+8) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 3 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO





C**********************************************************************
C
C
C     Continuity test cases
C
C
C**********************************************************************

C
C     M2Q output is discontinuous at the rotation angle pi. The output
C     of ZZCKE06 should be continuous.
C
C     We'll repeat the previous set of tests using sequence of rotation
C     angles that start out negative and cross over the boundary.
C
C     The total difference between the initial and final angle is about
C     1.21 degrees. We'll start with a rotation angle of 180-0.6
C     degrees.
C


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Continuity test using type 6, subtype 1 '
     .//           'attitude record.'                         )


      CALL CLEARD ( 4, PREVQ )
C
C     We'll use a fixed axis and a rotation angle that
C     increases quadratically with time.
C
C     Our time step will be 1 second (10000 ticks )
C
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, AXIS )
      CALL VHATIP ( AXIS )

      SCLKDP = 0.D0
      TDELTA = 1.D4
      RATE   = 1.D-4

      SUBTYP = 1
      DEGREE = 11
      WINSIZ = DEGREE + 1
      PKTSIZ = 4

      ANGLE0 = (180.D0 -0.6D0) * RPD()

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2   +  ANGLE0

         CALL AXISAR ( AXIS, -ANGLE, CMAT )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

      END DO




      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 1 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

         CALL M2Q ( XCMAT, XQUAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

C
C        Check quaternion "continuity." We expect a rotation angle
C        of less than two degrees between the quaternion and its
C        predecessor. Note that the dot product of two quaternions
C        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
C
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4),
     .                              -1.D0,
     .                               1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

C
C        Save the quaternion for the continuity check on the
C        next pass.
C
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO

C
C---- Case -------------------------------------------------------------
C
C
      CALL TCASE ( 'Continuity test using type 6, subtype 0 '
     .//           'attitude record.'                         )

C
C     We use the packet set from the previous test.
C
      SUBTYP = 0

      DEGREE = 11
      WINSIZ = ( DEGREE + 1 ) / 2
      PKTSIZ = C06PS0

      ANGLE0 = (180.D0 -0.6D0) * RPD()

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2   +  ANGLE0

         CALL AXISAR ( AXIS, -ANGLE, CMAT )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

C
C        Compute the quaternion derivative with respect
C        to TDB; store it after the quaternion.
C
C        As in the previous case, the angular rate
C        in radians/second is
C
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 0 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

C
C        Check quaternion "continuity." We expect a rotation angle
C        of less than two degrees between the quaternion and its
C        predecessor. Note that the dot product of two quaternions
C        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
C
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4),
     .                              -1.D0,
     .                               1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

C
C        Save the quaternion for the continuity check on the
C        next pass.
C
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Continuity test using type 6, subtype 2 '
     .//           'attitude record.'                         )

C
C     We use the packet set from the previous test.
C
      SUBTYP = 2

      DEGREE = 11
      WINSIZ = ( DEGREE + 1 ) / 2
      PKTSIZ = C06PS2

      ANGLE0 = (180.D0 -0.6D0) * RPD()

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2     +  ANGLE0

         CALL AXISAR ( AXIS, -ANGLE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

C
C        Compute the quaternion derivative with respect
C        to TDB; store it after the quaternion.
C
C        As in the previous case, the angular rate
C        in radians/second is
C
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

C
C        Next up is angular velocity.
C
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+8) )

C
C        We need angular acceleration. The angular velocity
C        direction is fixed; we just need the acceleration
C        of the rotation angle. We already have
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C
C        so
C
C
C                d(ANGLE)
C              ------------ =   2 * ALPHA / TDELTA**2
C              d**2(SCLKDP)
C
C
C        and
C
C              d(ANGLE)
C              ---------    =  ALPHA / (RATE*TDELTA)**2
C              d**2(TDB)
C
C
         CALL VSCL ( ALPHA/(RATE*TDELTA)**2, AXIS, DAV )

         CALL VEQU ( DAV, PACKTS(PTR+11) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 2 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

C
C        Check quaternion "continuity." We expect a rotation angle
C        of less than two degrees between the quaternion and its
C        predecessor. Note that the dot product of two quaternions
C        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
C
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4),
     .                              -1.D0,
     .                               1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

C
C        Save the quaternion for the continuity check on the
C        next pass.
C
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Continuity test using type 6, subtype 3 '
     .//           'attitude record.'                         )

C
C     We use the packet set from the previous test.
C
      SUBTYP = 3

      DEGREE = 11
      WINSIZ = DEGREE + 1
      PKTSIZ = C06PS3

      ANGLE0 = (180.D0 -0.6D0) * RPD()

      DO I = 1, WINSIZ

         SCLKDP    =   (I-1)*TDELTA
         ALPHA     =   RPD() * 100 / TDELTA
         ANGLE     =   ALPHA * (I-1)**2  +  ANGLE0

         CALL AXISAR ( AXIS, -ANGLE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         C         = COS( ANGLE/2 )
         S         = SIN( ANGLE/2 )

         QUAT(0)   = C
         CALL VSCL ( S, AXIS, QUAT(1) )

         CLKBUF(I) = SCLKDP
         PTR       = (I-1)*PKTSIZ + 1

         CALL MOVED ( QUAT, 4, PACKTS(PTR) )

C
C        Compute the quaternion derivative with respect
C        to TDB; store it after the quaternion.
C
C        As in the previous case, the angular rate
C        in radians/second is
C
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

C
C        Next up is angular velocity.
C
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+8) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
C
C        Set the sample time.
C
         SCLKDP = (I-1)*DELTA

C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Subtype 3 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

C
C        Create a type 6 pointing record using the SCLK, subtype, and
C        packet count. We have to re-create the record for each call
C        because the CK evaluators use it destructively.
C
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

C
C        Compute the angular velocity at the sample time.
C        Units are radians/second.
C
C           We have
C
C              ANGLE     =   ALPHA * (I-1)**2
C
C              SCLKDP    =   (I-1)*TDELTA
C
C           so
C
C              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
C
C           and
C
C              d(ANGLE)
C              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
C              d(SCLKDP)
C

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

C
C        Compute the attitude quaternion and its derivative
C        at SCLKDP.
C
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Create C-matrix and AV from the quaternion state.
C
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

C
C        Compute the expected C-matrix and angular velocity.
C
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Check C-matrix derived from ZZCKE06 output state.
C
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

C
C        Check C-matrix quotient rotation angle.
C
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

C
C        Check angular velocity derived from ZZCKE06 output state.
C
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

C
C        Check output SCLK from ZZCKE06.
C
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

C
C        Check quaternion "continuity." We expect a rotation angle
C        of less than two degrees between the quaternion and its
C        predecessor. Note that the dot product of two quaternions
C        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
C
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4),
     .                              -1.D0,
     .                               1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

C
C        Save the quaternion for the continuity check on the
C        next pass.
C
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO



      CALL T_SUCCESS( OK )
      RETURN
      END
