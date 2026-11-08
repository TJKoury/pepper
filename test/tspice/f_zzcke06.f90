!$Procedure      F_ZZCKE06 ( Family of tests for ZZCKE06 )

      SUBROUTINE F_ZZCKE06 ( OK )

! $ Abstract
!
!     This test family exercises ZZCKE06.
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

      INCLUDE 'ckparam.inc'
      INCLUDE 'ck06.inc'

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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the CK type 6 quaternion state
!     evaluator ZZCKE06.
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
!-    TSPICE Version 1.1.0, 11-OCT-2021 (NJB)
!
!        Initialized CLKBUF in all divide-by-zero error test cases.
!
!-    TSPICE Version 1.0.0, 07-AUG-2015 (NJB)
!
!-&



!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VDOTG

!
!     Local parameters
!
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

!
!     Local Variables
!
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

!
!     Saved variables
!
!     Save all variables to avoid CSPICE stack overflow
!     problems.
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN  ( 'F_ZZCKE06' )




!**********************************************************************
!
!
!     Error test cases
!
!
!**********************************************************************

!
!---- Case -------------------------------------------------------------
!
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



!
!---- Case -------------------------------------------------------------
!
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

!
!        Compute the quaternion derivative with respect
!        to TDB; store it after the quaternion.
!
!        As in the previous case, the angular rate
!        in radians/second is
!
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




!
!---- Case -------------------------------------------------------------
!
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



!
!---- Case -------------------------------------------------------------
!
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


!
!---- Case -------------------------------------------------------------
!
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


!
!---- Case -------------------------------------------------------------
!
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


!**********************************************************************
!
!
!     Normal test cases
!
!
!**********************************************************************



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test using type 6, subtype 1 attitude record.' )

!
!     We'll use a fixed axis and a rotation angle that
!     increases quadratically with time.
!
!     Our time step will be 1 second (10000 ticks )
!
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
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 1 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test using type 6, subtype 0 attitude record.' )

!
!     We use the packet set from the previous test.
!
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

!
!        Compute the quaternion derivative with respect
!        to TDB; store it after the quaternion.
!
!        As in the previous case, the angular rate
!        in radians/second is
!
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 0 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test using type 6, subtype 2 attitude record.' )

!
!     We use the packet set from the previous test.
!
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

!
!        Compute the quaternion derivative with respect
!        to TDB; store it after the quaternion.
!
!        As in the previous case, the angular rate
!        in radians/second is
!
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

!
!        Next up is angular velocity.
!
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+C06PS0) )

!
!        We need angular acceleration. The angular velocity
!        direction is fixed; we just need the acceleration
!        of the rotation angle. We already have
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!
!        so
!
!
!                d(ANGLE)
!              ------------ =   2 * ALPHA / TDELTA**2
!              d**2(SCLKDP)
!
!
!        and
!
!              d(ANGLE)
!              ---------    =  ALPHA / (RATE*TDELTA)**2
!              d**2(TDB)
!
!
         CALL VSCL ( ALPHA/(RATE*TDELTA)**2, AXIS, DAV )

         CALL VEQU ( DAV, PACKTS(PTR+11) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 2 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test using type 6, subtype 3 attitude record.' )

!
!     We use the packet set from the previous test.
!
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

!
!        Compute the quaternion derivative with respect
!        to TDB; store it after the quaternion.
!
!        As in the previous case, the angular rate
!        in radians/second is
!
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

!
!        Next up is angular velocity.
!
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+8) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 3 test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

      END DO





!**********************************************************************
!
!
!     Continuity test cases
!
!
!**********************************************************************

!
!     M2Q output is discontinuous at the rotation angle pi. The output
!     of ZZCKE06 should be continuous.
!
!     We'll repeat the previous set of tests using sequence of rotation
!     angles that start out negative and cross over the boundary.
!
!     The total difference between the initial and final angle is about
!     1.21 degrees. We'll start with a rotation angle of 180-0.6
!     degrees.
!


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Continuity test using type 6, subtype 1 ' &
      //           'attitude record.'                         )


      CALL CLEARD ( 4, PREVQ )
!
!     We'll use a fixed axis and a rotation angle that
!     increases quadratically with time.
!
!     Our time step will be 1 second (10000 ticks )
!
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
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 1 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

         CALL M2Q ( XCMAT, XQUAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

!
!        Check quaternion "continuity." We expect a rotation angle
!        of less than two degrees between the quaternion and its
!        predecessor. Note that the dot product of two quaternions
!        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
!
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4), &
                                    -1.D0, &
                                     1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

!
!        Save the quaternion for the continuity check on the
!        next pass.
!
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO

!
!---- Case -------------------------------------------------------------
!
!
      CALL TCASE ( 'Continuity test using type 6, subtype 0 ' &
      //           'attitude record.'                         )

!
!     We use the packet set from the previous test.
!
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

!
!        Compute the quaternion derivative with respect
!        to TDB; store it after the quaternion.
!
!        As in the previous case, the angular rate
!        in radians/second is
!
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 0 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = TIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = TIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

!
!        Check quaternion "continuity." We expect a rotation angle
!        of less than two degrees between the quaternion and its
!        predecessor. Note that the dot product of two quaternions
!        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
!
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4), &
                                    -1.D0, &
                                     1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

!
!        Save the quaternion for the continuity check on the
!        next pass.
!
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Continuity test using type 6, subtype 2 ' &
      //           'attitude record.'                         )

!
!     We use the packet set from the previous test.
!
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

!
!        Compute the quaternion derivative with respect
!        to TDB; store it after the quaternion.
!
!        As in the previous case, the angular rate
!        in radians/second is
!
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

!
!        Next up is angular velocity.
!
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+8) )

!
!        We need angular acceleration. The angular velocity
!        direction is fixed; we just need the acceleration
!        of the rotation angle. We already have
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!
!        so
!
!
!                d(ANGLE)
!              ------------ =   2 * ALPHA / TDELTA**2
!              d**2(SCLKDP)
!
!
!        and
!
!              d(ANGLE)
!              ---------    =  ALPHA / (RATE*TDELTA)**2
!              d**2(TDB)
!
!
         CALL VSCL ( ALPHA/(RATE*TDELTA)**2, AXIS, DAV )

         CALL VEQU ( DAV, PACKTS(PTR+11) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 2 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

!
!        Check quaternion "continuity." We expect a rotation angle
!        of less than two degrees between the quaternion and its
!        predecessor. Note that the dot product of two quaternions
!        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
!
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4), &
                                    -1.D0, &
                                     1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

!
!        Save the quaternion for the continuity check on the
!        next pass.
!
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Continuity test using type 6, subtype 3 ' &
      //           'attitude record.'                         )

!
!     We use the packet set from the previous test.
!
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

!
!        Compute the quaternion derivative with respect
!        to TDB; store it after the quaternion.
!
!        As in the previous case, the angular rate
!        in radians/second is
!
         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         DQ(0) = -0.5D0 * S * W

         CALL VSCL ( 0.5D0*C*W, AXIS, DQ(1) )

         CALL MOVED ( DQ, 4, PACKTS(PTR+4) )

!
!        Next up is angular velocity.
!
         CALL QDQ2AV ( QUAT, DQ, AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AV, PACKTS(PTR+8) )

      END DO



      NSAMP   = 300
      SPAN    = CLKBUF(WINSIZ) - CLKBUF(1)
      DELTA   = SPAN / NSAMP

      DO I = 1, NSAMP
!
!        Set the sample time.
!
         SCLKDP = (I-1)*DELTA

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Subtype 3 continuity test for I = #; SCLKDP #'
         CALL REPMI ( TITLE, '#', I,          TITLE )
         CALL REPMD ( TITLE, '#', SCLKDP, 14, TITLE )
         CALL TCASE ( TITLE )

!
!        Create a type 6 pointing record using the SCLK, subtype, and
!        packet count. We have to re-create the record for each call
!        because the CK evaluators use it destructively.
!
         RECORD(1) = SCLKDP
         RECORD(2) = DBLE( SUBTYP )
         RECORD(3) = DBLE( WINSIZ )
         RECORD(4) = RATE

         CALL MOVED ( PACKTS, PKTSIZ*WINSIZ, RECORD(5) )

         PTR = PKTSIZ*WINSIZ + 5

         CALL MOVED ( CLKBUF, WINSIZ, RECORD(PTR) )

         SIZE = 4 + (PKTSIZ+1)*WINSIZ


         CALL MOVED ( RECORD, SIZE, REC2 )

!
!        Compute the angular velocity at the sample time.
!        Units are radians/second.
!
!           We have
!
!              ANGLE     =   ALPHA * (I-1)**2
!
!              SCLKDP    =   (I-1)*TDELTA
!
!           so
!
!              ANGLE     =   ALPHA * (SCLKDP/TDELTA)**2
!
!           and
!
!              d(ANGLE)
!              --------- =   2 * ALPHA * SCLKDP / TDELTA**2
!              d(SCLKDP)
!

         W = RATE * 2 * ALPHA * ( SCLKDP/TDELTA**2 )

         CALL VSCL ( W, AXIS, AV )

!
!        Compute the attitude quaternion and its derivative
!        at SCLKDP.
!
         CALL ZZCKE06 ( RECORD, QSTATE, CLKOUT )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Create C-matrix and AV from the quaternion state.
!
         CALL Q2M ( QSTATE, CMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL QDQ2AV ( QSTATE, QSTATE(5), AV )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MOVED ( QSTATE, 4, QUAT )

!
!        Compute the expected C-matrix and angular velocity.
!
         CALL CKE06 ( .TRUE., REC2, XCMAT, XAV, XSCLK )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Check C-matrix derived from ZZCKE06 output state.
!
         TOL = VTIGHT

         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TOL, OK )

!
!        Check C-matrix quotient rotation angle.
!
         CALL MTXM ( CMAT, XCMAT, QMAT )

         CALL RAXISA ( QMAT, QAXIS, QANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKSD ( 'quotient angle', QANGLE, '~', 0.D0, TOL, OK )

!
!        Check angular velocity derived from ZZCKE06 output state.
!
         TOL = VTIGHT
         CALL CHCKAD ( 'AV',   AV, '~~/',   XAV, 3, TOL, OK )

!
!        Check output SCLK from ZZCKE06.
!
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', XSCLK, TOL, OK )

!
!        Check quaternion "continuity." We expect a rotation angle
!        of less than two degrees between the quaternion and its
!        predecessor. Note that the dot product of two quaternions
!        Q1, Q2 is the real part of their quotient (Q1_conj * Q2).
!
         IF ( I .GT. 1 ) THEN

            QQANGL = ACOS(  BRCKTD( VDOTG(QUAT,PREVQ,4), &
                                    -1.D0, &
                                     1.D0               )  ) * DPR()

            CALL CHCKSD ( 'QQANGL', QQANGL, '<', 2.D0, 0.D0, OK )

         END IF

!
!        Save the quaternion for the continuity check on the
!        next pass.
!
         CALL MOVED ( QUAT, 4, PREVQ )

      END DO



      CALL T_SUCCESS( OK )
      RETURN
      END
