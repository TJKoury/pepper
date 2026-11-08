!$Procedure F_PCK20 ( PCK data type 20 tests )

      SUBROUTINE F_PCK20 ( OK )

! $ Abstract
!
!     Exercise routines associated with PCK data type 20.
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

      INCLUDE 'spkrec.inc'
      INCLUDE 'spk20.inc'

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
!     This routine tests the following routines that write and read
!     type 20 PCK data:
!
!        PCKE20
!        PCKR20
!        PCKW20
!
!     The higher level PCK routine
!
!        PCKMAT
!
!     is also exercised by this test family.
!
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
!-    TSPICE Version 1.0.0, 20-MAR-2014 (NJB)
!
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
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'test.bpc' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'pck_test_20_v1.bpc' )

      CHARACTER*(*)         PCK2
      PARAMETER           ( PCK2   = 'pck_test_20_v2.bpc' )

      CHARACTER*(*)         DEFSID
      PARAMETER           ( DEFSID = 'pckw20 test segment' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'source_data.bsp' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'source_data1.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      INTEGER               MXNREC
      PARAMETER           ( MXNREC  = 10000 )

      INTEGER               COFSIZ
      PARAMETER           ( COFSIZ = MXNREC * MAXREC )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXSMP
      PARAMETER           ( MAXSMP = 100001 )

!
!     Local Variables
!
!
!     Note: SEGID is declared longer than SIDLEN because of the need to
!     hold a long string for testing error handling.
!
      CHARACTER*(BDNMLN)    BODYNM
      CHARACTER*(BDNMLN)    CENTNM
      CHARACTER*(LNSIZE)    SEGID
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(FRNMLN)    SRCREF

      DOUBLE PRECISION      AVBUFF ( 3, MAXSMP )
      DOUBLE PRECISION      CDATA  ( MAXREC * MXNREC )
      DOUBLE PRECISION      DAY0
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      EPOCHS ( MAXSMP )
      DOUBLE PRECISION      EULSTA ( 6 )
      DOUBLE PRECISION      ASCALE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      INITFR
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      POS0   ( 3 )
      DOUBLE PRECISION      POSCOF ( (MAXDEG+1) * 3 )
      DOUBLE PRECISION      RBUFF  ( 3, 3, MAXSMP )
      DOUBLE PRECISION      TSCALE
      DOUBLE PRECISION      VELCOF ( (MAXDEG+1) * 3 )
      DOUBLE PRECISION      XAVBUF ( 3, MAXSMP )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XRBUFF ( 3, 3, MAXSMP )
      DOUBLE PRECISION      XSTBUF ( 6, MAXSMP )
      DOUBLE PRECISION      WORK   ( MAXDEG+1 )

      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               DEGREE
      INTEGER               FRCENT
      INTEGER               FRCODE
      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NSAMP
      INTEGER               NTERMS
      INTEGER               POLYDG
      INTEGER               RECNO

      LOGICAL               FOUND
      LOGICAL               UNIQUE

!
!     Saved variables
!
!
!     Save all local variables to avoid stack problems on some
!     platforms.
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_PCK20' )


!
!     Open a new PCK file for writing.
!
!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Setup: create kernels.' )


      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )


      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK1, ' ', 0, HANDLE )

!
!     Initialize the data buffer with values that are recognizable but
!     otherwise bogus.
!
      DO I = 1, COFSIZ
         CDATA(I) = DBLE( I )
      END DO

!
!     Pick class ID and frame.
!
      CLSSID = 3
      FRAME  = 'J2000'

!
!     Initial record count.
!
      N = 100

!
!     Polynomial degree.
!
      POLYDG = 16

!
!     Record interval length and start time. Units
!     are Julian ephemeris days.
!
      INTLEN = 5.D0

      INITJD = 2451545.0D0
      INITFR = 0.25D0

!
!     Pick nominal time bounds.
!
      FIRST = (  ( INITJD - J2000() ) + INITFR             ) * SPD()
      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()

!
!     Initialize segment identifier.
!
      SEGID = DEFSID


!
!*****************************************************************
!*
!*    PCKW20 error cases:
!*
!*****************************************************************
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid frame.' )


      POLYDG = 1
      ASCALE = RPD()
      TSCALE = SPD()

      CALL CLEARD ( COFSIZ, CDATA )


      CALL PCKW20 ( HANDLE,  CLSSID,  'XXX',   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree too high.' )

!
!     POLYDG = MAXDEG + 1
!
      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       51, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree too low.' )

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       -1, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SEGID too long.' )

      SEGID = '1234567890123456789012345678912345678901234567890'

      FRAME = 'J2000'

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(IDSTRINGTOOLONG)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SEGID contains non-printable characters.' )

      SEGID      = DEFSID

      SEGID(5:5) = CHAR(7)

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid coefficient count' )

      SEGID = DEFSID
      N     = 0

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = -1

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = 100



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor times out of order' )

      FIRST = LAST + 1.D0


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      FIRST = (  ( INITJD - J2000() ) + INITFR ) * SPD()

      LAST  = FIRST - 1.D0


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

!
!     Restore original descriptor time bounds.
!
      FIRST = (  ( INITJD - J2000() ) + INITFR             ) * SPD()
      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Gap following last epoch' )



      LAST  = LAST + 2.D-4


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )



      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()


      FIRST = FIRST - 2.D-4

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


!
!     Restore original descriptor time bounds.
!
      FIRST = (  ( INITJD - J2000() ) + INITFR             ) * SPD()
      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-positive interval length' )


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   0.D0,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,  -1.D0,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-positive distance or time scale' )


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   0.D0,    TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   -1.D0,   TSCALE,  INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  0.D0,    INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )

      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  -1.D0,   INITJD,  INITFR )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )



!
!     Delete PCK used for error test cases.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*    PCKW20 non-error exception cases:
!*
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create segment with coverage gaps at each end.' )


!
!     The following segment will have a two-day record coverage
!     interval, and the descriptor coverage will extend an extra
!     9 ms in each direction.
!
      SEGID  = 'Small test segment: type 20'
      CLSSID = 301
      FRAME  = 'J2000'


      INTLEN = 2.D0

      INITJD = J2000() - 1.D0

      INITFR = 0.D0

      FIRST  = -SPD() - 1.D-9
      LAST   =  SPD() + 1.D-9

      N      = 1
      POLYDG = 20

!
!     The angle scale is radians; the time scale is Julian days.
!
      ASCALE = 1.D0
      TSCALE = SPD()

      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK1, ' ', 0, HANDLE )


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL PCKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create segment with long coverage interval and ' &
      //           'coverage gaps at each end.'                      )

!
!     For this segment, the gap tolerance will be based on
!     a relative scale.
!
!
!     The following segment will have a 1000000-day record coverage
!     interval, and the descriptor coverage will extend an extra
!     40 ms in each direction.
!
      SEGID  = 'Small test segment: type 20'
      CLSSID = 301
      FRAME  = 'J2000'


      INTLEN = 1.D6

      INITJD = J2000() - INTLEN/2

      INITFR = 0.D0

      FIRST  =  ( INITJD       -J2000() )* SPD() - 0.001D0
      LAST   =  ( INITJD+INTLEN-J2000() )* SPD() + 0.001D0

      N      = 1
      POLYDG = 20

!
!     The distance scale is radians; the time scale is Julian days.
!
      ASCALE = 1.D0
      TSCALE = SPD()

      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK1, ' ', 0, HANDLE )


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     And one more error case:
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create segment with long coverage interval and ' &
      //           'excessive coverage gaps at the left end.'      )

!
!     For this segment, the gap tolerance will be based on
!     a relative scale.
!
!
!     The following segment will have a 1000000-day record coverage
!     interval, and the descriptor coverage will extend an extra
!     100 ms in the negative direction.
!
      SEGID  = 'Small test segment: type 20'

      CALL NAMFRM ( 'IAU_MOON', FRCODE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FRINFO ( FRCODE, FRCENT, CLASS, CLSSID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      FRAME  = 'J2000'


      INTLEN = 1.D6

      INITJD = J2000() - INTLEN/2

      INITFR = 0.D0

      FIRST  =  ( INITJD       -J2000() )* SPD() - 0.1D0
      LAST   =  ( INITJD+INTLEN-J2000() )* SPD()

      N      = 1
      POLYDG = 20

!
!     The distance scale is degrees; the time scale is Julian days.
!
      ASCALE = DPR()
      TSCALE = SPD()

      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK1, ' ', 0, HANDLE )


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )
      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create segment with long coverage interval and ' &
      //           'excessive coverage gaps at the right end.'      )

!
!     For this segment, the gap tolerance will be based on
!     a relative scale.
!
!
!     The following segment will have a 1000000-day record coverage
!     interval, and the descriptor coverage will extend an extra
!     100 ms in the negative direction.
!
      SEGID  = 'Small test segment: type 20'
      CLSSID = 301
      FRAME  = 'J2000'


      INTLEN = 1.D6

      INITJD = J2000() - INTLEN/2

      INITFR = 0.D0

      FIRST  =  ( INITJD       -J2000() )* SPD()
      LAST   =  ( INITJD+INTLEN-J2000() )* SPD() + 0.1D0

      N      = 1
      POLYDG = 20

!
!     The angle scale is degrees; the time scale is Julian days.
!
      ASCALE = RPD()
      TSCALE = SPD()

      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK1, ' ', 0, HANDLE )


      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )
      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )






!*****************************************************************
!*
!*    PCKW20, PCKR20, PCKE20 normal cases:
!*
!*****************************************************************





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create PCK with one small type 20 segment.' &
      //           'The PCK provides IAU_MOON orientation.' )

!
!     Create a small PCK file containing 1 record. We'll represent
!     orientation of the IAU_MOON frame relative to J2000.
!
!     We're going to do something dirty here: we'll create a type
!     9 SPK file containing Euler angle states. This will enable
!     us to fit Chebys to the states using existing test utility
!     code.
!

!
!     Create and load a test PCK to provide orientation data.
!     Keep the file after loading.
!
      CALL TSTPCK ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sample orientation from the PCK and convert to
!     Euler states.
!
      FIRST  = -SPD()
      LAST   =  SPD()

      NSAMP  = 10000

      DELTA  = ( LAST - FIRST ) / ( NSAMP - 1 )

      SRCREF = 'IAU_MOON'

      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )
         EPOCHS(I) = ET

         CALL SXFORM ( 'J2000', SRCREF, ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL XF2EUL ( XFORM, 3, 1, 3, EULSTA, UNIQUE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Reorder the state as we pack it into the state buffer.
!        The rightmost angle comes first.
!
         DO J = 1, 3

            XSTBUF(4-J, I) = EULSTA(J  )
            XSTBUF(7-J, I) = EULSTA(J+3)

         END DO

      END DO

!
!     Create a type 9 SPK from the state data.
!
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK0, ' ', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY   = 301
      CENTER = 3
      FRAME  = 'J2000'
      SEGID  = 'Source Euler states'
      DEGREE = 3

      CALL SPKW09 ( HANDLE,  BODY,    CENTER,  FRAME, &
                    FIRST,   LAST,    SEGID,   DEGREE, &
                    NSAMP,   XSTBUF,  EPOCHS           )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load test SPK to provide data.
!
      CALL FURNSH ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate angle and angular rate coefficients for the segment.
!     We'll store only the rate coefficients.
!
      CALL BODC2N ( CENTER, CENTNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL BODC2N ( BODY,   BODYNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NTERMS = POLYDG+1

      CALL T_SPKCHB ( BODYNM, CENTNM, FRAME, FIRST,  LAST, &
                      NTERMS, NTERMS, WORK,  POSCOF, VELCOF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scale the coefficients to represent positions in units
!     of ASCALE and velocities in units of ASCALE/TSCALE.
!
      ASCALE = RPD()
      TSCALE = SPD()


      DO I = 1, 3*NTERMS

         POSCOF(I) =   POSCOF(I) / ASCALE

         VELCOF(I) = ( VELCOF(I) / ASCALE )  *  TSCALE

      END DO

!
!     Get the state at the interval midpoint.
!
      MIDPT = ( FIRST + LAST ) / 2

      CALL SPKEZP ( BODY, MIDPT, FRAME, 'NONE', CENTER, POS0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scale the midpoint position to units of ASCALE.
!
      CALL VSCLIP ( 1.D0/ASCALE, POS0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Pack the coefficient array.
!
      K = 1

      DO I = 1, 3

         J = (I-1)*NTERMS + 1

         CALL MOVED ( VELCOF(J), NTERMS, CDATA(K) )
         CDATA( K + NTERMS ) = POS0(I)

         K = K + NTERMS + 1

      END DO

!
!     Get class ID of "to" frame. We're going to pretend this
!     is the IAU_JUPITER frame. All we need is an IAU frame
!     distinct from IAU_MOON.
!
      FIXREF = 'IAU_JUPITER'

      CALL NAMFRM ( FIXREF, FRCODE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FRINFO ( FRCODE, FRCENT, CLASS, CLSSID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
!
!     The record covers two days.
!
      FIRST  = -SPD()
      LAST   =  SPD()

      N      = 1
      POLYDG = 20

!
!     The interval length has units of Julian days.
!
      INTLEN = ( LAST - FIRST ) / SPD()

      DAY0   =  J2000() + ( FIRST / SPD() )

      INITJD = AINT ( DAY0 )
      INITFR = DAY0 - INITJD


      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK1, ' ', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL PCKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Sample orientation from small IAU_MOON PCK.' )


      CALL FURNSH ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sample and check orientation from the new type 20 PCK file.
!
      NSAMP = 10001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )


      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )

         CALL SXFORM ( FRAME, FIXREF,       ET,         XFORM )
         CALL XF2RAV ( XFORM, RBUFF(1,1,I), AVBUFF(1,I)       )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )

         CALL SXFORM ( FRAME, SRCREF,        ET,         XFORM )
         CALL XF2RAV ( XFORM, XRBUFF(1,1,I), XAVBUF(1,I)       )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Test orientation results.
!
      DO I = 1, NSAMP

         LABEL = 'R(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, RBUFF (1,1,I), '~~/', &
                              XRBUFF(1,1,I), 9,    TIGHT,  OK )

      END DO

!
!     Test angular velocity results.
!
      DO I = 1, NSAMP

         LABEL = 'AV(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, AVBUFF(1,I), '~~/', &
                              XAVBUF(1,I), 3,    TIGHT,  OK )

      END DO



      CALL UNLOAD ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create PCK with one small type 20 segment. ' &
      //           'The PCK provides IAU_EARTH orientation.' )
!
!     Create a small PCK file containing 1 record. We'll represent
!     orientation of the IAU_EARTH frame relative to ECLIPJ2000
!
!     We're going to do something dirty here: we'll create a type
!     9 SPK file containing Euler angle states. This will enable
!     us to fit Chebys to the states using existing test utility
!     code.
!

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sample orientation from the PCK and convert to
!     Euler states.
!
      FIRST  = -SPD()
      LAST   =  FIRST + 2 * SPD()

      NSAMP  = 10000

      DELTA  = ( LAST - FIRST ) / ( NSAMP - 1 )

      FRAME  = 'ECLIPJ2000'
      SRCREF = 'IAU_EARTH'

      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )
         EPOCHS(I) = ET

         CALL SXFORM ( FRAME, SRCREF, ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL XF2EUL ( XFORM, 3, 1, 3, EULSTA, UNIQUE )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Adjust the angle corresponding to RA+pi/2
!        so it remains continuous.
!
         IF ( EULSTA(3) .LT. 0.D0 ) THEN
            EULSTA(3) = EULSTA(3) + TWOPI()
         END IF
!
!        Reorder the state as we pack it into the state buffer.
!        The rightmost angle comes first.
!
         DO J = 1, 3

            XSTBUF(4-J, I) = EULSTA(J  )
            XSTBUF(7-J, I) = EULSTA(J+3)

         END DO

      END DO

!
!     Create a type 9 SPK from the state data.
!
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK0, ' ', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY   = 399
      CENTER = 3
      SEGID  = 'Source Euler states'
      DEGREE = 7

      CALL SPKW09 ( HANDLE,  BODY,    CENTER,  FRAME, &
                    FIRST,   LAST,    SEGID,   DEGREE, &
                    NSAMP,   XSTBUF,  EPOCHS           )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load test SPK to provide data.
!
      CALL FURNSH ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate angle and angular rate coefficients for the segment.
!     We'll store only the rate coefficients.
!
      CALL BODC2N ( CENTER, CENTNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL BODC2N ( BODY,   BODYNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NTERMS = POLYDG+1

      CALL T_SPKCHB ( BODYNM, CENTNM, FRAME, FIRST,  LAST, &
                      NTERMS, NTERMS, WORK,  POSCOF, VELCOF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scale the coefficients to represent positions in units
!     of ASCALE and velocities in units of ASCALE/TSCALE.
!
      ASCALE = RPD()
      TSCALE = SPD()


      DO I = 1, 3*NTERMS

         POSCOF(I) =   POSCOF(I) / ASCALE

         VELCOF(I) = ( VELCOF(I) / ASCALE )  *  TSCALE

      END DO

!
!     Get the state at the interval midpoint.
!
      MIDPT = ( FIRST + LAST ) / 2

!
!     The position information from the SPK is garbage; we must
!     compute POS0 directly.
!

      CALL SPKEZP ( BODY, MIDPT, FRAME, 'NONE', CENTER, POS0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scale the midpoint position to units of ASCALE.
!
      CALL VSCLIP ( 1.D0/ASCALE, POS0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Pack the coefficient array.
!
      K = 1

      DO I = 1, 3

         J = (I-1)*NTERMS + 1

         CALL MOVED ( VELCOF(J), NTERMS, CDATA(K) )
         CDATA( K + NTERMS ) = POS0(I)

         K = K + NTERMS + 1

      END DO

!
!     Get class ID of "to" frame. We're going to pretend this
!     is the IAU_JUPITER frame. All we need is an IAU frame
!     distinct from IAU_EARTH.
!
      FIXREF = 'IAU_JUPITER'

      CALL NAMFRM ( FIXREF, FRCODE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FRINFO ( FRCODE, FRCENT, CLASS, CLSSID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      N      = 1
      POLYDG = 20

!
!     The interval length has units of Julian days.
!
      INTLEN = ( LAST - FIRST ) / SPD()

      DAY0   =  J2000() + ( FIRST / SPD() )

      INITJD = AINT ( DAY0 )
      INITFR = DAY0 - INITJD


      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK1, ' ', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL PCKW20 ( HANDLE,  CLSSID,  FRAME,   FIRST, &
                    LAST,    SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA,   ASCALE,  TSCALE,  INITJD,  INITFR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL PCKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL UNLOAD ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Sample orientation from small IAU_EARTH PCK.' )


      CALL FURNSH ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sample and check orientation from the new type 20 PCK file.
!
      NSAMP = 10001
      NSAMP = 10

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )


      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )

         CALL SXFORM ( FRAME, FIXREF,       ET,         XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL XF2RAV ( XFORM, RBUFF(1,1,I), AVBUFF(1,I)       )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )

         CALL SXFORM ( FRAME, SRCREF,        ET,         XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL XF2RAV ( XFORM, XRBUFF(1,1,I), XAVBUF(1,I)       )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Test orientation results.
!
      DO I = 1, NSAMP

         LABEL = 'R(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, RBUFF (1,1,I), '~~/', &
                              XRBUFF(1,1,I), 9,    TIGHT,  OK )

         IF ( .NOT. OK ) THEN
            STOP
         END IF

      END DO

!
!     Test angular velocity results.
!
      DO I = 1, NSAMP

         LABEL = 'AV(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, AVBUFF(1,I), '~~/', &
                              XAVBUF(1,I), 3,    TIGHT,  OK )

         IF ( .NOT. OK ) THEN
            STOP
         END IF

      END DO

      CALL UNLOAD ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create a PCK with a large type 20 segment ' &
      //           'having multiple records. ' &
      //           'The PCK provides IAU_MOON orientation.' )


      FRAME  = 'J2000'
      FIXREF = 'IAU_JUPITER'
      SRCREF = 'IAU_MOON'
      CLSSID = 599

      BODY   = 301
      CENTER = 3

      CALL BODC2N ( CENTER, CENTNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL BODC2N ( BODY,   BODYNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     N is the record count.
!
      N = 1000

!
!     The interval length has units of Julian days.
!
!     Each record covers 1/8 day.
!
      INTLEN = 0.125D0

      FIRST  =  -10 * JYEAR()
      LAST   =  FIRST   + N  * INTLEN * SPD()

      POLYDG = 20


      DAY0   =  J2000() + ( FIRST / SPD() )

      INITJD = AINT ( DAY0 )
      INITFR = DAY0 - INITJD


!
!     Sample orientation from the PCK and convert to
!     Euler states.
!
      NSAMP  = 100000

      DELTA  = ( LAST - FIRST ) / ( NSAMP - 1 )


      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )
         EPOCHS(I) = ET

         CALL SXFORM ( FRAME, SRCREF, ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL XF2EUL ( XFORM, 3, 1, 3, EULSTA, UNIQUE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Reorder the state as we pack it into the state buffer.
!        The rightmost angle comes first.
!
         DO J = 1, 3

            XSTBUF(4-J, I) = EULSTA(J  )
            XSTBUF(7-J, I) = EULSTA(J+3)

         END DO

      END DO

!
!     Create a type 9 SPK from the state data.
!
      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY   = 301
      CENTER = 3
      SEGID  = 'Source Euler states'
      DEGREE = 3

      CALL SPKW09 ( HANDLE,  BODY,    CENTER,  FRAME, &
                    FIRST,   LAST,    SEGID,   DEGREE, &
                    NSAMP,   XSTBUF,  EPOCHS           )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now create the PCK file.
!
!
!     Load test SPK to provide data.
!
      CALL FURNSH ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The interval length has units of Julian days.
!
!     Each record covers 1/8 day.
!
!
!     Create N data records.
!
      NTERMS = POLYDG+1

      K      = 1

      DO RECNO = 1, N

         ET0 = FIRST + ( (RECNO-1) * INTLEN * SPD() )
         ET1 = ET0   + ( INTLEN * SPD() )

         CALL T_SPKCHB ( BODYNM, CENTNM, FRAME, ET0,    ET1, &
                         NTERMS, NTERMS, WORK,  POSCOF, VELCOF )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Scale the coefficients to represent positions in units
!        of ASCALE and velocities in units of ASCALE/TSCALE.
!

         DO I = 1, 3*NTERMS

            POSCOF(I) =   POSCOF(I) / ASCALE

            VELCOF(I) = ( VELCOF(I) / ASCALE )  *  TSCALE

         END DO

!
!        Get the state at the interval midpoint.
!
         MIDPT = ( ET0 + ET1 ) / 2

         CALL SPKEZP ( BODY, MIDPT, FRAME, 'NONE', CENTER, POS0, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Scale the midpoint position to units of ASCALE.
!
         CALL VSCLIP ( 1.D0/ASCALE, POS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Pack the coefficient array.
!

         DO I = 1, 3

            J = (I-1)*NTERMS + 1

            CALL MOVED ( VELCOF(J), NTERMS, CDATA(K) )
            CDATA( K + NTERMS ) = POS0(I)

            K = K + NTERMS + 1

         END DO

      END DO


      IF ( EXISTS(PCK2) ) THEN
         CALL DELFIL( PCK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL PCKOPN ( PCK2, ' ', 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL PCKW20 ( HAN2,  CLSSID,  FRAME,   FIRST, &
                    LAST,  SEGID,   INTLEN,  N,       POLYDG, &
                    CDATA, ASCALE,  TSCALE,  INITJD,  INITFR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Sample orientation from large PCK.' )


      CALL FURNSH ( PCK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sample and check orientation from the new type 20 PCK file.
!
      NSAMP = 100001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )




      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )

         CALL SXFORM ( FRAME, FIXREF,       ET,         XFORM )
         CALL XF2RAV ( XFORM, RBUFF(1,1,I), AVBUFF(1,I)       )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,  LAST  )

         CALL SXFORM ( FRAME, SRCREF,        ET,         XFORM )
         CALL XF2RAV ( XFORM, XRBUFF(1,1,I), XAVBUF(1,I)       )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Test orientation results.
!
      DO I = 1, NSAMP

         LABEL = 'R(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, RBUFF (1,1,I), '~~/', &
                              XRBUFF(1,1,I), 9,    TIGHT,  OK )

      END DO

!
!     Test angular velocity results.
!
      DO I = 1, NSAMP

         LABEL = 'AV(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, AVBUFF(1,I), '~~/', &
                              XAVBUF(1,I), 3,    TIGHT,  OK )

      END DO



      CALL UNLOAD ( PCK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
!
!     Close and delete the PCK files.
!
      CALL TCASE ( 'Clean up.' )


      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL UNLOAD ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
