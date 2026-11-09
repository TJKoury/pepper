!$Procedure F_GFEVNT ( GFEVNT family tests )

      SUBROUTINE F_GFEVNT ( OK )
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFEVNT
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
!     This routine does not generate errors. Routines in its
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
!     This routine tests the mid-level SPICELIB
!     geometry routine GFEVNT.
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
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 03-MAR-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD

!
!     Local parameters
!
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfevnt.bsp' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfevnt.pck' )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-4 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 20000 )

      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 10 )

      INTEGER               NENAMES
      PARAMETER           ( NENAMES = 3 )

      INTEGER               NXNAMES
      PARAMETER           ( NXNAMES = 4 )

      INTEGER               NC
      PARAMETER           ( NC = 7 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ENAMES ( NENAMES )
      CHARACTER*(LNSIZE)    XNAMES ( NXNAMES )
      CHARACTER*(LNSIZE)    CNAMES ( NC )
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    TIME0
      CHARACTER*(LNSIZE)    TIME1

      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXVAL )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXVAL )
      DOUBLE PRECISION      WORK   ( LBCELL : MAXVAL, NWMAX)

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J

      LOGICAL               BAIL
      LOGICAL               GFBAIL
      EXTERNAL              GFBAIL
      LOGICAL               RPT

!
!     Geometric quantity name.
!
!
      CHARACTER*(LNSIZE)    EVENT

!
!     Relational string
!
      CHARACTER*(LNSIZE)    RELATE

!
!     Quantity definition parameter arrays:
!
      INTEGER               QNPARS
      CHARACTER*(LNSIZE)    QPNAMS ( MAXPAR )
      CHARACTER*(LNSIZE)    QCPARS ( MAXPAR )
      DOUBLE PRECISION      QDPARS ( MAXPAR )
      INTEGER               QIPARS ( MAXPAR )
      LOGICAL               QLPARS ( MAXPAR )

!
!     Routines to set step size, refine transition times
!     and report work.
!
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFSTEP

      SAVE

!
!     Initial values
!

      DATA                  CNAMES / '>', &
                                     '=', &
                                     '<', &
                                     'LOCMAX', &
                                     'LOCMIN', &
                                     'ABSMAX', &
                                     'ABSMIN'   /


      DATA                  CORR / 'NONE', &
                                   'lt', &
                                   ' lt+s', &
                                   ' cn', &
                                   ' cn+s', &
                                   'XLT', &
                                   'XLT+S', &
                                   'XCN', &
                                   'XCN+S'   /

!
!     Coded and usable event conditions.
!
      DATA                ENAMES / 'DISTANCE', &
                                   'ANGULAR SEPARATION', &
                                   'COORDINATE' /

!
!     Coded but not yet usable event conditions. Use
!     these condition names to test unknown EVENT values.
!     Eventualy the elements of this list will exist in the
!     ENAMES list.
!
      DATA                XNAMES / 'ANGSPD', &
                                   'APPDIAM', &
                                   'PHASE', &
                                   'RNGRAT'  /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFEVNT' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a PCK, load using FURNSH.
!
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create an SPK, load using FURNSH.
!
      CALL TSTSPK( SPK, .TRUE., HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL FURNSH( SPK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a confinement window from ET0 and ET1.
!
      TIME0 = '2000 JAN 1  00:00:00 TDB'
      TIME1 = '2000 APR 1  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXVAL, CNFINE )
      CALL SSIZED ( MAXVAL, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!   Case 1
!
!     Test the implemented geometric quantities will cause an
!     error signal for a QNPARS beyond the GFEVNT MAXPAR value.
!
      RELATE    = 'ABSMAX'
      QNPARS    = MAXPAR + 1
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     Add 2 points to the confinement interval window.
!
      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, NENAMES

         EVENT = ENAMES(I)

         TITLE = 'QNPARS > MAXPARS: ' //EVENT
         CALL TCASE ( TITLE )

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)',   OK )

      END DO


!
!   Case 2
!
!     Test the unimplemented geometric quantities will cause an
!     error signal.
!
      RELATE    = 'ABSMAX'
      QNPARS    = 3
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.


      DO I = 1, NXNAMES

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Add 2 points to the confinement interval window.
!
         CALL WNINSD ( ET0, ET1, CNFINE )

         EVENT = XNAMES(I)

         TITLE = 'Unimplemented quantity: ' //EVENT
         CALL TCASE ( TITLE )

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )

      END DO


!
!   Case 3
!
!     Cause an error signal due to an invalid relation operator.
!
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      RELATE = '==='

      CALL TCASE ( 'Invalid: '//RELATE )

      CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                    QNPARS,     QPNAMS,   QCPARS, &
                    QDPARS,     QIPARS,   QLPARS, &
                    RELATE,     REFVAL,   CNVTOL, &
                    ADJUST,     CNFINE,   RPT, &
                    GFREPI,     GFREPU,   GFREPF, &
                    MAXVAL,     NWMAX,    WORK, &
                    BAIL,       GFBAIL,    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


!
!     Case 4
!
!     Run a valid, simple search using each relation operator
!     and each aberration correction.
!
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      DO I = 1, NC

         DO J = 1, NCORR

            CALL SCARDD ( 0, CNFINE )
            CALL SCARDD ( 0, RESULT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL WNINSD ( ET0, ET1, CNFINE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            RELATE    = CNAMES(I)
            QCPARS(3) = CORR(J)

            CALL TCASE ( RELATE(1:7) // CORR(J)(1:6) )

            CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                        QNPARS,     QPNAMS,   QCPARS, &
                        QDPARS,     QIPARS,   QLPARS, &
                        RELATE,     REFVAL,   CNVTOL, &
                        ADJUST,     CNFINE,   RPT, &
                        GFREPI,     GFREPU,   GFREPF, &
                        MAXVAL,     NWMAX,    WORK, &
                        BAIL,       GFBAIL,    RESULT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

         END DO

      END DO


!
!   Case 5
!
!     Event DISTANCE - Test for an error when QPNAMS has
!     an empty element.
!
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RELATE = '='
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, QNPARS

         QPNAMS(1) = 'TARGET'
         QCPARS(1) = 'MOON'
         QPNAMS(2) = 'OBSERVER'
         QCPARS(2) = 'EARTH'
         QPNAMS(3) = 'ABCORR'
         QCPARS(3) = 'LT+S'

         CALL TCASE ( 'Empty value: '// QPNAMS(I) )
         QPNAMS(I) = ' '

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(MISSINGVALUE)', OK )

      END DO


!
!   Case 6
!
!     Event ANGULAR SEPARATION - Test for an error when QPNAMS has
!     an empty element.
!
      EVENT     = 'ANGULAR SEPARATION'
      QNPARS    = 8
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RELATE = '='
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, QNPARS

         QPNAMS(1) = 'TARGET1'
         QCPARS(1) =  'MOON'
         QPNAMS(2) = 'FRAME1'
         QCPARS(2) =  'NULL'
         QPNAMS(3) = 'SHAPE1'
         QCPARS(3) = 'SPHERE'
         QPNAMS(4) = 'TARGET2'
         QCPARS(4) =  'EARTH'
         QPNAMS(5) = 'FRAME2'
         QCPARS(5) =  'NULL'
         QPNAMS(6) = 'SHAPE2'
         QCPARS(6) = 'SPHERE'
         QPNAMS(7) = 'OBSERVER'
         QCPARS(7) =  'SUN'
         QPNAMS(8) = 'ABCORR'
         QCPARS(8) =  'NONE'

         CALL TCASE ( 'Empty value: '// QPNAMS(I) )
         QPNAMS(I) = ' '

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(MISSINGVALUE)', OK )

      END DO


!
!   Case 7
!
!     Event COORDINATE - Test for an error when QPNAMS has
!     an empty element.
!
      EVENT     = 'COORDINATE'
      QNPARS    = 10
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RELATE = '='
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, QNPARS

         QPNAMS(1) = 'TARGET'
         QCPARS(1) =  'MOON'

         QPNAMS(2) = 'OBSERVER'
         QCPARS(2) =  'EARTH'

         QPNAMS(3) = 'ABCORR'
         QCPARS(3) =  'NONE'

         QPNAMS(4) = 'COORDINATE SYSTEM'
         QCPARS(4) =  'RECTANGULAR'

         QPNAMS(5) = 'COORDINATE'
         QCPARS(5) =  'X'

         QPNAMS(6) = 'REFERENCE FRAME'
         QCPARS(6) =  'J2000'

         QPNAMS(7) = 'VECTOR DEFINITION'
         QCPARS(7) = 'POSITION'

         QPNAMS(8) = 'METHOD'
         QCPARS(8) = ' '

         QPNAMS(9) = 'DREF'
         QCPARS(9) =  ' '

         QPNAMS(10) = 'DVEC'
         QDPARS(1) = 0.D0
         QDPARS(2) = 0.D0
         QDPARS(3) = 0.D0

         CALL TCASE ( 'Empty value: '// QPNAMS(I) )
         QPNAMS(I) = ' '

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(MISSINGVALUE)', OK )

      END DO



!
!   Case 8
!
!     Event DISTANCE - Test for an error when ADJUST !=0 for
!     RELATE != 'ABSMAX' or 'ABSMIN
!
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'

      ADJUST    =  1.D0

      DO I = 1, NC-2

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         RELATE = CNAMES(I)

         TITLE = 'DISTANCE: Non-zero ADJUST ' // RELATE
         CALL TCASE ( TITLE )

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      END DO


!
!   Case 9
!
!     Event ANGULAR SEPARATION - Test for an error when ADJUST !=0 for
!     RELATE != 'ABSMAX'
!
      EVENT     = 'ANGULAR SEPARATION'
      QNPARS    = 8
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      QPNAMS(1) = 'TARGET1'
      QCPARS(1) =  'MOON'
      QPNAMS(2) = 'FRAME1'
      QCPARS(2) =  'NULL'
      QPNAMS(3) = 'SHAPE1'
      QCPARS(3) = 'SPHERE'
      QPNAMS(4) = 'TARGET2'
      QCPARS(4) =  'EARTH'
      QPNAMS(5) = 'FRAME2'
      QCPARS(5) =  'NULL'
      QPNAMS(6) = 'SHAPE2'
      QCPARS(6) = 'SPHERE'
      QPNAMS(7) = 'OBSERVER'
      QCPARS(7) =  'SUN'
      QPNAMS(8) = 'ABCORR'
      QCPARS(8) =  'NONE'

      ADJUST    =  1.D0

      DO I = 1, NC-2

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         RELATE = CNAMES(I)

         TITLE = 'ANGULAR SEPARATION: Non zero ADJUST ' // RELATE
         CALL TCASE ( TITLE )

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      END DO


!
!   Case 10
!
!     Event COORDINATE - Test for an error when ADJUST !=0 for
!     RELATE != 'ABSMAX'
!
      EVENT     = 'COORDINATE'
      QNPARS    = 10
      STEP      = 1.D0*SPD()
      REFVAL    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      QPNAMS(1) = 'TARGET'
      QCPARS(1) =  'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) =  'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) =  'NONE'
      QPNAMS(4) = 'COORDINATE SYSTEM'
      QCPARS(4) =  'RECTANGULAR'
      QPNAMS(5) = 'COORDINATE'
      QCPARS(5) =  'X'
      QPNAMS(6) = 'REFERENCE FRAME'
      QCPARS(6) =  'J2000'
      QPNAMS(7) = 'VECTOR DEFINITION'
      QCPARS(7) = 'POSITION'
      QPNAMS(8) = 'METHOD'
      QCPARS(8) = ' '
      QPNAMS(9) = 'DREF'
      QCPARS(9) =  ' '
      QPNAMS(10) = 'DVEC'
      QDPARS(1) = 0.D0
      QDPARS(2) = 0.D0
      QDPARS(3) = 0.D0

      ADJUST    =  1.D0

      DO I = 1, NC-2

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         RELATE = CNAMES(I)

         TITLE = 'COORDINATE: Non zero ADJUST ' // RELATE
         CALL TCASE ( TITLE )

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, &
                       QNPARS,     QPNAMS,   QCPARS, &
                       QDPARS,     QIPARS,   QLPARS, &
                       RELATE,     REFVAL,   CNVTOL, &
                       ADJUST,     CNFINE,   RPT, &
                       GFREPI,     GFREPU,   GFREPF, &
                       MAXVAL,     NWMAX,    WORK, &
                       BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      END DO


!
! Case n
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END

!
!23456789012345678901234567890123456789012345678901234567890123456789012
!