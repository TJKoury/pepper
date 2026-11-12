!$Procedure F_GFSCRP ( Test GF sub-observer coord progress reporting )

      SUBROUTINE F_GFSCRP ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF subsystem's use of passed-in progress reporting and
!     interrupt handling functions for the "sub-observer point
!     coordinate" quantity.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
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
!     This family tests the GF subsystem's use of the default progress
!     reporting functions in the context of searches involving
!     sub-observer point coordinate constraints. The point is to test
!     the callers of the functions, not the functions themselves.
!
!     The routines exercised by this test family are
!
!        GFEVNT
!        ZZGFREL
!        ZZGFSOLV
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-MAR-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!


      EXTERNAL T_GFREPI
      EXTERNAL T_GFREPU
      EXTERNAL T_GFREPF

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfscrp.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfscrp.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 200 )

      INTEGER               MXPASS
      PARAMETER           ( MXPASS = 20 )

      INTEGER               MXCALL
      PARAMETER           ( MXCALL = 20000 )

!
!     Maximum progress report prefix or suffix length.
!     MXMSG is declared in zzgf.inc.
!
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = MXMSG )

      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 20 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    GQUANT
      CHARACTER*(MAXMSG)    MSGLOG ( 2, MXPASS )
      CHARACTER*(LNSIZE)    OP
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(LNSIZE)    QCPARS ( MAXPAR )
      CHARACTER*(LNSIZE)    QPNAMS ( MAXPAR )
      CHARACTER*(MAXMSG)    XPREFX

      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CENTRL
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      CNFLOG ( LBCELL : MAXWIN, MXPASS )
      DOUBLE PRECISION      CNFMES
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      EXPCNF ( LBCELL : MAXWIN )
      DOUBLE PRECISION      MEASUR
      DOUBLE PRECISION      QDPARS ( MAXPAR )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      REPLOG ( 3, MXCALL )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
      DOUBLE PRECISION      XMESUR

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               ISQLOG ( MXPASS )
      INTEGER               J
      INTEGER               MW
      INTEGER               NCALLS
      INTEGER               NIVL
      INTEGER               NPASS
      INTEGER               NUPDAT
      INTEGER               NW
      INTEGER               QIPARS ( MAXPAR )
      INTEGER               QNPARS
      INTEGER               SEQLOG ( MXCALL )
      INTEGER               TOTCAL
      INTEGER               TRMLOG ( MXCALL )
      INTEGER               XSQLOG ( MXCALL )

      LOGICAL               BAIL
      LOGICAL               QLPARS ( MAXPAR )
      LOGICAL               RPT

!
!     Saved variables
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFSCRP' )





!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load a PCK file.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************





!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate LOCMIN search ' &
      //           'using GFEVNT.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO

!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     when a local maximum occurs.
!
!
!     Note: the epoch below was derived using a DE ephemeris and
!     probably doesn't match the value that would be obtained
!     using the test SPK.
!
      CALL STR2ET ( '2000 JUN 21 01:27:06.528438 TDB', CENTRL )

      NIVL   = 3
      CNFMES = 0.D0

      DO I = 1, NIVL

         ET0 = -800.D0 + (I-2)*3600.D0 + CENTRL
         ET1 =  ET0    + 1600.D0

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CNFMES = CNFMES + ET1 - ET0

      END DO


      GQUANT     = 'COORDINATE'

      QNPARS     = 10
      QPNAMS(1)  = 'TARGET'
      QPNAMS(2)  = 'OBSERVER'
      QPNAMS(3)  = 'ABCORR'
      QPNAMS(4)  = 'COORDINATE SYSTEM'
      QPNAMS(5)  = 'COORDINATE'
      QPNAMS(6)  = 'REFERENCE FRAME'
      QPNAMS(7)  = 'VECTOR DEFINITION'
      QPNAMS(8)  = 'METHOD'
      QPNAMS(9)  = 'DREF'
      QPNAMS(10) = 'DVEC'

      QCPARS(1)  = 'EARTH'
      QCPARS(2)  = 'SUN'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LATITUDE'
      QCPARS(6)  = 'IAU_EARTH'
      QCPARS(7)  = 'SUB-OBSERVER POINT'
      QCPARS(8)  = 'NEAR POINT: ELLIPSOID'
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'LOCMIN'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity local extrema searches (excluding longitude, RA,
!     etc.) require one pass.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there was 1 progress reporting pass.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity extrema searches require 1 pass,
!     so we expect one report termination call.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate ABSMIN search ' &
      //           'using GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator.
!
      OP = 'ABSMIN'

!
!     Initialize inputs for the search. We'll keep the
!     previous confinement window.
!
      CALL SCARDD ( 0, RESULT )
!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity absolute extrema searches (excluding longitude,
!     RA, etc.) require one pass.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there was 1 progress reporting pass.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity extrema searches require 1 pass,
!     so we expect one report termination call.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate LOCMAX search ' &
      //           'using GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator.
!
      OP = 'LOCMAX'

!
!     This search will produce an empty result window, but that
!     has no bearing on progress reporting.
!
!     Initialize inputs for the search. We'll keep the
!     previous confinement window.
!
      CALL SCARDD ( 0, RESULT )
!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity local extremum searches (excluding longitude, RA,
!     etc.) require one pass.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there was 1 progress reporting pass.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity extrema searches require 1 pass,
!     so we expect one report termination call.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate ABSMAX search ' &
      //           'using GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator.
!
      OP = 'ABSMAX'

!
!     This search will produce an empty result window, but that
!     has no bearing on progress reporting.
!
!     Initialize inputs for the search. We'll keep the
!     previous confinement window.
!
      CALL SCARDD ( 0, RESULT )
!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity absolute extrema searches (excluding longitude,
!     RA, etc.) require one pass.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there was 1 progress reporting pass.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity extrema searches require 1 pass,
!     so we expect one report termination call.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate > inequality ' &
      //           'search using GFEVNT.' )


!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO

      MW     = MAXWIN
      NW     = NWMAX

      OP     = '>'
      REFVAL = 3.8D5

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity inequality searches (excluding longitude, RA,
!     etc.) require 2 passes.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there were 2 initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sub-observer point coordinate inequality searches require 2
!     passes.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!
!         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window. Since the first pass covers the
!     confinement window, and the second and third partial passes
!     also combine to cover the confinement window, the total
!     progress measure should be twice that of the confinement
!     window.
!
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate < inequality ' &
      //           'search using GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator.
!
      OP = '<'

!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO



      GQUANT    = 'COORDINATE'


      MW     = MAXWIN
      NW     = NWMAX

      REFVAL = 3.8D5

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity inequality searches (excluding longitude, RA,
!     etc.) require 2 passes.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there were 2 initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sub-observer point coordinate inequality searches require 2
!     passes.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!
!         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window. Since the first pass covers the
!     confinement window, and the second and third partial passes
!     also combine to cover the confinement window, the total
!     progress measure should be twice that of the confinement
!     window.
!
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate equality ' &
      //           'search using GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator and the confinement window.
!
      OP = '='

!
!     The confinement window actually used by ZZGFREL in an
!     equality search is obtained by expanding the input
!     confinement window by 1 second.
!
      CALL SSIZED ( MAXWIN, EXPCNF )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


      CALL COPYD  ( CNFINE, EXPCNF )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNEXPD ( ADDWIN, ADDWIN, EXPCNF )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

      MW     = MAXWIN
      NW     = NWMAX

      REFVAL = 3.8D5

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity equality searches (excluding longitude, RA,
!     etc.) require 2 passes.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there were 2 initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     sub-observer point coordinate equality searches require 2 passes.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!
!         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window. Since the first pass covers the
!     confinement window, and the second and third partial passes
!     also combine to cover the confinement window, the total
!     progress measure should be twice that of the expanded
!     confinement window.
!
      XMESUR = 0.D0

      DO I = 2, CARDD( EXPCNF ), 2

         XMESUR = XMESUR + EXPCNF(I) - EXPCNF(I-1)

      END DO

      XMESUR = 2 * XMESUR

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )







!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate adjusted ' &
      //           'ABSMIN search using GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator.
!
      OP     = 'ABSMIN'
      ADJUST = 1.D0

!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


      GQUANT    = 'COORDINATE'

      MW     = MAXWIN
      NW     = NWMAX

      REFVAL = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity adjusted absolute extrema searches (excluding
!     longitude, RA, etc.) require 2 passes.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there were 2 initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     sub-observer point coordinate adjusted absolute extrema searches
!     require 2
!     passes.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!
!         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window. Since the first pass covers the
!     confinement window, and the second and third partial passes
!     also combine to cover the confinement window, the total
!     progress measure should be twice that of the confinement
!     window.
!
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a sub-observer point coordinate adjusted ' &
      //           'ABSMAX search using GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator.
!
      OP     = 'ABSMAX'
      ADJUST = 1.D0

!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize the confinement window log.
!
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


      GQUANT = 'COORDINATE'

      MW     = MAXWIN
      NW     = NWMAX

      REFVAL = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, &
                    QPNAMS,   QCPARS,   QDPARS,   QIPARS, &
                    QLPARS,   OP,       REFVAL,   TOL, &
                    ADJUST,   CNFINE,   RPT,      T_GFREPI, &
                    T_GFREPU, T_GFREPF, MW,       NW, &
                    WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Scalar quantity adjusted absolute extrema searches (excluding
!     longitude, RA, etc.) require 2 passes.
!
!     Look up the log of the progress report initialization calls. We
!     expect that there were 2 initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

!
!     Check the sequence number of the first call.
!
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

!
!     Check the progress report message prefixes and suffixes.
!
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Coordinate pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK )

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', &
                       'done.', OK )

      END DO

!
!     Get the log of the report termination calls.
!
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sub-observer point coordinate adjusted absolute extrema searches
!     require 2 passes.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

!
!     Retrieve the log of calls made to the update routine.
!
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the updates for each pass.
!
      TOTCAL = 0

      DO I = 1, NPASS
!
!        Check the sequence numbers of the calls. They should
!        range from the sequence number of the last init
!        call +1 to the sequence number of the Ith termination
!        call -1. They should be in increasing
!        order.
!
!         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), &
                       '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

!
!     Make sure that:
!
!       - Each update time lies within the corresponding interval.
!
!       - Within each interval, the update times are monotonically
!         non-decreasing.
!
!     Also record the measure of the reported progress.
!
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
!
!              The current interval is the same as the previous one.
!
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', &
                                    REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

!
!     Compare the measure of the reported progress to that of the
!     confinement window. Since the first pass covers the
!     confinement window, and the second and third partial passes
!     also combine to cover the confinement window, the total
!     progress measure should be twice that of the confinement
!     window.
!
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )


      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!     To be added, if necessary.
!

!
!---- Case -------------------------------------------------------------
!

      CALL T_SUCCESS ( OK )
      RETURN
      END

