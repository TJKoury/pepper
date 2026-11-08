!$Procedure F_GFLORP ( Test GF longitude progress reporting )

      SUBROUTINE F_GFLORP ( OK )

! $ Abstract
!
!     Test the GF subsystem's use of passed-in progress reporting and
!     interrupt handling functions for the "longitude coordinate"
!     quantity.
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

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
!     longitude or right ascension constraints. The point is to
!     test the callers of the functions, not the functions themselves.
!
!     The routines exercised by this test family are
!
!        GFEVNT
!        ZZGFLONG
!        ZZGFCSLV
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
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD

      INTEGER               CARDD

      LOGICAL               GFBAIL

      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFSTEP
      EXTERNAL              T_GFREPI
      EXTERNAL              T_GFREPU
      EXTERNAL              T_GFREPF

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gflorp.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gflorp.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-6 )


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
      DOUBLE PRECISION      MEASUR
      DOUBLE PRECISION      QDPARS ( MAXPAR )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      REPLOG ( 3, MXCALL )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )

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
      CALL TOPEN ( 'F_GFLORP' )





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
!     Load a PCK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

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
      CALL TCASE ( 'Run a longitude LOCMIN search using ' &
      //           'GFEVNT.' )



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
!     when a local minimum occurs.
!
!
!     Note: the epoch below was derived using SPK and
!     PCK files created by the test utilities called above.
!
      CALL STR2ET ( '2000 JUN 05 04:09:54.717 TDB', CENTRL )

      NIVL   = 3
      CNFMES = 0.D0

      DO I = 1, NIVL

         ET0 = -SPD() + 4*(I-2)*SPD() + CENTRL
         ET1 =  ET0   + 2*SPD()

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CNFMES = CNFMES + ET1 - ET0

      END DO


      GQUANT    = 'COORDINATE'

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
      QCPARS(2)  = 'MOON'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'IAU_MOON'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
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

      STEP   = 3000.D0

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
!     expect that there were 6 progress reporting passes.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 6, 0, OK )

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

         XPREFX = 'Coordinate pass * of 6'
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
!     Longitude local extrema searches require 6 passes,
!     so we expect 6 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 6, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )







!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude LOCMAX search using ' &
      //           'GFEVNT.' )

!
!     We'll use all inputs from the previous case except for the
!     relational operator and the confinement window.
!
      OP = 'LOCMAX'

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     when a local maximum occurs.
!
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: the epoch below was derived using SPK and
!     PCK files created by the test utilities called above.
!
      CALL STR2ET ( '2000 JUN 16 04:37:06.960 TDB', CENTRL )

      NIVL   = 3
      CNFMES = 0.D0

      DO I = 1, NIVL

         ET0 = -SPD() + 4*(I-2)*SPD() + CENTRL
         ET1 =  ET0   + 2*SPD()

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CNFMES = CNFMES + ET1 - ET0

      END DO

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )



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
!     expect that there were 6 progress reporting passes.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 6, 0, OK )

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

         XPREFX = 'Coordinate pass * of 6'
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
!     Longitude local extrema searches require 6 passes,
!     so we expect 6 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 6, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude ABSMAX search using ' &
      //           'GFEVNT. Max is in 2nd quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
      CALL STR2ET ( '1999 SEP 25', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 150*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 150*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMAX'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA unadjusted absolute extrema searches require
!     7 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
!     Longitude absolute extrema searches require 7 passes,
!     so we expect 7 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude ABSMAX search using ' &
      //           'GFEVNT. Max is in right half circle.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
      CALL STR2ET ( '1999 SEP 25', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 90*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 90*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMAX'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA unadjusted absolute extrema searches require
!     7 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
!     Longitude absolute extrema searches require 7 passes,
!     so we expect 7 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude ABSMAX search using ' &
      //           'GFEVNT. Max is in 4th quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
      CALL STR2ET ( '1999 SEP 25', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMAX'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA unadjusted absolute extrema searches require
!     7 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
!     Longitude absolute extrema searches require 7 passes,
!     so we expect 7 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude ABSMIN search using ' &
      //           'GFEVNT. Min is in 2nd quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
      CALL STR2ET ( '2000 JUL 15', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMIN'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA unadjusted absolute extrema searches require
!     7 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
!     Longitude absolute extrema searches require 7 passes,
!     so we expect 7 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude ABSMIN search using ' &
      //           'GFEVNT. Min is in right half.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
      CALL STR2ET ( '2000 FEB 15', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMIN'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA unadjusted absolute extrema searches require
!     7 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
!     Longitude absolute extrema searches require 7 passes,
!     so we expect 7 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )






!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude ABSMIN search using ' &
      //           'GFEVNT. Min is in 3rd quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
      CALL STR2ET ( '2000 OCT 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMIN'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA unadjusted absolute extrema searches require
!     7 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
!     Longitude absolute extrema searches require 7 passes,
!     so we expect 7 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )









!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run an RA adjusted ABSMAX search using ' &
      //           'GFEVNT. Max is in 4th quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 FEB 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'RA/DEC'
      QCPARS(5)  = 'RIGHT ASCENSION'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMAX'
      REFVAL = 0.D0

!
!     Note non-zero adjustment value:
!
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA adjusted absolute extrema searches require
!     9 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
!     Longitude adjusted absolute extrema searches require 7-9 passes;
!     in this case we expect 9 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )






!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run an RA adjusted ABSMAX search using ' &
      //           'GFEVNT. Max is in left half circle.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 SEP 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'RA/DEC'
      QCPARS(5)  = 'RIGHT ASCENSION'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMAX'
      REFVAL = 0.D0

!
!     Note non-zero adjustment value:
!
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA adjusted absolute extrema searches require
!     9 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
!     Longitude adjusted absolute extrema searches require 7-9 passes;
!     in this case we expect 9 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )





!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run an RA adjusted ABSMAX search using ' &
      //           'GFEVNT. Max is in 1st quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 APR 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'RA/DEC'
      QCPARS(5)  = 'RIGHT ASCENSION'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMAX'
      REFVAL = 0.D0

!
!     Note non-zero adjustment value:
!
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA adjusted absolute extrema searches require
!     9 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
!     Longitude adjusted absolute extrema searches require 7-9 passes;
!     in this case we expect 9 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run an RA adjusted ABSMIN search using ' &
      //           'GFEVNT. Min is in 4th quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 FEB 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'RA/DEC'
      QCPARS(5)  = 'RIGHT ASCENSION'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMIN'
      REFVAL = 0.D0

!
!     Note non-zero adjustment value:
!
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA adjusted absolute extrema searches require
!     9 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
!     Longitude adjusted absolute extrema searches require 7-9 passes;
!     in this case we expect 9 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )





!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run an RA adjusted ABSMIN search using ' &
      //           'GFEVNT. Min is in left half circle.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 SEP 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'RA/DEC'
      QCPARS(5)  = 'RIGHT ASCENSION'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMIN'
      REFVAL = 0.D0

!
!     Note non-zero adjustment value:
!
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA adjusted absolute extrema searches require
!     9 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
!     Longitude adjusted absolute extrema searches require 7-9 passes;
!     in this case we expect 9 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )






!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run an RA adjusted ABSMIN search using ' &
      //           'GFEVNT. Min is in 1st quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 APR 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'RA/DEC'
      QCPARS(5)  = 'RIGHT ASCENSION'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = 'ABSMIN'
      REFVAL = 0.D0

!
!     Note non-zero adjustment value:
!
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA adjusted absolute extrema searches require
!     9 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
!     Longitude adjusted absolute extrema searches require 7-9 passes;
!     in this case we expect 9 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )





!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run an RA < (inequality) search using ' &
      //           'GFEVNT. Reference value is in 1st quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 APR 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'RA/DEC'
      QCPARS(5)  = 'RIGHT ASCENSION'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = '<'
      REFVAL = PI()/4

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA inequality searches require 8 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 8, 0, OK )

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

         XPREFX = 'Coordinate pass * of 8'
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
!     Longitude inequality searches require 8 passes,
!     so we expect 8 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 8, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude > (inequality) search using ' &
      //           'GFEVNT. Reference value is in 1st quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 APR 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = '>'
      REFVAL = PI()/4

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA inequality searches require 8 passes.
!
!     Look up the log of the progress report initialization calls.
!
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 8, 0, OK )

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

         XPREFX = 'Coordinate pass * of 8'
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
!     Longitude inequality searches require 8 passes,
!     so we expect 8 report termination calls.
!
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 8, 0, OK )

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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )





!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Run a longitude equality search using ' &
      //           'GFEVNT. Reference value is in 1st quadrant.' )



      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Initialize the progress reporting test utility package.
!
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     For absolute extrema, the Sun and Earth provide a good test
!     case, since the Earth-Sun position vector traverses the full
!     range of possible longitudes.
!
!     Create a confinement window with 3 intervals. We pick
!     as the central epoch of the middle interval a time
!     near the epoch of the absolute minimum.
!
!
      CALL STR2ET ( '2000 APR 1', ET0 )

      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ET0 = ET1 + SPD()
      ET1 = ET0 + 10*SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'COORDINATE'

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

      QCPARS(1)  = 'SUN'
      QCPARS(2)  = 'EARTH'
      QCPARS(3)  = 'NONE'
      QCPARS(4)  = 'LATITUDINAL'
      QCPARS(5)  = 'LONGITUDE'
      QCPARS(6)  = 'J2000'
      QCPARS(7)  = 'POSITION'
      QCPARS(8)  = ' '
      QCPARS(9)  = ' '

      CALL CLEARD ( 3, QDPARS )

      MW     = MAXWIN
      NW     = NWMAX

      OP     = '='
      REFVAL = PI()/4

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
!     Longitude/RA equality searches require 2 passes.
!
!     Look up the log of the progress report initialization calls.
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
!     Longitude equality searches require 2 passes,
!     so we expect 2 report termination calls.
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
!     Find the sum of the measures of the confinement windows
!     for each pass.
!
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

!
!     Compare the measure of the reported progress to the sum of
!     the measures of the confinement windows used for each
!     search.
!
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )



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

