C$Procedure F_GFSPRP ( Test GF angular separation progress reporting )
  
      SUBROUTINE F_GFSPRP ( OK )

C$ Abstract
C
C     Test the GF subsystem's use of passed-in progress reporting and 
C     interrupt handling functions for the "angular separation" 
C     quantity.
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
C     This family tests the GF subsystem's use of the default progress
C     reporting functions in the context of searches involving angular
C     separation constraints. The point is to test the callers of the
C     functions, not the functions themselves.
C
C     The routines exercised by this test family are
C
C        GFEVNT
C        ZZGFREL
C        ZZGFSOLV
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 08-MAR-2009 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      INTEGER               CARDD

      LOGICAL               GFBAIL

      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFSTEP
      EXTERNAL              T_GFREPI
      EXTERNAL              T_GFREPU
      EXTERNAL              T_GFREPF
      
C     
C     Local parameters
C
      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         NIK
      PARAMETER           ( NIK    = 'nat.ti' )


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

C
C     Maximum progress report prefix or suffix length.
C     MXMSG is declared in zzgf.inc.
C
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = MXMSG )

      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 20 )

C
C     Local Variables
C     
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

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
 

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFSPRP' )



      

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create and load Nat's solar system SPK, PCK/FK, and IK
C     files.
C     
      CALL NATPCK ( NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATSPK ( NSPK, .TRUE., HANDLE    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATIK  ( NIK,  NSPK,   NPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


  


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation LOCMIN search using '
     .//           'GFEVNT.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO

C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     when a local minimum occurs.
C
      CENTRL = 0.D0

      NIVL   = 3
      CNFMES = 0.D0

      DO I = 1, NIVL

         ET0 = -800.D0 + (I-2)*3600.D0 + CENTRL
         ET1 =  ET0    + 1600.D0

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CNFMES = CNFMES + ET1 - ET0

      END DO


      GQUANT    = 'ANGULAR SEPARATION'

      QNPARS    = 9
      QPNAMS(1) = 'TARGET1'
      QPNAMS(2) = 'FRAME1'
      QPNAMS(3) = 'SHAPE1'
      QPNAMS(4) = 'TARGET2'
      QPNAMS(5) = 'FRAME2'
      QPNAMS(6) = 'SHAPE2'
      QPNAMS(7) = 'OBSERVER'
      QPNAMS(8) = 'ABCORR'
      QPNAMS(9) = 'REFERENCE FRAME'

      QCPARS(1) = 'ALPHA'
      QCPARS(2) = ' '
      QCPARS(3) = 'POINT'
      QCPARS(4) = 'BETA'
      QCPARS(5) = '  '
      QCPARS(6) = 'POINT'
      QCPARS(7) = 'SUN'
      QCPARS(8) = 'NONE'
      QCPARS(9) = 'J2000'

      MW     = MAXWIN
      NW     = NWSEP

      OP     = 'LOCMIN'
      REFVAL = 0.D0

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )
 
      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity local extrema searches (excluding longitude, RA,
C     etc.) require one pass.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there was 1 progress reporting pass.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity extrema searches require 1 pass,
C     so we expect one report termination call.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation ABSMIN search using '
     .//           'GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator.
C
      OP = 'ABSMIN'

C
C     Initialize inputs for the search. We'll keep the
C     previous confinement window.
C
      CALL SCARDD ( 0, RESULT )
C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO
 

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )

 
      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity absolute extrema searches (excluding longitude,
C     RA, etc.) require one pass.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there was 1 progress reporting pass.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity extrema searches require 1 pass,
C     so we expect one report termination call.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation LOCMAX search using '
     .//           'GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator.
C
      OP = 'LOCMAX'

C
C     This search will produce an empty result window, but that
C     has no bearing on progress reporting.
C
C     Initialize inputs for the search. We'll keep the
C     previous confinement window.
C
      CALL SCARDD ( 0, RESULT )
C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO
 

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )

 
      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity local extremum searches (excluding longitude, RA,
C     etc.) require one pass.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there was 1 progress reporting pass.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity extrema searches require 1 pass,
C     so we expect one report termination call.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation ABSMAX search using '
     .//           'GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator.
C
      OP = 'ABSMAX'

C
C     This search will produce an empty result window, but that
C     has no bearing on progress reporting.
C
C     Initialize inputs for the search. We'll keep the
C     previous confinement window.
C
      CALL SCARDD ( 0, RESULT )
C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO
 

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )

 
      MW = MAXWIN

      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity absolute extrema searches (excluding longitude,
C     RA, etc.) require one pass.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there was 1 progress reporting pass.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 1, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 1'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity extrema searches require 1 pass,
C     so we expect one report termination call.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation > inequality search '
     .//           'using GFEVNT.' )


C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO

      MW     = MAXWIN
      NW     = NWSEP

      OP     = '>'
      REFVAL = 3.8D5

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity inequality searches (excluding longitude, RA,
C     etc.) require 2 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Angular separation inequality searches require 2 passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C
C         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window. Since the first pass covers the 
C     confinement window, and the second and third partial passes 
C     also combine to cover the confinement window, the total
C     progress measure should be twice that of the confinement
C     window.
C
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation < inequality search '
     .//           'using GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator.
C
      OP = '<'

C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO
 
 

      GQUANT    = 'ANGULAR SEPARATION'


      MW     = MAXWIN
      NW     = NWSEP

      REFVAL = 3.8D5

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity inequality searches (excluding longitude, RA,
C     etc.) require 2 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Angular separation inequality searches require 2 passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C
C         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window. Since the first pass covers the 
C     confinement window, and the second and third partial passes 
C     also combine to cover the confinement window, the total
C     progress measure should be twice that of the confinement
C     window.
C
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation equality search using '
     .//           'GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator and the confinement window. 
C
      OP = '='

C
C     The confinement window actually used by ZZGFREL in an
C     equality search is obtained by expanding the input
C     confinement window by 1 second.
C
      CALL SSIZED ( MAXWIN, EXPCNF )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO
 

      CALL COPYD  ( CNFINE, EXPCNF )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNEXPD ( ADDWIN, ADDWIN, EXPCNF )
      CALL CHCKXC ( .FALSE., ' ',   OK )



      GQUANT    = 'ANGULAR SEPARATION'

      MW     = MAXWIN
      NW     = NWSEP

      REFVAL = 3.8D5

      ADJUST = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity equality searches (excluding longitude, RA,
C     etc.) require 2 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Angular separation equality searches require 2 passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C
C         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window. Since the first pass covers the 
C     confinement window, and the second and third partial passes 
C     also combine to cover the confinement window, the total
C     progress measure should be twice that of the expanded 
C     confinement window. 
C
      XMESUR = 0.D0

      DO I = 2, CARDD( EXPCNF ), 2

         XMESUR = XMESUR + EXPCNF(I) - EXPCNF(I-1)

      END DO

      XMESUR = 2 * XMESUR

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )







C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation adjusted ABSMIN search '
     .//           'using GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator.
C
      OP     = 'ABSMIN'
      ADJUST = 1.D0

C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO
 

      GQUANT    = 'ANGULAR SEPARATION'
 
      MW     = MAXWIN
      NW     = NWSEP

      REFVAL = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity adjusted absolute extrema searches (excluding
C     longitude, RA, etc.) require 2 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Angular separation adjusted absolute extrema searches require 2
C     passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C
C         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window. Since the first pass covers the 
C     confinement window, and the second and third partial passes 
C     also combine to cover the confinement window, the total
C     progress measure should be twice that of the confinement
C     window.
C
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an angular separation adjusted ABSMAX search '
     .//           'using GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator.
C
      OP     = 'ABSMAX'
      ADJUST = 1.D0

C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


      GQUANT = 'ANGULAR SEPARATION'

      MW     = MAXWIN
      NW     = NWSEP

      REFVAL = 0.D0

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 300.D0

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFEVNT ( GFSTEP,   GFREFN,   GQUANT,   QNPARS, 
     .              QPNAMS,   QCPARS,   QDPARS,   QIPARS, 
     .              QLPARS,   OP,       REFVAL,   TOL, 
     .              ADJUST,   CNFINE,   RPT,      T_GFREPI,
     .              T_GFREPU, T_GFREPF, MW,       NW,    
     .              WORK,     BAIL,     GFBAIL,   RESULT   )

      CALL CHCKXC ( .FALSE., ' ', OK )


      MW = MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NPASS, ISQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scalar quantity adjusted absolute extrema searches (excluding
C     longitude, RA, etc.) require 2 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, NPASS

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XPREFX = 'Angular separation pass * of 2'
         CALL REPMI ( XPREFX, '*', I, XPREFX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSC ( QNAME, MSGLOG(1,I), '=', XPREFX, OK ) 

         CALL CHCKSC ( 'Suffix', MSGLOG(2,I), '=', 
     .                 'done.', OK )

      END DO

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Angular separation adjusted absolute extrema searches require 2
C     passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 2, 0, OK )

C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the updates for each pass.
C
      TOTCAL = 0

      DO I = 1, NPASS
C
C        Check the sequence numbers of the calls. They should
C        range from the sequence number of the last init
C        call +1 to the sequence number of the Ith termination
C        call -1. They should be in increasing
C        order.
C
C         WRITE (*,*) I, ISQLOG(I), TRMLOG(I)

         NCALLS = TRMLOG(I) - ISQLOG(I) - 1

         DO J = 1, NCALLS
            XSQLOG(J) = ISQLOG(I)+J
         END DO

         QNAME = 'Update SEQLOG, pass *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         
         CALL CHCKAI ( QNAME, SEQLOG( TOTCAL+1 ), 
     .                 '=',   XSQLOG,                NCALLS, OK )

         TOTCAL = TOTCAL + NCALLS

      END DO

C
C     Make sure that:
C
C       - Each update time lies within the corresponding interval.
C
C       - Within each interval, the update times are monotonically
C         non-decreasing.
C 
C     Also record the measure of the reported progress.
C
      MEASUR = 0.D0

      DO I = 1, NUPDAT

         QNAME = '(0) Update time no. *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD( QNAME, REPLOG (3,I), '>=', REPLOG(1,I), 0.D0, OK )
         CALL CHCKSD( QNAME, REPLOG (3,I), '<=', REPLOG(2,I), 0.D0, OK )

         IF ( I .GT. 1 ) THEN

            IF ( REPLOG(1,I) .EQ. REPLOG(1,I-1) ) THEN
C
C              The current interval is the same as the previous one.
C
               QNAME = '(1) Update time no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( QNAME, REPLOG(3,I),   '>=', 
     .                              REPLOG(3,I-1), 0.D0, OK )


               MEASUR = MEASUR + REPLOG(3,I) - REPLOG(3,I-1)

            END IF

         END IF

      END DO

C
C     Compare the measure of the reported progress to that of the 
C     confinement window. Since the first pass covers the 
C     confinement window, and the second and third partial passes 
C     also combine to cover the confinement window, the total
C     progress measure should be twice that of the confinement
C     window.
C
      XMESUR = 2*CNFMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )

 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )
 

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C     To be added, if necessary.
C

C
C---- Case -------------------------------------------------------------
C
 
      CALL T_SUCCESS ( OK )
      RETURN
      END

