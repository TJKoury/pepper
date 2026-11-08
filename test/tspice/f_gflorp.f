C$Procedure F_GFLORP ( Test GF longitude progress reporting )
  
      SUBROUTINE F_GFLORP ( OK )

C$ Abstract
C
C     Test the GF subsystem's use of passed-in progress reporting and 
C     interrupt handling functions for the "longitude coordinate" 
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
C     reporting functions in the context of searches involving
C     longitude or right ascension constraints. The point is to
C     test the callers of the functions, not the functions themselves.
C
C     The routines exercised by this test family are
C
C        GFEVNT
C        ZZGFLONG
C        ZZGFCSLV
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
      
C     
C     Local parameters
C
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
      CALL TOPEN ( 'F_GFLORP' )



      

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
C     Load a PCK.
C
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

C
C     Load an SPK file as well.
C     
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


  


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude LOCMIN search using '
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
C
C     Note: the epoch below was derived using SPK and
C     PCK files created by the test utilities called above.
C     
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
C     expect that there were 6 progress reporting passes.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 6, 0, OK )

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

         XPREFX = 'Coordinate pass * of 6'
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
C     Longitude local extrema searches require 6 passes,
C     so we expect 6 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 6, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )







C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude LOCMAX search using '
     .//           'GFEVNT.' )

C
C     We'll use all inputs from the previous case except for the
C     relational operator and the confinement window.
C
      OP = 'LOCMAX'

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     when a local maximum occurs.
C
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: the epoch below was derived using SPK and
C     PCK files created by the test utilities called above.
C     
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

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )



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
C     expect that there were 6 progress reporting passes.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 6, 0, OK )

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

         XPREFX = 'Coordinate pass * of 6'
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
C     Longitude local extrema searches require 6 passes,
C     so we expect 6 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 6, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude ABSMAX search using '
     .//           'GFEVNT. Max is in 2nd quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
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
C     Longitude/RA unadjusted absolute extrema searches require
C     7 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
C     Longitude absolute extrema searches require 7 passes,
C     so we expect 7 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude ABSMAX search using '
     .//           'GFEVNT. Max is in right half circle.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
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
C     Longitude/RA unadjusted absolute extrema searches require
C     7 passes. 
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
C     Longitude absolute extrema searches require 7 passes,
C     so we expect 7 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude ABSMAX search using '
     .//           'GFEVNT. Max is in 4th quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
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
C     Longitude/RA unadjusted absolute extrema searches require
C     7 passes. 
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
C     Longitude absolute extrema searches require 7 passes,
C     so we expect 7 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )


 
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude ABSMIN search using '
     .//           'GFEVNT. Min is in 2nd quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
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
C     Longitude/RA unadjusted absolute extrema searches require
C     7 passes. 
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
C     Longitude absolute extrema searches require 7 passes,
C     so we expect 7 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude ABSMIN search using '
     .//           'GFEVNT. Min is in right half.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
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
C     Longitude/RA unadjusted absolute extrema searches require
C     7 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
C     Longitude absolute extrema searches require 7 passes,
C     so we expect 7 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )






C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude ABSMIN search using '
     .//           'GFEVNT. Min is in 3rd quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
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
C     Longitude/RA unadjusted absolute extrema searches require
C     7 passes. 
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 7, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7'
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
C     Longitude absolute extrema searches require 7 passes,
C     so we expect 7 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 7, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )









C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an RA adjusted ABSMAX search using '
     .//           'GFEVNT. Max is in 4th quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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

C
C     Note non-zero adjustment value:
C
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
C     Longitude/RA adjusted absolute extrema searches require
C     9 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
C     Longitude adjusted absolute extrema searches require 7-9 passes;
C     in this case we expect 9 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )






C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an RA adjusted ABSMAX search using '
     .//           'GFEVNT. Max is in left half circle.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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

C
C     Note non-zero adjustment value:
C
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
C     Longitude/RA adjusted absolute extrema searches require
C     9 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
C     Longitude adjusted absolute extrema searches require 7-9 passes;
C     in this case we expect 9 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )





C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an RA adjusted ABSMAX search using '
     .//           'GFEVNT. Max is in 1st quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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

C
C     Note non-zero adjustment value:
C
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
C     Longitude/RA adjusted absolute extrema searches require
C     9 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
C     Longitude adjusted absolute extrema searches require 7-9 passes;
C     in this case we expect 9 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an RA adjusted ABSMIN search using '
     .//           'GFEVNT. Min is in 4th quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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

C
C     Note non-zero adjustment value:
C
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
C     Longitude/RA adjusted absolute extrema searches require
C     9 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
C     Longitude adjusted absolute extrema searches require 7-9 passes;
C     in this case we expect 9 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )





C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an RA adjusted ABSMIN search using '
     .//           'GFEVNT. Min is in left half circle.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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

C
C     Note non-zero adjustment value:
C
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
C     Longitude/RA adjusted absolute extrema searches require
C     9 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
C     Longitude adjusted absolute extrema searches require 7-9 passes;
C     in this case we expect 9 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )






C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an RA adjusted ABSMIN search using '
     .//           'GFEVNT. Min is in 1st quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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

C
C     Note non-zero adjustment value:
C
      ADJUST = 2.D0 * RPD()

      TOL    = CNVTOL

      RPT    = .TRUE.
      BAIL   = .FALSE.

      STEP   = 1.D5

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
C     Longitude/RA adjusted absolute extrema searches require
C     9 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 9, 0, OK )

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

         XPREFX = 'Coordinate pass * of 7-9'
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
C     Longitude adjusted absolute extrema searches require 7-9 passes;
C     in this case we expect 9 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 9, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )

 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an RA < (inequality) search using '
     .//           'GFEVNT. Reference value is in 1st quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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
C     Longitude/RA inequality searches require 8 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 8, 0, OK )

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

         XPREFX = 'Coordinate pass * of 8'
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
C     Longitude inequality searches require 8 passes,
C     so we expect 8 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 8, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )

 


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude > (inequality) search using '
     .//           'GFEVNT. Reference value is in 1st quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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
C     Longitude/RA inequality searches require 8 passes.
C
C     Look up the log of the progress report initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 8, 0, OK )

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

         XPREFX = 'Coordinate pass * of 8'
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
C     Longitude inequality searches require 8 passes,
C     so we expect 8 report termination calls.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 8, 0, OK )

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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )

 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run a longitude equality search using '
     .//           'GFEVNT. Reference value is in 1st quadrant.' )


 
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     For absolute extrema, the Sun and Earth provide a good test 
C     case, since the Earth-Sun position vector traverses the full
C     range of possible longitudes.
C
C     Create a confinement window with 3 intervals. We pick
C     as the central epoch of the middle interval a time
C     near the epoch of the absolute minimum.
C
C     
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
C     Longitude/RA equality searches require 2 passes.
C
C     Look up the log of the progress report initialization calls.
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

         XPREFX = 'Coordinate pass * of 2'
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
C     Longitude equality searches require 2 passes,
C     so we expect 2 report termination calls.
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
C     Find the sum of the measures of the confinement windows
C     for each pass.
C
      CNFMES = 0.D0

      DO I = 1, NPASS

         DO J = 2, CARDD( CNFLOG(LBCELL,I) ), 2

            CNFMES = CNFMES + CNFLOG(J,I) - CNFLOG(J-1,I)

         END DO

      END DO

C
C     Compare the measure of the reported progress to the sum of 
C     the measures of the confinement windows used for each 
C     search.
C
      CALL CHCKSD (  'MEASUR', MEASUR, '~', CNFMES, TIGHT, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )
 

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
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

