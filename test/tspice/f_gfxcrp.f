C$Procedure F_GFXCRP ( Test GF intercept coord progress reporting )
  
      SUBROUTINE F_GFXCRP ( OK )

C$ Abstract
C
C     Test the GF subsystem's use of passed-in progress reporting and
C     interrupt handling functions for the "surface intercept point
C     coordinate" quantity.
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
C     reporting functions in the context of searches involving surface
C     intercept point coordinate constraints. The point is to test the
C     callers of the functions, not the functions themselves.
C
C     The routines exercised by this test family are
C
C        GFEVNT
C        ZZGFREL
C        ZZGFSOLV
C
C     Note that the sequence of progress reports for this quantity
C     differs from those of other scalar quantities due to the
C     initial existence window search performed for surface intercept
C     searches.
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
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfxcrp.tpc' ) 

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfxcrp.bsp' ) 


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
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      EXPCNF ( LBCELL : MAXWIN )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MEASUR
      DOUBLE PRECISION      QDPARS ( MAXPAR )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      REPLOG ( 3, MXCALL )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
      DOUBLE PRECISION      XMESUR
      DOUBLE PRECISION      XSTMES

      INTEGER               CARD
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
      CALL TOPEN ( 'F_GFXCRP' )



      

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
C     Load a PCK file.
C
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

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
      CALL TCASE ( 'Run an intercept coordinate LOCMIN search using '
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
C     when a local maximum occurs.
C
C     Note: the epoch below was derived using a DE ephemeris and
C     probably doesn't match the value that would be obtained
C     using the test SPK.
C     
      CALL STR2ET ( '2000 JUN 21 01:27:06.528438 TDB', CENTRL )

C
C     We'll use as the ray's direction vector the Sun-Earth
C     direction at the central epoch, expressed in the J2000
C     frame. This vector sweeps across the Earth in a few
C     minutes.
C
      CALL SPKPOS ( 'EARTH', CENTRL, 'J2000', 'NONE', 'SUN', DVEC, LT )



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
      QCPARS(7)  = 'SURFACE INTERCEPT POINT'
      QCPARS(8)  = 'ELLIPSOID'
      QCPARS(9)  = 'J2000'

      CALL VEQU ( DVEC, QDPARS )

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
C     Intercept coordinate local extrema searches (excluding longitude,
C     RA, etc.) require two passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 progress reporting passes: one for
C     the existence window search, and one for the extremum search.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, 2

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'
         ELSE
            XPREFX = 'Coordinate pass 1 of 1'
         END IF

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
C     Surface intercept extrema searches require two passes,
C     so we expect two report termination calls.
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
C     Check the measure of the reported progress. The measure
C     should be the sum of the measures of the original
C     confinement window, which the intercept existence search
C     was done, and the measure of the intercept existence window
C     itself, over which the coordinate constraint search was
C     done. 
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     of the existence window.
C
      XMESUR = CNFMES + XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate ABSMIN search using '
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
C     Intercept coordinate absolute extrema searches (excluding
C     longitude, RA, etc.) require two passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 progress reporting passes: one for
C     the existence window search, and one for the extremum search.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, 2

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'
         ELSE
            XPREFX = 'Coordinate pass 1 of 1'
         END IF

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
C     Surface intercept extrema searches require two passes,
C     so we expect two report termination calls.
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
C     Check the measure of the reported progress. The measure
C     should be the sum of the measures of the original
C     confinement window, which the intercept existence search
C     was done, and the measure of the intercept existence window
C     itself, over which the coordinate constraint search was
C     done. 
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     of the existence window.
C
      XMESUR = CNFMES + XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK ) 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate LOCMAX search using '
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
C     Intercept coordinate local extrema searches (excluding longitude,
C     RA, etc.) require two passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 progress reporting passes: one for
C     the existence window search, and one for the extremum search.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, 2

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'
         ELSE
            XPREFX = 'Coordinate pass 1 of 1'
         END IF

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
C     Surface intercept extrema searches require two passes,
C     so we expect two report termination calls.
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
C     Check the measure of the reported progress. The measure
C     should be the sum of the measures of the original
C     confinement window, which the intercept existence search
C     was done, and the measure of the intercept existence window
C     itself, over which the coordinate constraint search was
C     done. 
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     of the existence window.
C
      XMESUR = CNFMES + XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate ABSMAX search using '
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
C     Intercept coordinate absolute extrema searches (excluding
C     longitude, RA, etc.) require two passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 progress reporting passes: one for
C     the existence window search, and one for the extremum search.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 2, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, 2

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'
         ELSE
            XPREFX = 'Coordinate pass 1 of 1'
         END IF

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
C     Surface intercept extrema searches require two passes,
C     so we expect two report termination calls.
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
C     Check the measure of the reported progress. The measure
C     should be the sum of the measures of the original
C     confinement window, which the intercept existence search
C     was done, and the measure of the intercept existence window
C     itself, over which the coordinate constraint search was
C     done. 
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     of the existence window.
C
      XMESUR = CNFMES + XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK ) 


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate > inequality search '
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
C     Surface intercept inequality searches (excluding longitude, RA,
C     etc.) require 3 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 3 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 3, 0, OK )

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

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'

         ELSE IF ( I .EQ. 2 ) THEN

            XPREFX = 'Coordinate pass 1 of 2'

         ELSE

            XPREFX = 'Coordinate pass 2 of 2'

         END IF

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
C     Surface intercept coordinate inequality searches require 3
C     passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 3, 0, OK )

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
C     Check the measure of the reported progress. The measure should be
C     the sum of the measures of the original confinement window, which
C     the intercept existence search was done, and twice the measure of
C     the intercept existence window itself, over which the coordinate
C     constraint search was done.
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     twice that of the existence window.
C
      XMESUR = CNFMES + 2*XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK ) 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate < inequality search '
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
C     Surface intercept inequality searches (excluding longitude, RA,
C     etc.) require 3 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 3 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 3, 0, OK )

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

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'

         ELSE IF ( I .EQ. 2 ) THEN

            XPREFX = 'Coordinate pass 1 of 2'

         ELSE

            XPREFX = 'Coordinate pass 2 of 2'

         END IF

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
C     Surface intercept coordinate inequality searches require 3
C     passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 3, 0, OK )

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
C     Check the measure of the reported progress. The measure should be
C     the sum of the measures of the original confinement window, which
C     the intercept existence search was done, and twice the measure of
C     the intercept existence window itself, over which the coordinate
C     constraint search was done.
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     twice that of the existence window.
C
      XMESUR = CNFMES + 2*XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK ) 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate equality search using '
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

      CALL WNEXPD ( 1.D0, 1.D0, EXPCNF )
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
C     Surface intercept equality searches (excluding longitude, RA,
C     etc.) require 3 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 3 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 3, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', ISQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefixes and suffixes.
C
C
C     Check the progress report message prefixes and suffixes.
C
      DO I = 1, 3

         QNAME = 'Prefix *'
         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'

         ELSE IF ( I .EQ. 2 ) THEN

            XPREFX = 'Coordinate pass 1 of 2'
         ELSE
            XPREFX = 'Coordinate pass 2 of 2'
         END IF

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
C     Intercept coordinate equality searches require 3 passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 3, 0, OK )

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
C     Check the measure of the reported progress. The measure should be
C     the sum of the measures of the original confinement window, which
C     the intercept existence search was done, and twice the measure of
C     the intercept existence window itself, over which the coordinate
C     constraint search was done.
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     twice that of the existence window.
C
      XMESUR = CNFMES + 2*XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK ) 

 




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate adjusted ABSMIN search '
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
C     Surface intercept adjusted absolute extrema searches (excluding
C     longitude, RA, etc.) require 3 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 3, 0, OK )

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

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'

         ELSE IF ( I .EQ. 2 ) THEN

            XPREFX = 'Coordinate pass 1 of 2'

         ELSE

            XPREFX = 'Coordinate pass 2 of 2'

         END IF

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
C     Intercept coordinate adjusted absolute extrema searches require 3
C     passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 3, 0, OK )

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
C     Check the measure of the reported progress. The measure should be
C     the sum of the measures of the original confinement window, which
C     the intercept existence search was done, and twice the measure of
C     the intercept existence window itself, over which the coordinate
C     constraint search was done.
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     twice that of the existence window.
C
      XMESUR = CNFMES + 2*XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK ) 

 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run an intercept coordinate adjusted ABSMAX search '
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
C     Surface intercept adjusted absolute extrema searches (excluding
C     longitude, RA, etc.) require 3 passes.
C
C     Look up the log of the progress report initialization calls. We
C     expect that there were 2 initialization calls.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NPASS, '=', 3, 0, OK )

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

         IF ( I .EQ. 1 ) THEN

            XPREFX = 'Intercept existence pass 1 of 1'

         ELSE IF ( I .EQ. 2 ) THEN

            XPREFX = 'Coordinate pass 1 of 2'

         ELSE

            XPREFX = 'Coordinate pass 2 of 2'

         END IF

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
C     Intercept coordinate adjusted absolute extrema searches require 3
C     passes.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 3, 0, OK )

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
C     Check the measure of the reported progress. The measure should be
C     the sum of the measures of the original confinement window, which
C     the intercept existence search was done, and twice the measure of
C     the intercept existence window itself, over which the coordinate
C     constraint search was done.
C
C     We can extract the existence window from the initialization
C     confinement window log. The existence window is stored in
C     
C        CNFLOG(LBCELL:*,2)
C
C     Let EXTMES be the measure of this window.
C 
      CARD = CARDD ( CNFLOG(LBCELL,2) )

      XSTMES = 0.D0

      DO I = 2, CARD, 2
         XSTMES = XSTMES + CNFLOG(I,2) - CNFLOG(I-1,2)
      END DO
C
C     Compare the measure of the reported progress to that of the 
C     sum of the measures of the original confinement window and
C     twice that of the existence window.
C
      XMESUR = CNFMES + 2*XSTMES

      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK ) 
 


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

