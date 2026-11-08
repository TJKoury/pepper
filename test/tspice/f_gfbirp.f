C$Procedure      F_GFBIRP ( Test GF binary quantity progress reporting )
  
      SUBROUTINE F_GFBIRP ( OK )

C$ Abstract
C
C     Test the GF binary state quantity progress reporting and 
C     interrupt handling functionality.
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
C     This family tests the GF subsystem's use of the default
C     progress reporting and interrupt handling functions. 
C     The point is to test the callers of the functions, not
C     the functions themselves.
C
C     The routines exercised by this test family are
C
C        GFFOVE
C        GFOCCE
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

 
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )

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


C
C     Local Variables
C     
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(MAXMSG)    MSGLOG ( 2, MXPASS )
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TFRAME
      CHARACTER*(SHPLEN)    TSHAPE

      DOUBLE PRECISION      CNFINE ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      CNFLOG ( LBCELL : 2*MAXWIN, MXPASS )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      MEASUR
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      REPLOG ( 3, MXCALL )
      DOUBLE PRECISION      RESULT ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XMESUR

      INTEGER               HAN2
      INTEGER               I
      INTEGER               MW
      INTEGER               NCALLS
      INTEGER               NIVL
      INTEGER               NUPDAT
      INTEGER               SEQLOG ( MXCALL )
      INTEGER               TRMLOG ( MXCALL )
      INTEGER               XSQLOG ( MXCALL )

      LOGICAL               BAIL
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
      CALL TOPEN ( 'F_GFBIRP' )



      

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

      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATIK  ( NIK,  NSPK,   NPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************




C*********************************************************************
C*
C*    Binary state search tests
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run simple occultation search using GFOCCE.' )

C
C     Search for any occultation of ALPHA by BETA, abcorr = NONE.
C
      

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( 2*MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


C
C     Note: the stellar aberration correction spec should be ignored.
C
      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

C
C     Create a confinement window with 5 intervals.
C
      NIVL   = 5
      XMESUR = 0.D0

      DO I = 1, NIVL

         ET0 = -900.D0 + (I-3)*3600.D0
         ET1 =  ET0    + 1800.D0

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         XMESUR = XMESUR + ET1 - ET0

      END DO

      BAIL = .FALSE.
      RPT  = .TRUE.


      STEP = 300.D0
      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL  = CNVTOL

      CALL GFOCCE ( 'ANY',
     .              'beta',    'ellipsoid',  'betafixed',
     .              'alpha',   'ellipsoid',  'alphafixed',
     .              'NONE',    'sun',        TOL,
     .              GFSTEP,    GFREFN,       RPT,
     .              T_GFREPI,  T_GFREPU,     T_GFREPF,
     .              BAIL,      GFBAIL,       CNFINE,  RESULT )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now that the search is done, the interesting work starts. We're
C     going to do a post-mortem on the progress reporting calls that
C     were made during the search. 
C
C     Start out by fetching the information that was passed to T_GFREPI
C     during the search.
C
      MW = 2*MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NCALLS, SEQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Occultation searches require one pass, so we expect that
C     just one call to T_GFREPI was made.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NCALLS, '=', 1, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', SEQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefix and suffix.
C
      CALL CHCKSC ( 'Prefix', MSGLOG(1,1), '=', 
     .              'Occultation/transit search', OK )

      CALL CHCKSC ( 'Suffix', MSGLOG(2,1), '=', 
     .              'done.', OK )



C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the sequence numbers of the calls. They should
C     range from 2 to NUPDAT+1 and should be in increasing
C     order.
C
      DO I = 1, NUPDAT
         XSQLOG(I) = I+1
      END DO

      CALL CHCKAI ( 'Update SEQLOG', SEQLOG, '=', 
     .                               XSQLOG, NUPDAT, OK )
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
      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Occultation searches require one pass, so we expect that
C     just one call to T_GFRTRM was made.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

C
C     Check the sequence number of the first termination call. Take
C     into account that the first call was to the init routine.
C
      CALL CHCKSI ( 'T_GFREPF seq', TRMLOG(1), '=', NUPDAT+2, 0, OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Run simple FOV search using GFFOVE.' )


C
C     We'll use the instrument defined in nat.ti:
C
C        ALPHA_ELLIPSE_NONE
C
C     This instrument tracks body Alpha so body Beta's FOV entry and
C     exit times match the start and stop times of Beta's transit
C     across Alpha.
C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

C
C     Initialize the progress reporting test utility package.
C
      CALL T_GFUINI
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize the confinement window log.
C
      DO I = 1, MXPASS

         CALL SSIZED ( 2*MAXWIN, CNFLOG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO
 
      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

C
C     Create a confinement window with 3 intervals.
C
      NIVL   = 3
      XMESUR = 0.D0

      DO I = 1, NIVL

         ET0 = -800.D0 + (I-3)*3600.D0
         ET1 =  ET0    + 1600.D0

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         XMESUR = XMESUR + ET1 - ET0

      END DO



      INST   = 'ALPHA_ELLIPSE_NONE'
      TARGET = 'BETA'
      TSHAPE = 'ELLIPSOID'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0
      TOL    = CNVTOL
      ABCORR = 'NONE'

      CALL CLEARD ( 3, RAYDIR )

      RPT    = .TRUE.
      BAIL   = .FALSE.


      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL GFFOVE ( INST,      TSHAPE,  RAYDIR,     
     .              TARGET,    TFRAME,  ABCORR,  OBSRVR,    TOL,    
     .              GFSTEP,    GFREFN,  RPT,     T_GFREPI,  T_GFREPU,
     .              T_GFREPF,  BAIL,    GFBAIL,  CNFINE,    RESULT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now that the search is done, the interesting work starts. We're
C     going to do a post-mortem on the progress reporting calls that
C     were made during the search. 
C
C     Start out by fetching the information that was passed to T_GFREPI
C     during the search.
C     
      MW = 2*MAXWIN
      CALL T_GFRINI ( MXPASS, MW, NCALLS, SEQLOG, CNFLOG, MSGLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     FOV searches require one pass.
C
      CALL CHCKSI ( 'No. of T_GFREPI calls', NCALLS, '=', 1, 0, OK )

C
C     Check the sequence number of the first call.
C
      CALL CHCKSI ( 'SEQLOG(1)', SEQLOG(1), '=', 1, 0, OK )

C
C     Check the progress report message prefix and suffix.
C
      CALL CHCKSC ( 'Prefix', MSGLOG(1,1), '=', 
     .              'Target visibility search', OK )

      CALL CHCKSC ( 'Suffix', MSGLOG(2,1), '=', 
     .              'done.', OK )


C
C     Retrieve the log of calls made to the update routine.
C
      CALL T_GFRPLO ( MXCALL, NUPDAT, SEQLOG, REPLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the sequence numbers of the calls. They should
C     range from 2 to NUPDAT+1 and should be in increasing
C     order.
C
      DO I = 1, NUPDAT
         XSQLOG(I) = I+1
      END DO

      CALL CHCKAI ( 'Update SEQLOG', SEQLOG, '=', 
     .                               XSQLOG, NUPDAT, OK )
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
      CALL CHCKSD (  'MEASUR', MEASUR, '~', XMESUR, TIGHT, OK )

C
C     Get the log of the report termination calls.
C
      CALL T_GFRTRM ( MXPASS, NCALLS, TRMLOG )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     FOV searches require one pass, so we expect that
C     just one call to T_GFRTRM was made.
C
      CALL CHCKSI ( 'No. of T_GFRTRM calls', NCALLS, '=', 1, 0, OK )

C
C     Check the sequence number of the first termination call. Take
C     into account that the first call was to the init routine.
C
      CALL CHCKSI ( 'T_GFREPF seq', TRMLOG(1), '=', NUPDAT+2, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )
 
 
      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

 


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
 
      CALL T_SUCCESS ( OK )
      RETURN
      END

