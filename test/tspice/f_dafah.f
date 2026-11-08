C$Procedure F_DAFAH ( DAFAH Test Family )
 
      SUBROUTINE F_DAFAH ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the DAFAH
C     umbrella.
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
 
      INCLUDE              'zzddhman.inc'
 
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
C$ Files
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
C$ Particulars
C
C     This routine exercises a subset of DAFAH's logic that concerns
C     the handle manager implementation.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C-    TSPICE Version 1.0.0, 15-NOV-2001 (FST)
C
C
C-&
 
C
C     Local Parameters
C
      INTEGER               NUMDAF
      PARAMETER           ( NUMDAF = UTSIZE + 5 )
 
C
C     Local Variables
C
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
      CHARACTER*(FILEN)     OUTNAM
      CHARACTER*(FILEN)     DASNAM
 
      INTEGER               I
      INTEGER               HANLST ( NUMDAF )
      INTEGER               HANDLE
      INTEGER               OUTHAN
      INTEGER               UNIT
 
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DAFAH' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_DAFAH Initialization' )
 
C
C     Create some test DAFs to use.
C
      FNMTMP = 'test#.daf'
 
      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL TSTSPK ( FNAME, .FALSE., HANDLE )
      END DO
 
C
C     Create something that looks like a DAS.
C
      DASNAM = 'test.das'
 
      CALL KILFIL ( DASNAM )
 
      CALL T_CPTFIL ( DASNAM, DAS,    2, '        ', 'ABCD',
     .                'EFGH', 'IJKL', .FALSE., .FALSE., ' '  )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFHLU Lock Check' )
 
C
C     Open UTSIZE - RSVUNT - SCRUNT and lock them to their
C     units.
C
      DO I = 1, UTSIZE - RSVUNT - SCRUNT
 
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPR ( FNAME, HANLST(I) )
         CALL DAFHLU ( HANLST(I), UNIT )
 
      END DO
 
C
C     Now attempt to open and lock one additional file.
C     This should generate an error from the handle manager.
C
      I = UTSIZE - RSVUNT - SCRUNT + 1
 
      CALL REPMI  ( FNMTMP, '#', I, FNAME )
      CALL DAFOPR ( FNAME, HANDLE )
 
C
C     Check to make sure we have no exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now call DAFHLU and generate the exception.
C
      CALL DAFHLU ( HANDLE, UNIT )
 
C
C     Verify the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(HLULOCKFAILED)', OK )
 
C
C     Check the value of UNIT.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )
 
C
C     Clean up.
C
      CALL DAFCLS ( HANDLE )
 
      DO I = 1, UTSIZE - RSVUNT - SCRUNT
         CALL DAFCLS ( HANLST(I) )
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFLUH Exceptions' )
 
C
C     First, load up a DAS file and see what DAFLUH does
C     with it.
C
      CALL ZZDDHOPN ( DASNAM, 'READ', 'DAS', HANDLE )
 
C
C     Fetch the logical unit assigned to DASNAM.
C
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )
 
C
C     Toss UNIT into DAFLUH and see what happens.
C
      CALL DAFLUH ( UNIT, OUTHAN )
 
C
C     We expect an error, check for it.
C
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHUNIT)', OK )
 
C
C     Check the value of OUTHAN, it should be 0.
C
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )
 
C
C     Clean up from this exception.
C
      CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )
 
C
C     Now we know HANDLE is no longer known to the handle
C     manager.  See what DAFLUH does with UNIT again.
C
      CALL DAFLUH ( UNIT, OUTHAN )
 
C
C     This is a little redundant, check for the same error
C     again.
C
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHUNIT)', OK )
 
C
C     Check the value of OUTHAN, it should be 0.
C
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFHFN Nominal Execution' )
 
C
C     Setup inputs and outputs.
C
      OUTNAM = ' '
 
C
C     Open a DAF with DAFOPR.
C
      CALL REPMI  ( FNMTMP, '#', 1, FNAME )
 
      CALL DAFOPR ( FNAME, HANLST(1) )
 
C
C     See what file name is associated with HANDLE.
C
      CALL DAFHFN ( HANLST(1), OUTNAM )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the output argument.
C
      CALL CHCKSC ( 'FNAME', OUTNAM, '=', FNAME, OK )
 
C
C     Clean up from this test case.
C
      CALL DAFCLS ( HANLST(1) )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFHFN Exceptions' )
 
C
C     First, load up a DAS file and see what DAFHFN does
C     with it.
C
      CALL ZZDDHOPN ( DASNAM, 'READ', 'DAS', HANDLE )
 
C
C     Set OUTNAM.
C
      OUTNAM = 'NOT A FILENAME'
 
C
C     Invoke the module.
C
      CALL DAFHFN ( HANDLE, OUTNAM )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHHANDLE)', OK )
 
C
C     Check the value of OUTNAM.
C
      CALL CHCKSC ( 'FNAME', OUTNAM, '=', 'NOT A FILENAME', OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )
 
C
C     Now try passing HANDLE into DAFHFN again.
C
      CALL DAFHFN ( HANDLE, OUTNAM )
 
C
C     Check for the exception again.
C
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHHANDLE)', OK )
 
C
C     Check the value of OUTNAM.
C
      CALL CHCKSC ( 'FNAME', OUTNAM, '=', 'NOT A FILENAME', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFFNH Exceptions' )
 
C
C     Load the test DAS, as we are going to use it to exercise
C     exceptions in DAFFNH.
C
      CALL ZZDDHOPN ( DASNAM, 'READ', 'DAS', HANDLE )
 
C
C     Set OUTHAN to something ridiculous.
C
      OUTHAN = -1
 
C
C     Now pass DASNAM into DAFFNH and see what happens.
C
      CALL DAFFNH ( DASNAM, OUTHAN )
 
C
C     We expect an exception, check for it.
C
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHFILE)', OK )
 
C
C     Check the value of OUTHAN.
C
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )
 
C
C     Now as with half the other test cases, unload the DAS and
C     try again.
C
      CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )
 
      OUTHAN = -1
 
C
C     Invoke the module with again.
C
      CALL DAFFNH ( DASNAM, OUTHAN )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHFILE)', OK )
 
C
C     Check the value of OUTHAN.
C
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFSIH SPICE(DAFINVALIDACCESS) Exception' )
 
C
C     Open a DAF with DAFOPR.
C
      CALL REPMI  ( FNMTMP, '#', 1, FNAME )
 
      CALL DAFOPR ( FNAME, HANLST(1) )
 
C
C     See what file name is associated with HANDLE.
C
      CALL DAFSIH ( HANLST(1), 'WRITE' )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )
 
C
C     Clean up from this test case.
C
      CALL DAFCLS ( HANLST(1) )
 
 
C
C     Clean up the files we used for this test family.
C
      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO
 
      CALL KILFIL ( DASNAM )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
