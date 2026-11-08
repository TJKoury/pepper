C$Procedure F_DAFANA ( DAFANA Test Family )
 
      SUBROUTINE F_DAFANA ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the DAFANA
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
C     This routine exercises a subset of DAFANA's logic that concerns
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
C-    TSPICE Version 1.0.0, 15-NOV-2001 (FST)
C
C
C-&
 
C
C     Local Parameters
C
      INTEGER               NUMDAF
      PARAMETER           ( NUMDAF = 21 )
 
      INTEGER               SUMLEN
      PARAMETER           ( SUMLEN = 125 )
 
C
C     Local Variables
C
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
 
      DOUBLE PRECISION      SUM    ( SUMLEN )
      DOUBLE PRECISION      ONES   ( SUMLEN )
      DOUBLE PRECISION      VALS   ( SUMLEN )
 
      INTEGER               I
      INTEGER               HANLST ( NUMDAF )
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DAFANA' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_DAFANA Initialization' )
 
C
C     Set the file name template.
C
      FNMTMP = 'test#.daf'
 
C
C     Initialize the summary record for testing purposes.
C
      DO I = 1, SUMLEN
         SUM(I) = 0.0D0
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFBNA SPICE(STFULL) Exception' )
 
C
C     Create and load some test DAFs to use.
C
      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
         CALL DAFONW ( FNAME, 'SPK ', 2, 6, 'TEST DAF', 0, HANLST(I) )
      END DO
 
C
C     Since NUMDAF is 1 more than the number of DAFs DAFANA's
C     state table can track, attempt to initiate a search on
C     all NUMDAF DAFs and check for the error.
C
      DO I = 1, NUMDAF
         CALL DAFBNA ( HANLST(I), SUM, 'TEST SEGMENT' )
      END DO
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(STFULL)', OK )
 
C
C     Clean up.
C
      DO I = 1, NUMDAF
         CALL DAFCLS ( HANLST(I) )
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAFBNA Remove Inactive Entries in State Table' )
 
C
C     Create and load some test DAFs to use.
C
      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
         CALL DAFONW ( FNAME, 'SPK ', 2, 6, 'TEST DAF', 0, HANLST(I) )
      END DO
 
C
C     Now start appends on the first NUMDAF-1 DAFs.
C
      DO I = 1, NUMDAF-1
         CALL DAFBNA ( HANLST(I), SUM, 'TEST SEGMENT' )
         CALL DAFADA ( SUM, SUMLEN )
      END DO
 
C
C     Conclude writing to the first DAF.
C
      CALL DAFCAD ( HANLST(1) )
      CALL DAFENA (           )
 
C
C     Now attempt to write to the NUMDAF DAF.
C
      CALL DAFBNA ( HANLST(NUMDAF), SUM, 'TEST SEGMENT' )
 
      DO I = 1, SUMLEN
         ONES(I) = 1.0D0
      END DO
 
      CALL DAFADA ( ONES, SUMLEN )
      CALL DAFENA (              )
 
C
C     Check to see that no exception was signaled.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     And that the data was added appropriately.
C
      CALL CLEARD ( SUMLEN, VALS )
      CALL DAFRDA ( HANLST(NUMDAF), 385, 509, VALS )
 
      CALL CHCKAD ( 'VALS', VALS, '=', ONES, SUMLEN, 0.0D0, OK )
 
C
C     Lastly, begin appending a new array to the file we originally
C     bumped.
C
      CALL DAFBNA ( HANLST(1), SUM, 'TEST SEGMENT 2' )
      CALL DAFADA ( ONES, SUMLEN )
      CALL DAFENA (              )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the values of the data we just added.
C
      CALL CLEARD ( SUMLEN, VALS )
      CALL DAFRDA ( HANLST(1), 510, 634, VALS )
 
      CALL CHCKAD ( 'VALS', VALS, '=', ONES, SUMLEN, 0.0D0, OK )
 
C
C     Clean up.
C
      DO I = 1, NUMDAF
         CALL DAFCLS ( HANLST(I) )
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
