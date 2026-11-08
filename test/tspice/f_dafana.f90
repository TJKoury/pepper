!$Procedure F_DAFANA ( DAFANA Test Family )

      SUBROUTINE F_DAFANA ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the DAFANA
!     umbrella.
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
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

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
! $ Files
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
! $ Particulars
!
!     This routine exercises a subset of DAFANA's logic that concerns
!     the handle manager implementation.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 15-NOV-2001 (FST)
!
!
!-&

!
!     Local Parameters
!
      INTEGER               NUMDAF
      PARAMETER           ( NUMDAF = 21 )

      INTEGER               SUMLEN
      PARAMETER           ( SUMLEN = 125 )

!
!     Local Variables
!
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP

      DOUBLE PRECISION      SUM    ( SUMLEN )
      DOUBLE PRECISION      ONES   ( SUMLEN )
      DOUBLE PRECISION      VALS   ( SUMLEN )

      INTEGER               I
      INTEGER               HANLST ( NUMDAF )

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DAFANA' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_DAFANA Initialization' )

!
!     Set the file name template.
!
      FNMTMP = 'test#.daf'

!
!     Initialize the summary record for testing purposes.
!
      DO I = 1, SUMLEN
         SUM(I) = 0.0D0
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFBNA SPICE(STFULL) Exception' )

!
!     Create and load some test DAFs to use.
!
      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
         CALL DAFONW ( FNAME, 'SPK ', 2, 6, 'TEST DAF', 0, HANLST(I) )
      END DO

!
!     Since NUMDAF is 1 more than the number of DAFs DAFANA's
!     state table can track, attempt to initiate a search on
!     all NUMDAF DAFs and check for the error.
!
      DO I = 1, NUMDAF
         CALL DAFBNA ( HANLST(I), SUM, 'TEST SEGMENT' )
      END DO

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(STFULL)', OK )

!
!     Clean up.
!
      DO I = 1, NUMDAF
         CALL DAFCLS ( HANLST(I) )
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFBNA Remove Inactive Entries in State Table' )

!
!     Create and load some test DAFs to use.
!
      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
         CALL DAFONW ( FNAME, 'SPK ', 2, 6, 'TEST DAF', 0, HANLST(I) )
      END DO

!
!     Now start appends on the first NUMDAF-1 DAFs.
!
      DO I = 1, NUMDAF-1
         CALL DAFBNA ( HANLST(I), SUM, 'TEST SEGMENT' )
         CALL DAFADA ( SUM, SUMLEN )
      END DO

!
!     Conclude writing to the first DAF.
!
      CALL DAFCAD ( HANLST(1) )
      CALL DAFENA (           )

!
!     Now attempt to write to the NUMDAF DAF.
!
      CALL DAFBNA ( HANLST(NUMDAF), SUM, 'TEST SEGMENT' )

      DO I = 1, SUMLEN
         ONES(I) = 1.0D0
      END DO

      CALL DAFADA ( ONES, SUMLEN )
      CALL DAFENA (              )

!
!     Check to see that no exception was signaled.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     And that the data was added appropriately.
!
      CALL CLEARD ( SUMLEN, VALS )
      CALL DAFRDA ( HANLST(NUMDAF), 385, 509, VALS )

      CALL CHCKAD ( 'VALS', VALS, '=', ONES, SUMLEN, 0.0D0, OK )

!
!     Lastly, begin appending a new array to the file we originally
!     bumped.
!
      CALL DAFBNA ( HANLST(1), SUM, 'TEST SEGMENT 2' )
      CALL DAFADA ( ONES, SUMLEN )
      CALL DAFENA (              )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the values of the data we just added.
!
      CALL CLEARD ( SUMLEN, VALS )
      CALL DAFRDA ( HANLST(1), 510, 634, VALS )

      CALL CHCKAD ( 'VALS', VALS, '=', ONES, SUMLEN, 0.0D0, OK )

!
!     Clean up.
!
      DO I = 1, NUMDAF
         CALL DAFCLS ( HANLST(I) )
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
