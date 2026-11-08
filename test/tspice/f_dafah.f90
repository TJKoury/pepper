!$Procedure F_DAFAH ( DAFAH Test Family )

      SUBROUTINE F_DAFAH ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the DAFAH
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
!     This routine exercises a subset of DAFAH's logic that concerns
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
!-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
!
!        Updated module as the result of changes to the handle
!        manager interface, ZZDDHCLS.
!
!-    TSPICE Version 1.0.0, 15-NOV-2001 (FST)
!
!
!-&

!
!     Local Parameters
!
      INTEGER               NUMDAF
      PARAMETER           ( NUMDAF = UTSIZE + 5 )

!
!     Local Variables
!
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
      CHARACTER*(FILEN)     OUTNAM
      CHARACTER*(FILEN)     DASNAM

      INTEGER               I
      INTEGER               HANLST ( NUMDAF )
      INTEGER               HANDLE
      INTEGER               OUTHAN
      INTEGER               UNIT


!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DAFAH' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_DAFAH Initialization' )

!
!     Create some test DAFs to use.
!
      FNMTMP = 'test#.daf'

      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL TSTSPK ( FNAME, .FALSE., HANDLE )
      END DO

!
!     Create something that looks like a DAS.
!
      DASNAM = 'test.das'

      CALL KILFIL ( DASNAM )

      CALL T_CPTFIL ( DASNAM, DAS,    2, '        ', 'ABCD', &
                      'EFGH', 'IJKL', .FALSE., .FALSE., ' '  )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFHLU Lock Check' )

!
!     Open UTSIZE - RSVUNT - SCRUNT and lock them to their
!     units.
!
      DO I = 1, UTSIZE - RSVUNT - SCRUNT

         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL DAFOPR ( FNAME, HANLST(I) )
         CALL DAFHLU ( HANLST(I), UNIT )

      END DO

!
!     Now attempt to open and lock one additional file.
!     This should generate an error from the handle manager.
!
      I = UTSIZE - RSVUNT - SCRUNT + 1

      CALL REPMI  ( FNMTMP, '#', I, FNAME )
      CALL DAFOPR ( FNAME, HANDLE )

!
!     Check to make sure we have no exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now call DAFHLU and generate the exception.
!
      CALL DAFHLU ( HANDLE, UNIT )

!
!     Verify the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(HLULOCKFAILED)', OK )

!
!     Check the value of UNIT.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )

!
!     Clean up.
!
      CALL DAFCLS ( HANDLE )

      DO I = 1, UTSIZE - RSVUNT - SCRUNT
         CALL DAFCLS ( HANLST(I) )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFLUH Exceptions' )

!
!     First, load up a DAS file and see what DAFLUH does
!     with it.
!
      CALL ZZDDHOPN ( DASNAM, 'READ', 'DAS', HANDLE )

!
!     Fetch the logical unit assigned to DASNAM.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

!
!     Toss UNIT into DAFLUH and see what happens.
!
      CALL DAFLUH ( UNIT, OUTHAN )

!
!     We expect an error, check for it.
!
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHUNIT)', OK )

!
!     Check the value of OUTHAN, it should be 0.
!
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )

!
!     Clean up from this exception.
!
      CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )

!
!     Now we know HANDLE is no longer known to the handle
!     manager.  See what DAFLUH does with UNIT again.
!
      CALL DAFLUH ( UNIT, OUTHAN )

!
!     This is a little redundant, check for the same error
!     again.
!
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHUNIT)', OK )

!
!     Check the value of OUTHAN, it should be 0.
!
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFHFN Nominal Execution' )

!
!     Setup inputs and outputs.
!
      OUTNAM = ' '

!
!     Open a DAF with DAFOPR.
!
      CALL REPMI  ( FNMTMP, '#', 1, FNAME )

      CALL DAFOPR ( FNAME, HANLST(1) )

!
!     See what file name is associated with HANDLE.
!
      CALL DAFHFN ( HANLST(1), OUTNAM )

!
!     Check for the exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the output argument.
!
      CALL CHCKSC ( 'FNAME', OUTNAM, '=', FNAME, OK )

!
!     Clean up from this test case.
!
      CALL DAFCLS ( HANLST(1) )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFHFN Exceptions' )

!
!     First, load up a DAS file and see what DAFHFN does
!     with it.
!
      CALL ZZDDHOPN ( DASNAM, 'READ', 'DAS', HANDLE )

!
!     Set OUTNAM.
!
      OUTNAM = 'NOT A FILENAME'

!
!     Invoke the module.
!
      CALL DAFHFN ( HANDLE, OUTNAM )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHHANDLE)', OK )

!
!     Check the value of OUTNAM.
!
      CALL CHCKSC ( 'FNAME', OUTNAM, '=', 'NOT A FILENAME', OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )

!
!     Now try passing HANDLE into DAFHFN again.
!
      CALL DAFHFN ( HANDLE, OUTNAM )

!
!     Check for the exception again.
!
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHHANDLE)', OK )

!
!     Check the value of OUTNAM.
!
      CALL CHCKSC ( 'FNAME', OUTNAM, '=', 'NOT A FILENAME', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFFNH Exceptions' )

!
!     Load the test DAS, as we are going to use it to exercise
!     exceptions in DAFFNH.
!
      CALL ZZDDHOPN ( DASNAM, 'READ', 'DAS', HANDLE )

!
!     Set OUTHAN to something ridiculous.
!
      OUTHAN = -1

!
!     Now pass DASNAM into DAFFNH and see what happens.
!
      CALL DAFFNH ( DASNAM, OUTHAN )

!
!     We expect an exception, check for it.
!
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHFILE)', OK )

!
!     Check the value of OUTHAN.
!
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )

!
!     Now as with half the other test cases, unload the DAS and
!     try again.
!
      CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )

      OUTHAN = -1

!
!     Invoke the module with again.
!
      CALL DAFFNH ( DASNAM, OUTHAN )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(DAFNOSUCHFILE)', OK )

!
!     Check the value of OUTHAN.
!
      CALL CHCKSI ( 'HANDLE', OUTHAN, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DAFSIH SPICE(DAFINVALIDACCESS) Exception' )

!
!     Open a DAF with DAFOPR.
!
      CALL REPMI  ( FNMTMP, '#', 1, FNAME )

      CALL DAFOPR ( FNAME, HANLST(1) )

!
!     See what file name is associated with HANDLE.
!
      CALL DAFSIH ( HANLST(1), 'WRITE' )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(DAFINVALIDACCESS)', OK )

!
!     Clean up from this test case.
!
      CALL DAFCLS ( HANLST(1) )


!
!     Clean up the files we used for this test family.
!
      DO I = 1, NUMDAF
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO

      CALL KILFIL ( DASNAM )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
