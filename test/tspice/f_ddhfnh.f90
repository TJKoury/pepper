!$Procedure F_DDHFNH ( ZZDDHFNH Test Family )

      SUBROUTINE F_DDHFNH ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHFNH
!     routine.
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
!     This routine exercises ZZDDHFNH's logic.
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
!-    TSPICE Version 1.0.0, 06-NOV-2001 (FST)
!
!-&

!
!     Local Variables
!
      CHARACTER*(FILEN)     ALTFNM
      CHARACTER*(20)        BINFMT
      CHARACTER*(20)        ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(20)        METHOD

      INTEGER               ALTHAN
      INTEGER               HANDLE

      LOGICAL               FOUND

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHFNH' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_DDHFNH Initialization' )

!
!     Setup the test cases.
!
      FNAME  = 'test.fil'
      ARCH   = 'DAF'
      METHOD = 'READ'
      HANDLE = 0
      FOUND  = .TRUE.

!
!     Get the native binary file format string to utilize in
!     creating the test file.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', BINFMT )

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/CK  '  )

!
!     Open the file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Nominal ZZDDHFNH Logic' )

!
!     Now exercise ZZDDHFNH.
!
      CALL ZZDDHFNH ( FNAME, ALTHAN, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,  OK            )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    HANDLE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise LJUST of FNAME Logic' )

!
!     Now exercise the insignificance of leading white space.
!
      ALTFNM = '       ' // FNAME

      CALL ZZDDHFNH ( ALTFNM, ALTHAN, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,  OK            )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    HANDLE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Blank Filename Exception' )

!
!     Lastly exercise 2 "NOT FOUND" cases.
!
      CALL ZZDDHFNH ( '     ', ALTHAN, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK       )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Unknown Filename Exception' )

      CALL ZZDDHFNH ( 'BOGUS_FILENAME_GOES_HERE', ALTHAN, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK       )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    0, 0, OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME        )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN


      END
