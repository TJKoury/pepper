!$Procedure F_ZZDGFR ( ZZDAFGFR Test Family )

      SUBROUTINE F_ZZDGFR ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDAFGFR
!     routine.
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
!     This routine exercises ZZDAFGFR's logic.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 11-OCT-2021 (NJB)
!
!        Bug fix: in test case for invalid handle, changed index
!        of HANLST from I to 1.
!
!        Reordered header sections.
!
!-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
!
!        Updated module as the result of changes to the handle
!        manager interface, ZZDDHCLS.
!
!-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
!
!
!-&

!
!     Local Variables
!
      CHARACTER*(80)        CASENM
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
      CHARACTER*(8)         IDWORD
      CHARACTER*(60)        IFNAME
      CHARACTER*(60)        RDIFN
      CHARACTER*(8)         RDIDW
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )

      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANLST ( NUMBFF )
      INTEGER               I
      INTEGER               NATBFF
      INTEGER               ND
      INTEGER               NI
      INTEGER               NUMSUP
      INTEGER               RDBWD
      INTEGER               RDFRE
      INTEGER               RDFWD
      INTEGER               RDND
      INTEGER               RDNI
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               UNIT

      LOGICAL               ADDFTP
      LOGICAL               FOUND

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_ZZDGFR' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_ZZDGFR Initialization' )

!
!     Retrieve the native format and other related information.
!
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )

!
!     Setup the filename template.
!
      FNMTMP = 'daf#.daf'

!
!     Construct the contents of the DAF to create.
!
      IDWORD = 'DAF/TEST'
      ND     = 12
      NI     = 16
      IFNAME = 'TSPICE Test DAF Striving for 60 Characters Going Once' &
      //       ' Twice.'
      FWARD  = -4
      BWARD  = 12
      FREE   = 4*128+1
      ADDFTP = .TRUE.

!
!     Construct the DAFs for each environment and load them
!     into the handle manager.
!
      DO I = 1, NUMSUP

         CALL REPMI    ( FNMTMP, '#', I, FNAME )

!
!        Start a new DAF.
!
         CALL T_DAFOPN ( FNAME, SUPBFF(I), UNIT )

!
!        Dump the file record to it.
!
         CALL T_DAFWFR ( UNIT,  SUPBFF(I), IDWORD, ND,    NI, IFNAME, &
                         FWARD, BWARD,     FREE,   ADDFTP             )

!
!        Close the file.  Since we added the FTP string, ZZDDHOPN
!        only requires a file record to open the file.
!
         CLOSE ( UNIT = UNIT )

!
!        Open the DAF in the handle manager.
!
         CALL ZZDDHOPN ( FNAME, 'READ', 'DAF', HANLST(I) )

!
!        Check for the absence of an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Dynamically construct the test cases based on the contents
!     of SUPBFF.
!
      DO I = 1, NUMSUP

         CASENM = '# reading # file record data.'
         CALL REPMC ( CASENM, '#', STRBFF(NATBFF),    CASENM )
         CALL REPMC ( CASENM, '#', STRBFF(SUPBFF(I)), CASENM )

         CALL TCASE ( CASENM )

!
!        Setup the inputs and outputs.
!
         RDND  = 0
         RDNI  = 0
         RDIDW = ' '
         RDIFN = ' '
         RDFWD = 0
         RDBWD = 0
         RDFRE = 0

         FOUND = .FALSE.

!
!        We have all the pieces, invoke the module.
!
         CALL ZZDAFGFR ( HANLST(I), &
                         RDIDW, &
                         RDND, &
                         RDNI, &
                         RDIFN, &
                         RDFWD, &
                         RDBWD, &
                         RDFRE, &
                         FOUND      )

!
!        Check for the absence of an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check outputs.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL CHCKSC ( 'IDWORD', RDIDW, '=', IDWORD, OK )

         CALL CHCKSI ( 'ND', RDND, '=', ND, 0, OK )
         CALL CHCKSI ( 'NI', RDNI, '=', NI, 0, OK )

         CALL CHCKSC ( 'IFNAME', RDIFN, '=', IFNAME, OK )

         CALL CHCKSI ( 'FWARD', RDFWD, '=', FWARD, 0, OK )
         CALL CHCKSI ( 'BWARD', RDBWD, '=', BWARD, 0, OK )
         CALL CHCKSI ( 'FREE',  RDFRE, '=', FREE,  0, OK )

      END DO

!
!     Clean up by unloading and removing the test files.
!
      DO I = 1, NUMSUP
         CALL ZZDDHCLS ( HANLST(I), 'DAF', .FALSE. )
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPICE(HANDLENOTFOUND) Exception' )

!
!     Setup outputs.
!
      FOUND = .TRUE.

      RDIDW = ' '
      RDND  = 0
      RDNI  = 0
      RDIFN = ' '
      RDFWD = 0
      RDBWD = 0
      RDFRE = 0

!
!     Since we know we just unloaded HANLST(1), attempt to read
!     from that handle.
!
      CALL ZZDAFGFR ( HANLST(1), &
                      RDIDW, &
                      RDND, &
                      RDNI, &
                      RDIFN, &
                      RDFWD, &
                      RDBWD, &
                      RDFRE, &
                      FOUND      )


!
!     Check for the presence of an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(HANDLENOTFOUND)', OK )

!
!     Check outputs. FOUND should be FALSE, and RDREC should be
!     untouched.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKSC ( 'IDWORD', RDIDW, '=', ' ', OK )

      CALL CHCKSI ( 'ND', RDND, '=', 0, 0, OK )
      CALL CHCKSI ( 'NI', RDNI, '=', 0, 0, OK )

      CALL CHCKSC ( 'IFNAME', RDIFN, '=', ' ', OK )

      CALL CHCKSI ( 'FWARD', RDFWD, '=', 0, 0, OK )
      CALL CHCKSI ( 'BWARD', RDBWD, '=', 0, 0, OK )
      CALL CHCKSI ( 'FREE',  RDFRE, '=', 0,  0, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
