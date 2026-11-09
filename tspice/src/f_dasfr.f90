!$Procedure F_DASFR ( DASFR Test Family )

      SUBROUTINE F_DASFR ( OK )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the DASFR
!     umbrella.
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
!     This routine exercises a few simple test cases that verify the
!     FTP validation string and the binary file format strings were
!     added properly to DAS files.
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
!-    TSPICE Version 1.0.0, 11-DEC-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM

!
!     Local Parameters
!
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN = 8 )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

!
!     See DASWFR for a detailed explanation of TAILEN.
!
      INTEGER               TAILEN
      PARAMETER           ( TAILEN = 932 )

!
!     Local Variables
!
      CHARACTER*(FMTLEN)    FORMAT
      CHARACTER*(FMTLEN)    SYSFMT
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(TAILEN)    TAIL

      INTEGER               HANDLE
      INTEGER               IOSTAT
      INTEGER               LUN
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC

      LOGICAL               FTPERR

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DASFR' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDASNFR Test' )

!
!     Setup the file.
!
      FNAME = 'test.das'
      CALL KILFIL ( FNAME )

      CALL GETLUN ( LUN )

      OPEN ( UNIT   = LUN, &
             FILE   = FNAME(1:RTRIM(FNAME)), &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'NEW', &
             IOSTAT = IOSTAT           )

      CALL ZZDASNFR ( LUN, 'DAS/TEST', 'TEST DAS', -1, 1, -2, 2, &
                      'BFFIDWRD'                                  )

!
!     Initialize the values we are about to read.
!
      IDWORD = ' '
      IFNAME = ' '
      NRESVR = 0
      NRESVC = 0
      NCOMR  = 0
      NCOMC  = 0
      FORMAT = ' '
      TAIL   = ' '

      READ ( UNIT   = LUN, &
             REC    = 1, &
             IOSTAT = IOSTAT ) IDWORD, &
                               IFNAME, &
                               NRESVR, &
                               NRESVC, &
                               NCOMR, &
                               NCOMC, &
                               FORMAT, &
                               TAIL

!
!     Check the results.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=',  0, 0,      OK )
      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'DAS/TEST', OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'TEST DAS', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=', -1, 0,      OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  1, 0,      OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=', -2, 0,      OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  2, 0,      OK )
      CALL CHCKSC ( 'FORMAT', FORMAT, '=', 'BFFIDWRD', OK )

      CALL ZZFTPCHK ( TAIL, FTPERR )

      CALL CHCKSL ( 'FTPERR', FTPERR, .FALSE., OK )

      CALL CHCKSC ( 'FTPSTR', TAIL(608:613), '=', 'FTPSTR', OK )

      CLOSE ( UNIT = LUN )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DASOPN Test' )

      FNAME = 'test.das'

      CALL KILFIL ( FNAME )

      CALL DASOPN ( FNAME, 'TEST DASOPN', HANDLE )

      CALL DASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, NRESVC, &
                    NCOMR,  NCOMC                           )

!
!     Check the results.  We don't need to worry about FORMAT or
!     the FTP string since DASOPN invokes ZZDASNFR which we just
!     checked.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'NAIF/DAS',    OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'TEST DASOPN', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',  0, 0,         OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  0, 0,         OK )

      CALL DASCLS ( HANDLE )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DASONW Test' )

      FNAME = 'test.das'

      CALL KILFIL ( FNAME )

      CALL DASONW ( FNAME, 'TEST', 'TEST DASONW', 5, HANDLE )

      CALL DASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, NRESVC, &
                    NCOMR,  NCOMC                           )

!
!     Check the results.  We don't need to worry about FORMAT or
!     the FTP string since DASONW invokes ZZDASNFR which we just
!     checked.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'DAS/TEST',    OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'TEST DASONW', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',  5, 0,         OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  0, 0,         OK )

      CALL DASCLS ( HANDLE )

      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DASWFR Test' )

      FNAME = 'test.das'

      CALL KILFIL ( FNAME )

      CALL DASONW ( FNAME, 'TEST', 'TEST DASWFR', 5, HANDLE )

      CALL DASWFR ( HANDLE, 'DAS/WORD', 'DASWFR UPDATE', 2, 4, &
                    6, 8                                       )

!
!     Check the results.
!
      CALL DASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, NRESVC, &
                    NCOMR,  NCOMC                           )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'DAS/WORD',      OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'DASWFR UPDATE', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=',  2, 0,           OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  4, 0,           OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',  6, 0,           OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  8, 0,           OK )

!
!     Check that DASWFR perserved the binary file format and FTP
!     strings.
!
      CALL DASHLU ( HANDLE, LUN )

      READ ( UNIT   = LUN, &
             REC    = 1, &
             IOSTAT = IOSTAT ) IDWORD, &
                               IFNAME, &
                               NRESVR, &
                               NRESVC, &
                               NCOMR, &
                               NCOMC, &
                               FORMAT, &
                               TAIL

!
!     Check the results.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=',  0, 0,      OK )

      CALL ZZPLATFM ( 'FILE_FORMAT', SYSFMT )
      CALL CHCKSC ( 'FORMAT', FORMAT, '=', SYSFMT, OK )

      CALL ZZFTPCHK ( TAIL, FTPERR )
      CALL CHCKSL ( 'FTPERR', FTPERR, .FALSE., OK )
      CALL CHCKSC ( 'FTPSTR', TAIL(608:613), '=', 'FTPSTR', OK )

      CALL DASCLS ( HANDLE )

      CALL KILFIL ( FNAME )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
