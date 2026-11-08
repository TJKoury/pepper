!$Procedure F_DDHF2H ( ZZDDHF2H Test Family )

      SUBROUTINE F_DDHF2H ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHF2H
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
!     This routine exercises ZZDDHF2H's logic.
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
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 12-APR-2012 (BVS)
!
!        Updated for changed ZZDDHF2H calling sequence and use
!        of magic numbers.
!
!-    TSPICE Version 2.0.0, 05-AUG-2002 (FST)
!
!        Promoted module to master file status.  Check that
!        read-only files on VAX systems can be opened by
!        zzddhf2h if necessary.
!
!-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM
      INTEGER               ISRCHI
      DOUBLE PRECISION      ZZDDHMNM

!
!     Local Parameters
!
!
!     This parameter defines the number of test files we are going
!     to create.  It should be something more than UTSIZE, to
!     properly execute logic in ZZDDHF2H.
!
      INTEGER               NUMFIL
      PARAMETER           ( NUMFIL = UTSIZE+5 )

!
!     Local Variables
!
!     File Table
!
      INTEGER               NFT

      INTEGER               FTABS  ( FTSIZE )
      INTEGER               FTAMH  ( FTSIZE )
      INTEGER               FTARC  ( FTSIZE )
      INTEGER               FTBFF  ( FTSIZE )
      INTEGER               FTHAN  ( FTSIZE )
      CHARACTER*(FILEN)     FTNAM  ( FTSIZE )
      INTEGER               FTRTM  ( FTSIZE )
      DOUBLE PRECISION      FTMNM  ( FTSIZE )

!
!     Unit Table
!
      INTEGER               NUT

      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )

!
!     Other Variables
!
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INDEX
      INTEGER               IOSTAT
      INTEGER               LUN
      DOUBLE PRECISION      MNM

      DOUBLE PRECISION      FILMNM ( NUMFIL )

      LOGICAL               EXISTS
      LOGICAL               FOUND
      LOGICAL               OPENED

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHF2H' )

      FNMTMP = 'spk#.bsp'

!
!     Start by creating NUMFIL files for testing purposes.
!
      DO I = 1, NUMFIL

         CALL REPMI  ( FNMTMP, '#', I, FNAME  )
         CALL TSTSPK ( FNAME, .FALSE., HANDLE )

!
!        Save the magic number for each test file. Open file first.
!
         CALL GETLUN ( LUN )

         OPEN ( UNIT   = LUN, &
                FILE   = FNAME, &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'OLD', &
                IOSTAT = IOSTAT      )

!
!        Check the IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF

!
!        Compute the magic number.
!
         FILMNM( I ) = ZZDDHMNM( LUN )

!
!        Close the file.
!
         CLOSE ( UNIT = LUN )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'File does not exist exceptional case.' )

!
!     First, kill the file name we are about to check against, since
!     it should not exist.
!
      CALL KILFIL ( 'spk0.bsp' )

!
!     Prepare the inputs and output default values.
!
      NUT = 0
      NFT = 0

      EXISTS = .TRUE.
      OPENED = .TRUE.
      HANDLE = -1
      FOUND  = .TRUE.

!
!     Invoke the module.
!
      CALL ZZDDHF2H ( 'spk0.bsp', FTABS,  FTAMH, FTARC, FTBFF, &
                      FTHAN,      FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                      UTHAN,      UTLCK,  UTLUN, NUT,   EXISTS, &
                      OPENED,     HANDLE, FOUND, MNM                 )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE.,   OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,   OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .FALSE.,   OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'File exists and is opened externally exceptional ' &
      //           'case.'                                             )

!
!     Open the first test file we created.
!
      CALL GETLUN ( LUN )

      OPEN ( UNIT   = LUN, &
             FILE   = 'spk1.bsp', &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'OLD', &
             IOSTAT = IOSTAT      )

!
!     Check the IOSTAT.
!
      IF ( IOSTAT .NE. 0 ) THEN
         CLOSE ( UNIT = LUN )
         CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
         CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
      END IF

!
!     Setup the inputs and output default values.
!
      NUT = 0
      NFT = 0

      EXISTS = .FALSE.
      OPENED = .FALSE.
      HANDLE = -1
      FOUND  = .TRUE.

!
!     Invoke the module.
!
      CALL ZZDDHF2H ( 'spk1.bsp', FTABS,  FTAMH, FTARC, FTBFF, &
                      FTHAN,      FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                      UTHAN,      UTLCK,  UTLUN, NUT,   EXISTS, &
                      OPENED,     HANDLE, FOUND, MNM                 )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE.,   OK )
      CALL CHCKSL ( 'OPENED', OPENED, .TRUE.,    OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,    OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close the test file.
!
      CLOSE ( UNIT = LUN )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'File exists and is opened internally and is in ' &
      //           'the unit table case.'                            )

!
!     Open the first test file we created.
!
      CALL GETLUN ( LUN )

      OPEN ( UNIT   = LUN, &
             FILE   = 'spk1.bsp', &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'OLD', &
             IOSTAT = IOSTAT      )

!
!     Check the IOSTAT.
!
      IF ( IOSTAT .NE. 0 ) THEN
         CLOSE ( UNIT = LUN )
         CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
         CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
      END IF

!
!     Setup the inputs and output default values. Create a row for
!     spk1.bsp in the file table and unit tables.
!
      NFT        = 1
      FTABS(NFT) = ABS(NFT)
      FTAMH(NFT) = READ
      FTARC(NFT) = DAF
      FTBFF(NFT) = BIGI3E
      FTHAN(NFT) = NFT
      FTNAM(NFT) = 'spk1.bsp'
      FTRTM(NFT) = RTRIM( FTNAM(NFT) )
      FTMNM(NFT) = FILMNM(1)

      NUT        = 1
      UTCST(NUT) = 2
      UTHAN(NUT) = FTHAN(NFT)
      UTLCK(NUT) = .FALSE.
      UTLUN(NUT) = LUN

      EXISTS = .FALSE.
      OPENED = .FALSE.
      HANDLE = -1
      FOUND  = .FALSE.

!
!     Invoke the module.
!
      CALL ZZDDHF2H ( 'spk1.bsp', FTABS,  FTAMH, FTARC, FTBFF, &
                      FTHAN,      FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                      UTHAN,      UTLCK,  UTLUN, NUT,   EXISTS, &
                      OPENED,     HANDLE, FOUND, MNM                 )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,    OK )
      CALL CHCKSL ( 'OPENED', OPENED, .TRUE.,    OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,    OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 1, 0, OK )

!
!     Close the test file.
!
      CLOSE ( UNIT = LUN )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'File exists and is opened internally and is not ' &
      //           'in the unit table case.'                          )

!
!     Open the files we want to put in the unit table.
!
      NUT = 0
      NFT = 0

      DO I = 1, 10

!
!        Get a unit.
!
         CALL GETLUN ( LUN )

!
!        Compute the filename.
!
         CALL REPMI  ( FNMTMP, '#', I, FNAME  )

         OPEN ( UNIT   = LUN, &
                FILE   = FNAME, &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'OLD', &
                IOSTAT = IOSTAT      )

!
!        Check the IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF

!
!        Setup the inputs and output default values. Create a row for
!        FNAME in the file table and unit tables.
!
         NFT        = NFT+1
         FTABS(NFT) = ABS(NFT)
         FTAMH(NFT) = READ
         FTARC(NFT) = DAF
         FTBFF(NFT) = BIGI3E
         FTHAN(NFT) = NFT
         FTNAM(NFT) = FNAME
         FTRTM(NFT) = RTRIM( FTNAM(NFT) )
         FTMNM(NFT) = FILMNM(I)

         NUT        = NUT+1
         UTCST(NUT) = I*2
         UTHAN(NUT) = FTHAN(NFT)
         UTLCK(NUT) = .FALSE.
         UTLUN(NUT) = LUN

      END DO

!
!     Add a few more files to the file table.
!
      DO I = 1, 5

         CALL REPMI ( FNMTMP, '#', I+10, FNAME )

         NFT        = NFT+1
         FTABS(NFT) = ABS(NFT)
         FTAMH(NFT) = READ
         FTARC(NFT) = DAF
         FTBFF(NFT) = BIGI3E
         FTHAN(NFT) = NFT
         FTNAM(NFT) = FNAME
         FTRTM(NFT) = RTRIM( FTNAM(NFT) )
         FTMNM(NFT) = FILMNM(I+10)

      END DO

      EXISTS = .FALSE.
      OPENED = .TRUE.
      HANDLE = -1
      FOUND  = .FALSE.

!
!     Invoke the module.
!
      CALL ZZDDHF2H ( 'spk13.bsp', FTABS,  FTAMH, FTARC, FTBFF, &
                      FTHAN,       FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                      UTHAN,       UTLCK,  UTLUN, NUT,   EXISTS, &
                      OPENED,      HANDLE, FOUND, MNM                 )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,    OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,     OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 13, 0, OK )

!
!     Close the test files.
!
      DO I = 1, NUT
         CLOSE ( UNIT = UTLUN(I) )
      END DO

!
!     We need to free the logical unit reserved implicitly by ZZDDHF2H's
!     use of ZZDDHRMU.
!
      INDEX = ISRCHI ( 0, NUT, UTHAN )

      IF ( INDEX .GT. 0 ) THEN
         CALL FRELUN ( UTLUN(INDEX) )
      END IF

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unit table full, file not found, but in file ' &
      //           'table case.'                                   )

!
!     Open the files we want to put in the unit table.
!
      NUT = 0
      NFT = 0

      DO I = 1, UTSIZE

!
!        Get a unit.
!
         CALL GETLUN ( LUN )

!
!        Compute the filename.
!
         CALL REPMI  ( FNMTMP, '#', I, FNAME  )

         OPEN ( UNIT   = LUN, &
                FILE   = FNAME, &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'OLD', &
                IOSTAT = IOSTAT      )

!
!        Check the IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF

!
!        Setup the inputs and output default values. Create a row for
!        FNAME in the file table and unit tables.
!
         NFT        = NFT+1
         FTABS(NFT) = ABS(NFT)
         FTAMH(NFT) = READ
         FTARC(NFT) = DAF
         FTBFF(NFT) = BIGI3E
         FTHAN(NFT) = NFT
         FTNAM(NFT) = FNAME
         FTRTM(NFT) = RTRIM( FTNAM(NFT) )
         FTMNM(NFT) = FILMNM(I)

         NUT        = NUT+1
         UTCST(NUT) = I*2
         UTHAN(NUT) = FTHAN(NFT)
         UTLCK(NUT) = .FALSE.
         UTLUN(NUT) = LUN

      END DO

!
!     Add a few more files to the file table.
!
      DO I = 1, 5

         CALL REPMI ( FNMTMP, '#', I+UTSIZE, FNAME )

         NFT        = NFT+1
         FTABS(NFT) = ABS(NFT)
         FTAMH(NFT) = READ
         FTARC(NFT) = DAF
         FTBFF(NFT) = BIGI3E
         FTHAN(NFT) = NFT
         FTNAM(NFT) = FNAME
         FTRTM(NFT) = RTRIM( FTNAM(NFT) )
         FTMNM(NFT) = FILMNM(I+UTSIZE)

      END DO

      EXISTS = .FALSE.
      OPENED = .TRUE.
      HANDLE = -1
      FOUND  = .FALSE.

      CALL REPMI ( FNMTMP, '#', NUMFIL, FNAME )

!
!     Invoke the module.
!
      CALL ZZDDHF2H ( FNAME,  FTABS,  FTAMH, FTARC, FTBFF, &
                      FTHAN,  FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                      UTHAN,  UTLCK,  UTLUN, NUT,   EXISTS, &
                      OPENED,  HANDLE, FOUND, MNM                 )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,         OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,        OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,         OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', NUMFIL, 0, OK )

!
!     Close the test files.
!
      DO I = 1, UTSIZE
         CLOSE ( UNIT = UTLUN(I) )
      END DO

!
!     We need to free the logical unit reserved implicitly by ZZDDHF2H's
!     use of ZZDDHRMU.
!
      INDEX = ISRCHI ( 0, NUT, UTHAN )

      IF ( INDEX .GT. 0 ) THEN
         CALL FRELUN ( UTLUN(INDEX) )
      END IF

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unit table full, file not found case.' )

!
!     Open the files we want to put in the unit table.
!
      NUT = 0
      NFT = 0

      DO I = 1, UTSIZE

!
!        Get a unit.
!
         CALL GETLUN ( LUN )

!
!        Compute the filename.
!
         CALL REPMI  ( FNMTMP, '#', I, FNAME  )

         OPEN ( UNIT   = LUN, &
                FILE   = FNAME, &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'OLD', &
                IOSTAT = IOSTAT      )

!
!        Check the IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF

!
!        Setup the inputs and output default values. Create a row for
!        FNAME in the file table and unit tables.
!
         NFT        = NFT+1
         FTABS(NFT) = ABS(NFT)
         FTAMH(NFT) = READ
         FTARC(NFT) = DAF
         FTBFF(NFT) = BIGI3E
         FTHAN(NFT) = NFT
         FTNAM(NFT) = FNAME
         FTRTM(NFT) = RTRIM( FTNAM(NFT) )
         FTMNM(NFT) = FILMNM(I)

         NUT        = NUT+1
         UTCST(NUT) = I*2
         UTHAN(NUT) = FTHAN(NFT)
         UTLCK(NUT) = .FALSE.
         UTLUN(NUT) = LUN

      END DO

!
!     Add a few more files to the file table.
!
      DO I = 1, 4

         CALL REPMI ( FNMTMP, '#', I+UTSIZE, FNAME )

         NFT        = NFT+1
         FTABS(NFT) = ABS(NFT)
         FTAMH(NFT) = READ
         FTARC(NFT) = DAF
         FTBFF(NFT) = BIGI3E
         FTHAN(NFT) = NFT
         FTNAM(NFT) = FNAME
         FTRTM(NFT) = RTRIM( FTNAM(NFT) )
         FTMNM(NFT) = FILMNM( I+UTSIZE )

      END DO

      EXISTS = .FALSE.
      OPENED = .TRUE.
      HANDLE = -1
      FOUND  = .FALSE.

      CALL REPMI ( FNMTMP, '#', NUMFIL, FNAME )

!
!     Invoke the module.
!
      CALL ZZDDHF2H ( FNAME,  FTABS,  FTAMH, FTARC, FTBFF, &
                      FTHAN,  FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                      UTHAN,  UTLCK,  UTLUN, NUT,   EXISTS, &
                      OPENED, HANDLE, FOUND, MNM                 )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE.,   OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,   OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,    OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )

!
!     Close the test files.
!
      DO I = 1, UTSIZE
         CLOSE ( UNIT = UTLUN(I) )
      END DO

!
!     We need to free the logical unit reserved implicitly by ZZDDHF2H's
!     use of ZZDDHRMU.
!
      INDEX = ISRCHI ( 0, NUT, UTHAN )

      IF ( INDEX .GT. 0 ) THEN
         CALL FRELUN ( UTLUN(INDEX) )
      END IF


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
