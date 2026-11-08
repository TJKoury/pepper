C$Procedure F_DDHF2H ( ZZDDHF2H Test Family )
 
      SUBROUTINE F_DDHF2H ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHF2H
C     routine.
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
C     This routine exercises ZZDDHF2H's logic.
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
C     F.S. Turner     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0, 12-APR-2012 (BVS)
C
C        Updated for changed ZZDDHF2H calling sequence and use
C        of magic numbers.
C
C-    TSPICE Version 2.0.0, 05-AUG-2002 (FST)
C
C        Promoted module to master file status.  Check that
C        read-only files on VAX systems can be opened by
C        zzddhf2h if necessary.
C
C-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
      INTEGER               ISRCHI
      DOUBLE PRECISION      ZZDDHMNM
 
C
C     Local Parameters
C
C
C     This parameter defines the number of test files we are going
C     to create.  It should be something more than UTSIZE, to
C     properly execute logic in ZZDDHF2H.
C
      INTEGER               NUMFIL
      PARAMETER           ( NUMFIL = UTSIZE+5 )
 
C
C     Local Variables
C
C     File Table
C
      INTEGER               NFT
 
      INTEGER               FTABS  ( FTSIZE )
      INTEGER               FTAMH  ( FTSIZE )
      INTEGER               FTARC  ( FTSIZE )
      INTEGER               FTBFF  ( FTSIZE )
      INTEGER               FTHAN  ( FTSIZE )
      CHARACTER*(FILEN)     FTNAM  ( FTSIZE )
      INTEGER               FTRTM  ( FTSIZE )
      DOUBLE PRECISION      FTMNM  ( FTSIZE )
 
C
C     Unit Table
C
      INTEGER               NUT
 
      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )
 
C
C     Other Variables
C
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
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHF2H' )
 
      FNMTMP = 'spk#.bsp'
 
C
C     Start by creating NUMFIL files for testing purposes.
C
      DO I = 1, NUMFIL
 
         CALL REPMI  ( FNMTMP, '#', I, FNAME  )
         CALL TSTSPK ( FNAME, .FALSE., HANDLE )
 
C
C        Save the magic number for each test file. Open file first.
C
         CALL GETLUN ( LUN )
 
         OPEN ( UNIT   = LUN,
     .          FILE   = FNAME,
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'OLD',
     .          IOSTAT = IOSTAT      )
 
C
C        Check the IOSTAT.
C
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF
 
C
C        Compute the magic number.
C
         FILMNM( I ) = ZZDDHMNM( LUN )
 
C
C        Close the file.
C
         CLOSE ( UNIT = LUN )
 
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'File does not exist exceptional case.' )
 
C
C     First, kill the file name we are about to check against, since
C     it should not exist.
C
      CALL KILFIL ( 'spk0.bsp' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 0
      NFT = 0
 
      EXISTS = .TRUE.
      OPENED = .TRUE.
      HANDLE = -1
      FOUND  = .TRUE.
 
C
C     Invoke the module.
C
      CALL ZZDDHF2H ( 'spk0.bsp', FTABS,  FTAMH, FTARC, FTBFF,
     .                FTHAN,      FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                UTHAN,      UTLCK,  UTLUN, NUT,   EXISTS,
     .                OPENED,     HANDLE, FOUND, MNM                 )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE.,   OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,   OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .FALSE.,   OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'File exists and is opened externally exceptional '
     .//           'case.'                                             )
 
C
C     Open the first test file we created.
C
      CALL GETLUN ( LUN )
 
      OPEN ( UNIT   = LUN,
     .       FILE   = 'spk1.bsp',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT      )
 
C
C     Check the IOSTAT.
C
      IF ( IOSTAT .NE. 0 ) THEN
         CLOSE ( UNIT = LUN )
         CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
         CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
      END IF
 
C
C     Setup the inputs and output default values.
C
      NUT = 0
      NFT = 0
 
      EXISTS = .FALSE.
      OPENED = .FALSE.
      HANDLE = -1
      FOUND  = .TRUE.
 
C
C     Invoke the module.
C
      CALL ZZDDHF2H ( 'spk1.bsp', FTABS,  FTAMH, FTARC, FTBFF,
     .                FTHAN,      FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                UTHAN,      UTLCK,  UTLUN, NUT,   EXISTS,
     .                OPENED,     HANDLE, FOUND, MNM                 )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE.,   OK )
      CALL CHCKSL ( 'OPENED', OPENED, .TRUE.,    OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,    OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close the test file.
C
      CLOSE ( UNIT = LUN )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'File exists and is opened internally and is in '
     .//           'the unit table case.'                            )
 
C
C     Open the first test file we created.
C
      CALL GETLUN ( LUN )
 
      OPEN ( UNIT   = LUN,
     .       FILE   = 'spk1.bsp',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT      )
 
C
C     Check the IOSTAT.
C
      IF ( IOSTAT .NE. 0 ) THEN
         CLOSE ( UNIT = LUN )
         CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
         CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
      END IF
 
C
C     Setup the inputs and output default values. Create a row for
C     spk1.bsp in the file table and unit tables.
C
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
 
C
C     Invoke the module.
C
      CALL ZZDDHF2H ( 'spk1.bsp', FTABS,  FTAMH, FTARC, FTBFF,
     .                FTHAN,      FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                UTHAN,      UTLCK,  UTLUN, NUT,   EXISTS,
     .                OPENED,     HANDLE, FOUND, MNM                 )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,    OK )
      CALL CHCKSL ( 'OPENED', OPENED, .TRUE.,    OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,    OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 1, 0, OK )
 
C
C     Close the test file.
C
      CLOSE ( UNIT = LUN )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'File exists and is opened internally and is not '
     .//           'in the unit table case.'                          )
 
C
C     Open the files we want to put in the unit table.
C
      NUT = 0
      NFT = 0
 
      DO I = 1, 10
 
C
C        Get a unit.
C
         CALL GETLUN ( LUN )
 
C
C        Compute the filename.
C
         CALL REPMI  ( FNMTMP, '#', I, FNAME  )
 
         OPEN ( UNIT   = LUN,
     .          FILE   = FNAME,
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'OLD',
     .          IOSTAT = IOSTAT      )
 
C
C        Check the IOSTAT.
C
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF
 
C
C        Setup the inputs and output default values. Create a row for
C        FNAME in the file table and unit tables.
C
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
 
C
C     Add a few more files to the file table.
C
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
 
C
C     Invoke the module.
C
      CALL ZZDDHF2H ( 'spk13.bsp', FTABS,  FTAMH, FTARC, FTBFF,
     .                FTHAN,       FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                UTHAN,       UTLCK,  UTLUN, NUT,   EXISTS,
     .                OPENED,      HANDLE, FOUND, MNM                 )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,    OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,     OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 13, 0, OK )
 
C
C     Close the test files.
C
      DO I = 1, NUT
         CLOSE ( UNIT = UTLUN(I) )
      END DO
 
C
C     We need to free the logical unit reserved implicitly by ZZDDHF2H's
C     use of ZZDDHRMU.
C
      INDEX = ISRCHI ( 0, NUT, UTHAN )
 
      IF ( INDEX .GT. 0 ) THEN
         CALL FRELUN ( UTLUN(INDEX) )
      END IF
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unit table full, file not found, but in file '
     .//           'table case.'                                   )
 
C
C     Open the files we want to put in the unit table.
C
      NUT = 0
      NFT = 0
 
      DO I = 1, UTSIZE
 
C
C        Get a unit.
C
         CALL GETLUN ( LUN )
 
C
C        Compute the filename.
C
         CALL REPMI  ( FNMTMP, '#', I, FNAME  )
 
         OPEN ( UNIT   = LUN,
     .          FILE   = FNAME,
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'OLD',
     .          IOSTAT = IOSTAT      )
 
C
C        Check the IOSTAT.
C
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF
 
C
C        Setup the inputs and output default values. Create a row for
C        FNAME in the file table and unit tables.
C
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
 
C
C     Add a few more files to the file table.
C
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
 
C
C     Invoke the module.
C
      CALL ZZDDHF2H ( FNAME,  FTABS,  FTAMH, FTARC, FTBFF,
     .                FTHAN,  FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                UTHAN,  UTLCK,  UTLUN, NUT,   EXISTS,
     .                OPENED,  HANDLE, FOUND, MNM                 )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,         OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,        OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,         OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', NUMFIL, 0, OK )
 
C
C     Close the test files.
C
      DO I = 1, UTSIZE
         CLOSE ( UNIT = UTLUN(I) )
      END DO
 
C
C     We need to free the logical unit reserved implicitly by ZZDDHF2H's
C     use of ZZDDHRMU.
C
      INDEX = ISRCHI ( 0, NUT, UTHAN )
 
      IF ( INDEX .GT. 0 ) THEN
         CALL FRELUN ( UTLUN(INDEX) )
      END IF
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unit table full, file not found case.' )
 
C
C     Open the files we want to put in the unit table.
C
      NUT = 0
      NFT = 0
 
      DO I = 1, UTSIZE
 
C
C        Get a unit.
C
         CALL GETLUN ( LUN )
 
C
C        Compute the filename.
C
         CALL REPMI  ( FNMTMP, '#', I, FNAME  )
 
         OPEN ( UNIT   = LUN,
     .          FILE   = FNAME,
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'OLD',
     .          IOSTAT = IOSTAT      )
 
C
C        Check the IOSTAT.
C
         IF ( IOSTAT .NE. 0 ) THEN
            CLOSE ( UNIT = LUN )
            CALL TSTMSG ( '#', 'This IOSTAT error should never occur.' )
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
         END IF
 
C
C        Setup the inputs and output default values. Create a row for
C        FNAME in the file table and unit tables.
C
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
 
C
C     Add a few more files to the file table.
C
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
 
C
C     Invoke the module.
C
      CALL ZZDDHF2H ( FNAME,  FTABS,  FTAMH, FTARC, FTBFF,
     .                FTHAN,  FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                UTHAN,  UTLCK,  UTLUN, NUT,   EXISTS,
     .                OPENED, HANDLE, FOUND, MNM                 )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE.,   OK )
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE.,   OK )
      CALL CHCKSL ( 'EXISTS', EXISTS, .TRUE.,    OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', 0, 0, OK )
 
C
C     Close the test files.
C
      DO I = 1, UTSIZE
         CLOSE ( UNIT = UTLUN(I) )
      END DO
 
C
C     We need to free the logical unit reserved implicitly by ZZDDHF2H's
C     use of ZZDDHRMU.
C
      INDEX = ISRCHI ( 0, NUT, UTHAN )
 
      IF ( INDEX .GT. 0 ) THEN
         CALL FRELUN ( UTLUN(INDEX) )
      END IF
 
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
