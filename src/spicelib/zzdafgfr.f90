!$Procedure ZZDAFGFR ( Private --- DAF Get File Record )

      SUBROUTINE ZZDAFGFR ( HANDLE, IDWORD, ND,   NI,   IFNAME, &
                            FWARD,  BWARD,  FREE, FOUND         )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read the contents of the file record of a DAF.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         IDWORD
      INTEGER               ND
      INTEGER               NI
      CHARACTER*(*)         IFNAME
      INTEGER               FWARD
      INTEGER               BWARD
      INTEGER               FREE
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of the DAF.
!     IDWORD     O   DAF ID Word that indicates file type.
!     ND         O   Number of double precision components in summaries.
!     NI         O   Number of integer components in summaries.
!     IFNAME     O   Internal file name.
!     FWARD      O   Forward list pointer.
!     BWARD      O   Backward list pointer.
!     FREE       O   Free address pointer.
!     FOUND      O   Logical indicating whether the record was found.
!
! $ Detailed_Input
!
!     HANDLE     is the handle associated with the DAF.
!
! $ Detailed_Output
!
!     IDWORD     is a character string identifying the architecture
!                and type of a SPICE binary kernel.  In this case
!                it will be a string identifying the type of DAF.
!
!     ND,
!     NI         are the number of double precision and integer
!                components, respectively, in each array summary in
!                the specified file.
!
!     IFNAME     is the internal file name stored in the first
!                (or file) record of the specified file.
!
!     FWARD      is the forward list pointer. This points to the
!                first summary record in the file. (Records between
!                the first record and the first summary record are
!                reserved when the file is created, and are invisible
!                to DAF routines.)
!
!     BWARD      is the backward list pointer. This points
!                to the final summary record in the file.
!
!     FREE       is the free address pointer. This contains the
!                first free address in the file. (That is, the
!                initial address of the next array to be added
!                to the file.)
!
!     FOUND      is TRUE when the file record is found, and is
!                FALSE otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
!        found in the set of loaded handles.  The output arguments
!        are unmodified when this error occurs.
!
!     2) Routines in the call tree of this routine may trap and
!        signal errors.  The output arguments are unmodified in
!        these cases.
!
! $ Files
!
!     This routine reads data from the DAF associated with HANDLE.
!     This action may result in connecting a logical unit to the
!     file, if the handle manager has rotated the file out of the
!     unit table.
!
! $ Particulars
!
!     This routine reads the publicly available components of
!     file records from native and supported non-native DAFs.
!
!     The size of the character buffer and the number of records
!     read may have to change to support new environments.  As of
!     the original release of this routine, all systems currently
!     supported have a 1 kilobyte record length.
!
! $ Examples
!
!     See DAFRFR for sample usage.
!
! $ Restrictions
!
!     1) Numeric data when read as characters from a file preserves
!        the bit patterns present in the file in memory.
!
!     2) A record of double precision data is at most 1024 characters
!        in length.
!
!     3) Future updates to this module must preserve the fact that
!        FOUND is returned as FALSE whenever an error occurs.  An
!        incompletely translated or extracted file record is NOT
!        FOUND.
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
!-    SPICELIB Version 1.0.2, 03-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.0.1, 24-JUL-2014 (NJB)
!
!        Corrected typo in Procedure line of header.
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
!     Record Number of the file record in a DAF.
!
      INTEGER               FRLOCR
      PARAMETER           ( FRLOCR =  1 )

!
!     Length of the IDWORD string.
!
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )

!
!     Length of the internal filename string.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

!
!     Starting location in bytes of the internal filename in the
!     file record.
!
      INTEGER               IFNSTR
      PARAMETER           ( IFNSTR = 17 )

!
!     Size of an integer in bytes.
!
      INTEGER               INLEN
      PARAMETER           ( INLEN  =  4 )

!
!     Local Variables
!
      CHARACTER*(CBFSIZ)    CHRBUF
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(IDWLEN)    LOCIDW
      CHARACTER*(IFNLEN)    LOCIFN
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      INTEGER               CINDEX
      INTEGER               I
      INTEGER               IAMH
      INTEGER               IARCH
      INTEGER               IBFF
      INTEGER               IOSTAT
      INTEGER               LOCBWD
      INTEGER               LOCFRE
      INTEGER               LOCFWD
      INTEGER               LOCND
      INTEGER               LOCNI
      INTEGER               LUN
      INTEGER               NATBFF

      LOGICAL               FIRST
      LOGICAL               LOCFND

!
!     Saved Variables
!
      SAVE                  FIRST
      SAVE                  NATBFF
      SAVE                  STRBFF

!
!     Data Statements
!
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDAFGFR' )
      END IF

!
!     Perform some initialization tasks.
!
      IF ( FIRST ) THEN

!
!        Populate STRBFF, the buffer that contains the labels
!        for each binary file format.
!
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO

!
!        Fetch the native binary file format and determine its
!        integer code.
!
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )

         NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )

         IF ( NATBFF .EQ. 0 ) THEN

            CALL SETMSG ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZDAFGFR'                                   )
            RETURN

         END IF

!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

      END IF

!
!     Assume the data record will not be found, until it has been read
!     from the file, and if necessary, successfully translated.
!
      FOUND = .FALSE.

!
!     Retrieve information regarding the file from the handle manager.
!     The value of IARCH is not a concern, since this is a DAF routine
!     all values passed into handle manager entry points will have
!     'DAF' as their architecture arguments.
!
      CALL ZZDDHNFO ( HANDLE, FNAME, IARCH, IBFF, IAMH, LOCFND )

      IF ( .NOT. LOCFND ) THEN
         CALL SETMSG ( 'Unable to locate file associated with ' &
         //            'HANDLE, #.  The most likely cause of this ' &
         //            'is the file that you are trying to read ' &
         //            'has been closed.'                           )
         CALL ERRINT ( '#', HANDLE                                  )
         CALL SIGERR ( 'SPICE(HANDLENOTFOUND)'                      )
         CALL CHKOUT ( 'ZZDAFGFR'                                   )
         RETURN
      END IF

!
!     Now get a logical unit for the handle.  Check FAILED() in
!     case an error occurs.
!
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., LUN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDAFGFR' )
         RETURN
      END IF

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      IF ( IBFF .EQ. NATBFF ) THEN

!
!        In the native case, just read the components of the file
!        record from the file.
!
         READ ( UNIT   = LUN, &
                REC    = FRLOCR, &
                IOSTAT = IOSTAT  ) LOCIDW, LOCND,  LOCNI, LOCIFN, &
                                   LOCFWD, LOCBWD, LOCFRE

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CALL CHKOUT ( 'ZZDAFGFR' )
            RETURN
         END IF

!
!     Process the non-native binary file format case.
!
      ELSE

!
!        Read the data record as characters.
!
         READ ( UNIT   = LUN, &
                REC    = FRLOCR, &
                IOSTAT = IOSTAT  ) CHRBUF

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CALL CHKOUT ( 'ZZDAFGFR' )
            RETURN
         END IF

!
!        Assign the character components of the file record.
!
         LOCIDW = CHRBUF( 1      : IDWLEN            )
         LOCIFN = CHRBUF( IFNSTR : (IFNSTR+IFNLEN-1) )

!
!        Convert the integer components.
!
         CINDEX = IDWLEN + 1
         CALL ZZXLATEI ( IBFF, CHRBUF(CINDEX:CINDEX+INLEN-1), &
                         1,    LOCND                          )

         CINDEX = CINDEX + INLEN
         CALL ZZXLATEI ( IBFF, CHRBUF(CINDEX:CINDEX+INLEN-1), &
                         1,    LOCNI                          )

         CINDEX = IFNSTR + IFNLEN
         CALL ZZXLATEI ( IBFF, CHRBUF(CINDEX:CINDEX+INLEN-1), &
                         1,    LOCFWD                         )

         CINDEX = CINDEX + INLEN
         CALL ZZXLATEI ( IBFF, CHRBUF(CINDEX:CINDEX+INLEN-1), &
                         1,    LOCBWD                         )

         CINDEX = CINDEX + INLEN
         CALL ZZXLATEI ( IBFF, CHRBUF(CINDEX:CINDEX+INLEN-1), &
                         1,    LOCFRE                         )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDAFGFR' )
            RETURN
         END IF

      END IF

!
!     Transfer the contents of the record to the output arguments
!     and return to the caller.
!
      FOUND = .TRUE.

      IDWORD = LOCIDW
      ND     = LOCND
      NI     = LOCNI
      IFNAME = LOCIFN
      FWARD  = LOCFWD
      BWARD  = LOCBWD
      FREE   = LOCFRE

      CALL CHKOUT( 'ZZDAFGFR' )
      RETURN

      END
