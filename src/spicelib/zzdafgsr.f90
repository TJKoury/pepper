!$Procedure ZZDAFGSR ( Private --- DAF Get Summary/Descriptor Record )

      SUBROUTINE ZZDAFGSR ( HANDLE, RECNO, ND, NI, DPREC, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a summary/descriptor record from a DAF.
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
      INTEGER               RECNO
      INTEGER               ND
      INTEGER               NI
      DOUBLE PRECISION      DPREC  ( * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of the DAF.
!     RECNO      I   Record number.
!     ND         I   Number of double precision components in a summary.
!     NI         I   Number of integer components in a summary.
!     DPREC      O   Contents of the record.
!     FOUND      O   Logical indicating whether the record was found.
!
! $ Detailed_Input
!
!     HANDLE     is the handle associated with the DAF.
!
!     RECNO      is the record number of a particular summary record
!                within the DAF, whose contents are to be read.
!     ND,
!     NI         are the number of double precision and integer
!                components, respectively, in each array summary
!                in the specified file.
!
! $ Detailed_Output
!
!     DPREC      contains the contents of the specified record from
!                the DAF associated with HANDLE, properly translated
!                for use on the native environment.
!
!     FOUND      is TRUE when the specified record is found, and is
!                FALSE otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
!        found in the set of loaded handles.
!
!     2) Routines in the call tree of this routine may trap and
!        signal errors.
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
!     This routine reads summary records of double precision
!     numbers which contain integers packed through an EQUIVALENCE
!     statement from native and supported non-native DAFs.
!
!     The size of the character buffer and the number of records
!     read may have to change to support new environments.  As of
!     the original release of this routine, all systems currently
!     supported have a 1 kilobyte record length.
!
! $ Examples
!
!     See DAFGSR for sample usage.
!
! $ Restrictions
!
!     1) Numeric data when read as characters from a file preserves
!        the bit patterns present in the file in memory.
!
!     2) A record of double precision data is at most 1024 characters
!        in length.
!
!     3) DPREC has enough space to store 128 double precision numbers.
!
!     4) Characters a byte-sized, 8 characters constitute a double
!        precision number, and 4 characters an integer.
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
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typo in comments. Reordered header sections.
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
!     Length in bytes of double precision numbers and integers.
!
      INTEGER               DPLEN
      PARAMETER           ( DPLEN = 8       )

      INTEGER               INLEN
      PARAMETER           ( INLEN = DPLEN/2 )

!
!     Local Variables
!
      CHARACTER*(CBFSIZ)    CHRBUF
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      DOUBLE PRECISION      DPBUF  ( 128 )

      INTEGER               CINDEX
      INTEGER               DINDEX
      INTEGER               I
      INTEGER               IAMH
      INTEGER               IARCH
      INTEGER               IBFF
      INTEGER               INBUF  ( 256 )
      INTEGER               IOSTAT
      INTEGER               LEFT
      INTEGER               LUN
      INTEGER               NATBFF
      INTEGER               NSUM
      INTEGER               SUMSIZ

      LOGICAL               FIRST
      LOGICAL               LOCFND

!
!     Equivalence DPBUF to INBUF.
!
      EQUIVALENCE         ( DPBUF, INBUF )

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
         CALL CHKIN ( 'ZZDAFGSR' )
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
            CALL CHKOUT ( 'ZZDAFGSR'                                   )
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
         CALL CHKOUT ( 'ZZDAFGSR'                                   )
         RETURN
      END IF

!
!     Now get a logical unit for the handle.  Check FAILED()
!     in case an error occurs.
!
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., LUN )

      IF ( FAILED() ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'ZZDAFGSR' )
         RETURN
      END IF

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      IF ( IBFF .EQ. NATBFF ) THEN

!
!        In the native case, just read the array of double precision
!        numbers from the file.  The packed integers will be
!        processed properly by the READ.
!
         READ ( UNIT   = LUN, &
                REC    = RECNO, &
                IOSTAT = IOSTAT ) ( DPBUF(I), I = 1, 128 )

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CALL CHKOUT ( 'ZZDAFGSR' )
            RETURN
         END IF

!
!     Process the non-native binary file format case.
!
      ELSE

!
!        Read the record as characters.
!
         READ ( UNIT   = LUN, &
                REC    = RECNO, &
                IOSTAT = IOSTAT ) CHRBUF

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CALL CHKOUT ( 'ZZDAFGSR' )
            RETURN
         END IF

!
!        Translate the summary record.  First extract the leading
!        3 double precision numbers from the summary record as these
!        respectively are NEXT, PREV, and NSUM.
!
         CALL ZZXLATED ( IBFF, CHRBUF(1:3*DPLEN), 128, DPBUF )

!
!        Check FAILED() in case the translation process fails for
!        any reason.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDAFGSR' )
            RETURN
         END IF

!
!        Convert NSUM to an integer, and compute the number of
!        double precision numbers required to store each individual
!        summary in the record.
!
         NSUM   = INT ( DPBUF(3) )
         SUMSIZ = ( ND + (NI+1)/2 )

!
!        Convert each of the summaries one at a time.
!
         DO I = 1, NSUM

!
!           Set the start index into the double precision array
!           to receive the components.  Also set the character
!           substring index to the start location for this summary.
!           In the diagram below, each box represents a double
!           precision number.  The figure assumes SUMSIZ is 5
!           double precision numbers:
!
!                 |--- 1 ---|--- 2 ---|--- 3 ---|   |- (I-1) -|
!           -------------------------------------   -------------
!           | | | | | | | | | | | | | | | | | | |...| | | | | | |...
!           -------------------------------------   -------------
!           |-----|                                            ^
!              ^                                               |
!              |                                            Summary
!           NEXT, PREV, NSUM                                 Start
!
            DINDEX = (I-1)*SUMSIZ + 4
            CINDEX = DPLEN*(DINDEX-1) + 1

!
!           First, check to see if there are any double precision
!           numbers to translate.  If so, translate, and then
!           increment DINDEX and CINDEX accordingly.
!
            IF ( ND .GT. 0 ) THEN

!
!              DPBUF has room for 128 double precision numbers
!              total.  Compute the amount of space left in the
!              buffer.
!
               LEFT = 128 - (I-1)*SUMSIZ - 3

               CALL ZZXLATED ( IBFF, &
                               CHRBUF(CINDEX:CINDEX+ND*DPLEN-1), &
                               LEFT, &
                               DPBUF(DINDEX)                     )

!
!              If the translation routine fails for any reason,
!              check out and return.
!
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZDAFGSR' )
                  RETURN
               END IF

               DINDEX = DINDEX + ND
               CINDEX = CINDEX + DPLEN*ND

            END IF

!
!           At this point DINDEX and CINDEX are pointing at the
!           locations for the packed integers in the record.
!           Use DINDEX to compute the index into INBUF, the
!           equivalenced integer buffer and translate.
!
            IF ( NI .GT. 0 ) THEN

!
!              INBUF has room for 256 integers total.  Compute
!              the amount of space left in the buffer.  Since
!              it is equivalenced to DPBUF, account for the
!              double precision numbers that were just added.
!
               LEFT = 256 - 2*(I-1)*SUMSIZ - ND*2 - 6

               CALL ZZXLATEI ( IBFF, &
                               CHRBUF(CINDEX:CINDEX+NI*INLEN-1), &
                               LEFT, &
                               INBUF(2*DINDEX-1)                 )

!
!              If the translation routine fails for any reason,
!              check out and return.
!
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZDAFGSR' )
                  RETURN
               END IF

!
!              Now check to see if NI is odd.  If so, then zero
!              the last integer occupied by the newly translated
!              summary.  This is necessary to purge any garbage
!              present in memory.
!
               IF ( MOD(NI,2) .EQ. 1 ) THEN
                  INBUF(2*DINDEX-1+NI) = 0
               END IF

            END IF

         END DO

!
!        Translating garbage is a bad idea in general, so set
!        the any remaining double precision numbers in the summary
!        record to 0.
!
         DINDEX = NSUM*SUMSIZ + 4

         DO I = DINDEX, 128
            DPBUF(I) = 0
         END DO

      END IF

!
!     Transfer the DPs to the output argument and return to the
!     caller.
!
      FOUND = .TRUE.
      CALL MOVED ( DPBUF, 128, DPREC )

      CALL CHKOUT( 'ZZDAFGSR' )
      RETURN

      END
