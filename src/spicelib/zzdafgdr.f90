!$Procedure ZZDAFGDR ( Private --- DAF Get Data Record )

      SUBROUTINE ZZDAFGDR ( HANDLE, RECNO, DPREC, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a data record from a DAF.
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
      DOUBLE PRECISION      DPREC  ( * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of the DAF.
!     RECNO      I   Record number.
!     DPREC      O   Contents of the record.
!     FOUND      O   Logical indicating whether the record was found.
!
! $ Detailed_Input
!
!     HANDLE     is the handle associated with the DAF.
!
!     RECNO      is the record number of a particular double precision
!                record within the DAF, whose contents are to be read.
!
! $ Detailed_Output
!
!     DPREC      contains the contents of the specified record from
!                the DAF associated with HANDLE.
!
!     FOUND      is TRUE when the specified record is found, and is
!                FALSE otherwise.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine reads data from the DAF associated with HANDLE.
!     This action may result in connecting a logical unit to the
!     file, if the handle manager has rotated the file out of the
!     unit table.
!
! $ Exceptions
!
!     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
!        found in the set of loaded handles.
!
!     2) Routines in the call tree of this routine may trap and
!        signal errors.
!
! $ Particulars
!
!     This routine reads records of double precision numbers
!     from native and supported non-native DAFs.
!
!     The size of the character buffer and the number of records
!     read may have to change to support new environments.  As of
!     the original release of this routine, all systems currently
!     supported have a 1 kilobyte record length.
!
! $ Examples
!
!     See DAFGDR for sample usage.
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
!     4) RECNO points to a record that contains double precision data
!        in the file to which HANDLE refers.
!
!     5) ZZXLATED will translate all possible inputs (garbage or not)
!        coming from the file associated with HANDLE.
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
!     Local Variables
!
      CHARACTER*(CBFSIZ)    CHRBUF
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      DOUBLE PRECISION      DPBUF  ( 128 )

      INTEGER               I
      INTEGER               IAMH
      INTEGER               IARCH
      INTEGER               IBFF
      INTEGER               IOSTAT
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
         CALL CHKIN ( 'ZZDAFGDR' )
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
            CALL CHKOUT ( 'ZZDAFGDR'                                   )
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
         CALL CHKOUT ( 'ZZDAFGDR'                                   )
         RETURN
      END IF

!
!     Now get a logical unit for the handle.  Check FAILED() in
!     case an error occurs.
!
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., LUN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDAFGDR' )
         RETURN
      END IF

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      IF ( IBFF .EQ. NATBFF ) THEN

!
!        In the native case, just read the double precision
!        numbers from the file.
!
         READ ( UNIT   = LUN, &
                REC    = RECNO, &
                IOSTAT = IOSTAT ) ( DPBUF(I), I = 1, 128 )

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CALL CHKOUT ( 'ZZDAFGDR' )
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
                REC    = RECNO, &
                IOSTAT = IOSTAT ) CHRBUF

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         IF ( IOSTAT .NE. 0 ) THEN
            CALL CHKOUT ( 'ZZDAFGDR' )
            RETURN
         END IF

!
!        Translate the data record.  Assume (improperly in the
!        general case) that ZZXLATED will translate the contents
!        of the entire record without signaling an error.  This
!        is appropriate at this stage since ZZXLATED simply swaps
!        bytes between BIG-IEEE and LTL-IEEE environments.  In
!        the future, updates may be necessary to prevent
!        processing of garbage data.
!
         CALL ZZXLATED ( IBFF, CHRBUF, 128, DPBUF )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDAFGDR' )
            RETURN
         END IF

      END IF

!
!     Transfer the DPs to the output argument and return
!     to the caller.
!
      FOUND = .TRUE.
      CALL MOVED ( DPBUF, 128, DPREC )

      CALL CHKOUT( 'ZZDAFGDR' )
      RETURN

      END
