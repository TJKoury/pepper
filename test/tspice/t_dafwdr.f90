!$Procedure T_DAFWDR ( Write a DAF summary/descriptor record to file )

      SUBROUTINE T_DAFWDR ( UNIT, RECNO, OUTBFF, NUMDP, ARRAY )

! $ Abstract
!
!     Write a data record to a DAF.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               UNIT
      INTEGER               RECNO
      INTEGER               OUTBFF
      INTEGER               NUMDP
      DOUBLE PRECISION      ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Unit attached to DAF to receive new file record.
!     RECNO      I   Record number.
!     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
!     NUMDP      I   Number of entries in ARRAY to write to the file.
!     ARRAY      I   Sequence of packed summaries to write.
!
! $ Detailed_Input
!
!     UNIT       is the logical unit attached to the DAF to receive
!                the new summary record.
!
!     RECNO      is the record number of the record to write the summary
!                record.
!
!     OUTBFF     is an integer code that indicates the binary file
!                format targeted for OUTPUT.  Acceptable values
!                are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.incx'.
!
!     NUMDP      is the number of entries in ARRAY to write to the DAF.
!
!     ARRAY      is a double precision array of data to be written to
!                the DAF.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine will update the contents of the record RECNO
!     in the file associated with UNIT.
!
! $ Exceptions
!
!     1) SPICE(NUMOUTOFBOUNDS) is signaled if NUMDP does not possess
!        a reasonable value.
!
! $ Particulars
!
!     This routine takes the contents of ARRAY, converts it to the
!     appropriate format if necessary, and writes it to RECNO in
!     the file attached to UNIT.
!
!     Note: This code is not a model of perfection.  It is hacked
!     together from pieces of code written elsewhere to support
!     creation of non-native binaries for testing purposes only.
!
! $ Examples
!
!     See F_ZZDGDR for usage.
!
! $ Restrictions
!
!     1) The file was created with T_DAFOPN.
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
!-    SPICELIB Version 1.0.0, 28-OCT-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               ISRCHC
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               RECLEN
      PARAMETER           ( RECLEN = 1024 )

      INTEGER               DPRLEN
      PARAMETER           ( DPRLEN = 128 )

      INTEGER               INTLEN
      PARAMETER           ( INTLEN = 4 )

!
!     Local Variables
!
      CHARACTER*(RECLEN)    RECORD
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      DOUBLE PRECISION      DATREC ( DPRLEN )

      INTEGER               I
      INTEGER               NATBFF
      INTEGER               IOSTAT

      LOGICAL               FIRST

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
         CALL CHKIN ( 'T_DAFWDR' )
      END IF

!
!     Perform some initialization tasks.
!
      IF ( FIRST ) THEN

!
!        Populate STRBFF with the appropriate binary file
!        format labels.
!
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO

!
!        Fetch the native binary file format.
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
            CALL CHKOUT ( 'T_DAFWDR'                                   )
            RETURN

         END IF

!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

      END IF

!
!     Check to see if OUTBFF is valid.  This should never occur if this
!     routine is called properly.
!
      IF ( ( OUTBFF .LT. 1 ) .OR. ( OUTBFF .GT. NUMBFF ) ) THEN

         CALL SETMSG ( 'The integer code used to indicate the ' &
         //            'binary file format of the input integers, ' &
         //            '#, is out of range.  This error should ' &
         //            'never occur.'                               )
         CALL ERRINT ( '#', OUTBFF                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'T_DAFWDR'                                   )
         RETURN

      END IF

!
!     Perform some simple checks on NUMDP.
!
      IF ( ( NUMDP .LT. 0 ) .OR. ( NUMDP .GT. DPRLEN ) ) THEN
         CALL SETMSG ( '# double precision numbers were requested ' &
         //            'to be written to #.  Each record holds no ' &
         //            'more than # numbers.'                       )
         CALL ERRINT ( '#', NUMDP                                   )
         CALL ERRFNM ( '#', UNIT                                    )
         CALL ERRINT ( '#', DPRLEN                                  )
         CALL SIGERR ( 'SPICE(NUMOUTOFBOUNDS)'                      )
         CALL CHKOUT ( 'T_DAFWDR'                                   )
         RETURN
      END IF

!
!     First, determine if we are to write to the native file format.
!
      IF ( OUTBFF .EQ. NATBFF ) THEN

!
!        Clear DATREC, and then move the appropriate values from
!        ARRAY into it.  This is necessary to write a full record
!        to the file.
!
         CALL CLEARD ( DPRLEN, DATREC )
         CALL MOVED  ( ARRAY, NUMDP, DATREC )

         WRITE ( UNIT   = UNIT, &
                 REC    = RECNO, &
                 IOSTAT = IOSTAT ) ( DATREC(I), I = 1, DPRLEN )

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DAFWDR'                           )
            RETURN
         END IF

!
!     Handle the non-native case.
!
      ELSE

!
!        Clear RECORD.
!
         RECORD = ' '

!
!        Populate RECORD.
!
         CALL T_XLTFWD ( ARRAY, NUMDP, OUTBFF, &
                         RECORD(1:NUMDP*INTLEN*2) )

!
!        Dump the record to the file.
!
         WRITE ( UNIT = UNIT, REC = RECNO, IOSTAT = IOSTAT ) RECORD

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DAFWDR'                           )
            RETURN
         END IF

      END IF

      CALL CHKOUT ( 'T_DAFWDR' )
      RETURN

      END
