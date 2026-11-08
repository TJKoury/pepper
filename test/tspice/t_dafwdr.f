C$Procedure T_DAFWDR ( Write a DAF summary/descriptor record to file )
 
      SUBROUTINE T_DAFWDR ( UNIT, RECNO, OUTBFF, NUMDP, ARRAY )
 
C$ Abstract
C
C     Write a data record to a DAF.
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
C     TEST ROUTINE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      INTEGER               UNIT
      INTEGER               RECNO
      INTEGER               OUTBFF
      INTEGER               NUMDP
      DOUBLE PRECISION      ARRAY ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Unit attached to DAF to receive new file record.
C     RECNO      I   Record number.
C     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
C     NUMDP      I   Number of entries in ARRAY to write to the file.
C     ARRAY      I   Sequence of packed summaries to write.
C
C$ Detailed_Input
C
C     UNIT       is the logical unit attached to the DAF to receive
C                the new summary record.
C
C     RECNO      is the record number of the record to write the summary
C                record.
C
C     OUTBFF     is an integer code that indicates the binary file
C                format targeted for OUTPUT.  Acceptable values
C                are the parameters:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C                as defined in the include file 'zzddhman.incx'.
C
C     NUMDP      is the number of entries in ARRAY to write to the DAF.
C
C     ARRAY      is a double precision array of data to be written to
C                the DAF.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     This routine will update the contents of the record RECNO
C     in the file associated with UNIT.
C
C$ Exceptions
C
C     1) SPICE(NUMOUTOFBOUNDS) is signaled if NUMDP does not possess
C        a reasonable value.
C
C$ Particulars
C
C     This routine takes the contents of ARRAY, converts it to the
C     appropriate format if necessary, and writes it to RECNO in
C     the file attached to UNIT.
C
C     Note: This code is not a model of perfection.  It is hacked
C     together from pieces of code written elsewhere to support
C     creation of non-native binaries for testing purposes only.
C
C$ Examples
C
C     See F_ZZDGDR for usage.
C
C$ Restrictions
C
C     1) The file was created with T_DAFOPN.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 28-OCT-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               RECLEN
      PARAMETER           ( RECLEN = 1024 )
 
      INTEGER               DPRLEN
      PARAMETER           ( DPRLEN = 128 )
 
      INTEGER               INTLEN
      PARAMETER           ( INTLEN = 4 )
 
C
C     Local Variables
C
      CHARACTER*(RECLEN)    RECORD
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR
 
      DOUBLE PRECISION      DATREC ( DPRLEN )
 
      INTEGER               I
      INTEGER               NATBFF
      INTEGER               IOSTAT
 
      LOGICAL               FIRST
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  NATBFF
      SAVE                  STRBFF
 
C
C     Data Statements
C
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_DAFWDR' )
      END IF
 
C
C     Perform some initialization tasks.
C
      IF ( FIRST ) THEN
 
C
C        Populate STRBFF with the appropriate binary file
C        format labels.
C
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO
 
C
C        Fetch the native binary file format.
C
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )
 
         NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )
 
         IF ( NATBFF .EQ. 0 ) THEN
 
            CALL SETMSG ( 'The binary file format, ''#'', is not '
     .      //            'supported by this version of the toolkit. '
     .      //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'T_DAFWDR'                                   )
            RETURN
 
         END IF
 
C
C        Do not perform initialization tasks again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check to see if OUTBFF is valid.  This should never occur if this
C     routine is called properly.
C
      IF ( ( OUTBFF .LT. 1 ) .OR. ( OUTBFF .GT. NUMBFF ) ) THEN
 
         CALL SETMSG ( 'The integer code used to indicate the '
     .   //            'binary file format of the input integers, '
     .   //            '#, is out of range.  This error should '
     .   //            'never occur.'                               )
         CALL ERRINT ( '#', OUTBFF                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'T_DAFWDR'                                   )
         RETURN
 
      END IF
 
C
C     Perform some simple checks on NUMDP.
C
      IF ( ( NUMDP .LT. 0 ) .OR. ( NUMDP .GT. DPRLEN ) ) THEN
         CALL SETMSG ( '# double precision numbers were requested '
     .   //            'to be written to #.  Each record holds no '
     .   //            'more than # numbers.'                       )
         CALL ERRINT ( '#', NUMDP                                   )
         CALL ERRFNM ( '#', UNIT                                    )
         CALL ERRINT ( '#', DPRLEN                                  )
         CALL SIGERR ( 'SPICE(NUMOUTOFBOUNDS)'                      )
         CALL CHKOUT ( 'T_DAFWDR'                                   )
         RETURN
      END IF
 
C
C     First, determine if we are to write to the native file format.
C
      IF ( OUTBFF .EQ. NATBFF ) THEN
 
C
C        Clear DATREC, and then move the appropriate values from
C        ARRAY into it.  This is necessary to write a full record
C        to the file.
C
         CALL CLEARD ( DPRLEN, DATREC )
         CALL MOVED  ( ARRAY, NUMDP, DATREC )
 
         WRITE ( UNIT   = UNIT,
     .           REC    = RECNO,
     .           IOSTAT = IOSTAT ) ( DATREC(I), I = 1, DPRLEN )
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DAFWDR'                           )
            RETURN
         END IF
 
C
C     Handle the non-native case.
C
      ELSE
 
C
C        Clear RECORD.
C
         RECORD = ' '
 
C
C        Populate RECORD.
C
         CALL T_XLTFWD ( ARRAY, NUMDP, OUTBFF,
     .                   RECORD(1:NUMDP*INTLEN*2) )
 
C
C        Dump the record to the file.
C
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
