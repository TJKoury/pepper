!$Procedure T_DAFWSR ( Write a DAF summary/descriptor record to file )

      SUBROUTINE T_DAFWSR ( UNIT, RECNO, OUTBFF, ND,   NI, &
                            NEXT, PREV,  NSUM,   ARRAY     )

! $ Abstract
!
!     Write a descriptor record to a DAF.
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
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               UNIT
      INTEGER               RECNO
      INTEGER               OUTBFF
      INTEGER               ND
      INTEGER               NI
      INTEGER               NEXT
      INTEGER               PREV
      INTEGER               NSUM
      DOUBLE PRECISION      ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Unit attached to DAF to receive new file record.
!     RECNO      I   Record number.
!     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
!     ND         I   Number of double precision components in summaries.
!     NI         I   Number of integer components in summaries.
!     NEXT       I   Record number of the next summary record.
!     PREV       I   Record number of the previous summary record.
!     NSUM       I   Number of summaries in array.
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
!                as defined in the include file 'zzddhman.inc'.
!
!     ND,
!     NI         are the numbers of double precision and integer
!                components, respectively, in each array summary
!                in the specified file.
!
!     NEXT       is the record number of the next summary record in
!                the DAF, or zero if this is the last summary record.
!
!     PREV       is the record number of the previous summary record
!                in the DAF, or zero if this is the first summary
!                record.
!
!     NSUM       is the number of summary records contained in ARRAY.
!
!     ARRAY      is a double precision array of packed summaries to
!                be written to the DAF.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     This routine will update the contents of the record RECNO
!     in the file associated with UNIT.
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
!     2) NSUM, ND, NI and the contents of ARRAY are all consistent.
!        This means that NSUM*(ND+(NI+1)/2) is precisely the number
!        of entries in ARRAY to be written to the DAF.
!
!     3) NEXT and PREV point to actual summary records in the DAF,
!        or are 0.
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
!-    SPICELIB Version 1.1.0, 28-FEB-2014 (BVS)
!
!        Added an initial clearing of the output DP record to make sure
!        that an initialized garbage at its end does not cause any
!        numeric I/O problems when it's written to the file.
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

      INTEGER               INRLEN
      PARAMETER           ( INRLEN = 2*DPRLEN )

      INTEGER               INTLEN
      PARAMETER           ( INTLEN = 4 )

!
!     Local Variables
!
      CHARACTER*(RECLEN)    RECORD
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      DOUBLE PRECISION      DPREC  ( DPRLEN )
      DOUBLE PRECISION      DPBUF  ( DPRLEN )

      INTEGER               INBUF  ( INRLEN )
      INTEGER               NUMINT
      INTEGER               I
      INTEGER               CINDEX
      INTEGER               DINDEX
      INTEGER               NATBFF
      INTEGER               IOSTAT

      LOGICAL               FIRST

!
!     Equivalence DPBUF to INBUF to handle unpacking.
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
         CALL CHKIN ( 'T_DAFWSR' )
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
            CALL CHKOUT ( 'T_DAFWSR'                                   )
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
         CALL CHKOUT ( 'T_DAFWSR'                                   )
         RETURN

      END IF

!
!     First, determine if we are to write to the native file format.
!
      IF ( OUTBFF .EQ. NATBFF ) THEN

!
!        Clear DPREC.
!
         CALL CLEARD ( DPRLEN, DPREC )

         DPREC(1) = DBLE(NEXT)
         DPREC(2) = DBLE(PREV)
         DPREC(3) = DBLE(NSUM)

         CALL MOVED ( ARRAY, NSUM*(ND+(NI+1)/2), DPREC(4) )

         WRITE ( UNIT   = UNIT, &
                 REC    = RECNO, &
                 IOSTAT = IOSTAT ) ( DPREC(I), I = 1, DPRLEN )

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DAFWSR'                           )
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
         DPREC(1) = DBLE(NEXT)
         DPREC(2) = DBLE(PREV)
         DPREC(3) = DBLE(NSUM)

         CALL T_XLTFWD ( DPREC, 3, OUTBFF, RECORD(1:2*INTLEN*3) )

!
!        Set the index into RECORD to start placing summaries.
!
         CINDEX = 2*INTLEN*3+1

!
!        Now process the summaries.
!
         DO I = 1, NSUM

!
!           Set the starting index into ARRAY for the next summary.
!
            DINDEX = (I-1)*(ND + (NI+1)/2) + 1

!
!           Convert the DPs.
!
            CALL T_XLTFWD ( ARRAY(DINDEX), ND, OUTBFF, &
                            RECORD(CINDEX:CINDEX+ND*(2*INTLEN)-1) )

!
!           Increment CINDEX and DINDEX to the position where the
!           integers will be placed and located.
!
            DINDEX = DINDEX + ND
            CINDEX = CINDEX + ND*(2*INTLEN)

!
!           Unpack the integer components.
!
            CALL MOVED ( ARRAY(DINDEX), (NI+1)/2, DPBUF )

!
!           Translate the integers. We are translating 2*((NI+1)/2)
!           integers, which is not always NI integers.  Rather than
!           introduce special exception for the two cases (NI odd and
!           NI even, we will just translate the garbage in the last
!           integer in the odd case.
!
            NUMINT = 2*((NI+1)/2)
            CALL T_XLTFWI ( INBUF, NUMINT, OUTBFF, &
                            RECORD(CINDEX:CINDEX+(NUMINT*INTLEN)-1) )

!
!           Increment CINDEX.  We ignore DINDEX because it will be
!           set properly at the start of the loop.
!
            CINDEX = CINDEX + (NUMINT*INTLEN)

         END DO

!
!        Dump the record to the file.
!
         WRITE ( UNIT = UNIT, REC = RECNO, IOSTAT = IOSTAT ) RECORD

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DAFWSR'                           )
            RETURN
         END IF

      END IF

      CALL CHKOUT ( 'T_DAFWSR' )
      RETURN

      END
