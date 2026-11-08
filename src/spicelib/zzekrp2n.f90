!$Procedure      ZZEKRP2N ( EK, record pointer to number )

      INTEGER FUNCTION ZZEKRP2N ( HANDLE, SEGNO, RECPTR )

! $ Abstract
!
!     Find the EK record number corresponding to a specified record
!     pointer.  Beware, for type 1 segments, this is done by linear
!     searching.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'eksegdsc.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RECTPR     I   Record pointer.
!
!     The function returns the number of the record having the
!     specified record pointer.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     SEGNO          is the number of the segment containing the
!                    record of interest.
!
!     RECPTR         is a record pointer.  The number of the record
!                    having this pointer is sought.
!
! $ Detailed_Output
!
!     The function returns the number of the record having the
!     specified record pointer.  The record should always be found.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If SEGNO is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     3)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
!     4)  This routine should never be passed an input record pointer
!         that is not known to be valid.  If this error is trapped,
!         it is evidence of a bug.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This searches an EK record tree for the record number
!     corresponding to a specified record pointer.  Caution:  this
!     routine plods along in linear time.  It is intended primarily
!     for use in error handling.
!
! $ Examples
!
!     See EKDELR.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRLS
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               STYPE

!
!     Use discovery check-in.
!
      ZZEKRP2N  =  0

      CALL ZZEKSDSC ( HANDLE, SEGNO, SEGDSC )

      IF ( FAILED() ) THEN
         RETURN
      END IF


      STYPE = SEGDSC ( EKTIDX )

      IF ( STYPE .EQ. 1 ) THEN

         ZZEKRP2N  =  ZZEKTRLS ( HANDLE, SEGDSC(RTIDX), RECPTR )

         IF ( ZZEKRP2N .EQ. 0 ) THEN

            CALL CHKIN  ( 'ZZEKRP2N'                              )
            CALL SETMSG ( 'Record having pointer # not found in ' // &
                          'segment # of file #'                   )
            CALL ERRINT ( '#',  RECPTR                            )
            CALL ERRINT ( '#',  SEGNO                             )
            CALL ERRHAN ( '#',  HANDLE                            )
            CALL SIGERR ( 'SPICE(BUG)'                            )
            CALL CHKOUT ( 'ZZEKRP2N'                              )

         END IF


      ELSE IF ( STYPE .EQ. 2 ) THEN

         ZZEKRP2N = RECPTR

      ELSE

         CALL CHKIN  ( 'ZZEKRP2N'                                    )
         CALL SETMSG ( 'Segment type # is not supported.  SEGNO = #.'// &
                       ' File = #.'                                  )
         CALL ERRINT ( '#',  STYPE                                   )
         CALL ERRINT ( '#',  SEGNO                                   )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'ZZEKRP2N'                                    )

      END IF

      RETURN
      END
