!$Procedure ZZEKRPLK ( EK, look up record pointer )

      SUBROUTINE ZZEKRPLK ( HANDLE, SEGDSC, N, RECPTR )

! $ Abstract
!
!     Look up the record pointer of an EK record having a specified
!     ordinal position in a specified EK segment.
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

      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               N
      INTEGER               RECPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     N          I   Ordinal position of record.
!     RECPTR     O   Record pointer.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the descriptor of the segment to which the
!                    record of interest belongs.
!
!     N              is the ordinal position of the record in the
!                    segment.
!
! $ Detailed_Output
!
!     RECPTR         is the record pointer corresponding to the input
!                    key.  This pointer identifies the record of
!                    interest.  The interpretation of RECPTR depends
!                    on the type of segment to which the record belongs.
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
!     2)  If N is out of range, the error will be diagnosed by
!         routines called by this routine.
!
!     3)  If an I/O error occurs while reading or the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine finds the record pointer for a record having a
!     specified ordinal position in a segment.
!
! $ Examples
!
!     See EKSRCH.
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
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


!
!     Local variables
!
      INTEGER               STYPE
      INTEGER               TREE

!
!     Use discovery check-in.
!
      STYPE  =  SEGDSC ( EKTIDX )


      IF ( STYPE .EQ. 1 ) THEN
!
!        For type 1 segments, the record pointer is obtained from
!        the record tree.
!
         TREE  = SEGDSC ( RTIDX )

         CALL ZZEKTRDP ( HANDLE, TREE, N, RECPTR )


      ELSE IF ( STYPE .EQ. 2 ) THEN
!
!        For type 2 segments, the record pointer *is* the ordinal
!        position of the record.
!
         RECPTR = N

      ELSE
!
!        Sorry, no other types of segments are supported.
!
         CALL CHKIN  ( 'ZZEKRPLK'                             )
         CALL SETMSG ( 'The segment type # is not supported.' )
         CALL ERRINT ( '#',   STYPE                           )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                   )
         CALL CHKOUT ( 'ZZEKRPLK'                             )
         RETURN

      END IF


      END
