!$Procedure     ZZEKIXDL ( EK, delete record from index )

      SUBROUTINE ZZEKIXDL ( HANDLE, SEGDSC, COLDSC, RECPTR )

! $ Abstract
!
!     Update an EK column index to reflect deletion of a record
!     specified by a record pointer.
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


      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer to locate.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of an EK file open for write access.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column to be searched.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     RECPTR         is a pointer to a record whose corresponding
!                    index entry is to be deleted.
!
! $ Detailed_Output
!
!     None.  This routine operates by side effects.  See $Particulars
!     for a discussion of the action of this routine.
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
!     2)  If an I/O error occurs while reading or writing the indicated
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
!     This routine supports EK update or delete operations, both of
!     which involve removing pointers to records from column indexes.
!
! $ Examples
!
!     See ZZEKDE01.
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
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      INTEGER               IDX
      INTEGER               IDXTYP
      INTEGER               RECNO
      INTEGER               TREE

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKIXDL' )


      IDXTYP  =  COLDSC ( IXTIDX )

      IF ( IDXTYP .NE. IFALSE ) THEN
!
!        This column is indexed.
!
!        Some entry in the index points to RECPTR.  Find the entry
!        and delete it.
!
         CALL ZZEKFRX ( HANDLE, SEGDSC, COLDSC, RECPTR, IDX )


         IF ( IDXTYP .EQ. 1 ) THEN
!
!           For type 1 indexes, the index pointer is the root node of
!           a B*-tree.  Just use the tree deletion routine.
!
            TREE  = COLDSC ( IXPIDX )

            CALL ZZEKTRDL ( HANDLE, TREE, IDX )

         ELSE
!
!           Sorry, no other types of indexes are supported.
!
            CALL SETMSG ( 'The index type # is not supported.' )
            CALL ERRINT ( '#',   IDXTYP                        )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                 )
            CALL CHKOUT ( 'ZZEKIXDL'                           )
            RETURN

         END IF


      ELSE
!
!        This routine should not have been called if the column in
!        question is not indexed.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL SETMSG ( 'Column was not indexed. File = #; ' // &
                       'RECNO = #; COLIDX = #.'              )
         CALL ERRHAN ( '#',  HANDLE                          )
         CALL ERRINT ( '#',  RECNO                           )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                  )
         CALL SIGERR ( 'SPICE(BUG)'                          )
         CALL CHKOUT ( 'ZZEKIXDL'                            )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZEKIXDL' )
      RETURN
      END
