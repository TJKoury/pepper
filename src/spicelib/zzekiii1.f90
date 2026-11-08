!$Procedure ZZEKIII1 ( EK, insert into index, integer, type 1 )

      SUBROUTINE ZZEKIII1 ( HANDLE, SEGDSC, COLDSC, IKEY, RECPTR, NULL )

! $ Abstract
!
!     Insert into a type 1 EK index a record pointer associated with an
!     integer key.  The key and record pointer determine the insertion
!     point via dictionary ordering on (value, record pointer) pairs.
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
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               IKEY
      INTEGER               RECPTR
      LOGICAL               NULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     IKEY       I   Integer key.
!     RECPTR     I   Record pointer.
!     NULL       I   Null flag.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column specified by COLDSC.
!
!     COLDSC         is the column descriptor of the column to
!                    which the index corresponds.
!
!     IKEY           is an integer key.
!
!     RECPTR         is a record pointer associated with the input key.
!
!     NULL           is a logical flag indicating whether the input
!                    value is null.
!
! $ Detailed_Output
!
!     None.  This routine operates by side effects.  See $Particulars
!     for a description of the effect of this routine.
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
!     2)  If the data type of the input column is not integer,
!         the error SPICE(INVALIDTYPE) is signaled.
!
!     3)  If the input column is not indexed, the error
!         SPICE(NOTINDEXED) is signaled.
!
!     4)  If the index type of the input column is not recognized,
!         the error SPICE(INVALIDTYPE) is signaled.
!
!     5)  If an I/O error occurs while reading or writing the indicated
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
!     This routine updates the index of an EK segment to reflect the
!     addition of a record to the segment.  The index must be
!     associated with an integer, scalar column.  The type of the
!     index must be 1.
!
!     The ordinal position of the new item is determined by the key
!     IKEY.  The new item will follow the last item already present
!     in the column having a value less than or equal to IKEY.
!
!     In order to support the capability of creating an index for a
!     column that has already been populated with data, this routine
!     does not require that number of elements referenced by the
!     input column's index match the number of elements in the column;
!     the index is allowed to reference fewer elements.  However,
!     every record referenced by the index must be populated with data.
!
! $ Examples
!
!     See ZZEKAD01.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               DTYPE
      INTEGER               ITYPE
      INTEGER               PRVIDX
      INTEGER               PRVPTR
      INTEGER               TREE

      LOGICAL               INDEXD

!
!     Use discovery check-in.
!
      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     If the column's not indexed, we have no business being here.
!
      INDEXD  =  COLDSC(IXTIDX) .NE. IFALSE

      IF ( .NOT. INDEXD ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKIII1'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKIII1'                 )
         RETURN

      END IF

!
!     Check the column's data type.
!
      DTYPE  =  COLDSC(TYPIDX)

      IF ( DTYPE .NE. INT ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKIII1'                               )
         CALL SETMSG ( 'Column # should be INT but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                             )
         CALL ERRINT ( '#',  DTYPE                              )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                     )
         CALL CHKOUT ( 'ZZEKIII1'                               )
         RETURN

      END IF


      ITYPE  =  COLDSC(IXTIDX)


      IF ( ITYPE .EQ. 1 ) THEN
!
!        Get the tree pointer from the column descriptor.
!
         TREE  =  COLDSC(IXPIDX)

!
!        Locate the predecessor of the input key, record pointer pair.
!
         CALL ZZEKLERI (  HANDLE,  SEGDSC,  COLDSC,  IKEY, &
                          RECPTR,  NULL,    PRVIDX,  PRVPTR  )

!
!        Insert the new record pointer right after the item we've found.
!
         CALL ZZEKTRIN ( HANDLE, TREE, PRVIDX+1, RECPTR )


      ELSE

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKIII1'                   )
         CALL SETMSG ( 'Column # has index type #.' )
         CALL ERRCH  ( '#',  COLUMN                 )
         CALL ERRINT ( '#',  ITYPE                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'         )
         CALL CHKOUT ( 'ZZEKIII1'                   )
         RETURN

      END IF

      RETURN
      END
