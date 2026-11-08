!$Procedure ZZEKLERI ( EK, LLE, using record pointers, integer )

      SUBROUTINE ZZEKLERI (  HANDLE,  SEGDSC,  COLDSC,  IKEY, &
                             RECPTR,  NULL,    PRVIDX,  PRVPTR  )

! $ Abstract
!
!     Find the last column value less than or equal to a specified key,
!     for a specified, indexed integer EK column, using dictionary
!     ordering on integer data values and record pointers.
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
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               IKEY
      INTEGER               RECPTR
      LOGICAL               NULL
      INTEGER               PRVIDX
      INTEGER               PRVPTR

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
!     PRVIDX     O   Ordinal position of predecessor of IKEY.
!     PRVPTR     O   Pointer to record containing predecessor of IKEY.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column specified by COLDSC.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     IKEY,
!     RECPTR         are, respectively, an integer key and a
!                    pointer to the EK record containing that key.
!                    The last column entry less than or equal to
!                    this key is sought.  The order relation used
!                    is dictionary ordering on the pair (IKEY, RECPTR).
!
!     NULL           is a logical flag indicating whether the input
!                    key is null.  When NULL is .TRUE., IKEY is
!                    ignored by this routine.
!
! $ Detailed_Output
!
!     PRVIDX         is the ordinal position, according to the order
!                    relation implied by the column's index, of the
!                    record containing the last element less than or
!                    equal to IKEY, where the order relation is
!                    as indicated above.  If the column contains
!                    elements equal to IKEY, PRVIDX is the index of the
!                    last such element.
!
!                    If all elements of the column are greater than
!                    IKEY, PRVIDX is set to zero.
!
!     PRVPTR         is a pointer to the record containing the element
!                    whose ordinal position is PRVIDX.
!
!                    If all elements of the column are greater than
!                    IKEY, PRVPTR is set to zero.
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
!     This routine finds the last column element less than or equal
!     to a specified integer key, within a specified segment and
!     column.  The order relation used is dictionary ordering on the
!     pair (IKEY, RECPTR).
!
!     In order to support the capability of creating an index for a
!     column that has already been populated with data, this routine
!     does not require that the number of elements referenced by the
!     input column's index match the pointer of elements in the column;
!     the index is allowed to reference fewer elements.  However,
!     every record referenced by the index must be populated with data.
!
! $ Examples
!
!     See ZZEKIII1.
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
!-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Removed an unbalanced call to CHKOUT
!
!-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
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

         CALL CHKIN  ( 'ZZEKLERI'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKLERI'                 )
         RETURN

      END IF

!
!     Check the column's data type.
!
      DTYPE  =  COLDSC(TYPIDX)

      IF ( DTYPE .NE. INT ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKLERI'                               )
         CALL SETMSG ( 'Column # should be INT but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                             )
         CALL ERRINT ( '#',  DTYPE                              )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                     )
         CALL CHKOUT ( 'ZZEKLERI'                               )
         RETURN

      END IF

!
!     Hand the problem off to the subroutine that understands this
!     column's index type.
!
      ITYPE  =  COLDSC(IXTIDX)

      IF ( ITYPE .EQ. 1 ) THEN

         CALL ZZEKERI1 (  HANDLE,  SEGDSC,  COLDSC,  IKEY, &
                          RECPTR,  NULL,    PRVIDX,  PRVPTR  )

      ELSE

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKLERI'                   )
         CALL SETMSG ( 'Column # has index type #.' )
         CALL ERRCH  ( '#',  COLUMN                 )
         CALL ERRINT ( '#',  ITYPE                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'         )
         CALL CHKOUT ( 'ZZEKLERI'                   )
         RETURN

      END IF

      RETURN
      END
