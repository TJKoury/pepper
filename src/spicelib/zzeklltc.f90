!$Procedure ZZEKLLTC ( EK, last less than, character )

      SUBROUTINE ZZEKLLTC (  HANDLE,  SEGDSC,  COLDSC, &
                             CKEY,    PRVLOC,  PRVPTR  )

! $ Abstract
!
!     Find the last column value less than a specified key,
!     for a specified, indexed character EK column.
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
      CHARACTER*(*)         CKEY
      INTEGER               PRVLOC
      INTEGER               PRVPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     CKEY       I   Character key.
!     PRVLOC     O   Ordinal position of predecessor of CKEY.
!     PRVPTR     O   Pointer to record containing predecessor of CKEY.
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
!     CKEY           is a character string key.  The last column entry
!                    less than this key is sought.
!
! $ Detailed_Output
!
!     PRVLOC         is the ordinal position, according to the order
!                    relation implied by the column's index, of the
!                    record containing the last element less than CKEY.
!
!                    If all elements of the column are greater than
!                    or equal to CKEY, PRVLOC is set to zero.
!
!     PRVPTR         is a pointer to the record containing the element
!                    whose ordinal position is PRVLOC.
!
!                    If all elements of the column are greater than
!                    or equal to CKEY, PRVPTR is set to zero.
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
!     2)  If the data type of the input column is not character,
!         the error SPICE(INVALIDTYPE) is signaled.
!
!     3)  If the input column is not indexed, the error
!         SPICE(NOTINDEXED) is signaled.
!
!     4)  If the index type of the input column is not recognized,
!         the error will be diagnosed by routines called by this
!         routine.
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
!     This routine finds the last column element less than a specified
!     character key, within a specified segment and column.
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
!     See ZZEKILLT.
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
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


!
!     Non-SPICELIB functions
!
      LOGICAL               ZZEKSCMP

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               BEGIN
      INTEGER               BEGPTR
      INTEGER               DTYPE
      INTEGER               END
      INTEGER               ENDPTR
      INTEGER               MIDDLE
      INTEGER               MIDPTR
      INTEGER               NROWS

      LOGICAL               INDEXD

!
!     Use discovery check-in.
!
!
!     If the column's not indexed, we have no business being here.
!
      INDEXD  =  COLDSC(IXTIDX) .NE. IFALSE

      IF ( .NOT. INDEXD ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKLLTC'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKLLTC'                 )
         RETURN

      END IF


!
!     Check the column's data type.
!
      DTYPE  =  COLDSC(TYPIDX)

      IF ( DTYPE .NE. CHR ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKLLTC'                               )
         CALL SETMSG ( 'Column # should be CHR but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                             )
         CALL ERRINT ( '#',  DTYPE                              )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                     )
         CALL CHKOUT ( 'ZZEKLLTC'                               )
         RETURN

      END IF

!
!     Handle the case of an empty segment gracefully.
!
      NROWS  =  SEGDSC(NRIDX)

      IF ( NROWS .EQ. 0 ) THEN

         PRVLOC = 0
         PRVPTR = 0

         RETURN

      END IF

!
!     The algorithm used here is very like unto that used in LSTLTD.
!
      BEGIN   =   1
      END     =   NROWS

!
!     Get the record pointers BEGPTR and ENDPTR of the least and
!     greatest elements in the column.
!
      CALL ZZEKIXLK ( HANDLE, COLDSC, BEGIN, BEGPTR )
      CALL ZZEKIXLK ( HANDLE, COLDSC, END,   ENDPTR )


      IF  (  ZZEKSCMP ( GE,      HANDLE,  SEGDSC, &
                        COLDSC,  BEGPTR,  1,       CHR, &
                        CKEY,    0.D0,    0,      .FALSE.  )  ) THEN
!
!        The smallest entry of the column is greater than or equal to
!        the input value of the same data type, so none of the entries
!        are less than the input value.
!
         PRVLOC = 0
         PRVPTR = 0


      ELSE IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC, &
                            COLDSC,  ENDPTR,  1,       CHR, &
                            CKEY,    0.D0,    0,      .FALSE. )  ) THEN
!
!        The last element of the array is less than the input value of
!        the same data type, so it's the last item less than the input
!        value.
!
         PRVLOC = NROWS

         CALL ZZEKIXLK ( HANDLE, COLDSC, PRVLOC, PRVPTR )


      ELSE
!
!        The input value lies between some pair of column entries.
!        The value is greater than the smallest column entry and
!        less than or equal to the greatest entry.
!
         DO WHILE ( END .GT. BEGIN+1 )
!
!           Find the address of the element whose ordinal position
!           is halfway between BEGIN and END.
!
            MIDDLE   =  ( BEGIN  + END )  / 2

            CALL ZZEKIXLK ( HANDLE, COLDSC, MIDDLE, MIDPTR )


            IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC, &
                             COLDSC,  MIDPTR,  1,      CHR, &
                             CKEY,    0.D0,    0,     .FALSE. )  ) THEN
!
!              The middle value is less than the input value of the
!              same data type.
!
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF
!
!           The input value is greater than the element having ordinal
!           position BEGIN and less than or equal to the element having
!           ordinal position END.
!
         END DO

         PRVLOC = BEGIN
         CALL ZZEKIXLK ( HANDLE, COLDSC, PRVLOC, PRVPTR )

      END IF


      RETURN
      END
