!$Procedure ZZEKERC1 ( EK, LLE using record pointers, char, type 1 )

      SUBROUTINE ZZEKERC1 (  HANDLE,  SEGDSC,  COLDSC,  CKEY, &
                             RECPTR,  NULL,    PRVIDX,  PRVPTR  )

! $ Abstract
!
!     Find the last column value less than or equal to a specified key,
!     for a specified character EK column having a type 1 index, using
!     dictionary ordering on character data values and record pointers.
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


      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      CHARACTER*(*)         CKEY
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
!     CKEY       I   Character key.
!     RECPTR     I   Record pointer.
!     NULL       I   Null flag.
!     PRVIDX     O   Ordinal position of predecessor of CKEY.
!     PRVPTR     O   Record pointer for predecessor of CKEY.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column specified by COLDSC.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     CKEY,
!     RECPTR         are, respectively, a character key and a pointer
!                    to an EK record containing that key.  The last
!                    column entry less than or equal to this key is
!                    sought.  The order relation used is dictionary
!                    ordering on the pair (CKEY, RECPTR).
!
!     NULL           is a logical flag indicating whether the input
!                    key is null.  When NULL is .TRUE., CKEY is
!                    ignored by this routine.
!
! $ Detailed_Output
!
!     PRVIDX         is the ordinal position, according to the order
!                    relation implied by the column's index, of the
!                    record containing the last element less than or
!                    equal to CKEY, where the order relation is
!                    as indicated above.  If the column contains
!                    elements equal to CKEY, PRVIDX is the index of the
!                    record designated by the input RECPTR.
!
!                    If all elements of the column are greater than
!                    CKEY, PRVIDX is set to zero.
!
!     PRVPTR         is a pointer to the record containing the element
!                    whose ordinal position is PRVIDX.
!
!                    If all elements of the column are greater than
!                    CKEY, PRVPTR is set to zero.
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
!     3)  If the tree is empty, PRVIDX and PRVPTR are set to zero.
!         This case is not considered an error.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine finds the last column element less than or equal
!     to a specified character key, within a specified segment and
!     column.  The column must be indexed by a type 1 index.  The order
!     relation used is dictionary ordering on ordered pairs consisting
!     of data values and record pointers:  if the data values in two
!     column entries are equal, the associated record pointers determine
!     the order relation of the column entries.
!
!     Type 1 indexes are implemented as DAS B*-trees.  The data
!     pointers of an index tree contain record pointers.  Therefore, the
!     tree implements an abstract order vector.
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
!     See ZZEKLERC.
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
!        Corrected typo in comments.
!
!-    Beta Version 1.1.0, 18-MAY-1997 (NJB)
!
!        Errors in comparisons of items of equal value were fixed.
!        In such cases, items are compared according to order of
!        their record pointers.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRSZ

      LOGICAL               ZZEKSCMP

!
!     Local variables
!
      INTEGER               BEGIN
      INTEGER               BEGPTR
      INTEGER               END
      INTEGER               ENDPTR
      INTEGER               MIDDLE
      INTEGER               MIDPTR
      INTEGER               NREC
      INTEGER               TREE
      INTEGER               TSIZE

      LOGICAL               LEQ

!
!     Use discovery check-in.
!
      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Make sure the number of records in the segment is at least as
!     large as the number of entries in the index:  we must not look
!     up any entries that don't exist!
!
      TREE   =  COLDSC   ( IXPIDX )
      TSIZE  =  ZZEKTRSZ ( HANDLE, TREE )
      NREC   =  SEGDSC   ( NRIDX  )

      IF ( TSIZE .GT. NREC ) THEN

         CALL CHKIN  ( 'ZZEKERC1'                                      )
         CALL SETMSG ( 'Index size = # but column contains # records.' )
         CALL ERRINT ( '#',  TSIZE                                     )
         CALL ERRINT ( '#',  NREC                                      )
         CALL SIGERR ( 'SPICE(SIZEMISMATCH)'                           )
         CALL CHKOUT ( 'ZZEKERC1'                                      )
         RETURN

      END IF

!
!     Handle the case of an empty tree gracefully.
!
      IF ( TSIZE .EQ. 0 ) THEN

         PRVIDX = 0
         PRVPTR = 0

         RETURN

      END IF

!
!     The algorithm used here is very like unto that used in LSTLED.
!
      BEGIN   =   1
      END     =   TSIZE

!
!     Get the record pointers BEGPTR and ENDPTR of the least and
!     greatest elements in the column.
!
      CALL ZZEKTRDP ( HANDLE, TREE, BEGIN, BEGPTR )
      CALL ZZEKTRDP ( HANDLE, TREE, END,   ENDPTR )

!
!     Compare the input value to the smallest value in the column.
!
      IF  (  ZZEKSCMP ( GT,      HANDLE,  SEGDSC,  COLDSC, &
                        BEGPTR,  1,       CHR,     CKEY, &
                        0.D0,    0,       NULL            )  ) THEN
!
!        The smallest entry of the column is greater than
!        the input value, so none of the entries
!        are less than or equal to the input value.
!
         PRVIDX = 0
         PRVPTR = 0

         RETURN

      ELSE IF  (  ZZEKSCMP ( EQ,      HANDLE,  SEGDSC,  COLDSC, &
                             BEGPTR,  1,       CHR,     CKEY, &
                             0.D0,    0,       NULL            ) &

                .AND.      (  RECPTR  .LT.  BEGPTR )             ) THEN
!
!        The smallest entry of the column is greater than the input
!        value, based on a comparison of record pointers, so none of the
!        entries are less than or equal to the input value.
!
         PRVIDX = 0
         PRVPTR = 0

         RETURN

      END IF

!
!     At this point, we know the input value is greater than or equal
!     to the smallest element of the column.
!
!     Compare the input value to the greatest value in the column.
!
      IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC,   COLDSC, &
                       ENDPTR,  1,       CHR,      CKEY, &
                       0.D0,    0,       NULL             )  ) THEN
!
!        The last element of the column is less than the
!        input value.
!
         PRVIDX  =  TSIZE

         CALL ZZEKTRDP ( HANDLE, TREE, PRVIDX, PRVPTR )

         RETURN


      ELSE IF (  ZZEKSCMP ( EQ,      HANDLE,  SEGDSC,  COLDSC, &
                            ENDPTR,  1,       CHR,     CKEY, &
                            0.D0,    0,       NULL             ) &

               .AND.      (  ENDPTR  .LE.  RECPTR )       ) THEN
!
!        The last element of the column is less than or equal to the
!        input value, based on a comparison of record pointers.
!
         PRVIDX  =  TSIZE
         PRVPTR  =  ENDPTR

         RETURN

      END IF

!
!     The input value lies between some pair of column entries.
!     The value is greater than or equal to the smallest column entry
!     and less than the greatest entry, according to the dictionary
!     ordering we're using.
!
!     Below, we'll use the variable LEQ to indicate whether the "middle"
!     element in our search is less than or equal to the input value.
!
      DO WHILE ( END .GT. BEGIN+1 )
!
!        Find the record pointer of the element whose ordinal position
!        is halfway between BEGIN and END.
!
         MIDDLE   =  ( BEGIN  + END )  / 2

         CALL ZZEKTRDP ( HANDLE, TREE, MIDDLE, MIDPTR )

!
!        Determine the order relation between CKEY and the column
!        entry at record MIDPTR.
!
         IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC,  COLDSC, &
                          MIDPTR,  1,       CHR,     CKEY, &
                          0.D0,    0,       NULL            )  ) THEN
!
!           The column element at record MIDPTR is less than
!           or equal to CKEY, based on data values.
!
            LEQ  =  .TRUE.


         ELSE IF ( ZZEKSCMP ( EQ,      HANDLE,  SEGDSC,  COLDSC, &
                              MIDPTR,  1,       CHR,     CKEY, &
                              0.D0,    0,       NULL           ) ) THEN
!
!           The column entry's value matches CKEY.  We must
!           compare record pointers at this point.
!
            LEQ  =  MIDPTR .LE. RECPTR

         ELSE
!
!           The inequality of data values is strict.
!
            LEQ  = .FALSE.

         END IF


         IF ( LEQ ) THEN
!
!           The middle value is less than or equal to the input
!           value.
!
            BEGIN = MIDDLE
         ELSE
            END   = MIDDLE
         END IF
!
!        The input value is greater than or equal to the element
!        having ordinal position BEGIN and strictly less than the
!        element having ordinal position END.
!
      END DO

      PRVIDX = BEGIN
      CALL ZZEKTRDP ( HANDLE, TREE, PRVIDX, PRVPTR )

      RETURN
      END
