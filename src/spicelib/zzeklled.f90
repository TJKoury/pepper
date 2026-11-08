!$Procedure ZZEKLLED ( EK, last less than or equal to, d.p. )

      SUBROUTINE ZZEKLLED (  HANDLE,  SEGDSC,  COLDSC, &
                             DKEY,    PRVLOC,  PRVPTR  )

! $ Abstract
!
!     Find the last column value less than or equal to a specified key,
!     for a specified, indexed double precision EK column.
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
      DOUBLE PRECISION      DKEY
      INTEGER               PRVLOC
      INTEGER               PRVPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     DKEY       I   Double precision key.
!     PRVLOC     O   Ordinal position of predecessor of DKEY.
!     PRVPTR     O   Record pointer for predecessor of DKEY.
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
!     DKEY           is an double precision string key.  The last column
!                    entry less than this key is sought.
!
! $ Detailed_Output
!
!     PRVLOC         is the ordinal position, according to the order
!                    relation implied by the column's index, of the
!                    record containing the last element less than or
!                    equal to DKEY.  If the column contains elements
!                    equal to DKEY, PRVLOC is the index of the last
!                    such element.
!
!                    If all elements of the column are greater than
!                    DKEY, PRVLOC is set to zero.
!
!     PRVPTR         is a pointer to the record containing the element
!                    whose ordinal position is PRVLOC.
!
!                    If all elements of the column are greater than
!                    DKEY, PRVPTR is set to zero.
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
!     This routine finds the last column element less than or equal
!     to a specified double precision key, within a specified segment
!     and column.
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
!     See ZZEKILLE.
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
!     If the column's not indexed, we have no business being here.
!
      INDEXD  =  COLDSC(IXTIDX) .NE. IFALSE

      IF ( .NOT. INDEXD ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKLLED'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKLLED'                 )
         RETURN

      END IF


!
!     Check the column's data type.
!
      DTYPE  =  COLDSC(TYPIDX)

      IF (  ( DTYPE .NE. DP ) .AND. ( DTYPE .NE. TIME )  ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKLLED'                                      )
         CALL SETMSG ( 'Column # should be DP or TIME but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                            )
         CALL CHKOUT ( 'ZZEKLLED'                                      )
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
!     The algorithm used here is very like unto that used in LSTLED.
!
      BEGIN   =   1
      END     =   NROWS

!
!     Get the record pointers BEGPTR and ENDPTR of the least and
!     greatest elements in the column.
!
      CALL ZZEKIXLK ( HANDLE, COLDSC, BEGIN, BEGPTR )
      CALL ZZEKIXLK ( HANDLE, COLDSC, END,   ENDPTR )


      IF  (  ZZEKSCMP ( GT,      HANDLE,  SEGDSC, &
                        COLDSC,  BEGPTR,  1,       DP, &
                        ' ',     DKEY,    0,      .FALSE.  )  ) THEN
!
!        The smallest entry of the column is greater than
!        the input value, so none of the entries
!        are less than or equal to the input value.
!
         PRVLOC = 0
         PRVPTR = 0


      ELSE IF (  ZZEKSCMP ( LE,      HANDLE,  SEGDSC, &
                            COLDSC,  ENDPTR,  1,       DP, &
                            ' ',     DKEY,    0,      .FALSE. )  ) THEN
!
!        The last element of the array is less than or equal to the
!        input value.
!
         PRVLOC = NROWS

         CALL ZZEKIXLK ( HANDLE, COLDSC, PRVLOC, PRVPTR )


      ELSE
!
!        The input value lies between some pair of column entries.
!        The value is greater than or equal to the smallest column entry
!        and less than the greatest entry.
!
         DO WHILE ( END .GT. BEGIN+1 )
!
!           Find the address of the element whose ordinal position
!           is halfway between BEGIN and END.
!
            MIDDLE   =  ( BEGIN  + END )  / 2

            CALL ZZEKIXLK ( HANDLE, COLDSC, MIDDLE, MIDPTR )


            IF (  ZZEKSCMP ( LE,      HANDLE,  SEGDSC, &
                             COLDSC,  MIDPTR,  1,      DP, &
                             ' ',     DKEY,    0,     .FALSE. )  ) THEN
!
!              The middle value is less than or equal to the input
!              value.
!
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF
!
!           The input value is greater than or equal to the element
!           having ordinal position BEGIN and strictly less than the
!           element having ordinal position END.
!
         END DO

         PRVLOC = BEGIN
         CALL ZZEKIXLK ( HANDLE, COLDSC, PRVLOC, PRVPTR )

      END IF


      RETURN
      END
