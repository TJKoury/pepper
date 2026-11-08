!$Procedure ZZEKIID1 ( EK, insert into index, d.p., type 1 )

      SUBROUTINE ZZEKIID1 ( HANDLE, SEGDSC, COLDSC, DKEY, RECPTR, NULL )

! $ Abstract
!
!     Insert into a type 1 EK index a record pointer associated with a
!     d.p. key.  The key and record pointer determine the insertion
!     point via dictionary ordering on (value, record pointer) pairs.
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
      INTEGER               RECPTR
      LOGICAL               NULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     DKEY       I   Double precision key.
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
!     DKEY           is a double precision key.
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
!     2)  If the data type of the input column is not double precision,
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
!     associated with a double precision, scalar column.  The type of
!     the double precision index must be 1.
!
!     The ordinal position of the new item is determined by the key
!     DKEY.  The new item will follow the last item already present
!     in the column having a value less than or equal to DKEY.
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
!     See ZZEKAD02.
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
!        Removed an unbalanced call to CHKOUT.
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

         CALL CHKIN  ( 'ZZEKIID1'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKIID1'                 )
         RETURN

      END IF


!
!     Check the column's data type.
!
      DTYPE  =  COLDSC(TYPIDX)

      IF (  ( DTYPE .NE. DP ) .AND. ( DTYPE .NE. TIME )  )  THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKIID1'                                      )
         CALL SETMSG ( 'Column # should be DP or TIME but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                            )
         CALL CHKOUT ( 'ZZEKIID1'                                      )
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
         CALL ZZEKLERD (  HANDLE,  SEGDSC,  COLDSC,  DKEY, &
                          RECPTR,  NULL,    PRVIDX,  PRVPTR  )

!
!        Insert the new record pointer right after the item we've found.
!
         CALL ZZEKTRIN ( HANDLE, TREE, PRVIDX+1, RECPTR )


      ELSE

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKIID1'                   )
         CALL SETMSG ( 'Column # has index type #.' )
         CALL ERRCH  ( '#',  COLUMN                 )
         CALL ERRINT ( '#',  ITYPE                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'         )
         CALL CHKOUT ( 'ZZEKIID1'                   )
         RETURN

      END IF

      RETURN
      END
