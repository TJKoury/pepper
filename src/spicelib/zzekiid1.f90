!$Procedure ZZEKIID1 ( EK, insert into index, d.p., type 1 )

      subroutine zzekiid1 ( handle, segdsc, coldsc, dkey, recptr, null )

! $ Abstract
!
!     Insert into a type 1 EK index a record pointer associated with a
!     d.p. key.  The key and record pointer determine the insertion
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


      include 'ekbool.inc'
      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               segdsc ( sdscsz )
      integer               coldsc ( cdscsz )
      double precision      dkey
      integer               recptr
      logical               null

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
      logical               failed

!
!     Local variables
!
      character*(cnamsz)    column

      integer               dtype
      integer               itype
      integer               prvidx
      integer               prvptr
      integer               tree

      logical               indexd

!
!     Use discovery check-in.
!
      if ( failed() ) then
         return
      end if


!
!     If the column's not indexed, we have no business being here.
!
      indexd  =  coldsc(ixtidx) .ne. ifalse

      if ( .not. indexd ) then

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKIID1'                 )
         call setmsg ( 'Column # is not indexed.' )
         call errch  ( '#',  column               )
         call sigerr ( 'SPICE(NOTINDEXED)'        )
         call chkout ( 'ZZEKIID1'                 )
         return

      end if


!
!     Check the column's data type.
!
      dtype  =  coldsc(typidx)

      if (  ( dtype .ne. dp ) .and. ( dtype .ne. time )  )  then

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKIID1'                                      )
         call setmsg ( 'Column # should be DP or TIME but has type #.' )
         call errch  ( '#',  column                                    )
         call errint ( '#',  dtype                                     )
         call sigerr ( 'SPICE(INVALIDTYPE)'                            )
         call chkout ( 'ZZEKIID1'                                      )
         return

      end if


      itype  =  coldsc(ixtidx)


      if ( itype .eq. 1 ) then
!
!        Get the tree pointer from the column descriptor.
!
         tree  =  coldsc(ixpidx)

!
!        Locate the predecessor of the input key, record pointer pair.
!
         call zzeklerd (  handle,  segdsc,  coldsc,  dkey, &
                          recptr,  null,    prvidx,  prvptr  )

!
!        Insert the new record pointer right after the item we've found.
!
         call zzektrin ( handle, tree, prvidx+1, recptr )


      else

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKIID1'                   )
         call setmsg ( 'Column # has index type #.' )
         call errch  ( '#',  column                 )
         call errint ( '#',  itype                  )
         call sigerr ( 'SPICE(INVALIDTYPE)'         )
         call chkout ( 'ZZEKIID1'                   )
         return

      end if

      return
      end
