!$Procedure     ZZEKAD01 ( EK, add data to class 1 column )

      subroutine zzekad01 (  handle,  segdsc,  coldsc, &
                             recptr,  ival,    isnull  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add a column entry to a class 1 column in a specified EK record.
!     Class 1 columns contain scalar, integer values.
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
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( * )
      integer(ip)           coldsc ( * )
      integer(ip)           recptr
      integer(ip)           ival
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     IVAL       I   Integer value.
!     ISNULL     I   Null flag.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment in which
!                    the specified column entry is to be written.
!
!     COLDSC         is the descriptor of the column in which
!                    the specified column entry is to be written.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to be written.
!
!     IVAL           is the integer value that will be written
!                    to the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input IVAL is ignored.
!
! $ Detailed_Output
!
!     None.  See the $Particulars section for a description of the
!     effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file is not modified.
!
!     2)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!         The file is not modified.
!
!     3)  If the input flag ISNULL is .TRUE. but the target column
!         does not allow nulls, the error SPICE(BADATTRIBUTE) is
!         signaled.  The file is not modified.
!
!     4)  If RECPTR is invalid, a DAS addressing error may occur.  The
!         error in *not* trapped in advance.  This routine assumes that
!         a valid value of RECPTR has been supplied by the caller.
!
!     3)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.  The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it sets the value of a
!     column entry in an EK segment.  If the column is indexed, the
!     index is updated to reflect the presence of the new entry.  This
!     routine is intended to set values of uninitialized column entries
!     only.  To update existing entries, use the ZZEKUExx routines, or
!     at the user level, the EKUCEx routines.
!
!     This routine does not participate in shadowing functions.  If the
!     target EK is shadowed, the caller is responsible for performing
!     necessary backup operations.  If the target EK is not shadowed,
!     the target record's status is not modified.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKACEI.
!
! $ Restrictions
!
!     1) This routine cannot be used to update existing column entries.
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
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekrp2n

!
!     Local variables
!
      integer(ip)           colidx
      integer(ip)           datptr
      integer(ip)           itype
      integer(ip)           lastw
      integer(ip)           mbase
      integer(ip)           ncols
      integer(ip)           nlinks
      integer(ip)           p
      integer(ip)           pbase
      integer(ip)           ptrloc
      integer(ip)           recno

!
!     Use discovery check-in.
!
!     Make sure the column exists.
!
      ncols   =  segdsc ( ncidx  )
      colidx  =  coldsc ( ordidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     If the value is null, make sure that nulls are permitted
!     in this column.
!
      if (  isnull  .and.  ( coldsc(nflidx) /= itrue )  ) then

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Column having index # in segment # does '  // &
                       'not allow nulls, but a null value was '    // &
                       'supplied for the element in record #.'     )
         call errint ( '#',  colidx                                )
         call errint ( '#',  recno                                 )
         call sigerr ( 'SPICE(BADATTRIBUTE)'                       )
         return

      end if

!
!     Compute the data pointer location.  Check the data pointer to
!     make sure the column entry we're writing to is uninitialized.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( handle, ptrloc, ptrloc, datptr )


      if (  ( datptr /= uninit ) .and. ( datptr /= noback )  ) then

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Column having index # in segment # has '   // &
                       'non-empty element in record #.'            )
         call errint ( '#',  colidx                                )
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  recno                                 )
         call sigerr ( 'SPICE(NONEMPTYENTRY)'                      )
         return

      end if

!
!     Set the data value.
!
      if ( isnull ) then
!
!        All we need do is set the data pointer.  The segment's
!        metadata are not affected.
!
         call dasudi ( handle, ptrloc, ptrloc, null )


      else
!
!        Decide where to write the data value.  If there's room left
!        in the last integer data page, append the value there.
!
         lastw  =  segdsc ( liwidx )


         if ( lastw < ipsize ) then
!
!           There's room in the current page.  Set the data pointer
!           and count, and write the value out to the first free
!           location.
!
            p       =   segdsc(lipidx)

            call zzekpgbs ( int,  p,  pbase )

            datptr  =   pbase + lastw + 1

            call dasudi ( handle, ptrloc, ptrloc, datptr )
            call dasudi ( handle, datptr, datptr, ival   )

!
!           The page containing the data item gains a link.
!
            call zzekglnk ( handle, int, p, nlinks   )
            call zzekslnk ( handle, int, p, nlinks+1 )

!
!           The last integer word in use must be updated.
!
            segdsc ( liwidx )  =  lastw + 1


         else
!
!           Allocate a data page.  Write the data value into the
!           first word of the new page.
!
            call zzekaps  ( handle, segdsc,  int,    .false., p, pbase )
            call dasudi   ( handle, pbase+1, pbase+1, ival             )

!
!           The page containing the data item now has one link.
!
            call zzekslnk ( handle, int, p, 1 )

!
!           The last integer page and word in use must be updated.
!
            segdsc ( lipidx )  =  p
            segdsc ( liwidx )  =  1

!
!           The record pointer must point to this data item.
!
            call dasudi ( handle, ptrloc, ptrloc, pbase+1 )

         end if

      end if

!
!     Write out the updated segment descriptor.
!
      mbase  =  segdsc(imdidx)

      call dasudi ( handle, mbase+1, mbase+sdscsz, segdsc )

!
!     If the column is indexed, we must update the index to account
!     for the new element.
!
      itype  =  coldsc(ixtidx)

      if ( itype /= ifalse ) then
!
!        The column is indexed.
!
         if ( itype == 1 ) then
!
!           The column has a type 1 index.  Insert the record pointer
!           of the current element at the appropriate location.
!
            call zzekiii1 (  handle,  segdsc,  coldsc, &
                             ival,    recptr,  isnull  )

         else

            call setmsg ( 'Column having index # in segment # has '   // &
                          'index type #.'                             )
            call errint ( '#',  colidx                                )
            call errint ( '#',  segdsc(snoidx)                        )
            call errint ( '#',  itype                                 )
            call sigerr ( 'SPICE(INVALIDTYPE)'                        )
            return

         end if

      end if


      return
      end
