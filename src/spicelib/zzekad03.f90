!$Procedure     ZZEKAD03 ( EK, add data to class 3 column )

      subroutine zzekad03 (  handle,  segdsc,  coldsc, &
                             recptr,  cval,    isnull  )

! $ Abstract
!
!     Add a column entry to a class 3 column in a specified EK record.
!     Class 3 columns contain scalar, character values.
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

      integer               handle
      integer               segdsc ( * )
      integer               coldsc ( * )
      integer               recptr
      character*(*)         cval
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     CVAL       I   Character string value.
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
!     CVAL           is the character string value that will be written
!                    to the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input CVAL is ignored.
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
!     See EKACEC.
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
!     SPICELIB functions
!
      integer               rtrim

!
!     Non-SPICELIB functions
!
      integer               zzekrp2n

!
!     Local variables
!
      integer               colidx
      integer               datptr
      integer               itype
      integer               lastw
      integer               lnb
      integer               mbase
      integer               n
      integer               ncols
      integer               nlinks
      integer               nwrite
      integer               p
      integer               p2
      integer               pbase
      integer               pcount
      integer               pos
      integer               prvbas
      integer               ptrloc
      integer               recno
      integer               strlen

      logical               fixlen

!
!     Use discovery check-in.
!
!     Make sure the column exists.
!
      ncols   =  segdsc ( ncidx  )
      colidx  =  coldsc ( ordidx )

      if (  ( colidx .lt. 1 ) .or. ( colidx .gt. ncols )  ) then

         call chkin  ( 'ZZEKAD03'                              )
         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         call chkout ( 'ZZEKAD03'                              )
         return

      end if

!
!     If the value is null, make sure that nulls are permitted
!     in this column.
!
      if (  isnull  .and.  ( coldsc(nflidx) .ne. itrue )  ) then

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call chkin  ( 'ZZEKAD03'                                  )
         call setmsg ( 'Column having index # in segment # does '  // &
                       'not allow nulls, but a null value was '    // &
                       'supplied for the element in record #.'     )
         call errint ( '#',  colidx                                )
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  recno                                 )
         call sigerr ( 'SPICE(BADATTRIBUTE)'                       )
         call chkout ( 'ZZEKAD03'                                  )
         return

      end if

!
!     Decide the length of the string value.  If the column contains
!     variable-length strings, the effective length of the string is
!     just the non-blank length of CVAL.  Otherwise, the effective
!     string length is the minimum of the non-blank length and the
!     column's declared string length.  We don't store trailing blanks.
!
      fixlen  =  coldsc(lenidx) .ne. ifalse
      lnb     =  rtrim ( cval )

      if ( fixlen ) then
         strlen  =  min (  coldsc(lenidx),  lnb )
      else
         strlen  =  lnb
      end if

!
!     Compute the data pointer location.
!
      ptrloc  =  recptr + dptbas + colidx


      if ( isnull ) then
!
!        All we need do is set the data pointer.  The segment's
!        metadata are not affected.
!
         call dasudi ( handle, ptrloc, ptrloc, null )


      else
!
!        Write out the data value.  If we run out of room in the
!        page we're writing to, we allocate a new page and link
!        the previous page to it.
!
         n       =  strlen
         pos     =  1
         lastw   =  segdsc ( lcwidx )
         p       =  segdsc ( lcpidx )
         pcount  =  0


         do while ( n .gt. 0 )
!
!           Write as much data as possible into the current page.
!

            if ( lastw .lt. cpsize-encsiz ) then
!
!              There's room in the current page.  We never split an
!              encoded character count across pages, and we always
!              write at least one data character to the current page.
!              This practice is slightly wasteful of space but greatly
!              simplifies our logic.
!
!              Keep track of the number of pages our string spans.
!
               pcount  =  pcount + 1

!
!              If this is the first data page, write the data pointer
!              into the record pointer and the character count into
!              the data page.
!
               if ( pcount .eq. 1 ) then

                  call zzekpgbs ( chr, p, pbase )

                  datptr  =  pbase + lastw + 1

                  call dasudi  ( handle, ptrloc, ptrloc, datptr )
                  call zzeksei ( handle, datptr, strlen )
!
!                 Advance the data pointer to the first data
!                 character's position.  The last word in use
!                 increases as well.
!
                  datptr  =  datptr + encsiz
                  lastw   =  lastw  + encsiz

               else
!
!                 We still need the data pointer.
!
                  datptr  =  pbase + 1

               end if

!
!              Compute the number of characters to write into this page,
!              and write that number of characters.
!
               nwrite  =  min ( cpsize-lastw, n )

               call dasudc (  handle,   datptr,   datptr+nwrite-1, &
                              1,        nwrite,   cval(pos:)        )

               n       =  n   -  nwrite
               pos     =  pos +  nwrite

!
!              The page containing the data item gains a link.
!
               call zzekglnk ( handle, chr, p, nlinks   )
               call zzekslnk ( handle, chr, p, nlinks+1 )

!
!              The last character word in use must be updated.
!
               lastw              =  lastw + nwrite
               segdsc ( lcwidx )  =  lastw

!
!              Retain the base address of this data page.
!
               prvbas  =  pbase


            else
!
!              Allocate a data page.  If this is not the first data
!              page written to, link the previous page to the current
!              one.
!
               call zzekaps ( handle, segdsc, chr, .false., p2, pbase )

               if ( pcount .gt. 0 ) then
                  call zzeksfwd ( handle, chr, p, p2 )
               end if

!
!              The last character page and word in use must be updated.
!
               p                  =  p2
               lastw              =  0
               segdsc ( lcpidx )  =  p
               segdsc ( lcwidx )  =  lastw

!
!              Make sure the link count is zeroed out.
!
               call zzekslnk ( handle, chr, p, 0 )

            end if

         end do

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

      if ( itype .ne. ifalse ) then
!
!        The column is indexed.
!
         if ( itype .eq. 1 ) then
!
!           The column has a type 1 index.  Insert the record number
!           of the current element at the appropriate location.
!
            call zzekiic1 (  handle,  segdsc,  coldsc, &
                             cval,    recptr,  isnull   )

         else

            call chkin  ( 'ZZEKAD03'                                  )
            call setmsg ( 'Column having index # in segment # has '   // &
                          'index type #.'                             )
            call errint ( '#',  colidx                                )
            call errint ( '#',  segdsc(snoidx)                        )
            call errint ( '#',  itype                                 )
            call sigerr ( 'SPICE(INVALIDTYPE)'                        )
            call chkout ( 'ZZEKAD03'                                  )
            return

         end if

      end if

      return
      end
