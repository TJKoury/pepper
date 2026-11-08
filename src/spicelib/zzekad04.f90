!$Procedure     ZZEKAD04 ( EK, add data to class 4 column )

      subroutine zzekad04 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   ivals,  isnull  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add a column entry to a specified record in a class 4 column.
!     The entries of class 4 columns are arrays of integer values.
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
      integer(ip)           nvals
      integer(ip)           ivals  ( * )
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     NVALS      I   Number of values to add to column.
!     IVALS      I   Integer values to add to column.
!     ISNULL     I   Flag indicating whether column entry is null.
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
!     NVALS,
!     IVALS          are, respectively, the number of values to add to
!                    the specified column and the set of values
!                    themselves.  The data values are written into the
!                    specified column and record.
!
!                    If the column has fixed-size entries, then NVALS
!                    must equal the entry size for the specified column.
!
!                    Only one value can be added to a virtual column.
!
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.  If ISNULL is .FALSE., the column entry
!                    defined by NVALS and IVALS is added to the
!                    specified kernel file.
!
!                    If ISNULL is .TRUE., NVALS and IVALS are ignored.
!                    The contents of the column entry are undefined.
!                    If the column has fixed-length, variable-size
!                    entries, the number of entries is considered to
!                    be 1.
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
!-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
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
      integer(ip)           lastw
      integer(ip)           mbase
      integer(ip)           ncols
      integer(ip)           nlinks
      integer(ip)           nrec
      integer(ip)           nwrite
      integer(ip)           p
      integer(ip)           p2
      integer(ip)           pbase
      integer(ip)           ptrloc
      integer(ip)           recno
      integer(ip)           remain
      integer(ip)           room
      integer(ip)           start

      logical               fstpag

!
!     Use discovery check-in.
!
      nrec    =  segdsc ( nridx  )
      colidx  =  coldsc ( ordidx )

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  nrec                              )
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
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  recno                                 )
         call sigerr ( 'SPICE(BADATTRIBUTE)'                       )
         return

      end if

!
!     Check NVALS.  If the column has fixed-size entries, NVALS must
!     match the declared entry size.  In all cases, NVALS must be
!     positive.
!
      if ( nvals < 1 ) then

         call setmsg ( 'COLIDX = #;  segment = #; NVALS = #;  '  // &
                       'NVALS must be positive '                   )
         call errint ( '#',  colidx                                )
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  nvals                                 )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
         return

      end if


      if ( coldsc(sizidx) /= ifalse ) then

         if ( nvals /= coldsc(sizidx) ) then

            call setmsg ( 'COLIDX = #;  segment = #; NVALS = #; '     // &
                          'declared entry size = #.  Sizes must match.')
            call errint ( '#',  colidx                                 )
            call errint ( '#',  segdsc(snoidx)                         )
            call errint ( '#',  nvals                                  )
            call errint ( '#',  coldsc(sizidx)                         )
            call sigerr ( 'SPICE(INVALIDCOUNT)'                        )
            return

         end if

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

         lastw   =  segdsc ( liwidx )
         room    =  ipsize - lastw
         remain  =  nvals
         start   =  1
         fstpag  =  .true.


         do while ( remain > 0 )
!
!           Decide where to write the data values.  In order to write
!           to the current page, we require enough room for the count
!           and at least one column entry element.
!
            if ( room >= 2 ) then
!
!              There's room in the current page.  If this is the first
!              page this entry is written on, set the data pointer
!              and count.  Write as much of the value as possible to
!              the current page.
!
               p       =   segdsc(lipidx)

               call zzekpgbs ( int,  p,  pbase )

               datptr  =  pbase + lastw + 1


               if ( fstpag ) then

                  call dasudi ( handle, ptrloc, ptrloc, datptr )
                  call dasudi ( handle, datptr, datptr, nvals  )

                  room    =  room   - 1
                  datptr  =  datptr + 1

               end if


               nwrite  =  min ( remain, room )

               call dasudi ( handle, &
                             datptr, &
                             datptr+nwrite-1, &
                             ivals ( start )  )

               remain  =  remain - nwrite
               room    =  room   - nwrite
               start   =  start  + nwrite

!
!              The page containing the data item gains a link.
!
               call zzekglnk ( handle, int, p, nlinks   )
               call zzekslnk ( handle, int, p, nlinks+1 )

!
!              The last integer word in use must be updated.  Account
!              for the count, if this is the first page on which the
!              current entry is written.
!
               if ( fstpag ) then
                  segdsc ( liwidx )  =  lastw + 1 + nwrite
                  fstpag             =  .false.
               else
                  segdsc ( liwidx )  =  lastw + nwrite
               end if


            else
!
!              Allocate a data page.  If this is not the first data
!              page written to, link the previous page to the current
!              one.
!
               call zzekaps ( handle, segdsc, int, .false., p2, pbase )

               if ( .not. fstpag ) then
                  call zzeksfwd ( handle, int, p, p2 )
               end if

!
!              The last integer page and word in use must be updated.
!
               p                  =  p2
               lastw              =  0
               segdsc ( lipidx )  =  p
               segdsc ( liwidx )  =  lastw
               room               =  ipsize

!
!              Make sure the link count is zeroed out.
!
               call zzekslnk ( handle, int, p, 0 )

            end if

         end do


      end if

!
!     Write out the updated segment descriptor.
!
      mbase  =  segdsc(imdidx)

      call dasudi ( handle, mbase+1, mbase+sdscsz, segdsc )

!
!     Class 4 columns are not indexed, so we need not update any
!     index to account for the new element.
!

      return
      end
