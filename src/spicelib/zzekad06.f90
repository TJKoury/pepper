!$Procedure     ZZEKAD06 ( EK, add data to class 6 column )

      subroutine zzekad06 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   cvals,  isnull  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add a column entry to a specified record in a class 6 column.
!     The entries of class 6 columns are arrays of character string
!     values.
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
      character*(*)         cvals  ( * )
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     SEGNO      I   Index of segment containing record.
!     RECNO      I   Record to which data is to be added.
!     COLUMN     I   Column name.
!     NVALS      I   Number of values to add to column.
!     CVALS      I   Character values to add to column.
!     ISNULL     I   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file that is open for write
!                    access.
!
!     SEGNO          is the index of the segment to which data is to
!                    be added.
!
!     RECNO          is the index of the record to which data is to be
!                    added.  This record number is relative to the start
!                    of the segment indicated by SEGNO; the first
!                    record in the segment has index 1.
!
!     COLUMN         is the name of the column to which data is to be
!                    added.
!
!     NVALS,
!     CVALS          are, respectively, the number of values to add to
!                    the specified column and the set of values
!                    themselves.  The data values are written into the
!                    specified column and record.
!
!                    If the  column has fixed-size entries, then NVALS
!                    must equal the entry size for the specified column.
!
!                    Only one value can be added to a virtual column.
!
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.  If ISNULL is .FALSE., the column entry
!                    defined by NVALS and CVALS is added to the
!                    specified kernel file.
!
!                    If ISNULL is .TRUE., NVALS and CVALS are ignored.
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
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekrp2n

!
!     Local parameters
!
      integer(ip)           maxpad
      parameter           ( maxpad = 100 )

!
!     Local variables
!
      character*(maxpad)    padbuf

      integer(ip)           colidx
      integer(ip)           cvlen
      integer(ip)           datptr
      integer(ip)           eltidx
      integer(ip)           lastw
      integer(ip)           mbase
      integer(ip)           mnroom
      integer(ip)           n
      integer(ip)           nchrs
      integer(ip)           ncols
      integer(ip)           nlinks
      integer(ip)           np
      integer(ip)           npad
      integer(ip)           nrec
      integer(ip)           nwrite
      integer(ip)           p
      integer(ip)           p2
      integer(ip)           padlen
      integer(ip)           pbase
      integer(ip)           pos
      integer(ip)           ptrloc
      integer(ip)           recno
      integer(ip)           remain
      integer(ip)           room
      integer(ip)           strlen
      integer(ip)           wpad

      logical               first
      logical               fstpag
      logical               pad

!
!     Saved variables
!
      save

!
!     Initial values
!
      data                  first  / .true. /


!
!     Use discovery check-in.
!
      if ( first ) then
         padbuf = ' '
         first  = .false.
      end if

!
!     Make sure the record exists.
!
      nrec    =  segdsc ( nridx  )
      colidx  =  coldsc ( ordidx )

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx )

      if (  ( colidx .lt. 1 ) .or. ( colidx .gt. ncols )  ) then

         call chkin  ( 'ZZEKAD06'                              )
         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  nrec                              )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         call chkout ( 'ZZEKAD06'                              )
         return

      end if

!
!     If the value is null, make sure that nulls are permitted
!     in this column.
!
      if (  isnull  .and.  ( coldsc(nflidx) .ne. itrue )  ) then

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call chkin  ( 'ZZEKAD06'                                  )
         call setmsg ( 'Column having index # in segment # does '  // &
                       'not allow nulls, but a null value was '    // &
                       'supplied for the element in record #.'     )
         call errint ( '#',  colidx                                )
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  recno                                 )
         call sigerr ( 'SPICE(BADATTRIBUTE)'                       )
         call chkout ( 'ZZEKAD06'                                  )
         return

      end if

!
!     Check NVALS.  If the column has fixed-size entries, NVALS must
!     match the declared entry size.  In all cases, NVALS must be
!     positive.
!
      if ( nvals .lt. 1 ) then

         call chkin  ( 'ZZEKAD06'                                  )
         call setmsg ( 'COLIDX = #;  segment = #; NVALS = #;  '  // &
                       'NVALS must be positive '                   )
         call errint ( '#',  colidx                                )
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  nvals                                 )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
         call chkout ( 'ZZEKAD06'                                  )
         return

      end if


      if ( coldsc(sizidx) .ne. ifalse ) then

         if ( nvals .ne. coldsc(sizidx) ) then

            call chkin  ( 'ZZEKAD06'                                   )
            call setmsg ( 'COLIDX = #;  segment = #; NVALS = #; '     // &
                          'declared entry size = #.  Sizes must match.')
            call errint ( '#',  colidx                                 )
            call errint ( '#',  segdsc(snoidx)                         )
            call errint ( '#',  nvals                                  )
            call errint ( '#',  coldsc(sizidx)                         )
            call sigerr ( 'SPICE(INVALIDCOUNT)'                        )
            call chkout ( 'ZZEKAD06'                                   )
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
!
!        Decide now whether we will need to pad the input entry
!        elements with trailing blanks, and if so how much padding
!        we'll need.
!
         strlen  =  coldsc ( lenidx   )
         cvlen   =  len    ( cvals(1) )
         pad     =  cvlen  .lt.  strlen

         if ( pad ) then
            padlen  =  strlen - cvlen
         end if

         lastw   =  segdsc ( lcwidx )
         room    =  cpsize - lastw
         fstpag  =  .true.

!
!        Initialize the page base and target data pointer, if possible.
!        If the current page is full, these functions will be performed
!        below in the code section in which a new page is allocated.
!
         if ( lastw .lt. cpsize ) then

            p       =  segdsc(lcpidx)

            call zzekpgbs ( chr,  p,  pbase )

            datptr  =  pbase + lastw + 1

         end if


         eltidx  =  1

         do while (  ( eltidx .le. nvals ) .and. ( .not. failed() )  )
!
!           Write out the element having index ELTIDX.
!
            pos     =  0
            remain  =  strlen


            do while ( remain .gt. 0 )
!
!              Decide where to write the data values.  In order to write
!              a new entry, we require enough room for the count
!              and at least one character of data.
!
               if ( fstpag ) then
                  mnroom  =  1 + encsiz
               else
                  mnroom  =  1
               end if


               if ( room  .ge.  mnroom ) then
!
!                 There's room in the current page.  If this is the
!                 first page this entry is written on, set the data
!                 pointer and count.  Write as much of the value as
!                 possible to the current page.
!
                  if ( fstpag ) then

                     call dasudi  ( handle, ptrloc, ptrloc, datptr )
                     call zzeksei ( handle, datptr, nvals          )

                     room    =  room   - encsiz
                     datptr  =  datptr + encsiz

!
!                    The first page containing some or all of the data
!                    item gains a link.
!
                     call zzekglnk ( handle, chr, p, nlinks   )
                     call zzekslnk ( handle, chr, p, nlinks+1 )

                  end if

!
!                 Write the characters we can fit onto the current page.
!
                  nwrite  =  min ( remain, room )
                  n       =  nwrite

                  do while ( n .gt. 0 )

                     if ( pos .lt. cvlen )   then
!
!                       Take data from the input string CVALS(ELTIDX).
!
                        nchrs  =  min ( n, cvlen - pos )

                        call dasudc ( handle, &
                                      datptr, &
                                      datptr + nchrs - 1, &
                                      pos    + 1, &
                                      pos    + nchrs, &
                                      cvals( eltidx )     )

                        n      =  n      - nchrs
                        pos    =  pos    + nchrs
                        datptr =  datptr + nchrs


                     else if ( pad ) then
!
!                       We must add trailing blanks to the column
!                       entry at this point.
!
                        npad  =  min ( n, padlen )
                        np    =  npad

                        do while ( np .gt. 0 )

                           wpad  =  min ( np, maxpad )

                           call dasudc ( handle, &
                                         datptr, &
                                         datptr + wpad - 1, &
                                         1, &
                                         wpad, &
                                         padbuf          )

                           np      =  np     - wpad
                           datptr  =  datptr + wpad

                        end do

                        n   =   n   - npad
                        pos =   pos + npad

                     end if

                  end do
!
!                 We've written all we can to the current page.
!
                  remain  =  remain - nwrite
                  room    =  room   - nwrite

!
!                 The last character word in use must be updated.
!                 Account for the count, if this is the first page on
!                 which the current entry is written.
!
                  if ( fstpag ) then

                     lastw              =  lastw + encsiz + nwrite
                     segdsc ( lcwidx )  =  lastw
                     fstpag             =  .false.

                  else

                     lastw              =  lastw + nwrite
                     segdsc ( lcwidx )  =  lastw

                  end if


               else
!
!                 Allocate a character data page.  If this is not the
!                 first data page written to, link the previous page to
!                 the current one.
!
                  call zzekaps ( handle,   segdsc,  chr, &
                                 .false.,  p2,      pbase  )

                  if ( .not. fstpag ) then
                     call zzeksfwd ( handle, chr, p, p2 )
                  end if

                  p                  =  p2
                  lastw              =  0
                  segdsc ( lcpidx )  =  p
                  segdsc ( lcwidx )  =  lastw

                  room               =  cpsize
                  datptr             =  pbase  +  1

!
!                 Set the link count.  If this is the first page
!                 onto which the input column entry is written,
!                 just zero out the count; the count will be set above.
!                 Additional pages get one link.
!
                  if ( fstpag ) then
                     nlinks  =  0
                  else
                     nlinks  =  1
                  end if

                  call zzekslnk ( handle, chr, p, nlinks )

               end if


            end do
!
!           We've written out the current element.
!

            eltidx  =  eltidx + 1

         end do


      end if

!
!     Write out the updated segment descriptor.
!
      mbase  =  segdsc(imdidx)

      call dasudi ( handle, mbase+1, mbase+sdscsz, segdsc )

!
!     Class 6 columns are not indexed, so we need not update any
!     index to account for the new element.
!

      return
      end
