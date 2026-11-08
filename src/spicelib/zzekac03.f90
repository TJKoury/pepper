!$Procedure     ZZEKAC03 ( EK, add class 3 column to segment )

      subroutine zzekac03 (  handle,  segdsc,  coldsc,  cvals, &
                             nlflgs,  rcptrs,  wkindx  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add an entire class 3 column to an EK segment.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!
! $ Declarations


      include  'ekbool.inc'
      include  'ekcnamsz.inc'
      include  'ekcoldsc.inc'
      include  'ekdatpag.inc'
      include  'ekpage.inc'
      include  'ekrecptr.inc'
      include  'eksegdsc.inc'
      include  'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( * )
      integer(ip)           coldsc ( * )
      character(*)          cvals  ( * )
      logical               nlflgs ( * )
      integer(ip)           rcptrs ( * )
      integer(ip)           wkindx ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     CVALS      I   Character values to add to column.
!     NLFLGS     I   Array of null flags for column entries.
!     RCPTRS     I   Array of record pointers for segment.
!     WKINDX    I-O  Work space for column index.
!
! $ Detailed_Input
!
!     HANDLE         the handle of an EK file that is open for writing.
!                    A `begin segment for fast load' operation must
!                    have already been performed for the designated
!                    segment.
!
!     SEGDSC         is a descriptor for the segment to which data is
!                    to be added.  The segment descriptor is not
!                    updated by this routine, but some fields in the
!                    descriptor will become invalid after this routine
!                    returns.
!
!     COLDSC         is a descriptor for the column to be added.  The
!                    column attributes must be filled in, but any
!                    pointers may be uninitialized.
!
!     CVALS          is an array containing the entire set of column
!                    entries for the specified column.  The entries
!                    are listed in row-order:  the column entry for the
!                    first row of the segment is first, followed by the
!                    column entry for the second row, and so on.  The
!                    number of column entries must match the declared
!                    number of rows in the segment.  Elements must be
!                    allocated for each column entry, including null
!                    entries.
!
!     NLFLGS         is an array of logical flags indicating whether
!                    the corresponding entries are null.  If the Ith
!                    element of NLFLGS is .FALSE., the Ith column entry
!                    defined by CVALS is added to the specified segment
!                    in the specified kernel file.
!
!                    If the Ith element of NLFGLS is .TRUE., the
!                    contents of the Ith column entry are undefined.
!
!                    NLFLGS is used only for columns that allow null
!                    values; it's ignored for other columns.
!
!     RCPTRS         is an array of record pointers for the input
!                    segment.  These pointers are base addresses of the
!                    `record pointer structures' for the segment.
!                    These pointers are used instead of record numbers
!                    in column indexes:  the indexes map ordinal
!                    positions to record pointers.
!
!     WKINDX         is a work space array used for building a column
!                    index.  If the column is indexed, the dimension of
!                    WKINDX must be at NROWS, where NROWS is the number
!                    of rows in the column.  If the column is not
!                    indexed, this work space is not used, so the
!                    dimension may be any positive value.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it modifies the named
!     EK file by adding data to the specified column.  This routine
!     writes the entire contents of the specified column in one shot.
!     This routine creates columns much more efficiently than can be
!     done by sequential calls to EKACEC, but has the drawback that
!     the caller must use more memory for the routine's inputs.  This
!     routine cannot be used to add data to a partially completed
!     column.
!
! $ Examples
!
!     See EKACLC.
!
! $ Restrictions
!
!     1)  This routine assumes the EK scratch area has been set up
!         properly for a fast load operation.  This routine writes
!         to the EK scratch area as well.
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
!-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           rtrim

      logical               return

!
!     Local parameters
!
      integer(ip)           bufsiz
      parameter           ( bufsiz = cpsize )

!
!     Local variables
!
      character(cnamsz)     column
      character(pgsizc)     page

      integer(ip)           adrbuf ( bufsiz )
      integer(ip)           bufptr
      integer(ip)           class
      integer(ip)           colidx
      integer(ip)           colwid
      integer(ip)           dscbas
      integer(ip)           from
      integer(ip)           i
      integer(ip)           idxtyp
      integer(ip)           p
      integer(ip)           p2
      integer(ip)           pbase
      integer(ip)           pos
      integer(ip)           mbase
      integer(ip)           n
      integer(ip)           nchars
      integer(ip)           ndata
      integer(ip)           nlinks
      integer(ip)           nnull
      integer(ip)           nrows
      integer(ip)           nulptr
      integer(ip)           nwrite
      integer(ip)           remain
      integer(ip)           room
      integer(ip)           strlen
      integer(ip)           to
      integer(ip)           tree

      logical               fixlen
      logical               indexd
      logical               nullok

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Grab the column's attributes.  Initialize the maximum non-blank
!     width of the column.
!
      class  =  coldsc ( clsidx )
      idxtyp =  coldsc ( ixtidx )
      nulptr =  coldsc ( nflidx )
      colidx =  coldsc ( ordidx )
      colwid =  coldsc ( lenidx )

      nullok =  nulptr /= ifalse
      indexd =  idxtyp /= ifalse
      fixlen =  colwid /= ifalse

!
!     This column had better be class 3.
!
      if ( class /= 3 ) then

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 3.'                 )
         call errint ( '#', class                                      )
         call errch  ( '#', column                                     )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         return

      end if

!
!     If the column is indexed, the index type should be 1; we don't
!     know how to create any other type of index.
!
      if  (  indexd  .and.  ( idxtyp /= 1 )  ) then

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Index type code # found in descriptor for '   // &
                       'column #.  Code should be 1.'                  )
         call errint ( '#', idxtyp                                     )
         call errch  ( '#', column                                     )
         call sigerr ( 'SPICE(UNRECOGNIZEDTYPE)'                       )
         return

      end if

!
!     Push the column's ordinal index on the stack.  This allows us
!     to identify the column the addresses belong to.
!
      call zzekspsh ( 1, colidx )

!
!     Find the number of rows in the segment.
!
      nrows   =  segdsc ( nridx )

!
!     Count the number of strings to write.
!
      if ( nullok ) then
!
!        Count the non-null column entries; these are the
!        ones that will take up space.
!
         nnull  =  0
         ndata  =  0

         do i = 1, nrows

            if ( nlflgs(i) ) then
               nnull  =  nnull + 1
            end if

         end do

         ndata  =   nrows  -  nnull

      else

         ndata  =   nrows

      end if


      if ( ndata > 0 ) then
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         call zzekaps ( handle, segdsc, chr, .false., p, pbase )
         page  =  ' '

!
!        The link count starts out at zero.
!
         call prtenc ( 0,  page(clcidx:clcidx+encsiz-1) )

      end if

!
!     Write the input data out to the target file a page at a time.
!     Null values don't get written.
!
!     While we're at it, we'll push onto the EK stack the addresses
!     of the column entries.  We use the constant NULL rather than an
!     address to represent null entries.
!
!     We'll use FROM to indicate the element of CVALS we're
!     considering, TO to indicate the first character of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.  The variable N indicates the number of characters
!     written to the current page.  NCHARS indicates the number of
!     characters left to write from the current input element.  NWRITE
!     will be used to count the column entries written so far.
!

      remain  =  nrows
      from    =  0
      to      =  1
      bufptr  =  1
      nwrite  =  0
      n       =  0

      do while ( remain > 0 )
!
!        Examine a column entry.  Write it out if it's non-null.
!
         from  =  from   + 1


         if (  nullok  .and.  ( nlflgs(from) )  ) then

            adrbuf(bufptr) =  null

         else
!
!           Write out the current column entry.  The entry
!           might span multiple pages.  However, we're guaranteed
!           enough room to write out to the current page the encoded
!           character count and at least one character of data.
!
!           Update the non-blank width for the column each time we
!           determine the length of an input string.
!
            if ( fixlen ) then
               strlen  =  min (  rtrim( cvals(from) ),  colwid  )
            else
               strlen  =  rtrim( cvals(from) )
            end if

            adrbuf(bufptr) =  to  +  pbase
            pos            =  1

!
!           Start out with the string length.
!
            call prtenc (  strlen,  page(to : to+encsiz-1)  )

            n       =   n  +  encsiz
            to      =   n  +  1
            nchars  =   strlen


            do while ( nchars > 0 )

               room  =  cpsize - n

               if ( nchars <= room ) then
!
!                 The remaining portion of the string will fit on the
!                 current page.
!
                  page(to:to+nchars-1) =  cvals(from) (pos:pos+nchars-1)
                  n                    =  n   +  nchars
                  to                   =  n   +  1
                  nchars               =  0

!
!                 Add a link to the current page.
!

                  call prtdec ( page(clcidx:clcidx+encsiz-1),   nlinks )

                  call prtenc ( nlinks+1, page(clcidx:clcidx+encsiz-1) )


               else
!
!                 The string will have to be continued on another page.
!                 Write out the first ROOM characters to the current
!                 page first.
!
                  page(to:cpsize)  =  cvals(from) (pos:pos+room-1)
                  pos              =  pos    + room
                  nchars           =  nchars - room

!
!                 Add a link to the current page.
!
                  call prtdec ( page(clcidx:clcidx+encsiz-1),   nlinks )

                  call prtenc ( nlinks+1, page(clcidx:clcidx+encsiz-1) )

!
!                 Allocate another page.  Fill in the forward pointer
!                 in the previous page.
!
                  call zzekaps (  handle,  segdsc,  chr, &
                                 .false.,  p2,      pbase )

                  call prtenc   ( p2,  page(cfpidx:cfpidx+encsiz-1) )

!
!                 Write out the full data page.  Get ready to write
!                 to the new page.
!
                  call zzekpgwc ( handle, p, page  )

                  p    =  p2
                  page = ' '
                  call prtenc ( 0,  page(clcidx:clcidx+encsiz-1) )

                  n    =  0
                  to   =  1

               end if

            end do
!
!           We've written out a column entry.
!
            nwrite  =  nwrite + 1


         end if
!
!        We're done with the current column entry, null or not.
!

         if ( nwrite < ndata ) then
!
!           There is at least one more column entry to write.
!           If there's not enough room on the current page to begin
!           writing another column entry, write out the page and
!           allocate another.
!
            room  =  cpsize - n

            if ( room  <  ( 1 + encsiz )  ) then

               call zzekpgwc ( handle, p,      page  )
               call zzekaps  ( handle, segdsc, chr, .false., p, pbase )

               page  =  ' '
               call prtenc ( 0,  page(clcidx:clcidx+encsiz-1) )

               n     =  0
               to    =  1

            end if


         else if ( n > 0 ) then
!
!           We've written the last of the non-null data to the current
!           page.  Write out this page.
!
            call zzekpgwc ( handle,  p,  page  )
            n  =  0

         end if


         remain  =  remain - 1


         if (  ( bufptr == bufsiz ) .or. ( remain == 0 )  ) then
!
!           The address buffer is full or we're out of input values
!           to look at, so push the buffer contents on the stack.
!
            call zzekspsh ( bufptr, adrbuf )
            bufptr  =  1

         else

            bufptr  =  bufptr + 1

         end if

      end do

!
!     If the column is supposed to have an index, now is the time to
!     build that index.  We'll find the order vector for the input
!     values, overwrite the elements of the order vector with the
!     corresponding elements of the input array of record pointers, then
!     load this sorted copy of the record pointer array into a tree in
!     one shot.
!
      if ( indexd ) then

         call zzekordc ( cvals,  nullok, nlflgs, nrows, wkindx )

         do i = 1, nrows
            wkindx(i) =  rcptrs( wkindx(i) )
         end do

         call zzektrit ( handle, tree                )
         call zzektr1s ( handle, tree, nrows, wkindx )

!
!        Update the segment's metadata to point to the index.  The
!        pointer indicates the root page of the tree.
!
         mbase   =  segdsc ( imdidx )
         dscbas  =  mbase  +  sdscsz  +  (colidx-1)*cdscsz

         call dasudi ( handle,  dscbas+ixpidx,  dscbas+ixpidx,  tree )

      end if


      return
      end
