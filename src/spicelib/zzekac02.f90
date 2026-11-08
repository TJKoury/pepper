!$Procedure     ZZEKAC02 ( EK, add class 2 column to segment )

      subroutine zzekac02 (  handle,  segdsc,  coldsc,  dvals, &
                             nlflgs,  rcptrs,  wkindx         )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Add an entire class 2 column to an EK segment.
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
      real(wp)              dvals  ( * )
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
!     DVALS      I   D.p. values to add to column.
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
!     DVALS          is an array containing the entire set of column
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
!                    defined by DVALS is added to the specified segment
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
!     done by sequential calls to EKACED, but has the drawback that
!     the caller must use more memory for the routine's inputs.  This
!     routine cannot be used to add data to a partially completed
!     column.
!
! $ Examples
!
!     See EKACLD.
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
!-    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB)
!
!        Bug fix:  case of 100% null data values is now handled
!        correctly.  Previous version line was changed from "Beta"
!        to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB)
!
!        Bug fix:  case of 100% null data values is now handled
!        correctly.  The test to determine when to write a page
!        was fixed to handle this case.
!
!        Previous version line was changed from "Beta"
!        to "SPICELIB."
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip)           bufsiz
      parameter           ( bufsiz = dpsize )

!
!     Local variables
!
      character*(cnamsz)    column

      real(wp)              page   ( pgsizd )

      integer(ip)           adrbuf ( bufsiz )
      integer(ip)           bufptr
      integer(ip)           class
      integer(ip)           colidx
      integer(ip)           dscbas
      integer(ip)           from
      integer(ip)           i
      integer(ip)           idxtyp
      integer(ip)           p
      integer(ip)           pbase
      integer(ip)           mbase
      integer(ip)           n
      integer(ip)           ndata
      integer(ip)           nnull
      integer(ip)           nrows
      integer(ip)           nulptr
      integer(ip)           nwrite
      integer(ip)           remain
      integer(ip)           to
      integer(ip)           tree

      logical               indexd
      logical               nullok

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKAC02' )
      end if

!
!     Grab the column's attributes.
!
      class  =  coldsc( clsidx )
      idxtyp =  coldsc( ixtidx )
      nulptr =  coldsc( nflidx )
      colidx =  coldsc ( ordidx )

      nullok =  nulptr .ne. ifalse
      indexd =  idxtyp .ne. ifalse

!
!     This column had better be class 2.
!
      if ( class .ne. 2 ) then

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 2.'                 )
         call errint ( '#', class                                      )
         call errch  ( '#', column                                     )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         call chkout ( 'ZZEKAC02'                                      )
         return

      end if

!
!     If the column is indexed, the index type should be 1; we don't
!     know how to create any other type of index.
!
      if  (  indexd  .and.  ( idxtyp .ne. 1 )  ) then

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Index type code # found in descriptor for '   // &
                       'column #.  Code should be 1.'                  )
         call errint ( '#', idxtyp                                     )
         call errch  ( '#', column                                     )
         call sigerr ( 'SPICE(UNRECOGNIZEDTYPE)'                       )
         call chkout ( 'ZZEKAC02'                                      )
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
!     Decide how many pages we'll need to store the data.  Also
!     record the number of data values to write.
!
      if ( nullok ) then
!
!        Count the non-null rows; these are the ones that will take
!        up space.
!
         nnull  =  0

         do i = 1, nrows

            if ( nlflgs(i) ) then
               nnull  =  nnull + 1
            end if

         end do

         ndata  =  nrows - nnull


      else

         ndata  =  nrows

      end if


      if ( ndata .gt. 0 ) then
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         call zzekaps  ( handle, segdsc, dp, .false., p, pbase )
         call cleard   ( pgsizd, page )

      end if

!
!     Write the input data out to the target file a page at a time.
!     Null values don't get written.
!
!     While we're at it, we'll push onto the EK stack the addresses
!     of the column entries.  We use the constant NULL rather than an
!     address to represent null entries.
!
!     We'll use FROM to indicate the element of DVALS we're
!     considering, TO to indicate the element of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.  The variable N indicates the number of data
!     items in the current page.
!

      remain  =  nrows
      from    =  1
      to      =  1
      bufptr  =  1
      nwrite  =  0
      n       =  0

      do while ( remain .gt. 0 )

         if (  nullok  .and.  ( nlflgs(from) )  ) then

            adrbuf(bufptr) =  null

         else

            adrbuf(bufptr) =  to      +  pbase
            page(to)       =  dvals(from)
            to             =  to      +  1
            nwrite         =  nwrite  +  1
            n              =  n       +  1

         end if


         from    =  from   + 1
         remain  =  remain - 1


         if (  ( bufptr .eq. bufsiz ) .or. ( remain .eq. 0 )  ) then
!
!           The address buffer is full or we're out of input values
!           to look at, so push the buffer contents on the stack.
!
            call zzekspsh ( bufptr, adrbuf )
            bufptr  =  1

         else

            bufptr  =  bufptr + 1

         end if


         if (  (   n      .eq. dpsize ) .or. &
               ( ( nwrite .eq. ndata  ) .and. ( ndata  .ne. 0 ) ) ) then
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.  First set the link count.
!
            page ( dlcidx ) =  n

!
!           Write out the data page.
!
            call zzekpgwd ( handle, p, page  )

!
!           If there's more data to write, allocate another page.
!
            if ( nwrite .lt. ndata ) then

               call zzekaps  ( handle, segdsc, dp, .false., p, pbase )
               call cleard   ( pgsizd, page )

               n   =  0
               to  =  1

            end if

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

         call zzekordd ( dvals,  nullok, nlflgs, nrows, wkindx )

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


      call chkout ( 'ZZEKAC02' )
      return
      end
