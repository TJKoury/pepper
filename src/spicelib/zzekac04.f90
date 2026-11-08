!$Procedure     ZZEKAC04 ( EK, add class 4 column to segment )

      subroutine zzekac04 (  handle,  segdsc,  coldsc, &
                             ivals,   entszs,  nlflgs  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add an entire class 4 column to an EK segment.
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
      include  'ekcoldsc.inc'
      include  'ekcnamsz.inc'
      include  'ekdatpag.inc'
      include  'ekpage.inc'
      include  'ekrecptr.inc'
      include  'eksegdsc.inc'
      include  'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( * )
      integer(ip)           coldsc ( * )
      integer(ip)           ivals  ( * )
      integer(ip)           entszs ( * )
      logical               nlflgs ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     IVALS      I   Integer values to add to column.
!     ENTSZS     I   Array of sizes of column entries.
!     NLFLGS     I   Array of null flags for column entries.
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
!     ENTSZS         is an array containing sizes of column entries.
!                    The Ith element of ENTSZS gives the size of the
!                    Ith column entry.  ENTSZS is used only for columns
!                    having variable-size entries.  For such columns,
!                    the dimension of ENTSZS must be at least NROWS.
!                    The size of null entries should be set to zero.
!
!                    For columns having fixed-size entries, the
!                    dimension of this array may be any positive value.
!
!     IVALS          is an array containing the entire set of column
!                    entries for the specified column.  The entries
!                    are listed in row-order:  the column entry for the
!                    first row of the segment is first, followed by the
!                    column entry for the second row, and so on.  The
!                    number of column entries must match the declared
!                    number of rows in the segment.  For columns having
!                    fixed-size entries, a null entry must be allocated
!                    the same amount of space occupied by a non-null
!                    entry in the array IVALS.  For columns having
!                    variable-size entries, null entries do not require
!                    any space in the IVALS array, but in any case must
!                    have their allocated space described correctly by
!                    the corresponding element of the ENTSZS array
!                    (described below).
!
!     ENTSZS         is an array containing sizes of column entries.
!                    The Ith element of ENTSZS gives the size of the
!                    Ith column entry.  ENTSZS is used only for columns
!                    having variable-size entries.  For such columns,
!                    the dimension of ENTSZS must be at least NROWS.
!                    The size of null entries should be set to zero.
!
!                    For columns having fixed-size entries, the
!                    dimension of this array may be any positive value.
!
!     NLFLGS         is an array of logical flags indicating whether
!                    the corresponding entries are null.  If the Ith
!                    element of NLFLGS is .FALSE., the Ith column entry
!                    defined by IVALS is added to the specified segment
!                    in the specified kernel file.
!
!                    If the Ith element of NLFGLS is .TRUE., the
!                    contents of the Ith column entry are undefined.
!
!                    NLFLGS is used only for columns that allow null
!                    values; it's ignored for other columns.
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
!     done by sequential calls to EKACEI, but has the drawback that
!     the caller must use more memory for the routine's inputs.  This
!     routine cannot be used to add data to a partially completed
!     column.
!
! $ Examples
!
!     See EKACLI.
!
! $ Restrictions
!
!     1)  This routine assumes the EK scratch area has been set up
!         properly for a fast load operation.  This routine writes
!         to the EK scratch area as well.
!
!     2)  Only one segment can be created at a time using the fast
!         load routines.
!
!     3)  No other EK operation may interrupt a fast load.  For
!         example, it is not valid to issue a query while a fast load
!         is in progress.
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
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
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
      integer(ip), parameter :: bufsiz = ipsize

!
!     Local variables
!
      character(cnamsz)     column

      integer(ip)           adrbuf ( bufsiz )
      integer(ip)           bufptr
      integer(ip)           class
      integer(ip)           colidx
      integer(ip)           cursiz
      integer(ip)           from
      integer(ip)           i
      integer(ip)           n
      integer(ip)           ndata
      integer(ip)           nelt
      integer(ip)           nlink
      integer(ip)           nrows
      integer(ip)           nulptr
      integer(ip)           p
      integer(ip)           p2
      integer(ip)           page   ( pgsizi )
      integer(ip)           pbase
      integer(ip)           remain
      integer(ip)           row
      integer(ip)           size
      integer(ip)           to

      logical               cntinu
      logical               fixsiz
      logical               newreq
      logical               nullok

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Grab the column's attributes.
!
      class  =  coldsc( clsidx )
      nulptr =  coldsc( nflidx )
      colidx =  coldsc( ordidx )
      size   =  coldsc( sizidx )

      nullok =  nulptr /= ifalse
      fixsiz =  size   /= ifalse

!
!     This column had better be class 4.
!
      if ( class /= 4 ) then

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 4.'                 )
         call errint ( '#', class                                      )
         call errch  ( '#', column                                     )
         call sigerr ( 'SPICE(NOCLASS)'                                )
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
!     Record the number of data values to write.
!
      if ( nullok ) then
!
!        Sum the sizes of the non-null column entries; these are the
!        ones that will take up space.
!
         ndata  =  0

         do i = 1, nrows

            if ( .not. nlflgs(i) ) then

               if ( fixsiz ) then
                  ndata  =  ndata + size
               else
                  ndata  =  ndata + entszs(i)
               end if

            end if

         end do


      else


         if ( fixsiz ) then

            ndata  =  nrows * size

         else

            ndata  =  0

            do i = 1, nrows
               ndata  =  ndata + entszs(i)
            end do

         end if


      end if


      if ( ndata > 0 ) then
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         call zzekaps  ( handle, segdsc, int, .false., p, pbase )
         call cleari   ( pgsizi, page )

      end if

!
!     Write the input data out to the target file a page at a time.
!     Null values don't get written.
!
!     While we're at it, we'll push onto the EK stack the addresses
!     of the column entries.  We use the constant NULL rather than an
!     address to represent null entries.
!
!     We'll use FROM to indicate the element of IVALS we're
!     considering, TO to indicate the element of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.  The variable NELT is the count of the column entry
!     elements written for the current entry.  The variable N indicates
!     the number of integers written to the current page.
!

      remain  =  ndata
      from    =  1
      to      =  1
      bufptr  =  1
      row     =  1
      nelt    =  1
      n       =  0
      nlink   =  0


      do while ( row <= nrows )

!
!        NEWREQ is set to TRUE if we discover that the next column
!        entry must start on a new page.
!
         newreq  =  .false.


         if (  nullok  .and.  ( nlflgs(row) )  ) then


            if ( fixsiz ) then
               cursiz  =  size
            else
               cursiz  =  entszs(row)
            end if


            from           =  from   +  cursiz
            adrbuf(bufptr) =  null
            bufptr         =  bufptr +  1
            row            =  row    +  1
            nelt           =  1
            cntinu         =  .false.


         else


            if ( nelt == 1 ) then
!
!              We're about to write out a new column entry.  We must
!              insert the element count into the page before writing the
!              data.  The link count for the current page must be
!              incremented to account for this new entry.
!
!              At this point, we're guaranteed at least two free
!              spaces in the current page.
!
               if ( fixsiz ) then
                  cursiz  =  size
               else
                  cursiz  =  entszs(row)
               end if

               adrbuf(bufptr) =  to     +  pbase
               bufptr         =  bufptr + 1
               page(to)       =  cursiz
               to             =  to     + 1
               n              =  n      + 1
               nlink          =  nlink  + 1

            end if

!
!           At this point, there's at least one free space in the
!           current page.
!
            page(to)  =  ivals(from)
            to        =  to      +  1
            n         =  n       +  1
            from      =  from    +  1
            remain    =  remain  -  1

!
!           Decide whether we must continue the current entry on another
!           data page.
!
            cntinu  =  ( nelt < cursiz ) .and. ( n == ipsize )

            if ( nelt == cursiz ) then
!
!              The current element is the last of the current column
!              entry.
!
!              Determine whether we must start the next column entry on
!              a new page.  To start a column entry on the current page,
!              we must have enough room for the element count and at
!              least the first entry element.
!
               if ( remain > 0 ) then
                  newreq   =   n  > ( ipsize - 2 )
               end if


               nelt  =  1
               row   =  row   +  1

            else

               nelt  =  nelt  +  1

            end if

         end if


         if (  ( bufptr > bufsiz ) .or. ( row > nrows )  ) then
!
!           The address buffer is full or we're out of input values
!           to look at, so push the buffer contents on the stack.
!
            call zzekspsh ( bufptr-1, adrbuf )
            bufptr  =  1

         end if


         if (         cntinu &
             .or.     newreq &
             .or. ( ( row > nrows ) .and. ( ndata > 0 ) )  ) then

!
!           It's time to write out the current page.  First set the link
!           count.
!
            page ( ilcidx ) =  nlink

!
!           Write out the data page.
!
            call zzekpgwi ( handle, p, page  )

!
!           If there's more data to write, allocate another page.
!
            if ( remain > 0 ) then

               call zzekaps ( handle, segdsc, int, .false., p2, pbase )
               call cleari  ( pgsizi, page )

               n      =  0
               nlink  =  0
               to     =  1

!
!              If we're continuing an element from the previous page,
!              link the previous page to the current one.
!
               if ( cntinu ) then
                  call zzeksfwd ( handle, int, p, p2 )
               end if

               p   =  p2

            end if
!
!           We've allocated a new data page if we needed one.
!

         end if
!
!        We've written out the last completed data page.
!

      end do
!
!     We've processed all entries of the input array.
!

      return
      end
