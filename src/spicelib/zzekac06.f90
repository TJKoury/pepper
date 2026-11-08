!$Procedure     ZZEKAC06 ( EK, add class 6 column to segment )

      subroutine zzekac06 (  handle,  segdsc,  coldsc, &
                             cvals,   entszs,  nlflgs )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add an entire class 6 column to an EK segment.
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
      character(*)          cvals  ( * )
      integer(ip)           entszs ( * )
      logical               nlflgs ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     CVALS      I   Character values to add to column.
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
!     CVALS          is an array containing the entire set of column
!                    entries for the specified column.  The entries
!                    are listed in row-order:  the column entry for the
!                    first row of the segment is first, followed by the
!                    column entry for the second row, and so on.  The
!                    number of column entries must match the declared
!                    number of rows in the segment.  For columns having
!                    fixed-size entries, a null entry must be allocated
!                    the same amount of space occupied by a non-null
!                    entry in the array CVALS.  For columns having
!                    variable-size entries, null entries do not require
!                    any space in the CVALS array, but in any case must
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
!                    defined by CVALS is added to the specified segment
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
!     See EKACLC.
!
! $ Restrictions
!
!     1)  This routine assumes the EK scratch area has been set up
!         properly for a fast load operation.  This routine writes
!         to the EK scratch area as well.
!
!     2)  Currently, the EK system can handle only one fast load
!         at at time---one segment created by a fast load must be
!         be completed by a call to EKFFLD before another segment
!         can be created by a fast load, even if the two segments
!         reside in different files.
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
      integer(ip), parameter :: bufsiz = cpsize

!
!     Local variables
!
      character(cnamsz)     column
      character(pgsizc)     page

      integer(ip)           adrbuf ( bufsiz )
      integer(ip)           bufptr
      integer(ip)           class
      integer(ip)           colidx
      integer(ip)           cp
      integer(ip)           curchr
      integer(ip)           cursiz
      integer(ip)           cvlen
      integer(ip)           from
      integer(ip)           i
      integer(ip)           l
      integer(ip)           n
      integer(ip)           nchars
      integer(ip)           ndata
      integer(ip)           nlink
      integer(ip)           nrows
      integer(ip)           nulptr
      integer(ip)           nw
      integer(ip)           nwrite
      integer(ip)           p
      integer(ip)           p2
      integer(ip)           padlen
      integer(ip)           pbase
      integer(ip)           remain
      integer(ip)           room
      integer(ip)           row
      integer(ip)           size
      integer(ip)           strlen
      integer(ip)           to

      logical               cntinu
      logical               fixsiz
      logical               newent
      logical               newreq
      logical               nullok
      logical               pad

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
      strlen =  coldsc( lenidx )

      nullok =  nulptr /= ifalse
      fixsiz =  size   /= ifalse

!
!     This column had better be class 6.
!
      if ( class /= 6 ) then

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 6.'                 )
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
                  ndata  =  ndata   +  strlen * size
               else
                  ndata  =  ndata   +  strlen * entszs(i)
               end if

            end if

         end do

      else

         if ( fixsiz ) then

            ndata  =  nrows * strlen * size

         else

            ndata  =  0

            do i = 1, nrows
               ndata  =  ndata   +  strlen * entszs(i)
            end do

         end if

      end if

      if ( ndata > 0 ) then
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         call zzekaps  ( handle, segdsc, chr, .false., p, pbase )
         page  =  ' '

!
!        Decide now whether we will need to pad the input entry
!        elements with trailing blanks, and if so how much padding
!        we'll need.
!
         cvlen   =  min (  len( cvals(1) ),  strlen  )
         pad     =  cvlen  <  strlen

         if ( pad ) then
            padlen  =  strlen - cvlen
         end if

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
!     considering, TO to indicate the element of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.   The variable N indicates the number of characters
!     written to the current page.  NCHARS is the number of characters
!     written in the current column entry. CP is the position in the
!     current input string of the character which we'll read next.
!

      remain  =  ndata
      from    =  1
      to      =  1
      bufptr  =  1
      row     =  1
      cp      =  1
      n       =  0
      nchars  =  0
      nlink   =  0
      newent  =  .true.

      do while ( row <= nrows )
!
!        NEWREQ is set to TRUE if we discover that the next column
!        entry must start on a new page.
!
         newreq  =  .false.

!
!        FROM and TO are expected to be properly set at this point.
!

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
            cntinu         =  .false.
            newent         =  .true.

         else

            if ( newent ) then
!
!              We're about to write out a new column entry.  We must
!              insert the element count into the page before writing the
!              data.  The link count for the current page must be
!              incremented to account for this new entry.
!
!              At this point, we're guaranteed at least ENCSIZ+1 free
!              spaces in the current page.
!
               if ( fixsiz ) then
                  cursiz  =  size
               else
                  cursiz  =  entszs(row)
               end if

               curchr         =  cursiz *  strlen
               nchars         =  0
               cp             =  1

               adrbuf(bufptr) =  to     +  pbase
               bufptr         =  bufptr + 1

               call prtenc (  cursiz,  page( to : to+encsiz-1 )  )

               to             =  to     + encsiz
               n              =  n      + encsiz
               nlink          =  nlink  + 1
               newent         =  .false.

            end if

!
!           At this point, there's at least one free space in the
!           current page.  There's also at least one character to
!           write.  Transfer as much as possible of the current
!           column entry to the current page.
!
            room     =  cpsize - n
            nwrite   =  min ( curchr-nchars, room )
            nw       =  nwrite

            do while ( nw > 0 )
!
!              At this point, we're guaranteed that
!
!                 CP      <=  STRLEN
!                 TO      <   CPSIZE
!                 FROM is set correctly.
!
               if ( pad ) then
!
!                 The input strings must be padded with blanks up to
!                 a length of STRLEN characters.  The number of blanks
!                 used to pad the input is PADLEN.
!
                  if ( cp < cvlen ) then
!
!                    Compute the number of `actual' characters of data
!                    left in the current input string.
!
!                    Transfer the characters we have room for from the
!                    current input string to the current page.
!
                     l                    =  cvlen - cp + 1
                     l                    =  min ( l, nw  )

                     page( to : to+l-1 )  =  cvals(from)( cp : cp+l-1 )
                     cp                   =  cp  + l
                     nw                   =  nw  - l
                     to                   =  to  + l

                  else
!
!                    The input character pointer is in the `pad' zone.
!                    Let L be the length of padding that is required
!                    and can fit in the page.
!
                     l                    =  strlen - cp + 1
                     l                    =  min ( l, nw )

                     page( to : to+l-1 )  =  ' '
                     cp                   =  cp  + l
                     nw                   =  nw  - l
                     to                   =  to  + l

                  end if

               else
!
!                 The input data doesn't require padding.
!
!                 Compute the number of `actual' characters of data
!                 left in the current input string.
!
!                 Transfer the characters we have room for from the
!                 current input string to the current page.
!
                  l                    =  strlen - cp + 1
                  l                    =  min ( l, nw )

                  page( to : to+l-1 )  =  cvals(from)( cp : cp+l-1 )
                  cp                   =  cp + l
                  nw                   =  nw - l
                  to                   =  to + l

               end if

!
!              If the input pointer is beyond the end of the declared
!              length of the target column's strings STRLEN, it's time
!              to look at the next input string.
!
               if ( cp > strlen ) then
                  from  =  from + 1
                  cp    =  1
               end if

            end do

!
!           We've written NWRITE characters to the current page.  FROM,
!           TO, and CP are set.
!
            n         =  n       +  nwrite
            remain    =  remain  -  nwrite
            nchars    =  nchars  +  nwrite

!
!           Decide whether we must continue the current entry on another
!           data page.
!
            cntinu  =  ( nchars < curchr ) .and. ( n == cpsize )

!
!           If we've finished writing out a column entry, get ready
!           to write the next one.
!
            if ( nchars == curchr ) then
!
!              The current character is the last of the current column
!              entry.
!
!              Determine whether we must start the next column entry on
!              a new page.  To start a column entry on the current page,
!              we must have enough room for the element count and at
!              least one character of data.
!
               if ( remain > 0 ) then
                  newreq   =   n  > ( cpsize - encsiz - 1 )
               end if

               row     =   row   +  1
               newent  =  .true.

            end if

         end if

!
!        At this point, CNTINU indicates whether we need to continue
!        the current entry on another page.  If we finished writing out
!        the entry, CNTINU is .FALSE.
!

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
            call prtenc (  nlink,  page ( clcidx : clcidx+encsiz-1 )  )

!
!           Write out the data page.
!
            call zzekpgwc ( handle, p, page  )

!
!           If there's more data to write, allocate another page.
!
            if ( remain > 0 ) then

               call zzekaps ( handle, segdsc, chr, .false., p2, pbase )
               page   = ' '

               n      =  0
               nlink  =  0
               to     =  1

!
!              If we're continuing an element from the previous page,
!              link the previous page to the current one.
!
               if ( cntinu ) then
                  call zzeksfwd ( handle, chr, p, p2 )
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
