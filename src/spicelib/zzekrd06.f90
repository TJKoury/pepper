!$Procedure   ZZEKRD06 ( EK, read class 6 column entry elements )

      subroutine zzekrd06 ( handle,  segdsc,  coldsc,  recptr, &
                            beg,     end,     cvals,   isnull,  found )
      use pepper_kinds, only: ip

      implicit none

! $ Abstract
!
!     Read a specified element range from a column entry in a specified
!     record in a class 6 column.  Class 6 columns have character arrays
!     as column entries.
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
      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      integer(ip)           beg
      integer(ip)           end
      character(*)          cvals  ( * )
      logical               isnull
      logical               found

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     BEG        I   Start element index.
!     END        I   End element index.
!     CVALS      O   Character values in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
!     FOUND      O   Flag indicating whether elements were found.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.
!
!     SEGDSC         is the descriptor of the segment from which data is
!                    to be read.
!
!     COLDSC         is the descriptor of the column from which data is
!                    to be read.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to be written.
!
!     BEG,
!     END            are, respectively, the start and end indices of
!                    the contiguous range of elements to be read from
!                    the specified column entry.
!
! $ Detailed_Output
!
!     CVALS          are the values read from the specified column
!                    entry.  The mapping of elements of the column entry
!                    to elements of CVALS is as shown below:
!
!                       Column entry element       CVALS element
!                       --------------------       -------------
!                       BEG                        1
!                       BEG+1                      2
!                       .                          .
!                       .                          .
!                       .                          .
!                       END                        END-BEG+1
!
!                    CVALS must have sufficient string length to hold
!                    the longest returned string value.  Entries that
!                    are shorter than the string length of CVALS are
!                    padded with trailing blanks.
!
!                    CVALS is valid only if the output argument
!                    FOUND is returned .TRUE.
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.  ISNULL is set on output whether or not
!                    the range of elements designated by BEG and END
!                    exists.
!
!     FOUND          is a logical flag indicating whether the range
!                    of elements designated by BEG and END exists.
!                    If the number of elements in the specified column
!                    entry is not at least END, FOUND will be returned
!                    .FALSE.
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
!     2)  If the specified column entry has not been initialized, the
!         error SPICE(UNINITIALIZED) is signaled.
!
!     3)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If the string length of CVALS is shorter than the declared
!         string length of the specified column, the error
!         SPICE(STRINGTRUNCATED) is signaled.
!
!     5)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility for reading data from class 6 columns.
!
! $ Examples
!
!     See EKRCEC.
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
!-    SPICELIB Version 1.2.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).  Error messages were enhanced so
!        as to use column names rather than indices.
!
!-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).  Error messages were enhanced so
!        as to use column names rather than indices.
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
!     Local variables
!
      character(cnamsz)     column

      integer(ip)           avail
      integer(ip)           base
      integer(ip)           colidx
      integer(ip)           cvlen
      integer(ip)           d
      integer(ip)           datptr
      integer(ip)           delta
      integer(ip)           eltidx
      integer(ip)           eltoff
      integer(ip)           maxelt
      integer(ip)           ncols
      integer(ip)           nelt
      integer(ip)           nread
      integer(ip)           nskip
      integer(ip)           offset
      integer(ip)           p
      integer(ip)           pagnum
      integer(ip)           pg
      integer(ip)           ptrloc
      integer(ip)           ptroff
      integer(ip)           recno
      integer(ip)           remain
      integer(ip)           start
      integer(ip)           strlen

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx  )
      colidx =  coldsc ( ordidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     Make sure the output buffer is wide enough to hold the returned
!     strings.
!
      cvlen   =  len    ( cvals(1) )
      strlen  =  coldsc ( lenidx   )

      if ( strlen > cvlen ) then
!
!        We have a string truncation error.  Look up the column
!        name, record number, and file name before signaling an
!        error.
!
         call zzekcnam ( handle, coldsc, column )

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'String value has length #; output string '// &
                       'can hold only # characters.  COLUMN = #; '// &
                       'SEGNO = #; RECNO = #; EK = #'              )
         call errint ( '#',  strlen                                )
         call errint ( '#',  cvlen                                 )
         call errch  ( '#',  column                                )
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  recno                                 )
         call errhan ( '#',  handle                                )
         call sigerr ( 'SPICE(STRINGTRUNCATED)'                    )
         return

      end if

!
!     Compute the data pointer location, and read the pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( handle, ptrloc, ptrloc, datptr )

      if ( datptr > 0 ) then
!
!        The entry is non-null.
!
         isnull  =  .false.

!
!        Get the element count.  Check for range specifications that
!        can't be met.
!
         call zzekgei ( handle, datptr, nelt )

         if (  ( beg < 1 ) .or. ( beg > nelt )  )  then

            found  =  .false.
            return

         else if (  ( end < 1 ) .or. ( end > nelt )  )  then

            found  =  .false.
            return

         else if ( end < beg ) then

            found  =  .false.
            return

         end if

!
!        The request is valid, so read the data.  The first step is to
!        locate the element at index BEG.  We'll first decide on which
!        page the desired element starts.  The first page holds up to
!        CPSIZE - ENCSIZ characters; the rest hold CPSIZE characters.
!        While we're at it, we'll compute the offset ELTOFF of the
!        element from the base of the page on which the element starts.
!        We'll use the name OFFSET to represent the character offset
!        of the element from the base of the page on which the column
!        entry starts.
!
         call zzekpgpg ( chr, datptr, p, base )

         ptroff  =  datptr  -  base
         offset  =  ptroff  +  encsiz   +   strlen * ( beg - 1 )

         if ( offset <= cpsize ) then

            pagnum   =    1
            eltoff   =    offset

         else

            pagnum   =  ( offset  +  cpsize   -   1 ) / cpsize
            eltoff   =    offset  -    ( pagnum - 1 ) * cpsize

         end if

!
!        Get the absolute page number and base address of the page
!        on which the element starts.  If this is not the page on
!        which the column entry starts, we'll chain along using
!        the page's forward links until we arrive at the correct page.
!
         pg  =  1

         do while ( pg < pagnum )
!
!           Get the link to the next page, then look up the base
!           address of that page.
!
            call zzekgei  ( handle, base+cfpidx, p    )
            call zzekpgbs ( chr,    p,           base )

            pg  =  pg + 1

         end do

!
!        The desired element starts at address BASE + ELTOFF.
!
         datptr  =  base + eltoff

!
!        At this point, P is the page on which the element having index
!        BEG is located.  BASE is the base address of this page.
!
!        Read the strings one at a time.
!
         eltidx   =  1
         maxelt   =  end - beg + 1

         do while (  ( eltidx <= maxelt ) .and. ( .not. failed() )  )
!
!           Read the current string.  The string may be continued over
!           multiple pages.  Read only as many characters as will fit
!           in the output buffer element CVALS(ELTIDX).
!
            remain  =  min ( cvlen, strlen )
            start   =  1

            do while (  ( remain > 0 ) .and. ( .not. failed() )  )

               avail  =  base + cpsize - datptr + 1
               nread  =  min  ( remain,  avail )

               if ( nread > 0 ) then

                  call dasrdc ( handle, &
                                datptr, &
                                datptr + nread - 1, &
                                start, &
                                start + nread - 1, &
                                cvals ( eltidx )    )

                  start  =  start  + nread
                  remain =  remain - nread
                  datptr =  datptr + nread

               else
!
!                 Go to the next page for the continuation of the
!                 current string.
!
                  call zzekgei  ( handle, base+cfpidx, p    )
                  call zzekpgbs ( chr,    p,           base )

                  datptr  =  base   +  1

               end if

            end do

!
!           If we did not read all of the current array element,
!           we'll need to advance DATPTR past the end of the element.
!           If this advance moved DATPTR beyond the last character
!           of the current page, the logic above will set DATPTR to
!           indicate the first character of the next continuation page.
!
            delta  =  strlen - cvlen

            if ( delta > 0 ) then

               d  =  delta

               do while ( d > 0 )

                  avail  =  base + cpsize - datptr + 1
                  nskip  =  min  ( d,  avail )

                  if ( nskip > 0 ) then

                     d      =  d      - nskip
                     datptr =  datptr + nskip

                  else
!
!                    Go to the next page for the continuation of the
!                    current string.
!
                     call zzekgei  ( handle, base+cfpidx, p    )
                     call zzekpgbs ( chr,    p,           base )

                     datptr  =  base   +  1

                  end if

               end do

            end if

!
!           Blank-pad the output string if necessary.
!
            if ( cvlen > strlen ) then
               cvals ( eltidx ) ( strlen+1: ) = ' '
            end if

            eltidx  =  eltidx + 1

         end do

         found =  .not. failed()

      else if ( datptr == null ) then
!
!        The value is null.
!
         isnull =  .true.
         found  =  .true.

      else if ( datptr == uninit ) then
!
!        The data value is absent.  This is an error.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call zzekcnam ( handle,   coldsc, column )

         call setmsg ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLUMN = #; RECNO = #; ' // &
                       'EK = #'                                      )
         call errint ( '#',  segdsc(snoidx)                          )
         call errch  ( '#',  column                                  )
         call errint ( '#',  recno                                   )
         call errhan ( '#',  handle                                  )
         call sigerr ( 'SPICE(UNINITIALIZED)'                        )
         return

      else
!
!        The data pointer is corrupted.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call zzekcnam ( handle,   coldsc, column )

         call setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLUMN =  #; RECNO = #; EK = #'          )
         call errint ( '#',  segdsc(snoidx)                      )
         call errch  ( '#',  column                              )
         call errint ( '#',  recno                               )
         call errhan ( '#',  handle                              )
         call sigerr ( 'SPICE(BUG)'                              )
         return

      end if

      return
      end
