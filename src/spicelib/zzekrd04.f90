!$Procedure   ZZEKRD04 ( EK, read class 4 column entry elements )

      subroutine zzekrd04 ( handle,  segdsc,  coldsc,  recptr, &
                            beg,     end,     ivals,   isnull,  found )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Read a specified element range from a column entry in a specified
!     record in a class 4 column.  Class 4 columns have integer arrays
!     as column entries.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!     UTILITY
!
! $ Declarations

      implicit none

      include 'ekbool.inc'
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
      integer(ip)           ivals  ( * )
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
!     IVALS      O   Integer values in column entry.
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
!     IVALS          are the values read from the specified column
!                    entry.  The mapping of elements of the column entry
!                    to elements of IVALS is as shown below:
!
!                       Column entry element       IVALS element
!                       --------------------       -------------
!                       BEG                        1
!                       BEG+1                      2
!                       .                          .
!                       .                          .
!                       .                          .
!                       END                        END-BEG+1
!
!                    IVALS is valid only if the output argument
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
!         error SPICE(UNINITIALIZEDVALUE) is signaled.
!
!     3)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If an I/O error occurs while reading the indicated file,
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
!     This routine is a utility for reading data from class 4 columns.
!
! $ Examples
!
!     See EKRCEI.
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
!-    SPICELIB Version 1.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in ZZEKGFWD call.
!
!-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in ZZEKGFWD call.
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
      integer(ip)           base
      integer(ip)           colidx
      integer(ip)           datptr
      integer(ip)           maxidx
      integer(ip)           minidx
      integer(ip)           ncols
      integer(ip)           nelt
      integer(ip)           nread
      integer(ip)           p
      integer(ip)           ptemp
      integer(ip)           ptrloc
      integer(ip)           recno
      integer(ip)           remain
      integer(ip)           start

!
!     Use discovery check-in.
!

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
         call dasrdi ( handle, datptr, datptr, nelt )

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
!        locate the element at index BEG.
!
         call zzekpgpg ( int, datptr, p, base )

         minidx  =  1
         maxidx  =  base   + ipsize - datptr
         datptr  =  datptr + beg


         do while ( maxidx < beg )
!
!           Locate the page on which the element is continued.
!
            call dasrdi ( handle, base+ifpidx, base+ifpidx, p )

!
!           Determine the highest-indexed element of the column entry
!           located on the current page.
!
            call zzekpgbs ( int, p, base )

            minidx  =  maxidx  +   1
            maxidx  =  min ( maxidx + ipsize, nelt )

!
!           The following assignment will set DATPTR to the correct
!           value on the last pass through this loop.
!
            datptr  =  base    +   1   +  ( beg - minidx )

         end do

!
!        At this point, P is the page on which the element having index
!        BEG is located.  BASE is the base address of this page.
!        MAXIDX is the highest index of any element on the current page.
!
         remain  =  end    -   beg   +   1
         start   =  1

!
!        Decide how many elements to read from the current page, and
!        read them.
!
         nread   =  min ( remain,  ( base + ipsize - datptr + 1 )  )

         call dasrdi ( handle, datptr, datptr+nread-1, ivals(start) )

         remain  =  remain  -  nread


         do while (  ( remain > 0 ) .and.  ( .not. failed() )  )
!
!           Locate the page on which the element is continued.
!
            call zzekgfwd ( handle, int, p, ptemp )
            p = ptemp

            call zzekpgbs ( int,         p, base  )

            datptr  =  base   +  1
            start   =  start  +  nread
            nread   =  min ( remain,  ipsize )

            call dasrdi ( handle, datptr, datptr+nread-1, ivals(start) )

            remain  =  remain  -  nread

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

         call setmsg ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLIDX = #; RECNO = #; ' // &
                       'EK = #'                                      )
         call errint ( '#',  segdsc(snoidx)                          )
         call errint ( '#',  colidx                                  )
         call errint ( '#',  recno                                   )
         call errhan ( '#',  handle                                  )
         call sigerr ( 'SPICE(UNINITIALIZEDVALUE)'                   )
         return


      else
!
!        The data pointer is corrupted.
!
         call setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         call errint ( '#',  segdsc(snoidx)                      )
         call errint ( '#',  colidx                              )
         call errint ( '#',  recno                               )
         call errhan ( '#',  handle                              )
         call sigerr ( 'SPICE(BUG)'                              )
         return

      end if


      return
      end
