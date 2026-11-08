!$Procedure   ZZEKRD05 ( EK, read class 5 column entry elements )

      subroutine zzekrd05 ( handle,  segdsc,  coldsc,  recptr, &
                            beg,     end,     dvals,   isnull,  found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a specified element range from a column entry in a specified
!     record in a class 5 column.  Class 5 columns have d.p. arrays
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
      real(wp)              dvals  ( * )
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
!     DVALS      O   Double precision values in column entry.
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
!     DVALS          are the values read from the specified column
!                    entry.  The mapping of elements of the column entry
!                    to elements of DVALS is as shown below:
!
!                       Column entry element       DVALS element
!                       --------------------       -------------
!                       BEG                        1
!                       BEG+1                      2
!                       .                          .
!                       .                          .
!                       .                          .
!                       END                        END-BEG+1
!
!                    DVALS is valid only if the output argument
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
!     This routine is a utility for reading data from class 5 columns.
!
! $ Examples
!
!     See EKRCED.
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
!-    SPICELIB Version 1.1.0, 12-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in ZZEKGFWD calls.
!
!-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 12-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in ZZEKGFWD calls.
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
      real(wp)              dpnelt

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

         call chkin  ( 'ZZEKRD05'                              )
         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         call chkout ( 'ZZEKRD05'                              )
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
         call dasrdd ( handle, datptr, datptr, dpnelt )

         nelt  =  nint ( dpnelt )


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
         call zzekpgpg ( dp, datptr, p, base )

         minidx  =  1
         maxidx  =  base   + dpsize - datptr
         datptr  =  datptr + beg


         do while ( maxidx < beg )
!
!           Locate the page on which the element is continued.
!
            call zzekgfwd ( handle, dp, p, ptemp )
            p = ptemp

            call zzekpgbs ( dp,         p, base  )

!
!           Determine the highest-indexed element of the column entry
!           located on the current page.
!
            minidx  =  maxidx  +   1
            maxidx  =  min ( maxidx + dpsize, nelt )

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
         nread   =  min ( remain,  ( base + dpsize - datptr + 1 )  )

         call dasrdd ( handle, datptr, datptr+nread-1, dvals(start) )

         remain  =  remain  -  nread


         do while (  ( remain > 0 ) .and.  ( .not. failed() )  )
!
!           Locate the page on which the element is continued.
!
            call zzekgfwd ( handle, dp, p, ptemp )
            p = ptemp

            call zzekpgbs ( dp,         p, base  )

            datptr  =  base   +  1
            start   =  start  +  nread
            nread   =  min ( remain,  dpsize )

            call dasrdd ( handle, datptr, datptr+nread-1, dvals(start) )

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

         call chkin  ( 'ZZEKRD05'                                    )
         call setmsg ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLIDX = #; RECNO = #; ' // &
                       'EK = #'                                      )
         call errint ( '#',  segdsc(snoidx)                          )
         call errint ( '#',  colidx                                  )
         call errint ( '#',  recno                                   )
         call errhan ( '#',  handle                                  )
         call sigerr ( 'SPICE(UNINITIALIZEDVALUE)'                   )
         call chkout ( 'ZZEKRD05'                                    )
         return


      else
!
!        The data pointer is corrupted.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call chkin  ( 'ZZEKRD05'                                )
         call setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         call errint ( '#',  segdsc(snoidx)                      )
         call errint ( '#',  colidx                              )
         call errint ( '#',  recno                               )
         call errhan ( '#',  handle                              )
         call sigerr ( 'SPICE(BUG)'                              )
         call chkout ( 'ZZEKRD05'                                )
         return

      end if


      return
      end
