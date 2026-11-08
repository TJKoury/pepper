!$Procedure   ZZEKRD02 ( EK, read class 2 column entry )

      subroutine zzekrd02 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a column entry from a specified record in a class 2 column.
!     Class 2 columns contain scalar double precision values.
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
      real(wp)              dval
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     DVAL       O   Double precision value in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
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
! $ Detailed_Output
!
!     DVAL           is the value read from the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.
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
!     This routine is a utility for reading data from class 2 columns.
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
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
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
      integer(ip)           ncols
      integer(ip)           ptrloc
      integer(ip)           recno

!
!     Use discovery check-in.
!
!
!     Make sure the column exists.
!
      ncols   =  segdsc ( ncidx  )
      colidx  =  coldsc ( ordidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call chkin  ( 'ZZEKRD02'                              )
         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         call chkout ( 'ZZEKRD02'                              )
         return

      end if

!
!     Compute the data pointer location, and read the pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( handle, ptrloc, ptrloc, datptr )


      if ( datptr > 0 ) then
!
!        Just read the value.
!
         call dasrdd ( handle, datptr, datptr, dval )
         isnull  =  .false.


      else if ( datptr == null ) then
!
!        The value is null.
!
         isnull =  .true.


      else if (       ( datptr == uninit ) &
                 .or. ( datptr == noback )  )  then
!
!        The data value is absent.  This is an error.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call chkin  ( 'ZZEKRD02'                                    )
         call setmsg ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLIDX = #; RECNO = #; ' // &
                       'EK = #'                                      )
         call errint ( '#',  segdsc(snoidx)                          )
         call errint ( '#',  colidx                                  )
         call errint ( '#',  recno                                   )
         call errhan ( '#',  handle                                  )
         call sigerr ( 'SPICE(UNINITIALIZEDVALUE)'                   )
         call chkout ( 'ZZEKRD02'                                    )
         return


      else
!
!        The data pointer is corrupted.
!
         call chkin  ( 'ZZEKRD02'                                )
         call setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         call errint ( '#',  segdsc(snoidx)                      )
         call errint ( '#',  colidx                              )
         call errint ( '#',  recno                               )
         call errhan ( '#',  handle                              )
         call sigerr ( 'SPICE(BUG)'                              )
         call chkout ( 'ZZEKRD02'                                )
         return

      end if


      return
      end
