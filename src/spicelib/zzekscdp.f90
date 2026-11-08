!$Procedure      ZZEKSCDP ( EK, set column data pointer )

      subroutine zzekscdp ( handle, segdsc, coldsc, recptr, datptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the data pointer for a specified EK column entry.
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
      include 'eksegdsc.inc'
      include 'ekrecptr.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      integer(ip)           datptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     DATPTR     I   Data pointer of column entry.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment containing
!                    the specified column entry.
!
!     COLDSC         is the descriptor of the column containing
!                    the specified column entry.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry whose data pointer is to be set.
!
!     DATPTR         is the data pointer of the specified column entry.
!                    When DATPTR is positive, it represents a pointer
!                    to a data value.  The interpretation of the
!                    pointer depends on the class of the column entry.
!                    DATPTR may also take on the distinguished values
!
!                       UNINIT  (indicated uninitialized entry)
!                       NULL    (indicated null entry)
!                       NOBACK  (indicated uninitialized backup entry)
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the action of this
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
!     This routine hides details of column entry data pointer access.
!     The inverse of this routine is ZZEKGCDP.
!
! $ Examples
!
!     1)  Set a column's data pointer to indicate that a column entry
!         is uninitialized.  The parameter UNINIT is defined in
!         ekrecptr.inc
!
!           CALL ZZEKSCDP ( HANDLE, SEGDSC, COLDSC, RECPTR, UNINIT )
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
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
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
      integer(ip)           colidx
      integer(ip)           ncols
      integer(ip)           ptrloc
      integer(ip)           recno

!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx  )
      colidx =  coldsc ( ordidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call errint ( '#',  segdsc(snoidx)                    )
         call errint ( '#',  recno                             )
         call errhan ( '#',  handle                            )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     Compute the data pointer location, and set the pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasudi ( handle, ptrloc, ptrloc, datptr )

      return
      end
