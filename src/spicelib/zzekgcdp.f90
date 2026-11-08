!$Procedure      ZZEKGCDP ( EK, get column data pointer )

      subroutine zzekgcdp ( handle, segdsc, coldsc, recptr, datptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the data pointer for a specified EK column entry.
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
      include 'ektype.inc'

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
!     DATPTR     O   Data pointer of column entry.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     SEGDSC         is the descriptor of the segment containing
!                    the specified column entry.
!
!     COLDSC         is the descriptor of the column containing
!                    the specified column entry.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry whose data pointer is desired.
!
!
! $ Detailed_Output
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
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine hides details of column entry data pointer access.
!
! $ Examples
!
!     See ZZEKRFIL.
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
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
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
      integer(ip)           ncols
      integer(ip)           ptrloc
      integer(ip)           recno

!
!     Use discovery check-in.
!

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
!     Compute the data pointer location, and read the pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( handle, ptrloc, ptrloc, datptr )

      return
      end
