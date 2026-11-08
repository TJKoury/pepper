!$Procedure ZZEKSZ06 ( EK, element entry size, class 6 )

      integer(ip) function zzeksz06 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the size of a specified entry in a class 6 column.
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
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           recptr
      integer(ip)           coldsc ( cdscsz )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!
!     The function returns the number of elements in the specified
!     column entry.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column specified by COLDSC.
!
!     COLDSC         is the column descriptor of the column containing
!                    the entry whose size is requested.  The column
!                    must be class 6.
!
!     RECPTR         is a pointer to the EK record containing the
!                    column entry of interest.
!
! $ Detailed_Output
!
!     The function returns the number of elements in the specified
!     column entry.
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
!     2)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
!     3)  If the column index contained in the input column descriptor
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This utility supports the commonly performed function of
!     determining the element count of a column entry.
!
! $ Examples
!
!     See ZZEKESIZ.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&


!
!     Local variables
!
      integer(ip)           colidx
      integer(ip)           datptr
      integer(ip)           ncols
      integer(ip)           nrec
      integer(ip)           ptrloc

!
!     Use discovery check-in.
!
!     Initialize the function's return value.
!
      zzeksz06  =  0

      nrec    =  segdsc ( nridx  )
      colidx  =  coldsc ( ordidx )

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call chkin  ( 'ZZEKSZ06'                              )
         call setmsg ( 'Column index = #; valid range is 1:#.' )
         call errint ( '#',  colidx                            )
         call errint ( '#',  nrec                              )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         call chkout ( 'ZZEKSZ06'                              )
         return

      end if

!
!     If the column has fixed-size entries, just return the declared
!     size.
!
      if ( coldsc(sizidx) /= ifalse ) then

         zzeksz06  =  coldsc(sizidx)
         return

      end if

!
!     Compute the data pointer location.  Read the data pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( handle, ptrloc, ptrloc, datptr )


      if ( datptr < 1 ) then
!
!        The value is null.  Null entries are always considered to have
!        size 1.
!
         zzeksz06  =  1

      else
!
!        DATPTR points to the element count.
!
         call zzekgei ( handle, datptr, zzeksz06 )

      end if

      return
      end
