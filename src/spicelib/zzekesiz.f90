!$Procedure ZZEKESIZ ( EK, element entry size )

      integer(ip) function zzekesiz ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the size of a specified column entry.
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
      include 'ekcnamsz.inc'
      include 'eksegdsc.inc'

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
!                    the entry whose size is requested.
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This utility centralizes the commonly performed function of
!     determining the element count of a column entry.
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
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error message.
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekrp2n
      integer(ip)           zzeksz04
      integer(ip)           zzeksz05
      integer(ip)           zzeksz06

!
!     Local variables
!
      character(cnamsz)     column

      integer(ip)           class
      integer(ip)           recno
      integer(ip)           segno

!
!     Initialize the function's return value.
!
      zzekesiz  =  0

!
!     Delegate the problem to the routine of the appropriate class.
!     The first three classes are scalars.
!
      class  =  coldsc ( clsidx )

      if ( class == 1 ) then

         zzekesiz  =  1

      else if ( class == 2 ) then

         zzekesiz  =  1

      else if ( class == 3 ) then

         zzekesiz  =  1

      else if ( class == 4 ) then

         zzekesiz  =  zzeksz04 ( handle, segdsc, coldsc, recptr )

      else if ( class == 5 ) then

         zzekesiz  =  zzeksz05 ( handle, segdsc, coldsc, recptr )

      else if ( class == 6 ) then

         zzekesiz  =  zzeksz06 ( handle, segdsc, coldsc, recptr )

      else if ( class == 7 ) then

         zzekesiz  =  1

      else if ( class == 8 ) then

         zzekesiz  =  1

      else if ( class == 9 ) then

         zzekesiz  =  1

      else

!
!        This is an unsupported column class.
!
         call zzekcnam ( handle, coldsc, column )

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )
         segno  =  segdsc ( snoidx )

         call setmsg ( 'Class # from input column descriptor is not ' // &
                       'a supported integer class.  COLUMN = #; '     // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         call errint ( '#',  class                                     )
         call errch  ( '#',  column                                    )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         return

      end if

      return
      end
