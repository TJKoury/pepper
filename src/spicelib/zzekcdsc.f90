!$Procedure  ZZEKCDSC ( Private: EK, return column descriptor )

      subroutine zzekcdsc ( handle, segdsc, column, coldsc )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Look up the column descriptor for a column of a given name
!     in a specified segment.
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

      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      character*(*)         column
      integer(ip)           coldsc ( cdscsz )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to an EK file.
!     SEGDSC     I   Segment descriptor.
!     COLUMN     I   Name of column.
!     COLDSC     O   Descriptor for specified column.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle for the file containing the
!                    column of interest.  The EK may be open for read
!                    or write access.
!
!     SEGDSC         is the descriptor of the segment containing the
!                    column for which a descriptor is desired.
!
!     COLUMN         is the name of the column whose descriptor is
!                    desired.  Case and white space are not significant.
!
! $ Detailed_Output
!
!     COLDSC         is the descriptor of the column belonging to the
!                    specified file and segment and having name COLUMN.
!                    See the include file ekcoldsc.inc for details
!                    regarding the structure of EK column descriptors.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input column name does not match any column in the
!         designated segment, the error SPICE(BUG) is signaled.  It
!         is the caller's responsibility to call this routine with
!         valid input arguments.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine exists for the sole purpose of centralizing code
!     used to perform column descriptor look-ups.
!
! $ Examples
!
!     See the EKACEx routines.
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
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error message.
!
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr

!
!     Local variables
!
      character*(cnamsz)    cname

      integer(ip)           dscbas
      integer(ip)           i
      integer(ip)           mbase
      integer(ip)           nambas
      integer(ip)           ncols

      logical               found

!
!     Use discovery check-in.
!
!     Get the segment's integer metadata's base address.
!
      mbase  =  segdsc ( imdidx )

!
!     Get the number of columns.
!
      ncols  =  segdsc ( ncidx )

!
!     Search linearly through the column descriptors, looking for
!     a column name match.  It's an error if we don't find the input
!     name.
!
      found = .false.
      i     =  1

      do while (  ( i .le. ncols )  .and.  ( .not. found )  )

         dscbas  =  mbase + sdscsz + (i-1)*cdscsz
!
!        Get the character base address of the column name from the
!        current descriptor.
!
         call dasrdi ( handle, dscbas+1, dscbas+cdscsz, coldsc )

         nambas = coldsc( namidx )

!
!        Look up the name and compare.
!
         call dasrdc ( handle,  nambas+1,  nambas+cnamsz, &
                       1,       cnamsz,    cname         )

         if (  eqstr( cname, column )  ) then
            found = .true.
         else
            i     =  i + 1
         end if


      end do


      if ( .not. found ) then

         call chkin  ( 'ZZEKCDSC'                                 )
         call setmsg ( 'Descriptor for column # was not found. ' // &
                       'Segment base = #; file = #.'              )
         call errch  ( '#',  column                               )
         call errint ( '#',  mbase                                )
         call errhan ( '#',  handle                               )
         call sigerr ( 'SPICE(BUG)'                               )
         call chkout ( 'ZZEKCDSC'                                 )
         return

      end if

      end
