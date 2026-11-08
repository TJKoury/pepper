!$Procedure      ZZEKCIX1 ( EK, create index, type 1 )

      subroutine zzekcix1 ( handle, coldsc )

! $ Abstract
!
!     Create a new type 1 index for a specified EK column.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations


      include 'ekcoldsc.inc'

      integer               handle
      integer               coldsc ( cdscsz )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     COLDSC     I   Column descriptor.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK that is open for write
!                    access.
!
!     COLDSC         is the column descriptor of the column for
!                    which the index is to be created.
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
!     This routine operates by side effects:  it creates a new, empty
!     type 1 index for a specified EK column.  Though this routine
!     does not require a segment to be specified, normally indexes
!     are created for columns belonging to specific segments.
!
!     Type 1 indexes are implemented as DAS B*-trees.  The data
!     pointers of an index tree contain record numbers.  Therefore, the
!     tree implements an abstract order vector.
!
!     In order to support the capability of creating an index for a
!     column that has already been populated with data, this routine
!     does not check that the specified column is empty.  The caller
!     must populate the index appropriately to reflect the order of
!     elements in the associated column.
!
! $ Examples
!
!     See EKBSEG.
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
!-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKCIX1' )
      end if

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) then
         call chkout ( 'ZZEKCIX1' )
         return
      end if

!
!     An empty type 1 segment is just an empty B*-tree.  The root
!     page number of the tree serves as the index pointer.
!
      coldsc(ixtidx)  =  1

      call zzektrit ( handle, coldsc(ixpidx) )


      call chkout ( 'ZZEKCIX1' )
      return
      end
