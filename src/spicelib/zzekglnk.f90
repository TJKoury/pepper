!$Procedure      ZZEKGLNK ( EK, get link count for data page )

      subroutine zzekglnk ( handle, type, p, nlinks )

! $ Abstract
!
!     Return the link count for a specified EK data page.
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

      include 'ekdatpag.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               type
      integer               p
      integer               nlinks

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TYPE       I   Data type of page.
!     P          I   Page number.
!     NLINKS     O   Number of links to page.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TYPE           is the data type of the desired page.
!
!     P              is the page number of the allocated page.  This
!                    number is recognized by the EK paged access
!                    routines.
!
! $ Detailed_Output
!
!     NLINKS         is the currently held number of links to the
!                    specified data page.
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
!     2)  If TYPE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     3)  If an I/O error occurs while reading or writing the indicated
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
!     This routine centralizes EK data page link count accesses.
!
! $ Examples
!
!     See EKDELR.
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
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      double precision      dplnk
      integer               base

!
!     Use discovery check-in.
!
!     Look up the base address of the page.
!
      call zzekpgbs ( type, p, base )

      if ( failed() ) then
         return
      end if


      if ( type .eq. chr ) then
!
!        Look up the encoded count.
!
         call zzekgei ( handle, base+clcidx, nlinks )


      else if ( type .eq. dp ) then
!
!        Convert the encoded count to integer type.
!
         call dasrdd ( handle, base+dlcidx, base+dlcidx, dplnk )

         nlinks  =  nint ( dplnk )


      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         call dasrdi ( handle, base+ilcidx, base+ilcidx, nlinks )

      end if

      return
      end
