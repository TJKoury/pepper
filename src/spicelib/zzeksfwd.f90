!$Procedure      ZZEKSFWD ( EK, set forward pointer for data page )

      subroutine zzeksfwd ( handle, type, p, fward )

! $ Abstract
!
!     Set the forward data pointer for a specified EK data page.
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
      integer               fward

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TYPE       I   Data type of page.
!     P          I   Page number.
!     FWARD      I   Forward data pointer.
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
!     FWARD          is a forward data pointer.  This is the number
!                    of a data page on which the last column entry
!                    on page P is continued.
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
!     This routine sets the forward data pointer of the specified EK
!     data page.  The value of the pointer is a page number.
!
! $ Examples
!
!     See ZZEKAPS.
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
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      double precision      dpptr
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
!        Set the encoded count.
!
         call zzeksei ( handle, base+cfpidx, fward )


      else if ( type .eq. dp ) then
!
!        Convert the input count to d.p. type.
!
         dpptr  =  fward

         call dasudd ( handle, base+dfpidx, base+dfpidx, dpptr )


      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         call dasudi ( handle, base+ifpidx, base+ifpidx, fward )

      end if

      return
      end
