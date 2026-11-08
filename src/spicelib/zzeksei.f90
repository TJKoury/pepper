!$Procedure  ZZEKSEI ( Private: EK, set encoded integer )

      subroutine zzeksei ( handle, addrss, ival )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Set an encoded integer at a specified address from a character
!     data page.
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

      integer(ip)           handle
      integer(ip)           addrss
      integer(ip)           ival

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     ADDRSS     I   DAS character address.
!     IVAL       I   Integer value to write.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file open for write access.
!
!     ADDRSS         is the DAS character start address at which an
!                    integer, encoded as a string, is to be written.
!                    An encoded integer occupies ENCSIZ characters,
!                    where the parameter ENCSIZ is defined in the
!                    include file ekdatpag.inc.
!
!     IVAL           is an integer value to be written in encoded form.
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
!     2)  If the DAS character address range
!
!            ADDRSS .. ADDRSS+ENCSIZ-1
!
!         is not a range of DAS character addresses that have been
!         initialized, the error wll be diagnosed by routines
!         called by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine should be used for all EK applications requiring
!     storage of encoded integer values as characters.  Use of this
!     routine should ensure consistent encoding across the library.
!
!     Encoded integers written by this routine should be read using
!     ZZEKGEI.
!
! $ Examples
!
!     See ZZEKAD03.
!
! $ Restrictions
!
!     1)  Portability dictates that the base used for encoding be
!         no greater than 128.
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
!-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     Local variables
!
      character(encsiz)     cval

!
!     Discovery error handling should be used in this utility.
!
!     Encode the number.
!
      call prtenc ( ival, cval )

!
!     Write the encoded value.
!
      call dasudc ( handle, addrss, addrss+encsiz-1, 1, encsiz, cval )

      return
      end
