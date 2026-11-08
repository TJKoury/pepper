!$Procedure  ZZEKGEI ( Private: EK, get encoded integer )

      SUBROUTINE ZZEKGEI ( HANDLE, ADDRSS, IVAL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Get an encoded integer at a specified address from a character
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

      INCLUDE 'ekdatpag.inc'

      INTEGER               HANDLE
      INTEGER               ADDRSS
      INTEGER               IVAL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     ADDRSS     I   DAS character address to read encoded value from.
!     IVAL       O   Decoded integer value.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file open for read or
!                    write access.
!
!     ADDRSS         is the DAS character start address from which an
!                    integer, encoded as a string, is to be read.
!                    An encoded integer occupies ENCSIZ characters,
!                    where the parameter ENCSIZ is defined in the
!                    include file ekdatpag.inc.
!
! $ Detailed_Output
!
!     IVAL           is an integer value obtained by decoding an
!                    encoded integer read from the specified
!                    location.
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
!     3)  If the character data starting at the specified address
!         does not represent an encoded integer, the error wll be
!         diagnosed by routines called by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine should be used for all EK applications reading
!     integer values that have been encoded as characters.  This
!     routine expects the encoding to have been done by ZZEKSEI.
!
! $ Examples
!
!     See ZZEKRD03.
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
      CHARACTER*(ENCSIZ)    CVAL


!
!     Discovery error handling should be used in this utility.
!
!
!     Read the encoded value.  The value is represented by a string of
!     characters.
!
      CALL DASRDC ( HANDLE, ADDRSS, ADDRSS+ENCSIZ-1, 1, ENCSIZ, CVAL )

!
!     Decode the number.
!
      CALL PRTDEC ( CVAL, IVAL )

      RETURN
      END
