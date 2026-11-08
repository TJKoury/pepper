! $ Abstract
!
!     SPICE private include file intended solely for the support of
!     SPICE routines. User software should not include this file
!     due to the volatile nature of this file.
!
!     Declare private surface name/ID mapping parameters.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     CONVERSION
!     NAME
!     STRING
!     SURFACE
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 04-FEB-2017 (NJB)
!
!        Original version 03-DEC-2015 (NJB)
!
!-&

!
!     Size of the lists and hashes storing the POOL-defined name/ID
!     mappings. To ensure efficient hashing, this size is set to the
!     first prime number greater than MXNSRF defined in the public
!     include file
!
!        srftrn.inc.
!
      integer               nroom
      parameter           ( nroom   = 2003 )

!
!     Singly-linked list pool lower bound:
!
      integer               lbsngl
      parameter           ( lbsngl = -5 )

!
!     End of file zzsrftrn.inc.
!
