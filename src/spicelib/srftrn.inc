! $ Abstract
!
!     Declare public surface name/ID mapping parameters.
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
!-    SPICELIB Version 1.0.0, 02-DEC-2015 (NJB)
!
!-&



!
!     Maximum number of surface name/ID mapping entries:
!
      integer               mxnsrf
      parameter           ( mxnsrf = 2000 )

!
!     Maximum length of a surface name string:
!
      integer               sfnmln
      parameter           ( sfnmln = 36 )

!
!     End of file srftrn.inc.
!
