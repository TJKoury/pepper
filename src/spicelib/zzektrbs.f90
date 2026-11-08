!$Procedure      ZZEKTRBS ( EK tree, base address )

      integer function zzektrbs ( node )

! $ Abstract
!
!     Map a node in a tree to its DAS base integer address.
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

      include 'ektype.inc'

      integer               node

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NODE       I   Node number.
!
!     The function returns the DAS integer base address of the
!     specified node.
!
! $ Detailed_Input
!
!     NODE           is the number of a node in an EK tree.
!
! $ Detailed_Output
!
!     The function returns the DAS integer base address of the
!     specified node.  This address is the predecessor of the first
!     DAS integer word occupied by the node.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  No error checking is done on the input node number.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility that allows callers to locate a node
!     of a tree in the DAS integer address space.  Most commonly, this
!     routine is used to locate the root of a tree.
!
! $ Examples
!
!     See EKNSEG.
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
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&

!
!     Just use the mapping supplied by the paging system.
!
      call zzekpgbs ( int, node, zzektrbs )

      return
      end
