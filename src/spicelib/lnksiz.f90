!$Procedure LNKSIZ ( LNK, size )

      integer(ip) function lnksiz ( pool )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the size of a doubly linked list pool.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     LIST
!
! $ Declarations

      implicit none

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           pool ( 2,  lbpool : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POOL       I   A doubly linked list pool.
!     LBPOOL     P   Lower bound of pool column indices.
!
!     The function returns the size of the pool.
!
! $ Detailed_Input
!
!     POOL     is a doubly linked list pool.
!
! $ Detailed_Output
!
!     The function returns the size (total number of nodes) of the pool.
!
! $ Parameters
!
!     LBPOOL   is the lower bound of the column indices of the POOL
!              array. The columns indexed LBPOOL to 0 are reserved
!              as a control area for the pool.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows an application program to determine the size
!     of a doubly linked list pool at run-time, without having to rely
!     on knowledge of the internals of the doubly linked list pool
!     structure.
!
! $ Examples
!
!     1)  Let POOL be a doubly linked list pool. The total number of
!         nodes in a doubly linked list pool is set when the pool is
!         initialized by LNKINI. This number is returned by LNKSIZ:
!
!            C
!            C     This sequence of calls will assign the value 100
!            C     to the variable SIZE:
!            C
!                  CALL LNKINI ( 100, POOL )
!                  SIZE = LNKSIZ ( POOL )
!
! $ Restrictions
!
!     1)  Linked list pools must be initialized via the routine
!         LNKINI. Failure to initialize a linked list pool
!         will almost certainly lead to confusing results.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     return size of linked list
!
!-&


!
!     Local parameters
!

!
!     The control area contains 3 elements.  They are:
!
!        The "size" of the pool, that is, the number
!        of nodes in the pool.
!
!        The number of free nodes in the pool.
!
!        The "free pointer," which is the column index of the first free
!        node.
!
!     Parameters defining the row and column indices of these control
!     elements are given below.
!
      integer(ip)           sizrow
      parameter           ( sizrow =   1 )

      integer(ip)           sizcol
      parameter           ( sizcol =   0 )

      integer(ip)           nfrrow
      parameter           ( nfrrow =   2 )

      integer(ip)           nfrcol
      parameter           ( nfrcol =   0 )

      integer(ip)           frerow
      parameter           ( frerow =   1 )

      integer(ip)           frecol
      parameter           ( frecol =  -1 )

!
!     Each assigned node consists of a backward pointer and a forward
!     pointer.
!
!        +-------------+       +-------------+       +-------------+
!        |  forward--> |       |  forward--> |       |  forward--> |
!        +-------------+  ...  +-------------+  ...  +-------------+
!        | <--backward |       | <--backward |       | <--backward |
!        +-------------+       +-------------+       +-------------+
!
!            node 1                 node I              node SIZE
!
!
!
      integer(ip)           forwrd
      parameter           ( forwrd  = 1 )

      integer(ip)           bckwrd
      parameter           ( bckwrd  = 2 )

!
!     Free nodes say that that's what they are.  The way they say it
!     is by containing the value FREE in their backward pointers.
!     Needless to say, FREE is a value that cannot be a valid pointer.
!
      integer(ip)           free
      parameter           ( free   = 0 )



!
!     Grab the pool size from the control area.
!
      lnksiz  =  pool ( sizrow, sizcol )

      return
      end
