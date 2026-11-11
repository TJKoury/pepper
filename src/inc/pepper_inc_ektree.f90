!> EK Tree Parameters.
!>
!>     The parameters in this file define the tree structure
!>     used by the EK system.  This structure is a variant of the
!>     B*-tree structure described in Knuth's book, that is
!>
!>     The trees used in the EK system differ from generic B*-trees
!>     primarily in the way keys are treated.  Rather than storing
!>     unique primary key values in each node, EK trees store integer
!>     counts that represent the ordinal position of each data value,
!>     counting from the lowest indexed element in the subtree whose
!>     root is the node in question.  Thus the keys are unique within
!>     a node but not across multiple nodes:  in fact the Nth key in
!>     every leaf node is N.  The absolute ordinal position of a data
!>     item is defined recursively as the sum of the key of the data item
!>     and the absolute ordinal position of the data item in the parent
!>     node that immediately precedes all elements of the node in
!>     question.  This data structure allows EK trees to support lookup
!>     of data items based on their ordinal position in a data set.  The
!>     two prime applications of this capability in the EK system are:
!>
!>     The following parameters give the maximum number of children
!>     allowed in the root and child nodes.  During insertions, the
!>     number of children may overflow by 1.
!>
!>## Version
!>
!>        ektree.inc  Version 3    22-OCT-1995 (NJB)
!>
!>         Knuth, Donald E.  "The Art of Computer Programming,
!>         Volume 3/Sorting and Searching" 1973, pp 471-479.
!>
!>        1) Using trees to index the records in a table, allowing
!>           the Nth record to be located efficiently.
!>
!>        2) Using trees to implement order vectors that can be
!>           maintained when insertions and deletions are done.
!>
!>                           Root node
!>
!>        +--------------------------------------------+
!>        |              Tree version code             |
!>        +--------------------------------------------+
!>        |           Number of nodes in tree          |
!>        +--------------------------------------------+
!>        |            Number of keys in tree          |
!>        +--------------------------------------------+
!>        |                Depth of tree               |
!>        +--------------------------------------------+
!>        |            Number of keys in root          |
!>        +--------------------------------------------+
!>        |              Space for n keys,             |
!>        |                                            |
!>        |        n = 2 * INT( ( 2*m - 2 )/3 )        |
!>        |                                            |
!>        |  where m is the max number of children per |
!>        |          node in the child nodes           |
!>        +--------------------------------------------+
!>        |        Space for n+1 child pointers,       |
!>        |         where n is as defined above.       |
!>        +--------------------------------------------+
!>        |          Space for n data pointers,        |
!>        |         where n is as defined above.       |
!>        +--------------------------------------------+
!>
!>                           Child node
!>
!>        +--------------------------------------------+
!>        |        Number of keys present in node      |
!>        +--------------------------------------------+
!>        |              Space for m-1 keys            |
!>        +--------------------------------------------+
!>        |         Space for m child pointers         |
!>        +--------------------------------------------+
!>        |         Space for m-1 data pointers        |
!>        +--------------------------------------------+
module pepper_inc_ektree

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: mxkidc = 63  !! Maximum number of children allowed in a child node:

   integer(ip), parameter :: mxkeyc = mxkidc - 1  !! Maximum number of keys allowed in a child node:

   integer(ip), parameter :: mnkidc = ( 2*mxkidc + 1 ) / 3  !! Minimum number of children allowed in a child node:

   integer(ip), parameter :: mnkeyc = mnkidc - 1  !! Minimum number of keys allowed in a child node:

   integer(ip), parameter :: mxkidr = 2 * ( (2*mxkidc - 2)/3 ) + 1  !! Maximum number of children allowed in the root node:

   integer(ip), parameter :: mxkeyr = mxkidr - 1  !! Maximum number of keys allowed in the root node:

   integer(ip), parameter :: mnkidr = 2  !! Minimum number of children allowed in the root node:

!
!
!     The following parameters indicate positions of elements in the
!     tree node structures shown above.
!
!
!     The following parameters are for the root node only:
!

   integer(ip), parameter :: trtype = 1  !! Location of version code:

   integer(ip), parameter :: trvers = 1  !! Version code:

   integer(ip), parameter :: trnnod = trtype + 1  !! Location of node count:

   integer(ip), parameter :: trnkey = trnnod + 1  !! Location of total key count for the tree:

   integer(ip), parameter :: trdpth = trnkey + 1  !! Location of tree depth:

   integer(ip), parameter :: trnkr = trdpth + 1  !! Location of count of keys in root node:

   integer(ip), parameter :: trkeyr = trnkr  !! Base address of keys in the root node:

   integer(ip), parameter :: trkidr = trkeyr + mxkeyr + 1  !! Base address of child pointers in root node:

   integer(ip), parameter :: trdatr = trkidr + mxkidr + 1  !! Base address of data pointers in the root node (allow room for
                                                           !! overflow):

   integer(ip), parameter :: trsizr = trdatr + mxkeyr + 1  !! Size of root node:

   integer(ip), parameter :: trnkc = 1  !! The following parameters are for child nodes only:
                                        !!
                                        !! Location of number of keys in node:

   integer(ip), parameter :: trkeyc = trnkc  !! Base address of keys in child nodes:

   integer(ip), parameter :: trkidc = trkeyc + mxkeyc + 1  !! Base address of child pointers in child nodes:

   integer(ip), parameter :: trdatc = trkidc + mxkidc + 1  !! Base address of data pointers in child nodes (allow room
                                                           !! for overflow):

   integer(ip), parameter :: trsizc = trdatc + mxkeyc + 1  !! Size of child node:

   integer(ip), parameter :: trmxdp = 10  !! A number of EK tree routines must declare stacks of fixed
                                          !! depth; this depth limit imposes a limit on the maximum depth
                                          !! that an EK tree can have.  Because of the large branching
                                          !! factor of EK trees, the depth limit is of no practical
                                          !! importance:  The number of keys that can be held in an EK
                                          !! tree of depth N is
                                          !!
                                          !!                       N-1
                                          !!                 MXKIDC   -  1
                                          !!     MXKIDR  *   -------------
                                          !!                  MXKIDC  - 1
                                          !!
                                          !! This formula yields a capacity of over 1 billion keys for a
                                          !! tree of depth 6.

end module pepper_inc_ektree
