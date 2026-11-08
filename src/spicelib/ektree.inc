!     Include Section:  EK Tree Parameters
!
!        ektree.inc  Version 3    22-OCT-1995 (NJB)
!
!
!     The parameters in this file define the tree structure
!     used by the EK system.  This structure is a variant of the
!     B*-tree structure described in Knuth's book, that is
!
!         Knuth, Donald E.  "The Art of Computer Programming,
!         Volume 3/Sorting and Searching" 1973, pp 471-479.
!
!     The trees used in the EK system differ from generic B*-trees
!     primarily in the way keys are treated.  Rather than storing
!     unique primary key values in each node, EK trees store integer
!     counts that represent the ordinal position of each data value,
!     counting from the lowest indexed element in the subtree whose
!     root is the node in question.  Thus the keys are unique within
!     a node but not across multiple nodes:  in fact the Nth key in
!     every leaf node is N.  The absolute ordinal position of a data
!     item is defined recursively as the sum of the key of the data item
!     and the absolute ordinal position of the data item in the parent
!     node that immediately precedes all elements of the node in
!     question.  This data structure allows EK trees to support lookup
!     of data items based on their ordinal position in a data set.  The
!     two prime applications of this capability in the EK system are:
!
!        1) Using trees to index the records in a table, allowing
!           the Nth record to be located efficiently.
!
!        2) Using trees to implement order vectors that can be
!           maintained when insertions and deletions are done.
!
!
!
!                           Root node
!
!        +--------------------------------------------+
!        |              Tree version code             |
!        +--------------------------------------------+
!        |           Number of nodes in tree          |
!        +--------------------------------------------+
!        |            Number of keys in tree          |
!        +--------------------------------------------+
!        |                Depth of tree               |
!        +--------------------------------------------+
!        |            Number of keys in root          |
!        +--------------------------------------------+
!        |              Space for n keys,             |
!        |                                            |
!        |        n = 2 * INT( ( 2*m - 2 )/3 )        |
!        |                                            |
!        |  where m is the max number of children per |
!        |          node in the child nodes           |
!        +--------------------------------------------+
!        |        Space for n+1 child pointers,       |
!        |         where n is as defined above.       |
!        +--------------------------------------------+
!        |          Space for n data pointers,        |
!        |         where n is as defined above.       |
!        +--------------------------------------------+
!
!
!                           Child node
!
!        +--------------------------------------------+
!        |        Number of keys present in node      |
!        +--------------------------------------------+
!        |              Space for m-1 keys            |
!        +--------------------------------------------+
!        |         Space for m child pointers         |
!        +--------------------------------------------+
!        |         Space for m-1 data pointers        |
!        +--------------------------------------------+
!
!
!
!
!     The following parameters give the maximum number of children
!     allowed in the root and child nodes.  During insertions, the
!     number of children may overflow by 1.
!
!
!     Maximum number of children allowed in a child node:
!
      INTEGER               MXKIDC
      PARAMETER           ( MXKIDC = 63 )

!
!     Maximum number of keys allowed in a child node:
!
      INTEGER               MXKEYC
      PARAMETER           ( MXKEYC = MXKIDC - 1 )

!
!     Minimum number of children allowed in a child node:
!
      INTEGER               MNKIDC
      PARAMETER           ( MNKIDC =  ( 2*MXKIDC + 1 ) / 3  )

!
!     Minimum number of keys allowed in a child node:
!
      INTEGER               MNKEYC
      PARAMETER           ( MNKEYC =  MNKIDC - 1  )

!
!     Maximum number of children allowed in the root node:
!
      INTEGER               MXKIDR
      PARAMETER           ( MXKIDR =   2 * ( (2*MXKIDC - 2)/3 )  +  1  )

!
!     Maximum number of keys allowed in the root node:
!
      INTEGER               MXKEYR
      PARAMETER           ( MXKEYR =   MXKIDR - 1   )

!
!     Minimum number of children allowed in the root node:
!
      INTEGER               MNKIDR
      PARAMETER           ( MNKIDR =  2   )


!
!
!     The following parameters indicate positions of elements in the
!     tree node structures shown above.
!
!
!     The following parameters are for the root node only:
!

!
!     Location of version code:
!
      INTEGER               TRTYPE
      PARAMETER           ( TRTYPE = 1 )

!
!     Version code:
!
      INTEGER               TRVERS
      PARAMETER           ( TRVERS = 1 )

!
!     Location of node count:
!
      INTEGER               TRNNOD
      PARAMETER           ( TRNNOD = TRTYPE + 1 )

!
!     Location of total key count for the tree:
!
      INTEGER               TRNKEY
      PARAMETER           ( TRNKEY = TRNNOD + 1 )

!
!     Location of tree depth:
!
      INTEGER               TRDPTH
      PARAMETER           ( TRDPTH = TRNKEY + 1 )

!
!     Location of count of keys in root node:
!
      INTEGER               TRNKR
      PARAMETER           ( TRNKR  = TRDPTH + 1 )

!
!     Base address of keys in the root node:
!
      INTEGER               TRKEYR
      PARAMETER           ( TRKEYR = TRNKR )

!
!     Base address of child pointers in root node:
!
      INTEGER               TRKIDR
      PARAMETER           ( TRKIDR = TRKEYR + MXKEYR + 1 )

!
!     Base address of data pointers in the root node (allow room for
!     overflow):
!
      INTEGER               TRDATR
      PARAMETER           ( TRDATR = TRKIDR + MXKIDR + 1 )

!
!     Size of root node:
!
      INTEGER               TRSIZR
      PARAMETER           ( TRSIZR = TRDATR + MXKEYR + 1 )



!
!     The following parameters are for child nodes only:
!
!
!     Location of number of keys in node:
!
      INTEGER               TRNKC
      PARAMETER           ( TRNKC  =  1 )

!
!     Base address of keys in child nodes:
!
      INTEGER               TRKEYC
      PARAMETER           ( TRKEYC = TRNKC )

!
!     Base address of child pointers in child nodes:
!
      INTEGER               TRKIDC
      PARAMETER           ( TRKIDC = TRKEYC + MXKEYC + 1 )

!
!     Base address of data pointers in child nodes (allow room
!     for overflow):
!
      INTEGER               TRDATC
      PARAMETER           ( TRDATC = TRKIDC + MXKIDC + 1 )

!
!     Size of child node:
!
      INTEGER               TRSIZC
      PARAMETER           ( TRSIZC = TRDATC + MXKEYC + 1 )

!
!     A number of EK tree routines must declare stacks of fixed
!     depth; this depth limit imposes a limit on the maximum depth
!     that an EK tree can have.  Because of the large branching
!     factor of EK trees, the depth limit is of no practical
!     importance:  The number of keys that can be held in an EK
!     tree of depth N is
!
!                           N-1
!                     MXKIDC   -  1
!         MXKIDR  *   -------------
!                      MXKIDC  - 1
!
!
!     This formula yields a capacity of over 1 billion keys for a
!     tree of depth 6.
!
      INTEGER               TRMXDP
      PARAMETER           ( TRMXDP = 10 )

!
!     End Include Section:  EK Tree Parameters
!
