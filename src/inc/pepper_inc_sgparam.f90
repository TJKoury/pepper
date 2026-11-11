!> Parameter declarations for the generic segments subroutines.
!>
!>## Particulars
!>
!>     This include file contains the parameters used by the generic
!>     segments subroutines, SGxxxx. A generic segment is a
!>     generalization of a DAF array which places a particular structure
!>     on the data contained in the array, as described below.
!>
!>     This file defines the mnemonics that are used for the index types
!>     allowed in generic segments as well as mnemonics for the meta data
!>     items which are used to describe a generic segment.
!>
!>     A DAF generic segment contains several logical data partitions:
!>
!>        1) A partition for constant values to be associated with each
!>           data packet in the segment.
!>
!>        2) A partition for the data packets.
!>
!>        3) A partition for reference values.
!>
!>        4) A partition for a packet directory, if the segment contains
!>           variable sized packets.
!>
!>        5) A partition for a reference value directory.
!>
!>        6) A reserved partition that is not currently used. This
!>           partition is only for the use of the NAIF group at the Jet
!>           Propulsion Laboratory (JPL).
!>
!>        7) A partition for the meta data which describes the locations
!>           and sizes of other partitions as well as providing some
!>           additional descriptive information about the generic
!>           segment.
!>
!>                 +============================+
!>                 |         Constants          |
!>                 +============================+
!>                 |          Packet 1          |
!>                 |----------------------------|
!>                 |          Packet 2          |
!>                 |----------------------------|
!>                 |              .             |
!>                 |              .             |
!>                 |              .             |
!>                 |----------------------------|
!>                 |          Packet N          |
!>                 +============================+
!>                 |      Reference Values      |
!>                 +============================+
!>                 |      Packet Directory      |
!>                 +============================+
!>                 |    Reference  Directory    |
!>                 +============================+
!>                 |       Reserved  Area       |
!>                 +============================+
!>                 |     Segment Meta Data      |
!>                 +----------------------------+
!>
!>     Only the placement of the meta data at the end of a generic
!>     segment is required. The other data partitions may occur in any
!>     order in the generic segment because the meta data will contain
!>     pointers to their appropriate locations within the generic
!>     segment.
!>
!>     The meta data for a generic segment should only be obtained
!>     through use of the subroutine SGMETA. The meta data should not be
!>     written through any mechanism other than the ending of a generic
!>     segment begun by SGBWFS or SGBWVS using SGWES.
!>
!>## Restrictions
!>
!>     1) If new reference index types are added, the new type(s) should
!>        be defined to be the consecutive integer(s) after the last
!>        defined reference index type used. In this way a value for
!>        the maximum allowed index type may be maintained. This value
!>        must also be updated if new reference index types are added.
!>
!>     2) If new meta data items are needed, mnemonics for them must be
!>        added to the end of the current list of mnemonics and before
!>        the NMETA mnemonic. In this way compatibility with files having
!>        a different, but smaller, number of meta data items may be
!>        maintained. See the description and example below.
!>
!>## Author and Institution
!>
!>     N.J. Bachman      (JPL)
!>     K.R. Gehringer    (JPL)
!>     W.L. Taber        (JPL)
!>     F.S. Turner       (JPL)
!>
!>## Literature References
!>
!>     Generic Segments Required Reading.
!>     DAF Required Reading.
!>
!>## Version
!>
!>-    SPICELIB Version 1.1.1, 28-JAN-2004 (NJB)
!>
!>        Header update: equations for comptutations of packet indices
!>        for the cases of index types 0 and 1 were corrected.
!>
!>-    SPICELIB Version 1.1.0, 25-09-98 (FST)
!>
!>        Added parameter MNMETA, the minimum number of meta data items
!>        that must be present in a generic DAF segment.
!>
!>-    SPICELIB Version 1.0.0, 04-03-95 (KRG) (WLT)
module pepper_inc_sgparam

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: imple = 0  !! Mnemonics for the type of reference value index.
                                        !!
                                        !! Two forms of indexing are provided:
                                        !!
                                        !!     1) An implicit form of indexing based on using two values, a
                                        !!        starting value, which will have an index of 1, and a step
                                        !!        size between reference values, which are used to compute an
                                        !!        index and a reference value associated with a specified key
                                        !!        value. See the descriptions of the implicit types below for
                                        !!        the particular formula used in each case.
                                        !!
                                        !!     2) An explicit form of indexing based on a reference value for
                                        !!        each data packet.
                                        !!
                                        !! Reference Index Type 0
                                        !! ----------------------
                                        !!
                                        !! Implied index. The index and reference value of a data packet
                                        !! associated with a specified key value are computed from the two
                                        !! generic segment reference values using the formula below. The two
                                        !! generic segment reference values, REF(1) and REF(2), represent,
                                        !! respectively, a starting value and a step size between reference
                                        !! values. The index of the data packet associated with a key value
                                        !! of VALUE is given by:
                                        !!
                                        !!                       /    VALUE - REF(1)    \
                                        !!     INDEX = 1  +  INT | -------------------- |
                                        !!                       \        REF(2)        /
                                        !!
                                        !! and the reference value associated with VALUE is given by:
                                        !!
                                        !!     REFVAL = REF(1) + DBLE (INDEX-1) * REF(2)

   integer(ip), parameter :: impcls = 1  !! Reference Index Type 1
                                         !! ----------------------
                                         !!
                                         !! Implied index. The index and reference value of a data packet
                                         !! associated with a specified key value are computed from the two
                                         !! generic segment reference values using the formula below. The two
                                         !! generic segment reference values, REF(1) and REF(2), represent,
                                         !! respectively, a starting value and a step size between reference
                                         !! values. The index of the data packet associated with a key value
                                         !! of VALUE is given by:
                                         !!
                                         !!                       /          VALUE - REF(1)    \
                                         !!     INDEX = 1  +  INT | 0.5 + -------------------- |
                                         !!                       \              REF(2)        /
                                         !!
                                         !! and the reference value associated with VALUE is given by:
                                         !!
                                         !!     REFVAL = REF(1) + DBLE (INDEX-1) * REF(2)
                                         !!
                                         !! We get the larger index in the event that VALUE is halfway between
                                         !! X(I) and X(I+1), where X(I) = BUFFER(1) + DBLE (I-1) * REFDAT(2).

   integer(ip), parameter :: explt = 2  !! Reference Index Type 2
                                        !! ----------------------
                                        !!
                                        !! Explicit index. In this case the number of packets must equal the
                                        !! number of reference values. The index of the packet associated
                                        !! with a key value of VALUE is the index of the last reference item
                                        !! that is strictly less than VALUE. The reference values must be in
                                        !! ascending order, REF(I) < REF(I+1).

   integer(ip), parameter :: exple = 3  !! Reference Index Type 3
                                        !! ----------------------
                                        !!
                                        !! Explicit index. In this case the number of packets must equal the
                                        !! number of reference values. The index of the packet associated
                                        !! with a key value of VALUE is the index of the last reference item
                                        !! that is less than or equal to VALUE. The reference values must be
                                        !! in ascending order, REF(I) < REF(I+1).

   integer(ip), parameter :: expcls = 4  !! Reference Index Type 4
                                         !! ----------------------
                                         !!
                                         !! Explicit index. In this case the number of packets must equal the
                                         !! number of reference values. The index of the packet associated
                                         !! with a key value of VALUE is the index of the reference item
                                         !! that is closest to the value of VALUE. In the event of a "tie"
                                         !! the larger index is selected. The reference values must be in
                                         !! ascending order, REF(I) < REF(I+1).

   integer(ip), parameter :: mnidxt = 0  !! These parameters define the valid range for the index types. An
                                         !! index type code, MYTYPE, for a generic segment must satisfy the
                                         !! relation MNIDXT <= MYTYPE <= MXIDXT.

   integer(ip), parameter :: mxidxt = 4

   integer(ip), parameter :: conbas = 1  !! The following meta data items will appear in all generic segments.
                                         !! Other meta data items may be added if a need arises.
                                         !!
                                         !!      1)  CONBAS  Base Address of the constants in a generic segment.
                                         !!
                                         !!      2)  NCON    Number of constants in a generic segment.
                                         !!
                                         !!      3)  RDRBAS  Base Address of the reference directory for a
                                         !!                  generic segment.
                                         !!
                                         !!      4)  NRDR    Number of items in the reference directory of a
                                         !!                  generic segment.
                                         !!
                                         !!      5)  RDRTYP  Type of the reference directory 0, 1, 2 ... for a
                                         !!                  generic segment.
                                         !!
                                         !!      6)  REFBAS  Base Address of the reference items for a generic
                                         !!                  segment.
                                         !!
                                         !!      7)  NREF    Number of reference items in a generic segment.
                                         !!
                                         !!      8)  PDRBAS  Base Address of the Packet Directory for a generic
                                         !!                  segment.
                                         !!
                                         !!      9)  NPDR    Number of items in the Packet Directory of a generic
                                         !!                  segment.
                                         !!
                                         !!     10)  PDRTYP  Type of the packet directory 0, 1, ... for a generic
                                         !!                  segment.
                                         !!
                                         !!     11)  PKTBAS  Base Address of the Packets for a generic segment.
                                         !!
                                         !!     12)  NPKT    Number of Packets in a generic segment.
                                         !!
                                         !!     13)  RSVBAS  Base Address of the Reserved Area in a generic
                                         !!                  segment.
                                         !!
                                         !!     14)  NRSV    Number of items in the reserved area of a generic
                                         !!                  segment.
                                         !!
                                         !!     15)  PKTSZ   Size of the packets for a segment with fixed width
                                         !!                  data packets or the size of the largest packet for a
                                         !!                  segment with variable width data packets.
                                         !!
                                         !!     16)  PKTOFF  Offset of the packet data from the start of a packet
                                         !!                  record. Each data packet is placed into a packet
                                         !!                  record which may have some bookkeeping information
                                         !!                  prepended to the data for use by the generic
                                         !!                  segments software.
                                         !!
                                         !!     17)  NMETA   Number of meta data items in a generic segment.
                                         !!
                                         !! Meta Data Item  1
                                         !! -----------------

   integer(ip), parameter :: ncon = conbas + 1  !! Meta Data Item  2
                                                !! -----------------
   integer(ip), parameter :: rdrbas = ncon + 1  !! Meta Data Item  3
                                                !! -----------------
   integer(ip), parameter :: nrdr = rdrbas + 1  !! Meta Data Item  4
                                                !! -----------------
   integer(ip), parameter :: rdrtyp = nrdr + 1  !! Meta Data Item  5
                                                !! -----------------
   integer(ip), parameter :: refbas = rdrtyp + 1  !! Meta Data Item  6
                                                  !! -----------------
   integer(ip), parameter :: nref = refbas + 1  !! Meta Data Item  7
                                                !! -----------------
   integer(ip), parameter :: pdrbas = nref + 1  !! Meta Data Item  8
                                                !! -----------------
   integer(ip), parameter :: npdr = pdrbas + 1  !! Meta Data Item  9
                                                !! -----------------
   integer(ip), parameter :: pdrtyp = npdr + 1  !! Meta Data Item 10
                                                !! -----------------
   integer(ip), parameter :: pktbas = pdrtyp + 1  !! Meta Data Item 11
                                                  !! -----------------
   integer(ip), parameter :: npkt = pktbas + 1  !! Meta Data Item 12
                                                !! -----------------
   integer(ip), parameter :: rsvbas = npkt + 1  !! Meta Data Item 13
                                                !! -----------------
   integer(ip), parameter :: nrsv = rsvbas + 1  !! Meta Data Item 14
                                                !! -----------------
   integer(ip), parameter :: pktsz = nrsv + 1  !! Meta Data Item 15
                                               !! -----------------
   integer(ip), parameter :: pktoff = pktsz + 1  !! Meta Data Item 16
                                                 !! -----------------
   integer(ip), parameter :: nmeta = pktoff + 1  !! If new meta data items are to be added to this list, they should
                                                 !! be added above this comment block as described below.
                                                 !!
                                                 !!     INTEGER               NEW1
                                                 !!     PARAMETER           ( NEW1   = PKTOFF + 1 )
                                                 !!
                                                 !!     INTEGER               NEW2
                                                 !!     PARAMETER           ( NEW2   = NEW1   + 1 )
                                                 !!
                                                 !!     INTEGER               NEWEST
                                                 !!     PARAMETER           ( NEWEST = NEW2   + 1 )
                                                 !!
                                                 !! and then the value of NMETA must be changed as well to be:
                                                 !!
                                                 !!     INTEGER               NMETA
                                                 !!     PARAMETER           ( NMETA  = NEWEST + 1 )
                                                 !!
                                                 !! Meta Data Item 17
                                                 !! -----------------
   integer(ip), parameter :: mxmeta = nmeta  !! Maximum number of meta data items. This is always set equal to
                                             !! NMETA.
   integer(ip), parameter :: mnmeta = 15  !! Minimum number of meta data items that must be present in a DAF
                                          !! generic segment.  This number is to remain fixed even if more
                                          !! meta data items are added for compatibility with old DAF files.

end module pepper_inc_sgparam
