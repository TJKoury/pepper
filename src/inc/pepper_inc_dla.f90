!> The parameters of SPICELIB's include file dla.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_dla

   implicit none

   public


!
!     Include file dla.inc
!
!     This include file declares parameters for DLA format
!     version zero.
!
!        Version 3.0.1 17-OCT-2016 (NJB)
!
!           Corrected comment: VERIDX is now described as a DAS
!           integer address rather than a d.p. address.
!
!        Version 3.0.0 20-JUN-2006 (NJB)
!
!           Changed name of parameter DSCSIZ to DLADSZ.
!
!        Version 2.0.0 09-FEB-2005 (NJB)
!
!           Changed descriptor layout to make backward pointer
!           first element.  Updated DLA format version code to 1.
!
!           Added parameters for format version and number of bytes per
!           DAS comment record.
!
!        Version 1.0.0 28-JAN-2004 (NJB)
!


!
!     DAS integer address of DLA version code.
!
   integer               veridx
   parameter           ( veridx = 1 )

!
!     Linked list parameters
!
!     Logical arrays (aka "segments") in a DAS linked array (DLA) file
!     are organized as a doubly linked list.  Each logical array may
!     actually consist of character, double precision, and integer
!     components.  A component of a given data type occupies a
!     contiguous range of DAS addresses of that type.  Any or all
!     array components may be empty.
!
!     The segment descriptors in a SPICE DLA (DAS linked array) file
!     are connected by a doubly linked list.  Each node of the list is
!     represented by a pair of integers acting as forward and backward
!     pointers.  Each pointer pair occupies the first two integers of a
!     segment descriptor in DAS integer address space.  The DLA file
!     contains pointers to the first integers of both the first and
!     last segment descriptors.
!
!     At the DLA level of a file format implementation, there is
!     no knowledge of the data contents.  Hence segment descriptors
!     provide information only about file layout (in contrast with
!     the DAF system).  Metadata giving specifics of segment contents
!     are stored within the segments themselves in DLA-based file
!     formats.
!
!
!     Parameter declarations follow.
!
!     DAS integer addresses of first and last segment linked list
!     pointer pairs.  The contents of these pointers
!     are the DAS addresses of the first integers belonging
!     to the first and last link pairs, respectively.
!
!     The acronyms "LLB" and "LLE" denote "linked list begin"
!     and "linked list end" respectively.
!
   integer               llbidx
   parameter           ( llbidx = veridx + 1 )

   integer               lleidx
   parameter           ( lleidx = llbidx + 1 )

!
!     Null pointer parameter.
!
   integer               nulptr
   parameter           ( nulptr = -1 )

!
!     Segment descriptor parameters
!
!     Each segment descriptor occupies a contiguous
!     range of DAS integer addresses.
!
!        The segment descriptor layout is:
!
!           +---------------+
!           | BACKWARD PTR  | Linked list backward pointer
!           +---------------+
!           | FORWARD PTR   | Linked list forward pointer
!           +---------------+
!           | BASE INT ADDR | Base DAS integer address
!           +---------------+
!           | INT COMP SIZE | Size of integer segment component
!           +---------------+
!           | BASE DP ADDR  | Base DAS d.p. address
!           +---------------+
!           | DP COMP SIZE  | Size of d.p. segment component
!           +---------------+
!           | BASE CHR ADDR | Base DAS character address
!           +---------------+
!           | CHR COMP SIZE | Size of character segment component
!           +---------------+
!
!     Parameters defining offsets for segment descriptor elements
!     follow.
!
   integer               bwdidx
   parameter           ( bwdidx = 1 )

   integer               fwdidx
   parameter           ( fwdidx = bwdidx + 1 )

   integer               ibsidx
   parameter           ( ibsidx = fwdidx + 1 )

   integer               iszidx
   parameter           ( iszidx = ibsidx + 1 )

   integer               dbsidx
   parameter           ( dbsidx = iszidx + 1 )

   integer               dszidx
   parameter           ( dszidx = dbsidx + 1 )

   integer               cbsidx
   parameter           ( cbsidx = dszidx + 1 )

   integer               cszidx
   parameter           ( cszidx = cbsidx + 1 )

!
!     Descriptor size:
!
   integer               dladsz
   parameter           ( dladsz = cszidx )


!
!     Other DLA parameters:
!
!
!     DLA format version.  (This number is expected to occur very
!     rarely at integer address VERIDX in uninitialized DLA files.)
!
   integer               fmtver
   parameter           ( fmtver = 1000000 )

!
!     Characters per DAS comment record.
!
   integer               nchrec
   parameter           ( nchrec = 1024 )


!
!     End of include file dla.inc
!

end module pepper_inc_dla
