!> The parameters of SPICELIB's include file eksegdsc.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_eksegdsc

   implicit none

   public

!     Include Section:  EK Segment Descriptor Parameters
!
!        eksegdsc.inc  Version 8  06-NOV-1995 (NJB)
!
!
!     All `base addresses' referred to below are the addresses
!     *preceding* the item the base applies to.  This convention
!     enables simplied address calculations in many cases.
!
!     Size of segment descriptor.  Note:  the include file ekcoldsc.inc
!     must be updated if this parameter is changed.  The parameter
!     CDOFF in that file should be kept equal to SDSCSZ.
!
   integer               sdscsz
   parameter           ( sdscsz = 24  )

!
!     Index of the segment type code:
!
   integer               ektidx
   parameter           ( ektidx = 1 )

!
!     Index of the segment's number.  This number is the segment's
!     index in the list of segments contained in the EK to which
!     the segment belongs.
!
   integer               snoidx
   parameter           ( snoidx = ektidx  + 1 )

!
!     Index of the DAS integer base address of the segment's integer
!     meta-data:
!
   integer               imdidx
   parameter           ( imdidx = snoidx  + 1 )

!
!     Index of the DAS character base address of the table name:
!
   integer               tnmidx
   parameter           ( tnmidx = imdidx  + 1 )

!
!     Index of the segment's column count:
!
   integer               ncidx
   parameter           ( ncidx  = tnmidx  + 1 )

!
!     Index of the segment's record count:
!
   integer               nridx
   parameter           ( nridx  = ncidx + 1 )

!
!     Index of the root page number of the record tree:
!
   integer               rtidx
   parameter           ( rtidx  = nridx + 1 )

!
!     Index of the root page number of the character data page tree:
!
   integer               cptidx
   parameter           ( cptidx = rtidx  + 1 )

!
!     Index of the root page number of the double precision data page
!     tree:
!
   integer               dptidx
   parameter           ( dptidx = cptidx + 1 )

!
!     Index of the root page number of the integer data page tree:
!
   integer               iptidx
   parameter           ( iptidx = dptidx + 1 )

!
!     Index of the `modified' flag:
!
   integer               mflidx
   parameter           ( mflidx = iptidx + 1 )

!
!     Index of the `initialized' flag:
!
   integer               iflidx
   parameter           ( iflidx = mflidx + 1 )

!
!     Index of the shadowing flag:
!
   integer               shdidx
   parameter           ( shdidx = iflidx + 1 )

!
!     Index of the companion file handle:
!
   integer               cfhidx
   parameter           ( cfhidx = shdidx + 1 )

!
!     Index of the companion segment number:
!
   integer               csnidx
   parameter           ( csnidx = cfhidx + 1 )

!
!     The next three items are, respectively, the page numbers of the
!     last character, d.p., and integer data pages allocated by the
!     segment:
!
   integer               lcpidx
   parameter           ( lcpidx = csnidx + 1 )

   integer               ldpidx
   parameter           ( ldpidx = lcpidx + 1 )

   integer               lipidx
   parameter           ( lipidx = ldpidx + 1 )

!
!     The next three items are, respectively, the page-relative
!     indices of the last DAS word in use in the segment's
!     last character, d.p., and integer data pages:
!
   integer               lcwidx
   parameter           ( lcwidx = lipidx + 1 )

   integer               ldwidx
   parameter           ( ldwidx = lcwidx + 1 )

   integer               liwidx
   parameter           ( liwidx = ldwidx + 1 )

!
!     Index of the DAS character base address of the column name list:
!
   integer               nmlidx
   parameter           ( nmlidx = liwidx  + 1 )

!
!     The last descriptor element is reserved for future use.  No
!     parameter is defined to point to this location.
!

!
!     End Include Section:  EK Segment Descriptor Parameters
!

end module pepper_inc_eksegdsc
