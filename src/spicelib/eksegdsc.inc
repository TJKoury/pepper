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
      INTEGER               SDSCSZ
      PARAMETER           ( SDSCSZ = 24  )

!
!     Index of the segment type code:
!
      INTEGER               EKTIDX
      PARAMETER           ( EKTIDX = 1 )

!
!     Index of the segment's number.  This number is the segment's
!     index in the list of segments contained in the EK to which
!     the segment belongs.
!
      INTEGER               SNOIDX
      PARAMETER           ( SNOIDX = EKTIDX  + 1 )

!
!     Index of the DAS integer base address of the segment's integer
!     meta-data:
!
      INTEGER               IMDIDX
      PARAMETER           ( IMDIDX = SNOIDX  + 1 )

!
!     Index of the DAS character base address of the table name:
!
      INTEGER               TNMIDX
      PARAMETER           ( TNMIDX = IMDIDX  + 1 )

!
!     Index of the segment's column count:
!
      INTEGER               NCIDX
      PARAMETER           ( NCIDX  = TNMIDX  + 1 )

!
!     Index of the segment's record count:
!
      INTEGER               NRIDX
      PARAMETER           ( NRIDX  = NCIDX + 1 )

!
!     Index of the root page number of the record tree:
!
      INTEGER               RTIDX
      PARAMETER           ( RTIDX  = NRIDX + 1 )

!
!     Index of the root page number of the character data page tree:
!
      INTEGER               CPTIDX
      PARAMETER           ( CPTIDX = RTIDX  + 1 )

!
!     Index of the root page number of the double precision data page
!     tree:
!
      INTEGER               DPTIDX
      PARAMETER           ( DPTIDX = CPTIDX + 1 )

!
!     Index of the root page number of the integer data page tree:
!
      INTEGER               IPTIDX
      PARAMETER           ( IPTIDX = DPTIDX + 1 )

!
!     Index of the `modified' flag:
!
      INTEGER               MFLIDX
      PARAMETER           ( MFLIDX = IPTIDX + 1 )

!
!     Index of the `initialized' flag:
!
      INTEGER               IFLIDX
      PARAMETER           ( IFLIDX = MFLIDX + 1 )

!
!     Index of the shadowing flag:
!
      INTEGER               SHDIDX
      PARAMETER           ( SHDIDX = IFLIDX + 1 )

!
!     Index of the companion file handle:
!
      INTEGER               CFHIDX
      PARAMETER           ( CFHIDX = SHDIDX + 1 )

!
!     Index of the companion segment number:
!
      INTEGER               CSNIDX
      PARAMETER           ( CSNIDX = CFHIDX + 1 )

!
!     The next three items are, respectively, the page numbers of the
!     last character, d.p., and integer data pages allocated by the
!     segment:
!
      INTEGER               LCPIDX
      PARAMETER           ( LCPIDX = CSNIDX + 1 )

      INTEGER               LDPIDX
      PARAMETER           ( LDPIDX = LCPIDX + 1 )

      INTEGER               LIPIDX
      PARAMETER           ( LIPIDX = LDPIDX + 1 )

!
!     The next three items are, respectively, the page-relative
!     indices of the last DAS word in use in the segment's
!     last character, d.p., and integer data pages:
!
      INTEGER               LCWIDX
      PARAMETER           ( LCWIDX = LIPIDX + 1 )

      INTEGER               LDWIDX
      PARAMETER           ( LDWIDX = LCWIDX + 1 )

      INTEGER               LIWIDX
      PARAMETER           ( LIWIDX = LDWIDX + 1 )

!
!     Index of the DAS character base address of the column name list:
!
      INTEGER               NMLIDX
      PARAMETER           ( NMLIDX = LIWIDX  + 1 )

!
!     The last descriptor element is reserved for future use.  No
!     parameter is defined to point to this location.
!

!
!     End Include Section:  EK Segment Descriptor Parameters
!
