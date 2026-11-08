!$Procedure      ZZEKBS02 ( EK, begin segment, type 1 )

      subroutine zzekbs02 (  handle,  tabnam,  ncols, &
                             cnames,  cdscrs,  segno  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Start a new type 2 segment in an E-kernel.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!
! $ Declarations


      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekcnamsz.inc'
      include 'ekdatpag.inc'
      include 'ekfilpar.inc'
      include 'ekglimit.inc'
      include 'ekpage.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektnamsz.inc'
      include 'ektype.inc'

      integer(ip)           handle
      character(*)          tabnam
      integer(ip)           ncols
      character(*)          cnames ( * )
      integer(ip)           cdscrs ( cdscsz, * )
      integer(ip)           segno

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TABNAM     I   Table name.
!     NCOLS      I   Number of columns in the segment.
!     CNAMES     I   Names of columns.
!     CDSCRS    I-O  Descriptors of columns.
!     SEGNO      O   Segment number.
!
! $ Detailed_Input
!
!     HANDLE         the handle of an EK file that is open for writing.
!
!     TABNAM         is the name of the EK table to which the current
!                    segment belongs.  All segments in the EK file
!                    designated by HANDLE must have identical column
!                    attributes. TABNAM must not exceed 32 characters
!                    in length.  Case is not significant.  Table names
!                    must start with a letter and contain only
!                    characters from the set {A-Z,a-z,0-9,$,_}.
!
!     NCOLS          is the number of columns in a new segment.
!
!     CNAMES,
!     CDSCRS         are, respectively, and array of column names and
!                    their corresponding descriptors:  the Ith element
!                    of CNAMES and the Ith descriptor apply to
!                    the Ith column in the segment.
!
!
! $ Detailed_Output
!
!     CDSCRS         are the input column descriptors, with their name
!                    base and ordinal position elements filled in.
!
!     SEGNO          is the number of the segment created by this
!                    routine.  Segment numbers are used as unique
!                    identifiers by other EK access routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it prepares an EK for
!     the addition of a new type 2 segment.  Type 2 segments have
!     fixed record counts:  they do not support record insertion,
!     or deletion operations.  They do not support arbitrary column
!     entry update operations either, since some updates change the
!     size of the affected entries.
!
!     Type 2 segments may contains columns of class 7 through 9.
!
!     By way of contrast, type 1 segments support variable record
!     counts.
!
! $ Examples
!
!     See EKBSEG.
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
!-    Beta Version 1.0.0, 17-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           eknseg

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           mxspec
      parameter           ( mxspec = 512 )

      integer(ip)           namlim
      parameter           ( namlim = 32 )

!
!     Local variables
!
      character(pgsizc)     cpage
      character(cnamsz)     tmpcnm
      character(tnamsz)     tmptnm

      integer(ip)           base
      integer(ip)           cbase
      integer(ip)           cp1
      integer(ip)           cp
      integer(ip)           cpagno
      integer(ip)           cpt
      integer(ip)           dpt
      integer(ip)           dscbas
      integer(ip)           i
      integer(ip)           ipage  ( pgsizi )
      integer(ip)           ipagno
      integer(ip)           ipt
      integer(ip)           metasz
      integer(ip)           nambas
      integer(ip)           ncpage
      integer(ip)           nipage
      integer(ip)           p
      integer(ip)           p1
      integer(ip)           p1base
      integer(ip)           room
      integer(ip)           sgtree

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return


!
!     The metadata layout has the following form:
!
!        +------------------------------------------+
!        |                                          |
!        |            segment descriptor            |
!        |                                          |
!        +------------------------------------------+
!        |            column descriptor 1           |
!        +------------------------------------------+
!        |            column descriptor 2           |
!        +------------------------------------------+
!                              .
!                              .
!                              .
!        +------------------------------------------+
!        |           column descriptor m            |
!        +------------------------------------------+
!
!     The column descriptors may span multiple pages, but they
!     always occupy contiguous DAS integer addresses.
!
!     In addition, the metadata area includes a character page
!     that contains the segment's table name and the table's
!     column names.
!
!     Calculate the number of contiguous integer pages we'll need.
!     This value is a function of the number of columns.
!
      metasz  =    sdscsz  +  ncols * cdscsz

      nipage  =  ( metasz  +  pgsizi  -  1 ) / pgsizi
!
!     Allocate NIPAGE new integer pages.  Insisting on new pages
!     enforces contiguity.  Also allocate one character page, which
!     need not be new.
!
      call zzekpgan ( handle, int, p1, p1base )

      do i = 2, nipage
         call zzekpgan ( handle, int, p, base )
      end do

!
!     Calculate the number of contiguous character pages we'll need.
!
      ncpage  =  ( tnamsz  +  ncols * cnamsz  +  pgsizc - 1 ) / pgsizc

      call zzekpgan ( handle, chr, cp1, cbase )

      do i = 2, ncpage
         call zzekpgan ( handle, chr, p, base )
      end do


!
!     On the third day of Christmas, we initialized three data page
!     trees:  one for each data type.
!
      call zzektrit ( handle, cpt  )
      call zzektrit ( handle, dpt )
      call zzektrit ( handle, ipt )

!
!     Prepare the contents of the first integer page:  initialize
!     everything other than the column descriptors.
!
!     The last data word in use for each data type is initialized
!     to indicate that no room is left in the current page.  This
!     forces allocation of a new page when data must be added.  The
!     `last word' counts of each type for both the data and modified
!     record trees are initialized in this fashion.
!
      call cleari ( pgsizi, ipage )
!
!     The value at index EKTIDX is the segment type.
!
      ipage ( ektidx )  =  2
      ipage ( snoidx )  =  eknseg ( handle )  +  1
      ipage ( imdidx )  =  p1base
      ipage ( tnmidx )  =  cbase
      ipage ( ncidx  )  =  ncols
      ipage ( nridx  )  =  0
      ipage ( rtidx  )  =  0
      ipage ( cptidx )  =  cpt
      ipage ( dptidx )  =  dpt
      ipage ( iptidx )  =  ipt
      ipage ( mflidx )  =  itrue
      ipage ( iflidx )  =  ifalse
      ipage ( shdidx )  =  ifalse
      ipage ( cfhidx )  =  0
      ipage ( csnidx )  =  0
      ipage ( lcpidx )  =  0
      ipage ( ldpidx )  =  0
      ipage ( lipidx )  =  0
      ipage ( lcwidx )  =  cpsize
      ipage ( ldwidx )  =  dpsize
      ipage ( liwidx )  =  ipsize
      ipage ( nmlidx )  =  cbase + tnamsz

!
!     Initialize the character metadata page:  fill in the table name.
!     The table name gets converted to upper case and is left justified.
!
      cpage  =  ' '

      call ljust  ( tabnam, tmptnm )
      call ucase  ( tmptnm, tmptnm )

      cpage ( :tnamsz )  =  tmptnm

!
!     Now for the column-specific tasks.  We write out a descriptor for
!     each column.  At the same time, we write out the column's name.
!
      ipagno = 1
      cpagno = 1
      p      = p1
      cp     = cp1

      do i = 1, ncols
!
!        Insert the column's ordinal position in the segment into
!        the column's descriptor.
!
         cdscrs( ordidx, i )  =  i

!
!        Write the Ith column name into the character metdata page.  (We
!        know the name is non-blank.)  Blank-pad the name on the right,
!        up to a length of CNAMSZ characters, if necessary.  Convert the
!        name to upper case as well.
!
         call ucase  ( cnames(i), tmpcnm )

         nambas  =  tnamsz  +  (i-1)*cnamsz  -  (cpagno-1)*pgsizc
         room    =  pgsizc  -  nambas

         if ( cnamsz <= room ) then

            cpage ( nambas+1 : nambas+cnamsz ) =  tmpcnm

!
!           Fill the column name's base address into the descriptor.
!
            cdscrs( namidx, i ) =  cbase + (cpagno-1)*pgsizc + nambas

         else
!
!           Some or all of the column name will overflow onto the next
!           page.
!
            if ( room > 0 ) then

               cpage ( nambas+1 : nambas+room ) = tmpcnm(:room)

               cdscrs( namidx, i ) =  cbase + (cpagno-1)*pgsizc + nambas

            else

               cdscrs( namidx, i ) =  cbase +  cpagno*pgsizc

            end if

!
!           Write out the page we just filled up.
!
            call zzekpgwc ( handle, cp, cpage )

!
!           The next character page will hold the overflow.  The next
!           page is the successor of page CP, since we allocated
!           consecutive character pages.
!
            cp       =  cp     + 1
            cpagno   =  cpagno + 1
            cpage    =  tmpcnm(room+1:)

         end if

!
!        Add the column descriptor to the metadata page, if the
!        descriptor will fit.  We may need to allocate another page
!        to hold the descriptor.
!
         dscbas  =  sdscsz  +  (i-1)*cdscsz  -  (ipagno-1)*pgsizi
         room    =  pgsizi  -  dscbas

         if ( cdscsz <= room ) then
!
!           The whole descriptor fits in the current page.
!
            call movei ( cdscrs(1,i), cdscsz, ipage(dscbas+1) )


         else
!
!           Some or all of the descriptor will overflow onto the next
!           page.
!
            if ( room > 0 ) then
               call movei ( cdscrs(1,i), room, ipage(dscbas+1) )
            end if

!
!           Write out the page we just filled up.
!
            call zzekpgwi ( handle, p, ipage )

!
!           The next integer page will hold the overflow.  The next page
!           is the successor of page P, since we allocated consecutive
!           pages.
!
            p       =  p + 1
            ipagno  =  ipagno + 1

            call cleari ( pgsizi,                        ipage )
            call movei  ( cdscrs(room+1,i), cdscsz-room, ipage )

         end if

!
!        If we encountered a DAS error, leave now.
!
         if ( failed() ) return


      end do

!
!     Write out the last integer metadata page, and write out the
!     character metadata page.
!
      call zzekpgwi ( handle, p,  ipage )
      call zzekpgwc ( handle, cp, cpage )

!
!     At this point, the segment's metadata is filled in.  We must
!     update the file's segment list information to account for this
!     segment.  All we need do is add a new entry to the file's
!     segment pointer tree.  First, look up the tree.
!
      call zzekpgbs ( int, 1, base )

      call dasrdi   ( handle, base+sgtidx, base+sgtidx, sgtree )

!
!     Append the head node of this segment at the end of the segment
!     tree.  The tree will point to the first integer metadata page of
!     the new segment.
!
      call zzektrap ( handle, sgtree, p1, segno )

      return
      end
