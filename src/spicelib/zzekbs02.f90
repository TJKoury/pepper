!$Procedure      ZZEKBS02 ( EK, begin segment, type 1 )

      SUBROUTINE ZZEKBS02 (  HANDLE,  TABNAM,  NCOLS, &
                             CNAMES,  CDSCRS,  SEGNO  )

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


      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekfilpar.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         TABNAM
      INTEGER               NCOLS
      CHARACTER*(*)         CNAMES ( * )
      INTEGER               CDSCRS ( CDSCSZ, * )
      INTEGER               SEGNO

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
      INTEGER               EKNSEG

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MXSPEC
      PARAMETER           ( MXSPEC = 512 )

      INTEGER               NAMLIM
      PARAMETER           ( NAMLIM = 32 )

!
!     Local variables
!
      CHARACTER*(PGSIZC)    CPAGE
      CHARACTER*(CNAMSZ)    TMPCNM
      CHARACTER*(TNAMSZ)    TMPTNM

      INTEGER               BASE
      INTEGER               CBASE
      INTEGER               CP1
      INTEGER               CP
      INTEGER               CPAGNO
      INTEGER               CPT
      INTEGER               DPT
      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               IPAGE  ( PGSIZI )
      INTEGER               IPAGNO
      INTEGER               IPT
      INTEGER               METASZ
      INTEGER               NAMBAS
      INTEGER               NCPAGE
      INTEGER               NIPAGE
      INTEGER               P
      INTEGER               P1
      INTEGER               P1BASE
      INTEGER               ROOM
      INTEGER               SGTREE

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKBS02' )
      END IF

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKBS02' )
         RETURN
      END IF


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
      METASZ  =    SDSCSZ  +  NCOLS * CDSCSZ

      NIPAGE  =  ( METASZ  +  PGSIZI  -  1 ) / PGSIZI
!
!     Allocate NIPAGE new integer pages.  Insisting on new pages
!     enforces contiguity.  Also allocate one character page, which
!     need not be new.
!
      CALL ZZEKPGAN ( HANDLE, INT, P1, P1BASE )

      DO I = 2, NIPAGE
         CALL ZZEKPGAN ( HANDLE, INT, P, BASE )
      END DO

!
!     Calculate the number of contiguous character pages we'll need.
!
      NCPAGE  =  ( TNAMSZ  +  NCOLS * CNAMSZ  +  PGSIZC - 1 ) / PGSIZC

      CALL ZZEKPGAN ( HANDLE, CHR, CP1, CBASE )

      DO I = 2, NCPAGE
         CALL ZZEKPGAN ( HANDLE, CHR, P, BASE )
      END DO


!
!     On the third day of Christmas, we initialized three data page
!     trees:  one for each data type.
!
      CALL ZZEKTRIT ( HANDLE, CPT  )
      CALL ZZEKTRIT ( HANDLE, DPT )
      CALL ZZEKTRIT ( HANDLE, IPT )

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
      CALL CLEARI ( PGSIZI, IPAGE )
!
!     The value at index EKTIDX is the segment type.
!
      IPAGE ( EKTIDX )  =  2
      IPAGE ( SNOIDX )  =  EKNSEG ( HANDLE )  +  1
      IPAGE ( IMDIDX )  =  P1BASE
      IPAGE ( TNMIDX )  =  CBASE
      IPAGE ( NCIDX  )  =  NCOLS
      IPAGE ( NRIDX  )  =  0
      IPAGE ( RTIDX  )  =  0
      IPAGE ( CPTIDX )  =  CPT
      IPAGE ( DPTIDX )  =  DPT
      IPAGE ( IPTIDX )  =  IPT
      IPAGE ( MFLIDX )  =  ITRUE
      IPAGE ( IFLIDX )  =  IFALSE
      IPAGE ( SHDIDX )  =  IFALSE
      IPAGE ( CFHIDX )  =  0
      IPAGE ( CSNIDX )  =  0
      IPAGE ( LCPIDX )  =  0
      IPAGE ( LDPIDX )  =  0
      IPAGE ( LIPIDX )  =  0
      IPAGE ( LCWIDX )  =  CPSIZE
      IPAGE ( LDWIDX )  =  DPSIZE
      IPAGE ( LIWIDX )  =  IPSIZE
      IPAGE ( NMLIDX )  =  CBASE + TNAMSZ

!
!     Initialize the character metadata page:  fill in the table name.
!     The table name gets converted to upper case and is left justified.
!
      CPAGE  =  ' '

      CALL LJUST  ( TABNAM, TMPTNM )
      CALL UCASE  ( TMPTNM, TMPTNM )

      CPAGE ( :TNAMSZ )  =  TMPTNM

!
!     Now for the column-specific tasks.  We write out a descriptor for
!     each column.  At the same time, we write out the column's name.
!
      IPAGNO = 1
      CPAGNO = 1
      P      = P1
      CP     = CP1

      DO I = 1, NCOLS
!
!        Insert the column's ordinal position in the segment into
!        the column's descriptor.
!
         CDSCRS( ORDIDX, I )  =  I

!
!        Write the Ith column name into the character metdata page.  (We
!        know the name is non-blank.)  Blank-pad the name on the right,
!        up to a length of CNAMSZ characters, if necessary.  Convert the
!        name to upper case as well.
!
         CALL UCASE  ( CNAMES(I), TMPCNM )

         NAMBAS  =  TNAMSZ  +  (I-1)*CNAMSZ  -  (CPAGNO-1)*PGSIZC
         ROOM    =  PGSIZC  -  NAMBAS

         IF ( CNAMSZ .LE. ROOM ) THEN

            CPAGE ( NAMBAS+1 : NAMBAS+CNAMSZ ) =  TMPCNM

!
!           Fill the column name's base address into the descriptor.
!
            CDSCRS( NAMIDX, I ) =  CBASE + (CPAGNO-1)*PGSIZC + NAMBAS

         ELSE
!
!           Some or all of the column name will overflow onto the next
!           page.
!
            IF ( ROOM .GT. 0 ) THEN

               CPAGE ( NAMBAS+1 : NAMBAS+ROOM ) = TMPCNM(:ROOM)

               CDSCRS( NAMIDX, I ) =  CBASE + (CPAGNO-1)*PGSIZC + NAMBAS

            ELSE

               CDSCRS( NAMIDX, I ) =  CBASE +  CPAGNO*PGSIZC

            END IF

!
!           Write out the page we just filled up.
!
            CALL ZZEKPGWC ( HANDLE, CP, CPAGE )

!
!           The next character page will hold the overflow.  The next
!           page is the successor of page CP, since we allocated
!           consecutive character pages.
!
            CP       =  CP     + 1
            CPAGNO   =  CPAGNO + 1
            CPAGE    =  TMPCNM(ROOM+1:)

         END IF

!
!        Add the column descriptor to the metadata page, if the
!        descriptor will fit.  We may need to allocate another page
!        to hold the descriptor.
!
         DSCBAS  =  SDSCSZ  +  (I-1)*CDSCSZ  -  (IPAGNO-1)*PGSIZI
         ROOM    =  PGSIZI  -  DSCBAS

         IF ( CDSCSZ .LE. ROOM ) THEN
!
!           The whole descriptor fits in the current page.
!
            CALL MOVEI ( CDSCRS(1,I), CDSCSZ, IPAGE(DSCBAS+1) )


         ELSE
!
!           Some or all of the descriptor will overflow onto the next
!           page.
!
            IF ( ROOM .GT. 0 ) THEN
               CALL MOVEI ( CDSCRS(1,I), ROOM, IPAGE(DSCBAS+1) )
            END IF

!
!           Write out the page we just filled up.
!
            CALL ZZEKPGWI ( HANDLE, P, IPAGE )

!
!           The next integer page will hold the overflow.  The next page
!           is the successor of page P, since we allocated consecutive
!           pages.
!
            P       =  P + 1
            IPAGNO  =  IPAGNO + 1

            CALL CLEARI ( PGSIZI,                        IPAGE )
            CALL MOVEI  ( CDSCRS(ROOM+1,I), CDSCSZ-ROOM, IPAGE )

         END IF

!
!        If we encountered a DAS error, leave now.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZEKBS02' )
            RETURN
         END IF


      END DO

!
!     Write out the last integer metadata page, and write out the
!     character metadata page.
!
      CALL ZZEKPGWI ( HANDLE, P,  IPAGE )
      CALL ZZEKPGWC ( HANDLE, CP, CPAGE )

!
!     At this point, the segment's metadata is filled in.  We must
!     update the file's segment list information to account for this
!     segment.  All we need do is add a new entry to the file's
!     segment pointer tree.  First, look up the tree.
!
      CALL ZZEKPGBS ( INT, 1, BASE )

      CALL DASRDI   ( HANDLE, BASE+SGTIDX, BASE+SGTIDX, SGTREE )

!
!     Append the head node of this segment at the end of the segment
!     tree.  The tree will point to the first integer metadata page of
!     the new segment.
!
      CALL ZZEKTRAP ( HANDLE, SGTREE, P1, SEGNO )

      CALL CHKOUT ( 'ZZEKBS02' )
      RETURN
      END
