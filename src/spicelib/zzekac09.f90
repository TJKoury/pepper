!$Procedure     ZZEKAC09 ( EK, add class 9 column to segment )

      SUBROUTINE ZZEKAC09 (  HANDLE,  SEGDSC,  COLDSC, &
                             CVALS,   NLFLGS,  WKINDX  )

! $ Abstract
!
!     Add an entire class 9 column to an EK segment.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE  'ekbool.inc'
      INCLUDE  'ekclas09.inc'
      INCLUDE  'ekcnamsz.inc'
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               NLFLGS ( * )
      INTEGER               WKINDX ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     CVALS      I   Character values to add to column.
!     NLFLGS     I   Array of null flags for column entries.
!     WKINDX    I-O  Work space for column index.
!
! $ Detailed_Input
!
!     HANDLE         the handle of an EK file that is open for writing.
!                    A `begin segment for fast load' operation must
!                    have already been performed for the designated
!                    segment.
!
!     SEGDSC         is a descriptor for the segment to which data is
!                    to be added.  The segment descriptor is not
!                    updated by this routine, but some fields in the
!                    descriptor will become invalid after this routine
!                    returns.
!
!     COLDSC         is a descriptor for the column to be added.  The
!                    column attributes must be filled in, but any
!                    pointers may be uninitialized.
!
!     CVALS          is an array containing the entire set of column
!                    entries for the specified column.  The entries
!                    are listed in row-order:  the column entry for the
!                    first row of the segment is first, followed by the
!                    column entry for the second row, and so on.  The
!                    number of column entries must match the declared
!                    number of rows in the segment.  Elements must be
!                    allocated for each column entry, including null
!                    entries.
!
!     NLFLGS         is an array of logical flags indicating whether
!                    the corresponding entries are null.  If the Ith
!                    element of NLFLGS is .FALSE., the Ith column entry
!                    defined by CVALS is added to the specified segment
!                    in the specified kernel file.
!
!                    If the Ith element of NLFGLS is .TRUE., the
!                    contents of the Ith column entry are undefined.
!
!                    NLFLGS is used only for columns that allow null
!                    values; it's ignored for other columns.
!
!     WKINDX         is a work space array used for building a column
!                    index.  If the column is indexed, the dimension of
!                    WKINDX must be at NROWS, where NROWS is the number
!                    of rows in the column.  If the column is not
!                    indexed, this work space is not used, so the
!                    dimension may be any positive value.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
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
!     This routine operates by side effects:  it modifies the named
!     EK file by adding data to the specified column.  This routine
!     writes the entire contents of the specified column in one shot.
!     This routine creates columns much more efficiently than can be
!     done by sequential calls to EKACEC, but has the drawback that
!     the caller must use more memory for the routine's inputs.  This
!     routine cannot be used to add data to a partially completed
!     column.
!
!     Class 9 columns have fixed record counts, and contain
!     fixed-length strings.
!
! $ Examples
!
!     See EKACLC.
!
! $ Restrictions
!
!     1)  This routine assumes the EK file has been set up
!         properly for a fast load operation.
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
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN
      CHARACTER*(PGSIZC)    PAGE

      INTEGER               CLASS
      INTEGER               CMBASE
      INTEGER               COLIDX
      INTEGER               DATBAS
      INTEGER               DSCBAS
      INTEGER               FROM
      INTEGER               IDXBAS
      INTEGER               IDXPAG
      INTEGER               IDXTYP
      INTEGER               P
      INTEGER               L
      INTEGER               MBASE
      INTEGER               NFLBAS
      INTEGER               NFLPAG
      INTEGER               NPAGE
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               SPP
      INTEGER               TO

      LOGICAL               FIXLEN
      LOGICAL               INDEXD
      LOGICAL               NULLOK

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC09' )
      END IF

!
!     Grab the column's attributes.  Initialize the maximum non-blank
!     width of the column.
!
      CLASS  =  COLDSC ( CLSIDX )
      IDXTYP =  COLDSC ( IXTIDX )
      NULPTR =  COLDSC ( NFLIDX )
      COLIDX =  COLDSC ( ORDIDX )
      L      =  COLDSC ( LENIDX )

      NULLOK =  NULPTR .NE. IFALSE
      INDEXD =  IDXTYP .NE. IFALSE
      FIXLEN =  L      .NE. IFALSE

!
!     This column had better be class 9.
!
      IF ( CLASS .NE. 9 ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL SETMSG ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 9.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC09'                                      )
         RETURN

      END IF

!
!     Make sure the column has fixed-length strings.
!
      IF  ( .NOT. FIXLEN ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL SETMSG ( 'Column # has variable string length; class '  // &
                       '9 supports fixed-length strings only.'         )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKAC09'                                      )
         RETURN

      END IF

!
!     Check the input string length.
!
      IF (      ( L .LT. 0             ) &
           .OR. ( L .GT. LEN(CVALS(1)) ) &
           .OR. ( L .GT. CPSIZE        )  ) THEN

         CALL SETMSG ( 'String length # is just plain wrong.' )
         CALL ERRINT ( '#',  L                                )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                   )
         CALL CHKOUT ( 'ZZEKAC09'                             )
         RETURN

      END IF

!
!     Compute the number of strings we can hold in one page.
!
      SPP  =  CPSIZE / L

!
!     Find the number of rows in the segment.
!
      NROWS = SEGDSC ( NRIDX )

!
!     Decide how many pages are required to hold the array, and
!     allocate that many new, contiguous pages.
!
      NPAGE  =  ( NROWS + SPP - 1 ) / SPP

      CALL ZZEKACPS ( HANDLE, SEGDSC, CHR, NPAGE, P, DATBAS )

!
!     We'll use FROM to indicate the element of CVALS we're
!     considering and TO to indicate the element of PAGE to write
!     to.
!
      TO   =  1
      PAGE =  ' '


      DO FROM = 1, NROWS
!
!        The Assignment.
!
         IF (  ( .NOT. NULLOK ) .OR. ( .NOT. NLFLGS(FROM) )  ) THEN

            PAGE( TO : TO+L-1 ) =  CVALS(FROM)

         END IF

         TO  =  TO + L


         IF (      ( TO   .GT. CPSIZE-L+1 ) &
              .OR. ( FROM .EQ. NROWS      )  ) THEN
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.
!
            CALL ZZEKPGWC ( HANDLE, P, PAGE  )

!
!           Set the link count.
!
            CALL ZZEKSLNK ( HANDLE, CHR, P,  (TO-L)/L  )

!
!           Next page.
!
            P   =  P + 1
            TO  =  1

         END IF

      END DO

!
!     Update the column's metadata area to point to the data array.
!
      CMBASE = COLDSC(METIDX)

      CALL DASUDI ( HANDLE, CMBASE+DBIX09, CMBASE+DBIX09, DATBAS )

!
!     If the column is supposed to have an index, now is the time to
!     build that index.  Type 2 indexes are just order vectors.
!
      IF ( INDEXD ) THEN
!
!        Compute the order vector.
!
         CALL ZZEKORDC ( CVALS,  NULLOK, NLFLGS, NROWS, WKINDX )

!
!        Write out the index.
!
         CALL ZZEKWPAI ( HANDLE, SEGDSC, NROWS, WKINDX, IDXPAG, IDXBAS )

!
!        Update the column's metadata to point to the index.  The
!        pointer indicates base address of the index.  Also set the
!        index type in the column descriptor.
!
         MBASE   =  SEGDSC ( IMDIDX )
         DSCBAS  =  MBASE  +  SDSCSZ  +  (COLIDX-1)*CDSCSZ

         CALL DASUDI ( HANDLE,  DSCBAS+IXPIDX,  DSCBAS+IXPIDX,  IDXBAS )
         CALL DASUDI ( HANDLE,  DSCBAS+IXTIDX,  DSCBAS+IXTIDX,  2      )

      END IF


      IF ( NULLOK ) THEN
!
!        Nulls are allowed.  Write out the null flag array.
!
         CALL ZZEKWPAL ( HANDLE, SEGDSC, NROWS, NLFLGS, NFLPAG, NFLBAS )

!
!        Update the column's metadata area to point to the null flag
!        array.
!
         CALL DASUDI ( HANDLE, CMBASE+NFIX09, CMBASE+NFIX09, NFLBAS )

      END IF

      CALL CHKOUT ( 'ZZEKAC09' )
      RETURN
      END
