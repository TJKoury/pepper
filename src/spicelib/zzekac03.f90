!$Procedure     ZZEKAC03 ( EK, add class 3 column to segment )

      SUBROUTINE ZZEKAC03 (  HANDLE,  SEGDSC,  COLDSC,  CVALS, &
                             NLFLGS,  RCPTRS,  WKINDX  )

! $ Abstract
!
!     Add an entire class 3 column to an EK segment.
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
      INCLUDE  'ekcnamsz.inc'
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'ekrecptr.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               NLFLGS ( * )
      INTEGER               RCPTRS ( * )
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
!     RCPTRS     I   Array of record pointers for segment.
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
!     RCPTRS         is an array of record pointers for the input
!                    segment.  These pointers are base addresses of the
!                    `record pointer structures' for the segment.
!                    These pointers are used instead of record numbers
!                    in column indexes:  the indexes map ordinal
!                    positions to record pointers.
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
! $ Examples
!
!     See EKACLC.
!
! $ Restrictions
!
!     1)  This routine assumes the EK scratch area has been set up
!         properly for a fast load operation.  This routine writes
!         to the EK scratch area as well.
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
!-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               RTRIM

      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = CPSIZE )

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN
      CHARACTER*(PGSIZC)    PAGE

      INTEGER               ADRBUF ( BUFSIZ )
      INTEGER               BUFPTR
      INTEGER               CLASS
      INTEGER               COLIDX
      INTEGER               COLWID
      INTEGER               DSCBAS
      INTEGER               FROM
      INTEGER               I
      INTEGER               IDXTYP
      INTEGER               P
      INTEGER               P2
      INTEGER               PBASE
      INTEGER               POS
      INTEGER               MBASE
      INTEGER               N
      INTEGER               NCHARS
      INTEGER               NDATA
      INTEGER               NLINKS
      INTEGER               NNULL
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               NWRITE
      INTEGER               REMAIN
      INTEGER               ROOM
      INTEGER               STRLEN
      INTEGER               TO
      INTEGER               TREE

      LOGICAL               FIXLEN
      LOGICAL               INDEXD
      LOGICAL               NULLOK

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC03' )
      END IF

!
!     Grab the column's attributes.  Initialize the maximum non-blank
!     width of the column.
!
      CLASS  =  COLDSC ( CLSIDX )
      IDXTYP =  COLDSC ( IXTIDX )
      NULPTR =  COLDSC ( NFLIDX )
      COLIDX =  COLDSC ( ORDIDX )
      COLWID =  COLDSC ( LENIDX )

      NULLOK =  NULPTR .NE. IFALSE
      INDEXD =  IDXTYP .NE. IFALSE
      FIXLEN =  COLWID .NE. IFALSE

!
!     This column had better be class 3.
!
      IF ( CLASS .NE. 3 ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL SETMSG ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 3.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC03'                                      )
         RETURN

      END IF

!
!     If the column is indexed, the index type should be 1; we don't
!     know how to create any other type of index.
!
      IF  (  INDEXD  .AND.  ( IDXTYP .NE. 1 )  ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL SETMSG ( 'Index type code # found in descriptor for '   // &
                       'column #.  Code should be 1.'                  )
         CALL ERRINT ( '#', IDXTYP                                     )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(UNRECOGNIZEDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKAC03'                                      )
         RETURN

      END IF

!
!     Push the column's ordinal index on the stack.  This allows us
!     to identify the column the addresses belong to.
!
      CALL ZZEKSPSH ( 1, COLIDX )

!
!     Find the number of rows in the segment.
!
      NROWS   =  SEGDSC ( NRIDX )

!
!     Count the number of strings to write.
!
      IF ( NULLOK ) THEN
!
!        Count the non-null column entries; these are the
!        ones that will take up space.
!
         NNULL  =  0
         NDATA  =  0

         DO I = 1, NROWS

            IF ( NLFLGS(I) ) THEN
               NNULL  =  NNULL + 1
            END IF

         END DO

         NDATA  =   NROWS  -  NNULL

      ELSE

         NDATA  =   NROWS

      END IF


      IF ( NDATA .GT. 0 ) THEN
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         CALL ZZEKAPS ( HANDLE, SEGDSC, CHR, .FALSE., P, PBASE )
         PAGE  =  ' '

!
!        The link count starts out at zero.
!
         CALL PRTENC ( 0,  PAGE(CLCIDX:CLCIDX+ENCSIZ-1) )

      END IF

!
!     Write the input data out to the target file a page at a time.
!     Null values don't get written.
!
!     While we're at it, we'll push onto the EK stack the addresses
!     of the column entries.  We use the constant NULL rather than an
!     address to represent null entries.
!
!     We'll use FROM to indicate the element of CVALS we're
!     considering, TO to indicate the first character of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.  The variable N indicates the number of characters
!     written to the current page.  NCHARS indicates the number of
!     characters left to write from the current input element.  NWRITE
!     will be used to count the column entries written so far.
!

      REMAIN  =  NROWS
      FROM    =  0
      TO      =  1
      BUFPTR  =  1
      NWRITE  =  0
      N       =  0

      DO WHILE ( REMAIN .GT. 0 )
!
!        Examine a column entry.  Write it out if it's non-null.
!
         FROM  =  FROM   + 1


         IF (  NULLOK  .AND.  ( NLFLGS(FROM) )  ) THEN

            ADRBUF(BUFPTR) =  NULL

         ELSE
!
!           Write out the current column entry.  The entry
!           might span multiple pages.  However, we're guaranteed
!           enough room to write out to the current page the encoded
!           character count and at least one character of data.
!
!           Update the non-blank width for the column each time we
!           determine the length of an input string.
!
            IF ( FIXLEN ) THEN
               STRLEN  =  MIN (  RTRIM( CVALS(FROM) ),  COLWID  )
            ELSE
               STRLEN  =  RTRIM( CVALS(FROM) )
            END IF

            ADRBUF(BUFPTR) =  TO  +  PBASE
            POS            =  1

!
!           Start out with the string length.
!
            CALL PRTENC (  STRLEN,  PAGE(TO : TO+ENCSIZ-1)  )

            N       =   N  +  ENCSIZ
            TO      =   N  +  1
            NCHARS  =   STRLEN


            DO WHILE ( NCHARS .GT. 0 )

               ROOM  =  CPSIZE - N

               IF ( NCHARS .LE. ROOM ) THEN
!
!                 The remaining portion of the string will fit on the
!                 current page.
!
                  PAGE(TO:TO+NCHARS-1) =  CVALS(FROM) (POS:POS+NCHARS-1)
                  N                    =  N   +  NCHARS
                  TO                   =  N   +  1
                  NCHARS               =  0

!
!                 Add a link to the current page.
!

                  CALL PRTDEC ( PAGE(CLCIDX:CLCIDX+ENCSIZ-1),   NLINKS )

                  CALL PRTENC ( NLINKS+1, PAGE(CLCIDX:CLCIDX+ENCSIZ-1) )


               ELSE
!
!                 The string will have to be continued on another page.
!                 Write out the first ROOM characters to the current
!                 page first.
!
                  PAGE(TO:CPSIZE)  =  CVALS(FROM) (POS:POS+ROOM-1)
                  POS              =  POS    + ROOM
                  NCHARS           =  NCHARS - ROOM

!
!                 Add a link to the current page.
!
                  CALL PRTDEC ( PAGE(CLCIDX:CLCIDX+ENCSIZ-1),   NLINKS )

                  CALL PRTENC ( NLINKS+1, PAGE(CLCIDX:CLCIDX+ENCSIZ-1) )

!
!                 Allocate another page.  Fill in the forward pointer
!                 in the previous page.
!
                  CALL ZZEKAPS (  HANDLE,  SEGDSC,  CHR, &
                                 .FALSE.,  P2,      PBASE )

                  CALL PRTENC   ( P2,  PAGE(CFPIDX:CFPIDX+ENCSIZ-1) )

!
!                 Write out the full data page.  Get ready to write
!                 to the new page.
!
                  CALL ZZEKPGWC ( HANDLE, P, PAGE  )

                  P    =  P2
                  PAGE = ' '
                  CALL PRTENC ( 0,  PAGE(CLCIDX:CLCIDX+ENCSIZ-1) )

                  N    =  0
                  TO   =  1

               END IF

            END DO
!
!           We've written out a column entry.
!
            NWRITE  =  NWRITE + 1


         END IF
!
!        We're done with the current column entry, null or not.
!

         IF ( NWRITE .LT. NDATA ) THEN
!
!           There is at least one more column entry to write.
!           If there's not enough room on the current page to begin
!           writing another column entry, write out the page and
!           allocate another.
!
            ROOM  =  CPSIZE - N

            IF ( ROOM  .LT.  ( 1 + ENCSIZ )  ) THEN

               CALL ZZEKPGWC ( HANDLE, P,      PAGE  )
               CALL ZZEKAPS  ( HANDLE, SEGDSC, CHR, .FALSE., P, PBASE )

               PAGE  =  ' '
               CALL PRTENC ( 0,  PAGE(CLCIDX:CLCIDX+ENCSIZ-1) )

               N     =  0
               TO    =  1

            END IF


         ELSE IF ( N .GT. 0 ) THEN
!
!           We've written the last of the non-null data to the current
!           page.  Write out this page.
!
            CALL ZZEKPGWC ( HANDLE,  P,  PAGE  )
            N  =  0

         END IF


         REMAIN  =  REMAIN - 1


         IF (  ( BUFPTR .EQ. BUFSIZ ) .OR. ( REMAIN .EQ. 0 )  ) THEN
!
!           The address buffer is full or we're out of input values
!           to look at, so push the buffer contents on the stack.
!
            CALL ZZEKSPSH ( BUFPTR, ADRBUF )
            BUFPTR  =  1

         ELSE

            BUFPTR  =  BUFPTR + 1

         END IF

      END DO

!
!     If the column is supposed to have an index, now is the time to
!     build that index.  We'll find the order vector for the input
!     values, overwrite the elements of the order vector with the
!     corresponding elements of the input array of record pointers, then
!     load this sorted copy of the record pointer array into a tree in
!     one shot.
!
      IF ( INDEXD ) THEN

         CALL ZZEKORDC ( CVALS,  NULLOK, NLFLGS, NROWS, WKINDX )

         DO I = 1, NROWS
            WKINDX(I) =  RCPTRS( WKINDX(I) )
         END DO

         CALL ZZEKTRIT ( HANDLE, TREE                )
         CALL ZZEKTR1S ( HANDLE, TREE, NROWS, WKINDX )

!
!        Update the segment's metadata to point to the index.  The
!        pointer indicates the root page of the tree.
!
         MBASE   =  SEGDSC ( IMDIDX )
         DSCBAS  =  MBASE  +  SDSCSZ  +  (COLIDX-1)*CDSCSZ

         CALL DASUDI ( HANDLE,  DSCBAS+IXPIDX,  DSCBAS+IXPIDX,  TREE )

      END IF


      CALL CHKOUT ( 'ZZEKAC03' )
      RETURN
      END
