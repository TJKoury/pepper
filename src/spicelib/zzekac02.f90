!$Procedure     ZZEKAC02 ( EK, add class 2 column to segment )

      SUBROUTINE ZZEKAC02 (  HANDLE,  SEGDSC,  COLDSC,  DVALS, &
                             NLFLGS,  RCPTRS,  WKINDX         )

! $ Abstract
!
!     Add an entire class 2 column to an EK segment.
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
      DOUBLE PRECISION      DVALS  ( * )
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
!     DVALS      I   D.p. values to add to column.
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
!     DVALS          is an array containing the entire set of column
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
!                    defined by DVALS is added to the specified segment
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
!     done by sequential calls to EKACED, but has the drawback that
!     the caller must use more memory for the routine's inputs.  This
!     routine cannot be used to add data to a partially completed
!     column.
!
! $ Examples
!
!     See EKACLD.
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
!-    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB)
!
!        Bug fix:  case of 100% null data values is now handled
!        correctly.  Previous version line was changed from "Beta"
!        to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB)
!
!        Bug fix:  case of 100% null data values is now handled
!        correctly.  The test to determine when to write a page
!        was fixed to handle this case.
!
!        Previous version line was changed from "Beta"
!        to "SPICELIB."
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DPSIZE )

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      DOUBLE PRECISION      PAGE   ( PGSIZD )

      INTEGER               ADRBUF ( BUFSIZ )
      INTEGER               BUFPTR
      INTEGER               CLASS
      INTEGER               COLIDX
      INTEGER               DSCBAS
      INTEGER               FROM
      INTEGER               I
      INTEGER               IDXTYP
      INTEGER               P
      INTEGER               PBASE
      INTEGER               MBASE
      INTEGER               N
      INTEGER               NDATA
      INTEGER               NNULL
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               NWRITE
      INTEGER               REMAIN
      INTEGER               TO
      INTEGER               TREE

      LOGICAL               INDEXD
      LOGICAL               NULLOK

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC02' )
      END IF

!
!     Grab the column's attributes.
!
      CLASS  =  COLDSC( CLSIDX )
      IDXTYP =  COLDSC( IXTIDX )
      NULPTR =  COLDSC( NFLIDX )
      COLIDX =  COLDSC ( ORDIDX )

      NULLOK =  NULPTR .NE. IFALSE
      INDEXD =  IDXTYP .NE. IFALSE

!
!     This column had better be class 2.
!
      IF ( CLASS .NE. 2 ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL SETMSG ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 2.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC02'                                      )
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
         CALL CHKOUT ( 'ZZEKAC02'                                      )
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
!     Decide how many pages we'll need to store the data.  Also
!     record the number of data values to write.
!
      IF ( NULLOK ) THEN
!
!        Count the non-null rows; these are the ones that will take
!        up space.
!
         NNULL  =  0

         DO I = 1, NROWS

            IF ( NLFLGS(I) ) THEN
               NNULL  =  NNULL + 1
            END IF

         END DO

         NDATA  =  NROWS - NNULL


      ELSE

         NDATA  =  NROWS

      END IF


      IF ( NDATA .GT. 0 ) THEN
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         CALL ZZEKAPS  ( HANDLE, SEGDSC, DP, .FALSE., P, PBASE )
         CALL CLEARD   ( PGSIZD, PAGE )

      END IF

!
!     Write the input data out to the target file a page at a time.
!     Null values don't get written.
!
!     While we're at it, we'll push onto the EK stack the addresses
!     of the column entries.  We use the constant NULL rather than an
!     address to represent null entries.
!
!     We'll use FROM to indicate the element of DVALS we're
!     considering, TO to indicate the element of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.  The variable N indicates the number of data
!     items in the current page.
!

      REMAIN  =  NROWS
      FROM    =  1
      TO      =  1
      BUFPTR  =  1
      NWRITE  =  0
      N       =  0

      DO WHILE ( REMAIN .GT. 0 )

         IF (  NULLOK  .AND.  ( NLFLGS(FROM) )  ) THEN

            ADRBUF(BUFPTR) =  NULL

         ELSE

            ADRBUF(BUFPTR) =  TO      +  PBASE
            PAGE(TO)       =  DVALS(FROM)
            TO             =  TO      +  1
            NWRITE         =  NWRITE  +  1
            N              =  N       +  1

         END IF


         FROM    =  FROM   + 1
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


         IF (  (   N      .EQ. DPSIZE ) .OR. &
               ( ( NWRITE .EQ. NDATA  ) .AND. ( NDATA  .NE. 0 ) ) ) THEN
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.  First set the link count.
!
            PAGE ( DLCIDX ) =  N

!
!           Write out the data page.
!
            CALL ZZEKPGWD ( HANDLE, P, PAGE  )

!
!           If there's more data to write, allocate another page.
!
            IF ( NWRITE .LT. NDATA ) THEN

               CALL ZZEKAPS  ( HANDLE, SEGDSC, DP, .FALSE., P, PBASE )
               CALL CLEARD   ( PGSIZD, PAGE )

               N   =  0
               TO  =  1

            END IF

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

         CALL ZZEKORDD ( DVALS,  NULLOK, NLFLGS, NROWS, WKINDX )

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


      CALL CHKOUT ( 'ZZEKAC02' )
      RETURN
      END
