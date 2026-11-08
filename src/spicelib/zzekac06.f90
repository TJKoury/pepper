!$Procedure     ZZEKAC06 ( EK, add class 6 column to segment )

      SUBROUTINE ZZEKAC06 (  HANDLE,  SEGDSC,  COLDSC, &
                             CVALS,   ENTSZS,  NLFLGS )

! $ Abstract
!
!     Add an entire class 6 column to an EK segment.
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
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'ekcnamsz.inc'
      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'ekrecptr.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
      CHARACTER*(*)         CVALS  ( * )
      INTEGER               ENTSZS ( * )
      LOGICAL               NLFLGS ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     CVALS      I   Character values to add to column.
!     ENTSZS     I   Array of sizes of column entries.
!     NLFLGS     I   Array of null flags for column entries.
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
!     ENTSZS         is an array containing sizes of column entries.
!                    The Ith element of ENTSZS gives the size of the
!                    Ith column entry.  ENTSZS is used only for columns
!                    having variable-size entries.  For such columns,
!                    the dimension of ENTSZS must be at least NROWS.
!                    The size of null entries should be set to zero.
!
!                    For columns having fixed-size entries, the
!                    dimension of this array may be any positive value.
!
!     CVALS          is an array containing the entire set of column
!                    entries for the specified column.  The entries
!                    are listed in row-order:  the column entry for the
!                    first row of the segment is first, followed by the
!                    column entry for the second row, and so on.  The
!                    number of column entries must match the declared
!                    number of rows in the segment.  For columns having
!                    fixed-size entries, a null entry must be allocated
!                    the same amount of space occupied by a non-null
!                    entry in the array CVALS.  For columns having
!                    variable-size entries, null entries do not require
!                    any space in the CVALS array, but in any case must
!                    have their allocated space described correctly by
!                    the corresponding element of the ENTSZS array
!                    (described below).
!
!     ENTSZS         is an array containing sizes of column entries.
!                    The Ith element of ENTSZS gives the size of the
!                    Ith column entry.  ENTSZS is used only for columns
!                    having variable-size entries.  For such columns,
!                    the dimension of ENTSZS must be at least NROWS.
!                    The size of null entries should be set to zero.
!
!                    For columns having fixed-size entries, the
!                    dimension of this array may be any positive value.
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
!     done by sequential calls to EKACEI, but has the drawback that
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
!     2)  Currently, the EK system can handle only one fast load
!         at at time---one segment created by a fast load must be
!         be completed by a call to EKFFLD before another segment
!         can be created by a fast load, even if the two segments
!         reside in different files.
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
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
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
      INTEGER               CP
      INTEGER               CURCHR
      INTEGER               CURSIZ
      INTEGER               CVLEN
      INTEGER               FROM
      INTEGER               I
      INTEGER               L
      INTEGER               N
      INTEGER               NCHARS
      INTEGER               NDATA
      INTEGER               NLINK
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               NW
      INTEGER               NWRITE
      INTEGER               P
      INTEGER               P2
      INTEGER               PADLEN
      INTEGER               PBASE
      INTEGER               REMAIN
      INTEGER               ROOM
      INTEGER               ROW
      INTEGER               SIZE
      INTEGER               STRLEN
      INTEGER               TO

      LOGICAL               CNTINU
      LOGICAL               FIXSIZ
      LOGICAL               NEWENT
      LOGICAL               NEWREQ
      LOGICAL               NULLOK
      LOGICAL               PAD

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC06' )
      END IF

!
!     Grab the column's attributes.
!
      CLASS  =  COLDSC( CLSIDX )
      NULPTR =  COLDSC( NFLIDX )
      COLIDX =  COLDSC( ORDIDX )
      SIZE   =  COLDSC( SIZIDX )
      STRLEN =  COLDSC( LENIDX )

      NULLOK =  NULPTR .NE. IFALSE
      FIXSIZ =  SIZE   .NE. IFALSE

!
!     This column had better be class 6.
!
      IF ( CLASS .NE. 6 ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL SETMSG ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 6.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC06'                                      )
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
!     Record the number of data values to write.
!
      IF ( NULLOK ) THEN
!
!        Sum the sizes of the non-null column entries; these are the
!        ones that will take up space.
!
         NDATA  =  0

         DO I = 1, NROWS

            IF ( .NOT. NLFLGS(I) ) THEN

               IF ( FIXSIZ ) THEN
                  NDATA  =  NDATA   +  STRLEN * SIZE
               ELSE
                  NDATA  =  NDATA   +  STRLEN * ENTSZS(I)
               END IF

            END IF

         END DO


      ELSE


         IF ( FIXSIZ ) THEN

            NDATA  =  NROWS * STRLEN * SIZE

         ELSE

            NDATA  =  0

            DO I = 1, NROWS
               NDATA  =  NDATA   +  STRLEN * ENTSZS(I)
            END DO

         END IF


      END IF


      IF ( NDATA .GT. 0 ) THEN
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         CALL ZZEKAPS  ( HANDLE, SEGDSC, CHR, .FALSE., P, PBASE )
         PAGE  =  ' '

!
!        Decide now whether we will need to pad the input entry
!        elements with trailing blanks, and if so how much padding
!        we'll need.
!
         CVLEN   =  MIN (  LEN( CVALS(1) ),  STRLEN  )
         PAD     =  CVLEN  .LT.  STRLEN

         IF ( PAD ) THEN
            PADLEN  =  STRLEN - CVLEN
         END IF

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
!     considering, TO to indicate the element of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.   The variable N indicates the number of characters
!     written to the current page.  NCHARS is the number of characters
!     written in the current column entry. CP is the position in the
!     current input string of the character which we'll read next.
!

      REMAIN  =  NDATA
      FROM    =  1
      TO      =  1
      BUFPTR  =  1
      ROW     =  1
      CP      =  1
      N       =  0
      NCHARS  =  0
      NLINK   =  0
      NEWENT  =  .TRUE.


      DO WHILE ( ROW .LE. NROWS )
!
!        NEWREQ is set to TRUE if we discover that the next column
!        entry must start on a new page.
!
         NEWREQ  =  .FALSE.

!
!        FROM and TO are expected to be properly set at this point.
!

         IF (  NULLOK  .AND.  ( NLFLGS(ROW) )  ) THEN

            IF ( FIXSIZ ) THEN
               CURSIZ  =  SIZE
            ELSE
               CURSIZ  =  ENTSZS(ROW)
            END IF

            FROM           =  FROM   +  CURSIZ
            ADRBUF(BUFPTR) =  NULL
            BUFPTR         =  BUFPTR +  1
            ROW            =  ROW    +  1
            CNTINU         =  .FALSE.
            NEWENT         =  .TRUE.


         ELSE


            IF ( NEWENT ) THEN
!
!              We're about to write out a new column entry.  We must
!              insert the element count into the page before writing the
!              data.  The link count for the current page must be
!              incremented to account for this new entry.
!
!              At this point, we're guaranteed at least ENCSIZ+1 free
!              spaces in the current page.
!
               IF ( FIXSIZ ) THEN
                  CURSIZ  =  SIZE
               ELSE
                  CURSIZ  =  ENTSZS(ROW)
               END IF

               CURCHR         =  CURSIZ *  STRLEN
               NCHARS         =  0
               CP             =  1

               ADRBUF(BUFPTR) =  TO     +  PBASE
               BUFPTR         =  BUFPTR + 1

               CALL PRTENC (  CURSIZ,  PAGE( TO : TO+ENCSIZ-1 )  )

               TO             =  TO     + ENCSIZ
               N              =  N      + ENCSIZ
               NLINK          =  NLINK  + 1
               NEWENT         =  .FALSE.

            END IF

!
!           At this point, there's at least one free space in the
!           current page.  There's also at least one character to
!           write.  Transfer as much as possible of the current
!           column entry to the current page.
!
            ROOM     =  CPSIZE - N
            NWRITE   =  MIN ( CURCHR-NCHARS, ROOM )
            NW       =  NWRITE

            DO WHILE ( NW .GT. 0 )
!
!              At this point, we're guaranteed that
!
!                 CP      <=  STRLEN
!                 TO      <   CPSIZE
!                 FROM is set correctly.
!
               IF ( PAD ) THEN
!
!                 The input strings must be padded with blanks up to
!                 a length of STRLEN characters.  The number of blanks
!                 used to pad the input is PADLEN.
!
                  IF ( CP .LT. CVLEN ) THEN
!
!                    Compute the number of `actual' characters of data
!                    left in the current input string.
!
!                    Transfer the characters we have room for from the
!                    current input string to the current page.
!
                     L                    =  CVLEN - CP + 1
                     L                    =  MIN ( L, NW  )

                     PAGE( TO : TO+L-1 )  =  CVALS(FROM)( CP : CP+L-1 )
                     CP                   =  CP  + L
                     NW                   =  NW  - L
                     TO                   =  TO  + L

                  ELSE
!
!                    The input character pointer is in the `pad' zone.
!                    Let L be the length of padding that is required
!                    and can fit in the page.
!
                     L                    =  STRLEN - CP + 1
                     L                    =  MIN ( L, NW )

                     PAGE( TO : TO+L-1 )  =  ' '
                     CP                   =  CP  + L
                     NW                   =  NW  - L
                     TO                   =  TO  + L

                  END IF


               ELSE
!
!                 The input data doesn't require padding.
!
!                 Compute the number of `actual' characters of data
!                 left in the current input string.
!
!                 Transfer the characters we have room for from the
!                 current input string to the current page.
!
                  L                    =  STRLEN - CP + 1
                  L                    =  MIN ( L, NW )

                  PAGE( TO : TO+L-1 )  =  CVALS(FROM)( CP : CP+L-1 )
                  CP                   =  CP + L
                  NW                   =  NW - L
                  TO                   =  TO + L

               END IF

!
!              If the input pointer is beyond the end of the declared
!              length of the target column's strings STRLEN, it's time
!              to look at the next input string.
!
               IF ( CP .GT. STRLEN ) THEN
                  FROM  =  FROM + 1
                  CP    =  1
               END IF

            END DO

!
!           We've written NWRITE characters to the current page.  FROM,
!           TO, and CP are set.
!
            N         =  N       +  NWRITE
            REMAIN    =  REMAIN  -  NWRITE
            NCHARS    =  NCHARS  +  NWRITE

!
!           Decide whether we must continue the current entry on another
!           data page.
!
            CNTINU  =  ( NCHARS .LT. CURCHR ) .AND. ( N .EQ. CPSIZE )

!
!           If we've finished writing out a column entry, get ready
!           to write the next one.
!
            IF ( NCHARS .EQ. CURCHR ) THEN
!
!              The current character is the last of the current column
!              entry.
!
!              Determine whether we must start the next column entry on
!              a new page.  To start a column entry on the current page,
!              we must have enough room for the element count and at
!              least one character of data.
!
               IF ( REMAIN .GT. 0 ) THEN
                  NEWREQ   =   N  .GT. ( CPSIZE - ENCSIZ - 1 )
               END IF

               ROW     =   ROW   +  1
               NEWENT  =  .TRUE.

            END IF


         END IF

!
!        At this point, CNTINU indicates whether we need to continue
!        the current entry on another page.  If we finished writing out
!        the entry, CNTINU is .FALSE.
!

         IF (  ( BUFPTR .GT. BUFSIZ ) .OR. ( ROW .GT. NROWS )  ) THEN
!
!           The address buffer is full or we're out of input values
!           to look at, so push the buffer contents on the stack.
!
            CALL ZZEKSPSH ( BUFPTR-1, ADRBUF )
            BUFPTR  =  1

         END IF


         IF (         CNTINU &
             .OR.     NEWREQ &
             .OR. ( ( ROW .GT. NROWS ) .AND. ( NDATA .GT. 0 ) )  ) THEN
!
!           It's time to write out the current page.  First set the link
!           count.
!
            CALL PRTENC (  NLINK,  PAGE ( CLCIDX : CLCIDX+ENCSIZ-1 )  )

!
!           Write out the data page.
!
            CALL ZZEKPGWC ( HANDLE, P, PAGE  )

!
!           If there's more data to write, allocate another page.
!
            IF ( REMAIN .GT. 0 ) THEN

               CALL ZZEKAPS ( HANDLE, SEGDSC, CHR, .FALSE., P2, PBASE )
               PAGE   = ' '

               N      =  0
               NLINK  =  0
               TO     =  1

!
!              If we're continuing an element from the previous page,
!              link the previous page to the current one.
!
               IF ( CNTINU ) THEN
                  CALL ZZEKSFWD ( HANDLE, CHR, P, P2 )
               END IF

               P   =  P2

            END IF
!
!           We've allocated a new data page if we needed one.
!

         END IF
!
!        We've written out the last completed data page.
!

      END DO
!
!     We've processed all entries of the input array.
!

      CALL CHKOUT ( 'ZZEKAC06' )
      RETURN
      END
