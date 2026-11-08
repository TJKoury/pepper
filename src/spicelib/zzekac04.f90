!$Procedure     ZZEKAC04 ( EK, add class 4 column to segment )

      SUBROUTINE ZZEKAC04 (  HANDLE,  SEGDSC,  COLDSC, &
                             IVALS,   ENTSZS,  NLFLGS  )

! $ Abstract
!
!     Add an entire class 4 column to an EK segment.
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
      INTEGER               IVALS  ( * )
      INTEGER               ENTSZS ( * )
      LOGICAL               NLFLGS ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     IVALS      I   Integer values to add to column.
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
!     IVALS          is an array containing the entire set of column
!                    entries for the specified column.  The entries
!                    are listed in row-order:  the column entry for the
!                    first row of the segment is first, followed by the
!                    column entry for the second row, and so on.  The
!                    number of column entries must match the declared
!                    number of rows in the segment.  For columns having
!                    fixed-size entries, a null entry must be allocated
!                    the same amount of space occupied by a non-null
!                    entry in the array IVALS.  For columns having
!                    variable-size entries, null entries do not require
!                    any space in the IVALS array, but in any case must
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
!                    defined by IVALS is added to the specified segment
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
!     See EKACLI.
!
! $ Restrictions
!
!     1)  This routine assumes the EK scratch area has been set up
!         properly for a fast load operation.  This routine writes
!         to the EK scratch area as well.
!
!     2)  Only one segment can be created at a time using the fast
!         load routines.
!
!     3)  No other EK operation may interrupt a fast load.  For
!         example, it is not valid to issue a query while a fast load
!         is in progress.
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
      PARAMETER           ( BUFSIZ = IPSIZE )

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               ADRBUF ( BUFSIZ )
      INTEGER               BUFPTR
      INTEGER               CLASS
      INTEGER               COLIDX
      INTEGER               CURSIZ
      INTEGER               FROM
      INTEGER               I
      INTEGER               N
      INTEGER               NDATA
      INTEGER               NELT
      INTEGER               NLINK
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               P
      INTEGER               P2
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               PBASE
      INTEGER               REMAIN
      INTEGER               ROW
      INTEGER               SIZE
      INTEGER               TO

      LOGICAL               CNTINU
      LOGICAL               FIXSIZ
      LOGICAL               NEWREQ
      LOGICAL               NULLOK

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC04' )
      END IF

!
!     Grab the column's attributes.
!
      CLASS  =  COLDSC( CLSIDX )
      NULPTR =  COLDSC( NFLIDX )
      COLIDX =  COLDSC( ORDIDX )
      SIZE   =  COLDSC( SIZIDX )

      NULLOK =  NULPTR .NE. IFALSE
      FIXSIZ =  SIZE   .NE. IFALSE

!
!     This column had better be class 4.
!
      IF ( CLASS .NE. 4 ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL SETMSG ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 4.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC04'                                      )
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
                  NDATA  =  NDATA + SIZE
               ELSE
                  NDATA  =  NDATA + ENTSZS(I)
               END IF

            END IF

         END DO


      ELSE


         IF ( FIXSIZ ) THEN

            NDATA  =  NROWS * SIZE

         ELSE

            NDATA  =  0

            DO I = 1, NROWS
               NDATA  =  NDATA + ENTSZS(I)
            END DO

         END IF


      END IF


      IF ( NDATA .GT. 0 ) THEN
!
!        There's some data to write, so allocate a page.  Also
!        prepare a data buffer to be written out as a page.
!
         CALL ZZEKAPS  ( HANDLE, SEGDSC, INT, .FALSE., P, PBASE )
         CALL CLEARI   ( PGSIZI, PAGE )

      END IF

!
!     Write the input data out to the target file a page at a time.
!     Null values don't get written.
!
!     While we're at it, we'll push onto the EK stack the addresses
!     of the column entries.  We use the constant NULL rather than an
!     address to represent null entries.
!
!     We'll use FROM to indicate the element of IVALS we're
!     considering, TO to indicate the element of PAGE to write
!     to, and BUFPTR to indicate the element of ADRBUF to write
!     addresses to.  The variable NELT is the count of the column entry
!     elements written for the current entry.  The variable N indicates
!     the number of integers written to the current page.
!

      REMAIN  =  NDATA
      FROM    =  1
      TO      =  1
      BUFPTR  =  1
      ROW     =  1
      NELT    =  1
      N       =  0
      NLINK   =  0


      DO WHILE ( ROW .LE. NROWS )

!
!        NEWREQ is set to TRUE if we discover that the next column
!        entry must start on a new page.
!
         NEWREQ  =  .FALSE.


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
            NELT           =  1
            CNTINU         =  .FALSE.


         ELSE


            IF ( NELT .EQ. 1 ) THEN
!
!              We're about to write out a new column entry.  We must
!              insert the element count into the page before writing the
!              data.  The link count for the current page must be
!              incremented to account for this new entry.
!
!              At this point, we're guaranteed at least two free
!              spaces in the current page.
!
               IF ( FIXSIZ ) THEN
                  CURSIZ  =  SIZE
               ELSE
                  CURSIZ  =  ENTSZS(ROW)
               END IF

               ADRBUF(BUFPTR) =  TO     +  PBASE
               BUFPTR         =  BUFPTR + 1
               PAGE(TO)       =  CURSIZ
               TO             =  TO     + 1
               N              =  N      + 1
               NLINK          =  NLINK  + 1

            END IF

!
!           At this point, there's at least one free space in the
!           current page.
!
            PAGE(TO)  =  IVALS(FROM)
            TO        =  TO      +  1
            N         =  N       +  1
            FROM      =  FROM    +  1
            REMAIN    =  REMAIN  -  1

!
!           Decide whether we must continue the current entry on another
!           data page.
!
            CNTINU  =  ( NELT .LT. CURSIZ ) .AND. ( N .EQ. IPSIZE )

            IF ( NELT .EQ. CURSIZ ) THEN
!
!              The current element is the last of the current column
!              entry.
!
!              Determine whether we must start the next column entry on
!              a new page.  To start a column entry on the current page,
!              we must have enough room for the element count and at
!              least the first entry element.
!
               IF ( REMAIN .GT. 0 ) THEN
                  NEWREQ   =   N  .GT. ( IPSIZE - 2 )
               END IF


               NELT  =  1
               ROW   =  ROW   +  1

            ELSE

               NELT  =  NELT  +  1

            END IF

         END IF


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
            PAGE ( ILCIDX ) =  NLINK

!
!           Write out the data page.
!
            CALL ZZEKPGWI ( HANDLE, P, PAGE  )

!
!           If there's more data to write, allocate another page.
!
            IF ( REMAIN .GT. 0 ) THEN

               CALL ZZEKAPS ( HANDLE, SEGDSC, INT, .FALSE., P2, PBASE )
               CALL CLEARI  ( PGSIZI, PAGE )

               N      =  0
               NLINK  =  0
               TO     =  1

!
!              If we're continuing an element from the previous page,
!              link the previous page to the current one.
!
               IF ( CNTINU ) THEN
                  CALL ZZEKSFWD ( HANDLE, INT, P, P2 )
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

      CALL CHKOUT ( 'ZZEKAC04' )
      RETURN
      END
