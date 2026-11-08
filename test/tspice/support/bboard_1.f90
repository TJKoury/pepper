
!$Procedure BBOARD ( Bulletin board )

      SUBROUTINE BBOARD ( ACTION, ITEM, N, IVALS, &
                                           DVALS, &
                                           CVALS, &
                                           SVAL   )
      IMPLICIT NONE

! $ Abstract
!
!     Maintain a global bulletin board for use by application
!     programs.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         ACTION
      CHARACTER*(*)         ITEM
      INTEGER               N
      INTEGER               IVALS    ( * )
      DOUBLE PRECISION      DVALS    ( * )
      CHARACTER*(*)         CVALS    ( * )
      CHARACTER*(*)         SVAL

      INTEGER               MAXNL
      PARAMETER           ( MAXNL = 32 )

      INTEGER               MAXCL
      PARAMETER           ( MAXCL = 255 )

      INTEGER               MAXI
      PARAMETER           ( MAXI = 100 )

      INTEGER               MAXD
      PARAMETER           ( MAXD = 100 )

      INTEGER               MAXC
      PARAMETER           ( MAXC = 100 )

      INTEGER               MAXS
      PARAMETER           ( MAXS = 100 )

      INTEGER               MAXIV
      PARAMETER           ( MAXIV = 5000 )

      INTEGER               MAXDV
      PARAMETER           ( MAXDV = 5000 )

      INTEGER               MAXCV
      PARAMETER           ( MAXCV = 300 )

      INTEGER               MAXCHR
      PARAMETER           ( MAXCHR = 5000 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action to be taken.
!     ITEM       I   Item to be posted or retrieved.
!     N         I,O  Number of values posted or retrieved.
!     IVALS     I,O  Integer values.
!     DVALS     I,O  Double precision values
!     CVALS     I,O  Character values.
!     SVAL      I,O  String value.
!     MAXNL      P   Maximum name length.
!     MAXCL      P   Maximum character length.
!     MAXI       P   Maximum number of integer items.
!     MAXD       P   Maximum number of double precision items.
!     MAXC       P   Maximum number of character items.
!     MAXS       P   Maximum number of string items.
!     MAXIV      P   Maximum number of integer values.
!     MAXDV      P   Maximum number of double precision values.
!     MAXCV      P   Maximum number of character values.
!     MAXCHR     P   Maximum number of string characters.
!
! $ Detailed_Input
!
!     ACTION     is used by entry points BBPUT and BBGET to indicate
!                a specific action to be taken. Possible actions
!                are 'POST', 'COPY', 'TAKE', 'PUSH', 'APPEND', and
!                'POP'.
!
!     ITEM       is the name of an item to be posted, retrieved,
!                removed, and so on. Names are case-sensitive, but
!                leading and embedded blanks are ignored.
!
!     N          on input is the number of values to be posted.
!
!     IVALS,
!     DVALS,
!     CVALS,     on input are values to be associated with a specific
!                integer, DP, or character item on the board.
!
!     SVAL       on input is a string value to be associated with a
!                specific string item on the board.
!
! $ Detailed_Output
!
!     N          on output is the number of values being returned,
!                or the number of values associated with an item.
!
!     IVALS,
!     DVALS,
!     CVALS,     on output are values associated with a specific
!                integer, DP, or character item on the board.
!
!     SVAL       on output is a string value associated with a
!                specific string item on the board.
!
! $ Parameters
!
!     MAXNL      is the maximum number of characters that can make
!                up an item name.
!
!     MAXCL      is the declared length of the individual values
!                of character items. That is, each multi-valued
!                character item is equivalent to a CHARACTER*(MAXCL)
!                array.
!
!     MAXI,
!     MAXD,
!     MAXC,
!     MAXS,      are the maximum numbers of items of each type
!                (integer, DP, character, and string) that can be
!                stored simultaneously.
!
!     MAXIV,
!     MAXDV,
!     MAXCV      are the maximum numbers of values of each type
!                (integer, DP, and character) that can be stored
!                simultaneously. MAXIV, MAXDV, and MAXCV must be
!                at least as large as MAXI, MAXD, and MAXC,
!                respectively. (Note that string items are are
!                not multi-valued.)
!
!     MAXCHR     is the maximum number characters that can be used
!                to store string items at any one time. MAXCHR must
!                be an integer multiple of 100.
!
! $ Exceptions
!
!     1) If BBOARD is called directly, the error 'SPICE(BOGUSENTRY)'
!        is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!           BBOARD implements a global storage area, which may be
!           used by the individual modules of an application program
!           communicate with each other. The metaphor for this area
!           is a bulletin board: modules may leave messages (called
!           `items') on this board, to be copied, modified, or removed
!           by other modules.
!
!     Types
!
!           The board can contain four types of items: integer, double
!           precision (DP), character, and string. The first three types
!           may be multi-valued: for example, a single integer item may
!           be associated with more than one integer value. Thus, the
!           board may be used to store arrays as well as scalar values.
!
!           Each string item may contain only one value: however,
!           the number of characters in this value may be anywhere
!           between one and the total size of the string buffer
!           (parameter MAXCHR).
!
!     Posting items
!
!           An item may be posted on the board by calling BBPUTx
!           (where x indicates the type of the item: I, D, C, or S).
!           For example, the call
!
!              IMAGES(1) = '22421.36'
!              IMAGES(2) = '22421.39'
!              IMAGES(3) = '22421.45'
!
!              CALL BBPUTC ( 'POST', 'IMAGE QUEUE', 3, IMAGES )
!
!           creates an item with the name 'IMAGE QUEUE', which contains
!           the three character values in the array IMAGES. If an item
!           with that name already exists, it is replaced.
!
!     Item names
!
!           Item names are case-sensitive, but blanks are ignored.
!           The item 'IMAGE QUEUE' may be accessed under any of the
!           following names.
!
!              'IMAGE QUEUE'
!              'IMAGEQUEUE'
!              '  IMAGE   QUEUE  '
!
!           On the other hand, the names
!
!              'Image queue'
!              'image queue'
!              'Image Queue'
!
!           all refer to distinct items.
!
!           The same item name may be applied to one item of each
!           type. This allows you to associate values of different
!           types under a single name, as in the following example.
!
!              IMAGES(  1) = '22421.36'
!              BODIES(  1) =  801
!              RADII (1,1) =  1600.D0
!              RADII (2,1) =  1600.D0
!              RADII (3,1) =  1600.D0
!
!              IMAGES(  2) = '22427.19'
!              BODIES(  2) =  899
!              RADII (1,2) =  25295.D0
!              RADII (2,2) =  25295.D0
!              RADII (3,2) =  24738.D0
!
!              DESCR       = 'Preliminary NINA testing, 4/12/89'
!
!              CALL BBPUTC ( 'POST', 'IMAGE QUEUE', 2, IMAGES )
!              CALL BBPUTI ( 'POST', 'IMAGE QUEUE', 2, BODIES )
!              CALL BBPUTD ( 'POST', 'IMAGE QUEUE', 6, RADII  )
!              CALL BBPUTS ( 'POST', 'IMAGE QUEUE',    DESCR  )
!
!     Copying items
!
!           Once an item has been posted, its values may be copied
!           by calling BBGETx. For example, the call
!
!              CALL BBGETC ( 'COPY', 'IMAGE QUEUE', N, IMAGES )
!
!           copies the values associated with the character item
!           'IMAGE QUEUE' into the character array IMAGES. All of
!           the values associated with the item are returned.
!
!     Taking items
!
!           When an item is copied, its values remain intact, ready
!           to be copied by other modules. Posted items may also be
!           taken by calling BBGETx. For example, the call
!
!              CALL BBGETD ( 'TAKE', 'IMAGE QUEUE', N, IMAGES )
!
!           returns the values just as the previous call did; however,
!           following this call, the item is no longer on the board.
!
!     Removing items
!
!           It is possible to remove an item without copying its values,
!           by calling BBREMx. For example, the calls
!
!              CALL BBREMC ( 'IMAGE QUEUE' )
!              CALL BBREMI ( 'IMAGE QUEUE' )
!              CALL BBREMD ( 'IMAGE QUEUE' )
!              CALL BBREMS ( 'IMAGE QUEUE' )
!
!           removes these items from the board without allocating space
!           for the return of any values. Removing an item that is not
!           on the board does not cause an error.
!
!     Stacks and Queues
!
!           The list of values associated with a multi-valued item
!           may be thought of as a stack or queue. The values can be
!           popped (copied and removed) from this list in pieces,
!           instead of all at once. Thus, the images in 'IMAGE QUEUE'
!           can be processed as shown below.
!
!              DO I = 1, 2
!                 CALL BBGETC ( 'POP', 'IMAGE QUEUE', 1, IMAGE )
!                 CALL BBGETI ( 'POP', 'IMAGE QUEUE', 1, BODY  )
!                 CALL BBGETD ( 'POP', 'IMAGE QUEUE', 3, RADII )
!                  .
!                  .
!
!              END DO
!
!           Values may be added to the beginning of the value list
!           (treating it as a push-down stack), or to the end of the
!           list (treating it as a queue). The following sequence
!
!              CALL BBPUTI ( 'POST', 'SEQUENCE', 1, 5 )
!
!              DATA(1) = 1
!              DATA(2) = 2
!              DATA(3) = 3
!              DATA(4) = 4
!              CALL BBPUTI ( 'PUSH', 'SEQUENCE', 4, DATA )
!
!              DATA(1) = 6
!              DATA(2) = 7
!              DATA(3) = 8
!              DATA(4) = 9
!              CALL BBPUTI ( 'APPEND', 'SEQUENCE', 4, DATA )
!
!           creates an integer item 'SEQUENCE' containing the numbers
!           1-9 in order.
!
!           Pushing or appending values onto an item that doesn't exist
!           causes the item to be created.
!
!     Finding items
!
!           Attempting to copy, take, or pop values from an item not on
!           the board results in an error (which is reported through the
!           normal SPICELIB error handling mechanism). The presence of
!           an item may be confirmed by calling BBFNDx. For example,
!           the call
!
!              CALL BBFNDI ( 'SEQUENCE', N )
!
!           returns a value of 9 in N, because 'SEQUENCE' contains nine
!           values. Items not on the board contain zero values. (Note
!           that BBFNDS, which finds string items, can only return one
!           or zero.)
!
!     Clearing the board
!
!           The entire board may be cleared at any time by calling
!           BBCLR,
!
!              CALL BBCLR
!
!           The board MUST be cleared at least once (usually by the
!           main module of the calling program) before any items can
!           be posted.
!
!     Storage
!
!           Because standard Fortran-77 does not allow storage to be
!           allocated dynamically, the storage used by the bulletin
!           board must be allocated when BBOARD is compiled, by
!           setting the values of the parameters MAXNL, MAXCL, MAXI,
!           MAXD, MAXS, MAXIV, MAXDV, MAXCV, and MAXCHR.
!
! $ Examples
!
!     Consider the following program,
!
!        PROGRAM SIMPLE
!
!        CALL BBCLR
!
!        CALL READ
!        CALL BBFNDS ( 'NAME', N )
!
!        DO WHILE ( N .GT. 0 )
!           CALL LOOK_UP
!           CALL COMPUTE
!           CALL PRINT
!
!           CALL READ
!           CALL BBFNDS ( 'NAME', N )
!        END DO
!
!        END
!
!     which calls four modules:
!
!        READ      which reads the name of a picture file from the
!                  standard input file, and places it on the bulletin
!                  board as string item 'NAME'.
!
!        LOOK_UP   which looks up the spacecraft event time, filter
!                  number, exposure time, and pointing angles for the
!                  picture in the picture file.
!
!        COMPUTE   which computes the equivalent pointing in two
!                  auxiliary coordinate systems.
!
!        PRINT     which prints everything to the standard output file.
!
!     The program begins by clearing the bulletin board. This prepares
!     the board for use by the rest of the program.
!
!     READ begins by removing item NAME from the board. It then attempts
!     to read the name of the next picture file. If successful, it posts
!     the name. (If not the board will not contain the item, and the
!     program will terminate.)
!
!        SUBROUTINE READ
!
!        CHARACTER*128   FILE
!        INTEGER         IOSTAT
!
!        CALL BBREMS ( 'NAME' )
!        READ (*,*,IOSTAT=IOSTAT) FILE
!
!        IF ( IOSTAT .EQ. 0 ) THEN
!           CALL BBPUTS ( 'POST', 'NAME', FILE )
!        END IF
!
!        RETURN
!        END
!
!     LOOK_UP uses the name of the file as an index into a database
!     (the details of which are not important). It retrieves the items
!     of interest from the database, and posts them on the board.
!     (Note that the spacecraft event time is posted in UTC and ET.)
!
!
!        SUBROUTINE LOOK_UP
!
!        CALL BBGETS ( 'COPY', 'NAME', INDEX )
!         .
!         .
!
!        CALL BBPUTS ( 'POST', 'S/C EVENT (UTC)',    UTC   )
!        CALL BBPUTD ( 'POST', 'S/C EVENT (ET)',  1, ET    )
!        CALL BBPUTI ( 'POST', 'FILTER NUMBER',   1, FNUM  )
!        CALL BBPUTD ( 'POST', 'EXPOSURE',        1, EXP   )
!        CALL BBPUTD ( 'POST', 'POINTING (CCT)',  3, CCT   )
!
!        RETURN
!        END
!
!     COMPUTE begins with the nominal (Clock, Cone, Twist) pointing
!     and the spacecraft event time, and computes the equivalent
!     pointing in two other systems: Azimuth, Elevation, Twist; and
!     Right ascension, Declination, Twist. (Again, the details are not
!     important.) These are stored on the board.
!
!     The conversion depends on an optional bias angle, which may
!     or may not be posted. If not found, it defaults to zero.
!
!        SUBROUTINE COMPUTE
!         .
!         .
!
!        CALL BBGETD ( 'COPY', 'POINTING (CCT)', N, CCT )
!        CALL BBGETD ( 'COPY', 'S/C EVENT (ET)', N, ET  )
!
!        CALL BBFNDD ( 'BIAS', N )
!        IF ( N .EQ. 1 ) THEN
!           CALL BBGETD ( 'COPY', 'BIAS', N, BIAS )
!        ELSE
!           BIAS = 0.D0
!        END IF
!         .
!         .
!
!        CALL BBPUTD ( 'POST', 'POINTING (AET)', 3, AET )
!        CALL BBPUTD ( 'POST', 'POINTING (RDT)', 3, RDT )
!
!        RETURN
!        END
!
!     PRINT simply retrieves the items from the board and writes
!     them to the standard output file. The items are removed from
!     the board as their values are printed, freeing space for use
!     by other parts of the program. (This is largely a preventative
!     measure: it is not necessary for the program as it stands,
!     but it could become important as the program undergoes further
!     development.)
!
!        SUBROUTINE PRINT
!         .
!         .
!
!        CALL BBGETS ( 'TAKE', 'NAME', STRING )
!        WRITE (*,*)
!        WRITE (*,*) STRING
!
!         .
!         .
!
!        CALL BBGETS ( 'TAKE', 'POINTING (RDT)', N, NUMBERS )
!        WRITE (*,*) ( NUMBERS(I), I = 1, N )
!
!        RETURN
!        END
!
! $ Restrictions
!
!     1) The values of parameters MAXNL and MAXCL must not be smaller
!        than the value of parameter MINLEN in subroutine ENCHAR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      INTEGER               SYDIMC
      INTEGER               SYDIMD
      INTEGER               SYDIMI

!
!     Local variables
!
!     Integer, DP, and character items are stored in symbol tables.
!     Later, they should be stored in card catalogs, when the necessary
!     routines have been completed.
!
!     Strings are stored in a string buffer.
!
!     Actions, where input, are compressed and converted to uppercase
!     (WHAT). Item names are compressed (WHICH).
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )

      CHARACTER*(MAXNL)     INTAB     ( LBCELL:MAXI  )
      INTEGER               IPTAB     ( LBCELL:MAXI  )
      INTEGER               IVTAB     ( LBCELL:MAXIV )

      CHARACTER*(MAXNL)     DNTAB     ( LBCELL:MAXD  )
      INTEGER               DPTAB     ( LBCELL:MAXD  )
      DOUBLE PRECISION      DVTAB     ( LBCELL:MAXDV )

      CHARACTER*(MAXNL)     CNTAB     ( LBCELL:MAXC  )
      INTEGER               CPTAB     ( LBCELL:MAXC  )
      CHARACTER*(MAXCL)     CVTAB     ( LBCELL:MAXCV )

      CHARACTER*(MAXNL)     NBUF      ( LBCELL:MAXS         )
      INTEGER               PBUF      ( LBCELL:MAXS * 4 + 4 )
      CHARACTER*(100)       VBUF      ( LBCBUF:MAXCHR / 100 )

      CHARACTER*(MAXNL)     WHAT
      CHARACTER*(MAXNL)     WHICH

      INTEGER               POS
      INTEGER               I
      LOGICAL               FND

!
!     Save everything between calls.
!
      SAVE


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBOARD' )
      END IF

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'BBOARD' )
      RETURN



!$Procedure BBPUTI ( Bulletin board, put, integer )

      ENTRY BBPUTI_1 ( ACTION, ITEM, N, IVALS )

! $ Abstract
!
!     Put one or more values on the board, associated with
!     an integer item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!     INTEGER               IVALS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'.
!     ITEM       I   Item name.
!     N          I   Number of values to be posted.
!     IVALS      I   Values to be posted.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Possible actions
!                 are 'POST', 'PUSH', and 'APPEND'.
!
!     ITEM        is the name of an integer item, which may or
!                 may not be on the board already.
!
!     N           is the number of values to be associated with the
!                 specified item.
!
!     IVALS       are the values.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'POST' creates a new item, containing the specified values.
!     (If an item of the same type with the same name already exists,
!     it is replaced.)
!
!     'PUSH' modifies the list of values associated with an existing
!     item by placing items at the beginning of the list (treating the
!     list as a push-down stack).
!
!     'APPEND' modifies the list of values associated with an existing
!     item by placing items at the end of the list (treating the list
!     as a queue).
!
!     Both 'PUSH' and 'APPEND' will create a new item if the specified
!     item does not exist.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTI_1' )
      END IF

!
!     Compress spaces, change cases, as needed.
!
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by the symbol table routines. (Later,
!     it will be done by the card catalog routines.) Note that
!     items must be pushed and appended one at a time.
!
      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SYPUTI ( WHICH, IVALS, N, INTAB, IPTAB, IVTAB )

      ELSE IF ( WHAT .EQ. 'PUSH' ) THEN
         DO I = N, 1, -1
            CALL SYPSHI ( WHICH, IVALS(I), INTAB, IPTAB, IVTAB )
         END DO

      ELSE IF ( WHAT .EQ. 'APPEND' ) THEN
         DO I = 1, N
            CALL SYENQI ( WHICH, IVALS(I), INTAB, IPTAB, IVTAB )
         END DO

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBPUTI_1' )
      RETURN


!$Procedure BBPUTD ( Bulletin board, put, DP )

      ENTRY BBPUTD_1 ( ACTION, ITEM, N, DVALS )

! $ Abstract
!
!     Put one or more values on the board, associated with
!     a DP item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!     DOUBLE PRECISION      DVALS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'.
!     ITEM       I   Item name.
!     N          I   Number of values to be posted.
!     DVALS      I   Values to be posted.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Possible actions
!                 are 'POST', 'PUSH', and 'APPEND'.
!
!     ITEM        is the name of a DP item, which may or
!                 may not be on the board already.
!
!     N           is the number of values to be associated with the
!                 specified item.
!
!     DVALS       are the values.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'POST' creates a new item, containing the specified values.
!     (If an item of the same type with the same name already exists,
!     it is replaced.)
!
!     'PUSH' modifies the list of values associated with an existing
!     item by placing items at the beginning of the list (treating the
!     list as a push-down stack).
!
!     'APPEND' modifies the list of values associated with an existing
!     item by placing items at the end of the list (treating the list
!     as a queue).
!
!     Both 'PUSH' and 'APPEND' will create a new item if the specified
!     item does not exist.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTD_1' )
      END IF

!
!     Compress spaces, change cases, as needed.
!
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by the symbol table routines. (Later,
!     it will be done by the card catalog routines.) Note that
!     items must be pushed and appended one at a time.
!
      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SYPUTD ( WHICH, DVALS, N, DNTAB, DPTAB, DVTAB )

      ELSE IF ( WHAT .EQ. 'PUSH' ) THEN
         DO I = N, 1, -1
            CALL SYPSHD ( WHICH, DVALS(I), DNTAB, DPTAB, DVTAB )
         END DO

      ELSE IF ( WHAT .EQ. 'APPEND' ) THEN
         DO I = 1, N
            CALL SYENQD ( WHICH, DVALS(I), DNTAB, DPTAB, DVTAB )
         END DO

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBPUTD_1' )
      RETURN


!$Procedure BBPUTC ( Bulletin board, put, character )

      ENTRY BBPUTC_1 ( ACTION, ITEM, N, CVALS )

! $ Abstract
!
!     Put one or more values on the board, associated with
!     a character item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!     CHARACTER*(*)         CVALS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'.
!     ITEM       I   Item name.
!     N          I   Number of values to be posted.
!     CVALS      I   Values to be posted.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Possible actions
!                 are 'POST', 'PUSH', and 'APPEND'.
!
!     ITEM        is the name of a character item, which may or
!                 may not be on the board already.
!
!     N           is the number of values to be associated with the
!                 specified item.
!
!     CVALS       are the values.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'POST' creates a new item, containing the specified values.
!     (If an item of the same type with the same name already exists,
!     it is replaced.)
!
!     'PUSH' modifies the list of values associated with an existing
!     item by placing items at the beginning of the list (treating the
!     list as a push-down stack).
!
!     'APPEND' modifies the list of values associated with an existing
!     item by placing items at the end of the list (treating the list
!     as a queue).
!
!     Both 'PUSH' and 'APPEND' will create a new item if the specified
!     item does not exist.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTC_1' )
      END IF

!
!     Compress spaces, change cases, as needed.
!
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by the symbol table routines. (Later,
!     it will be done by the card catalog routines.) Note that
!     items must be pushed and appended one at a time.
!
      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SYPUTC ( WHICH, CVALS, N, CNTAB, CPTAB, CVTAB )

      ELSE IF ( WHAT .EQ. 'PUSH' ) THEN
         DO I = N, 1, -1
            CALL SYPSHC ( WHICH, CVALS(I), CNTAB, CPTAB, CVTAB )
         END DO

      ELSE IF ( WHAT .EQ. 'APPEND' ) THEN
         DO I = 1, N
            CALL SYENQC ( WHICH, CVALS(I), CNTAB, CPTAB, CVTAB )
         END DO

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBPUTC_1' )
      RETURN


!$Procedure BBPUTS ( Bulletin board, put, string )

      ENTRY BBPUTS_1 ( ACTION, ITEM, SVAL )

! $ Abstract
!
!     Put a value on the board, associated with a string item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!     CHARACTER*(*)         SVAL
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'POST'.
!     ITEM       I   Item name.
!     SVAL       I   Value to be posted.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Currently, the
!                 only possible action is 'POST'.
!
!     ITEM        is the name of a string item, which may or
!                 may not be on the board already.
!
!     SVAL        is the value to be associated with the specified
!                 item. Trailing blanks are ignored.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'POST' creates a new item, containing the specified value.
!     (If an item of the same type with the same name already exists,
!     it is replaced.)
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTS_1' )
      END IF

      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SBSET_1 ( WHICH, SVAL, NBUF, PBUF, VBUF )

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBPUTS_1' )
      RETURN



!$Procedure BBGETI ( Bulletin board, get, integer )

      ENTRY BBGETI_1 ( ACTION, ITEM, N, IVALS )

! $ Abstract
!
!     Get one or more values from the board, associated with
!     an integer item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!     INTEGER               IVALS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'.
!     ITEM       I   Item name.
!     N         I,O  Number of values returned.
!     IVALS      O   Values.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Possible actions
!                 are 'COPY', 'TAKE', and 'POP'.
!
!     ITEM        is the name of an integer item, which must be
!                 on the board.
!
!     N           on input is the number of values to be popped.
!
!
! $ Detailed_Output
!
!     N           on output is the number of values returned.
!
!     IVALS       are some or all of the values associated with the
!                 specified item.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
!     2) If ITEM is not found, or if the number of values to be popped
!        is smaller than the number of values associated with the item,
!        the error 'SPICE(ALLGONE)' is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'COPY' returns all of the values associated with the specified
!     item. The number of values is returned in N. Copying an item
!     leaves the item intact.
!
!     'TAKE' returns all of the values associated with the specified
!     item. The number of values is returned in N. Unlike copying,
!     taking an item removes the item from the board.
!
!     'POP' takes some of the values associated with the specified
!     item. Items are taken from the front of the list; the remaining
!     values are left intact. The number of values to be popped is
!     specified in N. Popping the final value of an item removes the
!     item from the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETI_1' )
      END IF

!
!     Compress spaces, change cases, as needed.
!
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by the symbol table routines. (Later,
!     it will be done by the card catalog routines.) Note that
!     items must be popped one at a time.
!
      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SYGETI ( WHICH, INTAB, IPTAB, IVTAB, N, IVALS, FND )

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )

         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SYDELI ( WHICH, INTAB, IPTAB, IVTAB )
         END IF

      ELSE IF ( WHAT .EQ. 'POP' ) THEN
         DO I = 1, N
            CALL SYPOPI ( WHICH, INTAB, IPTAB, IVTAB, IVALS(I), FND )
         END DO

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
         END IF

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBGETI_1' )
      RETURN


!$Procedure BBGETD ( Bulletin board, get, DP )

      ENTRY BBGETD_1 ( ACTION, ITEM, N, DVALS )

! $ Abstract
!
!     Get one or more values from the board, associated with
!     a DP item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!     DOUBLE PRECISION      DVALS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'.
!     ITEM       I   Item name.
!     N         I,O  Number of values returned.
!     DVALS      O   Values.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Possible actions
!                 are 'COPY', 'TAKE', and 'POP'.
!
!     ITEM        is the name of a DP item, which must be
!                 on the board.
!
!     N           on input is the number of values to be popped.
!
!
! $ Detailed_Output
!
!     N           on output is the number of values returned.
!
!     DVALS       are some or all of the values associated with the
!                 specified item.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
!     2) If ITEM is not found, or if the number of values to be popped
!        is smaller than the number of values associated with the item,
!        the error 'SPICE(ALLGONE)' is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'COPY' returns all of the values associated with the specified
!     item. The number of values is returned in N. Copying an item
!     leaves the item intact.
!
!     'TAKE' returns all of the values associated with the specified
!     item. The number of values is returned in N. Unlike copying,
!     taking an item removes the item from the board.
!
!     'POP' takes some of the values associated with the specified
!     item. Items are taken from the front of the list; the remaining
!     values are left intact. The number of values to be popped is
!     specified in N. Popping the final value of an item removes the
!     item from the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETD_1' )
      END IF

!
!     Compress spaces, change cases, as needed.
!
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by the symbol table routines. (Later,
!     it will be done by the card catalog routines.) Note that
!     items must be popped one at a time.
!
      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SYGETD ( WHICH, DNTAB, DPTAB, DVTAB, N, DVALS, FND )

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )

         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SYDELD ( WHICH, DNTAB, DPTAB, DVTAB )
         END IF

      ELSE IF ( WHAT .EQ. 'POP' ) THEN
         DO I = 1, N
            CALL SYPOPD ( WHICH, DNTAB, DPTAB, DVTAB, DVALS(I), FND )
         END DO

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
         END IF

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBGETD_1' )
      RETURN


!$Procedure BBGETC ( Bulletin board, get, character )

      ENTRY BBGETC_1 ( ACTION, ITEM, N, CVALS )

! $ Abstract
!
!     Get one or more values from the board, associated with
!     a character item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!     CHARACTER*(*)         CVALS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'.
!     ITEM       I   Item name.
!     N         I,O  Number of values returned.
!     CVALS      O   Values.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Possible actions
!                 are 'COPY', 'TAKE', and 'POP'.
!
!     ITEM        is the name of a character item, which must be
!                 on the board.
!
!     N           on input is the number of values to be popped.
!
!
! $ Detailed_Output
!
!     N           on output is the number of values returned.
!
!     CVALS       are some or all of the values associated with the
!                 specified item.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
!     2) If ITEM is not found, or if the number of values to be popped
!        is smaller than the number of values associated with the item,
!        the error 'SPICE(ALLGONE)' is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'COPY' returns all of the values associated with the specified
!     item. The number of values is returned in N. Copying an item
!     leaves the item intact.
!
!     'TAKE' returns all of the values associated with the specified
!     item. The number of values is returned in N. Unlike copying,
!     taking an item removes the item from the board.
!
!     'POP' takes some of the values associated with the specified
!     item. Items are taken from the front of the list; the remaining
!     values are left intact. The number of values to be popped is
!     specified in N. Popping the final value of an item removes the
!     item from the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETC_1' )
      END IF

!
!     Compress spaces, change cases, as needed.
!
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by the symbol table routines. (Later,
!     it will be done by the card catalog routines.) Note that
!     items must be popped one at a time.
!
      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SYGETC ( WHICH, CNTAB, CPTAB, CVTAB, N, CVALS, FND )

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )

         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SYDELC ( WHICH, CNTAB, CPTAB, CVTAB )
         END IF

      ELSE IF ( WHAT .EQ. 'POP' ) THEN
         DO I = 1, N
            CALL SYPOPC ( WHICH, CNTAB, CPTAB, CVTAB, CVALS(I), FND )
         END DO

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
         END IF

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBGETC_1' )
      RETURN


!$Procedure BBGETS ( Bulletin board, get, string )

      ENTRY BBGETS_1 ( ACTION, ITEM, SVAL )

! $ Abstract
!
!     Get a value from the board, associated with a string item.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ACTION
!     CHARACTER*(*)         ITEM
!     CHARACTER*(*)         SVAL
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action: 'COPY' or 'TAKE'.
!     ITEM       I   Item name.
!     SVAL       O   Value.
!
! $ Detailed_Input
!
!     ACTION      specifies an action to be taken. Possible actions
!                 are 'COPY' and 'TAKE'.
!
!     ITEM        is the name of a string item, which must be
!                 on the board.
!
! $ Detailed_Output
!
!     SVAL        is the value associated with the specified item.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
!        is signalled.
!
!     2) If ITEM is not found, the error 'SPICE(ALLGONE)' is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     'COPY' returns the value associated with the specified item.
!     Copying an item leaves the item intact.
!
!     'TAKE' returns the value associated with the specified item.
!     Unlike copying, taking an item removes the item from the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETS_1' )
      END IF

      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SBGET_1 ( WHICH, NBUF, PBUF, VBUF, SVAL, POS )

         IF ( POS .EQ. 0 ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )

         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SBREM_1 ( WHICH, NBUF, PBUF, VBUF )
         END IF

      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF

      CALL CHKOUT ( 'BBGETS_1' )
      RETURN



!$Procedure BBREMI ( Bulletin board, remove, integer )

      ENTRY BBREMI_1 ( ITEM )

! $ Abstract
!
!     Remove an integer item from the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!
! $ Detailed_Input
!
!     ITEM        is the name of an integer item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ITEM is not recognized, the board is not changed.
!        No error occurs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Items may also be removed by calling BBGETI, using 'TAKE'
!     or 'POP'. However, BBREMI allows you to remove an item without
!     providing space for its values. Also, it does not cause an
!     error if the item is not on the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMI_1' )
      END IF

!
!     Compress spaces as needed.
!
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by a symbol table routine. (Later,
!     it will be done by a card catalog routine.)
!
      CALL SYDELI ( WHICH, INTAB, IPTAB, IVTAB )

      CALL CHKOUT ( 'BBREMI_1' )
      RETURN


!$Procedure BBREMD ( Bulletin board, remove, DP )

      ENTRY BBREMD_1 ( ITEM )

! $ Abstract
!
!     Remove a DP item from the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!
! $ Detailed_Input
!
!     ITEM        is the name of a DP item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ITEM is not recognized, the board is not changed.
!        No error occurs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Items may also be removed by calling BBGETD, using 'TAKE'
!     or 'POP'. However, BBREMD allows you to remove an item without
!     providing space for its values. Also, it does not cause an
!     error if the item is not on the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMD_1' )
      END IF

!
!     Compress spaces as needed.
!
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by a symbol table routine. (Later,
!     it will be done by a card catalog routine.)
!
      CALL SYDELD ( WHICH, DNTAB, DPTAB, DVTAB )

      CALL CHKOUT ( 'BBREMD_1' )
      RETURN


!$Procedure BBREMC ( Bulletin board, remove, character )

      ENTRY BBREMC_1 ( ITEM )

! $ Abstract
!
!     Remove a character item from the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!
! $ Detailed_Input
!
!     ITEM        is the name of a character item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ITEM is not recognized, the board is not changed.
!        No error occurs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Items may also be removed by calling BBGETC, using 'TAKE'
!     or 'POP'. However, BBREMC allows you to remove an item without
!     providing space for its values. Also, it does not cause an
!     error if the item is not on the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMC_1' )
      END IF

!
!     Compress spaces as needed.
!
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by a symbol table routine. (Later,
!     it will be done by a card catalog routine.)
!
      CALL SYDELC ( WHICH, CNTAB, CPTAB, CVTAB )

      CALL CHKOUT ( 'BBREMC_1' )
      RETURN


!$Procedure BBREMS ( Bulletin board, remove, string )

      ENTRY BBREMS_1 ( ITEM )

! $ Abstract
!
!     Remove a string item from the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!
! $ Detailed_Input
!
!     ITEM        is the name of a string item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     1) If ITEM is not recognized, the board is not changed.
!        No error occurs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Items may also be removed by calling BBGETS, using 'TAKE'.
!     However, BBREMS allows you to remove an item without
!     providing space for its value. Also, it does not cause an
!     error if the item is not on the board.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMS_1' )
      END IF

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
      CALL SBREM_1  ( WHICH, NBUF, PBUF, VBUF )

      CALL CHKOUT ( 'BBREMS_1' )
      RETURN




!$Procedure BBFNDI ( Bulletin board, find, integer )

      ENTRY BBFNDI_1 ( ITEM, N )

! $ Abstract
!
!     Find an integer item on the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!     N          O   Number of values.
!
! $ Detailed_Input
!
!     ITEM        is the name of an integer item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     N           is the number of values associated with the item.
!                 If the item is not on the board, N is zero.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     BBFNDI has two main uses:
!
!        1) To confirm that an item exists before attempting to
!           copy or take its values (anticipating a possible error).
!
!        2) To determine the number of values associated with an
!           item, so that the right number of values can be popped
!           from the value list.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDI_1' )
      END IF

!
!     Compress spaces as needed.
!
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by a symbol table routine. (Later,
!     it will be done by a card catalog routine.)
!
      N = SYDIMI ( WHICH, INTAB, IPTAB, IVTAB )

      CALL CHKOUT ( 'BBFNDI_1' )
      RETURN


!$Procedure BBFNDD ( Bulletin board, find, DP )

      ENTRY BBFNDD_1 ( ITEM, N )

! $ Abstract
!
!     Find a DP item on the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!     N          O   Number of values.
!
! $ Detailed_Input
!
!     ITEM        is the name of a DP item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     N           is the number of values associated with the item.
!                 If the item is not on the board, N is zero.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     BBFNDD has two main uses:
!
!        1) To confirm that an item exists before attempting to
!           copy or take its values (anticipating a possible error).
!
!        2) To determine the number of values associated with an
!           item, so that the right number of values can be popped
!           from the value list.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDD_1' )
      END IF

!
!     Compress spaces as needed.
!
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by a symbol table routine. (Later,
!     it will be done by a card catalog routine.)
!
      N = SYDIMD ( WHICH, DNTAB, DPTAB, DVTAB )

      CALL CHKOUT ( 'BBFNDD_1' )
      RETURN


!$Procedure BBFNDC ( Bulletin board, find, character )

      ENTRY BBFNDC_1 ( ITEM, N )

! $ Abstract
!
!     Find a character item on the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!     N          O   Number of values.
!
! $ Detailed_Input
!
!     ITEM        is the name of a character item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     N           is the number of values associated with the item.
!                 If the item is not on the board, N is zero.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     BBFNDC has two main uses:
!
!        1) To confirm that an item exists before attempting to
!           copy or take its values (anticipating a possible error).
!
!        2) To determine the number of values associated with an
!           item, so that the right number of values can be popped
!           from the value list.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDC_1' )
      END IF

!
!     Compress spaces as needed.
!
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )

!
!     The real work is done by a symbol table routine. (Later,
!     it will be done by a card catalog routine.)
!
      N = SYDIMC ( WHICH, CNTAB, CPTAB, CVTAB )

      CALL CHKOUT ( 'BBFNDC_1' )
      RETURN


!$Procedure BBFNDS ( Bulletin board, find, string )

      ENTRY BBFNDS_1 ( ITEM, N )

! $ Abstract
!
!     Find a string item on the board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         ITEM
!     INTEGER               N
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item name.
!     N          O   Number of values.
!
! $ Detailed_Input
!
!     ITEM        is the name of a string item, which may or
!                 may not be on the board.
!
! $ Detailed_Output
!
!     N           is the number of values associated with the item.
!                 If the item is on the board, N is one. Otherwise
!                 N is zero.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     BBFNDS is used mainly to confirm that an item exists before
!     attempting to copy or take its value (anticipating a possible
!     error).
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDS_1' )
      END IF

      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
      CALL SBGET_1  ( WHICH, NBUF, PBUF, VBUF, WHAT, POS )

      IF ( POS .GT. 0 ) THEN
         N = 1
      ELSE
         N = 0
      END IF

      CALL CHKOUT ( 'BBFNDS_1' )
      RETURN




!$Procedure BBCLR ( Bulletin board, clear )

      ENTRY BBCLR_1

! $ Abstract
!
!     Clear the entire board.
!
! $ Required_Reading
!
!     BBOARD
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See BBOARD.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     BBCLR clears the entire bulletin board. The board MUST be
!     cleared at least once before any items can be posted.
!     This is usually done by the main module of the calling
!     program, during program initialization.
!
! $ Examples
!
!     See BBOARD.
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
!     Dagny Taggart  (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 5-APR-1989 (DMT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBCLR_1' )
      END IF

!
!     Wipe out all three symbol tables.
!
      CALL SSIZEC ( MAXI,  INTAB )
      CALL SSIZEI ( MAXI,  IPTAB )
      CALL SSIZEI ( MAXIV, IVTAB )

      CALL SSIZEC ( MAXD,  DNTAB )
      CALL SSIZEI ( MAXD,  DPTAB )
      CALL SSIZED ( MAXDV, DVTAB )

      CALL SSIZEC ( MAXC,  CNTAB )
      CALL SSIZEI ( MAXC,  CPTAB )
      CALL SSIZEC ( MAXCV, CVTAB )

!
!     Re-initialize the string buffer.
!
      CALL SBINIT_1 ( MAXS, &
                    MAXS * 4 + 4, &
                    MAXCHR / 100, &
                    NBUF, &
                    PBUF, &
                    VBUF          )

      CALL CHKOUT ( 'BBCLR_1' )
      RETURN


      END
