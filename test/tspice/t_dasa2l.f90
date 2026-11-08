!$Procedure      T_DASA2L ( DAS, address to physical location )

      SUBROUTINE T_DASA2L (  HANDLE,  TYPE,    ADDRSS, &
                             CLBASE,  CLSIZE,  RECNO,  WORDNO  )

! $ Abstract
!
!     This is a TSPICE utility routine.
!
!     Map a DAS address to a physical location in the DAS file
!     it refers to.
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
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     TRANSFORMATION
!     UTILITY
!
! $ Declarations


      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               ADDRSS
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               RECNO
      INTEGER               WORDNO

      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     TYPE       I   Data type specifier.
!     ADDRSS     I   DAS address of a word of data type TYPE.
!     CLBASE,
!     CLSIZE     O   Cluster base record number and size.
!     RECNO,
!     WORDNO     O   Record/word pair corresponding to ADDRSS.
!     CHAR       P   Parameter indicating character data type.
!     DP         P   Parameter indicating double precision data type.
!     INT        P   Parameter indicating integer data type.
!
! $ Detailed_Input
!
!     HANDLE         is the file handle of an open DAS file.
!
!     TYPE           is a data type specifier.  TYPE may be any of
!                    the parameters
!
!                       CHAR
!                       DP
!                       INT
!
!                    which indicate `character', `double precision',
!                    and `integer' respectively.
!
!
!     ADDRSS         is the address in a DAS of a word of data
!                    type TYPE.  For each data type (double precision,
!                    integer, or character), addresses range
!                    from 1 to the maximum current value for that type,
!                    which is available from DAFRFR.
!
! $ Detailed_Output
!
!     CLBASE,
!     CLSIZE         are, respectively, the base record number and
!                    size, in records, of the cluster containing the
!                    word corresponding to ADDRSS.  The cluster spans
!                    records numbered CLBASE through CLBASE +
!                    CLSIZE - 1.
!
!     RECNO,
!     WORD           are, respectively, the number of the physical
!                    record and the number of the word within the
!                    record that correspond to ADDRSS.  Word numbers
!                    start at 1 and go up to NC, ND, or NI in
!                    character, double precision, or integer records
!                    respectively.
!
! $ Parameters
!
!     CHAR,
!     DP,
!     INT            are data type specifiers which indicate
!                    `character', `double precision', and `integer'
!                    respectively.  These parameters are used in
!                    all DAS routines that require a data type
!                    specifier as input.
!
! $ Exceptions
!
!     1)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE)
!         will be signaled.
!
!     2)  ADDRSS must be between 1 and LAST inclusive, where LAST
!         is last address in the DAS for a word of the specified
!         type.  If ADDRSS is out of range, the error
!         SPICE(DASNOSUCHADDRESS) will be signaled.
!
!     3)  If this routine fails to find directory information for
!         the input address, the error SPICE(NOSUCHRECORD) will be
!         signaled.
!
!     4)  If the input handle is invalid, the error will be diagnosed
!         by routines called by this routine.
!
!
!     If any of the above exceptions occur, the output arguments may
!     contain bogus information.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     The DAS architecture allows a programmer to think of the data
!     within a DAS file as three one-dimensional arrays:  one of
!     double precision numbers, one of integers, and one of characters.
!     This model allows a programmer to ask the DAS system for the
!     `nth double precision number (or integer, or character) in the
!     file'.
!
!     DAS files are Fortran direct access files, so to find the
!     `nth double precision number', you must have the number of the
!     record containing it and the `word number', or position, within
!     the record of the double precision number.  This routine finds
!     the record/word number pair that specify the physical location
!     in a DAS file corresponding to a DAS address.
!
!     As opposed to DAFs, the mapping of addresses to physical locations
!     for a DAS file depends on the organization of data in the file.
!     Given a fixed set of DAS format parameters, the physical location
!     of the nth double precision number can depend on how many integer
!     and character records have been written prior to the record
!     containing that double precision number.
!
!     The cluster information output from this routine allows the
!     caller to substantially reduce the number of directory reads
!     required to read a from range of addresses that spans
!     multiple physical records; the reading program only need call
!     this routine once per cluster read, rather than once per
!     physical record read.
!
! $ Examples
!
!     1)  Use this routine to read integers from a range of
!         addresses.  This is done in the routine DASRDI.
!
!            C
!            C     Decide how many integers to read.
!            C
!                  NUMINT = LAST - FIRST + 1
!                  NREAD  = 0
!
!            C
!            C     Find out the physical location of the first
!            C     integer.  If FIRST is invalid, T_DASA2L will take
!            C     care of the problem.
!            C
!
!                  CALL T_DASA2L ( HANDLE,  INT,     FIRST,
!                 .                CLBASE,  CLSIZE,  RECNO,  WORDNO  )
!
!            C
!            C     Read as much data from record RECNO as necessary.
!            C
!                  N  =  MIN ( NUMINT,  NWI - WORDNO + 1 )
!
!                  CALL DASRRI ( HANDLE, RECNO, WORDNO, WORDNO + N-1,
!                 .              DATA                                 )
!
!                  NREAD  =  N
!                  RECNO  =  RECNO + 1
!
!            C
!            C     Read from as many additional records as necessary.
!            C
!                  DO WHILE ( NREAD .LT. NUMINT )
!            C
!            C        At this point, RECNO is the correct number of the
!            C        record to read from next.  CLBASE is the number
!            C        of the first record of the cluster we're about
!            C        to read from.
!            C
!
!                     IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
!            C
!            C           We can continue reading from the current
!            C           cluster.
!            C
!                        N  =  MIN ( NUMINT - NREAD,  NWI )
!
!                        CALL DASRRI (  HANDLE,
!                 .                     RECNO,
!                 .                     1,
!                 .                     N,
!                 .                     DATA ( NREAD + 1 )   )
!
!                        NREAD   =   NREAD + N
!                        RECNO   =   RECNO + 1
!
!
!                     ELSE
!            C
!            C           We must find the next integer cluster to
!            C           read from.  The first integer in this
!            C           cluster has address FIRST + NREAD.
!            C
!                        CALL T_DASA2L ( HANDLE,
!                 .                      INT,
!                 .                      FIRST + NREAD,
!                 .                      CLBASE,
!                 .                      CLSIZE,
!                 .                      RECNO,
!                 .                      WORDNO  )
!
!                     END IF
!
!                  END DO
!
!
! $ Restrictions
!
!     This is a TSPICE utility. It should not be called by routines
!     other than TSPICE test families.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer (JPL)
!     N.J. Bachman   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 24-FEB-2014 (NJB)
!
!        This is based on the DASA2L version
!
!           SPICELIB Version 1.2.1 20-NOV-2001
!
!        A bug fix to update TBFAST has been included. Various
!        initializations are now performed to satisfy ftnchek.
!
!-&

! $ Index_Entries
!
!     map DAS logical address to physical location
!
!-&

! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!
      INTEGER               ISRCHI

!
!     Local parameters
!

!
!     Words per data record, for each data type:
!
      INTEGER               NWC
      PARAMETER           ( NWC = 1024 )

      INTEGER               NWD
      PARAMETER           ( NWD =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI =  256 )

!
!     Forward directory pointer location:
!
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )

!
!     Directory address range locations
!
      INTEGER               CHRRNG
      PARAMETER           ( CHRRNG =          3 )

      INTEGER               DPRNG
      PARAMETER           ( DPRNG  = CHRRNG + 2 )

      INTEGER               INTRNG
      PARAMETER           ( INTRNG = DPRNG  + 2 )

!
!     Indices of highest address in a `range array':
!
      INTEGER               HIGH
      PARAMETER           ( HIGH = 2 )

!
!     Location of first type descriptor
!
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )

!
!     Access word length
!
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 10 )

!
!     File table size
!
      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = 20 )

      INTEGER               MAXVEC
      PARAMETER           ( MAXVEC = 3 * MAXFIL )

!
!     Local variables
!
      CHARACTER*(ACCLEN)    ACCESS

      INTEGER               CURTYP
      INTEGER               DIRREC ( NWI )
      INTEGER               DSCLOC
      INTEGER               FIDX
      INTEGER               FREE
      INTEGER               HIADDR
      INTEGER               I
      INTEGER               LSTREC ( 3 )
      INTEGER               LSTWRD ( 3 )
      INTEGER               MXADDR
      INTEGER               MXCLRC
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NDIRS
      INTEGER               NEXT   ( 3 )
      INTEGER               NFILES
      INTEGER               NREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NTYPES
      INTEGER               NW     ( 3 )
      INTEGER               NXTREC
      INTEGER               PREV   ( 3 )
      INTEGER               PRVHAN
      INTEGER               PRVTYP
      INTEGER               RANGE  ( 2 )
      INTEGER               RNGLOC ( 3 )
      INTEGER               TBBASE ( 3, MAXFIL )
      INTEGER               TBHAN  (    MAXFIL )
      INTEGER               TBMXAD ( 3, MAXFIL )
      INTEGER               TBSIZE ( 3, MAXFIL )
      INTEGER               UNIT

      LOGICAL               FAST
      LOGICAL               FIRST
      LOGICAL               KNOWN
      LOGICAL               RDONLY
      LOGICAL               TBFAST ( MAXFIL )
      LOGICAL               SAMFIL



!
!     Saved variables
!
      SAVE

!
!     Initial values
!
!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      DATA                  NEXT   /  2,   3,   1   /
      DATA                  PREV   /  3,   1,   2   /
      DATA                  NW     /  NWC,    NWD,   NWI    /
      DATA                  RNGLOC /  CHRRNG, DPRNG, INTRNG /

      DATA                  FIDX   / 0                /
      DATA                  FIRST  / .TRUE.           /
      DATA                  NFILES / 0                /
      DATA                  TBBASE / MAXVEC * -1      /
      DATA                  TBFAST / MAXFIL * .FALSE. /
      DATA                  TBHAN  / MAXFIL *  0      /
      DATA                  TBMXAD / MAXVEC * -1      /
      DATA                  TBSIZE / MAXVEC * -1      /

!
!     Discovery check-in is used in this routine.
!
!
!     DAS files have the following general structure:
!
!           +------------------------+
!           |      file record       |
!           +------------------------+
!           |    reserved records    |
!           |                        |
!           +------------------------+
!           |     comment records    |
!           |                        |
!           |                        |
!           |                        |
!           +------------------------+
!           | first data directory   |
!           +------------------------+
!           |      data records      |
!           |                        |
!           |                        |
!           |                        |
!           |                        |
!           +------------------------+
!                       .
!                       .
!           +------------------------+
!           | last data directory    |
!           +------------------------+
!           |     data records       |
!           |                        |
!           |                        |
!           +------------------------+
!
!
!        Within each DAS data record, word numbers start at one and
!        increase up to NWI, NWD, or NWC:  the number of words in an
!        integer, double precision, or character data record.
!
!
!           +--------------------------------+
!           |       |       |   ...  |       |
!           +--------------------------------+
!               1      2                NWD
!
!           +--------------------------------+
!           |   |   |       ...          |   |
!           +--------------------------------+
!             1   2                       NWI
!
!           +------------------------------------+
!           | | |           ...                | |
!           +------------------------------------+
!            1 2                               NWC
!
!
!        Directories are single records that describe the data
!        types of data records that follow.  The directories
!        in a DAS file form a doubly linked list:  each directory
!        contains forward and backward pointers to the next and
!        previous directories.
!
!        Each directory also contains, for each data type, the lowest
!        and highest logical address occurring in any of the records
!        described by the directory.
!
!        Following the pointers and address range information is
!        a sequence of data type descriptors.  These descriptors
!        indicate the data type of data records following the
!        directory record.  Each descriptor gives the data type
!        of a maximal set of contiguous data records, all having the
!        same type.  By `maximal set' we mean that no data records of
!        the same type bound the set of records in question.
!
!        Pictorially, the structure of a directory is as follows:
!
!           +----------------------------------------------------+
!           | <pointers> | <address ranges> | <type descriptors> |
!           +----------------------------------------------------+
!
!        where the <pointers> section looks like
!
!           +-----------------------------------------+
!           | <backward pointer> | <forward pointer>  |
!           +-----------------------------------------+
!
!        the <address ranges> section looks like
!
!           +-------------------------------------------+
!           | <char range> | <d.p. range> | <int range> |
!           +-------------------------------------------+
!
!        and each range looks like one of:
!
!           +------------------------------------------------+
!           | <lowest char address> | <highest char address> |
!           +------------------------------------------------+
!
!           +------------------------------------------------+
!           | <lowest d.p. address> | <highest d.p. address> |
!           +------------------------------------------------+
!
!           +------------------------------------------------+
!           | <lowest int address>  | <highest int address>  |
!           +------------------------------------------------+
!
!        The type descriptors implement a run-length encoding
!        scheme.  The first element of the series of descriptors
!        occupies two integers:  it contains a type code and a count.
!        The rest of the descriptors are just signed counts; the data
!        types of the records they describe are deduced from the sign
!        of the count and the data type of the previous descriptor.
!        The method of finding the data type for a given descriptor
!        in terms of its predecessor is as follows:  if the sign of a
!        descriptor is positive, the type of that descriptor is the
!        successor of the type of the preceding descriptor in the
!        sequence of types below.  If the sign of a descriptor is
!        negative, the type of the descriptor is the predecessor of the
!        type of the preceding descriptor.
!
!           C  -->  D  -->  I  -->  C
!
!        For example, if the preceding type is `I', and a descriptor
!        contains the number 16, the type of the descriptor is `C',
!        whereas if the descriptor contained the number -800, the type
!        of the descriptor would be `D'.
!


!
!     Make sure the data type is valid.
!
      IF (  ( TYPE .LT. CHAR ) .OR. ( TYPE .GT. INT )  ) THEN

         CALL CHKIN  ( 'T_DASA2L'                          )
         CALL DASHLU (  HANDLE, UNIT                       )
         CALL SETMSG ( 'Invalid data type: #.  File was #' )
         CALL ERRINT ( '#',  TYPE                          )
         CALL ERRFNM ( '#',  UNIT                          )
         CALL SIGERR ( 'SPICE(DASINVALIDTYPE)'             )
         CALL CHKOUT ( 'T_DASA2L'                          )
         RETURN

      END IF


!
!     Decide whether we're looking at the same file as we did on
!     the last call.
!
      IF ( FIRST ) THEN

         SAMFIL = .FALSE.
         FAST   = .FALSE.
         PRVHAN =  HANDLE
         FIRST  = .FALSE.

      ELSE

         SAMFIL =  HANDLE .EQ. PRVHAN
         PRVHAN =  HANDLE

      END IF

!
!     We have a special case if we're looking at a `fast' file
!     that we saw on the last call.  When we say a file is fast,
!     we're implying that it's open for read access only and that it's
!     segregated.  In this case, we can do an address calculation
!     without looking up any information from the file.
!
      IF ( SAMFIL .AND. FAST ) THEN

         CLBASE  =  TBBASE ( TYPE, FIDX )
         CLSIZE  =  TBSIZE ( TYPE, FIDX )
         MXADDR  =  TBMXAD ( TYPE, FIDX )
         HIADDR  =  CLSIZE * NW(TYPE)

!
!        Make sure that ADDRSS points to an existing location.
!
         IF (  ( ADDRSS .LT. 1 ) .OR. ( ADDRSS .GT. MXADDR )  ) THEN

            CALL CHKIN  ( 'T_DASA2L'                                   )
            CALL DASHLU (  HANDLE, UNIT                                )
            CALL SETMSG ( 'ADDRSS was #; valid range for type # is '  // &
                          '# to #.  File was #'                        )
            CALL ERRINT ( '#',  ADDRSS                                 )
            CALL ERRINT ( '#',  TYPE                                   )
            CALL ERRINT ( '#',  1                                      )
            CALL ERRINT ( '#',  MXADDR                                 )
            CALL ERRFNM ( '#',  UNIT                                   )
            CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'                    )
            CALL CHKOUT ( 'T_DASA2L'                                   )
            RETURN

         END IF


      ELSE
!
!        If the current file is not the same one we looked at on the
!        last call, find out whether the file is on record in our file
!        table.  Add the file to the table if necessary.  Bump the
!        oldest file in the table if there's no room.
!
         IF ( .NOT. SAMFIL ) THEN

            FIDX  =  ISRCHI ( HANDLE, NFILES, TBHAN )
            KNOWN =  FIDX .GT. 0

            IF ( KNOWN ) THEN
!
!              The file is in our list.
!
               FAST = TBFAST(FIDX)

               IF ( FAST ) THEN
!
!                 This is a segregated, read-only file.  Look up the
!                 saved information we'll need to calculate addresses.
!
                  CLBASE = TBBASE( TYPE, FIDX )
                  CLSIZE = TBSIZE( TYPE, FIDX )
                  MXADDR = TBMXAD( TYPE, FIDX )
                  HIADDR = CLSIZE * NW(TYPE)

!
!                 Make sure that ADDRSS points to an existing location.
!
                  IF (     ( ADDRSS .LT. 1      ) &
                      .OR. ( ADDRSS .GT. MXADDR )  ) THEN

                     CALL CHKIN  ( 'T_DASA2L'                        )
                     CALL DASHLU (  HANDLE, UNIT                     )
                     CALL SETMSG ( 'ADDRSS was #; valid range for ' // &
                                   ' type # is # to #.  File was #'  )
                     CALL ERRINT ( '#',  ADDRSS                      )
                     CALL ERRINT ( '#',  TYPE                        )
                     CALL ERRINT ( '#',  1                           )
                     CALL ERRINT ( '#',  MXADDR                      )
                     CALL ERRFNM ( '#',  UNIT                        )
                     CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'         )
                     CALL CHKOUT ( 'T_DASA2L'                        )
                     RETURN

                  END IF

               END IF
!
!              FAST is set.
!

            END IF
!
!           KNOWN is set.
!

         END IF

!
!        SAMFIL, FAST, and KNOWN are set.  If the file is the same one
!        we saw on the last call, the state variables FAST, and KNOWN
!        retain their values from the previous call.
!
!        FIDX is set at this point only if we're looking at a known
!        file.
!
!        Unless the file is recognized and known to be a fast file, we
!        look up all metadata for the file.
!
         IF ( .NOT. ( KNOWN .AND. FAST )  ) THEN


            IF ( .NOT. KNOWN ) THEN
!
!              This file is not in our list.  If the list is not full,
!              append the file to the list.  If the list is full,
!              replace the oldest (first) file with this one.
!
               IF ( NFILES .LT. MAXFIL ) THEN
                  NFILES  =  NFILES + 1
                  FIDX    =  NFILES
               ELSE
                  FIDX    =  1
               END IF


               TBHAN(FIDX)  =  HANDLE

!
!              Find out whether the file is open for read or write
!              access.  We consider the file to be `slow' until we find
!              out otherwise.  The contents of the arrays TBHIGH,
!              TBBASE, TBSIZE, and TBMXAD are left undefined for slow
!              files.
!
               CALL DASHAM ( HANDLE, ACCESS )

               RDONLY        =  ACCESS .EQ. 'READ'
               FAST          =  .FALSE.
               TBFAST(FIDX)  =  FAST

!
!              We'll set the flag KNOWN at the end of the outer IF
!              block.
!
            ELSE
!
!              We set RDONLY to .FALSE. for any known file that is
!              not fast.  It's actually possible for a read-only file
!              to be unsegregated, but this is expected to be a rare
!              case, one that's not worth complicating this routine
!              further for.
!
               RDONLY = .FALSE.

            END IF

!
!           RDONLY is set.
!
!           FIDX is now set whether or not the current file is known.
!
!           Get the number of reserved records, comment records, and
!           the current last address of the data type TYPE from the
!           file  summary.
!
            CALL DASHFS ( HANDLE, &
                          NRESVR, &
                          NRESVC, &
                          NCOMR, &
                          NCOMC, &
                          FREE, &
                          TBMXAD(1,FIDX), &
                          LSTREC, &
                          LSTWRD )

            MXADDR = TBMXAD( TYPE, FIDX )

!
!           Make sure that ADDRSS points to an existing location.
!
            IF (  ( ADDRSS .LT. 1 ) .OR. ( ADDRSS .GT. MXADDR )  ) THEN

               CALL CHKIN  ( 'T_DASA2L'                        )
               CALL DASHLU (  HANDLE, UNIT                     )
               CALL SETMSG ( 'ADDRSS was #; valid range for ' // &
                             ' type # is # to #.  File was #'  )
               CALL ERRINT ( '#',  ADDRSS                      )
               CALL ERRINT ( '#',  TYPE                        )
               CALL ERRINT ( '#',  1                           )
               CALL ERRINT ( '#',  MXADDR                      )
               CALL ERRFNM ( '#',  UNIT                        )
               CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'         )
               CALL CHKOUT ( 'T_DASA2L'                        )
               RETURN

            END IF

!
!           Find out which directory describes the cluster containing
!           this word.  To do this, we must traverse the directory
!           list.  The first directory record comes right after the
!           last comment record.  (Don't forget the file record when
!           counting the predecessors of the directory record.)
!
!           Note that we don't need to worry about not finding a
!           directory record that contains the address we're looking
!           for, since we've already checked that the address is in
!           range.
!
!           Keep track of the number of directory records we see.  We'll
!           use this later to determine whether we've got a segregated
!           file.
!
            NREC  =  NRESVR + NCOMR + 2
            NDIRS =  1

            CALL DASRRI ( HANDLE, &
                          NREC, &
                          RNGLOC(TYPE), &
                          RNGLOC(TYPE)+1, &
                          RANGE )


            DO WHILE ( RANGE(HIGH) .LT. ADDRSS )
!
!              The record number of the next directory is the forward
!              pointer in the current directory record.  Update NREC
!              with this pointer.  Get the address range for the
!              specified type covered by this next directory record.
!
               CALL DASRRI ( HANDLE, NREC, FWDLOC, FWDLOC, NXTREC )

               NREC  = NXTREC
               NDIRS = NDIRS + 1

               CALL DASRRI ( HANDLE, &
                             NREC, &
                             RNGLOC(TYPE), &
                             RNGLOC(TYPE)+1, &
                             RANGE           )

            END DO

!
!           NREC is now the record number of the directory that contains
!           the type descriptor for the address we're looking for.
!
!           Our next task is to find the descriptor for the cluster
!           containing the input address.  To do this, we must examine
!           the directory record in `left-to-right' order.  As we do so,
!           we'll keep track of the highest address of type TYPE
!           occurring in the clusters whose descriptors we've seen.
!           The variable HIADDR will contain this address.
!
            CALL DASRRI ( HANDLE, NREC, 1, NWI, DIRREC )

!
!           In the process of finding the physical location
!           corresponding to ADDRSS, we'll find the record number of the
!           base of the cluster containing ADDRSS.  We'll start out by
!           initializing this value with the number of the first data
!           record of the next cluster.
!
            CLBASE  =  NREC + 1

!
!           We'll initialize HIADDR with the value preceding the lowest
!           address of type TYPE described by the current directory.
!
            HIADDR  =  DIRREC(RNGLOC(TYPE)) - 1

!
!           Initialize the number of records described by the last seen
!           type descriptor.  This number, when added to CLBASE, should
!           yield the number of the first record of the current cluster;
!           that's why it's initialized to 0.
!
            CLSIZE  =  0

!
!           Now find the descriptor for the cluster containing ADDRSS.
!           Read descriptors until we get to the one that describes the
!           record containing ADDRSS.  Keep track of descriptor data
!           types as we go.  Also count the descriptors.
!
!           At this point, HIADDR is less than ADDRSS, so the loop will
!           always be executed at least once.
!
            PRVTYP  =  PREV ( DIRREC(BEGDSC) )
            DSCLOC  =  BEGDSC + 1

            DO WHILE ( HIADDR .LT. ADDRSS )
!
!              Update CLBASE so that it is the record number of the
!              first record of the current cluster.
!
               CLBASE = CLBASE + CLSIZE

!
!              Find the type of the current descriptor.
!
               IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
                  CURTYP  =   NEXT( PRVTYP )
               ELSE
                  CURTYP  =   PREV( PRVTYP )
               END IF

!
!              Forgetting to update PRVTYP is a Very Bad Thing (VBT).
!
               PRVTYP = CURTYP

!
!              If the current descriptor is of the type we're interested
!              in, update the highest address count.
!
               IF ( CURTYP .EQ. TYPE ) THEN
                  HIADDR = HIADDR + ( NW(TYPE) * ABS( DIRREC(DSCLOC) ) )
               END IF

!
!              Compute the number of records described by the current
!              descriptor.  Update the descriptor location.
!
               CLSIZE  =  ABS (  DIRREC( DSCLOC )  )
               DSCLOC  =  DSCLOC + 1

            END DO

!
!           If we have an unknown read-only file, see whether the file
!           is segregated.  If it is, we'll be able to compute
!           addresses much faster for subsequent reads to this file.
!
            IF (  RDONLY  .AND.  ( .NOT. KNOWN )  ) THEN

               IF ( NDIRS .EQ. 1 ) THEN
!
!                 If this file is segregated, there are at most three
!                 cluster descriptors, and each one points to a cluster
!                 containing all records of the corresponding data type.
!                 For each data type having a non-zero maximum address,
!                 the size of the corresponding cluster must be large
!                 enough to hold all addresses of that type.
!
                  NTYPES = 0

                  DO I = 1, 3

                     IF ( TBMXAD(I,FIDX) .GT. 0 ) THEN
                        NTYPES = NTYPES + 1
                     END IF

                  END DO

!
!                 Now look at the first NTYPES cluster descriptors,
!                 collecting cluster bases and sizes as we go.
!
                  MXCLRC  =  NREC   + 1
                  PRVTYP  =  PREV ( DIRREC(BEGDSC) )
                  DSCLOC  =  BEGDSC + 1
                  FAST    = .TRUE.

                  DO WHILE (       ( DSCLOC .LE. (BEGDSC+NTYPES) ) &
                             .AND.   FAST                            )
!
!                    Find the type of the current descriptor.
!
                     IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
                        CURTYP  =   NEXT( PRVTYP )
                     ELSE
                        CURTYP  =   PREV( PRVTYP )
                     END IF

                     PRVTYP                 =  CURTYP
                     TBBASE( CURTYP, FIDX ) =  MXCLRC
                     TBSIZE( CURTYP, FIDX ) =  ABS ( DIRREC( DSCLOC ) )
                     MXCLRC                 =  MXCLRC &
                                             + TBSIZE (CURTYP, FIDX )

                     FAST    =       TBMXAD( CURTYP, FIDX ) &
                               .LE.  TBSIZE( CURTYP, FIDX ) * NW(CURTYP)

                     DSCLOC  =  DSCLOC + 1

                  END DO
!
!                 FAST is set.
!
                  TBFAST( FIDX ) = FAST

               ELSE
!
!                 The file has more than one directory record.
!
                  FAST = .FALSE.

               END IF
!
!              If the file was unknown, readonly, and had one directory
!              record, we determined whether it was a fast file.
!
            ELSE
!
!              The file was already known and wasn't fast, or is not
!              readonly.
!
               FAST = .FALSE.

            END IF
!
!           FAST is set.
!

         END IF
!
!        This is the end of the `.NOT. ( KNOWN .AND. FAST )' case.
!
!        At this point, we've set or looked up CLBASE, CLSIZE, MXADDR,
!        and HIADDR.
!
!        If the file was unknown, we set TBHAN, TBRDON, and TBFAST.
!        If the file was unknown and turned out to be fast, we set
!        TBBASE, TBSIZE, TBHIGH, and TBMXAD as well.
!
!        At this point, it's safe to indicate that the file is known.
!
         KNOWN  =  .TRUE.

      END IF

!
!     At this point,
!
!        -- CLBASE is properly set:  it is the record number of the
!           first record of the cluster containing ADDRSS.
!
!        -- CLSIZE is properly set:  it is the size of the cluster
!           containing ADDRSS.
!
!        -- HIADDR is the last logical address in the cluster
!           containing ADDRSS.
!
!     Now we must find the physical record and word corresponding
!     to ADDRSS.  The structure of the cluster containing ADDRSS and
!     HIADDR is shown below:
!
!        +--------------------------------------+
!        |                                      |  Record # CLBASE
!        +--------------------------------------+
!                           .
!                           .
!                           .
!        +--------------------------------------+
!        |      |ADDRSS|                        |  Record # RECNO
!        +--------------------------------------+
!                           .
!                           .
!                           .
!        +--------------------------------------+  Record #
!        |                               |HIADDR|
!        +--------------------------------------+  CLBASE + CLSIZE - 1
!
!
!
      RECNO  =     ( CLBASE + CLSIZE - 1 ) &
                -  ( HIADDR - ADDRSS     ) / NW(TYPE)

      WORDNO =    ADDRSS    -   (  (ADDRSS-1) / NW(TYPE) ) * NW(TYPE)

      RETURN
      END
