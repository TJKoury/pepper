!$Procedure DASRWR ( DAS, read/write records )

      SUBROUTINE DASRWR ( HANDLE, &
                          RECNO, &
                          RECC, &
                          RECD, &
                          RECI, &
                          FIRST, &
                          LAST, &
                          DATAD, &
                          DATAI, &
                          DATAC   )

! $ Abstract
!
!     Read and write DAS physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               NWD
      PARAMETER           ( NWD    =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )

      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

      INTEGER               HANDLE
      INTEGER               RECNO
      CHARACTER*(*)         RECC
      DOUBLE PRECISION      RECD    ( NWD )
      INTEGER               RECI    ( NWI )
      INTEGER               FIRST
      INTEGER               LAST
      DOUBLE PRECISION      DATAD   ( * )
      INTEGER               DATAI   ( * )
      CHARACTER*(*)         DATAC

      INTEGER               BUFSZD
      PARAMETER           ( BUFSZD = 10 )

      INTEGER               BUFSZI
      PARAMETER           ( BUFSZI = 10 )

      INTEGER               BUFSZC
      PARAMETER           ( BUFSZC = 10 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     HANDLE     I   RRD, RRI, RRC, WRD, WRI, WRC, URD, URI, URC
!     RECNO      I   RRD, RRI, RRC, WRD, WRI, WRC, URD, URI, URC
!     RECC       I   WRC
!     RECD       I   WRD
!     RECI       I   WRI
!     FIRST      I   RRD, RRI, RRC, URD, URI, URC
!     LAST       I   RRD, RRI, RRC, URD, URI, URC
!     DATAD      O   RRD, URD
!     DATAI      O   RRI, URI
!     DATAC      O   RRC, URC
!     NWD        P   RRD, WRD, URD
!     NWI        P   RRI, WRI, URI
!     NWC        P   RRC, WRC, URC
!     BUFSZD     P   RRD, WRD, URD
!     BUFSZI     P   RRI, WRI, URI
!     BUFSZC     P   RRC, WRC, URC
!
! $ Detailed_Input
!
!     See the entry points for a discussion of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for a discussion of their outputs.
!
! $ Parameters
!
!     NWD      is the number of DPs in a single DAS record
!              containing DPs.
!
!     NWI      is the number of integers in a single DAS record
!              containing integers.
!
!     NWC      is the number of characters in a single DAS record
!              containing characters.
!
!     BUFSZD,
!     BUFSZI,
!     BUFSZC   are, respectively, the number of records in the
!              data buffers for double precision, integer, and
!              character records.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     2)  See the entry points for discussions of their exceptions.
!
! $ Files
!
!     See the description of the argument HANDLE in the headers of
!     the entry points for a description of files accessed by this
!     set of routines.
!
! $ Particulars
!
!     This suite of routines provides buffered read and write access to
!     DAS files. The purpose of this feature is to increase the
!     performance of application programs that access DAS files: in
!     particular, repeated reads from or writes to a given record
!     should be relatively fast, because the contents of the most
!     recently accessed records are buffered in memory. Thus DASRWR
!     and its entry points act as a miniature virtual memory system for
!     DAS files.
!
!     These routines are intended primarily for use by other SPICELIB
!     routines; users' application programs will not normally need to
!     call these routines. Writing to a DAS file with these routines
!     demands a particularly circumspect approach: it's quite easy to
!     end up with something other than a DAS file if one misuses the
!     routines.
!
!     The entry points of DASRWR support writing, reading, and updating
!     the records in a DAS file. The distinction between writing and
!     updating is that any record may be written (as long as the record
!     belongs to a file open for writing), but only existing records
!     may be updated.  `Writing' a record sets the values of all of
!     the elements of the record, while a subrange of the elements of an
!     existing record may be `updated'.
!
!     For each of these three operations, there are three DAS routines,
!     one for each supported data type. The names of the routines are
!
!        -- For writing:     DASWRC,  DASWRD,  DASWRI
!        -- For updating:    DASURC,  DASURD,  DASURI
!        -- For reading:     DASRRC,  DASRRD,  DASRRI
!
!     Users should note that, unlike in the case of SPICELIB's DAF
!     routines, the DAS routines buffer data that is written as well
!     as data that is read. Consequently a DAS file does not
!     necessarily yet contain, at any moment, all of the data that
!     has been written to it by the DASWRx or DASURx routines. The
!     written data that is buffered is written out when the need
!     to buffer additional data requires it, and also when the user
!     commands the closure of a file that has been written. So, at
!     the time a DAS file is closed, the contents of the physical file
!     do reflect what has been `written' to the file by the DASWRx and
!     DASURx entry points.
!
!     At any time, an application program can force the DAS system to
!     write to a DAS file any buffered records maintained for that
!     file. The entry point DASWBR (DAS, write buffered records)
!     provides this capability.
!
!     DASRWR contains three record buffers: one of character type,
!     one of double precision type, and one of integer type. Each
!     buffer has enough room for an integer number of records. The
!     sizes of the buffers are parameterized and can be increased if
!     necessary. When contemplating the revision of the buffer
!     sizes selected by NAIF, SPICELIB users should take note of the
!     following points:
!
!        -- Changing values of parameters in NAIF subroutines may cause
!           a maintenance burden for the users of the modified NAIF
!           code, since any changes made to a SPICELIB routine will have
!           to be made to any new version of that routine released by
!           NAIF in a later version of SPICELIB.
!
!        -- The effect of buffer size on the speed with which an
!           application executes is highly dependent on the specific
!           application. In some cases, increasing the buffer sizes
!           may slow the application down.
!
! $ Examples
!
!     See the entry points for examples specific to those routines.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 07-OCT-2021 (NJB) (JDR)
!
!        Added initializers for record buffers.
!
!        Edited the headers of DASRWR and all its entry points to comply
!        with NAIF standard.
!
!        Cleaned up the $Revisions history.
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration and
!        reading of non-native files.
!
!-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
!
!        Added description of NWD, NWI, and NWC to the $Parameters
!        and $Brief_I/O sections of the header.
!
!-    SPICELIB Version 1.1.0, 17-NOV-1995 (NJB)
!
!        Made modifications to the DASRRx routines to enhance
!        efficiency. Removed references to the function RETURN.
!
!        Removed weird spaces from ENTRY statements.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header for each entry point.
!        This was done in order to minimize documentation changes if the
!        DAS open routines ever change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     read and write DAS physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration and
!        reading of non-native files.
!
!-    SPICELIB Version 1.1.0, 17-NOV-1995 (NJB)
!
!        Made modifications to the DASRRx routines to enhance
!        efficiency. Removed references to the function RETURN.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LNKTL

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL  =   -5 )

      INTEGER               FORWRD
      PARAMETER           ( FORWRD  =    1 )

      INTEGER               BUFNVD
      PARAMETER           ( BUFNVD  = BUFSZD * NWD )

      INTEGER               BUFNVI
      PARAMETER           ( BUFNVI  = BUFSZI * NWI )

!
!     Local variables
!

!
!     The data structure maintained by this set of routines consists
!     of three record buffers, one each for use with records of double
!     precision, integer, and character data types.
!
!     Each buffer consists of five parallel arrays; the arrays contain:
!
!        -- data records
!        -- Fortran record numbers
!        -- file handles
!        -- Update flags
!
!     In addition, for each buffer there is a doubly linked list that
!     points to the buffer and keeps track of the order in which the
!     records in the buffer were accessed.  The three linked lists are
!     maintained in a doubly linked list pool structure.  The logical
!     structure of each buffer is illustrated below.  All of the array
!     elements in the same row are associated with the data record in
!     that row.
!
!
!
!     Linked          Record       Record   Handles   Update
!      List           buffer       Numbers            Flags
!
!      +---+      +------------+    +---+    +---+    +---+
!      |   | ---> |            |    |   |    |   |    |   |
!      +---+      +------------+    +---+    +---+    +---+
!      |   | ---> |            |    |   |    |   |    |   |
!      +---+      +------------+    +---+    +---+    +---+
!        .              .             .        .        .
!        .              .             .        .        .
!        .              .             .        .        .
!      +---+      +------------+    +---+    +---+    +---+
!      |   | ---> |            |    |   |    |   |    |   |
!      +---+      +------------+    +---+    +---+    +---+
!
!
      CHARACTER*(NWC)       RCBUFC (      BUFSZC )
      DOUBLE PRECISION      RCBUFD ( NWD, BUFSZD )
      INTEGER               RCBUFI ( NWI, BUFSZI )

      INTEGER               RNBUFC (      BUFSZC )
      INTEGER               RNBUFD (      BUFSZD )
      INTEGER               RNBUFI (      BUFSZI )

      INTEGER               HNBUFC (      BUFSZC )
      INTEGER               HNBUFD (      BUFSZD )
      INTEGER               HNBUFI (      BUFSZI )

      LOGICAL               UPBUFC (      BUFSZC )
      LOGICAL               UPBUFD (      BUFSZD )
      LOGICAL               UPBUFI (      BUFSZI )

      INTEGER               POOLC  ( 2, LBPOOL : BUFSZC )
      INTEGER               POOLD  ( 2, LBPOOL : BUFSZD )
      INTEGER               POOLI  ( 2, LBPOOL : BUFSZI )

      INTEGER               HEADC
      INTEGER               HEADD
      INTEGER               HEADI

      INTEGER               USEDC
      INTEGER               USEDD
      INTEGER               USEDI

!
!     Other local variables
!
      INTEGER               NEXT
      INTEGER               NODE
      INTEGER               UNIT
      INTEGER               WRUNIT

      LOGICAL               PASS1

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  PASS1   / .TRUE.           /

      DATA                  USEDC   /  0               /
      DATA                  USEDD   /  0               /
      DATA                  USEDI   /  0               /

      DATA                  RCBUFC  / BUFSZC * ' '     /
      DATA                  RCBUFD  / BUFNVD * 0.D0    /
      DATA                  RCBUFI  / BUFNVI * 0       /

      DATA                  RNBUFC  / BUFSZC * 0       /
      DATA                  RNBUFD  / BUFSZD * 0       /
      DATA                  RNBUFI  / BUFSZI * 0       /

      DATA                  HNBUFC  / BUFSZC * 0       /
      DATA                  HNBUFD  / BUFSZD * 0       /
      DATA                  HNBUFI  / BUFSZI * 0       /

      DATA                  UPBUFC  / BUFSZC * .FALSE. /
      DATA                  UPBUFD  / BUFSZD * .FALSE. /
      DATA                  UPBUFI  / BUFSZI * .FALSE. /

      DATA                  HEADC   / 0                /
      DATA                  HEADD   / 0                /
      DATA                  HEADI   / 0                /

      DATA                  UNIT    / -1 /
      DATA                  WRUNIT  / -1 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASRWR' )
      END IF

!
!     Never come here.
!
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )


      CALL CHKOUT ( 'DASRWR' )
      RETURN





!$Procedure DASRRD ( DAS, read record, double precision )

      ENTRY DASRRD ( HANDLE, RECNO, FIRST, LAST, DATAD )

! $ Abstract
!
!     Read DAS double precision physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     INTEGER               FIRST
!     INTEGER               LAST
!     DOUBLE PRECISION      DATAD   ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     FIRST,
!     LAST       I   First and last indices of range within record.
!     DATAD      O   Double precision data read from record.
!     BUFSZD     P   Number of records in the DP record buffer.
!     NWD        P   Number of DP in a single DAS DP record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an open DAS file.
!
!     RECNO    is the number of a record in a DAS file.
!
!     FIRST,
!     LAST     are the first and last indices of a range of
!              double precision numbers to be read from the
!              indicated record. The record contains NWD
!              double precision numbers; these have indices
!              ranging from 1 to NWD.
!
! $ Detailed_Output
!
!     DATAD    is a double precision array containing the
!              elements FIRST through LAST of the specified
!              record. The record element FIRST is placed
!              in DATAD(1), the record element FIRST+1 is placed
!              in DATAD(2), and so on; the record element LAST is
!              placed in DATAD(LAST-FIRST+1).
!
! $ Parameters
!
!     NWD      is the number of DPs in a single DAS record
!              containing DPs.
!
!     BUFSZD   is the number of records in the double precision
!              record buffer.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine. The output argument
!         DATAD will not be modified.
!
!     2)  If a read operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         output argument DATAD will not be modified.
!
!     3)  If a write operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         output argument DATAD will not be modified. This routine may
!         write out updated, buffered records in order to make room in
!         the double precision buffer for a newly read record. Note that
!         the file written to may be different than the file designated
!         by HANDLE if multiple DAS files are open for writing.
!
!     4)  If FIRST or LAST is not in the range [1, NWD], the error
!         SPICE(INDEXOUTOFRANGE) is signaled. The output argument
!         DATAD will not be modified.
!
!     5)  If FIRST > LAST, this routine will return without modifying
!         the output argument DATAD.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to read from a DAS file that is open for
!     reading or for writing. Any buffered double precision record
!     can be read with this routine. In particular, records that have
!     been written to the DAS double precision record buffer but have
!     not yet been written out to the DAS file they're intended to go
!     to ARE visible to this routine.
!
!     This routine should be used to read only records that contain
!     double precision data.
!
! $ Examples
!
!     1)  Read the 10th through 100th d.p. numbers from record number 9
!         in a DAS file designated by HANDLE.
!
!             CALL DASRRD ( HANDLE, 9, 10, 100, DATAD )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration and
!        reading of non-native files.
!
!-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
!
!        Added description of NWD to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
!
!        Made modifications to enhance efficiency. Removed references
!        to the function RETURN.
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     read DAS double precision physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
!
!        Made modifications to enhance efficiency. Removed references
!        to the function RETURN. For buffered reads, MOVED is not
!        called when a single word is to be read.
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     Check FIRST and LAST.  Use discovery check-in.
!
      IF (        ( FIRST .LT. 1   ) &
            .OR.  ( FIRST .GT. NWD ) &
            .OR.  ( LAST  .LT. 1   ) &
            .OR.  ( LAST  .GT. NWD )    )  THEN

         CALL CHKIN  ( 'DASRRD' )
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    // &
                       'allowed range for both is [#, #]. File was '  // &
                       '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWD                                       )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASRRD' )
         RETURN

      END IF

!
!     There's nothing to do if LAST < FIRST.  (We're not checked in at
!     this point.)
!
      IF ( LAST .LT. FIRST ) THEN
         RETURN
      END IF

!
!     See whether record number RECNO in file HANDLE is buffered.  We'll
!     search through the list of buffered records starting at the head
!     of the list.  If we find the desired record, transfer the
!     requested data to the array DATAD and return without further ado.
!
      NODE = HEADD

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFD(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFD(NODE)  )    )  THEN
!
!           Found it.  Move this record to the head of the list.
!           Update our head pointer as required.
!
            IF ( NODE .NE. HEADD ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLD )
               CALL LNKILB ( NODE, HEADD, POOLD )

               HEADD  =  NODE

            END IF

!
!           Don't forget to return the requested data.
!
            IF ( FIRST .EQ. LAST ) THEN

               DATAD(1) = RCBUFD( FIRST, NODE )

            ELSE

               CALL MOVED ( RCBUFD(FIRST, NODE),  LAST-FIRST+1,  DATAD )

            END IF

!
!           We haven't checked in, so don't check out.
!
            RETURN

         END IF

         NODE = POOLD ( FORWRD, NODE )

      END DO

!
!     The record wasn't buffered.  We need to allocate entries to
!     hold the record contents.  If the buffer isn't full, just
!     select a free set of entries.  If the buffer is full, use
!     the set of entries at the tail of the list.
!
!     Since we're now going to do a file read, it doesn't slow
!     us down much to check in, comparatively speaking.
!
      CALL CHKIN ( 'DASRRD' )

      IF (  USEDD .EQ. BUFSZD ) THEN
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  =  LNKTL ( HEADD, POOLD )

         CALL LNKXSL ( NODE, NODE, POOLD )

!
!        If the allocated buffer entry was updated, write it out.
!
         IF (  UPBUFD(NODE)  ) THEN
!
!           We'll need a logical unit in order to write to the file.
!
            CALL ZZDDHHLU ( HNBUFD(NODE), 'DAS', .FALSE., WRUNIT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASRRD' )
               RETURN
            END IF

            CALL DASIOD ( 'WRITE', &
                           WRUNIT, &
                           RNBUFD(    NODE ), &
                           RCBUFD( 1, NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASRRD' )
               RETURN
            END IF

         END IF


      ELSE
!
!        Allocate a new set of buffer entries, but don't link
!        them into the list yet.
!
         CALL LNKAN  ( POOLD, NODE )
         USEDD = USEDD + 1

      END IF

!
!     Try to read the record.
!
      CALL ZZDASGRD ( HANDLE, RECNO, RCBUFD(1,NODE) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRRD' )
         RETURN
      END IF

!
!     The read was successful.  Link the node pointing to the buffer
!     entries for this record in before the current head of the
!     list, thus putting them at the head.
!
!     Set the file handle, record number, and update flag for
!     this record.
!
      CALL LNKILB ( NODE, HEADD, POOLD )

      HNBUFD ( NODE )  =  HANDLE
      RNBUFD ( NODE )  =  RECNO
      UPBUFD ( NODE )  =  .FALSE.
      HEADD            =  NODE

!
!     Don't forget to return the requested data.
!
      CALL MOVED (  RCBUFD ( FIRST,  NODE ), &
                    LAST  -  FIRST + 1, &
                    DATAD                     )


      CALL CHKOUT ( 'DASRRD' )
      RETURN






!$Procedure DASRRI ( DAS, read record, integer )

      ENTRY DASRRI ( HANDLE, RECNO, FIRST, LAST, DATAI )

! $ Abstract
!
!     Read DAS integer physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     INTEGER               FIRST
!     INTEGER               LAST
!     INTEGER               DATAI   ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     FIRST,
!     LAST       I   First and last indices of range within record.
!     DATAI      O   Integer data read from record.
!     BUFSZI     P   Number of records in the integer record buffer.
!     NWI        P   Number of integers in a single DAS integer record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an open DAS file.
!
!     RECNO    is the number of a record in a DAS file.
!
!     FIRST,
!     LAST     are the first and last indices of a range of
!              integers to be read from the indicated record.
!              The record contains NWI integers; these have
!              indices ranging from 1 to NWI.
!
! $ Detailed_Output
!
!     DATAI    is an integer array containing the elements FIRST
!              through LAST of the specified record. The record
!              element FIRST is placed in DATAI(1), the record
!              element FIRST+1 is placed in DATAI(2), and so on;
!              the record element LAST is placed in
!              DATAI(LAST-FIRST+1).
!
! $ Parameters
!
!     NWI      is the number of integers in a single DAS record
!              containing integers.
!
!     BUFSZI   is the number of records in the integer record
!              buffer.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled
!         by a routine in the call tree of this routine. The
!         output argument DATAI will not be modified.
!
!     2)  If a read operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine.
!         The output argument DATAI will not be modified.
!
!     3)  If a write operation attempted by this routine fails, an
!         error is signaled by a routine in the call tree of this
!         routine. The output argument DATAI will not be modified. This
!         routine may write out updated, buffered records in order to
!         make room in the integer buffer for a newly read record. Note
!         that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
!     4)  If FIRST or LAST is not in the range [1, NWI], the error
!         SPICE(INDEXOUTOFRANGE) is signaled. The output argument
!         DATAI will not be modified.
!
!     5)  If FIRST > LAST, this routine will return without modifying
!         the output argument DATAI.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to read from a DAS file that is open for
!     reading or writing. Any buffered integer record can be read with
!     this routine. In particular, records that have been written to
!     the DAS integer record buffer but have not yet been written out
!     to the DAS file they're intended to go to ARE visible to this
!     routine.
!
!     This routine should be used to read only records that contain
!     integer data.
!
! $ Examples
!
!     1)  Read the 10th through 100th integers from record number 9
!         in a DAS file designated by HANDLE.
!
!             CALL DASRRI ( HANDLE, 9, 10, 100, DATAI )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Updated the header to comply with NAIF standard. Cleaned up
!        the $Revisions history.
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration and
!        reading of non-native files.
!
!-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
!
!        Added description of NWI to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
!
!        Made modifications to enhance efficiency. Removed references
!        to the function RETURN.
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     read DAS integer physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration and
!        reading of non-native files.
!
!-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
!
!        Made modifications to enhance efficiency. Removed references
!        to the function RETURN. For buffered reads, MOVEI is not
!        called when a single word is to be read.
!
!-&


!
!     Non-standard SPICE error handling.
!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     Check FIRST and LAST.  Use discovery check-in.
!
      IF (        ( FIRST .LT. 1   ) &
            .OR.  ( FIRST .GT. NWI ) &
            .OR.  ( LAST  .LT. 1   ) &
            .OR.  ( LAST  .GT. NWI )    )  THEN

         CALL CHKIN  ( 'DASRRI'      )
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    // &
                       'allowed range for both is [#, #]. File was '  // &
                       '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWI                                       )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASRRI' )
         RETURN

      END IF

!
!     There's nothing to do if LAST < FIRST.  (We're not checked in at
!     this point.)
!
      IF ( LAST .LT. FIRST ) THEN
         RETURN
      END IF

!
!     See whether record number RECNO in file HANDLE is buffered.  We'll
!     search through the list of buffered records starting at the head
!     of the list.  If we find the desired record, transfer the
!     requested data to the array DATAI and return without further ado.
!
      NODE = HEADI

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFI(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFI(NODE)  )    )  THEN
!
!
!           Found it.  Move this record to the head of the list.
!           Update our head pointer as required.
!
            IF ( NODE .NE. HEADI ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLI )
               CALL LNKILB ( NODE, HEADI, POOLI )

               HEADI  =  NODE

            END IF

!
!           Don't forget to return the requested data.
!
            IF ( FIRST .EQ. LAST ) THEN

               DATAI(1) = RCBUFI( FIRST, NODE )

            ELSE

               CALL MOVEI ( RCBUFI(FIRST, NODE),  LAST-FIRST+1,  DATAI )

            END IF

!
!           We haven't checked in, so don't check out.
!
            RETURN

         END IF

         NODE = POOLI ( FORWRD, NODE )

      END DO

!
!     The record wasn't buffered.  We need to allocate entries to
!     hold the record contents.  If the buffer isn't full, just
!     select a free set of entries.  If the buffer is full, use
!     the set of entries at the tail of the list.
!
!     Since we're now going to do a file read, it doesn't slow
!     us down much to check in, comparatively speaking.
!
      CALL CHKIN ( 'DASRRI' )

      IF (  USEDI .EQ. BUFSZI ) THEN
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  =  LNKTL ( HEADI, POOLI )

         CALL LNKXSL ( NODE, NODE, POOLI )

!
!        If the allocated buffer entry was updated, write it out.
!
         IF (  UPBUFI(NODE)  ) THEN

            CALL ZZDDHHLU ( HNBUFI(NODE), 'DAS', .FALSE., WRUNIT )

            CALL DASIOI ( 'WRITE', &
                          WRUNIT, &
                          RNBUFI(    NODE ), &
                          RCBUFI( 1, NODE )   )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASRRI' )
               RETURN
            END IF

         END IF


      ELSE
!
!        Allocate a new set of buffer entries, but don't link
!        them into the list yet.
!
         CALL LNKAN ( POOLI, NODE )
         USEDI = USEDI + 1

      END IF

!
!     Try to read the record.
!
      CALL ZZDASGRI ( HANDLE,  RECNO,  RCBUFI(1,NODE)  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRRI' )
         RETURN
      END IF

!
!     The read was successful.  Link the node pointing to the buffer
!     entries for this record in before the current head of the
!     list, thus putting them at the head.
!
!     Set the file handle, record number, and update flag for
!     this record.
!
      CALL LNKILB ( NODE, HEADI, POOLI )

      HNBUFI ( NODE )  =  HANDLE
      RNBUFI ( NODE )  =  RECNO
      UPBUFI ( NODE )  =  .FALSE.
      HEADI            =  NODE

!
!     Don't forget to return the requested data.
!
      CALL MOVEI (  RCBUFI ( FIRST,  NODE ), &
                    LAST  -  FIRST + 1, &
                    DATAI                     )


      CALL CHKOUT ( 'DASRRI' )
      RETURN







!$Procedure DASRRC ( DAS, read record, character )

      ENTRY DASRRC ( HANDLE, RECNO, FIRST, LAST, DATAC )

! $ Abstract
!
!     Read DAS character physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     INTEGER               FIRST
!     INTEGER               LAST
!     CHARACTER*(*)         DATAC
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     FIRST,
!     LAST       I   First and last indices of range within record.
!     DATAC      O   Character data read from record.
!     BUFSZC     P   Number of records in the character record buffer.
!     NWC        P   Number of characters in a single DAS char. record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an open DAS file.
!
!     RECNO    is the number of a record in a DAS file.
!
!     FIRST,
!     LAST     are the first and last indices of a range of
!              characters to be read from the indicated record.
!              The record contains NWC characters; these have
!              indices ranging from 1 to NWC.
!
! $ Detailed_Output
!
!     DATAC    is a character string containing the elements
!              FIRST through LAST of the specified record. The
!              record element FIRST is placed in DATAC(1:1), the
!              record element FIRST+1 is placed in DATAC(2:2),
!              and so on; the record element LAST is placed in
!              DATAC( LAST-FIRST+1 : LAST-FIRST+1 ).
!
! $ Parameters
!
!     NWC      is the number of characters in a single DAS record
!              containing characters.
!
!     BUFSZC   is the number of records in the character record
!              buffer.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is
!         signaled by a routine in the call tree of this routine. The
!         output argument DATAC will not be modified.
!
!     2)  If a read operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine.
!         The output argument DATAC will not be modified.
!
!     3)  If a write operation attempted by this routine fails, an
!         error is signaled by a routine in the call tree of this
!         routine. The output argument DATAC will not be modified. This
!         routine may write out updated, buffered records in order to
!         make room in the character buffer for a newly read record.
!         Note that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
!     4)  If FIRST or LAST is not in the range [1, NWC], the error
!         SPICE(INDEXOUTOFRANGE) is signaled. The output argument
!         DATAC will not be modified.
!
!     5)  If FIRST > LAST, this routine will return without modifying
!         the output argument DATAC.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to read from a DAS file that is open for
!     reading or writing. Any buffered character record can be read
!     with this routine. In particular, records that have been
!     written to the DAS character record buffer but have not yet been
!     written out to the DAS file they're intended to go to ARE
!     visible to this routine.
!
!     This routine should be used to read only records that contain
!     character data.
!
! $ Examples
!
!     1)  Read the 10th through 100th characters from record number 9
!         in a DAS file designated by HANDLE.
!
!             CALL DASRRC ( HANDLE, 9, 10, 100, DATAC )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Updated the header to comply with NAIF standard. Cleaned up
!        the $Revisions history.
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
!
!        Added description of NWC to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.1.0, 09-NOV-1995 (NJB)
!
!        Made modifications to enhance efficiency. Removed references
!        to the function RETURN.
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     read DAS character physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.1.0, 09-NOV-1995 (NJB)
!
!        Made modifications to enhance efficiency. Removed references
!        to the function RETURN.
!
!-&


!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     Check FIRST and LAST.  Use discovery check-in.
!
      IF (        ( FIRST .LT. 1   ) &
            .OR.  ( FIRST .GT. NWC ) &
            .OR.  ( LAST  .LT. 1   ) &
            .OR.  ( LAST  .GT. NWC )    )  THEN

         CALL CHKIN  ( 'DASRRC' )
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    // &
                       'allowed range for both is [#, #]. File was '  // &
                       '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWC                                       )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASRRC' )
         RETURN

      END IF

!
!     There's nothing to do if LAST < FIRST.  (We're not checked in at
!     this point.)
!
      IF ( LAST .LT. FIRST ) THEN
         RETURN
      END IF

!
!     See whether record number RECNO in file HANDLE is buffered.  We'll
!     search through the list of buffered records starting at the head
!     of the list.  If we find the desired record, transfer the
!     requested data to the array DATAC and return without further ado.
!
      NODE = HEADC

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFC(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFC(NODE)  )    )  THEN
!
!
!           Found it.  Move this record to the head of the list.
!           Update our head pointer as required.
!
            IF ( NODE .NE. HEADC ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLC )
               CALL LNKILB ( NODE, HEADC, POOLC )

               HEADC  =  NODE

            END IF

!
!           Don't forget to return the requested data.
!
            DATAC  =  RCBUFC( NODE ) ( FIRST : LAST )

!
!           We haven't checked in, so don't check out.
!
            RETURN

         END IF

         NODE = POOLC ( FORWRD, NODE )

      END DO

!
!     The record wasn't buffered.  We need to allocate entries to
!     hold the record contents.  If the buffer isn't full, just
!     select a free set of entries.  If the buffer is full, use
!     the set of entries at the tail of the list.
!
!     Since we're now going to do a file read, it doesn't slow
!     us down much to check in, comparatively speaking.
!
      CALL CHKIN ( 'DASRRC' )

      IF (  USEDC .EQ. BUFSZC ) THEN
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  =  LNKTL ( HEADC, POOLC )

         CALL LNKXSL ( NODE, NODE, POOLC )

!
!        If the allocated buffer entry was updated, write it out.
!
         IF (  UPBUFC(NODE)  ) THEN

            CALL ZZDDHHLU ( HNBUFC( NODE ), 'DAS', .FALSE., WRUNIT )

            CALL DASIOC (  'WRITE', &
                            WRUNIT, &
                            RNBUFC( NODE ), &
                            RCBUFC( NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASRRC' )
               RETURN
            END IF

         END IF


      ELSE
!
!        Allocate a new set of buffer entries, but don't link
!        them into the list yet.
!
         CALL LNKAN ( POOLC, NODE )
         USEDC = USEDC + 1

      END IF

!
!     Try to read the record.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

      CALL DASIOC ( 'READ',  UNIT,  RECNO, RCBUFC(NODE) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRRC' )
         RETURN
      END IF

!
!     The read was successful.  Link the node pointing to the buffer
!     entries for this record in before the current head of the
!     list, thus putting them at the head.
!
!     Set the file handle, record number, and update flag for
!     this record.
!
      CALL LNKILB ( NODE, HEADC, POOLC )

      HNBUFC ( NODE )  =  HANDLE
      RNBUFC ( NODE )  =  RECNO
      UPBUFC ( NODE )  =  .FALSE.
      HEADC            =  NODE

!
!     Don't forget to return the requested data.
!
      DATAC  =  RCBUFC( NODE ) ( FIRST : LAST )

      CALL CHKOUT ( 'DASRRC' )
      RETURN






!$Procedure DASWRD ( DAS, write record, double precision )

      ENTRY DASWRD ( HANDLE, RECNO, RECD )

! $ Abstract
!
!     Write DAS double precision physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     DOUBLE PRECISION      RECD   ( NWD )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     RECD       I   Double precision data to be written to record.
!     BUFSZD     P   Number of records in the DP record buffer.
!     NWD        P   Number of DP in a single DAS DP record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAS file opened for writing.
!
!     RECNO    is the number of a record in a DAS file.
!
!     RECD     is an array of NWD double precision numbers. The
!              contents of this array are to be written to the
!              physical file record having number RECNO.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     NWD      is the number of DPs in a single DAS record
!              containing DPs.
!
!     BUFSZD   is the number of records in the double precision
!              record buffer.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine. The DAS file
!         designated by HANDLE will not be modified.
!
!     2)  If a write operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         status of the DAS file written to is uncertain in this case.
!         Note that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to write to only DAS files that are open
!     for writing. Records written via this routine will always be
!     buffered immediately, but may not be written to the file until
!     they are cleared from the double precision buffer to make room
!     for other records, or until they are explicitly forced to to be
!     written via a call to DASWBR. In any case, at the moment this
!     routine returns, the data supplied on input may be read back by
!     DASRRD or updated by DASURD.
!
!     Closing a DAS file via DASCLS forces any remaining updated data
!     records buffered by this routine to be written to the file.
!
! $ Examples
!
!     1)  Write an array of NWD double precision numbers to the 9th
!         record in a DAS file designated by HANDLE.
!
!            DOUBLE PRECISION        RECD
!
!                         .
!                         .
!                         .
!
!            DO I = 1, NWD
!               RECD(I) = DBLE(I)
!            END DO
!
!            CALL DASWRD ( HANDLE, 9, RECD )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
!
!        Added description of NWD to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     write DAS double precision physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASWRD' )

!
!     Check that the file is open for writing.  Signal an error if not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWRD' )
         RETURN
      END IF

!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     See whether double precision record number RECNO from file HANDLE
!     is buffered.  We'll search through the list of buffered records
!     starting at the head of the list.  If the record is already
!     buffered, we'll update the buffer entry, but we'll defer writing
!     the record out until we need to free a record, or until the
!     d.p. buffer is flushed, whichever comes first.
!
      NODE = HEADD

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFD(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFD(NODE)  )    )  THEN
!
!           Found it.  Update the buffered record.
!
            CALL MOVED (  RECD,  NWD,  RCBUFD(1, NODE)  )

!
!           Set the update flag, indicating that this buffer entry
!           has been modified.
!
            UPBUFD ( NODE ) = .TRUE.

!
!           Put the information about this record at the head of the
!           active list, if it is not already there.
!
            IF ( NODE .NE. HEADD ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLD )
               CALL LNKILB ( NODE, HEADD, POOLD )
               HEADD = NODE

            END IF

            CALL CHKOUT ( 'DASWRD' )
            RETURN

         END IF

         NODE = POOLD ( FORWRD, NODE )

      END DO

!
!     The record we're writing to is not buffered.  We'll allocate
!     a buffer entry.  If the record buffer is full, we'll
!     commandeer the least recently accessed record.  Before using
!     this record, we'll write its contents out to the corresponding
!     file, if the record has been updated.
!
      IF ( USEDD .LT. BUFSZD ) THEN
!
!        There's a free buffer entry available.  Just allocate it.
!
         CALL LNKAN ( POOLD, NODE )

         USEDD = USEDD + 1

      ELSE
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  =  LNKTL ( HEADD, POOLD )

         CALL LNKXSL ( NODE, NODE, POOLD )

!
!        If the allocated record was updated, write it out.
!
         IF (  UPBUFD(NODE)  ) THEN

            CALL ZZDDHHLU ( HNBUFD( NODE ), 'DAS', .FALSE., WRUNIT )

            CALL DASIOD (  'WRITE', &
                            WRUNIT, &
                            RNBUFD(    NODE ), &
                            RCBUFD( 1, NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWRD' )
               RETURN
            END IF

         END IF

      END IF

!
!     Now update the allocated buffer entry with the input data.
!
      CALL MOVED (  RECD,  NWD,  RCBUFD( 1, NODE )  )

!
!     Set the update flag, indicating that this buffer entry
!     has been modified. Also set the handle and record number
!     entries.
!
      UPBUFD ( NODE ) = .TRUE.
      HNBUFD ( NODE ) =  HANDLE
      RNBUFD ( NODE ) =  RECNO

!
!     Link this buffer entry to the head of the list.
!
      CALL LNKILB ( NODE, HEADD, POOLD )

      HEADD = NODE

      CALL CHKOUT ( 'DASWRD' )
      RETURN






!$Procedure DASWRI ( DAS, write record, integer )

      ENTRY DASWRI ( HANDLE, RECNO, RECI )

! $ Abstract
!
!     Write DAS integer physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     INTEGER               RECI   ( NWI )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     RECI       I   Integer data to be written to record.
!     BUFSZI     P   Number of records in the integer record buffer.
!     NWI        P   Number of integers in a single DAS integer record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAS file opened for writing.
!
!     RECNO    is the number of a record in a DAS file.
!
!     RECI     is an array of NWI integers. The contents of this
!              array are to be written to the physical file
!              record having number RECNO.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     NWI      is the number of integers in a single DAS record
!              containing integers.
!
!     BUFSZI   is the number of records in the integer record
!              buffer.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine. The DAS file
!         designated by HANDLE will not be modified.
!
!     2)  If a write operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         status of the DAS file written to is uncertain in this case.
!         Note that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to write to only DAS files that are open
!     for writing. Records written via this routine will always be
!     buffered immediately, but may not be written to the file until
!     they are cleared from the integer buffer to make room for other
!     records, or until they are explicitly forced to to be written via
!     a call to DASWBR. In any case, at the moment this routine
!     returns, the data supplied on input may be read back by DASRRI
!     or updated by DASURI.
!
!     Closing a DAS file via DASCLS forces any remaining updated data
!     records buffered by this routine to be written to the file.
!
! $ Examples
!
!     1)  Write an array of NWI integers to the 9th record in a DAS
!         file designated by HANDLE.
!
!            INTEGER                RECI ( NWI )
!                         .
!                         .
!                         .
!
!            DO I = 1, NWI
!               RECI(I) = I
!            END DO
!
!            CALL DASWRI ( HANDLE, 9, RECI )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
!
!        Added description of NWI to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     write DAS integer physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASWRI' )

!
!     Check that the file is open for writing.  Signal an error if not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWRI' )
         RETURN
      END IF

!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     See whether integer record number RECNO from file HANDLE is
!     buffered.  We'll search through the list of buffered records
!     starting at the head of the list.  If the record is already
!     buffered, we'll update the buffer entry, but we'll defer writing
!     the record out until we need to free a record, or until the
!     integer buffer is flushed, whichever comes first.
!
      NODE = HEADI

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFI(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFI(NODE)  )    )  THEN
!
!           Found it.  Update the buffered record.
!
            CALL MOVEI (  RECI,  NWI,  RCBUFI( 1, NODE )  )

!
!           Set the update flag, indicating that this buffer entry
!           has been modified.
!
            UPBUFI ( NODE ) = .TRUE.

!
!           Put the information about this record at the head of the
!           active list, if it is not already there.
!
            IF ( NODE .NE. HEADI ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLI )
               CALL LNKILB ( NODE, HEADI, POOLI )
               HEADI = NODE

            END IF

            CALL CHKOUT ( 'DASWRI' )
            RETURN

         END IF

         NODE = POOLI ( FORWRD, NODE )

      END DO

!
!     The record we're writing to is not buffered.  We'll allocate
!     a buffer entry.  If the record buffer is full, we'll
!     commandeer the least recently accessed record.  Before using
!     this record, we'll write its contents out to the corresponding
!     file, if the record has been updated.
!
      IF ( USEDI .LT. BUFSZI ) THEN
!
!        There's a free buffer entry available.  Just allocate it.
!
         CALL LNKAN ( POOLI, NODE )
         USEDI = USEDI + 1

      ELSE
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  =  LNKTL ( HEADI, POOLI )

         CALL LNKXSL ( NODE, NODE, POOLI )

!
!        If the allocated record was updated, write it out.
!
         IF (  UPBUFI(NODE)  ) THEN

            CALL ZZDDHHLU ( HNBUFI( NODE ), 'DAS', .FALSE., WRUNIT )

            CALL DASIOI (  'WRITE', &
                            WRUNIT, &
                            RNBUFI(    NODE ), &
                            RCBUFI( 1, NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWRI' )
               RETURN
            END IF

         END IF

      END IF

!
!     Now update the allocated buffer entry with the input data.
!
      CALL MOVEI (  RECI,  NWI,  RCBUFI( 1, NODE )  )

!
!     Set the update flag, indicating that this buffer entry
!     has been modified.  Also set the handle and record number
!     entries.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

      UPBUFI ( NODE ) = .TRUE.
      HNBUFI ( NODE ) =  HANDLE
      RNBUFI ( NODE ) =  RECNO

!
!     Link this buffer entry to the head of the list.
!
      CALL LNKILB ( NODE, HEADI, POOLI )

      HEADI = NODE

      CALL CHKOUT ( 'DASWRI' )
      RETURN






!$Procedure DASWRC ( DAS, write record, character )

      ENTRY DASWRC ( HANDLE, RECNO, RECC )

! $ Abstract
!
!     Write DAS character physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     CHARACTER*(*)         RECC
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     RECC       I   Character data to be written to record.
!     BUFSZC     P   Number of records in the character record buffer.
!     NWC        P   Number of characters in a single DAS char. record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAS file opened for writing.
!
!     RECNO    is the number of a record in a DAS file.
!
!     RECC     is a string of length NWC. The contents of this
!              string are to be written to the physical file
!              record having number RECNO.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     NWC      is the number of characters in a single DAS record
!              containing characters.
!
!     BUFSZC   is the number of records in the character record
!              buffer.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine. The DAS file
!         designated by HANDLE will not be modified.
!
!     2)  If a write operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         status of the DAS file written to is uncertain in this case.
!         Note that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to write to only DAS files that are open
!     for writing. Records written via this routine will always be
!     buffered immediately, but may not be written to the file until
!     they are cleared from the character buffer to make room for other
!     records, or until they are explicitly forced to to be written via
!     a call to DASWBR. In any case, at the moment this routine
!     returns, the data supplied on input may be read back by DASRRC
!     or updated by DASURC.
!
!     Closing a DAS file via DASCLS forces any remaining updated data
!     records buffered by this routine to be written to the file.
!
! $ Examples
!
!     1)  Write a string of NWC characters to the 9th record in a DAS
!         file designated by HANDLE.
!
!            CHARACTER*(NWC)           RECC
!
!                         .
!                         .
!                         .
!
!            RECC = 'This example string is blank-padded on the '    //
!           .       'right.  All of the trailing blanks will be '    //
!           .       'written to the DAS file by the following call.'
!
!            CALL DASWRC ( HANDLE, 9, RECC )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
!
!        Added description of NWC to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     write DAS character physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASWRC' )

!
!     Check that the file is open for writing.  Signal an error if not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWRC' )
         RETURN
      END IF

!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     See whether character record number RECNO from file HANDLE is
!     buffered.  We'll search through the list of buffered records
!     starting at the head of the list.  If the record is already
!     buffered, we'll update the buffer entry, but we'll defer writing
!     the record out until we need to free a record, or until the
!     character buffer is flushed, whichever comes first.
!
      NODE = HEADC

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFC(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFC(NODE)  )    )  THEN
!
!           Found it.  Update the buffered record.
!
            RCBUFC (NODE)  =  RECC

!
!           Set the update flag, indicating that this buffer entry
!           has been modified.
!
            UPBUFC ( NODE ) = .TRUE.

!
!           Put the information about this record at the head of the
!           active list, if it is not already there.
!
            IF ( NODE .NE. HEADC ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLC )
               CALL LNKILB ( NODE, HEADC, POOLC )
               HEADC = NODE

            END IF

            CALL CHKOUT ( 'DASWRC' )
            RETURN

         END IF

         NODE = POOLC ( FORWRD, NODE )

      END DO

!
!     The record we're writing to is not buffered.  We'll allocate
!     a buffer entry.  If the record buffer is full, we'll
!     commandeer the least recently accessed record.  Before using
!     this record, we'll write its contents out to the corresponding
!     file, if the record has been updated.
!
      IF ( USEDC .LT. BUFSZC ) THEN
!
!        There's a free buffer entry available.  Just allocate it.
!
         CALL LNKAN ( POOLC, NODE )
         USEDC = USEDC + 1

      ELSE
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  = LNKTL ( HEADC, POOLC )

         CALL LNKXSL ( NODE, NODE, POOLC )

!
!        If the allocated record was updated, write it out.
!
         IF (  UPBUFC(NODE)  ) THEN

            CALL ZZDDHHLU ( HNBUFC( NODE ), 'DAS', .FALSE., WRUNIT )

            CALL DASIOC ( 'WRITE', &
                           WRUNIT, &
                           RNBUFC( NODE ), &
                           RCBUFC( NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWRC' )
               RETURN
            END IF

         END IF

      END IF

!
!     Now update the allocated buffer entry with the input data.
!
      RCBUFC(NODE)  =  RECC

!
!     Set the update flag, indicating that this buffer entry
!     has been modified.  Also set the handle and record number
!     entries.
!
      UPBUFC ( NODE ) = .TRUE.
      HNBUFC ( NODE ) =  HANDLE
      RNBUFC ( NODE ) =  RECNO

!
!     Link this buffer entry to the head of the list.
!
      CALL LNKILB ( NODE, HEADC, POOLC )

      HEADC = NODE

      CALL CHKOUT ( 'DASWRC' )
      RETURN






!$Procedure DASURD ( DAS, update record, double precision )

      ENTRY DASURD ( HANDLE, RECNO, FIRST, LAST, DATAD )

! $ Abstract
!
!     Update DAS double precision physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     INTEGER               FIRST
!     INTEGER               LAST
!     DOUBLE PRECISION      DATAD  ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     FIRST,
!     LAST       I   First and last indices of range within record.
!     DATAD      I   Double precision data to write to record.
!     BUFSZD     P   Number of records in the DP record buffer.
!     NWD        P   Number of DPs in a single DAS DP record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAS file opened for writing.
!
!     RECNO    is the number of a record in a DAS file.
!
!     FIRST,
!     LAST     are the first and last indices of a range of
!              elements to be updated in the indicated record.
!              The record contains NWD double precision numbers;
!              these have indices ranging from 1 to NWD.
!
!     DATAD    is a double precision array to be written to
!              elements FIRST through LAST of the specified
!              record. The array element DATAD(1) is placed in
!              record element FIRST, the array element DATAD(2)
!              is placed in record element FIRST+1, and so on;
!              the array element DATAD(LAST-FIRST+1) is placed in
!              the record element LAST.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     NWD      is the number of DPs in a single DAS record
!              containing DPs.
!
!     BUFSZD   is the number of records in the double precision
!              record buffer.
!
! $ Exceptions
!
!     1)  This routine may be used to update only records that have
!         already been written by DASWRD or that already exist in the
!         file designated by HANDLE. Attempting to update a record
!         that hasn't yet been written will cause the read operation
!         performed by this routine to fail.
!
!         If a read operation attempted by this routine fails for this
!         or any other reason, an error is signaled by a routine in the
!         call tree of this routine. The indicated record will not be
!         modified.
!
!     2)  If a write operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         status of the DAS file written to is uncertain in this case.
!         Note that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
!     3)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine. The indicated record
!         will not be modified.
!
!     4)  If FIRST or LAST is not in the range [1, NWD], the error
!         SPICE(INDEXOUTOFRANGE) is signaled. The indicated
!         record will not be modified.
!
!     5)  If FIRST > LAST, this routine will return without modifying
!         the indicated record.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to update any existing record in a DAS
!     file that is open for writing, or any record that has been
!     `written' by DASWRD, whether or not that record has yet been
!     physically written to the file it belongs to. Records that have
!     never been written cannot be updated.
!
!     Because the DAS system buffers records that are written, multiple
!     updates of parts of a record can be made without incurring a
!     large number of file reads and writes.
!
!     This routine should be used to update only records that contain
!     double precision data.
!
! $ Examples
!
!     1)  Update the 10th through 100th d.p. numbers in record number 9
!         in a DAS file designated by HANDLE.
!
!             DOUBLE PRECISION      DATAD ( 100 )
!
!                         .
!                         .
!                         .
!
!             DO I = 1, 91
!                DATAD  =  DBLE(I)
!             END DO
!
!             CALL DASURD ( HANDLE, 9, 10, 100, DATAD )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
!
!        Added description of NWD to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     update DAS double precision physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASURD' )

!
!     Check that the file is open for writing.  Signal an error if not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURD' )
         RETURN
      END IF

!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     If FIRST or LAST are out of range, no dice.
!
      IF (        ( FIRST .LT. 1   ) &
            .OR.  ( FIRST .GT. NWD ) &
            .OR.  ( LAST  .LT. 1   ) &
            .OR.  ( LAST  .GT. NWD )    )  THEN

         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    // &
                       'allowed range for both is [#, #]. File was '  // &
                       '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWD                                       )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASURD' )
         RETURN

      END IF

!
!     There's nothing to do if LAST < FIRST.
!
      IF ( LAST .LT. FIRST ) THEN
         CALL CHKOUT ( 'DASURD' )
         RETURN
      END IF

!
!     See whether double precision record number RECNO from file HANDLE
!     is buffered.  We'll search through the list of buffered records
!     starting at the head of the list.  If the record is already
!     buffered, we'll update the buffer entry, but we'll defer writing
!     the record out until we need to free a record, or until the
!     d.p. buffer is flushed, whichever comes first.
!
      NODE = HEADD

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFD(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFD(NODE)  )    )  THEN
!
!           Found it.  Update the buffered record.
!
            CALL MOVED ( DATAD,   LAST-FIRST+1,   RCBUFD(FIRST, NODE)  )

!
!           Set the update flag, indicating that this buffer entry
!           has been modified.
!
            UPBUFD ( NODE ) = .TRUE.

!
!           Put the information about this record at the head of the
!           active list, if it is not already there.
!
            IF ( NODE .NE. HEADD ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLD )
               CALL LNKILB ( NODE, HEADD, POOLD )
               HEADD = NODE

            END IF

            CALL CHKOUT ( 'DASURD' )
            RETURN

         END IF

         NODE  =  POOLD ( FORWRD, NODE )

      END DO

!
!     The record we're writing to is not buffered.  In order to
!     update this record, we'll need to read it first.  But before
!     we do that, we'll need to allocate a buffer entry.  If the record
!     buffer is full, we'll commandeer the least recently accessed
!     record.  Before using this record, we'll write its contents out
!     to the corresponding file, if the record has been updated.
!
      IF ( USEDD .LT. BUFSZD ) THEN
!
!        There's a free buffer entry available.  Just allocate it.
!
         CALL LNKAN ( POOLD, NODE )

         USEDD = USEDD + 1

      ELSE
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  =  LNKTL ( HEADD, POOLD )

         CALL LNKXSL ( NODE, NODE, POOLD )

!
!        If the allocated record was updated, write it out.
!
         IF (  UPBUFD(NODE)  ) THEN

            CALL ZZDDHHLU ( HNBUFD( NODE ), 'DAS', .FALSE., WRUNIT )

            CALL DASIOD ( 'WRITE', &
                           WRUNIT, &
                           RNBUFD(    NODE ), &
                           RCBUFD( 1, NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASURD' )
               RETURN
            END IF

         END IF

      END IF


!
!     Now try to read the record we're going to update.
!
      CALL ZZDASGRD ( HANDLE, RECNO, RCBUFD(1,NODE) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURD' )
         RETURN
      END IF

!
!     The read was successful, so set the record number, handle,
!     and update flag for this buffer entry, and link these buffer
!     entries in before the current head of the list, thus putting
!     them at the head.
!
!     Update the head pointer.
!
      CALL LNKILB ( NODE, HEADD, POOLD )

      HNBUFD ( NODE )  =  HANDLE
      RNBUFD ( NODE )  =  RECNO
      UPBUFD ( NODE )  = .TRUE.
      HEADD            =  NODE

!
!     At long last, make the requested update.  Note that we don't
!     have to write the record back to the file; that will get done
!     automatically before or at the time the file is closed.
!
      CALL MOVED (  DATAD,  LAST-FIRST+1,  RCBUFD( FIRST, NODE )  )


      CALL CHKOUT ( 'DASURD' )
      RETURN







!$Procedure DASURI ( DAS, update record, integer )

      ENTRY DASURI ( HANDLE, RECNO, FIRST, LAST, DATAI )

! $ Abstract
!
!     Update DAS integer physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     INTEGER               FIRST
!     INTEGER               LAST
!     INTEGER               DATAI  ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     FIRST,
!     LAST       I   First and last indices of range within record.
!     DATAI      I   Integer data to write to record.
!     BUFSZI     P   Number of records in the integer record buffer.
!     NWI        P   Number of integers in a single DAS integer record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAS file opened for writing.
!
!     RECNO    is the number of a record in a DAS file.
!
!     FIRST,
!     LAST     are the first and last indices of a range of
!              elements to be updated in the indicated record.
!              The record contains NWI integers; these have
!              indices ranging from 1 to NWI.
!
!     DATAI    is an integer array to be written to elements FIRST
!              through LAST of the specified record. The array
!              element DATAI(1) is placed in record element FIRST,
!              the array element DATAI(2) is placed in record
!              element FIRST+1, and so on; the array element
!              DATAI(LAST-FIRST+1) is placed in the record element
!              LAST.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     NWI      is the number of integers in a single DAS record
!              containing integers.
!
!     BUFSZI   is the number of records in the integer record
!              buffer.
!
! $ Exceptions
!
!     1)  This routine may be used to update only records that have
!         already been written by DASWRI or that already exist in the
!         file designated by HANDLE. Attempting to update a record
!         that hasn't yet been written will cause the read operation
!         performed by this routine to fail.
!
!         If a read operation attempted by this routine fails for this
!         or any other reason, an error is signaled by a routine in the
!         call tree of this routine. The indicated record will not be
!         modified.
!
!     2)  If a write operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         status of the DAS file written to is uncertain in this case.
!         Note that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
!     3)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine. The indicated record
!         will not be modified.
!
!     4)  If FIRST or LAST is not in the range [1, NWI], the error
!         SPICE(INDEXOUTOFRANGE) is signaled. The indicated
!         record will not be modified.
!
!     5)  If FIRST > LAST, this routine will return without modifying
!         the indicated record.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to update any existing record in a DAS
!     file that is open for writing, or any record that has been
!     `written' by DASWRI, whether or not that record has yet been
!     physically written to the file it belongs to. Records that have
!     never been written cannot be updated.
!
!     Because the DAS system buffers records that are written, multiple
!     updates of parts of a record can be made without incurring a
!     large number of file reads and writes.
!
!     This routine should be used to update only records that contain
!     integer data.
!
! $ Examples
!
!     1)  Update the 10th through 100th integers in record number 9
!         in a DAS file designated by HANDLE.
!
!             INTEGER               DATAI ( 100 )
!
!                         .
!                         .
!                         .
!
!             DO I = 1, 91
!                DATAI  =  I
!             END DO
!
!             CALL DASURI ( HANDLE, 9, 10, 100, DATAI )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
!
!        Added description of NWI to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     update DAS integer physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASURI' )

!
!     Check that the file is open for writing.  Signal an error if not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURI' )
         RETURN
      END IF

!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     If FIRST or LAST are out of range, no dice.
!
      IF (        ( FIRST .LT. 1   ) &
            .OR.  ( FIRST .GT. NWI ) &
            .OR.  ( LAST  .LT. 1   ) &
            .OR.  ( LAST  .GT. NWI )    )  THEN

         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    // &
                       'allowed range for both is [#, #]. File was '  // &
                       '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWI                                       )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASURI' )
         RETURN

      END IF

!
!     There's nothing to do if LAST < FIRST.
!
      IF ( LAST .LT. FIRST ) THEN
         CALL CHKOUT ( 'DASURI' )
         RETURN
      END IF

!
!     See whether integer record number RECNO from file HANDLE is
!     buffered.  We'll search through the list of buffered records
!     starting at the head of the list.  If the record is already
!     buffered, we'll update the buffer entry, but we'll defer writing
!     the record out until we need to free a record, or until the
!     integer buffer is flushed, whichever comes first.
!
      NODE = HEADI

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFI(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFI(NODE)  )    )  THEN
!
!           Found it.  Update the buffered record.
!
            CALL MOVEI ( DATAI,   LAST-FIRST+1,   RCBUFI(FIRST, NODE) )

!
!           Set the update flag, indicating that this buffer entry
!           has been modified.
!
            UPBUFI ( NODE ) = .TRUE.

!
!           Put the information about this record at the head of the
!           active list, if it is not already there.
!
            IF ( NODE .NE. HEADI ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLI )
               CALL LNKILB ( NODE, HEADI, POOLI )
               HEADI = NODE

            END IF

            CALL CHKOUT ( 'DASURI' )
            RETURN

         END IF

         NODE  =  POOLI ( FORWRD, NODE )

      END DO

!
!     The record we're writing to is not buffered.  We'll allocate
!     a buffer entry.  If the record buffer is full, we'll
!     commandeer the least recently accessed record.  Before using
!     this record, we'll write its contents out to the corresponding
!     file, if the record has been updated.
!
      IF ( USEDI .LT. BUFSZI ) THEN
!
!        There's a free buffer entry available.  Just allocate it.
!
         CALL LNKAN ( POOLI, NODE )
         USEDI = USEDI + 1

      ELSE
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  =  LNKTL ( HEADI, POOLI )

         CALL LNKXSL ( NODE, NODE, POOLI )

!
!        If the allocated record was updated, write it out.
!
         IF (  UPBUFI(NODE)  ) THEN

            CALL ZZDDHHLU ( HNBUFI( NODE ), 'DAS', .FALSE., WRUNIT )

            CALL DASIOI ( 'WRITE', &
                           WRUNIT, &
                           RNBUFI(    NODE ), &
                           RCBUFI( 1, NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASURI' )
               RETURN
            END IF

         END IF

      END IF

!
!     Now try to read the record we're going to update.
!
      CALL ZZDASGRI ( HANDLE,   RECNO, RCBUFI(1,NODE) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURI' )
         RETURN
      END IF

!
!     The read was successful, so set the record number, handle,
!     and update flag for this buffer entry, and link these buffer
!     entries in before the current head of the list, thus putting
!     them at the head.
!
!     Update the head pointer.
!
      CALL LNKILB ( NODE, HEADI, POOLI )

      HNBUFI ( NODE )  =  HANDLE
      RNBUFI ( NODE )  =  RECNO
      UPBUFI ( NODE )  = .TRUE.
      HEADI            =  NODE

!
!     At long last, make the requested update.  Note that we don't
!     have to write the record back to the file; that will get done
!     automatically before or at the time the file is closed.
!
      CALL MOVEI (  DATAI,  LAST-FIRST+1,  RCBUFI( FIRST, NODE )  )


      CALL CHKOUT ( 'DASURI' )
      RETURN







!$Procedure DASURC ( DAS, update record, character )

      ENTRY DASURC ( HANDLE, RECNO, FIRST, LAST, DATAC )

! $ Abstract
!
!     Update DAS character physical records.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               RECNO
!     INTEGER               FIRST
!     INTEGER               LAST
!     CHARACTER*(*)         DATAC
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     FIRST,
!     LAST       I   First and last indices of range within record.
!     DATAC      I   Character data to write to record.
!     BUFSZC     P   Number of records in the character record buffer.
!     NWC        P   Number of characters in a single DAS char. record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAS file opened for writing.
!
!     RECNO    is the number of a record in a DAS file.
!
!     FIRST,
!     LAST     are the first and last indices of a range of
!              elements to be updated in the indicated record.
!              The record contains NWC characters; these have
!              indices ranging from 1 to NWC.
!
!     DATAC    is a character string to be written to elements
!              FIRST through LAST of the specified record. The
!              character DATAC(1:1) is placed in record element
!              FIRST, the character DATAC(2) is placed in record
!              element FIRST+1, and so on; the character
!              DATAC(LAST-FIRST+1) is placed in the record element
!              LAST.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     NWC      is the number of characters in a single DAS record
!              containing characters.
!
!     BUFSZC   is the number of records in the character record
!              buffer.
!
! $ Exceptions
!
!     1)  This routine may be used to update only records that have
!         already been written by DASWRC or that already exist in the
!         file designated by HANDLE. Attempting to update a record
!         that hasn't yet been written will cause the read operation
!         performed by this routine to fail.
!
!         If a read operation attempted by this routine fails for this
!         or any other reason, an error is signaled by a routine in the
!         call tree of this routine. The indicated record will not be
!         modified.
!
!     2)  If a write operation attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         status of the DAS file written to is uncertain in this case.
!         Note that the file written to may be different than the file
!         designated by HANDLE if multiple DAS files are open for
!         writing.
!
!     3)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine. The indicated record
!         will not be modified.
!
!     4)  If FIRST or LAST is not in the range [1, NWC], the error
!         SPICE(INDEXOUTOFRANGE) is signaled. The indicated
!         record will not be modified.
!
!     5)  If FIRST > LAST, this routine will return without modifying
!         the indicated record.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine can be used to update any existing record in a DAS
!     file that is open for writing, or any record that has been
!     `written' by DASWRC, whether or not that record has yet been
!     physically written to the file it belongs to. Records that have
!     never been written cannot be updated.
!
!     Because the DAS system buffers records that are written, multiple
!     updates of parts of a record can be made without incurring a
!     large number of file reads and writes.
!
!     Any buffered character record can be updated with this routine.
!     In particular, records that have been written to the DAS character
!     record buffer but have not yet been written out to the DAS file
!     they're intended to go to ARE visible to this routine.
!
!     This routine should be used to update only records that contain
!     character data.
!
! $ Examples
!
!     1)  Update the 10th through 100th characters in record number 9
!         in a DAS file designated by HANDLE.
!
!             CHARACTER*(100)       DATAC
!
!                         .
!                         .
!                         .
!
!             DATAC = 'The first 91 characters of this string, '      //
!            .        'including trailing blanks, will be written '   //
!            .        'to the indicated DAS file.'
!
!             CALL DASURC ( HANDLE, 9, 10, 100, DATAC )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
!
!        Added description of NWC to the $Parameters and $Brief_I/O
!        sections of the header.
!
!-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     update DAS character physical records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASURC' )

!
!     Check that the file is open for writing.  Signal an error if not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURC' )
         RETURN
      END IF

!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF


!
!     If FIRST or LAST are out of range, no dice.
!
      IF (        ( FIRST .LT. 1   ) &
            .OR.  ( FIRST .GT. NWC ) &
            .OR.  ( LAST  .LT. 1   ) &
            .OR.  ( LAST  .GT. NWC )    )  THEN

         CALL SETMSG ( 'String indices FIRST and LAST were #,  #; '   // &
                       'allowed range for both is [#, #]. File was '  // &
                       '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWC                                       )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASURC' )
         RETURN

      END IF

!
!     There's nothing to do if LAST < FIRST.
!
      IF ( LAST .LT. FIRST ) THEN
         CALL CHKOUT ( 'DASURC' )
         RETURN
      END IF

!
!     See whether character record number RECNO from file HANDLE is
!     buffered.  We'll search through the list of buffered records
!     starting at the head of the list.  If the record is already
!     buffered, we'll update the buffer entry, but we'll defer writing
!     the record out until we need to free a record, or until the
!     character buffer is flushed, whichever comes first.
!
      NODE = HEADC

      DO WHILE ( NODE .GT. 0 )

         IF (         (  HANDLE .EQ. HNBUFC(NODE)  ) &
               .AND.  (  RECNO  .EQ. RNBUFC(NODE)  )    )  THEN
!
!           Found it.  Update the buffered record.
!
            RCBUFC (NODE) ( FIRST : LAST )  =  DATAC

!
!           Set the update flag, indicating that this buffer entry
!           has been modified.
!
            UPBUFC ( NODE ) = .TRUE.

!
!           Put the information about this record at the head of the
!           active list, if it is not already there.
!
            IF ( NODE .NE. HEADC ) THEN

               CALL LNKXSL ( NODE, NODE,  POOLC )
               CALL LNKILB ( NODE, HEADC, POOLC )
               HEADC = NODE

            END IF

            CALL CHKOUT ( 'DASURC' )
            RETURN

         END IF

         NODE  =  POOLC ( FORWRD, NODE )

      END DO

!
!     The record we're writing to is not buffered.  We'll allocate
!     a buffer entry.  If the record buffer is full, we'll
!     commandeer the least recently accessed record.  Before using
!     this record, we'll write its contents out to the corresponding
!     file, if the record has been updated.
!
      IF ( USEDC .LT. BUFSZC ) THEN
!
!        There's a free buffer entry available.  Just allocate it.
!
         CALL LNKAN ( POOLC, NODE )
         USEDC = USEDC + 1

      ELSE
!
!        Grab the buffer entry at the tail end of the list.
!
         NODE  = LNKTL ( HEADC, POOLC )

         CALL LNKXSL ( NODE, NODE, POOLC )

!
!        If the allocated record was updated, write it out.
!
         IF (  UPBUFC(NODE)  ) THEN


            CALL ZZDDHHLU ( HNBUFC( NODE ), 'DAS', .FALSE., WRUNIT )

            CALL DASIOC ( 'WRITE', &
                           WRUNIT, &
                           RNBUFC( NODE ), &
                           RCBUFC( NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASURC' )
               RETURN
            END IF

         END IF

      END IF

!
!     Now try to read the record we're going to update.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE.,  UNIT )

      CALL DASIOC ( 'READ',  UNIT,  RECNO, RCBUFC(NODE) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURC' )
         RETURN
      END IF

!
!     The read was successful, so set the record number, handle,
!     and update flag for this buffer entry, and link these buffer
!     entries in before the current head of the list, thus putting
!     them at the head.
!
!     Update the head pointer.
!
      CALL LNKILB ( NODE, HEADC, POOLC )

      HNBUFC ( NODE )  =  HANDLE
      RNBUFC ( NODE )  =  RECNO
      UPBUFC ( NODE )  = .TRUE.
      HEADC            =  NODE

!
!     At long last, make the requested update.  Note that we don't
!     have to write the record back to the file; that will get done
!     automatically before or at the time the file is closed.
!
      RCBUFC ( NODE )  ( FIRST : LAST )   =   DATAC

      CALL CHKOUT ( 'DASURC' )
      RETURN







!$Procedure DASWBR ( DAS, write buffered records )

      ENTRY DASWBR ( HANDLE )

! $ Abstract
!
!     Write out all buffered records of a specified DAS file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAS file opened for writing.
!
! $ Detailed_Output
!
!     None.
!
!     See $Particulars for a description of the action of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled
!         by a routine in the call tree of this routine. The indicated
!         file will not be modified.
!
!     2)  If a write operation attempted by this routine fails, an
!         error is signaled by a routine in the call tree of this
!         routine. The status of the DAS file written to is uncertain
!         in this case.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine writes buffered records out to the DAS file to which
!     they correspond.
!
!     Because the DAS system buffers records that are written as well as
!     those that are read, data supplied to the DAS add data (DASADC,
!     DASADD, DASADI) and DAS update data (DASUDC, DASUDD, DASUDI)
!     routines on input has not necessarily been physically written to
!     the DAS file specified by the caller of those routines, at the
!     time those routines return. Before closing a DAS file that has
!     been opened for writing, the DAS system must write out to the file
!     any updated records present in the DAS buffers. The SPICELIB
!     routine DASCLS uses this routine to perform this function. The
!     routines DASAC and DASDC, through the use of the SPICELIB routines
!     DASACR and DASRCR, which respectively add comment records to or
!     delete comment records from a DAS file, use this routine to ensure
!     that the SPICELIB routine DASRWR record buffers don't become out
!     of sync with the file they operate upon.
!
!     In addition, this routine can be used by application programs
!     that create or update DAS files. The reason for calling this
!     routine directly would be to provide a measure of safety when
!     writing a very large file: if the file creation or update were
!     interrupted, the amount of work lost due to the loss of buffered,
!     unwritten records could be reduced.
!
!     However, routines outside of SPICELIB will generally not need to
!     call this routine directly.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Write a DAS file by adding data to it over multiple passes.
!        Avoid spending time on file segregation between writes.
!
!        Each pass opens the file, adds character, double precision,
!        and integer data to the file, writes out buffered data by
!        calling DASWBR, and closes the file without segregating the
!        data by calling DASLLC.
!
!        The program also checks the file: after the final write,
!        the program reads the data and compares it to expected values.
!
!        Note that most user-oriented applications should segregate a
!        DAS file after writing it, since this greatly enhances file
!        reading efficiency. The technique demonstrated here may be
!        useful for cases in which a file will be written via many
!        small data additions, and in which the file is read between
!        write operations.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASWBR_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               FTYPLN
!              PARAMETER           ( FTYPLN = 3 )
!
!              INTEGER               CHRLEN
!              PARAMETER           ( CHRLEN = 50 )
!
!              INTEGER               IBUFSZ
!              PARAMETER           ( IBUFSZ = 20 )
!
!              INTEGER               DBUFSZ
!              PARAMETER           ( DBUFSZ = 30 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(CHRLEN)    CHRBUF
!              CHARACTER*(FILSIZ)    FNAME
!              CHARACTER*(FTYPLN)    FTYPE
!              CHARACTER*(CHRLEN)    XCHRBF
!
!              DOUBLE PRECISION      DPBUF  ( DBUFSZ )
!              DOUBLE PRECISION      XDPBUF ( DBUFSZ )
!
!              INTEGER               FIRSTC
!              INTEGER               FIRSTD
!              INTEGER               FIRSTI
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               INTBUF ( IBUFSZ )
!              INTEGER               J
!              INTEGER               LASTC
!              INTEGER               LASTD
!              INTEGER               LASTI
!              INTEGER               NCALL
!              INTEGER               NCOMR
!              INTEGER               NPASS
!              INTEGER               PASSNO
!              INTEGER               XINTBF ( IBUFSZ )
!
!
!        C
!        C     Initial values
!        C
!              DATA                  FNAME  / 'daswbr_ex1.das' /
!              DATA                  FTYPE  / 'ANG' /
!              DATA                  NCALL  / 1000  /
!              DATA                  NCOMR  / 10    /
!              DATA                  NPASS  / 3     /
!
!        C
!        C     Open a new DAS file. We'll allocate NCOMR records
!        C     for comments. The file type is not one of the standard
!        C     types recognized by SPICE; however it can be used to
!        C     ensure the database file is of the correct type.
!        C
!        C     We'll use the file name as the internal file name.
!        C
!              CALL DASONW ( FNAME, FTYPE, FNAME, NCOMR, HANDLE )
!
!        C
!        C     Add data of character, integer, and double precision
!        C     types to the file in interleaved fashion. We'll add to
!        C     the file over NPASS "passes," in each of which we close
!        C     the file after writing.
!        C
!              DO PASSNO = 1, NPASS
!
!                 IF ( PASSNO .GT. 1 ) THEN
!
!                    WRITE (*,*) 'Opening file for write access...'
!
!                    CALL DASOPW( FNAME, HANDLE )
!
!                 END IF
!
!                 DO I = 1, NCALL
!        C
!        C           Add string data to the file.
!        C
!                    CHRBUF = 'Character value #'
!                    CALL REPMI( CHRBUF, '#', I, CHRBUF )
!
!                    CALL DASADC ( HANDLE, CHRLEN, 1, CHRLEN, CHRBUF )
!
!        C
!        C           Add double precision data to the file.
!        C
!                    DO J = 1, DBUFSZ
!                       DPBUF(J) = DBLE( 100000000*PASSNO + 100*I + J )
!                    END DO
!
!                    CALL DASADD ( HANDLE, DBUFSZ, DPBUF )
!
!        C
!        C           Add integer data to the file.
!        C
!                    DO J = 1, IBUFSZ
!                       INTBUF(J) = 100000000*PASSNO  +  100 * I  +  J
!                    END DO
!
!                    CALL DASADI ( HANDLE, IBUFSZ, INTBUF )
!
!                 END DO
!
!        C
!        C        Write buffered data to the file.
!        C
!                 WRITE (*,*) 'Writing buffered data...'
!                 CALL DASWBR ( HANDLE )
!
!        C
!        C        Close the file without segregating it.
!        C
!                 WRITE (*,*) 'Closing DAS file...'
!                 CALL DASLLC ( HANDLE )
!
!              END DO
!
!              WRITE (*,*) 'File write is done.'
!
!        C
!        C     Check file contents.
!        C
!              CALL DASOPR( FNAME, HANDLE )
!
!        C
!        C     Read data from the file; compare to expected values.
!        C
!        C     Initialize end addresses.
!        C
!              LASTC = 0
!              LASTD = 0
!              LASTI = 0
!
!              DO PASSNO = 1, NPASS
!
!                 DO I = 1, NCALL
!
!        C
!        C           Check string data.
!        C
!                    XCHRBF = 'Character value #'
!                    CALL REPMI( XCHRBF, '#', I, XCHRBF )
!
!                    FIRSTC = LASTC + 1
!                    LASTC  = LASTC + CHRLEN
!
!                    CALL DASRDC ( HANDLE, FIRSTC, LASTC,
!             .                    1,      CHRLEN, CHRBUF )
!
!                    IF ( CHRBUF .NE. XCHRBF ) THEN
!                       WRITE (*,*) 'Character data mismatch: '
!                       WRITE (*,*) 'PASS     = ', PASSNO
!                       WRITE (*,*) 'I        = ', I
!                       WRITE (*,*) 'Expected = ', XCHRBF
!                       WRITE (*,*) 'Actual   = ', CHRBUF
!                       STOP
!                    END IF
!
!        C
!        C           Check double precision data.
!        C
!                    DO J = 1, DBUFSZ
!                       XDPBUF(J) = DBLE(   100000000*PASSNO
!             .                           + 100*I + J        )
!                    END DO
!
!                    FIRSTD = LASTD + 1
!                    LASTD  = LASTD + DBUFSZ
!
!                    CALL DASRDD ( HANDLE, FIRSTD, LASTD, DPBUF )
!
!                    DO J = 1, DBUFSZ
!
!                       IF ( DPBUF(J) .NE. XDPBUF(J) ) THEN
!
!                          WRITE (*,*)
!             .                   'Double precision data mismatch: '
!                          WRITE (*,*) 'PASS     = ', PASSNO
!                          WRITE (*,*) 'I        = ', I
!                          WRITE (*,*) 'J        = ', J
!                          WRITE (*,*) 'Expected = ', XDPBUF(J)
!                          WRITE (*,*) 'Actual   = ', DPBUF(J)
!                          STOP
!
!                       END IF
!
!                    END DO
!
!        C
!        C           Check integer data.
!        C
!                    DO J = 1, IBUFSZ
!                       XINTBF(J) = 100000000*PASSNO  +  100 * I  +  J
!                    END DO
!
!                    FIRSTI = LASTI + 1
!                    LASTI  = LASTI + IBUFSZ
!
!                    CALL DASRDI ( HANDLE, FIRSTI, LASTI, INTBUF )
!
!                    DO J = 1, IBUFSZ
!
!                       IF ( INTBUF(J) .NE. XINTBF(J) ) THEN
!
!                          WRITE (*,*) 'Integer data mismatch: '
!                          WRITE (*,*) 'PASS     = ', PASSNO
!                          WRITE (*,*) 'I        = ', I
!                          WRITE (*,*) 'J        = ', J
!                          WRITE (*,*) 'Expected = ', XINTBF(J)
!                          WRITE (*,*) 'Actual   = ', INTBUF(J)
!                          STOP
!
!                       END IF
!
!                    END DO
!
!                 END DO
!
!              END DO
!
!              WRITE (*,*) 'File check is done.'
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Writing buffered data...
!         Closing DAS file...
!         Opening file for write access...
!         Writing buffered data...
!         Closing DAS file...
!         Opening file for write access...
!         Writing buffered data...
!         Closing DAS file...
!         File write is done.
!         File check is done.
!
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 19-MAY-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Updated $Particulars in order to provide information about the
!        high level APIs that actually use this routine.
!
!-    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB)
!
!        Upgraded to support handle manager integration.
!
!-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
!
!        Removed weird spaces from ENTRY statement.
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     write buffered records to a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASWBR' )

!
!     Check that the file is open for writing.  Signal an error if not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWBR' )
         RETURN
      END IF

!
!     If it hasn't been done yet, initialize the pointer list pools.
!
      IF ( PASS1 ) THEN

         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )

         PASS1  = .FALSE.

      END IF

!
!     Obtain a logical unit for HANDLE.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., WRUNIT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWBR' )
         RETURN
      END IF

!
!     For each buffer, find the records belonging to this file, and
!     write them out to the file.
!
!     Double precision records first.
!
      NODE = HEADD

      DO WHILE ( NODE .GT. 0 )

         IF ( HANDLE .EQ. HNBUFD(NODE) ) THEN
!
!           This record belongs to the file of interest, so write the
!           the record out.
!
            CALL DASIOD ( 'WRITE', &
                           WRUNIT, &
                           RNBUFD(    NODE ), &
                           RCBUFD( 1, NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWBR' )
               RETURN
            END IF

!
!           The record is no longer in use; return it to the
!           free list.  But grab the successor first.  Update
!           the head of the list, if the node we're freeing is
!           the head node.  Decrement the number of used d.p.
!           buffer elements.
!
            NEXT = POOLD ( FORWRD, NODE )

            IF ( NODE .EQ. HEADD ) THEN
               HEADD = NEXT
            END IF

            CALL LNKFSL ( NODE, NODE, POOLD )

            NODE   =  NEXT
            USEDD  =  USEDD - 1

         ELSE
!
!           Just get the next node.
!
            NODE = POOLD ( FORWRD, NODE )

         END IF

      END DO



!
!     Next, integer records.
!
      NODE = HEADI

      DO WHILE ( NODE .GT. 0 )

         IF ( HANDLE .EQ. HNBUFI(NODE) ) THEN
!
!           This record belongs to the file of interest, so write the
!           the record out.
!
            CALL DASIOI ( 'WRITE', &
                           WRUNIT, &
                           RNBUFI(    NODE ), &
                           RCBUFI( 1, NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWBR' )
               RETURN
            END IF

!
!           The record is no longer in use; return it to the
!           free list.  But grab the successor first.  Update
!           the head of the list, if the node we're freeing is
!           the head node.  Decrement the number of used integer
!           buffer elements.
!
            NEXT = POOLI ( FORWRD, NODE )

            IF ( NODE .EQ. HEADI ) THEN
               HEADI = NEXT
            END IF

            CALL LNKFSL ( NODE, NODE, POOLI )

            NODE   =  NEXT
            USEDI  =  USEDI - 1

         ELSE
!
!           Just get the next node.
!
            NODE = POOLI ( FORWRD, NODE )

         END IF

      END DO



!
!     And last, character records.
!
      NODE = HEADC

      DO WHILE ( NODE .GT. 0 )

         IF ( HANDLE .EQ. HNBUFC(NODE) ) THEN
!
!           This record belongs to the file of interest, so write the
!           the record out.
!
            CALL DASIOC ( 'WRITE', &
                           WRUNIT, &
                           RNBUFC( NODE ), &
                           RCBUFC( NODE )  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWBR' )
               RETURN
            END IF

!
!           The record is no longer in use; return it to the
!           free list.  But grab the successor first.  Update
!           the head of the list, if the node we're freeing is
!           the head node.  Decrement the number of used character
!           buffer elements.
!
            NEXT = POOLC ( FORWRD, NODE )

            IF ( NODE .EQ. HEADC ) THEN
               HEADC = NEXT
            END IF

            CALL LNKFSL ( NODE, NODE, POOLC )

            NODE   =  NEXT
            USEDC  =  USEDC - 1

         ELSE
!
!           Just get the next node.
!
            NODE = POOLC ( FORWRD, NODE )

         END IF

      END DO

      CALL CHKOUT ( 'DASWBR' )
      RETURN
      END
