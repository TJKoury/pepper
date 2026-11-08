!$Procedure DASSDR ( DAS, segregate data records )

      SUBROUTINE DASSDR ( HANDLE )

! $ Abstract
!
!     Segregate the data records in a DAS file into clusters, using
!     one cluster per data type present in the file.
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
!     ORDER
!     SORT
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle of a DAS file opened for writing.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If a Fortran read attempted by this routine fails, an error is
!         signaled by a routine in the call tree of this routine. The
!         state of the DAS file undergoing re-ordering will be
!         indeterminate.
!
!     3)  If a Fortran write attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         state of the DAS file undergoing re-ordering will be
!         indeterminate.
!
!     4)  If any other I/O error occurs during the re-arrangement of the
!         records in the indicated DAS file, the error is signaled by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Normally, there should be no need for routines outside of
!     SPICELIB to call this routine.
!
!     The effect of this routine is to re-arrange the data records
!     in a DAS file so that the file contains a single cluster for
!     each data type present in the file: in the general case, there
!     will be a single cluster of each of the integer, double
!     precision, and character data types.
!
!     The relative order of data records of a given type is not
!     affected by this re-ordering. After the re-ordering, the DAS
!     file contains a single directory record that has one descriptor
!     for each cluster. After that point, the order in the file of the
!     sets of data records of the various data types will be:
!
!        +-------+
!        |  CHAR |
!        +-------+
!        |  DP   |
!        +-------+
!        |  INT  |
!        +-------+
!
!     Files that contain multiple directory records will have all but
!     the first directory record moved to the end of the file when the
!     re-ordering is complete. These records are not visible to the
!     DAS system and will be overwritten if data is subsequently added
!     to the DAS file.
!
!     The purpose of segregating a DAS file's data records into three
!     clusters is to make read access more efficient: when a DAS file
!     contains a single directory with at most three cluster type
!     descriptors, mapping logical to physical addresses can be done
!     in constant time.
!
! $ Examples
!
!     1)  Segregate data records in a DAS file designated by
!         HANDLE:
!
!            CALL DASSDR ( HANDLE )
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
!     H.A. Neilan        (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 19-DEC-1995 (NJB)
!
!        Corrected title of permuted index entry section.
!
!-    SPICELIB Version 2.0.0, 17-NOV-1993 (KRG)
!
!        Added test of FAILED after each DAS call, or sequence of calls,
!        which returns immediately if FAILED is .TRUE. This fixes a bug
!        where DASOPS signals an error and then DASSDR has a
!        segmentation fault.
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS)
!
!        Bug fix: call to CLEARD replaced with call to
!        CLEARI.
!
!-    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB) (MJS)
!
!        Bug fix: extraneous commas removed from argument lists
!        in calls to DASADI.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     segregate the data records in a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 17-NOV-1993 (KRG)
!
!        Added test of failed after each DAS call, or sequence of calls,
!        which returns immediately if FAILED is .TRUE. This fixes a bug
!        where DASOPS signals an error and then DASSDR has a
!        segmentation fault.
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS)
!
!        Bug fix: call to CLEARD replaced with call to
!        CLEARI.
!
!-    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB)
!
!        Bug fix: extraneous commas removed from argument lists
!        in calls to DASADI. This bug had no visible effect on
!        VAX and Sun systems, but generated a compile error under
!        Lahey Fortran.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               SUMAI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NWC
      PARAMETER           ( NWC   = 1024 )

      INTEGER               NWD
      PARAMETER           ( NWD   =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI   =  256 )

!
!     Data type parameters
!
      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )

      INTEGER               DIR
      PARAMETER           ( DIR    =  4  )

!
!     Directory pointer locations (backward and forward):
!
      INTEGER               BWDLOC
      PARAMETER           ( BWDLOC = 1 )

      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )

!
!     Directory address range location base
!
      INTEGER               RNGBAS
      PARAMETER           ( RNGBAS  =  2  )


!
!     Location of first type descriptor
!
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )

!
!     Local variables
!
      CHARACTER*(NWC)       CREC
      CHARACTER*(NWC)       SAVEC

      DOUBLE PRECISION      DREC   ( NWD )
      DOUBLE PRECISION      SAVED  ( NWD )

      INTEGER               BASE
      INTEGER               COUNT  ( 4 )
      INTEGER               DEST
      INTEGER               DRBASE
      INTEGER               FREE
      INTEGER               I
      INTEGER               IREC   ( NWI    )
      INTEGER               J
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LOC
      INTEGER               LREC
      INTEGER               LTYPE
      INTEGER               LWORD
      INTEGER               MAXADR
      INTEGER               MINADR
      INTEGER               N
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               OFFSET
      INTEGER               POS
      INTEGER               PRVTYP
      INTEGER               RECNO
      INTEGER               SAVEI  ( NWI    )
      INTEGER               SAVTYP
      INTEGER               SCRHAN
      INTEGER               START
      INTEGER               TOTAL
      INTEGER               TYPE
      INTEGER               UNIT

      LOGICAL               MORE



!
!     Saved variables
!

!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      INTEGER               NEXT   ( 3 )
      SAVE                  NEXT

      INTEGER               PREV   ( 3 )
      SAVE                  PREV

!
!     Initial values
!
      DATA                  NEXT   /  2,   3,   1  /
      DATA                  PREV   /  3,   1,   2  /




!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASSDR' )
      END IF

!
!     Before starting, make sure that this DAS file is open for
!     writing.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

!
!     Get the logical unit for this file.
!
      CALL DASHLU ( HANDLE, UNIT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF

!
!     Write out any buffered records that belong to the file.
!
      CALL DASWBR ( HANDLE )

      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF

!
!     We're going to re-order the physical records in the DAS file,
!     starting with the first record after the first directory.
!     The other directory records are moved to the end of the file
!     as a result of the re-ordering.
!
!     The re-ordering algorithm is based on that used in the REORDx
!     routines.  To use this algorithm, we'll build an order vector
!     for the records to be ordered; we'll construct this order vector
!     in a scratch DAS file.  First, we'll traverse the directories
!     to build up a sort of inverse order vector that tells us the
!     final destination and data type of each data record;  from this
!     inverse vector we can easily build a true order vector.  The
!     cycles of the true order vector can be traversed without
!     repetitive searching, and with a minimum of assignment of the
!     contents of data records to temporary variables.
!

!
!     Allocate a scratch DAS file to keep our vectors in.
!
      CALL DASOPS ( SCRHAN )

      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF

!
!     Now build up our `inverse order vector'.   This array is an
!     inverse order vector only in loose sense:  it actually consists
!     of an integer array that contains a sequence of pairs of integers,
!     the first of which indicates a data type, and the second of which
!     is an ordinal number.  There is one pair for each data record in
!     the file.  The ordinal number gives the ordinal position of the
!     record described by the number pair, relative to the other records
!     of the same type.  Directory records are considered to have type
!     `directory', which is represented by the code DIR.
!
!     We also must maintain a count of records of each type.
!
      CALL CLEARI ( 4, COUNT )

!
!     Get the file summary for the DAS file to be segregated.
!
      CALL DASHFS ( HANDLE, &
                    NRESVR, &
                    NRESVC, &
                    NCOMR, &
                    NCOMC, &
                    FREE, &
                    LASTLA, &
                    LASTRC, &
                    LASTWD )

      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF
!
!     Find the record and word positions LREC and LWORD of the last
!     descriptor in the file, and also find the type of the descriptor
!     LTYPE.
!
      CALL MAXAI  ( LASTRC,  3,  LREC,  LOC )
      LWORD = 0

      DO I = 1, 3

         IF (       ( LASTRC(I) .EQ. LREC  ) &
              .AND. ( LASTWD(I) .GT. LWORD )  ) THEN

            LWORD = LASTWD(I)
            LTYPE = I

         END IF

      END DO

!
!     The first directory starts after the last comment record.
!
      RECNO  =  NRESVR + NCOMR + 2


      DO WHILE (  ( RECNO .LE. LREC ) .AND. ( RECNO .GT. 0 )  )
!
!        Read the directory record.
!
         CALL DASRRI ( HANDLE, RECNO, 1, NWI, IREC )

         IF ( FAILED () ) THEN
             CALL CHKOUT ( 'DASSDR' )
           RETURN
         END IF
!
!        Increment the directory count.
!
         COUNT(DIR)  =  COUNT(DIR) + 1

!
!        Add the data type (`directory') and count (1) of the current
!        record to the inverse order vector.
!
         CALL DASADI ( SCRHAN,  1,  DIR        )
         CALL DASADI ( SCRHAN,  1,  COUNT(DIR) )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
!
!        Set up our `finite state machine' that tells us the data
!        types of the records described by the last read directory.
!
         TYPE    =  IREC ( BEGDSC )
         PRVTYP  =  PREV ( TYPE   )

!
!        Now traverse the directory and update the inverse order
!        vector based on the descriptors we find.
!
         MORE  =  .TRUE.
         I     =   BEGDSC + 1


         DO WHILE ( MORE )
!
!           Obtain the count for the current descriptor.
!
            N  =  ABS (  IREC(I)  )

!
!           Update our inverse order vector to describe the positions
!           of the N records described by the current descriptor.
!
            DO J  =  1, N

               CALL DASADI (  SCRHAN,  1,  TYPE           )
               CALL DASADI (  SCRHAN,  1,  COUNT(TYPE)+J  )

               IF ( FAILED () ) THEN
                  CALL CHKOUT ( 'DASSDR' )
                  RETURN
               END IF

            END DO

!
!           Adjust the count of records of data type TYPE.
!
            COUNT(TYPE)  =  COUNT(TYPE)   +   N

!
!           Find the next type.
!
            I = I + 1

            IF (      (  I     .GT. NWI                        ) &
                 .OR. ( (RECNO .EQ. LREC) .AND. (I .GT. LWORD) )  ) THEN

               MORE = .FALSE.

            ELSE

               IF ( IREC(I) .GT. 0 ) THEN
                  TYPE = NEXT(TYPE)

               ELSE IF ( IREC(I) .LT. 0 ) THEN
                  TYPE = PREV(TYPE)

               ELSE
                  MORE = .FALSE.

               END IF

            END IF

         END DO

!
!        The forward pointer in this directory tells us where the
!        next directory record is.  When there are no more directory
!        records, this pointer will be zero.
!
         RECNO = IREC ( FWDLOC )

      END DO

!
!     At this point, the inverse order vector is set up.  The array
!     COUNT contains counts of the number of records of each type we've
!     seen.  Set TOTAL to the total number of records that we've going
!     to permute.
!
      TOTAL =  SUMAI ( COUNT, 4 )

!
!     The next step is to build a true order vector.  Let BASE be
!     the base address for the order vector; this address is the
!     last logical address of the inverse order vector.
!
      BASE  =  2 * TOTAL

!
!     We'll store the actual order vector in locations BASE + 1
!     through BASE + TOTAL.  In addition, we'll build a parallel array
!     that contains, for each element of the order vector, the type of
!     data corresponding to that element.  This type vector will
!     reside in locations BASE + TOTAL + 1 through BASE + 2*TOTAL.
!
!     Before setting the values of the order vector and its parallel
!     type vector, we'll allocate space in the scratch DAS file by
!     zeroing out the locations we plan to use.  After this, locations
!     BASE+1 through BASE + 2*TOTAL can be written to in random access
!     fashion using DASUDI.
!
!
      DO I = 1, 2*TOTAL

         CALL DASADI ( SCRHAN, 1, 0 )

      END DO

      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF
!
!     We note that the way to construct the inverse of a permutation
!     SIGMA in a single loop is suggested by the relation
!
!             -1
!        SIGMA   (  SIGMA(I)  )   =   I
!
!     We'll use this method.  In our case, our order vector plays
!     the role of
!
!             -1
!        SIGMA
!
!     and the `inverse order vector' plays the role of SIGMA.  We'll
!     exclude the first directory from the order vector, since it's
!     an exception:  we wish to reserve this record.  Since the first
!     element of the order vector (logically) contains the index 1, we
!     can ignore it.
!
!
      DO I = 2, TOTAL

         CALL DASRDI ( SCRHAN,  (2*I)-1,  (2*I)-1,  TYPE  )
         CALL DASRDI ( SCRHAN,   2*I,      2*I,     DEST  )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
!
!        Set DEST to the destination location, measured as an offset
!        from the last comment record, of the Ith record by adding
!        on the count of the predecessors of the block of records of
!        TYPE.
!
         DO J = 1, 3

            IF ( TYPE .GT. J ) THEN
               DEST = DEST + COUNT(J)
            END IF

         END DO

!
!        The destination offset of each record should be incremented to
!        allow room for the first directory record.  However, we don't
!        need to do this for directory records; they'll already have
!        this offset accounted for.
!
         IF ( TYPE .NE. DIR ) THEN
            DEST = DEST + 1
         END IF

!
!        The value of element DEST of the order vector is I.
!        Write this value to location BASE + DEST.
!
         CALL DASUDI ( SCRHAN, BASE+DEST, BASE+DEST, I )
!
!        We want the ith element of the order vector to give us the
!        number of the record to move to position i (offset from the
!        last comment record),  but we want the corresponding element
!        of the type array to give us the type of the record currently
!        occupying position i.
!
         CALL DASUDI ( SCRHAN, BASE+I+TOTAL, BASE+I+TOTAL, TYPE )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF

      END DO

!
!     Ok, here's what we've got in the scratch file that's still of
!     interest:
!
!        -- In integer logical addresses BASE + 1 : BASE + TOTAL,
!           we have an order vector.  The Ith element of this
!           vector indicates the record that should be moved to
!           location DRBASE + I in the DAS file we're re-ordering,
!           where DRBASE is the base address of the data records
!           (the first directory record follows the record having this
!           index).
!
!
!        -- In integer logical addresses BASE + TOTAL + 1  :  BASE +
!           2*TOTAL, we have data type indicators for the records to
!           be re-ordered.  The type for the Ith record in the file,
!           counted from the last comment record, is located in logical
!           address BASE + TOTAL + I.
!
!
      DRBASE  =  NRESVR + NCOMR + 1

!
!     As we traverse the order vector, we flip the sign of elements
!     we've accessed, so that we can tell when we encounter an element
!     of a cycle that we've already traversed.
!
!     Traverse the order vector.  The variable START indicates the
!     first element to look at.  Ignore the first element; it's a
!     singleton cycle.
!
!
      START = 2

      DO WHILE ( START .LT. TOTAL )
!
!        Traverse the current cycle of the order vector.
!
!        We `make a hole' in the file by saving the record in position
!        START, then we traverse the cycle in reverse order, filling in
!        the hole at the ith position with the record whose number is
!        the ith element of the order vector.  At the end, we deposit
!        the saved record into the `hole' left behind by the last
!        record we moved.
!
!        We're going to read and write records to and from the DAS file
!        directly, rather than going through the buffering system.
!        This will allow us to avoid any untoward interactions between
!        the buffers for different data types.
!
         CALL DASRDI ( SCRHAN, &
                       BASE + TOTAL + START, &
                       BASE + TOTAL + START, &
                       SAVTYP                )

         CALL DASRDI ( SCRHAN,  BASE + START,  BASE + START,  OFFSET )

!
!        Save the record at the location DRBASE + START.
!
         IF ( SAVTYP .EQ. CHAR ) THEN

            CALL DASIOC ( 'READ', UNIT, DRBASE+START, SAVEC )

         ELSE IF ( SAVTYP .EQ. DP ) THEN

            CALL DASIOD ( 'READ', UNIT, DRBASE+START, SAVED )

         ELSE

            CALL DASIOI ( 'READ', UNIT, DRBASE+START, SAVEI )

         END IF

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
!
!        Let I be the index of the record that we are going to move
!        data into next.  I is an offset from the last comment record.
!
         I  =  START

         DO WHILE ( OFFSET .NE. START )
!
!           Mark the order vector element by writing its negative
!           back to the location it came from.
!
            CALL DASUDI ( SCRHAN,  BASE+I,  BASE+I,  -OFFSET )
!
!           Move the record at location
!
!              DRBASE + OFFSET
!
!           to location
!
!              DRBASE + I
!
!           There is no need to do anything about the corresponding
!           elements of the type vector; we won't need them again.
!
!           The read and write operations, as well as the temporary
!           record required to perform the move, are dependent on the
!           data type of the record to be moved.
!
            CALL DASRDI ( SCRHAN, &
                          BASE + TOTAL + OFFSET, &
                          BASE + TOTAL + OFFSET, &
                          TYPE                    )

            IF ( FAILED () ) THEN
               CALL CHKOUT ( 'DASSDR' )
               RETURN
            END IF
!
!           Only pick records up if we're going to put them down in
!           a location other than their original one.
!
            IF ( I .NE. OFFSET ) THEN

               IF ( TYPE .EQ. CHAR ) THEN

                  CALL DASIOC ( 'READ',  UNIT, DRBASE+OFFSET, CREC )
                  CALL DASIOC ( 'WRITE', UNIT, DRBASE+I,      CREC )

               ELSE IF ( TYPE .EQ. DP ) THEN

                  CALL DASIOD ( 'READ',  UNIT, DRBASE+OFFSET, DREC )
                  CALL DASIOD ( 'WRITE', UNIT, DRBASE+I,      DREC )

               ELSE

                  CALL DASIOI ( 'READ',  UNIT, DRBASE+OFFSET, IREC )
                  CALL DASIOI ( 'WRITE', UNIT, DRBASE+I,      IREC )

               END IF

               IF ( FAILED () ) THEN
                  CALL CHKOUT ( 'DASSDR' )
                  RETURN
               END IF

            END IF

!
!           OFFSET is the index of the next order vector element to
!           look at.
!
            I  =  OFFSET

            CALL DASRDI ( SCRHAN, BASE+I,       BASE+I,       OFFSET )
            CALL DASRDI ( SCRHAN, BASE+I+TOTAL, BASE+I+TOTAL, TYPE   )

            IF ( FAILED () ) THEN
               CALL CHKOUT ( 'DASSDR' )
               RETURN
            END IF

         END DO

!
!        The last value of I is the location in the cycle that element
!        START followed.  Therefore, the saved record corresponding
!        to index START should be written to this location.
!
         IF ( SAVTYP .EQ. CHAR ) THEN

            CALL DASIOC ( 'WRITE',  UNIT,  DRBASE+I,  SAVEC )

         ELSE IF ( SAVTYP .EQ. DP ) THEN

            CALL DASIOD ( 'WRITE',  UNIT,  DRBASE+I,  SAVED )

         ELSE

            CALL DASIOI ( 'WRITE',  UNIT,  DRBASE+I,  SAVEI )

         END IF
!
!        Mark the order vector element by writing its negative
!        back to the location it came from.
!
         CALL DASUDI ( SCRHAN,  BASE+I,  BASE+I, -START )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
!
!        Update START so that it points to the first element of a cycle
!        of the order vector that has not yet been traversed.  This will
!        be the first positive element of the order vector in a location
!        indexed higher than the current value of START.  Note that
!        this way of updating START guarantees that we don't have to
!        backtrack to find an element in the next cycle.
!

         OFFSET = -1

         DO WHILE ( ( OFFSET .LT. 0 ) .AND. ( START .LT. TOTAL ) )

            START  =  START  + 1

            CALL DASRDI (  SCRHAN,  BASE+START,  BASE+START,  OFFSET  )

            IF ( FAILED () ) THEN
               CALL CHKOUT ( 'DASSDR' )
               RETURN
            END IF

         END DO

!
!        At this point, START is the index of an element in the order
!        vector that belongs to a cycle where no routine has gone
!        before, or else START is the last index in the order vector,
!        in which case we're done.
!

      END DO

!
!     At this point, the records in the DAS are organized as follows:
!
!        +----------------------------------+
!        |           File record            |  ( 1 )
!        +----------------------------------+
!        |         Reserved records         |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |          Comment records         |  ( 0 or more )
!        |                                  |
!        |                                  |
!        +----------------------------------+
!        |      First directory  record     |  ( 1 )
!        +----------------------------------+
!        |      Character data records      |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |   Double precision data records  |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |       Integer data records       |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |   Additional directory records   |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!
!
!     Not all of the indicated components must be present; only the
!     file record and first directory record will exist in all cases.
!     The `additional directory records' at the end of the file serve
!     no purpose; if more data is appended to the file, they will be
!     overwritten.
!
!     The last step in preparing the file is to fill in the first
!     directory record with the correct information, and to update
!     the file summary.
!
!
      RECNO  =  DRBASE + 1

      CALL CLEARI ( NWI, IREC )

!
!     Set the logical address ranges in the directory record, for each
!     data type.
!

      DO TYPE = 1, 3

         MAXADR  =  LASTLA(TYPE)

         IF ( MAXADR .GT. 0 ) THEN
            MINADR = 1
         ELSE
            MINADR = 0
         END IF

         IREC (  RNGBAS + (2*TYPE) - 1  )  =  MINADR
         IREC (  RNGBAS + (2*TYPE)      )  =  MAXADR

      END DO

!
!     Set the descriptors in the directory.  Determine which type
!     comes first:  the order of priority is character, double
!     precision, integer.
!
      POS = BEGDSC

      DO TYPE = 1, 3

         IF (  LASTLA(TYPE)  .GT.  0  ) THEN

            IF ( POS .EQ. BEGDSC ) THEN
!
!              This is the first type for which any data is present.
!              We must enter a type code at position BEGDSC in the
!              directory, and we must enter a count at position
!              BEGDSC+1.
!
               IREC   ( BEGDSC       )  =  TYPE
               IREC   ( BEGDSC + 1   )  =  COUNT(TYPE)
               LASTRC ( TYPE )          =  RECNO
               LASTWD ( TYPE )          =  BEGDSC + 1
               POS                      =  POS + 2
               PRVTYP                   =  TYPE

            ELSE
!
!              Place an appropriately signed count at location POS in
!              the directory.
!
               IF ( TYPE .EQ. NEXT(PRVTYP) ) THEN
                  IREC ( POS )  =   COUNT(TYPE)
               ELSE
                  IREC ( POS )  =  -COUNT(TYPE)
               END IF

               LASTRC ( TYPE )  =  RECNO
               LASTWD ( TYPE )  =  POS
               POS              =  POS + 1
               PRVTYP           =  TYPE

            END IF

         END IF

      END DO

!
!     Since we've done away with all but the first directory, the first
!     free record is decremented by 1 less than the directory count.
!
      FREE  =  FREE  -  COUNT( DIR )  +  1

!
!     Write out the new directory record.  Don't use the DAS buffered
!     write mechanism; this could trash the file by dumping buffered
!     records in the wrong places.
!
      CALL DASIOI ( 'WRITE',  UNIT,  RECNO,  IREC  )
!
!     Write out the updated file summary.
!
      CALL DASUFS ( HANDLE, &
                    NRESVR, &
                    NRESVC, &
                    NCOMR, &
                    NCOMC, &
                    FREE, &
                    LASTLA, &
                    LASTRC, &
                    LASTWD )
!
!     Clean up the DAS data buffers:  we don't want buffered scratch
!     file records hanging around there.  Then get rid of the scratch
!     file.
!
      CALL DASWBR ( SCRHAN )
      CALL DASLLC ( SCRHAN )


      CALL CHKOUT ( 'DASSDR' )
      RETURN
      END
