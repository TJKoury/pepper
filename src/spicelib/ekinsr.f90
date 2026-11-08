!$Procedure EKINSR ( EK, insert record into segment )

      SUBROUTINE EKINSR ( HANDLE, SEGNO, RECNO )

! $ Abstract
!
!     Add a new, empty record to a specified E-kernel segment at
!     a specified index.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     PRIVATE
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECNO

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RECNO      I   Record number.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle of an EK open for write access.
!
!     SEGNO    is the number of the segment to which the record
!              is to be added.
!
!     RECNO    is the index of the new record. RECNO must be
!              in the range 1 : (NREC+1), where NREC is the
!              number of records in the segment prior to the
!              insertion. If RECNO is equal to NREC+1, the
!              new record is appended. Otherwise, the new
!              record has the ordinal position specified by
!              RECNO, and the records previously occupying
!              positions RECNO : NREC have their indexes
!              incremented by 1.
!
! $ Detailed_Output
!
!     None. See the $Particulars section for a description of the
!     effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine. The file will not be modified.
!
!     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX)
!         is signaled. The file will not be modified.
!
!     3)  If RECNO is out of range, the error SPICE(INVALIDINDEX)
!         is signaled. The file will not be modified.
!
!     4)  If an I/O error occurs while reading or writing the indicated
!         file, the error is signaled by a routine in the call tree of
!         this routine. The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading ek.req for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects: It adds a new, empty
!     record structure to an EK segment at a specified ordinal position.
!
!     After a record has been inserted into a segment by this routine,
!     the record must be populated with data using the EKACEx
!     routines. EKs are valid only when all of their column entries
!     are initialized.
!
!     To append a record to a segment, use the routine EKAPPR.
!
!     This routine cannot be used with the "fast write" suite of
!     routines. See the EK Required Reading ek.req for a discussion of
!     the fast writers.
!
!     When a record is inserted into an EK file that is not shadowed,
!     the status of the record starts out set to OLD. The status
!     does not change when data is added to the record.
!
!     If the target EK is shadowed, the new record will be given the
!     status NEW. Updating column values in the record does not change
!     its status. When changes are committed, the status is set to OLD.
!     If a rollback is performed before changes are committed, the
!     record is deleted. Closing the target file without committing
!     changes implies a rollback.
!
! $ Examples
!
!     1)  Insert a record into a specified E-kernel segment at a
!         specified ordinal position.
!
!         Suppose we have an E-kernel named ORDER_DB.EK which contains
!         records of orders for data products. The E-kernel has a
!         table called DATAORDERS that consists of the set of columns
!         listed below:
!
!            DATAORDERS
!
!               Column Name     Data Type
!               -----------     ---------
!               ORDER_ID        INTEGER
!               CUSTOMER_ID     INTEGER
!               LAST_NAME       CHARACTER*(*)
!               FIRST_NAME      CHARACTER*(*)
!               ORDER_DATE      TIME
!               COST            DOUBLE PRECISION
!
!         The order database also has a table of items that have been
!         ordered. The columns of this table are shown below:
!
!            DATAITEMS
!
!               Column Name     Data Type
!               -----------     ---------
!               ITEM_ID         INTEGER
!               ORDER_ID        INTEGER
!               ITEM_NAME       CHARACTER*(*)
!               DESCRIPTION     CHARACTER*(*)
!               PRICE           DOUBLE PRECISION
!
!
!         We'll suppose that the file ORDER_DB.EK contains two segments,
!         the first containing the DATAORDERS table and the second
!         containing the DATAITEMS table.
!
!         If we wanted to insert a new record into the DATAORDERS
!         table in position 1, we'd make the following calls:
!
!            C
!            C     Open the database for write access.
!            C
!                  CALL EKOPW ( 'ORDER_DB.EK', HANDLE )
!
!            C
!            C     Insert a new, empty record into the DATAORDERS
!            C     table at record number 1.  This moves the existing
!            C     records down, so the old record 1 becomes record 2,
!            C     and so on.  Recall that the DATAORDERS table
!            C     is in segment number 1.
!            C
!                  RECNO = 1
!                  SEGNO = 1
!
!                  CALL EKINSR ( HANDLE, SEGNO, RECNO )
!
!            C
!            C     At this point, the new record is empty.  A valid EK
!            C     cannot contain empty records.  We fill in the data
!            C     here.  Data items are filled in one column at a time.
!            C     The order in which the columns are filled in is not
!            C     important.  We use the EKACEx (add column entry)
!            C     routines to fill in column entries.  We'll assume
!            C     that no entries are null.  All entries are scalar,
!            C     so the entry size is 1.
!            C
!                  ISNULL   =  .FALSE.
!                  ESIZE    =  1
!
!            C
!            C     The following variables will contain the data for
!            C     the new record.
!            C
!                  ORDID    =   10011
!                  CUSTID   =   531
!                  LNAME    =   'Scientist'
!                  FNAME    =   'Joe'
!                  ODATE    =   '1995-SEP-20'
!                  COST     =   0.D0
!
!            C
!            C     Note that the names of the routines called
!            C     correspond to the data types of the columns: the
!            C     last letter of the routine name is C, I, or D,
!            C     depending on the data type. Time values are
!            C     converted to ET for storage.
!            C
!                  CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ORDER_ID',
!                 .              SIZE,   ORDID,  ISNULL               )
!
!                  CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'CUSTOMER_ID',
!                 .              SIZE,   CUSTID, ISNULL               )
!
!                  CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'LAST_NAME',
!                 .              SIZE,   LNAME,  ISNULL               )
!
!                  CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'FIRST_NAME',
!                 .              SIZE,   FNAME,  ISNULL               )
!
!
!                  CALL UTC2ET ( ODATE,  ET )
!                  CALL EKACED ( HANDLE, SEGNO,  RECNO, 'ORDER_DATE',
!                 .              SIZE,   ET,     ISNULL               )
!
!                  CALL EKACED ( HANDLE, SEGNO,  RECNO, 'COST',
!                 .              SIZE,   COST,   ISNULL               )
!
!            C
!            C     Close the file to make the update permanent.
!            C
!                  CALL EKCLS ( HANDLE )
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 25-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 09-JAN-2002 (NJB)
!
!        Documentation change: instances of the phrase "fast load"
!        were replaced with "fast write."
!
!-    Beta Version 1.0.0, 19-DEC-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     insert record into EK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               LASTP
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               MP
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NREC
      INTEGER               P
      INTEGER               RECBAS
      INTEGER               RECPTR ( IPSIZE )
      INTEGER               ROOM
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               SIZE

      LOGICAL               ISSHAD

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKINSR' )
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
         CALL CHKOUT ( 'EKINSR' )
         RETURN
      END IF

!
!     Look up the integer metadata page and page base for the segment.
!     Given the base address, we can read the pertinent metadata in
!     one shot.
!
      CALL ZZEKMLOC ( HANDLE,  SEGNO,  MP,  MBASE  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKINSR' )
         RETURN
      END IF

      CALL DASRDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )

!
!     We'll need to know how many columns the segment has in order to
!     compute the size of the record pointer.  The record pointer
!     contains DPTBAS items plus two elements for each column.
!
      NCOLS  =  SEGDSC ( NCIDX )
      SIZE   =  DPTBAS + NCOLS

!
!     We're assuming the record pointer can fit on an integer page.
!     If this is not the case, we've got a bug.
!
      IF ( SIZE .GT. IPSIZE ) THEN

         CALL SETMSG ( 'Record pointer requires # integer words; '  // &
                       'EK software assumes size is <= #.  This is '// &
                       'an EK software bug.  Contact NAIF.'          )
         CALL ERRINT ( '#',  SIZE                                    )
         CALL ERRINT ( '#',  IPSIZE                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'EKINSR'                                      )
         RETURN

      END IF

!
!     Check the number of records already present.  RECNO must not
!     exceed this count by more than 1.
!
      NREC  =  SEGDSC ( NRIDX )

      IF (  ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC+1 )  ) THEN

         CALL SETMSG ( 'Record number = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  RECNO                              )
         CALL ERRINT ( '#',  NREC+1                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                    )
         CALL CHKOUT ( 'EKINSR'                                 )
         RETURN

      END IF

!
!     Find the last integer data page and the last word in use in that
!     page.  If there's enough room, we can store the record pointer
!     in the current page.
!
      LASTP  =  SEGDSC ( LIPIDX )
      LASTW  =  SEGDSC ( LIWIDX )
      ROOM   =  IPSIZE - LASTW

!
!     Initialize the record pointer:  set the record's status and
!     set the data pointers to indicate no data is present.  To
!     determine the status, we must know whether the parent file is
!     shadowed.
!
      CALL CLEARI ( IPSIZE,                RECPTR )
      CALL FILLI  ( UNINIT, IPSIZE-DPTBAS, RECPTR )

      CALL EKSHDW ( HANDLE, ISSHAD )

      IF ( ISSHAD ) THEN
         RECPTR ( STAIDX )  =  NEW
      ELSE
         RECPTR ( STAIDX )  =  OLD
      END IF

!
!     Find a place to write the record pointer.
!
      IF ( SIZE .LE. ROOM ) THEN
!
!        Just write the record pointer into the current integer page.
!
         CALL ZZEKPGBS ( INT, LASTP, BASE )

         RECBAS = BASE + LASTW

         CALL DASUDI ( HANDLE, RECBAS+1, RECBAS+SIZE, RECPTR )

!
!        Update the page's metadata to reflect the addition.  The
!        page gains a link.
!
         CALL DASRDI ( HANDLE, BASE+ILCIDX, BASE+ILCIDX, NLINKS   )
         CALL DASUDI ( HANDLE, BASE+ILCIDX, BASE+ILCIDX, NLINKS+1 )

!
!        The last integer word in use has changed too.
!
         SEGDSC ( LIWIDX )  =  SEGDSC ( LIWIDX ) + SIZE


      ELSE
!
!        Allocate an integer page.
!
         CALL ZZEKAPS  ( HANDLE, SEGDSC, INT, .FALSE., P, RECBAS )

!
!        Write out the record pointer.
!
         CALL DASUDI ( HANDLE, RECBAS+1, RECBAS+SIZE, RECPTR )

!
!        Update the page's metadata to reflect the addition.  The
!        page starts out with one link.
!
         CALL DASUDI ( HANDLE, RECBAS+ILCIDX, RECBAS+ILCIDX, 1 )

!
!        Update the segment's metadata to reflect the addition of a
!        data page.  The last page in use is the one we just wrote to.
!        The last word in use is the last word of the record pointer.
!
         SEGDSC ( LIPIDX )  =  P
         SEGDSC ( LIWIDX )  =  SIZE

      END IF

!
!     Update the segment's metadata to reflect the addition of the
!     new record.  The base address of the record is inserted into
!     the data record tree at index RECNO.  The record count gets
!     incremented.
!
      CALL ZZEKTRIN ( HANDLE,  SEGDSC(RTIDX),  RECNO,  RECBAS )

      SEGDSC ( NRIDX )  =  SEGDSC ( NRIDX ) + 1

!
!     If the segment is shadowed but no backup segment exists yet, we
!     need to create one.  We'll let ZZEKRBCK take care of the details.
!     Note that for data additions, the input argument COLDSC is
!     ignored.
!
      CALL ZZEKRBCK ( 'ADD', HANDLE, SEGDSC, COLDSC, RECNO )

!
!     Write out the updated segment descriptor.
!
      CALL DASUDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )


      CALL CHKOUT ( 'EKINSR' )
      RETURN
      END
