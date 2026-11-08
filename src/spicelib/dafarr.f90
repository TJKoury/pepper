!$Procedure DAFARR ( DAF, add reserved records )

      SUBROUTINE DAFARR ( HANDLE, RESV )

! $ Abstract
!
!     Add a specified number of reserved records to a Double Precision
!     Array File (DAF).
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               RESV

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF file opened for writing.
!     RESV       I   Number of records to reserve.
!
! $ Detailed_Input
!
!     HANDLE   is the handle associated with a DAF file that has
!              been opened with write access.
!
!     RESV     is the number of reserved records to be added
!              to the specified file.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If RESV is less than one, the file is not changed.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     Normally, the reserved records in an array file are reserved
!     when the file is created. However, it may occasionally become
!     necessary to add reserved records---when the contents of one
!     file are appended to another, for example. (In this case, any
!     information in the reserved records of either file should
!     be included in the resulting file.)
!
!     The new reserved records are appended to the old ones. The new
!     reserved records are also NULL filled.
!
! $ Examples
!
!     In the following call to DAFARR, assume that HANDLE is the file
!     handle for a DAF file that has been opened for write access, and
!     that the DAF file already contains 12 reserved records (located in
!     records 2-13 of the physical file).
!
!        CALL DAFARR ( HANDLE, 7 )
!
!     After this call, the DAF file attached to HANDLE will contain 19
!     reserved records. The new reserved records are located in
!     records 14-20 of the physical file.
!
! $ Restrictions
!
!     1)  This routine will only add reserved records to DAFs open for
!         write. These files are implicitly of the native binary file
!         format.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.6.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.5.0, 16-NOV-2001 (FST)
!
!        Added a call to DAFSIH to prevent this routine from
!        attempting to write to non-native binary file formats.
!        This will provide a more useful error diagnostic with
!        little impact on performance.
!
!-    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG)
!
!        Added code to write NULL filled records to the file for the
!        new reserved records.
!
!-    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG)
!
!        Added a missing call to CHKOUT before the RETURN statement in
!        the test
!
!              IF ( RESV .LT. 1 ) THEN
!                 RETURN
!              END IF
!
!-    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG)
!
!        $Detailed_Input and $Examples section of the header were
!        modified.
!
!        Added calls to the FORTRAN intrinsic functions INT and
!        DBLE in the code that updates the summary record.
!
!        Modified an IF loop to make logic clearer.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     add DAF reserved records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG)
!
!        Added code to write NULL filled records to the file for the
!        new reserved records.
!
!-    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG)
!
!        Added a missing call to CHKOUT before the RETURN statement in
!        the test
!
!              IF ( RESV .LT. 1 ) THEN
!                 RETURN
!              END IF
!
!-    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG)
!
!        $Detailed_Input section was modified. References to any
!        specific routines by name as a method for opening a DAF file
!        for write access were removed. The assumption is that a person
!        using DAF files would already know something about opening and
!        closing the files.
!
!        $Examples section was modified. References to any specific
!        routines by name as a method for opening a DAF file for writing
!        were removed, and the example was reworded in such a way that
!        the use of the subroutine remained clear.
!
!        Added calls to the INT intrinsic function to convert a DP
!        number to an integer before assigning it to NEXT, which is an
!        integer variable. Also added calls to INT in IF statements
!        where comparisons were made between DP numbers and INTEGERs,
!        when integral values were actually being compared.
!
!        Added calls to the intrinsic function DBLE to convert an
!        integer, RESV, into a DP number when doing some arithmetic.
!
!        Took an ELSE IF clause out of the initial IF return  ELSE
!        check in END IF at the beginning of the routine. Replaced the
!        code:
!
!              IF ( RETURN () ) THEN
!                 RETURN
!
!              ELSE IF ( RESV .LT. 1 ) THEN
!                 RETURN
!
!              ELSE
!                 CALL CHKIN ( 'DAFARR' )
!              END IF
!
!        with the equivalent code:
!
!              IF ( RETURN () ) THEN
!                 RETURN
!              ELSE
!                 CALL CHKIN ( 'DAFARR' )
!              END IF
!
!        C
!        C     Check to see if the number of records to be reserved is
!        C     less than one. If so, just return without changing
!        C     anything.
!        C
!              IF ( RESV .LT. 1 ) THEN
!                 RETURN
!              END IF
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!     IFNLEN      is the length of a DAF internal file name.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60       )

!
!     WPR         is the maximum number of double precision numbers
!                 (words) per record.
!
!     MAXD,       are the maximum number of double precision
!     MAXI,       numbers, integers, and characters, respectively,
!     MAXC        per record, not including space reserved for
!                 control information (3 dp numbers are reserved).
!                 There are two integers per double precision word,
!                 and eight characters per word.
!
      INTEGER               WPR
      PARAMETER           ( WPR    = 128      )

      INTEGER               MAXD
      PARAMETER           ( MAXD   = WPR  - 3 )

      INTEGER               MAXI
      PARAMETER           ( MAXI   = MAXD * 2 )

      INTEGER               MAXC
      PARAMETER           ( MAXC   = MAXD * 8 )

!
!     Local variables
!
      CHARACTER*(MAXC)      CREC
      CHARACTER*(IFNLEN)    IFNAME

      DOUBLE PRECISION      DC      ( MAXD )
      DOUBLE PRECISION      DREC    ( WPR  )
      DOUBLE PRECISION      SUM     ( MAXD )

      INTEGER               BEGBLK
      INTEGER               BWARD
      INTEGER               ENDBLK
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               INCR
      INTEGER               IC      ( MAXI )
      INTEGER               ND
      INTEGER               NEXT
      INTEGER               NI
      INTEGER               RECNO
      INTEGER               WORD

      LOGICAL               FOUND

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFARR' )
      END IF
!
!
!     Check to see if the number of records to be reserved is less than
!     one. If so, just return without changing anything.
!
      IF ( RESV .LT. 1 ) THEN
         CALL CHKOUT ( 'DAFARR' )
         RETURN
      END IF

!
!     Before proceeding any further, check that the DAF associated
!     with HANDLE is available for write access.
!
      CALL DAFSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFARR' )
         RETURN
      END IF

!
!     Get the contents of the file record. If it fails, then just check
!     out and return, as an appropriate error message should have
!     already been set.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

      IF ( FAILED () ) THEN

         CALL CHKOUT ( 'DAFARR' )
         RETURN

      END IF

!
!     Okay, here's the plan. We are just going to move records
!     in the direction of the end of the file, starting
!     with the last record in the file and ending with the first
!     summary record.
!
!     After everything has been moved, the initial and final
!     addresses of all the arrays have to be incremented by the
!     same amount: the number of words per record (128) times
!     the number of new records.
!
      INCR = WPR * RESV

!
!     Before we do that, however, we should write some bogus records
!     to the end of the file, to make sure we don't run out of space
!     later on. If this doesn't work, we will leave the logical
!     contents of the file uncorrupted (although it may get larger).
!
      CALL DAFARW ( FREE, RECNO, WORD )

      DO I = 1, RESV

         CALL DAFWDR ( HANDLE, RECNO + I, DREC )

      END DO

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFARR' )
         RETURN

      END IF

!
!     Records will be moved in `blocks', where each block contains
!
!        -- a summary record
!
!        -- a name record
!
!        -- one or more data records
!
!     The first block to be moved (that is, the last block in
!     the file) lies between the final summary record (BWARD) and
!     whatever record contains the first free address in the file.
!
      BEGBLK = BWARD
      CALL DAFARW ( FREE, ENDBLK, WORD )

      DO WHILE ( ( BEGBLK .GT. 0 )  .AND.  ( .NOT. FAILED() ) )

!
!        Move the data records first.
!
         DO RECNO = ENDBLK,  BEGBLK + 2,  -1

            CALL DAFRDR ( HANDLE, RECNO,        1, WPR, DREC, FOUND )
            CALL DAFWDR ( HANDLE, RECNO + RESV,         DREC        )

         END DO

!
!        Then the name record.
!
         RECNO = BEGBLK + 1
         CALL DAFRCR ( HANDLE, RECNO,        CREC )
         CALL DAFWCR ( HANDLE, RECNO + RESV, CREC )

!
!        Finally, the summary record.
!
!        To find the beginning of the next block, look at the backward
!        pointer from the summary record of the current block.
!
!        Be sure to adjust the forward and backward pointers;
!        otherwise, we won't be able to find the summaries again.
!
         RECNO = BEGBLK
         CALL DAFRDR ( HANDLE, RECNO, 1, WPR, DREC, FOUND )

         NEXT = INT ( DREC(2) )

         IF ( INT ( DREC(1) ) .GT. 0 ) THEN
            DREC(1) = DREC(1) + DBLE ( RESV )
         END IF

         IF ( INT ( DREC(2) ) .GT. 0 ) THEN
            DREC(2) = DREC(2) + DBLE ( RESV )
         END IF

         CALL DAFWDR ( HANDLE, RECNO + RESV, DREC )

!
!        The next block ends just before the current block begins.
!
         ENDBLK = BEGBLK  - 1
         BEGBLK = NEXT

      END DO

!
!     Rewrite the file record, to reflect the new organization of
!     the file.
!
      FWARD = FWARD + RESV
      BWARD = BWARD + RESV
      FREE  = FREE  + INCR

      CALL DAFWFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

!
!     Get the summary for each array, increment the addresses (stored
!     in the final two integer components), and replace the summary.
!
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )

      DO WHILE ( FOUND  .AND.  ( .NOT. FAILED() ) )

         CALL DAFGS  ( SUM                 )
         CALL DAFUS  ( SUM, ND, NI, DC, IC )

         IC(NI-1) = IC(NI-1) + INCR
         IC(NI  ) = IC(NI  ) + INCR

         CALL DAFPS ( ND, NI, DC, IC, SUM )
         CALL DAFWS (                 SUM )

         CALL DAFFNA ( FOUND )

      END DO
!
!     Write NULL filled records to the reserved record area.
!
      DO I = 1, MAXC
         CREC(I:I) = CHAR(0)
      END DO

      I = FWARD-RESV
      DO RECNO = I, I + RESV - 1
         CALL DAFWCR ( HANDLE, RECNO, CREC )
      END DO

      CALL CHKOUT ( 'DAFARR' )
      RETURN
      END
