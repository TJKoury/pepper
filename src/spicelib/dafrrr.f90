!$Procedure DAFRRR ( DAF, remove reserved records )

      SUBROUTINE DAFRRR ( HANDLE, RESV )

! $ Abstract
!
!     Remove a specified number of reserved records from a Double
!     Precision Array File (DAF).
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
!     HANDLE     I   DAF, opened for writing.
!     RESV       I   Number of records to remove.
!
! $ Detailed_Input
!
!     HANDLE   is the handle associated with a DAF that has been
!              opened with write access.
!
!     RESV     is the number of reserved records to be removed
!              from the specified file.
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
!     2)  If RESV is greater than the number of reserved records in the
!         file, all of the reserved records are removed.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     Normally, the reserved records in an array file are reserved
!     when the file is created. However, it may occasionally become
!     desirable to remove reserved records---when their contents are
!     significantly reduced, for example.
!
!     The records nearest the end of the file are removed. Note
!     that the physical size of the file is not reduced when reserved
!     records are removed.
!
! $ Examples
!
!     For the following call to DAFRRR, assume that HANDLE is the file
!     handle for a DAF file that has been opened for write access, and
!     that the DAF file already contains 12 reserved records (located in
!     records 2-13 of the physical file).
!
!        CALL DAFRRR ( HANDLE, 7 )
!
!     After this call to DAFRRR, the number of reserved records has been
!     decreased by 7, leaving only the first five of the original
!     reserved records, physical records 2-6.
!
! $ Restrictions
!
!     1)  This routine will only remove reserve records from DAFs open
!         for write. These files are implicitly of the native binary
!         file format.
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
!-    SPICELIB Version 1.3.0, 02-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 16-NOV-2001 (FST)
!
!        Added a call to DAFSIH to prevent this routine from
!        attempting to write to non-native binary file formats.
!        This will provide a more useful error diagnostic with
!        little impact on performance.
!
!-    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG)
!
!        $Detailed_Input and $Examples section of the header were
!        modified.
!
!        Added calls to the FORTRAN intrinsic functions INT and
!        DBLE in the code that updates the summary record.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 18-JUL-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     remove DAF reserved records
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG)
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
!        number to an integer before assigning it to NEXT or ENDBLK,
!        both of which are integer variables. Also added calls to INT
!        in IF statements where comparisons were made between DP numbers
!        and INTEGERs, when integral values were actually being
!        compared.
!
!        Added calls to the intrinsic function DBLE to convert an
!        integer, REMOVE, into a DP number when doing some arithmetic.
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
!     WPR         is the maximum number of double precision
!                 numbers per record.  WPR stands for words
!                 per record.
!
      INTEGER               WPR
      PARAMETER           ( WPR    = 128      )

!
!     MAXD,       are the maximum number of double precision
!     MAXI,       numbers, integers, and characters, respectively,
!     MAXC        not including space reserved for control information.
!
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
      INTEGER               DECR
      INTEGER               ENDBLK
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               IC      ( MAXI )
      INTEGER               ND
      INTEGER               NEXT
      INTEGER               NI
      INTEGER               RECNO
      INTEGER               REMOVE
      INTEGER               WORD

      LOGICAL               FOUND

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRRR' )
      END IF

!
!     Before proceeding any further, check that the DAF associated
!     with HANDLE is available for write access.
!
      CALL DAFSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFRRR' )
         RETURN
      END IF

!
!     Get the contents of the file record. If it fails, then just check
!     out and return, as an appropriate error message should have
!     already been set.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

      IF ( FAILED () ) THEN

         CALL CHKOUT ( 'DAFRRR' )
         RETURN

      END IF

!
!     Don't remove more than the current number of reserved records!
!     If there are none, check out.
!
      REMOVE = MIN ( RESV, FWARD - 2 )

      IF ( REMOVE .LT. 1 ) THEN

         CALL CHKOUT ( 'DAFRRR' )
         RETURN

      END IF

!
!     Okay, here's the plan. We are just going to move records
!     forward, starting with the first summary record in the file
!     and ending with the last data record.
!
!     After everything has been moved, the initial and final
!     addresses of all the arrays have to be decremented by the
!     same amount: the number of words per record (128) times
!     the number of records removed.
!
      DECR = WPR * REMOVE

!
!     Records will be moved in `blocks', where each block contains
!
!        -- a summary record
!
!        -- a name record
!
!        -- one or more data records
!
!     Most blocks lie between one summary record and the next.
!     The final block lies between the final summary record and
!     whatever data record contains the first free address.
!
!     BEGBLK is initially the first summary record location.
!
      BEGBLK = FWARD

      DO WHILE ( ( BEGBLK .GT. 0 )  .AND.  ( .NOT. FAILED() ) )

!
!        Move the summary record first. The location of the next
!        summary record determines the end of this block, and the
!        beginning of the next.
!
!        Be sure to adjust the forward and backward pointers;
!        otherwise, we won't be able to find the summaries again.
!
         RECNO = BEGBLK
         CALL DAFRDR ( HANDLE, RECNO, 1, WPR, DREC, FOUND )

         IF ( INT ( DREC(1) ) .GT. 0 ) THEN
            ENDBLK = INT ( DREC(1) ) - 1
            NEXT   = INT ( DREC(1) )
         ELSE
            CALL DAFARW ( FREE, ENDBLK, WORD )
            NEXT = 0
         END IF

         IF ( INT ( DREC(1) ) .GT. 0 ) THEN
            DREC(1) = DREC(1) - DBLE ( REMOVE )
         END IF

         IF ( INT ( DREC(2) ) .GT. 0 ) THEN
            DREC(2) = DREC(2) - DBLE ( REMOVE )
         END IF

         CALL DAFWDR ( HANDLE, RECNO - REMOVE, DREC )

!
!        Then the name record.
!
         RECNO = BEGBLK + 1
         CALL DAFRCR ( HANDLE, RECNO,          CREC )
         CALL DAFWCR ( HANDLE, RECNO - REMOVE, CREC )

!
!        Finally, the data records.
!
         DO RECNO = BEGBLK + 2, ENDBLK
            CALL DAFRDR ( HANDLE, RECNO,          1, WPR, DREC, FOUND )
            CALL DAFWDR ( HANDLE, RECNO - REMOVE,         DREC        )
         END DO

!
!        Start the next block, if one exists.
!
         BEGBLK = NEXT

      END DO

!
!     Rewrite the file record, to reflect the new organization of
!     the file.
!
      FWARD = FWARD - REMOVE
      BWARD = BWARD - REMOVE
      FREE  = FREE  - DECR

      CALL DAFWFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

!
!     Get the summary for each array, decrement the addresses (stored
!     in the final two integer components), and replace the summary.
!
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )

      DO WHILE ( FOUND  .AND.  ( .NOT. FAILED() ) )

         CALL DAFGS  ( SUM                 )
         CALL DAFUS  ( SUM, ND, NI, DC, IC )

         IC(NI-1) = IC(NI-1) - DECR
         IC(NI  ) = IC(NI  ) - DECR

         CALL DAFPS ( ND, NI, DC, IC, SUM )
         CALL DAFWS (                 SUM )

         CALL DAFFNA ( FOUND )

      END DO

      CALL CHKOUT ( 'DAFRRR' )
      RETURN
      END
