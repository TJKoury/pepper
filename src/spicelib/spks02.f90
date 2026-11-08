!$Procedure SPKS02 ( S/P Kernel, subset, type 2 )

      SUBROUTINE SPKS02 ( HANDLE, BADDR, EADDR, BEGIN, END )

! $ Abstract
!
!     Extract a subset of the data in a SPK segment of type 2
!     into a new segment.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               BADDR
      INTEGER               EADDR
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of source segment.
!     BADDR      I   Beginning address of source segment.
!     EADDR      I   Ending address of source segment.
!     BEGIN      I   Beginning (initial epoch) of subset.
!     END        I   End (final epoch) of subset.
!
! $ Detailed_Input
!
!     HANDLE,
!     BADDR,
!     EADDR    are the file handle assigned to a SPK file, and the
!              beginning and ending addresses of a segment within
!              the file. Together they determine a complete set of
!              ephemeris data, from which a subset is to be
!              extracted.
!
!     BEGIN,
!     END      are the initial and final epochs (ephemeris time)
!              of the subset to be extracted.
!
! $ Detailed_Output
!
!     None. This routine writes data to the SPK file currently
!     open for write access.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while reading data from the source SPK
!         file, the error is signaled by a routine in the call tree of
!         this routine.
!
!     2)  If an error occurs while writing data to the output SPK file,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     The exact structure of a segment of data type 2 is detailed in
!     the SPK Required Reading file.
!
! $ Examples
!
!     None.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.2, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.1.1, 30-DEC-2013 (NJB)
!
!        Enhanced header documentation.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
!
!        SPK02 was removed from the $Required_Reading section of the
!        header. The information in the SPK02 Required Reading file
!        is now part of the SPK Required Reading file.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
!
!-&


! $ Index_Entries
!
!     subset type_2 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      DATA    ( 50 )
      DOUBLE PRECISION      INIT
      DOUBLE PRECISION      INTLEN

      INTEGER               RECSIZ
      INTEGER               NREC
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               REMAIN
      INTEGER               ADDR
      INTEGER               MOVE


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS02' )
      END IF

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which records to extract by comparing the input
!     epochs with the initial time of the segment and the length of the
!     interval covered by each record.  These final two constants are
!     located at the end of the segment, along with the size of each
!     logical record and the total number of records.
!
      CALL DAFGDA ( HANDLE, EADDR-3, EADDR, DATA )

      INIT   =      DATA( 1 )
      INTLEN =      DATA( 2 )
      RECSIZ = INT( DATA( 3 ) )
      NREC   = INT( DATA( 4 ) )

      FIRST = INT( (BEGIN - INIT) / INTLEN ) + 1
      FIRST = MIN( FIRST, NREC )

      LAST = INT( (END - INIT) / INTLEN ) + 1
      LAST = MIN( LAST, NREC )

!
!     The number of records to be moved.
!
      NREC = LAST - FIRST + 1

!
!     We're going to move the data in chunks of 50 d.p. words.  Compute
!     the number of words left to move, the address of the beginning
!     of the records to move, and the number to move this time.
!
      REMAIN = NREC * RECSIZ
      ADDR   = BADDR + ( FIRST - 1 )*RECSIZ
      MOVE   = MIN( 50, REMAIN )

      DO WHILE ( REMAIN .GT. 0 )

         CALL DAFGDA ( HANDLE, ADDR, ADDR + MOVE - 1, DATA )
         CALL DAFADA ( DATA, MOVE )
         REMAIN = REMAIN - MOVE
         ADDR   = ADDR   + MOVE
         MOVE   = MIN( 50, REMAIN )

      END DO

!
!     That's all the records we have to move. But there are still four
!     final numbers left to write:
!
!        1)  The initial time for the polynomials (INIT).
!        2)  The time interval length for each polynomial (INTLEN).
!        3)  The record size (RECSIZ).
!        4)  The number of records (NREC).
!
!     INIT and NREC will probably be different for the new segment (in
!     fact, NREC has already been changed), the other two will not.
!
      INIT = INIT + ( FIRST - 1 )*INTLEN

      DATA( 1 ) = INIT
      DATA( 2 ) = INTLEN
      DATA( 3 ) = DBLE( RECSIZ )
      DATA( 4 ) = DBLE( NREC   )
      CALL DAFADA ( DATA, 4 )

      CALL CHKOUT ( 'SPKS02' )
      RETURN
      END
