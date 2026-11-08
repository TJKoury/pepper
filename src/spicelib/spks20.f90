!$Procedure SPKS20 ( S/P Kernel, subset, type 20 )

      SUBROUTINE SPKS20 ( HANDLE, BADDR, EADDR, BEGIN, END )

! $ Abstract
!
!     Extract a subset of the data in a SPK segment of type 20
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
!     The exact structure of a segment of data type 20 is detailed in
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
!     R.E. Thurman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 23-DEC-2013 (NJB) (RET)
!
!-&


! $ Index_Entries
!
!     subset type_20 SPK segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

!
!     Local variables
!
      DOUBLE PRECISION      BTIME
      DOUBLE PRECISION      DATA   ( BUFSIZ )
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      TSCALE
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INITFR
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      INTRVL
      DOUBLE PRECISION      SUBBEG
      DOUBLE PRECISION      SUBIFR
      DOUBLE PRECISION      SUBIJD

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
      END IF

      CALL CHKIN ( 'SPKS20' )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the integer and fractional parts of the initial time of the first
!     record's coverage interval, and the length of the interval
!     covered by each record. These constants are located at the end of
!     the segment, along with the size of each logical record and the
!     total number of records.
!
!     For convenience, we'll fetch the segment's distance and time
!     scales in the same call.
!
      CALL DAFGDA ( HANDLE, EADDR-6, EADDR, DATA )

      DSCALE =      DATA( 1 )
      TSCALE =      DATA( 2 )
      INITJD =      DATA( 3 )
      INITFR =      DATA( 4 )
      INTLEN =      DATA( 5 )
      RECSIZ = INT( DATA( 6 ) )
      NREC   = INT( DATA( 7 ) )

      BTIME  = ( ( INITJD - J2000() ) + INITFR ) * SPD()
      INTRVL = INTLEN * SPD()

      FIRST = INT( (BEGIN - BTIME) / INTRVL ) + 1
      FIRST = MAX( 1,  MIN( FIRST, NREC )  )

      LAST  = INT( (END   - BTIME) / INTRVL ) + 1
      LAST  = MAX( 1,  MIN( LAST,  NREC )  )

!
!     The number of records to be moved.
!
      NREC = LAST - FIRST + 1

!
!     We're going to move the data in chunks of BUFSIZ d.p. words.
!     Compute the number of words left to move, the address of the
!     beginning of the records to move, and the number to move this
!     time.
!
      REMAIN = NREC * RECSIZ
      ADDR   = BADDR + ( FIRST - 1 )*RECSIZ
      MOVE   = MIN( BUFSIZ, REMAIN )

      DO WHILE ( REMAIN .GT. 0 )

         CALL DAFGDA ( HANDLE, ADDR, ADDR + MOVE - 1, DATA )
         CALL DAFADA ( DATA, MOVE )
         REMAIN = REMAIN - MOVE
         ADDR   = ADDR   + MOVE
         MOVE   = MIN( BUFSIZ, REMAIN )

      END DO

!
!     That's all the records we have to move. But there are still seven
!     final numbers left to write:
!
!        1)  The distance scale (DSCALE).
!        2)  The time scale (TSCALE).
!        3)  The initial integer Julian date of the start time of the
!            first record.
!        4)  The fractional part of the state time of the first
!            record.
!        5)  The time interval length for each polynomial in days
!            (INTLEN).
!        6)  The record size (RECSIZ).
!        7)  The number of records (NREC).
!
!
!
!     Let SUBBEG be the subset begin time expressed as a TDB Julian
!     date.
!
      SUBBEG =  J2000()  +  (  BTIME + ( FIRST - 1 )*INTRVL  ) / SPD()

      SUBIJD =  AINT( SUBBEG )
      SUBIFR =  SUBBEG - SUBIJD

      DATA( 1 ) = DSCALE
      DATA( 2 ) = TSCALE
      DATA( 3 ) = SUBIJD
      DATA( 4 ) = SUBIFR
      DATA( 5 ) = INTLEN
      DATA( 6 ) = DBLE( RECSIZ )
      DATA( 7 ) = DBLE( NREC   )

      CALL DAFADA ( DATA, 7 )

      CALL CHKOUT ( 'SPKS20' )
      RETURN
      END
