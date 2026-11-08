!$Procedure SPKS08 ( S/P Kernel, subset, type 8 )

      SUBROUTINE SPKS08 ( HANDLE, BADDR, EADDR, BEGIN, END )

! $ Abstract
!
!     Extract a subset of the data in an SPK segment of type 8
!     into a new segment.
!
! $ Required_Reading
!
!     SPK
!     DAF
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
!     HANDLE     I   Handle of file containing source segment.
!     BADDR      I   Beginning address in file of source segment.
!     EADDR      I   Ending address in file of source segment.
!     BEGIN      I   Beginning (initial epoch) of subset.
!     END        I   End (final epoch) of subset.
!
! $ Detailed_Input
!
!     HANDLE,
!     BADDR,
!     EADDR    are the file handle assigned to an SPK file, and the
!              beginning and ending addresses of a segment within
!              that file. Together they determine a complete set of
!              ephemeris data, from which a subset is to be
!              extracted.
!
!     BEGIN,
!     END      are the initial and final epochs (ephemeris time)
!              of the subset.
!
!              The first epoch for which there will be ephemeris
!              data in the new segment will be the greatest time
!              in the source segment that is less than or equal
!              to BEGIN.
!
!              The last epoch for which there will be ephemeris
!              data in the new segment will be the smallest time
!              in the source segment that is greater than or equal
!              to END.
!
! $ Detailed_Output
!
!     See $Files section.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine relies on the caller to ensure that the
!         interval [BEGIN, END] is contained in the coverage
!         interval of the segment.
!
!     2)  If BEGIN > END, no data is written to the target file.
!
! $ Files
!
!     Data is extracted from the file connected to the input
!     handle, and written to the current DAF open for writing.
!
!     The segment descriptor and summary must already have been written
!     prior to calling this routine. The segment must be ended
!     external to this routine.
!
! $ Particulars
!
!     This routine is intended solely for use as a utility by the
!     routine SPKSUB.
!
!     It transfers a subset of a type 08 SPK data segment to
!     a properly initialized segment of a second SPK file.
!
!     The exact structure of a segment of data type 08 is described
!     in the section on type 08 in the SPK Required Reading.
!
! $ Examples
!
!     This routine is intended only for use as a utility by SPKSUB.
!     To use this routine successfully, you must:
!
!        Open the SPK file from which to extract data.
!        Locate the segment from which data should be extracted.
!
!        Open the SPK file to which this data should be written.
!        Begin a new segment (array).
!        Write the summary information for the array.
!
!        Call this routine to extract the appropriate data from the
!        SPK open for read.
!
!        End the array to which this routine writes data.
!
!     Much of this procedure is carried out by the routine SPKSUB. The
!     examples of that routine illustrate more fully the process
!     described above.
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
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 2.0.0, 20-AUG-1994 (NJB)
!
!        Bug fix: START value for output segment has been corrected.
!        Bug fix: Sufficient bracketing states are now included in the
!        output segment to ensure duplication of states given by source
!        segment.
!
!-    SPICELIB Version 1.0.0, 08-AUG-1993 (NJB) (JML) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     subset type_8 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      DATA     ( 6 )
      DOUBLE PRECISION      RATIO
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP

      INTEGER               DEGREE
      INTEGER               I
      INTEGER               NREC
      INTEGER               OFFSET
      INTEGER               REC      ( 2 )


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS08' )
      END IF

!
!     Look up the following items:
!
!        -- The start epoch
!        -- The step size
!        -- The polynomial degree
!        -- The number of records in the segment
!
      CALL DAFGDA ( HANDLE, EADDR-3, EADDR, DATA )

      START  =        DATA(1)
      STEP   =        DATA(2)
      DEGREE = NINT ( DATA(3) )
      NREC   = NINT ( DATA(4) )

!
!     See whether there's any work to do; return immediately if not.
!
      IF (      (  END   .LT.   BEGIN                    ) &
           .OR. (  END   .LT.   START                    ) &
           .OR. (  BEGIN .GT. ( START + (NREC-1)*STEP )  )   ) THEN

         CALL CHKOUT ( 'SPKS08' )
         RETURN

      END IF

!
!     Compute the index of the state having the last epoch
!     epoch less than or equal to BEGIN (or the initial epoch,
!     whichever comes last). This epoch corresponds to the first
!     state to be transferred.
!

      RATIO  =   MAX (  0.D0,   ( BEGIN - START ) / STEP  )

      REC(1) =   1  +  MIN (  INT(RATIO),  NREC-1 )

!
!     Make sure that there are DEGREE/2 additional states to the left
!     of the one having index REC(1), if possible.  If not, take as
!     many states as we can.
!
      REC(1)  =   MAX (  1,   REC(1) - DEGREE/2   )

!
!     Make sure that REC(1) is small enough so that there are are at
!     least DEGREE+1 states in the segment.
!
      REC(1)  =   MIN (  REC(1),  NREC - DEGREE  )

!
!     Now compute the index of the state having the first epoch greater
!     than or equal to END (or the final epoch, whichever comes first).
!     This epoch corresponds to the last state to be transferred.
!
      RATIO  =  ( END - START ) / STEP


      IF ( RATIO .EQ. AINT(RATIO) ) THEN
         REC(2)  =   1   +   MIN(  INT(RATIO),      NREC-1  )
      ELSE
         REC(2)  =   1   +   MIN(  INT(RATIO) + 1,  NREC-1  )
      END IF

!
!     Make sure that there are DEGREE/2 additional states to the right
!     of the one having index REC(2), if possible.  If not, take as
!     many states as we can.
!
      REC(2)  =   MIN (  NREC,   REC(2) + DEGREE/2   )

!
!     Make sure that REC(2) is large enough so that there are are at
!     least DEGREE+1 states in the segment.
!
      REC(2)  =   MAX (  REC(2),  DEGREE + 1 )

!
!     Copy states REC(1) through REC(2) to the output file.
!
      DO  I = REC(1), REC(2)
         OFFSET = BADDR  -  1  +  (I - 1) * 6

         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+6, DATA    )
         CALL DAFADA (                             DATA, 6 )
      END DO

!
!     Store the start time, step size, polynomial degree and the
!     number of records to end the segment.
!
      CALL DAFADA (  START + ( REC(1) - 1 ) * STEP,    1  )
      CALL DAFADA (  STEP,                             1  )
      CALL DAFADA (  DBLE ( DEGREE ),                  1  )
      CALL DAFADA (  DBLE ( REC(2) - REC(1) + 1 ),     1  )

      CALL CHKOUT ( 'SPKS08' )
      RETURN
      END
