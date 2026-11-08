!$Procedure SPKS05 ( S/P Kernel, subset, type 5 )

      SUBROUTINE SPKS05 ( HANDLE, BADDR, EADDR, BEGIN, END )

! $ Abstract
!
!     Extract a subset of the data in an SPK segment of type 5
!     into a new segment.
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
!     None.
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
!     It transfers a subset of a type 05 SPK data segment to
!     a properly initialized segment of a second SPK file.
!
!     The exact structure of a segment of data type 05 is described
!     in the section on type 05 in the SPK Required Reading.
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
!     J. Diaz del Rio    (ODC Space)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     subset type_5 SPK segment
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
      DOUBLE PRECISION      GM

      INTEGER               I
      INTEGER               NDIR
      INTEGER               NREC
      INTEGER               OFFE
      INTEGER               OFFSET
      INTEGER               REC      ( 2 )


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS05' )
      END IF

!
!     Get the number of records in the segment. While we're at it,
!     get the GM of the central body as well (it's adjacent to NREC)
!     since we'll need it anyway.
!
      CALL DAFGDA ( HANDLE, EADDR-1, EADDR, DATA )

      NREC = INT ( DATA(2) )
      GM   = DATA(1)

!
!     From the number of records, we can compute
!
!        NDIR      The number of directory epochs.
!
!        OFFE      The offset of the first epoch.
!
      NDIR = NREC / 100
      OFFE = EADDR - NDIR - NREC - 2

!
!     Examine the epochs in forward order, looking for the first
!     epoch greater than or equal to END (or the final epoch,
!     whichever comes first). This epoch corresponds to the last
!     state to be transferred.
!
      REC(2) = 1
      CALL DAFGDA ( HANDLE, OFFE+REC(2), OFFE+REC(2), DATA )

      DO WHILE ( REC(2) .LT. NREC  .AND.  DATA(1) .LT. END )
         REC(2) = REC(2) + 1
         CALL DAFGDA ( HANDLE, OFFE+REC(2), OFFE+REC(2), DATA )
      END DO

!
!     Now examine them in reverse order, looking for the first
!     epoch less than or equal to BEGIN (or the initial epoch,
!     whichever comes first). This epoch corresponds to the first
!     state to be transferred.
!
      REC(1) = NREC
      CALL DAFGDA ( HANDLE, OFFE+REC(1), OFFE+REC(1), DATA )

      DO WHILE ( REC(1) .GT. 1  .AND.  DATA(1) .GT. BEGIN )
         REC(1) = REC(1) - 1
         CALL DAFGDA ( HANDLE, OFFE+REC(1), OFFE+REC(1), DATA )
      END DO

!
!     Copy states REC(1) through REC(2) to the output file.
!
      DO I = REC(1), REC(2)
         OFFSET = BADDR  -  1  +  (I - 1) * 6

         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+6, DATA    )
         CALL DAFADA (                             DATA, 6 )
      END DO

!
!     Copy epochs REC(1) through REC(2) to the output file.
!
      DO I = REC(1), REC(2)
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO

!
!     Put every 100'th epoch into the directory.
!
      DO I = REC(1)+99, REC(2), 100
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO

!
!     Store the GM of the central body and the number of records
!     to end the segment.
!
      CALL DAFADA ( GM,                           1 )
      CALL DAFADA ( DBLE ( REC(2) - REC(1) + 1 ), 1 )

      CALL CHKOUT ( 'SPKS05' )
      RETURN
      END
