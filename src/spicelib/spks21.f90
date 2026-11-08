!$Procedure SPKS21 ( S/P Kernel, subset, type 21 )

      SUBROUTINE SPKS21 ( HANDLE, BADDR, EADDR, BEGIN, END )

! $ Abstract
!
!     Extract a subset of the data in a SPK segment of type 21
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
!     DAF
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'spk21.inc'

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
!     None.
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
!     The exact structure of a segment of data type 21 is detailed in
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
!     F.T. Krogh         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-OCT-2021 (JDR) (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls (SPKS01
!        -> SPKS21).
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     subset type_21 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               MAXDSZ
      PARAMETER           ( MAXDSZ =  (4 * MAXTRM) + 11 )

!
!     Local variables
!
      DOUBLE PRECISION      DATA     ( MAXDSZ )

      INTEGER               DLSIZE
      INTEGER               MAXDIM
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               OFFE
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               OFFSET
      INTEGER               I


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKS21' )

!
!     Get the number of records in the segment. From that, we can
!     compute
!
!        NDIR      The number of directory epochs.
!
!        OFFE      The offset of the first epoch.
!
!
!     the number of directory epochs.
!
      CALL DAFGDA ( HANDLE, EADDR-1, EADDR, DATA )

      MAXDIM = NINT( DATA(1) )
      NREC   = NINT( DATA(2) )

      NDIR   = NREC / DIRSIZ
      OFFE   = EADDR - NDIR - NREC - 2

!
!     Well, the new segment has already been begun. We just have to
!     decide what to move, and move it (using DAFADA).
!
!     Let's agree right now that speed is not of the greatest
!     importance here. We can probably do this with two passes
!     through the record epochs, and one pass through the records.
!
!        1) Determine the first and last records to be included
!           in the subset.
!
!        2) Move the records.
!
!        3) Write the epochs.
!
!     We can leap through the epochs one last time to get the
!     directory epochs.
!
!     First pass: which records are to be moved?
!
      FIRST = 0
      LAST  = 0

      DO I = 1, NREC

         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA )

         IF (       (  FIRST   .EQ. 0     ) &
              .AND. (  DATA(1) .GE. BEGIN ) ) THEN

             FIRST = I

         END IF

         IF (        ( FIRST   .NE. 0   ) &
              .AND.  ( LAST    .EQ. 0   ) &
              .AND.  ( DATA(1) .GE. END )  ) THEN

            LAST = I

         END IF

      END DO

!
!     Second pass. Move the records.
!
      DLSIZE = ( 4 * MAXDIM ) + 11

      OFFSET = (BADDR - 1) + (FIRST - 1) * DLSIZE

      DO I = FIRST, LAST

         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+DLSIZE, DATA         )
         CALL DAFADA (                                  DATA, DLSIZE )

         OFFSET = OFFSET + DLSIZE

      END DO

!
!     Third pass. Move the epochs.
!
      DO I = FIRST, LAST

         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )

      END DO

!
!     Get every DIRSIZ'th epoch for the directory.
!
      DO I = (FIRST + DIRSIZ - 1), LAST, DIRSIZ

         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )

      END DO

!
!     Add the maximum difference line dimension and the
!     number of records, and we're done.
!
      CALL DAFADA ( DBLE( MAXDIM ), 1 )

      DATA(1) = DBLE ( LAST - FIRST + 1 )

      CALL DAFADA ( DATA, 1 )

      CALL CHKOUT ( 'SPKS21' )
      RETURN
      END
