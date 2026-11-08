!$Procedure      ZZCKSPK ( SPK or CK )

      SUBROUTINE ZZCKSPK ( HANDLE, CKSPK )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine determines whether or not a DAF file attached to
!     the supplied handle is an SPK, CK or unknown file.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      PRIVATE
!
! $ Declarations

      IMPLICIT NONE
      INTEGER               HANDLE
      CHARACTER*(*)         CKSPK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   the handle of a DAF file open for read access.
!     CKSPK      O   the type of the DAF file (SPK,CK or ?)
!
! $ Detailed_Input
!
!     HANDLE     is the handle of a DAF file open for read.
!
! $ Detailed_Output
!
!     CKSPK      is a string containing one of the following 3 values
!                'SPK', 'CK' or '?'
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     This routine examines the first segment of a DAF that is
!     a candidate for being an SPK or CK and returns a diagnosis
!     of the type of the file.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 03-DEC-1999 (WLT)
!
!-&

      LOGICAL               RETURN
      LOGICAL               FAILED


!
!     Local parameters
!
!
!     The following parameters point to the various slots in the
!     integer portion of the DAF descriptor where the values are
!     located.
!
      INTEGER               BODID
      PARAMETER           ( BODID  =  1 )

      INTEGER               CENID
      PARAMETER           ( CENID  = BODID  + 1 )

      INTEGER               SPKFRM
      PARAMETER           ( SPKFRM = CENID  + 1 )

      INTEGER               SPKTYP
      PARAMETER           ( SPKTYP = SPKFRM + 1 )

      INTEGER               START
      PARAMETER           ( START  = SPKTYP + 1 )

      INTEGER               FINISH
      PARAMETER           ( FINISH = START  + 1 )

      INTEGER               CKRATE
      PARAMETER           ( CKRATE = SPKTYP     )

      INTEGER               CKTYPE
      PARAMETER           ( CKTYPE = SPKFRM )

!
!     These parameters give the number of integer and double precision
!     components of the descriptor for SPK and CK files.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )
!
!     The size of a summary.
!
      INTEGER               SUMSIZ
      PARAMETER           ( SUMSIZ = ND + ((NI+1)/2) )


      DOUBLE PRECISION      CHCKTM
      DOUBLE PRECISION      DC    ( ND )
      DOUBLE PRECISION      LASTDP
      DOUBLE PRECISION      FRSTTM
      DOUBLE PRECISION      SUM   ( SUMSIZ )
      DOUBLE PRECISION      TIMES ( 2 )

      INTEGER               ANGVEL
      INTEGER               FIRST
      INTEGER               FROM
      INTEGER               IC    ( NI )
      INTEGER               LAST
      INTEGER               NCK2
      INTEGER               NSPK
      INTEGER               SIZE
      INTEGER               THISND
      INTEGER               THISNI
      INTEGER               TO
      INTEGER               TYPE

      LOGICAL               CK2OK
      LOGICAL               FOUND
      LOGICAL               SPKOK

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCKSPK' )
!
!     Make sure the values of ND and NI associated with this file
!     have the correct values.
!
      CALL DAFHSF ( HANDLE, THISND, THISNI )

      IF (     THISND .NE. ND &
          .OR. THISNI .NE. NI ) THEN
         CKSPK = '?'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF

!
!     We've got the correct values for ND and NI, examine the descriptor
!     for the first array.
!
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )

      IF ( FAILED () ) THEN
         CKSPK = '?'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF

!
!     If we don't find any segments, we don't have a clue about
!     the file type.
!
      IF ( .NOT. FOUND ) THEN

         CKSPK = '?'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN

      END IF
!
!     Unpack the summary record.
!
      CALL DAFGS ( SUM                 )
      CALL DAFUS ( SUM, ND, NI, DC, IC )

!
!     Look at the slot where the angular velocity flag would
!     be located if this is a CK file.
!
      ANGVEL = IC(CKRATE)
      TYPE   = IC(CKTYPE)

!
!     Test 1. The value of ANGVEL may do the trick
!     right at the start.
!
      IF ( ANGVEL .EQ. 0 ) THEN
         CKSPK   = 'CK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF

      IF ( ANGVEL .GT. 1 ) THEN
         CKSPK   = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF

!
!     Test 2. If this is an SPK file, it has a type 01 segment.
!     See if this is something orbiting the solar system
!     barycenter.
!
      IF ( IC(CENID) .EQ. 0 ) THEN
         CKSPK   = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF

!
!     Test 3. This is the super test.  Compute the size of the
!     segment and fetch the last d.p. from the segment.
!

      FIRST  = IC(START)
      LAST   = IC(FINISH)
      SIZE   = LAST  - FIRST +  1
!
!     Check the size of the array to see if it has any chance
!     of being an SPK and if it does get the number of MDA records.
!
      CALL ZZSIZEOK ( SIZE-1, 72, 100, 0, SPKOK, NSPK )

      IF ( .NOT. SPKOK ) THEN
         CKSPK = 'CK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF


      CALL DAFGDA ( HANDLE, LAST, LAST, LASTDP )
!
!     See if the last number in the file is the allowed number of
!     MDA records.  If not, this must be a CK segment.
!
      IF ( LASTDP .NE. DBLE(NSPK) ) THEN
         CKSPK   = 'CK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
!
!     If we are still here, the last d.p. in the segment matches the
!     expected number of MDA records.  If the potential CK type is
!     not 2, we must have an SPK file.
!
      IF ( TYPE .NE. 2 ) THEN
         CKSPK = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
!
!     We are getting down to the nitty gritty here. See if the
!     size is compatible with a type 02 C-kernel.
!
      CALL ZZSIZEOK ( SIZE, 10, 100, 1, CK2OK, NCK2 )

      IF ( .NOT. CK2OK ) THEN
         CKSPK = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF

!
!     So much for being nice. We need to examine the structure of the
!     actual data in the segment.  There are two cases to consider:
!     when there is 1 or fewer type 02 CK directory records and when
!     there is more than 1.  Note that to get to this point there must
!     be at least 1 directory value if this is a CK type 02 segment.
!     (To see this check the sizes when ZZSIZEOK returns TRUE for
!     both type 1 SPK and type 02 CK. The only such sizes in which
!     there the number CK type 02 directory values is one or fewer
!     are SIZE = 1081, 1441, and 1801 which correspond to (NSPK,NCK2) =
!     (15,108), (20,144), (25, 180).  In all of these cases there is
!     exactly 1 ck type 02 directory value.)
!
      IF ( NCK2 .LT. 201 ) THEN
!
!        Recall that MDA record contains its stop time as the first
!        entry of the record.  These epochs show up duplicated in the
!        epochs portion of the segment.
!
!        If this is a type 01 SPK segment, there are no directory
!        records and the first epoch shows up in the slot NSPK before
!        the last slot of the segment.  If it is a type 02 CK segment
!        the last stop tick shows up in this slot.  We need to look
!        at this value to see what's up.
!
         CALL DAFGDA ( HANDLE, LAST-NSPK, LAST-NSPK, FRSTTM )

!
!        Now (under the assumption that we have an SPK segment) look
!        up the epoch from the last MDA record--- the NSPK'th
!        record.  This epoch must be greater than the first epoch
!        in the array of epochs.
         FROM = FIRST + (NSPK-1)*71
         TO   = FROM
         CALL DAFGDA ( HANDLE, FROM, TO, CHCKTM )

!
!        If this is a type 02 segment.  The value we just picked out
!        will come from the array of stop ticks.  The array of stop
!        ticks is non-decreasing so:
!
         IF ( CHCKTM .GT. FRSTTM ) THEN
            CKSPK = 'SPK'
         ELSE
            CKSPK = 'CK'
         END IF

      ELSE
!
!        In this case there are at least 2 directory records if we
!        have a CK.  We read the last potential tick value and the
!        first potential directory value..  Note that the last potential
!        stop tick must be greater than the first potential directory
!        record.
!
         FROM = LAST - (NCK2-1)/100
         TO   = FROM + 1

         CALL DAFGDA ( HANDLE, FROM, TO, TIMES )
!
!        If we happen to have a TYPE 01 SPK segment we've just
!        read two consecutive values from the epochs sub-array of the
!        segment. Here's a sketch of why this is so:
!
!          The number of directory records for a CK type 02 segment is
!          (NCK2-1)/100  which is the same as SIZE/1001.
!
!          The number of directory records for an SPK type 01 segment is
!          (NSPK-1)/100  which is the same as SIZE/7201.
!
!          The number of stop ticks for type 02 CK is NCK2 ~ SIZE/10
!
!          The number of epochs for a type 01 SPK is  NSPK ~ SIZE/72
!
!        so NSPK directories < NCK2 directories < NCK2 directories + 1
!        < NSPK + NSPK directories < NCK2.  Consequently, the
!        two values just read are either the last stop tick and the
!        first CK directory value or two consecutive epochs.
!        In the first case TIMES(1) > TIMES(2), in the later case
!        we have TIMES(1) < TIMES(2)
!
         IF ( TIMES(1) .GT. TIMES(2) ) THEN
            CKSPK = 'CK'
         ELSE
            CKSPK = 'SPK'
         END IF

      END IF

      CALL CHKOUT ( 'ZZCKSPK' )
      RETURN

      END
