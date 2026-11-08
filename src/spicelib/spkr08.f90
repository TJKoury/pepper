!$Procedure SPKR08 ( Read SPK record from segment, type 8 )

      SUBROUTINE SPKR08 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 8
!     (equally spaced discrete states, interpolated by Lagrange
!     polynomials).
!
! $ Required_Reading
!
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 8.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is a set of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | number of states (n) |
!                 +----------------------+
!                 | start epoch          |
!                 +----------------------+
!                 | step size            |
!                 +----------------------+
!                 | state 1 (6 elts.)    |
!                 +----------------------+
!                 | state 2 (6 elts.)    |
!                 +----------------------+
!                             .
!                             .
!                             .
!                 +----------------------+
!                 | state n (6 elts.)    |
!                 +----------------------+
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine follows the pattern established in the lower-numbered
!     SPK data type readers of not explicitly performing error
!     diagnoses. Exceptions are listed below nonetheless.
!
!     1)  If the input HANDLE does not designate a loaded SPK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 08,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     3)  If the input ET value is not within the range specified
!         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
!         is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 8 segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 8 ) THEN
!              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
!
! $ Restrictions
!
!     1)  Correctness of inputs must be ensured by the caller of
!         this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
!
!        Data type check was relaxed to enable reading type 12
!        segments.
!
!-    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB)
!
!        In-line comment concerning transpose of state data was
!        removed.
!
!-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_8 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               ODD
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               STATSZ
      PARAMETER           ( STATSZ = 6 )

      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 4 )

      INTEGER               STAIDX
      PARAMETER           ( STAIDX = 4 )

!
!     Local variables
!
      DOUBLE PRECISION      CONTRL ( CTRLSZ  )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP

      INTEGER               BEGIN
      INTEGER               DEGREE
      INTEGER               END
      INTEGER               FIRST
      INTEGER               GRPSIZ
      INTEGER               IC     ( NI )
      INTEGER               LAST
      INTEGER               LOW
      INTEGER               N
      INTEGER               NEAR
      INTEGER               TYPE

!
!     Use discovery check-in.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF


!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      TYPE  = IC( 4 )
      BEGIN = IC( 5 )
      END   = IC( 6 )

!
!     Make sure that this really is a type 8 or type 12 data segment.
!
      IF (  ( TYPE .NE. 08 ) .AND. ( TYPE .NE. 12 )  ) THEN
         CALL CHKIN ( 'SPKR08'                                        )
         CALL SETMSG( 'You are attempting to locate type 8 or type ' // &
                      '12 data in a type # data segment.'             )
         CALL ERRINT( '#',  TYPE                                      )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                           )
         CALL CHKOUT( 'SPKR08'                                        )
         RETURN
      END IF

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN
         CALL CHKIN ( 'SPKR08'                                      )
         CALL SETMSG( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         CALL ERRDP ( '#',  ET                                      )
         CALL ERRDP ( '#',  DC(1)                                   )
         CALL ERRDP ( '#',  DC(2)                                   )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         CALL CHKOUT( 'SPKR08'                                      )
         RETURN

      END IF

!
!     The type 8 segment structure is described by this diagram from
!     the SPK Required Reading:
!
!        +-----------------------+
!        | State 1               |
!        +-----------------------+
!        | State 2               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | State N               |
!        +-----------------------+
!        | Epoch of state 1 (ET) |
!        +-----------------------+
!        | Step size             |
!        +-----------------------+
!        | Polynomial degree     |
!        +-----------------------+
!        | Number of states      |
!        +-----------------------+
!
!
!     We'll need the last four items before we can determine which
!     states make up our output record.
!
!
      CALL DAFGDA ( HANDLE, END-3, END, CONTRL )

      START   =  CONTRL(1)
      STEP    =  CONTRL(2)
      DEGREE  =  NINT ( CONTRL(3) )
      N       =  NINT ( CONTRL(4) )

      GRPSIZ  =  DEGREE + 1

!
!     We'll now select the set of states that define the interpolating
!     polynomials.  The cases of odd and even GRPSIZ are handled
!     separately.
!
      IF ( ODD(GRPSIZ) ) THEN
!
!        Find the index of the state whose epoch is closest to the
!        input epoch.  Find the first and last indices in the record
!        of the (GRPSIZ-1)/2 states on either side of this central
!        state.
!
         NEAR   =  NINT ( ( ET - START ) / STEP )  +  1

         FIRST  =  MIN (  MAX( 1,  NEAR - (DEGREE/2) ),  N-DEGREE )
         LAST   =  FIRST  +  DEGREE


      ELSE
!
!        Find the index of the last state whose epoch is less than or
!        equal to that of the input epoch.  Find the first and last
!        indices in the record of the set of GRPSIZ consecutive states
!        having this state as the (GRPSIZ/2)th one.
!
         LOW    =  INT ( ( ET - START ) / STEP )  +  1

         FIRST  =  MIN (  MAX( 1,  LOW - DEGREE/2 ),  N-DEGREE )
         LAST   =  FIRST  +  DEGREE

      END IF

!
!     Put the size of the group of states, the epoch of the first
!     state in the record, and the step size into the output record.
!
      RECORD(1) =  GRPSIZ
      RECORD(2) =  START  +  (FIRST-1)*STEP
      RECORD(3) =  STEP

!
!     Read the states.
!
      CALL DAFGDA ( HANDLE, &
                    BEGIN + (FIRST-1)*STATSZ, &
                    BEGIN +      LAST*STATSZ  -  1, &
                    RECORD(STAIDX)                  )

      RETURN
      END
