!$Procedure ZZSWFXFM ( Private, get switch frame transformation )

      SUBROUTINE ZZSWFXFM ( INFRM, ET, NDIM, XFORM, OUTFRM, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return switch frame state or position transformation matrix.
!
! $ Required_Reading
!
!     CK
!     FRAMES
!     NAIF_IDS
!     TIME
!
! $ Keywords
!
!     None.
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'frmtyp.inc'
      INCLUDE              'zzswtchf.inc'
      INCLUDE              'zzctr.inc'

      INTEGER               INFRM
      DOUBLE PRECISION      ET
      INTEGER               NDIM
      DOUBLE PRECISION      XFORM ( NDIM, NDIM )
      INTEGER               OUTFRM
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     INFRM      I   Frame ID code of switch frame.
!     ET         I   Time, expressed as seconds past J2000 TDB.
!     NDIM       I   Dimension: 3 or 6.
!     XFORM      O   Transformation from switch to selected base frame.
!     OUTFRM     O   ID code of selected base frame.
!     FOUND      O   Flag indicating whether transformation was found.
!
! $ Detailed_Input
!
!     INFRM       is the frame ID code of switch frame for which
!                 the transformation to a base frame at time ET is
!                 sought.
!
!     ET          is a frame orientation evaluation epoch, expressed
!                 as seconds past J2000 TDB.
!
!     NDIM        is the common row and column dimension of the output
!                 matrix: 6 or 3. When NDIM is set to 6, a 6x6 state
!                 transformation matrix is returned; when NDIM is set
!                 to 3, a 3x3 position transformation matrix is
!                 returned.
!
! $ Detailed_Output
!
!     XFORM       is a matrix that transforms vectors of dimension NDIM
!                 from the switch frame designated by INFRM to the
!                 frame designated by OUTFRM, at time ET.
!
!                 By definition, a switch frame is aligned with a
!                 selected base frame at time ET. However, depending on
!                 the class of the selected base frame, OUTFRM may
!                 be set to that base frame, the base frame at ET of
!                 that base, or the frame J2000. See the description of
!                 OUTFRM below.
!
!                 XFORM is defined if and only if FOUND is .TRUE.
!
!
!     OUTFRM      is the frame ID code of a frame selected by this
!                 routine, which may be a base frame of the switch
!                 frame designated by INFRM, or which may be another
!                 frame, selected as described below. The output matrix
!                 XFORM transforms vectors of dimension NDIM from the
!                 switch frame designated by INFRM to the frame
!                 designated by OUTFRM.
!
!                 OUTFRM is defined if and only if FOUND is .TRUE.
!
!                 OUTFRM is selected as follows:
!
!                    - Base frames are obtained from the kernel
!                      variable
!
!                         FRAME_< INFRM >_ALIGNED_WITH
!
!                      Base frames are listed in order of ascending
!                      priority. A given frame may occur at multiple
!                      indices of the base frame array.
!
!                    - If time intervals are included in the switch
!                      frame specification, the highest priority
!                      base frame for which the associated time
!                      interval contains ET, and in which data are
!                      available at ET, is chosen.
!
!                      If time intervals are not provided, the highest
!                      priority base frame for which data are available
!                      at ET is chosen.
!
!                      Data availability may depend on NDIM. This is
!                      because 6x6 state transformations that rely on
!                      CK data require angular velocity to be present;
!                      3x3 position transformations don't.
!
!                      If no base frame providing data at ET is found
!                      by the process described above, FOUND is set to
!                      .FALSE. and OUTFRM is undefined.
!
!                    - If the process succeeds in finding a base frame
!                      providing data at ET, OUTFRM is set according to
!                      the class of the base frame:
!
!                         > Base is class INERTL: OUTFRM is set to
!                           the ID code of J2000.
!
!                         > Base is class PCK: OUTFRM is set to the ID
!                           code of J2000.
!
!                         > Base is class CK: OUTFRM is set to the ID
!                           code of the CK frame's base frame at time
!                           ET. This is the base returned by FRMGET for
!                           ET, if NDIM is 6, or the base returned by
!                           ROTGET at ET if NDIM is 3.
!
!                         > Base is class TK: OUTFRM is set to the ID
!                           code of the TK frame's base frame. This is
!                           the frame specified by the
!                           TKFRAME_<ID>_RELATIVE keyword of the TK
!                           frame's specification.
!
!                         > Base is class DYN: OUTFRM is set to the ID
!                           code of the base frame.
!
!                         > Base is class SWTCH: OUTFRM is set to the
!                           ID code of the base frame.
!
!
!     FOUND       is a logical flag indicating whether a frame
!                 transformation was found at time ET. XFORM and OUTFRM
!                 are valid if and only if FOUND is .TRUE.
!
! $ Parameters
!
!     See the include file zzswtchf.inc for SPICE-private parameters
!     defining sizes of buffers used by the switch frame subsystem.
!
! $ Exceptions
!
!     1)  If NDIM is a value other than 6 or 3, the error
!         SPICE(BADDIMENSION) will be signaled.
!
!     2)  If the switch frame designated by INFRM has more than MAXBAS
!         associated base frames, the error is signaled by a routine in
!         the call tree of this routine.
!
!     3)  If a frame specification cannot be found for a base frame of
!         the frame designated by INFRM, the error is signaled by a
!         routine in the call tree of this routine.
!
!     4)  If an error is found in the specification of the switch frame
!         designated by INFRM, the error is signaled by a routine in
!         the call tree of this routine.
!
!     5)  If an error occurs while computing the transformation from
!         the frame designated by INFRM to that designated by OUTFRM,
!         the error is signaled by a routine in the call tree of this
!         routine.

! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!         - A switch frame specification, normally provided by a
!           frame kernel.
!
!         - Frame specifications for all base frames. Normally these
!           specifications are provided by frame kernels.
!
!     The following data may be needed:
!
!         - A leapseconds kernel
!
!         - SCLK kernel parameters and data for each CK base frame.
!           This information is normally provided by SCLK kernels.
!
!         - CK data for each CK base frame. These data are provided
!           by C-kernels.
!
!         - PCK data for each PCK base frame.
!
!         - Any kernels required to specify any dynamic base frame.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     The switch frame subsystem does not participate in fake
!     recursion, hence there are no shadow routines corresponding to
!     ZZSWFXFM and ZZSWFFET.
!
!     The only frame orientation evaluations performed in the process
!     of computing the output transformation XFORM are those that
!     don't require recursive use of the frame subsystem. The frame
!     classes in this category are
!
!        Inertial
!        PCK
!        CK
!        TK
!
!     For dynamic and switch base frames, no orientation is computed.
!     Instead, the identity matrix is returned.
!
! $ Examples
!
!     None. See the routines FRMGET and ROTGET for examples of usage.
!
! $ Restrictions
!
!     1)  This routine is SPICE-private. User applications must not
!         call it.
!
!     2)  To improve efficiency, this routine buffers switch frame
!         specifications. The buffers are emptied after any update
!         to the kernel pool. Since the process of parsing, and
!         buffering parameters provided by, switch frame specifications
!         is rather slow, applications using switch frames should
!         avoid high-frequency kernel pool updates.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB) (BVS)
!
!-&

! $ Index_Entries
!
!     return switch frame transformation and base frame
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

      INTEGER               LSTLED

!
!     Local parameters
!

!
!     ID code of the J2000 frame:
!
      INTEGER               J2CODE
      PARAMETER           ( J2CODE = 1 )

!
!     Size of linked list pool:
!
!     The add-only hash pool for frame IDs is singly linked.
!
      INTEGER               FPLSIZ
      PARAMETER           ( FPLSIZ = MAXFRM - LBSNGL + 1 )

!
!     Dimensions
!
      INTEGER               D3
      PARAMETER           ( D3 = 3 )

      INTEGER               D6
      PARAMETER           ( D6 = 6 )

!
!     Matrix sizes.
!
      INTEGER               SIZE33
      PARAMETER           ( SIZE33 =  9 )

      INTEGER               SIZE66
      PARAMETER           ( SIZE66 = 36 )

!
!     Local variables
!

!
!     Overview of frame storage data structures
!     =========================================
!
!     This routine uses an add-only hash to index switch frames
!     known to this routine. The hash maps input frame ID codes to
!     head nodes of collision lists stored in the array HDFRAM.
!     The collision lists are stored in the singly linked list pool
!     FRPOOL.
!
!     Each node of a collision list is the index in the array FIDLST of
!     a switch frame ID. The array FIDLST has the following associated,
!     parallel arrays:
!
!        BASBEG       Start indices in the base frame attribute arrays
!                     of values associated with specific switch frames.
!                     For example, BASLST( BASBEG(FRAMAT) ) is the
!                     index of the first base frame associated with
!                     the switch frame having ID FIDLST(FRAMAT).
!
!        BASCNT       Base count for each switch frame
!
!        USETIM       Indicates whether time intervals are associated
!                     with that frame's base frames
!
!        BINARY       Indicates whether time intervals are eligible for
!                     binary search. To be eligible, the intervals
!                     must have start times in ascending order and
!                     must be disjoint or have singleton overlap.
!
!     Base frame attributes are stored in a set of parallel arrays.
!     Attributes for bases of a given switch frame are listed in
!     contiguous elements. The arrays and their contents are:
!
!        BASLST       Base frame ID codes. The base frames of a given
!                     switch frame are listed in ascending priority
!                     order in consecutive elements of BASLST.
!
!        STARTS       Base frame interval start times, expressed as
!                     seconds past J2000 TDB. Valid for all bases of
!                     a given switch frame, or unused for that frame.
!
!        STOPS        Base frame interval start times, expressed as
!                     seconds past J2000 TDB. Valid for all bases of
!                     a given switch frame, or unused for that frame.
!
!        CLSSES       Base frame classes.
!
!        CLSIDS       Base frame class IDs.
!
!     Each base frame list for a given switch frame is ordered
!     according to ascending priority.
!
      DOUBLE PRECISION      STARTS ( MAXBAS )
      DOUBLE PRECISION      STOPS  ( MAXBAS )

      INTEGER               BASBEG ( MAXFRM )
      INTEGER               BASCNT ( MAXFRM )
      INTEGER               BASLST ( MAXBAS )
      INTEGER               CLSIDS ( MAXBAS )
      INTEGER               CLSSES ( MAXBAS )
      INTEGER               FIDLST ( MAXFRM )
      INTEGER               FRPOOL ( LBSNGL : MAXFRM )
      INTEGER               HDFRAM ( MAXFRM )

      LOGICAL               BINARY ( MAXFRM )
      LOGICAL               USETIM ( MAXFRM )

!
!     Other declarations
!
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      XIDENT ( 6, 6 )

      INTEGER               BASIDX
      INTEGER               FRAMAT
      INTEGER               FREE
      INTEGER               HEAD
      INTEGER               I
      INTEGER               J
      INTEGER               LB
      INTEGER               LSTBEG
      INTEGER               POLCTR ( CTRSIZ )
      INTEGER               PRVAT
      INTEGER               PRVFRM

      LOGICAL               PASS1
      LOGICAL               SAMFRM
      LOGICAL               TIMEOK
      LOGICAL               UPDATE

!
!     Saved variables
!
      SAVE                  BASBEG
      SAVE                  BASCNT
      SAVE                  BASLST
      SAVE                  CLSIDS
      SAVE                  CLSSES
      SAVE                  BINARY
      SAVE                  FIDLST
      SAVE                  FREE
      SAVE                  FRPOOL
      SAVE                  HDFRAM
      SAVE                  PASS1
      SAVE                  POLCTR
      SAVE                  PRVAT
      SAVE                  PRVFRM
      SAVE                  STARTS
      SAVE                  STOPS
      SAVE                  USETIM
      SAVE                  XIDENT

!
!     Initial values
!
      DATA                  BASBEG / MAXFRM * 0       /
      DATA                  BASCNT / MAXFRM * 0       /
      DATA                  BASLST / MAXBAS * 0       /
      DATA                  CLSIDS / MAXBAS * 0       /
      DATA                  CLSSES / MAXBAS * 0       /
      DATA                  BINARY / MAXFRM * .FALSE. /
      DATA                  FIDLST / MAXFRM * 0       /
      DATA                  FREE   / 1                /
      DATA                  FRPOOL / FPLSIZ * 0       /
      DATA                  HDFRAM / MAXFRM * 0       /
      DATA                  PASS1  / .TRUE.           /
      DATA                  PRVAT  / 0                /
      DATA                  PRVFRM / 0                /
      DATA                  STARTS / MAXBAS * 0.D0    /
      DATA                  STOPS  / MAXBAS * 0.D0    /
      DATA                  USETIM / MAXFRM * .FALSE. /

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSWFXFM' )

!
!     No frame found yet.
!
      FOUND = .FALSE.

      IF ( PASS1 ) THEN
!
!        Initialize pool data structures.
!
         CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                         FREE,   PRVAT,  PRVFRM, SAMFRM  )
!
!        Initialize the local pool counter.
!
         CALL ZZCTRUIN ( POLCTR )
!
!        Note: no FAILED() check is needed after the two calls above.
!
!        Create a 6x6 identity matrix.
!
         CALL FILLD( 0.D0, SIZE66, XIDENT )

         DO I = 1, 6
            XIDENT(I,I) = 1.D0
         END DO

         PASS1 = .FALSE.

      END IF

!
!     Check the transformation dimension.
!
      IF (  ( NDIM .NE. D3 ) .AND. ( NDIM .NE. D6 )  ) THEN

         CALL SETMSG ( 'Transformation dimension must ' &
         //            'be 3 or 6 but was #.'           )
         CALL ERRINT ( '#', NDIM                        )
         CALL SIGERR ( 'SPICE(BADDIMENSION)'            )
         CALL CHKOUT ( 'ZZSWFXFM'                       )
         RETURN

      END IF

!
!     Reinitialize local database if the kernel pool has been
!     updated. The call below syncs POLCTR.
!
      CALL ZZPCTRCK ( POLCTR, UPDATE )

      IF ( UPDATE ) THEN
!
!        Initialize local data structures.
!
         CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                         FREE,   PRVAT,  PRVFRM, SAMFRM  )

!
!        SAMFRM is set to .FALSE. and PRVFRM is set to 0 at this point.
!
      ELSE
!
!        SAMFRM indicates that INFRM is the same switch frame we saw on
!        the previous call, no kernel pool update has occurred since
!        that call, and that the call was successful. We don't save the
!        previous switch frame information for unsuccessful calls: for
!        those, we set PRVFRM to zero so that it won't match any valid
!        frame ID.
!
         SAMFRM = ( INFRM .NE. 0 ) .AND. ( INFRM .EQ. PRVFRM )

      END IF

      IF ( SAMFRM ) THEN
!
!        We can use information saved from the previous call. FRAMAT is
!        the index of the switch frame ID in the switch frame ID array.
!
         FRAMAT = PRVAT

      ELSE
!
!        Look up the input frame.
!
         CALL ZZHSICHK ( HDFRAM, FRPOOL, FIDLST, INFRM, FRAMAT )

      END IF

      IF ( FRAMAT .EQ. 0 ) THEN
!
!        This frame is unknown to the switch frame subsystem. Try to
!        look up the frame's specification from the kernel pool.
!        ZZSWFFET checks and returns the frame specification.
!
         CALL ZZSWFFET ( INFRM,  HDFRAM, FRPOOL, FIDLST, BASBEG, &
                         FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                         CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

         IF (  FAILED()  .OR.  ( FRAMAT .EQ. 0 )  ) THEN
!
!           We were unable to obtain information for the requested
!           frame. Initialize all information about the previous frame.
!           For safety, initialize the local database.
!
            CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                            FREE,   PRVAT,  PRVFRM, SAMFRM  )

            CALL CHKOUT ( 'ZZSWFXFM' )
            RETURN

         END IF

      END IF

!
!     At this point, FRAMAT is set.
!
!     Below, HEAD is the common index in each parallel array of base
!     frame data of the first item for a given switch frame. BASIDX is
!     the current index. The index obtained by adding a switch frame's
!     base frame count to HEAD is the successor of the last index of
!     base frame data for that switch frame.
!
      HEAD = BASBEG( FRAMAT )

!
!     At this point, the frame's information is present in the
!     local database. FRAMAT is the index of the frame in the hash
!     data structure. HEAD is the start index of the base frame data
!     for the input switch frame.
!
!     Search for the highest-priority frame that provides data
!     for the request time.
!
!     For safety, reset FOUND.
!
      FOUND = .FALSE.

      IF ( BINARY(FRAMAT) ) THEN
!
!        We'll do a binary search. Suppress follow-on linear searching
!        and frame data lookup by default: when BASIDX is zero, the
!        linear search and data fetch loop won't execute. We'll update
!        BASIDX if an interval is found.
!
         BASIDX = 0

!
!        Find the last interval, if any, that contains ET. Start by
!        finding the last start time that's less than or equal to ET.
!
!        Here we rely on the assumption that start and stop times for
!        each frame are stored in increasing time order.
!
         LSTBEG = LSTLED( ET, BASCNT(FRAMAT), STARTS(HEAD) )

!
!        If LSTBEG is zero, then ET is strictly less than all of the
!        start times, in which case there's no match.
!
         IF ( LSTBEG .GT. 0 ) THEN
!
!           ET could belong to the interval having start time stored at
!           relative index LSTBEG, which is equivalent to the base
!           frame array index HEAD+LSTBEG-1.
!
            IF (  ET .LE. STOPS( HEAD+LSTBEG-1 )  ) THEN
!
!              This interval contains ET. Prepare to fetch data for
!              the base frame corresponding to this interval.
!
               BASIDX = HEAD + LSTBEG - 1

            END IF

         END IF

      ELSE
!
!        Prepare for a linear search and fetching base frame data.
!
         BASIDX = HEAD + BASCNT(FRAMAT) - 1

      END IF

!
!     Search the base list in order of decreasing priority. The search
!     will terminate once we've examined the base frame at index LB,
!     if not before.
!
      LB = HEAD

      DO WHILE (  ( BASIDX .GE. LB ) .AND. ( .NOT. FOUND )  )

         IF (  USETIM( FRAMAT )  ) THEN

            TIMEOK =       ( ET .GE. STARTS(BASIDX) ) &
                     .AND. ( ET .LE. STOPS (BASIDX) )
         ELSE
            TIMEOK = .FALSE.
         END IF

         IF (  TIMEOK .OR. ( .NOT. USETIM(FRAMAT) )  ) THEN
!
!           This base frame is applicable.
!
            IF ( CLSSES(BASIDX) .EQ. CK ) THEN
!
!              We can't know whether there are CK data for the request
!              time without looking up the data, so that's what we do.
!              If we find data, we return the transformation to the CK
!              frame's base frame and the frame ID of the CK frame's
!              base frame.
!
               IF ( NDIM .EQ. D6 ) THEN

                  CALL CKFXFM ( CLSIDS(BASIDX), ET, &
                                XFORM, OUTFRM,  FOUND )
               ELSE

                  CALL CKFROT ( CLSIDS(BASIDX), ET, &
                                XFORM, OUTFRM,  FOUND )
               END IF

               IF ( FAILED() ) THEN

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                                  FREE,   PRVAT,  PRVFRM, SAMFRM  )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

!
!              If we got this far and we're in binary search mode, we
!              won't find a result, except possibly when ET is equal to
!              the interval start time.
!
               IF ( BINARY(FRAMAT) .AND. ( .NOT. FOUND ) ) THEN

                  IF ( ET .GT. STARTS(BASIDX) ) THEN
!
!                    The next lowest-indexed interval doesn't cover ET,
!                    so no data can be found to satisfy this request.
!
                     PRVFRM = 0
                     PRVAT  = 0

                     CALL CHKOUT ( 'ZZSWFXFM' )
                     RETURN

                  END IF

               END IF
!
!              FOUND is set. If we did find data, XFORM and OUTFRM
!              are set. If FOUND is .FALSE., we'll look at the
!              remaining lower-priority bases, if any, if we're not in
!              binary search mode. If we're in binary search mode,
!              we'll look at the next lower-priority base only if ET is
!              equal to the interval start time.
!

            ELSE IF ( CLSSES(BASIDX) .EQ. TK ) THEN

               CALL TKFRAM ( CLSIDS(BASIDX), RMAT, OUTFRM, FOUND )

               IF ( FAILED() ) THEN

                  FOUND  = .FALSE.

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                                  FREE,   PRVAT,  PRVFRM, SAMFRM  )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

               IF ( NDIM .EQ. D6 ) THEN

                  DO I = 1,3
                     DO J = 1,3
                        XFORM(I,  J  ) = RMAT(I,J)
                        XFORM(I+3,J+3) = RMAT(I,J)
                        XFORM(I+3,J  ) = 0.0D0
                        XFORM(I,  J+3) = 0.0D0
                     END DO
                  END DO

               ELSE
                  CALL MOVED( RMAT, SIZE33, XFORM )
               END IF
!
!              OUTFRM and FOUND have been set by TKFRAM.
!

            ELSE IF ( CLSSES(BASIDX) .EQ. PCK ) THEN

               IF ( NDIM .EQ. D6 ) THEN

                  CALL TISBOD ( 'J2000', CLSIDS(BASIDX), ET, TSIPM )
                  CALL INVSTM ( TSIPM, XFORM )
               ELSE
                  CALL TIPBOD ( 'J2000', CLSIDS(BASIDX), ET, RMAT  )
                  CALL XPOSE  ( RMAT,  XFORM )
               END IF

               IF ( FAILED() ) THEN

                  FOUND  = .FALSE.

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                                  FREE,   PRVAT,  PRVFRM, SAMFRM  )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

               OUTFRM = J2CODE
               FOUND  = .TRUE.


            ELSE IF ( CLSSES(BASIDX) .EQ. INERTL ) THEN

               CALL IRFROT ( BASLST(BASIDX), J2CODE, RMAT )

               IF ( FAILED() ) THEN

                  FOUND  = .FALSE.

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                                  FREE,   PRVAT,  PRVFRM, SAMFRM )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

               IF ( NDIM .EQ. D6 ) THEN

                  DO I = 1,3
                     DO J = 1,3
                        XFORM(I,  J  ) = RMAT(I,J)
                        XFORM(I+3,J+3) = RMAT(I,J)
                        XFORM(I+3,J  ) = 0.0D0
                        XFORM(I,  J+3) = 0.0D0
                     END DO
                  END DO

               ELSE
                  CALL MOVED( RMAT, SIZE33, XFORM )
               END IF

               OUTFRM = J2CODE
               FOUND  = .TRUE.

            ELSE
!
!              For other non-CK frames---switch and dynamic---we just
!              return the base frame and the identity matrix.
!
               IF ( NDIM .EQ. D6 ) THEN

                  CALL MOVED ( XIDENT, SIZE66, XFORM )
               ELSE
                  CALL IDENT ( XFORM )
               END IF

               OUTFRM = BASLST( BASIDX )
               FOUND  = .TRUE.

            END IF

         END IF

!
!        If we got this far and we're in binary search mode,
!        we've already found a result. We'll exit the loop at
!        the next loop condition test.
!
!        Check the next lower priority base frame and interval.
!
         BASIDX = BASIDX - 1

      END DO

!
!     FOUND is set.
!
!     We either found data, or we ran out of base frames to try.
!     If we found data, the outputs XFORM and OUTFRM are set.
!
      IF ( FOUND ) THEN
!
!        Save values for re-use.
!
         PRVFRM = INFRM
         PRVAT  = FRAMAT

      ELSE
!
!        Ensure the saved values won't be re-used.
!
         PRVFRM = 0

      END IF

      CALL CHKOUT ( 'ZZSWFXFM' )
      RETURN
      END
