!$Procedure CKR05 ( Read CK record from segment, type 05 )

      SUBROUTINE CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, &
                         RECORD, FOUND                       )

! $ Abstract
!
!     Read a single CK data record from a segment of type 05
!     (MEX/Rosetta Attitude file interpolation).
!
! $ Required_Reading
!
!     CK
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ck05.inc'
      INCLUDE 'ckparam.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD   ( * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     SCLKDP     I   Pointing request time.
!     TOL        I   Lookup tolerance.
!     NEEDAV     I   Angular velocity flag.
!     RECORD     O   Data record.
!     FOUND      O   Flag indicating whether record was found.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a CK segment of type 05.
!
!     SCLKDP   is an encoded spacecraft clock time indicating
!              the epoch for which pointing is desired.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock.
!
!              When SCLKDP falls within the bounds of one of the
!              interpolation intervals then the tolerance has no
!              effect because pointing will be returned at the
!              request time.
!
!              However, if the request time is not in one of the
!              intervals, then the tolerance is used to determine
!              if pointing at one of the interval endpoints should
!              be returned.
!
!     NEEDAV   is .TRUE. if angular velocity is requested.
!
! $ Detailed_Output
!
!     RECORD   is a set of data from the specified segment which,
!              when evaluated at epoch SCLKDP, will give the
!              attitude and angular velocity of some body, relative
!              to the reference frame indicated by DESCR.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | evaluation epoch     |
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
!                 +----------------------+
!                 | nominal SCLK rate    |
!                 +----------------------+
!                 | packet 1             |
!                 +----------------------+
!                 | packet 2             |
!                 +----------------------+
!                             .
!                             .
!                             .
!                 +----------------------+
!                 | packet n             |
!                 +----------------------+
!                 | epochs 1--n          |
!                 +----------------------+
!
!              The packet size is a function of the subtype code.
!              All packets in a record have the same size.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine follows the pattern established in the lower-numbered
!     CK data type readers of not explicitly performing error
!     diagnoses. Exceptions are listed below nonetheless.
!
!     1)  If the input HANDLE does not designate a loaded CK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 05,
!         the error SPICE(WRONGCKTYPE) is signaled.
!
!     3)  If the input SCLK value is not within the range specified
!         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
!         is signaled.
!
!     4)  If the window size is non-positive or greater than the
!         maximum allowed value, the error SPICE(INVALIDVALUE) is
!         signaled.
!
!     5)  If the window size is not compatible with the segment
!         subtype, the error SPICE(INVALIDVALUE) is signaled.
!
!     6)  If the segment subtype is not recognized, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     7)  If the tolerance is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the CK Required Reading file for a description of the
!     structure of a data type 05 segment.
!
! $ Examples
!
!     The data returned by the CKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the CKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!     The search performed here does not mimic the behavior of the CK
!     reader APIs CKGP and CKGPAV, which continue searching when an
!     applicable segment doesn't satisfy a pointing request. See the CK
!     Required reading for details.
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV )
!           CALL CKSNS ( HANDLE, DESCR,  SEGID, SFND   )
!
!           IF ( .NOT. SFND ) THEN
!              [Handle case of pointing not being found]
!           END IF
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 05 ) THEN
!
!              CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                RECORD, FOUND                       )
!
!              IF ( .NOT. FOUND ) THEN
!                 [Handle case of pointing not being found]
!              END IF
!
!              [Look at the RECORD data]
!                  .
!                  .
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
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 06-JUL-2021 (NJB) (JDR)
!
!        Corrected code example: removed comment character preceding
!        CKBSS call. Added note regarding difference between this
!        search and those performed by the CK reader APIs CKGP and
!        CKGPAV.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 27-JAN-2014 (NJB)
!
!        Increased MAXDEG to 23 for compatibility with CK type 6.
!
!-    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_5 CK segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPMAX

      INTEGER               LSTLED
      INTEGER               LSTLTD

      LOGICAL               ODD
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               PBUFSZ
      PARAMETER           ( PBUFSZ = DIRSIZ + 1 )

      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 5 )

      INTEGER               PARSIZ
      PARAMETER           ( PARSIZ = 4 )

      INTEGER               SBUFSZ
      PARAMETER           ( SBUFSZ = DIRSIZ + 3 )

!
!     Maximum polynomial degree:
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 23 )

!
!     Local variables
!
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      HEPOCH
      DOUBLE PRECISION      LEPOCH
      DOUBLE PRECISION      NSTART
      DOUBLE PRECISION      NNSTRT
      DOUBLE PRECISION      PBUFFR ( PBUFSZ )
      DOUBLE PRECISION      PREVN
      DOUBLE PRECISION      PREVNN
      DOUBLE PRECISION      PREVS
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SBUFFR ( SBUFSZ )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      T

      INTEGER               PBEGIX
      INTEGER               BEGIN
      INTEGER               BUFBAS
      INTEGER               DIRBAS
      INTEGER               END
      INTEGER               PENDIX
      INTEGER               FIRST
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               LAST
      INTEGER               LBEG
      INTEGER               LEND
      INTEGER               LHAND
      INTEGER               LSIZE
      INTEGER               LOW
      INTEGER               MAXWND
      INTEGER               N
      INTEGER               NIDIR
      INTEGER               NPDIR
      INTEGER               NINTS
      INTEGER               NPREAD
      INTEGER               NSRCH
      INTEGER               NSREAD
      INTEGER               PACKSZ
      INTEGER               PGROUP
      INTEGER               REMAIN
      INTEGER               RSIZE
      INTEGER               SBEGIX
      INTEGER               SENDIX
      INTEGER               SGROUP
      INTEGER               SUBTYP
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ
      INTEGER               WSTART

!
!     Saved variables
!
      SAVE                  PREVS
      SAVE                  PREVN
      SAVE                  PREVNN
      SAVE                  LHAND
      SAVE                  LBEG
      SAVE                  LEND

!
!     Initial values
!
      DATA    LBEG    / -1    /
      DATA    LEND    / -1    /
      DATA    LHAND   / 0     /
      DATA    PREVN   / -1.D0 /
      DATA    PREVNN  / -1.D0 /
      DATA    PREVS   / -1.D0 /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKR05' )

!
!     No pointing found so far.
!
      FOUND = .FALSE.

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      TYPE  = IC( 3 )
      BEGIN = IC( 5 )
      END   = IC( 6 )

!
!     Make sure that this really is a type 05 data segment.
!
      IF ( TYPE .NE. 5 ) THEN
         CALL SETMSG( 'You are attempting to locate type * ' // &
                      'data in a type 5 data segment.'       )
         CALL ERRINT( '*',  TYPE                             )
         CALL SIGERR( 'SPICE(WRONGCKTYPE)'                   )
         CALL CHKOUT( 'CKR05'                                )
         RETURN
      END IF

!
!     Check the tolerance value.
!
      IF ( TOL .LT. 0.D0 ) THEN
         CALL SETMSG( 'Tolerance must be non-negative but '  // &
                      'was actually *.'                      )
         CALL ERRDP ( '*',  TOL                              )
         CALL SIGERR( 'SPICE(VALUEOUTOFRANGE)'               )
         CALL CHKOUT( 'CKR05'                                )
         RETURN
      END IF

!
!     Check the request time and tolerance against the bounds in
!     the segment descriptor.
!
      IF (       ( (SCLKDP + TOL) .LT. DC(1) ) &
           .OR.  ( (SCLKDP - TOL) .GT. DC(2) )  )  THEN
!
!        The request time is too far outside the segment's coverage
!        interval for any pointing to satisfy the request.
!
         CALL CHKOUT ( 'CKR05' )
         RETURN

      END IF

!
!     Set the request time to use for searching.
!
      T = BRCKTD ( SCLKDP, DC(1), DC(2) )

!
!     From this point onward, we assume the segment was constructed
!     correctly.  In particular, we assume:
!
!        1)  The segment descriptor's time bounds are in order and are
!            distinct.
!
!        2)  The epochs in the segment are in strictly increasing
!            order.
!
!
!        3)  The interpolation interval start times in the segment are
!            in strictly increasing order.
!
!
!        4)  The degree of the interpolating polynomial specified by
!            the segment is at least 1 and is no larger than MAXDEG.
!
!
      CALL DAFGDA ( HANDLE, END-CTRLSZ+1, END, CONTRL )

!
!     Check the FAILED flag just in case HANDLE is not attached to
!     any DAF file and the error action is not set to ABORT.  We
!     do this only after the first call to DAFGDA, as in CKR03.
!
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR05' )
         RETURN
      END IF

      RATE    =         CONTRL(1)
      SUBTYP  =  NINT ( CONTRL(2) )
      WNDSIZ  =  NINT ( CONTRL(3) )
      NINTS   =  NINT ( CONTRL(4) )
      N       =  NINT ( CONTRL(5) )

!
!     Set the packet size, which is a function of the subtype.
!
      IF ( SUBTYP .EQ. C05TP0 ) THEN

         PACKSZ = C05PS0

      ELSE IF ( SUBTYP .EQ. C05TP1 ) THEN

         PACKSZ = C05PS1

      ELSE IF ( SUBTYP .EQ. C05TP2 ) THEN

         PACKSZ = C05PS2

      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN

         PACKSZ = C05PS3

      ELSE

         CALL SETMSG ( 'Unexpected CK type 5 subtype # found in ' // &
                       'type 5 segment.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'CKR05'                                    )
         RETURN

      END IF

!
!     Check the window size.
!
      IF ( WNDSIZ .LE. 0 ) THEN

         CALL SETMSG ( 'Window size in type 05 segment was #; must ' // &
                       'be positive.'                                )
         CALL ERRINT ( '#',  WNDSIZ                                  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                         )
         CALL CHKOUT ( 'CKR05'                                       )
         RETURN

      END IF


      IF (      ( SUBTYP .EQ. C05TP0 ) &
           .OR. ( SUBTYP .EQ. C05TP2 )  ) THEN
!
!        These are the Hermite subtypes.
!
         MAXWND  =  (MAXDEG+1)/2

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; max '// &
                          'allowed value is # for subtypes 0 and 2 '  // &
                          '(Hermite, 8 or 14-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; '    // &
                          'must be even for subtypes 0 and 2 '        // &
                          '(Hermite, 8 or 14-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF


      ELSE IF (      ( SUBTYP .EQ. C05TP1 ) &
                .OR. ( SUBTYP .EQ. C05TP3 )  ) THEN
!
!        These are the Lagrange subtypes.
!
         MAXWND  =  MAXDEG + 1

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; max '// &
                          'allowed value is # for subtypes 1 and 3 '  // &
                          '(Lagrange, 4 or 7-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; '    // &
                          'must be even for subtypes 1 and 3 '        // &
                          '(Lagrange, 4 or 7-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF

      ELSE

         CALL SETMSG ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle the new subtype #'        )
         CALL ERRINT ( '#', SUBTYP                                  )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'CKR05'                                      )
         RETURN

      END IF

!
!     We now need to select the pointing values to interpolate
!     in order to satisfy the pointing request.  The first step
!     is to use the pointing directories (if any) to locate a set of
!     epochs bracketing the request time.  Note that the request
!     time might not be bracketed:  it could precede the first
!     epoch or follow the last epoch.
!
!     We'll use the variable PGROUP to refer to the set of epochs
!     to search.  The first group consists of the epochs prior to
!     and including the first pointing directory entry.  The last
!     group consists of the epochs following the last pointing
!     directory entry.  Other groups consist of epochs following
!     one pointing directory entry up to and including the next
!     pointing directory entry.
!

      NPDIR   =  (N-1) / DIRSIZ
      DIRBAS  =  BEGIN +  N*PACKSZ  +  N  -  1

      IF ( NPDIR .EQ. 0 ) THEN
!
!        There's no mystery about which group of epochs to search.
!
         PGROUP  = 1

      ELSE
!
!        There's at least one directory.  Find the first directory
!        whose time is greater than or equal to the request time, if
!        there is such a directory.  We'll search linearly through the
!        directory entries, reading up to DIRSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         BUFBAS  =  DIRBAS
         NPREAD  =  MIN ( NPDIR, DIRSIZ )

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NPREAD, PBUFFR )

         REMAIN  =  NPDIR - NPREAD


         DO WHILE (       ( PBUFFR(NPREAD) .LT. T ) &
                    .AND. ( REMAIN         .GT. 0 )   )

            BUFBAS  =  BUFBAS + NPREAD
            NPREAD  =  MIN ( REMAIN, DIRSIZ )
!
!           Note:  NPREAD is always > 0 here.
!
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NPREAD, PBUFFR )

            REMAIN  =  REMAIN - NPREAD

         END DO

!
!        At this point, BUFBAS - DIRBAS is the number of directory
!        entries preceding the one contained in PBUFFR(1).
!
!        PGROUP is one more than the number of directories we've
!        passed by.
!
         PGROUP  =     ( BUFBAS - DIRBAS ) &
                    +   LSTLTD ( T, NPREAD, PBUFFR ) &
                    +   1

      END IF

!
!     PGROUP now indicates the set of epochs in which to search for the
!     request epoch.  The following cases can occur:
!
!        PGROUP = 1
!        ==========
!
!           NPDIR = 0
!           --------
!           The request time may precede the first time tag
!           of the segment, exceed the last time tag, or lie
!           in the closed interval bounded by these time tags.
!
!           NPDIR >= 1
!           ---------
!           The request time may precede the first time tag
!           of the group but does not exceed the last epoch
!           of the group.
!
!
!        1 < PGROUP <= NPDIR
!        ===================
!
!           The request time follows the last time of the
!           previous group and is less than or equal to
!           the pointing directory entry at index PGROUP.
!
!        1 < PGROUP = NPDIR + 1
!        ======================
!
!           The request time follows the last time of the
!           last pointing directory entry.  The request time
!           may exceed the last time tag.
!
!
!     Now we'll look up the time tags in the group of epochs
!     we've identified.
!
!     We'll use the variable names PBEGIX and PENDIX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      IF ( PGROUP .EQ. 1 ) THEN

         PBEGIX  =  1
         PENDIX  =  MIN ( N, DIRSIZ )

      ELSE
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is strictly bracketed on the
!        low side by the time tag set we look up.
!
         PBEGIX  =      (  PGROUP  - 1 ) * DIRSIZ
         PENDIX  =  MIN (  PBEGIX + DIRSIZ,  N  )

      END IF

      TIMBAS  =  DIRBAS - N

      CALL DAFGDA ( HANDLE, TIMBAS+PBEGIX, TIMBAS+PENDIX, PBUFFR )

      NPREAD  =  PENDIX - PBEGIX + 1

!
!     At this point, we'll deal with the cases where T lies outside
!     of the range of epochs we've buffered.
!
      IF ( T .LT. PBUFFR(1) ) THEN
!
!        This can happen only if PGROUP = 1 and T precedes all epochs.
!        If the input request time is too far from PBUFFR(1) on
!        the low side, we're done.
!
         IF ( SCLKDP + TOL .LT. PBUFFR(1) ) THEN

            CALL CHKOUT ( 'CKR05' )
            RETURN

         END IF

!
!        Bracket T to move it within the range of buffered epochs.
!
         T = PBUFFR(1)


      ELSE IF ( T .GT. PBUFFR(NPREAD) ) THEN
!
!        This can happen only if T follows all epochs.
!
         IF ( SCLKDP - TOL .GT. PBUFFR(NPREAD) ) THEN

            CALL CHKOUT ( 'CKR05' )
            RETURN

         END IF

!
!        Bracket T to move it within the range of buffered epochs.
!
         T = PBUFFR(NPREAD)

      END IF

!
!     At this point,
!
!        | T - SCLKDP |  <=  TOL
!
!     Also, one of the following is true:
!
!        T is the first time of the segment
!
!        T is the last time of the segment
!
!        T equals SCLKDP
!
!
!
!     Find two adjacent time tags bounding the request epoch.  The
!     request time cannot be greater than all of time tags in the
!     group, and it cannot precede the first element of the group.
!
      I  =  LSTLTD (  T,  NPREAD,  PBUFFR  )

!
!     The variables LOW and HIGH are the indices of a pair of time
!     tags that bracket the request time.  Remember that NPREAD could
!     be equal to 1, in which case we would have LOW = HIGH.
!
      IF ( I .EQ. 0 ) THEN
!
!        This can happen only if PGROUP = 1 and T = PBUFFR(1).
!
         LOW    =  1
         LEPOCH =  PBUFFR(1)

         IF ( N .EQ. 1 ) THEN
            HIGH = 1
         ELSE
            HIGH = 2
         END IF

         HEPOCH = PBUFFR(HIGH)

      ELSE

         LOW    =  PBEGIX + I - 1
         LEPOCH =  PBUFFR(I)

         HIGH   =  LOW + 1
         HEPOCH =  PBUFFR(I+1)

      END IF

!
!     We now need to find the interpolation interval containing
!     T, if any.  We may be able to use the interpolation
!     interval found on the previous call to this routine.  If
!     this is the first call or if the previous interval is not
!     applicable, we'll search for the interval.
!
!     First check if the request time falls in the same interval as
!     it did last time.  We need to make sure that we are dealing
!     with the same segment as well as the same time range.
!
!
!        PREVS      is the start time of the interval that satisfied
!                   the previous request for pointing.
!
!        PREVN      is the start time of the interval that followed
!                   the interval specified above.
!
!        PREVNN     is the start time of the interval that followed
!                   the interval starting at PREVN.
!
!        LHAND      is the handle of the file that PREVS and PREVN
!                   were found in.
!
!        LBEG,      are the beginning and ending addresses of the
!        LEND       segment in the file LHAND that PREVS and PREVN
!                   were found in.
!
      IF (  ( HANDLE .EQ. LHAND ) .AND. &
            ( BEGIN  .EQ. LBEG  ) .AND. &
            ( END    .EQ. LEND  ) .AND. &
            ( T      .GE. PREVS ) .AND. &
            ( T      .LT. PREVN )      ) THEN


         START  = PREVS
         NSTART = PREVN
         NNSTRT = PREVNN

      ELSE
!
!        Search for the interpolation interval.
!
         NIDIR   =  (NINTS-1) / DIRSIZ
         DIRBAS  =   END - CTRLSZ - NIDIR

         IF ( NIDIR .EQ. 0 ) THEN
!
!           There's no mystery about which group of epochs to search.
!
            SGROUP  = 1

         ELSE
!
!           There's at least one directory.  Find the first directory
!           whose time is greater than or equal to the request time, if
!           there is such a directory.  We'll search linearly through
!           the directory entries, reading up to DIRSIZ of them at a
!           time. Having found the correct set of directory entries,
!           we'll perform a binary search within that set for the
!           desired entry.
!
            BUFBAS  =  DIRBAS
            NSREAD  =  MIN ( NIDIR, DIRSIZ )
            REMAIN  =  NIDIR - NSREAD

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NSREAD, SBUFFR )

            DO WHILE (       ( SBUFFR(NSREAD) .LT. T ) &
                       .AND. ( REMAIN         .GT. 0 )   )

               BUFBAS  =  BUFBAS + NSREAD
               NSREAD  =  MIN ( REMAIN, DIRSIZ )
               REMAIN  =  REMAIN - NSREAD
!
!              Note:  NSREAD is always > 0 here.
!
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NSREAD, SBUFFR )

            END DO

!
!           At this point, BUFBAS - DIRBAS is the number of directory
!           entries preceding the one contained in SBUFFR(1).
!
!           SGROUP is one more than the number of directories we've
!           passed by.
!
            SGROUP  =     ( BUFBAS - DIRBAS ) &
                       +   LSTLTD ( T, NSREAD, SBUFFR ) &
                       +   1

         END IF

!
!        SGROUP now indicates the set of interval start times in which
!        to search for the request epoch.
!
!        Now we'll look up the time tags in the group of epochs we've
!        identified.
!
!        We'll use the variable names SBEGIX and SENDIX to refer to the
!        indices, relative to the set of start times, of the first and
!        last start times in the set we're going to look up.
!
         IF ( SGROUP .EQ. 1 ) THEN

            SBEGIX  =  1
            SENDIX  =  MIN ( NINTS, DIRSIZ + 2 )

         ELSE
!
!           Look up the start times for the group of interest. Also
!           buffer last start time from the previous group. Also, it
!           turns out to be useful to pick up two extra start
!           times---the first two start times of the next group---if
!           they exist.
!
            SBEGIX  =      (  SGROUP  - 1 ) * DIRSIZ

            SENDIX  =  MIN (  SBEGIX + DIRSIZ + 2,  NINTS  )

         END IF

         TIMBAS  =  DIRBAS - NINTS

         CALL DAFGDA ( HANDLE, TIMBAS+SBEGIX, TIMBAS+SENDIX, SBUFFR )

         NSREAD  =  SENDIX - SBEGIX + 1

!
!        Find the last interval start time less than or equal to the
!        request time.  We know T is greater than or equal to the
!        first start time, so I will be > 0.
!
         NSRCH  =  MIN ( DIRSIZ + 1, NSREAD )

         I      =  LSTLED (  T,  NSRCH, SBUFFR  )

         START  =  SBUFFR(I)

!
!        Let NSTART ("next start") be the start time that follows
!        START, if START is not the last start time.  If NSTART
!        has a successor, let NNSTRT be that start time.
!
         IF ( I .LT. NSREAD ) THEN

            NSTART = SBUFFR(I+1)

            IF ( I+1 .LT. NSREAD ) THEN

               NNSTRT = SBUFFR(I+2)
            ELSE
               NNSTRT = DPMAX()
            END IF

         ELSE

            NSTART = DPMAX()
            NNSTRT = DPMAX()

         END IF

      END IF

!
!     If T does not lie within the interpolation interval starting
!     at time START, we'll determine whether T is closer to this
!     interval or the next.  If the distance between T and the
!     closer interval is less than or equal to TOL, we'll map T
!     to the closer endpoint of the closer interval.  Otherwise,
!     we return without finding pointing.
!
      IF ( HEPOCH .EQ. NSTART ) THEN
!
!        The first time tag greater than or equal to T is the start
!        time of the next interpolation interval.
!
!        The request time lies between interpolation intervals.
!        LEPOCH is the last time tag of the first interval; HEPOCH
!        is the first time tag of the next interval.
!
         IF (  ABS( T - LEPOCH ) .LE. ABS( HEPOCH - T )  ) THEN
!
!           T is closer to the first interval...
!
            IF (  ABS( T - LEPOCH )  .GT.  TOL  ) THEN
!
!              ...But T is too far from the interval.
!
               CALL CHKOUT ( 'CKR05' )
               RETURN

            END IF

!
!           Map T to the right endpoint of the preceding interval.
!
            T      = LEPOCH
            HIGH   = LOW
            HEPOCH = LEPOCH

         ELSE
!
!           T is closer to the second interval...
!
            IF (  ABS( HEPOCH - T )  .GT.  TOL  ) THEN
!
!              ...But T is too far from the interval.
!
               CALL CHKOUT ( 'CKR05' )
               RETURN

            END IF

!
!           Map T to the left endpoint of the next interval.
!
            T      = HEPOCH
            LOW    = HIGH
            LEPOCH = HEPOCH

!
!           Since we're going to be picking time tags from the next
!           interval, we'll need to adjust START and NSTART.
!
            START  = NSTART
            NSTART = NNSTRT

         END IF

      END IF

!
!     We now have
!
!        LEPOCH < T <  HEPOCH
!                -   -
!
!     where LEPOCH and HEPOCH are the time tags at indices
!     LOW and HIGH, respectively.
!
!     Now select the set of packets used for interpolation.  Note
!     that the window size is known to be even.
!
!     Unlike CK types 8, 9, 12, and 13, for type 05 we adjust
!     the window size to keep the request time within the central
!     interval of the window.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the request
!     time is too close to one end of the interpolation interval, we
!     reduce the window size, after which one endpoint of the window
!     will coincide with an endpoint of the interpolation interval.
!
!     We start out by looking up the set of time tags we'd use
!     if there were no gaps in the coverage.  We then trim our
!     time tag set to ensure all tags are in the interpolation
!     interval.  It's possible that the interpolation window will
!     collapse to a single point as a result of this last step.
!
!     Let LSIZE be the size of the "left half" of the window:  the
!     size of the set of window epochs to the left of the request time.
!     We want this size to be WNDSIZ/2, but if not enough states are
!     available, the set ranges from index 1 to index LOW.
!
      LSIZE =  MIN ( WNDSIZ/2, LOW )

!
!     RSIZE is defined analogously for the right half of the window.
!
      RSIZE =  MIN (  WNDSIZ/2,  ( N - HIGH + 1 )  )

!
!     The window size is simply the sum of LSIZE and RSIZE.
!
      WNDSIZ =  LSIZE + RSIZE

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      FIRST =  LOW    -  LSIZE   +  1

      LAST  =  FIRST  +  WNDSIZ  -  1

!
!     Buffer the epochs.
!
      WSTART  =  BEGIN  +  N * PACKSZ  +  FIRST  -  1

      CALL DAFGDA (  HANDLE, WSTART, WSTART+WNDSIZ-1, PBUFFR )

!
!     Discard any epochs less than START or greater than or equal
!     to NSTART.  The set of epochs we want ranges from indices
!     I+1 to J.  This range is non-empty unless START and NSTART
!     are both DPMAX().
!
      I = LSTLTD ( START,  WNDSIZ, PBUFFR )
      J = LSTLTD ( NSTART, WNDSIZ, PBUFFR )


      IF ( I .EQ. J ) THEN
!
!        Fuggedaboudit.
!
         CALL CHKOUT ( 'CKR05' )
         RETURN

      END IF

!
!     Update FIRST, LAST, and WNDSIZ.
!
      WNDSIZ = J     - I
      FIRST  = FIRST + I
      LAST   = FIRST + WNDSIZ - 1

!
!     Put the subtype into the output record.  The size of the group
!     of packets is derived from the subtype, so we need not include
!     the size.
!
      RECORD(1) =  T
      RECORD(2) =  SUBTYP
      RECORD(3) =  WNDSIZ
      RECORD(4) =  RATE

!
!     Read the packets.
!
      CALL DAFGDA ( HANDLE, &
                    BEGIN + (FIRST-1)*PACKSZ, &
                    BEGIN +      LAST*PACKSZ  -  1, &
                    RECORD(PARSIZ+1)                )

!
!     Finally, add the epochs to the output record.
!
      CALL MOVED ( PBUFFR(I+1), &
                   J-I, &
                   RECORD( PARSIZ + 1 + WNDSIZ*PACKSZ )  )


!
!     Save the information about the interval and segment.
!
      LHAND  = HANDLE
      LBEG   = BEGIN
      LEND   = END
      PREVS  = START
      PREVN  = NSTART
      PREVNN = NNSTRT

!
!     Indicate pointing was found.
!
      FOUND = .TRUE.

      CALL CHKOUT ( 'CKR05' )
      RETURN
      END
