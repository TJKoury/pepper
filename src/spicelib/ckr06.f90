!$Procedure CKR06 ( C-kernel, read record from segment, type 6 )

      SUBROUTINE CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, &
                         RECORD, FOUND                       )

! $ Abstract
!
!     Read a single CK data record from a segment of type 6
!     (ESOC/DDID Piecewise Interpolation).
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

      INCLUDE 'ckparam.inc'
      INCLUDE 'ck06.inc'

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
!     DESCR    are the file handle and segment descriptor for a CK
!              segment of type 6.
!
!     SCLKDP   is an encoded spacecraft clock time indicating the
!              epoch for which pointing is desired.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock.
!
!              When SCLKDP falls between the start time of one of
!              the mini-segment intervals and the last time tag of
!              that interval, the tolerance has no effect because
!              pointing will be returned at the request time.
!
!              However, if the request time falls within a coverage
!              gap in one of the intervals, or outside of any
!              interval, then the tolerance is used to determine if
!              pointing should be returned at the closest epoch for
!              which pointing is available. This epoch is either an
!              interval's start time or the smaller of an interval's
!              end time and its last time tag.
!
!
!     NEEDAV   is .TRUE. if angular velocity is requested. If the
!              input segment descriptor indicates angular velocity
!              is absent, the error SPICE(NOAVDATA) is signaled.
!
!              Note: although all subtypes of type 6 records either
!              contain or compute angular velocity, a CK creator may
!              choose to indicate that the provided angular velocity
!              data are not valid; this can be done by setting the
!              segment descriptor angular velocity flag to .FALSE.
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
!
!     FOUND    is a logical flag indicating whether data were found.
!              If NEEDAV is .FALSE., data will be found if the
!              request time is within TOL ticks of a time for which
!              the segment provides data. If NEEDAV is .TRUE., the
!              segment's angular velocity flag must also be set in
!              order for data to be found.
!
!              A type 6 segment provides data for times that are
!              between its descriptor time bounds and that are
!              within the coverage region of a mini-segment
!              interval. The coverage region of a mini-segment
!              interval extends from its start time to the lesser of
!              its stop time and its last time tag.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine roughly follows the pattern established in the
!     lower-numbered CK data type readers of not explicitly performing
!     error diagnoses. In particular, the C-kernel from which data are
!     read is assumed to be valid in most respects. The few exceptions
!     that are handled here are listed below.
!
!     1)  If the input HANDLE does not designate a loaded CK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 6, the
!         error SPICE(WRONGCKTYPE) is signaled.
!
!     3)  If the input SCLK value is not within TOL ticks of a time
!         for which the segment provides data, FOUND is set to .FALSE.
!         and the output record is undefined.
!
!     4)  If the window size is non-positive or greater than the
!         maximum allowed value, the error SPICE(INVALIDVALUE) is
!         signaled.
!
!     5)  If the window size is not compatible with the segment
!         subtype, the error SPICE(INVALIDVALUE) is signaled.
!
!     6)  If the segment subtype is not recognized, the error
!         SPICE(INVALIDSUBTYPE) is signaled.
!
!     7)  If the tolerance is negative, the error SPICE(NEGATIVETOL) is
!         signaled.
!
!     8)  If an error occurs while trying to read data from a C-kernel,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
!     9)  If the input segment descriptor indicates that angular
!         velocity data are not present, and if the input flag NEEDAV
!         is set to .TRUE., the error SPICE(NOAVDATA) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the CK Required Reading file for a description of the
!     structure of a data type 6 segment.
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
!           IF ( TYPE .EQ. 6 ) THEN
!
!              CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
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
!     1)  Correctness of the C-kernel read by this routine is
!         assumed.
!
!     2)  Correctness of inputs must be ensured by the caller of
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 06-JUL-2021 (NJB) (JDR)
!
!        Corrected code example: removed comment character preceding
!        CKBSS call. Added note regarding difference between this
!        search and those performed by the CK reader APIs CKGP and
!        CKGPAV.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     read record from type_6 CK segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD

      INTEGER               LSTLED
      INTEGER               LSTLTD

      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN
      LOGICAL               TOUCHL

!
!     Local parameters
!
      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DIRSIZ + 1 )

      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 4 )

      INTEGER               NSGPAR
      PARAMETER           ( NSGPAR = 2 )


!
!     Maximum window sizes, based on subtypes:
!
      INTEGER               C06MW0
      PARAMETER           ( C06MW0 =  ( MAXDEG + 1 )/2  )

      INTEGER               C06MW1
      PARAMETER           ( C06MW1 =  ( MAXDEG + 1 )    )

      INTEGER               C06MW2
      PARAMETER           ( C06MW2 =  ( MAXDEG + 1 )/2  )

      INTEGER               C06MW3
      PARAMETER           ( C06MW3 =  ( MAXDEG + 1 )    )

!
!     Local variables
!
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      GAP
      DOUBLE PRECISION      LSTEPC
      DOUBLE PRECISION      MINTIM ( 2 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SVBTIM
      DOUBLE PRECISION      SVETIM
      DOUBLE PRECISION      SVRATE
      DOUBLE PRECISION      T

      INTEGER               BADDR
      INTEGER               BEGIDX
      INTEGER               BUFBAS
      INTEGER               DIRBAS
      INTEGER               EADDR
      INTEGER               ENDIDX
      INTEGER               FIRST
      INTEGER               GROUP
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               ISEL
      INTEGER               IVBAS
      INTEGER               IVBIX
      INTEGER               IVEIX
      INTEGER               LAST
      INTEGER               LOW
      INTEGER               LSIZE
      INTEGER               MAXWND
      INTEGER               MINIB
      INTEGER               MINIE
      INTEGER               MINIIX
      INTEGER               MXWNSZ ( 0 : C06NST-1 )
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NPKDIR
      INTEGER               NPKT
      INTEGER               NRCPKT
      INTEGER               NREAD
      INTEGER               PKDBAS
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : C06NST-1 )
      INTEGER               REMAIN
      INTEGER               RSIZE
      INTEGER               SUBTYP
      INTEGER               SVBEG
      INTEGER               SVHAN
      INTEGER               SVMIIX
      INTEGER               SVMINB
      INTEGER               SVN
      INTEGER               SVNPKT
      INTEGER               SVPKDB
      INTEGER               SVPKND
      INTEGER               SVPKSZ
      INTEGER               SVSTYP
      INTEGER               SVWNSZ
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ

      LOGICAL               AVFLAG
      LOGICAL               IVLSEL
      LOGICAL               LVAL
      LOGICAL               PASS1
      LOGICAL               PRVFND
      LOGICAL               SAMIVL
      LOGICAL               SAMSEG
      LOGICAL               SVFND
      LOGICAL               SVLAST

!
!     Saved variables
!
      SAVE                  MXWNSZ
      SAVE                  PASS1
      SAVE                  PKTSZS
      SAVE                  SVBEG
      SAVE                  SVBTIM
      SAVE                  SVETIM
      SAVE                  SVFND
      SAVE                  SVHAN
      SAVE                  SVLAST
      SAVE                  SVMIIX
      SAVE                  SVMINB
      SAVE                  SVN
      SAVE                  SVNPKT
      SAVE                  SVPKDB
      SAVE                  SVPKND
      SAVE                  SVPKSZ
      SAVE                  SVRATE
      SAVE                  SVSTYP
      SAVE                  SVWNSZ

!
!     Initial values
!
      DATA                  MXWNSZ  /  C06MW0, C06MW1, C06MW2, C06MW3 /
      DATA                  PASS1   /  .TRUE.  /
      DATA                  PKTSZS  /  C06PS0, C06PS1, C06PS2, C06PS3 /
      DATA                  SVBEG   /  -1      /
      DATA                  SVBTIM  /   0.D0   /
      DATA                  SVETIM  /  -1.D0   /
      DATA                  SVFND   /  .FALSE. /
      DATA                  SVHAN   /   0      /
      DATA                  SVLAST  /  .FALSE. /
      DATA                  SVMIIX  /  -1      /
      DATA                  SVMINB  /  -1      /
      DATA                  SVN     /  -1      /
      DATA                  SVNPKT  /  -1      /
      DATA                  SVPKDB  /  -1      /
      DATA                  SVPKND  /  -1      /
      DATA                  SVPKSZ  /  -1      /
      DATA                  SVRATE  /  -1.D0   /
      DATA                  SVSTYP  /  -1      /
      DATA                  SVWNSZ  /  -1      /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKR06' )

!
!     Start with a parameter compatibility check on the first
!     pass.
!
      IF ( PASS1 ) THEN

         IF ( CKMRSZ .LT. MAXRSZ ) THEN

            CALL SETMSG ( 'CK type 6 record size may be as ' &
            //            'large as #, but CKPFS record size ' &
            //            '(defined in ckparam.inc) is #.'     )
            CALL ERRINT ( '#',  MAXRSZ                         )
            CALL ERRINT ( '#',  CKMRSZ                         )
            CALL SIGERR ( 'SPICE(BUG)'                         )

         END IF
!
!        Indicate the first pass was completed.
!
         PASS1 = .FALSE.

      END IF

!
!     No pointing found so far.
!
      FOUND  = .FALSE.

!
!     Let PRVFND indicate the last value of FOUND we returned. PRVFND
!     allows us to reset SVFND to .FALSE. at the start of this routine,
!     so we don't have to do this prior to every return (of which there
!     are more than 35).
!
      PRVFND = SVFND

!
!     Set the saved value of FOUND so as to reflect the value
!     of FOUND we'll return next.
!
      SVFND  = .FALSE.

!
!     "Touch" the input argument NEEDAV to suppress compiler warnings.
!
      LVAL = TOUCHL ( NEEDAV )
      LVAL = TOUCHL ( LVAL   )

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      TYPE   = IC( 3 )
      AVFLAG = IC( 4 ) .EQ. 1
      BADDR  = IC( 5 )
      EADDR  = IC( 6 )

!
!     Check whether angular velocity data are requested but not
!     available.
!
      IF (  NEEDAV  .AND. ( .NOT. AVFLAG ) ) THEN

         CALL SETMSG ( 'Segment descriptor indicates angular velocity ' &
         //            'data are not available, but such data were ' &
         //            'requested.'                                    )
         CALL SIGERR ( 'SPICE(NOAVDATA)'                               )
         CALL CHKOUT ( 'CKR06'                                         )
         RETURN

      END IF

!
!     Check the tolerance value.
!
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG( 'Tolerance must be non-negative but ' &
         //           'was actually *.'                      )
         CALL ERRDP ( '*',  TOL                              )
         CALL SIGERR( 'SPICE(NEGATIVETOL)'                   )
         CALL CHKOUT( 'CKR06'                                )
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
         CALL CHKOUT ( 'CKR06' )
         RETURN

      END IF

!
!     Set the request time to use for searching.
!
      T = BRCKTD ( SCLKDP, DC(1), DC(2) )

!
!     From this point onward, we assume the segment was constructed
!     correctly.
!
!
!     Terminology: below, the phrase "base address of 'X'" refers to
!     the DAF address immediately preceding X. Base addresses simplify
!     mapping DAF array (here "array" means an array stored in
!     consecutive DAF addresses, not "segment") indices to DAF
!     addresses, since the DAF address of the Ith array element is
!     obtained by adding I to the DAF array's base address.
!
!     Key variables:
!
!        Name      Meaning
!        ----      -------
!        BADDR     Segment begin DAF address.
!
!        DIRBAS    Base address of mini-segment interval directory.
!
!        EADDR     Segment end DAF address.
!
!        FIRST     Index (mini-segment-relative) of first time tag in
!                  sequence transferred to the output record.
!
!        HIGH      Index (mini-segment-relative) of time tag following
!                  the tag at index LOW (see description below).
!
!        IVBIX     Index in the mini-segment interval bounds array of
!                  the start time of the applicable interval.
!
!        IVLBAS    Base address of mini-segment interval time bounds.
!
!        IVLSEL    Interval selection flag: this routine selects the
!                  last applicable interval if true; otherwise it
!                  selects the first applicable interval.
!
!        LAST      Index (mini-segment-relative) of last time tag in
!                  sequence transferred to the output record.
!
!        LOW       Index (mini-segment-relative) of last time tag less
!                  than the request time, or of the first time tag if
!                  this tag equals the request time.
!
!        MINIB,
!        MINIE     Mini-segment begin and end DAF addresses. These
!                  addresses are absolute, not segment-relative.
!
!        MINIIX    Mini-segment/mini-segment interval index.
!
!        N         Count of mini-segments.
!
!        NDIR      Number of mini-segment interval time bounds
!                  directories.
!
!        NPKDIR    Number of packet directory entries for current
!                  mini-segment.
!
!        NPKT      Packet count for current mini-segment.
!
!        NRCPKT    Output record packet count. Note that this count,
!                  due to reduction of order at mini-segment
!                  boundaries, may be smaller than the window size
!                  stored in the current mini-segment.
!
!        PKDBAS    Base address of packet directory for current
!                  mini-segment.
!
!        PKTSIZ    Size of packets of current mini-segment.
!
!        SUBTYP    Subtype of current mini-segment.
!
!        TIMBAS    Base address of time tags of current mini-segment.
!
!        WNDSIZ    Interpolation window size of current mini-segment.
!
!
!     Re-used variables: the variables shown in the list below
!     are used as short-duration variables, much like loop index
!     variables; they are re-used as needed.
!
!        BUFBAS
!        BUFFER
!        GROUP
!        NREAD
!        REMAIN
!
!
!     Decide whether we're looking at the same segment we saw on the
!     previous call, and whether the mini-segment interval used on
!     that call is still applicable.
!
!     Re-use of data from a previous call requires that the saved
!     data were set on a successful call. Note that PRVFND can not
!     be true on the first pass.
!
      SAMSEG  =       ( HANDLE .EQ.  SVHAN ) &
                .AND. ( BADDR  .EQ.  SVBEG ) &
                .AND.   PRVFND

!
!     Give SAMIVL an initial value. If we do have the
!     same interval, update SAMIVL to indicate this.
!
      SAMIVL = .FALSE.

      IF ( SAMSEG ) THEN
!
!        We now assume that all data saved from the last
!        read of this segment are valid.
!
         IF ( SVLAST ) THEN
!
!           We pick the last interval containing T. For all intervals
!           but the last, T must be less than the interval end time.
!           For the last interval, T may equal the end time.
!
!           Note that we don't bother to test for the special case
!           where the interval is not the last, there's a gap at the
!           end of the interval and T equals the last epoch of the
!           interval. In this rare case, we do not reuse the old
!           interval data, even though it would be possible to
!           add code to do so.
!
            IF ( SVMIIX .LT. SVN ) THEN

               SAMIVL =       ( T .GE. SVBTIM ) &
                        .AND. ( T .LT. SVETIM )
            ELSE

               SAMIVL =         T .GE. SVBTIM &
                        .AND. ( T .LE. SVETIM )
            END IF

         ELSE
!
!           We pick the first interval containing T. For all intervals
!           but the first, T must be greater than the interval start
!           time. For the first interval, T may equal the start time.
!
            IF ( SVMIIX .GT. 1 ) THEN

               SAMIVL =       ( T  .GT. SVBTIM ) &
                        .AND. ( T  .LE. SVETIM )

            ELSE

               SAMIVL =       ( T  .GE. SVBTIM ) &
                        .AND. ( T  .LE. SVETIM )
            END IF

         END IF

      END IF


      IF ( SAMSEG .AND. SAMIVL ) THEN
!
!        We're looking at the same segment as last time, and the
!        mini-segment interval we looked up last time is applicable
!        for the input time T.
!
!        Simply restore the segment and interval parameters we
!        saved from the previous lookup.
!
!        We don't need to restore the segment start DAF address
!        BADDR, since we've already extracted it from DESCR.
!
!        Restore
!
!           - The mini-segment's packet directory count
!           - The mini-segment's packet directory base address
!
         NPKDIR = SVPKND
         PKDBAS = SVPKDB

!
!        Restore
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         N      = SVN
         MINIIX = SVMIIX
         MINIB  = SVMINB

!
!        Restore
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment interpolation window size
!           - The mini-segment clock rate
!
         SUBTYP = SVSTYP
         PKTSIZ = SVPKSZ
         NPKT   = SVNPKT
         WNDSIZ = SVWNSZ
         RATE   = SVRATE

      ELSE
!
!        The segment and interval information for the current segment
!        must be looked up.
!
!        Perform checks on this segment.
!
!        Make sure that this really is a type 06 data segment.
!
         IF ( TYPE .NE. 6 ) THEN

            CALL SETMSG( 'You are attempting to locate type * ' &
            //           'data in a type 6 data segment.'       )
            CALL ERRINT( '*',  TYPE                             )
            CALL SIGERR( 'SPICE(WRONGCKTYPE)'                   )
            CALL CHKOUT( 'CKR06'                                )
            RETURN

         END IF

!
!        Locate the mini-segment interval that contains the request
!        time. If the request time lies a common boundary of two
!        intervals, the choice of interval is determined by the
!        interval selection flag.
!
!        Before getting started, we need to determine which interval to
!        use if the request time lies on a boundary between two
!        intervals. The segment's interval selection flag tells us how
!        to resolve this.
!
         CALL DAFGDA ( HANDLE, EADDR-1, EADDR, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

         ISEL   = NINT ( CONTRL(1) )
         N      = NINT ( CONTRL(2) )

         IVLSEL = ISEL .EQ. ITRUE

!
!        Determine the number of interval directory entries in the
!        segment. Note that for most CK types, this computation is
!        performed by computing
!
!           ( N - 1 ) / DIRSIZ
!
!        where N is the segment's epoch count.
!
!        However the set of items in this case is a sequence
!        of N start times followed by a final stop time, so
!        the epoch count is
!
!           N + 1
!
!        and the numerator in the ratio above is incremented by 1.
!
         NDIR = N / DIRSIZ

!
!        Note that the directory placement scheme always leaves
!        a non-empty group of epochs following the last directory
!        entry.
!
!        Let DIRBAS be the base address of the interval directory.
!        We'll compute DIRBAS whether or not the interval directory
!        is non-empty.
!
!        If the interval directory is non-empty, it spans the address
!        range
!
!           DIRBAS+1 : DIRBAS+NDIR
!
!        We compute DIRBAS by starting at the end of the segment
!        and skipping over the control area, the mini-segment
!        start/stop pointers, and the interval directory itself.
!
         DIRBAS = EADDR - NSGPAR - ( N + 1 ) - NDIR

!
!        The way we search the directory depends on the treatment
!        of request times that lie on interval boundaries.
!
         IF ( IVLSEL ) THEN
!
!           We must pick the latest interval containing the request
!           time.
!
!           The stop time of the interval we seek is the first interval
!           boundary strictly greater than T, unless T is the stop time
!           of the final interval.
!
!           We want to find the group of interval boundaries containing
!           the stop time of the interval containing T. There are
!           NDIR+1 such groups; all but the last have a directory entry
!           that coincides with the final epoch of the group. We'll use
!           the variable GROUP as the group index.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            IF ( NDIR .EQ. 0 ) THEN
!
!              There's no question about which group of epochs to
!              search.
!
               GROUP = 1

            ELSE
!
!              The index of the group we seek is the index of the first
!              directory entry that is greater than T, if such an entry
!              exists. If there's no such entry, the group we seek is
!              the final one.
!
!              Find the last directory entry less than or equal to
!              the request time. The directory entry after that one,
!              if such exists, is the one to pick.
!
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS
!
!              Fetch the current batch of directory entries.
!
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'CKR06' )
                  RETURN
               END IF

               REMAIN = NDIR - NREAD
!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               DO WHILE (       ( REMAIN        .GT. 0 ) &
                          .AND. ( BUFFER(NREAD) .LE. T )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
!
!                 Fetch the current batch of directory entries.
!
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'CKR06' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD

               END DO
!
!              Count the directory entries that are less than or equal
!              to T. The number we skipped over before the final loop
!              iteration is BUFBAS-DIRBAS; the number of buffered
!              entries we're skipping is the number of entries that are
!              less than or equal to T. The index of the group of
!              epochs containing T exceeds the skipped directory count
!              by 1.
!
               GROUP =    BUFBAS - DIRBAS &
                       +  LSTLED ( T, NREAD, BUFFER ) &
                       +  1
!
!              GROUP is in the range 1 : NDIR+1.
!
            END IF

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            IVBAS  = DIRBAS - ( N + 1 )
!
!           Now find the index of the last interval boundary less than
!           or equal to T. We'll need to read the current group of
!           epochs first, so compute the base of the range of addresses
!           containing this group.

            BUFBAS = IVBAS + ( ( GROUP - 1 ) * DIRSIZ )
!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            REMAIN = ( N + 1 ) - ( ( GROUP - 1 ) * DIRSIZ )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            NREAD  = MIN ( DIRSIZ, REMAIN )

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'CKR06' )
               RETURN
            END IF

!
!           Find the index of the first epoch greater than T; this is
!           the number of epochs that are less than or equal to T, plus
!           1. The case where T matches the final epoch must be handled
!           here, since in this case no epoch exceeds T.
!
            IVEIX =    BUFBAS - IVBAS &
                    +  LSTLED ( T, NREAD, BUFFER ) &
                    +  1

            IVEIX = MIN ( IVEIX,  N+1 )

!
!           Backstop test:
!
            IF ( IVEIX .LT. 2 ) THEN

               CALL SETMSG ( 'IVEIX = #.' )
               CALL ERRINT ( '#', IVEIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'CKR06'      )
               RETURN

            END IF

!
!           The epoch at index IVEIX is the end time of the
!           mini-segment interval we'll use. The index of
!           the interval itself is IVEIX - 1.
!
            MINIIX = IVEIX - 1


         ELSE
!
!           IVLSEL is .FALSE., meaning we must pick the first interval
!           containing the request time.

!           The start time of the interval we seek is the last interval
!           boundary strictly less than T, unless T is the start time
!           of the first interval. The stop time of this interval is
!           the first boundary greater than or equal to T.
!
!           We want to find the group of interval boundaries containing
!           the stop time of the interval containing T. There are
!           NDIR+1 such groups; all but the last have a directory entry
!           that coincides with the final epoch of the group. We'll use
!           the variable GROUP as the group index.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            IF ( NDIR .EQ. 0 ) THEN
!
!              There's no question about which group of epochs to
!              search.
!
               GROUP = 1

            ELSE
!
!              Find the last directory entry strictly less than the
!              request time. The directory entry after that one, if
!              such exists, is the one to pick.
!
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS
               REMAIN = NDIR - NREAD
!
!              Fetch the current batch of directory entries.
!
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'CKR06' )
                  RETURN
               END IF
!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               DO WHILE (      ( REMAIN        .GT. 0 ) &
                         .AND. ( BUFFER(NREAD) .LT. T )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
!
!                 Fetch the current batch of directory entries.
!
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'CKR06' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD

               END DO
!
!              Count the directory entries that are less than T. The
!              number we skipped over before the final loop iteration
!              is BUFBAS-DIRBAS; the number of buffered entries we're
!              skipping is the number of entries that are less than T.
!              The index of the group of epochs containing T exceeds
!              the skipped directory count by 1.
!
               GROUP =    BUFBAS - DIRBAS &
                       +  LSTLTD ( T, NREAD, BUFFER ) &
                       +  1
!
!              GROUP is in the range 1 : NDIR+1.
!
            END IF

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            IVBAS  = DIRBAS - ( N + 1 )

!
!           Now find the index of the last interval boundary epoch less
!           than T. We'll need to read the current group of epochs
!           first, so compute the base of the range of addresses
!           containing this group.
!
            BUFBAS = IVBAS + ( ( GROUP - 1 ) * DIRSIZ )

!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            REMAIN = ( N + 1 ) - ( ( GROUP - 1 ) * DIRSIZ )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            NREAD  = MIN ( DIRSIZ, REMAIN )

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'CKR06' )
               RETURN
            END IF

!
!           Find the index of the last interval boundary less than T.
!           The case where T matches the first boundary must be handled
!           here, since in this case no boundary precedes T.
!
            IVBIX =    BUFBAS - IVBAS &
                    +  LSTLTD ( T, NREAD, BUFFER )

            IVBIX = MAX ( IVBIX,  1 )

!
!           Backstop test:
!
            IF ( IVBIX .GT. N ) THEN

               CALL SETMSG ( 'IVBIX = #.' )
               CALL ERRINT ( '#', IVBIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'CKR06'      )
               RETURN

            END IF

!
!           The epoch at index IVBIX is the begin time of the
!           mini-segment interval we'll use.
!
            MINIIX = IVBIX

         END IF
!
!        This is the end of the IF block that handles mini-segment
!        selection for the two possible values of IVLSEL.
!
!        If the mini-segment we just found has a gap, and if TOL is
!        positive, it's possible that the mini-segment we want actually
!        is the successor of the one at index MINIIX. We'll check this
!        by finding the last epoch of the mini-segment we just
!        identified.
!
!        Look up the begin and end pointers of the mini-segment at index
!        MINIIX. For the first N-1 mini-segments, the "end pointer"
!        of one mini-segment is the "begin" pointer of the next.
!
         BUFBAS = EADDR - NSGPAR - ( N + 1 ) + ( MINIIX - 1 )

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+2, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

         MINIB = NINT ( BUFFER(1) ) + BADDR - 1
!
!        Note that the end of the current mini-segment
!        precedes the start of the next mini-segment by
!        one address.
!
         MINIE = NINT ( BUFFER(2) ) + BADDR - 2

!
!        Look up the control area of the mini-segment.
!
         CALL DAFGDA ( HANDLE, MINIE-CTRLSZ+1, MINIE, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

!
!        Fetch the control area parameters for the mini-segment.
!
         RATE    =         CONTRL(1)
         SUBTYP  =  NINT ( CONTRL(2) )
         WNDSIZ  =  NINT ( CONTRL(3) )
         NPKT    =  NINT ( CONTRL(4) )

!
!        Compute the directory count for the mini-segment.
!
         NPKDIR  =  ( NPKT - 1 ) / DIRSIZ

!
!        The last epoch of the mini-segment precedes the epoch
!        directories and the control area. Look up this epoch.
!
         BUFBAS  =  MINIE - CTRLSZ - NPKDIR - 1

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, LSTEPC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

!
!        Determine whether the request time is in a gap.
!
         IF ( T .GT. LSTEPC ) THEN
!
!           Yep, T lies in a gap. But we may still be able to
!           find data for this request, if the lookup tolerance
!           is positive.
!
            IF ( TOL .EQ. 0.D0 ) THEN
!
!              We're out of luck. We can't find pointing for this
!              request. FOUND is already .FALSE., so just return.
!
               CALL CHKOUT ( 'CKR06' )
               RETURN

            ELSE
!
!              Determine the distance of T from the nearest epochs.
!
!              Look up the time bounds of the mini-segment at index
!              MINIIX.
!
               CALL DAFGDA ( HANDLE, IVBAS+MINIIX, &
                                     IVBAS+MINIIX+1, MINTIM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CKR06' )
                  RETURN
               END IF

!
!              See whether T is close enough to a stored epoch for
!              us to find pointing. If not, return now.
!
               IF (      (  ( T         - LSTEPC )  .GT.  TOL  ) &
                   .AND. (  ( MINTIM(2) - T      )  .GT.  TOL  )  ) THEN
!
!                 We can't find pointing for T. FOUND is already
!                 .FALSE., so just return.
!
                  CALL CHKOUT ( 'CKR06' )
                  RETURN

               END IF

!
!              Continue to look for pointing usable for time T.
!
               IF ( MINIIX .EQ. N ) THEN
!
!                 We're looking at the final mini-segment. If
!                 T is close enough to LSTEPC, we can find
!                 pointing.
!
                  IF (  ( T - LSTEPC )  .LE.  TOL  ) THEN
!
!                    We're going to carry on using the current
!                    mini-segment. We'll update T to be the last epoch
!                    of this mini-segment.
!
                     T = LSTEPC

                  ELSE
!
!                    T is too far from LSTEPC. We're done. FOUND is
!                    already .FALSE., so just return.
!
                     CALL CHKOUT ( 'CKR06' )
                     RETURN

                  END IF

               ELSE
!
!                 There's a successor to the current interval. Determine
!                 which interval contains an epoch closest to T.
!
!                 Compute the size of the gap at the right end of the
!                 interior of the current interval.
!
                  GAP  =  MINTIM(2) - LSTEPC

                  IF (  ( T - LSTEPC )  .LE.  ( GAP/2 )  ) THEN
!
!                    T is closer to LSTEPC than the start time of the
!                    next interval. We're going to carry on using the
!                    current mini-segment. We'll update T to be the
!                    last epoch of this mini-segment.
!
                     T = LSTEPC

                  ELSE
!
!                    T is closer to the start time of the next interval
!                    than to LSTEPC. than the start time of the next
!                    interval. We're going to use the next
!                    mini-segment.
!
                     MINIIX  =  MINIIX + 1

!
!                    Update the mini-segment parameters we already
!                    found, since these have been superseded.
!
!                    The mini-segment pointers:
!
                     BUFBAS = EADDR - NSGPAR - (N + 1) + ( MINIIX - 1 )

                     CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+2, BUFFER )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'CKR06' )
                        RETURN
                     END IF

                     MINIB = NINT ( BUFFER(1) ) + BADDR - 1
!
!                    Note that the end of the current mini-segment
!                    precedes the start of the next mini-segment by one
!                    address.
!
                     MINIE = NINT ( BUFFER(2) ) + BADDR - 2

!
!                    Look up the control area of the mini-segment.
!
                     CALL DAFGDA ( HANDLE, MINIE-CTRLSZ+1, &
                                           MINIE,          CONTRL )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'CKR06' )
                        RETURN
                     END IF

!
!                    Fetch the control area parameters for the
!                    mini-segment.
!
                     RATE    =  NINT ( CONTRL(1) )
                     SUBTYP  =  NINT ( CONTRL(2) )
                     WNDSIZ  =  NINT ( CONTRL(3) )
                     NPKT    =  NINT ( CONTRL(4) )
!
!                    Since we have new mini-segment parameters, we need
!                    to check them. We'll defer these checks until
!                    later, so we can perform one set of checks,
!                    regardless of which logic path we followed to
!                    select a mini-segment.
!
!                    Compute the directory count for the mini-segment.
!
                     NPKDIR  =  ( NPKT - 1 ) / DIRSIZ
!
!                    We're going to set T to the start time of the
!                    current mini-segment interval, which is the stop
!                    time of the previous one.
!
                     T = MINTIM(2)
!
!                    We still need to look up the last epoch of the
!                    current mini-segment. We'll use this when we save
!                    the time bounds of the mini-segment.
!
                     BUFBAS = MINIE - CTRLSZ - NPKDIR - 1

                     CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, LSTEPC )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'CKR06' )
                        RETURN
                     END IF

                  END IF

               END IF
!
!              At this point T is set. If we had to update the
!              mini-segment index and its parameters, we did so.
!
            END IF
!
!           We've handled the case where T lies in a gap and the
!           tolerance is non-zero.

         END IF
!
!        This is the end of the block that handles the case where T lies
!        in a gap. At this point, the following items are set:
!
!           T
!           MINIIX
!           MINIB
!           MINIE
!           SUBTYP
!           WNDSIZ
!           NPKT
!           NPKDIR
!           RATE
!
!        Look up the time bounds of the mini-segment at index MINIIX.
!        These bounds are used quite a bit farther on, when we save
!        them for future use.
!
         CALL DAFGDA ( HANDLE, IVBAS+MINIIX, IVBAS+MINIIX+1, MINTIM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

!
!        From this point onward, we'll work with the mini-segment
!        that occupies the address range MINIB : MINIE.
!
!        Set the packet size, which is a function of the subtype.
!        Also set the maximum window size. First check the subtype,
!        which will be used as an array index.
!
         IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GE. C06NST ) ) THEN

            CALL SETMSG ( 'Unexpected CK type 6 subtype # found in ' &
            //            'type 06 segment within mini-segment #.'     )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'                      )
            CALL CHKOUT ( 'CKR06'                                      )
            RETURN

         END IF

         PKTSIZ = PKTSZS ( SUBTYP )
         MAXWND = MXWNSZ ( SUBTYP )

!
!        Check the window size.
!
         IF ( ( WNDSIZ .LT. 2 ) .OR. ( WNDSIZ .GT. MAXWND ) ) THEN

            CALL SETMSG ( 'Window size in type 6 segment was #; must ' &
            //            'be in the range 2:# for subtype #. ' &
            //            'Mini-segment index is #.'                   )
            CALL ERRINT ( '#',  WNDSIZ                                 )
            CALL ERRINT ( '#',  MAXWND                                 )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                        )
            CALL CHKOUT ( 'CKR06'                                      )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 06 segment was #; ' &
            //            'must be even for subtype #. ' &
            //            'Mini-segment index is #.'               )
            CALL ERRINT ( '#',  WNDSIZ                             )
            CALL ERRINT ( '#',  SUBTYP                             )
            CALL ERRINT ( '#',  MINIIX                             )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                    )
            CALL CHKOUT ( 'CKR06'                                  )
            RETURN

         END IF

!
!        Compute the base address of the sequence of packet
!        directory entries for the current mini-segment/interval.
!
         PKDBAS  =  MINIB - 1 + ( NPKT * ( PKTSIZ + 1 ) )

!
!        The test below is done for safety. No SPICE errors
!        should ever be detected at this point.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

!
!        If we made it this far, we did so without a SPICE error. We
!        have valid segment parameters which can be saved for the next
!        call.
!
!        Save
!
!           - The DAF handle
!           - The segment begin DAF address
!           - The segment's "select last/first interval" flag
!
         SVHAN  = HANDLE
         SVBEG  = BADDR
         SVLAST = IVLSEL

!
!        Save the time bounds of the applicable mini-segment/interval.
!        We don't want to indicate data availability within a gap, since
!        the re-use logic assumes data availability.
!
         SVBTIM =       MINTIM(1)
         SVETIM = MIN ( MINTIM(2), LSTEPC )

!
!        Save
!
!           - The mini-segment/interval directory count
!           - The mini-segment/interval directory base address
!
         SVPKND = NPKDIR
         SVPKDB = PKDBAS

!
!        Save
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         SVN    = N
         SVMIIX = MINIIX
         SVMINB = MINIB

!
!        Save
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment window size
!           - The mini-segment clock rate
!
         SVSTYP = SUBTYP
         SVPKSZ = PKTSIZ
         SVNPKT = NPKT
         SVWNSZ = WNDSIZ
         SVRATE = RATE

      END IF

!
!     We're ready to construct the output record. The first step is to
!     identify the indices of the packets and epochs corresponding to
!     the request.
!
!     We'll now select the set of packets that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      IF ( NPKDIR .EQ. 0 ) THEN
!
!        There's no mystery about which group of epochs to search.
!
         GROUP  = 1

      ELSE
!
!        There's at least one directory entry. Find the first directory
!        entry whose time is greater than or equal to the request time,
!        if there is such an entry.  We'll search linearly through the
!        directory entries, reading up to DIRSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         BUFBAS  =  PKDBAS
         NREAD   =  MIN ( NPKDIR, DIRSIZ )
         REMAIN  =  NPKDIR - NREAD

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

         DO WHILE (       ( BUFFER(NREAD) .LT. T ) &
                    .AND. ( REMAIN        .GT. 0 )  )

            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, DIRSIZ )
            REMAIN  =  REMAIN - NREAD
!
!           Note:  NREAD is always > 0 here.
!
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKR06' )
               RETURN
            END IF

         END DO

!
!        At this point, BUFBAS - PKDBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         GROUP  =     ( BUFBAS - PKDBAS ) &
                    +   LSTLTD ( T, NREAD, BUFFER ) &
                    +   1
      END IF

!
!     GROUP now indicates the set of epochs in which to search for the
!     request epoch.  If GROUP is 1, the request time lies within the
!     inclusive time interval bounded by the first and last epochs of
!     the first group.  Otherwise, the request time lies in the time
!     interval bounded by the last element of the preceding group and
!     the last element of the current group.
!
!     We'll use the variable names BEGIDX and ENDIDX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      IF ( GROUP .EQ. 1 ) THEN

         BEGIDX  =  1
         ENDIDX  =  MIN ( NPKT, DIRSIZ )

      ELSE
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         BEGIDX  =      (  GROUP  - 1 ) * DIRSIZ
         ENDIDX  =  MIN (  BEGIDX + DIRSIZ,  NPKT  )

      END IF


      TIMBAS  =  PKDBAS - NPKT

      CALL DAFGDA ( HANDLE, TIMBAS+BEGIDX, TIMBAS+ENDIDX, BUFFER )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR06' )
         RETURN
      END IF

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      I  =  LSTLTD (  T,  ENDIDX-BEGIDX+1,  BUFFER  )

!
!     The variables LOW and high are the indices of a pair of time
!     tags that bracket the request time.
!
      IF ( I .EQ. 0 ) THEN
         LOW  =  1
      ELSE
         LOW  =  BEGIDX + I - 1
      END IF

      HIGH  =  LOW  +  1

!
!     Now select the set of packets used for interpolation. Note
!     that the window size is known to be even.
!
!     For CK type 6 we allow the window size to shrink when the window
!     must be truncated due to proximity to an interval boundary.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
!     request time is too close to one end of the coverage interval,
!     we reduce the window size, after which one endpoint of the
!     window will coincide with an endpoint of the coverage interval.
!
!     Let LSIZE be the size of the "left half" of the window: the size
!     set of window epochs to the left of the request time. We want
!     this size to be WNDSIZ/2, but if not enough packets are
!     available, the set ranges from index 1 to index LOW.
!

      LSIZE =  MIN ( WNDSIZ/2, LOW )

!
!     RSIZE is defined analogously for the right half of the window.
!
      RSIZE =  MIN (  WNDSIZ/2,  ( NPKT - HIGH + 1 )  )

!
!     The actual window size is simply the sum of LSIZE and RSIZE.
!
      NRCPKT =  LSIZE + RSIZE

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      FIRST =  LOW    -  LSIZE   +  1

      LAST  =  FIRST  +  NRCPKT  -  1

!
!     We're ready to construct the output record.
!
!     Put the subtype and window size into the output record.
!     The fourth element is the nominal SCLK rate.
!
      RECORD(1) =  T
      RECORD(2) =  SUBTYP
      RECORD(3) =  NRCPKT
      RECORD(4) =  RATE

!
!     Read the packets.
!
      CALL DAFGDA ( HANDLE, &
                    MINIB + (FIRST-1)*PKTSIZ, &
                    MINIB +      LAST*PKTSIZ  -  1, &
                    RECORD( CTRLSZ + 1 )           )
!
!     Finally, add the epochs to the output record.
!     Read the sequence of time tags.
!
      BUFBAS  =  ( MINIB - 1 )  +  ( NPKT * PKTSIZ )  +  ( FIRST - 1 )

      CALL DAFGDA ( HANDLE, &
                    BUFBAS+1, &
                    BUFBAS+NRCPKT, &
                    RECORD( CTRLSZ + NRCPKT*PKTSIZ + 1 )  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR06' )
         RETURN
      END IF
!
!     Indicate pointing was found.
!
      FOUND = .TRUE.
      SVFND = .TRUE.

      CALL CHKOUT ( 'CKR06' )
      RETURN
      END
