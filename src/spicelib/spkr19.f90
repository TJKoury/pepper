!$Procedure SPKR19 ( SPK, read record from segment, type 19 )

      SUBROUTINE SPKR19 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 19
!     (ESOC/DDID Piecewise Interpolation).
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

      INCLUDE 'spk19.inc'
      INCLUDE 'spkrec.inc'

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
!     DESCR    are the file handle and segment descriptor for an SPK
!              segment of type 19. The SPK file designated by HANDLE
!              must be open for read access.
!
!     ET       is an epoch for which a data record from a specific
!              segment is required. ET is expressed as seconds past
!              J2000 TDB.
!
! $ Detailed_Output
!
!     RECORD   is an array of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of the target body identified
!              by the input segment descriptor. The descriptor
!              specifies the center of motion and reference frame of
!              the state.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
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
!              The packet size is a function of the type 19 subtype.
!              All packets in a record have the same size.
!
! $ Parameters
!
!     See the Fortran INCLUDE file spk19.inc.
!
! $ Exceptions
!
!     1)  If the input HANDLE does not designate a loaded SPK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 19,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     3)  If the input ET value is not within the range specified
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
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     SPICE user applications normally will have no need to call this
!     routine directly. For further information, see the headers of the
!     SPICE SPK APIs
!
!        SPKEZR
!        SPKPOS
!
!     the SPK Required Reading file spk.req, and the SPICE SPK
!     tutorial.
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 19 segment.
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
!           IF ( TYPE .EQ. 19 ) THEN
!              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
!
!        Updated to support subtype 2.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     read record from type_19 SPK segment
!
!-&


!
!     SPICELIB functions
!
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

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DIRSIZ + 1 )

      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 3 )

!
!     Maximum window sizes, based on subtypes:
!
      INTEGER               S19MW0
      PARAMETER           ( S19MW0 =  ( MAXDEG + 1 )/2  )

      INTEGER               S19MW1
      PARAMETER           ( S19MW1 =  ( MAXDEG + 1 )    )

      INTEGER               S19MW2
      PARAMETER           ( S19MW2 =  ( MAXDEG + 1 )    )

!
!     Local variables
!
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      MINTIM ( ND )
      DOUBLE PRECISION      SVBTIM
      DOUBLE PRECISION      SVETIM

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
      INTEGER               MXWNSZ ( 0 : S19NST-1 )
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NPKDIR
      INTEGER               NPKT
      INTEGER               NRCPKT
      INTEGER               NREAD
      INTEGER               PKDBAS
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : S19NST-1 )
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

      LOGICAL               IVLSEL
      LOGICAL               PASS1
      LOGICAL               PRVOK
      LOGICAL               SAMIVL
      LOGICAL               SAMSEG
      LOGICAL               SVLAST
      LOGICAL               SVOK

!
!     Saved variables
!
      SAVE                  MXWNSZ
      SAVE                  PASS1
      SAVE                  PKTSZS
      SAVE                  SVBEG
      SAVE                  SVBTIM
      SAVE                  SVETIM
      SAVE                  SVHAN
      SAVE                  SVLAST
      SAVE                  SVMIIX
      SAVE                  SVMINB
      SAVE                  SVN
      SAVE                  SVNPKT
      SAVE                  SVOK
      SAVE                  SVPKDB
      SAVE                  SVPKND
      SAVE                  SVPKSZ
      SAVE                  SVSTYP
      SAVE                  SVWNSZ


!
!     Initial values
!
      DATA                  MXWNSZ  /  S19MW0, S19MW1, S19MW2 /
      DATA                  PASS1   /  .TRUE.  /
      DATA                  PKTSZS  /  S19PS0, S19PS1, S19PS2 /
      DATA                  SVBEG   /  -1      /
      DATA                  SVBTIM  /   0.D0   /
      DATA                  SVETIM  /  -1.D0   /
      DATA                  SVHAN   /   0      /
      DATA                  SVLAST  /  .FALSE. /
      DATA                  SVMIIX  /  -1      /
      DATA                  SVMINB  /  -1      /
      DATA                  SVN     /  -1      /
      DATA                  SVNPKT  /  -1      /
      DATA                  SVOK    /  .FALSE. /
      DATA                  SVPKDB  /  -1      /
      DATA                  SVPKND  /  -1      /
      DATA                  SVPKSZ  /  -1      /
      DATA                  SVSTYP  /  -1      /
      DATA                  SVWNSZ  /  -1      /



      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR19' )

!
!     Before any error checks are done, copy the status from
!     the previous call. Set the saved status variable to
!     .FALSE. here so it will be .FALSE. on exit unless this
!     call is successful.
!
      PRVOK = SVOK
      SVOK  = .FALSE.

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
!        DIRBAS    Base address of interpolation interval directory.
!
!        EADDR     Segment end DAF address.
!
!        FIRST     Index (mini-segment-relative) of first time tag in
!                  sequence transferred to to output record.
!
!        HIGH      Index (mini-segment-relative) of time tag following
!                  the tag at index LOW (see description below).
!
!        IVBIX     Index in the interpolation interval bounds array of
!                  the start time of the applicable interval.
!
!        IVLBAS    Base address of interpolation interval time bounds.
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
!        MINIIX    Interpolation interval/mini-segment index.
!
!        N         Count of interpolation intervals/mini-segments.
!
!        NDIR      Number of interpolation interval time bounds
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
!     Start with a parameter compatibility check on the first
!     pass.
!
      IF ( PASS1 ) THEN

         IF ( MAXREC .LT. MAXRSZ ) THEN

            CALL SETMSG ( 'SPK type 19 record size may be as ' &
            //            'large as #, but SPKPVN record size ' &
            //            '(defined in spkrec.inc) is #.'      )
            CALL ERRINT ( '#',  MAXRSZ                         )
            CALL ERRINT ( '#',  MAXREC                         )
            CALL SIGERR ( 'SPICE(BUG)'                         )

         END IF

!
!        Indicate the first pass was completed.
!
         PASS1 = .FALSE.

      END IF

!
!     Unpack the segment descriptor, and get the start and end
!     addresses of the segment.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      TYPE  = IC( 4 )
      BADDR = IC( 5 )
      EADDR = IC( 6 )


!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN

         CALL SETMSG( 'Request time # is outside of descriptor ' &
         //           'bounds # : #.'                           )
         CALL ERRDP ( '#',  ET                                  )
         CALL ERRDP ( '#',  DC(1)                               )
         CALL ERRDP ( '#',  DC(2)                               )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                  )
         CALL CHKOUT( 'SPKR19'                                  )
         RETURN

      END IF

!
!     Decide whether we're looking at the same segment we saw on the
!     previous call, and whether the interpolation interval used on
!     that call is still applicable.
!
!     Re-use of data from a previous call requires that the saved
!     data were set on a successful call.
!
      SAMSEG  =       ( HANDLE .EQ.  SVHAN ) &
                .AND. ( BADDR  .EQ.  SVBEG ) &
                .AND.                PRVOK

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
!           We pick the last interval containing ET. For
!           all intervals but the last, ET must be
!           less than the interval end time.
!
            IF ( SVMIIX .LT. SVN ) THEN

               SAMIVL =       ( ET .GE. SVBTIM ) &
                        .AND. ( ET .LT. SVETIM )
            ELSE

               SAMIVL =         ET .GE. SVBTIM &
                        .AND. ( ET .LE. SVETIM )

            END IF

         ELSE
!
!           We pick the first interval containing ET. For
!           all intervals but the first, ET must be
!           greater than the interval start time.
!
            IF ( SVMIIX .GT. 1 ) THEN

               SAMIVL =       ( ET  .GT. SVBTIM ) &
                        .AND. ( ET  .LE. SVETIM )

            ELSE

               SAMIVL =         ET  .GE. SVBTIM &
                        .AND. ( ET  .LE. SVETIM )

            END IF

         END IF

      END IF


      IF ( SAMSEG .AND. SAMIVL ) THEN
!
!        We're looking at the same segment as last time, and the
!        interpolation interval we looked up last time is applicable
!        for the input time ET.
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
!           - The mini-segment window size
!
         SUBTYP = SVSTYP
         PKTSIZ = SVPKSZ
         NPKT   = SVNPKT
         WNDSIZ = SVWNSZ

      ELSE
!
!        The segment and interval information for the current segment
!        must be looked up.
!
!        Perform checks on this segment.
!
!        Make sure that this really is a type 19 data segment.
!
         IF ( TYPE .NE. 19 ) THEN

            CALL SETMSG( 'You are attempting to locate type * ' &
            //           'data in a type 19 data segment.'      )
            CALL ERRINT( '*',  TYPE                             )
            CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                  )
            CALL CHKOUT( 'SPKR19'                               )
            RETURN
         END IF

!
!        Locate the interpolation interval that contains the request
!        time.
!
!        Before getting started, we need to determine which interval to
!        use if the request time lies on a boundary between two
!        intervals. The segment's interval selection flag tells us how
!        to resolve this.
!
         CALL DAFGDA ( HANDLE, EADDR-1, EADDR, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF

         ISEL   = NINT ( CONTRL(1) )
         N      = NINT ( CONTRL(2) )

         IVLSEL = ISEL .EQ. ITRUE

!
!        Determine the number of interval directory entries in the
!        segment. Note that for most SPK types, this computation is
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
!        a non-empty set of epochs following the last directory
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
         DIRBAS = EADDR - 2 - ( N + 1 ) - NDIR

!
!        The way we search the directory depends on the treatment
!        of request times that lie on interval boundaries.
!
         IF ( IVLSEL ) THEN
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            IF ( NDIR .GT. 0 ) THEN
!
!              Find the last directory entry *less than or equal to*
!              the request time. The directory entry *after* that one,
!              if such exists, is the one to pick.
!
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS

!
!              Fetch the current batch of directory entries.
!
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKR19' )
                  RETURN
               END IF

               REMAIN = NDIR - NREAD

!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               DO WHILE (       ( REMAIN        .GT. 0  ) &
                          .AND. ( BUFFER(NREAD) .LE. ET )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
!
!                 Fetch the current batch of directory entries.
!
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'SPKR19' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD

               END DO

!
!              Count the directory entries that are less than or equal
!              to ET. The number we skipped over before the final loop
!              iteration is BUFBAS-DIRBAS. The index of the group of
!              epochs containing ET exceeds the skipped directory count
!              by 1.
!
               GROUP =    BUFBAS - DIRBAS &
                       +  LSTLED ( ET, NREAD, BUFFER ) &
                       +  1

            ELSE
!
!              There's no question about which group of epochs to
!              search.
!
               GROUP = 1

            END IF

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            IVBAS  = DIRBAS - ( N + 1 )

!
!           Now find the index of the last interval boundary less than
!           or equal to ET. We'll need to read the current group of
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
               CALL CHKOUT( 'SPKR19' )
               RETURN
            END IF

!
!           Find the index of the first epoch greater than ET. The case
!           where ET matches the final epoch must be handled here,
!           since in this case no epoch exceeds ET.
!
            IVEIX =    BUFBAS - IVBAS &
                    +  LSTLED ( ET, NREAD, BUFFER ) &
                    +  1

            IVEIX = MIN ( IVEIX,  N+1 )

!
!           Backstop test:
!
            IF ( IVEIX .LT. 2 ) THEN

               CALL SETMSG ( 'IVEIX = #.' )
               CALL ERRINT ( '#', IVEIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'SPKR19'     )
               RETURN

            END IF

!
!           The epoch at index IVEIX is the end time of the
!           interpolation interval we'll use. The index of
!           the interval itself is IVEIX - 1.
!
            MINIIX = IVEIX - 1


         ELSE
!
!           IVLSEL is .FALSE., meaning we must pick the first interval
!           containing the request time.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            IF ( NDIR .GT. 0 ) THEN
!
!              Find the last directory entry *less than* the request
!              time. The directory entry *after* that one, if such
!              exists, is the one to pick.
!
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS
               REMAIN = NDIR - NREAD

!
!              Fetch the current batch of directory entries.
!
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKR19' )
                  RETURN
               END IF

!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
                DO WHILE (      ( REMAIN        .GT. 0  ) &
                          .AND. ( BUFFER(NREAD) .LT. ET )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
!
!                 Fetch the current batch of directory entries.
!
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'SPKR19' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD

               END DO

!
!              Count the directory entries that are less than ET. The
!              number we skipped over before the final loop iteration
!              is BUFBAS-DIRBAS. The index of the group of epochs
!              containing ET exceeds the skipped directory count by 1.
!
               GROUP =    BUFBAS - DIRBAS &
                       +  LSTLTD ( ET, NREAD, BUFFER ) &
                       +  1

            ELSE
!
!              There's no question about which group of epochs to
!              search.
!
               GROUP = 1

            END IF

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            IVBAS  = DIRBAS - ( N + 1 )

!
!           Now find the index of the last interval epoch less than ET.
!           We'll need to read the current group of epochs first, so
!           compute the base of the range of addresses containing this
!           group.

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
               CALL CHKOUT( 'SPKR19' )
               RETURN
            END IF

!
!           Find the index of the last epoch less than ET. The case
!           where ET matches the first epoch must be handled here,
!           since in this case no epoch precedes ET.
!
            IVBIX =    BUFBAS - IVBAS &
                    +  LSTLTD ( ET, NREAD, BUFFER )

            IVBIX = MAX ( IVBIX,  1 )

!
!           Backstop test:
!
            IF ( IVBIX .GT. N ) THEN

               CALL SETMSG ( 'IVBIX = #.' )
               CALL ERRINT ( '#', IVBIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'SPKR19'     )
               RETURN

            END IF

!
!           The epoch at index IVBIX is the begin time of the
!           interpolation interval we'll use. The index of the interval
!           itself is also IVBIX.
!
            MINIIX = IVBIX

         END IF
!
!        This is the end of the IF block that handles mini-segment
!        selection for the two possible values of IVLSEL.
!
!        Look up the begin and end pointers of the mini-segment at index
!        MINIIX. For the first N-1 mini-segments, the "end pointer"
!        of one mini-segment is the "begin" pointer of the next.
!
         BUFBAS = EADDR - 2 - ( N + 1 ) + ( MINIIX - 1 )

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+2, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
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
!        Look up the time bounds of the mini-segment at index MINIIX.
!        These bounds are used quite a bit farther on, when we save
!        them for future use.
!
         CALL DAFGDA ( HANDLE, IVBAS+MINIIX, IVBAS+MINIIX+1, MINTIM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF

!
!        From this point onward, we'll work with the mini-segment
!        that occupies the address range MINIB : MINIE.
!
!        Look up the control area of the mini-segment.
!
         CALL DAFGDA ( HANDLE, MINIE-CTRLSZ+1, MINIE, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF

!
!        Fetch the control area parameters for the mini-segment.
!
         SUBTYP  =  NINT ( CONTRL(1) )
         WNDSIZ  =  NINT ( CONTRL(2) )
         NPKT    =  NINT ( CONTRL(3) )

         IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GE. S19NST ) ) THEN

            CALL SETMSG ( 'Unexpected SPK type 19 subtype # found in ' &
            //            'type 19 segment within mini-segment #.'     )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
            CALL CHKOUT ( 'SPKR19'                                     )
            RETURN

         END IF

         PKTSIZ = PKTSZS ( SUBTYP )
         MAXWND = MXWNSZ ( SUBTYP )

!
!        Check the window size.
!
         IF ( ( WNDSIZ .LT. 2 ) .OR. ( WNDSIZ .GT. MAXWND ) ) THEN

            CALL SETMSG ( 'Window size in type 19 segment was #; must ' &
            //            'be in the range 2:# for subtype #. ' &
            //            'Mini-segment index is #.'                   )
            CALL ERRINT ( '#',  WNDSIZ                                 )
            CALL ERRINT ( '#',  MAXWND                                 )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                        )
            CALL CHKOUT ( 'SPKR19'                                     )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 19 segment was #; ' &
            //            'must be even for subtype #. ' &
            //            'Mini-segment index is #.'               )
            CALL ERRINT ( '#',  WNDSIZ                             )
            CALL ERRINT ( '#',  SUBTYP                             )
            CALL ERRINT ( '#',  MINIIX                             )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                    )
            CALL CHKOUT ( 'SPKR19'                                 )
            RETURN

         END IF

!
!        Compute the number of packet directory entries for
!        the current mini-segment/interval.
!
         NPKDIR  =  (NPKT-1) / DIRSIZ
!
!        Compute the base address of the sequence of packet
!        directory entries for the current mini-segment/interval.
!
         PKDBAS  =  MINIB - 1 + ( NPKT * ( PKTSIZ + 1 ) )

!
!        The test below is done for safety. No SPICE error s
!        should ever be detected at this point.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
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
!
         SVBTIM = MINTIM(1)
         SVETIM = MINTIM(2)

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
!
         SVSTYP = SUBTYP
         SVPKSZ = PKTSIZ
         SVNPKT = NPKT
         SVWNSZ = WNDSIZ

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
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF

         DO WHILE (       ( BUFFER(NREAD) .LT. ET ) &
                    .AND. ( REMAIN        .GT. 0  )   )

            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, DIRSIZ )
            REMAIN  =  REMAIN - NREAD
!
!           Note:  NREAD is always > 0 here.
!
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SPKR19' )
               RETURN
            END IF

         END DO

!
!        At this point, BUFBAS - PKDBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         GROUP  =     ( BUFBAS - PKDBAS ) &
                    +   LSTLTD ( ET, NREAD, BUFFER ) &
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
         CALL CHKOUT ( 'SPKR19' )
         RETURN
      END IF

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      I  =  LSTLTD (  ET,  ENDIDX-BEGIDX+1,  BUFFER  )

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
!     Now select the set of packets used for interpolation.  Note
!     that the window size is known to be even.
!
!     Unlike SPK types 8, 9, 12, and 13, for type 19 we allow the
!     window size to shrink when the window must be truncated due to
!     proximity to an interval boundary.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
!     request time is too close to one end of the coverage interval,
!     we reduce the window size, after which one endpoint of the
!     window will coincide with an endpoint of the coverage interval.
!
!     Let LSIZE be the size of the "left half" of the window:  the
!     size set of window epochs to the left of the request time.
!     We want this size to be WNDSIZ/2, but if not enough states are
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
!
      RECORD(1) =  SUBTYP

      RECORD(2) =  NRCPKT

!
!     Read the packets.
!
      CALL DAFGDA ( HANDLE, &
                    MINIB + (FIRST-1)*PKTSIZ, &
                    MINIB +      LAST*PKTSIZ  -  1, &
                    RECORD(3)                       )

!
!     Finally, add the epochs to the output record.
!     Read the sequence of time tags.
!
      BUFBAS  =  ( MINIB - 1 )  +  ( NPKT * PKTSIZ )  +  ( FIRST - 1 )

      CALL DAFGDA ( HANDLE, &
                    BUFBAS+1, &
                    BUFBAS+NRCPKT, &
                    RECORD( 3 + NRCPKT*PKTSIZ )  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKR19' )
         RETURN
      END IF

!
!     The call was successful. Record this fact so that saved
!     interval data are available for re-use.
!
      SVOK = .TRUE.

      CALL CHKOUT ( 'SPKR19' )
      RETURN
      END
