!$Procedure SPKR18 ( Read SPK record from segment, type 18 )

      SUBROUTINE SPKR18 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 18
!     (MEX/Rosetta Orbit file interpolation).
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

      INCLUDE 'spk18.inc'

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
!              a SPK segment of type 18.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is a set of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some reference frame.
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
!              The packet size is a function of the subtype code.
!              All packets in a record have the same size.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input HANDLE does not designate a loaded SPK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 18,
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
!     7)  If the input segment contains fewer than 2 packets, the
!         error SPICE(TOOFEWSTATES) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 18 segment.
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
!           IF ( TYPE .EQ. 18 ) THEN
!              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD )
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
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
!
!        An error check was added for segment packet counts
!        less than 2.
!
!        An in-line comment regarding deducibility of record size from
!        segment subtype was removed. The comment now says the actual
!        count of packets in the output record is inserted into the
!        record.
!
!-    SPICELIB Version 1.0.0, 04-SEP-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_18 SPK segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
!
!        An error check was added for segment packet counts
!        less than 2.
!
!-&


!
!     SPICELIB functions
!
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
!     Maximum polynomial degree:
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

!
!     Local variables
!
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )

      INTEGER               BEGIDX
      INTEGER               BEGIN
      INTEGER               BUFBAS
      INTEGER               DIRBAS
      INTEGER               END
      INTEGER               ENDIDX
      INTEGER               FIRST
      INTEGER               GROUP
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               LAST
      INTEGER               LSIZE
      INTEGER               LOW
      INTEGER               MAXWND
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NREAD
      INTEGER               PACKSZ
      INTEGER               REMAIN
      INTEGER               RSIZE
      INTEGER               START
      INTEGER               SUBTYP
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ


      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR18' )

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      TYPE  = IC( 4 )
      BEGIN = IC( 5 )
      END   = IC( 6 )

!
!     Make sure that this really is a type 18 data segment.
!
      IF ( TYPE .NE. 18 ) THEN
         CALL SETMSG( 'You are attempting to locate type * ' // &
                      'data in a type 18 data segment.'      )
         CALL ERRINT( '*',  TYPE                             )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                  )
         CALL CHKOUT( 'SPKR18'                               )
         RETURN
      END IF

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN
         CALL SETMSG( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         CALL ERRDP ( '#',  ET                                      )
         CALL ERRDP ( '#',  DC(1)                                   )
         CALL ERRDP ( '#',  DC(2)                                   )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         CALL CHKOUT( 'SPKR18'                                      )
         RETURN

      END IF

!     We'll need the last two items before we can determine which
!     packets make up our output record.
!
      CALL DAFGDA ( HANDLE, END-CTRLSZ+1, END, CONTRL )

!
!     Check the FAILED flag just in case HANDLE is not attached to
!     any DAF file and the error action is not set to ABORT. You need
!     need to do this only after the first call to DAFGDA.
!
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKR18' )
         RETURN
      END IF

      SUBTYP  =  NINT ( CONTRL(1) )
      WNDSIZ  =  NINT ( CONTRL(2) )
      N       =  NINT ( CONTRL(3) )

      IF ( N .LT. 2 )  THEN

         CALL SETMSG( 'Packet count # is less than the minimum '   // &
                      'valid value, which is 2.'                   )
         CALL ERRINT( '#',  N                                      )
         CALL SIGERR( 'SPICE(TOOFEWSTATES)'                        )
         CALL CHKOUT( 'SPKR18'                                     )
         RETURN

      END IF

!
!     From this point onward, we assume the segment was constructed
!     correctly.  In particular, we assume:
!
!        1)  The first and last epochs in the segment define a time
!            interval that contains the interval defined by the segment
!            descriptor's time bounds.
!
!        2)  The segment descriptor's time bounds are in order and are
!            distinct.
!
!        3)  The epochs in the segment are in strictly increasing
!            order.
!
!        4)  The degree of the interpolating polynomial specified by
!            the segment is at least 1 and is no larger than
!
!               MAXDEG
!


!
!     Set the packet size, which is a function of the subtype.
!
      IF ( SUBTYP .EQ. S18TP0 ) THEN

         PACKSZ = S18PS0

      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         PACKSZ = S18PS1

      ELSE

         CALL SETMSG ( 'Unexpected SPK type 18 subtype # found in ' // &
                       'type 18 segment.'                           )
         CALL ERRINT ( '#',  SUBTYP                                 )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'SPKR18'                                     )
         RETURN

      END IF

!
!     Check the window size.
!
      IF ( WNDSIZ .LE. 0 ) THEN

         CALL SETMSG ( 'Window size in type 18 segment was #; must ' // &
                       'be positive.'                                )
         CALL ERRINT ( '#',  SUBTYP                                  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                         )
         CALL CHKOUT ( 'SPKR18'                                      )
         RETURN

      END IF


      IF ( SUBTYP .EQ. S18TP0 ) THEN

         MAXWND  =  (MAXDEG+1)/2

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; max '// &
                          'allowed value is # for subtype 0 (Hermite,'// &
                          ' 12-element packets).'                     )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'SPKR18'                                    )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; '    // &
                          'must be even for subtype 0 (Hermite,'      // &
                          ' 12-element packets).'                     )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'SPKR18'                                    )
            RETURN

         END IF


      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         MAXWND  =  MAXDEG + 1

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; max '// &
                          'allowed value is # for subtype 1 '         // &
                          '(Lagrange, 6-element packets).'            )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'SPKR18'                                    )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; '// &
                          'must be even for subtype 1 '           // &
                          '(Lagrange, 6-element packets).'        )
            CALL ERRINT ( '#',  WNDSIZ                            )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                   )
            CALL CHKOUT ( 'SPKR18'                                )
            RETURN

         END IF

      ELSE

         CALL SETMSG ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle new subtypes.'            )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'SPKR18'                                     )
         RETURN

      END IF

!
!     We'll now select the set of packets that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      NDIR    =  (N-1) / DIRSIZ
      DIRBAS  =  END  -  NDIR  -  CTRLSZ

      IF ( NDIR .EQ. 0 ) THEN
!
!        There's no mystery about which group of epochs to search.
!
         GROUP  = 1

      ELSE
!
!        There's at least one directory.  Find the first directory
!        whose time is greater than or equal to the request time, if
!        there is such a directory.  We'll search linearly through the
!        directory entries, reading up to BUFSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         BUFBAS  =  DIRBAS
         NREAD   =  MIN ( NDIR, DIRSIZ )
         REMAIN  =  NDIR - NREAD

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

         DO WHILE (       ( BUFFER(NREAD) .LT. ET ) &
                    .AND. ( REMAIN        .GT. 0  )   )

            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, DIRSIZ )
            REMAIN  =  REMAIN - NREAD
!
!           Note:  NREAD is always > 0 here.
!
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

         END DO

!
!        At this point, BUFBAS - DIRBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         GROUP  =     ( BUFBAS - DIRBAS ) &
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
         ENDIDX  =  MIN ( N, DIRSIZ )

      ELSE
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         BEGIDX  =      (  GROUP  - 1 ) * DIRSIZ
         ENDIDX  =  MIN (  BEGIDX + DIRSIZ,  N  )

      END IF


      TIMBAS  =  DIRBAS - N

      CALL DAFGDA ( HANDLE, TIMBAS+BEGIDX, TIMBAS+ENDIDX, BUFFER )

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
!     Unlike SPK types 8, 9, 12, and 13, for type 18 we adjust
!     the window size to keep the request time within the central
!     interval of the window.
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
!     Put the subtype and actual window size, which is the number of
!     packets in the record, into the output record.
!
      RECORD(1) =  SUBTYP

      RECORD(2) =  WNDSIZ

!
!     Read the packets.
!
      CALL DAFGDA ( HANDLE, &
                    BEGIN + (FIRST-1)*PACKSZ, &
                    BEGIN +      LAST*PACKSZ  -  1, &
                    RECORD(3)                       )

!
!     Finally, add the epochs to the output record.
!
      START  =  BEGIN   +  N * PACKSZ  +  FIRST - 2

      CALL DAFGDA (  HANDLE, &
                     START + 1, &
                     START + WNDSIZ, &
                     RECORD( 3 + WNDSIZ*PACKSZ )   )

      CALL CHKOUT ( 'SPKR18' )
      RETURN
      END
