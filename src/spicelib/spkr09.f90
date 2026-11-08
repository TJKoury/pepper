!$Procedure SPKR09 ( Read SPK record from segment, type 9 )

      SUBROUTINE SPKR09 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 9
!     (Unequally spaced discrete states, interpolated by Lagrange
!     polynomials).
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
!              a SPK segment of type 9.
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
!                 | epochs 1--n          |
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
!     2)  If the segment specified by DESCR is not of data types 9 or
!         13, the error SPICE(WRONGSPKTYPE) is signaled.
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
!     structure of a data type 9 segment.
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
!           IF ( TYPE .EQ. 9 ) THEN
!              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD )
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
!        Data type check was relaxed to enable reading type 13
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
!     read record from type_9 SPK segment
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LSTLTD

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

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DIRSIZ + 1 )


!
!     Local variables
!
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( 2  )
      DOUBLE PRECISION      DC     ( ND )

      INTEGER               BEGIDX
      INTEGER               BEGIN
      INTEGER               BUFBAS
      INTEGER               DEGREE
      INTEGER               DIRBAS
      INTEGER               END
      INTEGER               ENDIDX
      INTEGER               FIRST
      INTEGER               GROUP
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               LAST
      INTEGER               LOW
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NEAR
      INTEGER               NREAD
      INTEGER               REMAIN
      INTEGER               START
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ


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
!     Make sure that this really is a type 9 or type 13 data segment.
!
      IF (  ( TYPE .NE. 09 ) .AND. ( TYPE .NE. 13 )  ) THEN
         CALL CHKIN ( 'SPKR09'                                        )
         CALL SETMSG( 'You are attempting to locate type 9 or type ' // &
                      '13 data in a type # data segment.'             )
         CALL ERRINT( '#',  TYPE                                      )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                           )
         CALL CHKOUT( 'SPKR09'                                        )
         RETURN
      END IF

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN
         CALL CHKIN ( 'SPKR09'                                      )
         CALL SETMSG( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         CALL ERRDP ( '#',  ET                                      )
         CALL ERRDP ( '#',  DC(1)                                   )
         CALL ERRDP ( '#',  DC(2)                                   )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         CALL CHKOUT( 'SPKR09'                                      )
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
!               ( L - 1 ) / 7         [integer division]
!
!            where L is the declared length of the argument RECORD.
!
!        5)  There are at least as many epochs in the segment as the
!            the number of points required to define an interpolating
!            polynomial of the specified degree.
!
!
!     We'll need the last two items before we can determine which
!     states make up our output record.
!
!
      CALL DAFGDA ( HANDLE, END-1, END, CONTRL )

      DEGREE  =  NINT ( CONTRL(1) )
      N       =  NINT ( CONTRL(2) )

      WNDSIZ  =  DEGREE + 1

!
!     We'll now select the set of states that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      NDIR    =  (N-1) / DIRSIZ
      DIRBAS  =  END  -  NDIR  -  2

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
         NREAD   =  MIN ( NDIR, BUFSIZ )
         REMAIN  =  NDIR - NREAD

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

         DO WHILE (       ( BUFFER(NREAD) .LT. ET ) &
                    .AND. ( REMAIN        .GT. 0  )   )

            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, BUFSIZ )
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
      I     =  LSTLTD (  ET,  ENDIDX-BEGIDX+1,  BUFFER  )

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
!     Now select the set of states used for interpolation.
!
      IF ( ODD(WNDSIZ) ) THEN
!
!        Find the index of the state whose epoch is closest to the
!        input epoch.  The index I is in the range [0, DIRSIZ],
!        since ENDIDX - BEGIDX never exceeds DIRSIZ, and ET is
!        never larger than the (ENDIDX-BEGIDX+1)th element of the
!        buffer.
!
         IF ( I .EQ. 0 ) THEN
!
!           This can happen only if the request time matches the
!           first time tag of the segment.
!
            NEAR = LOW

         ELSE IF (       DABS(  ET  -  BUFFER(I)    ) &
                   .LT.  DABS(  ET  -  BUFFER(I+1)  )    )  THEN

            NEAR = LOW

         ELSE
            NEAR = HIGH
         END IF

!
!        The epochs whose index is NEAR is the (WNDSIZ/2 + 1)th
!        of the interpolating set, unless the request time is too close
!        to the end of the coverage interval, in which case one endpoint
!        of the window will coincide with an endpoint of the coverage
!        interval.
!
         FIRST =  MIN (  MAX ( NEAR - DEGREE/2,  1),   N-DEGREE  )

         LAST  =  FIRST  +  DEGREE

      ELSE
!
!        The group size is even.
!
!        The bracketing epochs we've found are the (WNDSIZ/2)th
!        and (WNDSIZ/2 + 1)th of the interpolating set, unless the
!        request time is too close to the end of the coverage interval,
!        in which case one endpoint of the window will coincide with
!        an endpoint of the coverage interval.
!
         FIRST =  MIN (  MAX ( LOW - DEGREE/2,  1 ),   N-DEGREE  )

         LAST  =  FIRST  +  DEGREE

      END IF

!
!     Put the size of the group of states into the output record.
!
      RECORD(1) =  WNDSIZ

!
!     Read the states.
!
      CALL DAFGDA ( HANDLE, &
                    BEGIN + (FIRST-1)*STATSZ, &
                    BEGIN +      LAST*STATSZ  -  1, &
                    RECORD(2)                       )

!
!     Finally, add the epochs to the output record.
!
      START  =  BEGIN   +  N * STATSZ  +  FIRST - 2

      CALL DAFGDA (  HANDLE, &
                     START + 1, &
                     START + WNDSIZ, &
                     RECORD( 2 + WNDSIZ*STATSZ )   )

      RETURN
      END
