!$Procedure CKR02 ( C-kernel, read pointing record, data type 2 )

      SUBROUTINE CKR02 ( HANDLE, DESCR, SCLKDP, TOL, RECORD, FOUND )

! $ Abstract
!
!     Read a pointing record from a CK segment, data type 2.
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
!     CK
!     DAF
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      RECORD ( * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     SCLKDP     I   Spacecraft clock time.
!     TOL        I   Time tolerance
!     RECORD     O   Pointing data record.
!     FOUND      O   .TRUE. when data is found.
!
! $ Detailed_Input
!
!     HANDLE   is the integer handle of the CK file containing the
!              segment.
!
!     DESCR    is the descriptor of the segment.
!
!     SCLKDP   is the encoded spacecraft clock time for which
!              pointing is being requested.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock.
!
!              When SCLKDP falls within the bounds of one of the
!              intervals then the tolerance has no effect. However,
!              if the request time is not in one of the intervals
!              then the tolerance is used to determine if pointing
!              at one of the interval endpoints should be returned.
!
! $ Detailed_Output
!
!     RECORD   is the pointing record. Contents are as follows:
!
!                 RECORD( 1  ) = Start time of interval.
!                 RECORD( 2  ) = Time for which pointing was found.
!                 RECORD( 3  ) = Seconds per tick rate.
!
!                 RECORD( 4  ) = q0
!                 RECORD( 5  ) = q1
!                 RECORD( 6  ) = q2
!                 RECORD( 7  ) = q3
!
!                 RECORD( 8  ) = av1
!                 RECORD( 9  ) = av2
!                 RECORD( 10 ) = av3
!
!              The quantities q0 - q3 are the components of the
!              quaternion that represents the C-matrix associated with
!              the start time of the interval. The quantities av1,
!              av2, and av3 represent the angular velocity vector of
!              the interval. The components of the angular velocity
!              vector are specified relative to the inertial reference
!              frame of the segment.
!
!     FOUND    is .TRUE. if a record was found to satisfy the pointing
!              request.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to any file that is
!         currently known to be open, an error is signaled by a routine
!         in the call tree of this routine.
!
!     2)  If DESCR is not a valid, packed descriptor of a segment in
!         the CK file specified by HANDLE, the results of this routine
!         are unpredictable.
!
!     3)  If the segment is not of data type 2, as specified in the
!         third integer component of the segment descriptor,
!         the error SPICE(WRONGDATATYPE) is signaled.
!
! $ Files
!
!     The file containing the segment is specified by its handle, and
!     should be opened for read, either by CKLPF or DAFOPR.
!
! $ Particulars
!
!     See the CK Required Reading file for a detailed description of
!     the structure of a type 2 pointing segment.
!
!     This routine searches a type 2 segment and determines if the
!     request for pointing can be satisfied by the segment. If so,
!     then it returns information in the array RECORD that CKE02 uses
!     to evaluate the pointing at the time for which pointing was found.
!
!     When the time for which pointing was requested falls within one
!     of the intervals then the returned time is the same as the
!     requested time. However, when the request time is not within any
!     of the intervals then the returned time will be the interval
!     endpoint closest to the request time, provided that endpoint is
!     within the tolerance specified by the user.
!
! $ Examples
!
!     The CKRnn routines are usually used in tandem with the CKEnn
!     routines, which evaluate the record returned by CKRnn to give
!     the pointing information and output time.
!
!     The following code fragment searches backwards through a file
!     (attached to HANDLE) for all segments applicable to the Voyager 2
!     wide angle camera, for a particular spacecraft clock time, that
!     are of data types 1 or 2. It then evaluates the pointing for that
!     epoch and prints the result.
!
!     The search performed here does not mimic the behavior of the CK
!     reader APIs CKGP and CKGPAV, which consider data from multiple CK
!     files, when available. See the CK Required reading for details.
!
!           SC     = -32
!           INST   = -32002
!     C
!     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
!     C
!           CALL FURNSH ( 'VGR_SCLK.TSC'        )
!           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
!     C
!     C     Get the spacecraft clock time. Must encode it for use
!     C     in the C-kernel.
!     C
!           WRITE (*,*) 'Enter spacecraft clock time string:'
!           READ (*,FMT='(A)') SCLKCH
!           CALL SCENCD ( SC, SCLKCH, SCLKDP )
!
!     C
!     C     Search backwards from the end of the CK file through all
!     C     of the segments.
!     C
!           CALL DAFBBS ( HANDLE )
!           CALL DAFFPA ( SFND   )
!
!           DO WHILE ( SFND )
!
!              CALL DAFGN ( IDENT                 )
!              CALL DAFGS ( DESCR                 )
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!              IF ( INST          .EQ. ICD( 1 )  .AND.
!          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
!          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
!
!                 DTYPE = ICD ( 3 )
!
!                 IF ( DTYPE .EQ. 1 ) THEN
!
!                    CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                      RECORD, FOUND                       )
!
!                    IF ( FOUND ) THEN
!                       CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
!                    END IF
!
!                 ELSE  IF ( DTYPE .EQ. 2 ) THEN
!
!                    CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL,
!          .                      RECORD, FOUND )
!
!                    IF ( FOUND ) THEN
!                       CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
!                    END IF
!
!                 END IF
!
!                 IF ( FOUND ) THEN
!
!                    WRITE (*,*) 'Segment descriptor and identifier:'
!                    WRITE (*,*) DCD, ICD
!                    WRITE (*,*) IDENT
!
!                    WRITE (*,*) 'C-matrix:'
!                    WRITE (*,*) CMAT
!
!                 END IF
!
!              END IF
!
!              CALL DAFFPA ( SFND )
!
!           END DO
!
! $ Restrictions
!
!     1)  The file containing the segment should be opened for read,
!         either by CKLPF or DAFOPR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.2, 06-JUL-2021 (NJB) (JDR)
!
!        Updated code example to use backwards search. Added
!        note regarding difference between this search and those
!        performed by the CK reader APIs CKGP and CKGPAV.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
!
!        Replaced references to LDPOOL with references
!        to FURNSH.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
!
!-&


! $ Index_Entries
!
!     read CK type_2 pointing data record
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      INTEGER               LSTLED

!
!     Local parameters
!
!        DIRSIZ     is the directory size.
!
!        NDC        is the number of double precision components in an
!                   unpacked C-kernel segment descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel segment descriptor.
!
!        PSIZ       is the number of double precision numbers making up
!                   the record containing the quaternion, angular
!                   velocity vector, and seconds per tick rate.
!
!        DTYPE      is the data type of the segment that this routine
!                   operates on.
!
!
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )

      INTEGER               PSIZ
      PARAMETER           ( PSIZ   = 8 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE =  2 )

!
!     Local variables
!
      INTEGER               ICD    ( NIC )
      INTEGER               BEG
      INTEGER               END
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               GROUP
      INTEGER               DIRLOC
      INTEGER               REMAIN
      INTEGER               SKIP
      INTEGER               GRPNDX
      INTEGER               INDEX
      INTEGER               STPLOC
      INTEGER               ARRSIZ
      INTEGER               I
      INTEGER               N

      DOUBLE PRECISION      DCD    ( NDC    )
      DOUBLE PRECISION      BUFFER ( DIRSIZ )
      DOUBLE PRECISION      PREC   ( PSIZ   )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      STOPI
      DOUBLE PRECISION      DIFF1
      DOUBLE PRECISION      DIFF2

      LOGICAL               FND



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKR02' )
      END IF

!
!     To minimize the number of file reads performed during the search,
!     a buffer of 100 double precision numbers is used to read the SCLK
!     times from the C-kernel.  If there are 10,001 or fewer pointing
!     records, at most four reads will be needed to satisfy the request:
!     one to read in 100 or fewer directory times, one to read 100 or
!     fewer interval start times, one to read from the stop times, and
!     then, after the appropriate record has been located, one to read
!     the pointing record.
!
!     One more read would be required for every other group of 10,000
!     records in the segment.
!


!
!     Start off with FOUND equal to false.
!
      FOUND = .FALSE.
!
!     We need to look at a few of the descriptor components.
!
!     The unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!

      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )

!
!     Check to make sure that the segment is type 2.
!
      IF ( ICD( 3 ) .NE. DTYPE ) THEN

         CALL SETMSG ( 'The segment is not a type 2 segment.  ' // &
                       'Type is #'                                 )
         CALL ERRINT ( '#', ICD(3)                                 )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKR02'                                     )
         RETURN

      END IF

!
!     The beginning and ending addresses of the segment are in the
!     descriptor.
!
      BEG = ICD( 5 )
      END = ICD( 6 )

!
!     Get the number of records in this segment, and from that determine
!     the number of directory epochs.
!
!
!        Based on the structure of a type 2 segment, the size of a
!        segment with N pointing intervals is given as follows:
!
!           ARRSIZ  =  PSIZ * N  +  2 * N  +  ( N-1 ) / 100       (1)
!
!        In the above equation PSIZ is eight and integer arithmetic is
!        used.  This equation is equivalent to:
!
!
!           100 * ARRSIZ  =  1000 * N  + ( N-1 ) * 100            (2)
!                                        -------
!                                          100
!
!        If we can eliminate the integer division then, since all of
!        the other values represent whole numbers, we can solve the
!        equation for N in terms of ARRSIZ by using double precision
!        arithmetic and then rounding the result to the nearest integer.
!
!        This next equation uses double precision arithmetic and is
!        equivalent to (2):
!
!           100 * ARRSIZ  = 1000 * N + ( N-1 ) - ( N-1 ) MOD 100  (3)
!
!        Which means:
!
!           100 * ARRSIZ + 1     ( N-1 ) MOD 100
!           ----------------  +  ---------------   =   N          (4)
!                1001                 1001
!
!         Since the second term on the left side of (4) is always less
!         than 0.1, the first term will always round to the correct
!         value of N.
!
      ARRSIZ = END - BEG + 1

      NREC   = NINT (  ( 100.D0 * (DBLE(ARRSIZ)) + 1.D0 ) / 1001.D0  )

      NDIR   = ( NREC - 1 ) / DIRSIZ

!
!     The directory epochs narrow down the search to a group of DIRSIZ
!     or fewer records.
!
!     There is only one group if there are no directory epochs.
!
      IF ( NDIR .EQ. 0 ) THEN
         GROUP = 1

      ELSE
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, we need to go through all of the
!        pointing numbers (PSIZ*NREC of them), then through all of
!        the SCLK start and stop times (2*NREC more) to get to the
!        first SCLK directory.
!
         DIRLOC = BEG + ( PSIZ + 2 ) * NREC

!
!        Locate the last directory epoch less than or equal to SCLKDP.
!
!        Read in as many as DIRSIZ directory epochs at a time for
!        comparison.
!
         FND    = .FALSE.
         REMAIN =  NDIR
         GROUP  =  0

         DO WHILE ( .NOT. FND )
!
!           The number of records to read in the buffer.
!
            N = MIN( REMAIN, DIRSIZ )

            CALL DAFGDA ( HANDLE, DIRLOC, DIRLOC + N - 1, BUFFER )

            REMAIN = REMAIN - N

!
!           Determine the last directory element in BUFFER that's less
!           than or equal to SCLKDP.
!
!           If we reach the end of the directories, and still haven't
!           found one bigger than the epoch, the group is the last group
!           in the segment.
!
!           Otherwise keep looking.
!
            I = LSTLED( SCLKDP, N, BUFFER )

            IF ( I .LT. N ) THEN
               GROUP =  GROUP + I + 1
               FND   = .TRUE.

            ELSE IF ( REMAIN .EQ. 0 ) THEN
               GROUP =  NDIR + 1
               FND   = .TRUE.

            ELSE
               DIRLOC = DIRLOC + N
               GROUP  = GROUP  + N

            END IF

         END DO

      END IF

!
!     Now we know which group of DIRSIZ (or less) times to look at.
!     Out of the NREC START times, the number that we should skip over
!     to get to the proper group is DIRSIZ*( GROUP - 1 ).
!
      SKIP = DIRSIZ * ( GROUP - 1 )

!
!     From this we can compute the index into the segment of the group
!     of times we want.  From the beginning, we need to pass through
!     PSIZ*NREC pointing numbers to get to the first START time.
!     Then we skip over the number just computed above.
!
      GRPNDX = BEG + NREC * ( PSIZ ) + SKIP

!
!     The number of times that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
      N = MIN( DIRSIZ, NREC - SKIP )

      CALL DAFGDA ( HANDLE, GRPNDX, GRPNDX + N - 1, BUFFER )

!
!     Find the largest time in the group less than or equal to the input
!     time.
!
      I = LSTLED ( SCLKDP, N, BUFFER )

!
!     If the request time does not fall into one of the intervals, then
!     there are several cases in which this routine can return an
!     endpoint of an interval.
!
!        1)  If I = 0 then the request time falls before the first START
!            time in the group.  Because of the way that the directory
!            is constructed we already know that the preceding STOP
!            time is not the right one so all we have to check is if
!            SCLKDP + TOL is greater than or equal to the first START
!            time of the group.
!
!        2)  If I = N and the request time is not in the Nth interval
!            then we know that the request time is after the last STOP
!            time in the group.  Because of the way that the directory
!            is constructed we already know that the following START
!            time is not the right one so all we have to check is if
!            SCLKDP - TOL is less than or equal to the last STOP time
!            of the group.
!
!        3)  Finally, if I is between 1 and N-1 and the request time
!            does not fall in any of the intervals then we need to
!            return the closer of STOP(I) or START(I+1) if it is
!            within TOL of SCLKDP.
!
!
!     If SCLKDP is less than the first time in BUFFER then check to see
!     if we want the first START time in the group.
!
      IF ( I .EQ. 0 ) THEN

         IF ( ( SCLKDP + TOL ) .GE. BUFFER(1) ) THEN

            FOUND  = .TRUE.
            START  =  BUFFER(1)
            CLKOUT =  BUFFER(1)
            INDEX  =  1

         ELSE

            CALL CHKOUT ( 'CKR02' )
            RETURN

         END IF

      ELSE

!
!        I is not equal to zero. Determine if the request time falls
!        within the Ith interval.
!
         STPLOC = BEG + NREC*( PSIZ + 1 ) + SKIP + I - 1

         CALL DAFGDA ( HANDLE, STPLOC, STPLOC, STOPI )

         IF ( SCLKDP .LE. STOPI )  THEN

            FOUND  = .TRUE.
            START  =  BUFFER(I)
            CLKOUT =  SCLKDP
            INDEX  =  I

         ELSE

!
!           The request time does not fall within the interval. Check
!           to see if the Ith STOP time or the (I+1)th START time
!           satisfy the request.
!
!           If I = N then we need to consider only the STOP time
!           because of the way that the directory is constructed.
!
            IF ( I .EQ. N ) THEN

               IF ( ( SCLKDP - TOL ) .LE. STOPI ) THEN

                  FOUND  = .TRUE.
                  START  =  BUFFER(I)
                  CLKOUT =  STOPI
                  INDEX  =  I

               ELSE

                  CALL CHKOUT ( 'CKR02' )
                  RETURN

               END IF

            ELSE

!
!              Find which time SCLKDP is closest to and then see if
!              it is within the tolerance.
!
               DIFF1 = SCLKDP      - STOPI
               DIFF2 = BUFFER(I+1) - SCLKDP

               IF ( MIN ( DIFF1, DIFF2 ) .LE. TOL ) THEN

                  FOUND = .TRUE.
!
!                 Notice that if the request time is equidistant from
!                 the STOP and START time the START time will be chosen.
!
                  IF ( DIFF2 .LE. DIFF1 ) THEN

                     START  = BUFFER(I+1)
                     CLKOUT = BUFFER(I+1)
                     INDEX  = I+1

                  ELSE

                     START  = BUFFER(I)
                     CLKOUT = STOPI
                     INDEX  = I

                  END IF

               ELSE

                  CALL CHKOUT ( 'CKR02' )
                  RETURN

               END IF

            END IF

         END IF

      END IF
!
!
!     Now we know the exact record that we want and can begin
!     constructing the output record.
!
!     RECORD( 1 ) holds the interval start time.
!     RECORD( 2 ) holds the time for which pointing was found (CLKOUT).
!
      RECORD( 1 ) =  START
      RECORD( 2 ) =  CLKOUT

!
!     We need the pointing record out of GROUP indexed by INDEX.
!     This group of size DIRSIZ is SKIP records into the beginning
!     of the segment. And each record is PSIZ big.
!
      N = BEG + PSIZ * ( SKIP + INDEX - 1 )

      CALL DAFGDA ( HANDLE, N, N + PSIZ - 1, PREC )

      RECORD ( 3 ) = PREC ( PSIZ )

      CALL VEQUG ( PREC, 7, RECORD( 4 ) )


!
!     That is all.
!
      CALL CHKOUT ( 'CKR02' )
      RETURN
      END
