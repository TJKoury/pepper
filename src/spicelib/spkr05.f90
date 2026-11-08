!$Procedure SPKR05 ( Read SPK record from segment, type 5 )

      SUBROUTINE SPKR05 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 5
!     ( two body propagation between discrete state vectors ).
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
!              the type 05 SPK segment to be read.
!
!     ET       is a target epoch, specified as ephemeris seconds past
!              J2000, for which a data record from the segment is
!              required.
!
! $ Detailed_Output
!
!     RECORD   is a logical record from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
!
!              The structure of RECORD is:
!
!                  RECORD(1)
!                     .            state of the body at epoch 1.
!                     .
!                     .
!                  RECORD(6)
!
!                  RECORD(7)
!                     .
!                     .            state of the body at epoch 2.
!                     .
!                  RECORD(12)
!                  RECORD(13)      epoch 1 in seconds past 2000.
!                  RECORD(14)      epoch 2 in seconds past 2000.
!                  RECORD(15)      GM for the center of motion.
!
!
!              Epoch 1 and epoch 2 are the times in the segment that
!              bracket ET. If ET is less than the first time in the
!              segment then both epochs 1 and 2 are equal to the
!              first time. And if ET is greater than the last time
!              then, epochs 1 and 2 are set equal to this last time.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the segment specified by DESCR is not of data type 05,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     2)  No error is signaled if ET is outside the time bounds of
!         the segment. The output RECORD will contain epochs and the
!         associated states which satisfy the rules stated above.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This routine reads the segment specified by DESCR from the SPK
!     file attached to HANDLE to locate the two epochs in the segment
!     that bracket the input ET. It then returns a logical record which
!     contains these times and their associated states, and also the
!     mass of the center of motion. The routine makes explicit use of
!     the structure of the type 05 data segment to locate this data.
!
!     See the section of the SPK Required Reading on data type 05 for
!     a description of the structure of a type 05 segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRnn
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!           IF ( FOUND ) THEN
!
!     C
!     C        Look at parts of the descriptor.
!     C
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!              CENTER = ICD( 2 )
!              REF    = ICD( 3 )
!              TYPE   = ICD( 4 )
!
!              IF ( TYPE .EQ. 05 ) THEN
!
!                 CALL SPKR05 ( HANDLE, DESCR, ET, RECORD )
!                     .
!                     .  Look at the RECORD data.
!                     .
!              END IF
!
!           END IF
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
!     J. Diaz del Rio    (ODC Space)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     read record from type_5 SPK segment
!
!-&


!
!     Local parameters
!
      INTEGER               SIZE
      PARAMETER           ( SIZE    = 100 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = SIZE )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ  = SIZE )

      INTEGER               STASIZ
      PARAMETER           ( STASIZ =    6 )

      INTEGER               ND
      PARAMETER           ( ND =        2 )

      INTEGER               NI
      PARAMETER           ( NI =        6 )

!
!     SPICELIB functions
!
      INTEGER               LSTLTD
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      DC       (   ND )
      DOUBLE PRECISION      DATA     ( SIZE )

      INTEGER               IC       (   NI )
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               TYPE
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               GROUP
      INTEGER               DIRLOC
      INTEGER               REMAIN
      INTEGER               SKIP
      INTEGER               GRPADD
      INTEGER               ADDRSS
      INTEGER               I
      INTEGER               N

      LOGICAL               FND



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKR05' )
      END IF

!
!     Unpack the segment descriptor.
!
      CALL DAFUS ( DESCR, ND, NI, DC, IC )

      TYPE  = IC(4)
      BEGIN = IC(5)
      END   = IC(6)

!
!     Make sure that this really is a type 5 data segment.
!
      IF ( TYPE .NE. 05 ) THEN
         CALL SETMSG( 'You are attempting to locate type 5 '      // &
                      'data in a type # data segment.'              )
         CALL ERRINT( '#',  TYPE                                    )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                         )
         CALL CHKOUT( 'SPKR05'                                      )
         RETURN
      END IF

!
!     Get the number of records in the segment. While we're at it,
!     get the GM of the central body (it's adjacent to NREC)
!     since we'll need it anyway. Put it where it belongs, at the
!     end of the output record.
!
      CALL DAFGDA ( HANDLE, END-1, END, DATA )

      NREC       = NINT ( DATA(2) )
      RECORD(15) =        DATA(1)

!
!     From the number of records, we can compute the number of
!     directory epochs.
!
      NDIR = NREC / DIRSIZ

!
!     The directory epochs narrow down the search to a group of DIRSIZ
!     or fewer records. Because the Ith directory epoch is the I*100th
!     epoch, the Ith group will contain epochs ((I-1)*100 + 1) through
!     (I*100).  For example:
!                            group   first epoch #   last epoch #
!                            -----   -------------   ------------
!                              1               1          100
!                              2             101          200
!                              .               .            .
!                              .               .            .
!                             10             901         1000
!                              .               .            .
!                              .               .            .
!                              N     (N-1)*100+1        N*100

      IF ( NDIR .EQ. 0 ) THEN
!
!        There is only one group if there are no directory epochs.
!
         GROUP = 1

      ELSE
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, we need to go through all of the
!        NREC states and epochs.
!
         DIRLOC = BEGIN + ( STASIZ + 1 ) * NREC

!
!        Determine which group of DIRSIZ times to search, by finding
!        the last directory epoch that is less than ET.
!
         FND    = .FALSE.
         REMAIN =  NDIR
         GROUP  =  0

         DO WHILE ( .NOT. FND )
!
!           Read in as many as BUFSIZ directory epochs at a time
!           for comparison.
!
            N = MIN( REMAIN, BUFSIZ )

            CALL DAFGDA ( HANDLE, DIRLOC, DIRLOC + N - 1, DATA )

            REMAIN = REMAIN - N

!
!           Determine the last directory element in DATA that's less
!           than ET.
!
!           If we reach the end of the directories, and still haven't
!           found one bigger than the epoch, the group is the last group
!           in the segment.
!
!           Otherwise keep looking.
!
!
            I = LSTLTD( ET, N, DATA )

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
!     Now we know which group of DIRSIZ (or less) epochs to look at.
!     Out of the NREC epochs, the number that we should skip over
!     to get to the proper group is DIRSIZ * ( GROUP - 1 ).
!
      SKIP = DIRSIZ * ( GROUP - 1 )

!
!     From this we can compute the index into the segment of the group
!     of times we want.  From the beginning, we need to pass through
!     STASIZ * NREC state numbers to get to the first epoch. Then we
!     skip over the number just computed above.
!
      GRPADD = BEGIN + NREC * ( STASIZ ) + SKIP

!
!     The number of epochs that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
!     If there are no times in the last group then the time that we
!     are looking for is the same as the last directory epoch.
!     We should not try to read in this instance.
!
      N = MIN ( DIRSIZ, NREC - SKIP )

      IF ( N .NE. 0 ) THEN

         CALL DAFGDA ( HANDLE, GRPADD, GRPADD + N - 1, DATA )
!
!        Find the index of the largest time in the group that is less
!        than the input time.
!
         I = LSTLTD ( ET, N, DATA )

      ELSE
!
!        If we are here it means that ET is greater then the last
!        time in the segment and there are no elements in the last
!        group.  This can occur when the number of epochs is a multiple
!        DIRSIZ.
!
!        By setting N equal to I we can handle this case in the
!        same branch as when there are elements in the last group.
!        This is because the DATA array still contains the directory
!        epochs and I is pointing at the last element which is also the
!        last time in the segment.
!
         N = I

      END IF

!
!     At this point N is the number of epochs in this GROUP which is
!     also the size of the array DATA which contains the epochs. I is
!     the index of the largest time in DATA which is less than ET.
!
!     We need to take different actions depending on whether ET is less
!     than the first time or greater than the last one in the GROUP.
!

      IF ( I .EQ. 0 ) THEN

         IF ( GROUP .EQ. 1 ) THEN

!
!           ET is less than or equal to the first time in the segment.
!           Return the state at the first time twice.
!
            RECORD(13) = DATA(1)
            RECORD(14) = DATA(1)

            CALL DAFGDA ( HANDLE, BEGIN, BEGIN + 5, DATA )

            CALL MOVED  ( DATA, 6, RECORD(1) )
            CALL MOVED  ( DATA, 6, RECORD(7) )

            CALL CHKOUT( 'SPKR05' )
            RETURN

         ELSE
!
!           ET is less than or equal to the first time in this group
!           but not the first time in the segment. Get the last time
!           from the preceding group. The states for this case will
!           be read outside of the IF block.
!
            CALL DAFGDA ( HANDLE, GRPADD - 1, GRPADD, DATA )

            RECORD(13) = DATA(1)
            RECORD(14) = DATA(2)

         END IF

      ELSE IF ( I .EQ. N ) THEN

         IF ( GROUP .EQ. (NDIR + 1) ) THEN
!
!           ET is greater than all of the times in the segment. Return
!           the state for the last time twice.
!
            RECORD(13) = DATA(N)
            RECORD(14) = DATA(N)

            ADDRSS = BEGIN + ( NREC - 1 ) * STASIZ

            CALL DAFGDA ( HANDLE, ADDRSS, ADDRSS + 5, DATA )

            CALL MOVED ( DATA, 6, RECORD(1) )
            CALL MOVED ( DATA, 6, RECORD(7) )

            CALL CHKOUT( 'SPKR05' )
            RETURN

         ELSE
!
!           ET is greater than the last time in this group but this is
!           not the last time in the segment.  Need the first time from
!           the following group. The states for this case will be read
!           outside of the IF block.
!
            CALL DAFGDA ( HANDLE, GRPADD + N - 1, GRPADD + N, DATA )

            RECORD(13) = DATA(1)
            RECORD(14) = DATA(2)

         END IF

      ELSE
!
!        There are two times in the group that bracket ET. The states
!        for this case will be read outside of the IF block.
!
         RECORD(13) = DATA( I )
         RECORD(14) = DATA(I+1)

      END IF

!
!     Read the consecutive states for the two epochs found above.
!     ET is greater than the (SKIP + I)th time but less than or
!     equal to the time (SKIP + I + 1).
!

      ADDRSS = BEGIN + ( SKIP + I - 1 ) * STASIZ

      CALL DAFGDA ( HANDLE, ADDRSS, ADDRSS + 11, DATA )

      CALL MOVED  ( DATA, 12, RECORD(1) )

      CALL CHKOUT( 'SPKR05' )
      RETURN
      END
