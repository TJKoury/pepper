!$Procedure SGFRVI ( Generic Segments: Fetch ref. value and index )

      SUBROUTINE SGFRVI ( HANDLE, DESCR, X, VALUE, INDX, FOUND )

! $ Abstract
!
!     Find the reference value associated with the value X and its
!     index in a generic segment. The segment is identified by a DAF
!     file handle and segment descriptor.
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
!     DAF
!
! $ Keywords
!
!     GENERIC SEGMENTS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR ( * )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      VALUE
      INTEGER               INDX
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of a DAF open for reading.
!     DESCR      I   The descriptor for a DAF generic segment.
!     X          I   The key value used to find a reference and index.
!     VALUE      O   The reference value associated with X.
!     INDX       O   The index of VALUE within the reference values.
!     FOUND      O   A flag indicating whether values for X were found.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF open for reading
!
!     DESCR    is the descriptor of the generic segment that we are
!              going to search for a reference value to associate with
!              X.
!
!     X        is a value for which the associated reference value
!              and reference index is requested.
!
! $ Detailed_Output
!
!     VALUE    is the reference value associated with the input value
!              X.
!
!     INDX     is the index of VALUE within the set of reference
!              values for the generic segment. This value may be used
!              to obtain a particular packet of data from the generic
!              segment.
!
!     FOUND    is a logical flag indicating whether a reference value
!              associated with X was found. If a reference value was
!              found, FOUND will have a value of .TRUE.; otherwise it
!              will have a value of .FALSE.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the file
!     'sgparam.inc'.
!
! $ Exceptions
!
!     1)  If the reference directory structure is unrecognized, the
!         error SPICE(UNKNOWNREFDIR) is signaled. The most likely cause
!         of this error is that an upgrade to your version of the SPICE
!         toolkit is needed.
!
!     2)  If a value computed for the index of an implicitly indexed
!         generic segment is too large to be represented as an integer,
!         the error SPICE(INDEXTOOLARGE) is signaled.
!
! $ Files
!
!     See the description of HANDLE above.
!
! $ Particulars
!
!     This routine allows you to easily find the index and value
!     of the reference item that should be associated with a
!     value X. Given this information you can then easily retrieve
!     the packet that should be associated with X.
!
! $ Examples
!
!     Suppose that you have a generic segment that contains the
!     following items.
!
!         1)  Packets that model the motion of a body as a function
!             of time over some interval of time.
!
!         2)  Reference values that are the epochs corresponding
!             to the beginning of the intervals for the packets.
!
!     To retrieve the correct packet to use to compute the position
!     and velocity of the body at a particular epoch,  ET, you could
!     use the following code. (Note this block of code assumes that
!     you aren't going to run into any exceptional cases such as ET
!     falling outside the range of times for which the packets can
!     provide ephemeris data.)
!
!        Find out the index of the time that should be associated
!        with the ET we've been given
!
!        CALL SGFRVI ( HANDLE, DESCR, ET,  ETFND, INDX, FOUND )
!
!        Fetch the INDX'th ephemeris packet from the segment.
!
!        CALL SGFPKT ( HANDLE, DESCR, INDX, EPHEM )
!
! $ Restrictions
!
!     1)  The segment described by DESCR MUST be a generic segment,
!         otherwise the results of this routine are not predictable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!
!-    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT)
!
!        A bug was found in the EXPCLS index case when the
!        trying to retrieve the last value in a generic segment.
!        This bug was discovered by the HP compiler complaining
!        that an index used was not initialized.
!
!        The offending line was
!
!                 MYVALU = BUFFER(I)
!
!        The corrected line is:
!
!                 MYVALU = BUFFER(BFINDX)
!
!-    SPICELIB Version 1.0.0, 28-MAR-1994 (KRG) (WLT)
!
!-&


! $ Index_Entries
!
!     find the index of a reference value in a generic segment
!
!-&


!
!     SPICELIB Functions
!
      INTEGER               INTMAX
      INTEGER               LSTLED

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
!     Include the mnemonic values for the generic segment declarations.
!
      INCLUDE 'sgparam.inc'

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DIRSIZ + 1 )
!
!     Local Variables
!
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      DPIMAX
      DOUBLE PRECISION      DPTEMP
      DOUBLE PRECISION      ENDREF
      DOUBLE PRECISION      MYVALU

      INTEGER               BEGIN
      INTEGER               BFINDX
      INTEGER               FULLRD
      INTEGER               END
      INTEGER               I
      INTEGER               MYINDX
      INTEGER               NFETCH
      INTEGER               MYNPKT
      INTEGER               MYNRDR
      INTEGER               MYNREF
      INTEGER               MYRDRB
      INTEGER               MYRDRT
      INTEGER               MYREFB
      INTEGER               RDRIDX
      INTEGER               REMAIN

      LOGICAL               DONE
      LOGICAL               FIRST
      LOGICAL               ISDIRV
      LOGICAL               MYFND
!
!     Saved Variables
!
      SAVE                  FIRST
!
!     Initial Values
!
      DATA                  FIRST / .TRUE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SGFRVI' )
!
!     Set the value for the maximum index as a double precision number,
!     but only do it the first time into the subroutine.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         DPIMAX = DBLE( INTMAX() )

      END IF

!
!     Collect the necessary meta data values common to all cases.
!
      CALL SGMETA ( HANDLE, DESCR, NPKT,   MYNPKT )
      CALL SGMETA ( HANDLE, DESCR, NREF,   MYNREF )
      CALL SGMETA ( HANDLE, DESCR, RDRTYP, MYRDRT )
      CALL SGMETA ( HANDLE, DESCR, REFBAS, MYREFB )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFRVI' )
         RETURN
      END IF
!
!     Check to be sure that we know how to deal with the type of index
!     in the segment. The index type should be between the minimum
!     allowed index type, MNIDXT, and the maximum allowed index type,
!     MXIDXT, as specified in the file 'sgparam.inc'.
!
      IF ( ( MYRDRT .LT. MNIDXT) .OR. ( MYRDRT .GT. MXIDXT ) ) THEN

         CALL SETMSG ( 'The generic DAF segment you attempted to' &
         //            ' read has an unsupported reference directory' &
         //            ' structure. The integer code given for this' &
         //            ' structure is #, and allowed codes are within' &
         //            ' the range # to #. The likely cause of this' &
         //            ' anomaly is your version of SPICELIB needs' &
         //            ' updating. Contact your system administrator' &
         //            ' or NAIF for a toolkit update.'               )
         CALL ERRINT ( '#', MYRDRT                                    )
         CALL ERRINT ( '#', MNIDXT                                    )
         CALL ERRINT ( '#', MXIDXT                                    )
         CALL SIGERR ( 'SPICE(UNKNOWNREFDIR)'                         )
         CALL CHKOUT ( 'SGFRVI'                                       )
         RETURN

      END IF
!
!     We don't have an index yet and we initialize things to zero.
!
      MYFND  = .FALSE.
      MYINDX = 0
      MYVALU = 0.0D0
!
!     We pass the idiot checks, so lets proceed. We have a IF block for
!     each allowed reference directory type code.
!
!        For implicitly indexed data packets, the interval
!
!           [ BUFFER(1), BUFFER(1) + (N - 1) * BUFFER(2) )
!
!        is divided into subintervals as follows:
!
!           (-infinity, r1), [r_1,r_2) [r_2, r_3), ..., [r_i, r_(i+1)),
!            ..., [r_N, +infinity),
!
!        where N = the number of packets in the segment, MYNPKT, and
!        r_i = BUFFER(1) + (i-1) * BUFFER(2).
!
!        If X is in [r_i, r_(i+1)), i = 1, N-1, then we found a value
!        and the index returned will be i with the reference value
!        returned will be r_i.
!
!        If X is in [r_N, +infinity), then we found a value and the
!        index returned will be N and the reference value returned will
!        be r_N.
!
!        If X is in (-infinity, r1), we have two possibilities:
!
!           1) If the index type is implicit closest, we found a value,
!              the index returned will be 1 and the reference value
!              returned will be r_1.
!
!           2) If the index type is implicit less than or equal, we do
!              not find a value.
!
!        For explicitly indexed packets we simply search the reference
!        directory for an appropriate reference value.
!
      IF ( ( MYRDRT .NE. IMPLE ) .AND. ( MYRDRT .NE. IMPCLS ) ) THEN
!
!        In addition to the meta data items we already have, we also
!        need these.
!
         CALL SGMETA ( HANDLE, DESCR, NRDR,   MYNRDR )
         CALL SGMETA ( HANDLE, DESCR, RDRBAS, MYRDRB )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFRVI' )
            RETURN
         END IF
!
!        We need to scan the reference directory (if there is one) to
!        determine the appropriate block of reference values to read
!        from the generic segment. Then we compute the number of
!        reference values to fetch and examine. Finally, based on the
!        index type we figure out whether we have found a reference
!        value or not. It will take a little while to get there, so
!        let's get going.
!
!        We have not started yet, so we're not done and we cannot have a
!        reference directory value yet.
!
         DONE   = .FALSE.
         ISDIRV = .FALSE.
!
!        We have not read any full buffers of reference directory values
!        yet, all of the reference directory values remain to be read,
!        and we have no index for a reference directory value.
!
         FULLRD = 0
         REMAIN = MYNRDR
         RDRIDX = 0
!
!        Search the reference directory values to select the appropriate
!        block of reference values to read.
!
         DO WHILE ( ( .NOT. DONE ) .AND. ( REMAIN .GT. 0 ) )
!
!           Read a buffer of reference directory items.
!
            NFETCH = MIN ( DIRSIZ, REMAIN )
            BEGIN  = MYRDRB + FULLRD * DIRSIZ + 1
            END    = BEGIN  + NFETCH - 1

            CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGFRVI' )
               RETURN
            END IF
!
!           See if X is in the current buffer.
!
            RDRIDX = LSTLED ( X, NFETCH, BUFFER )

            IF ( RDRIDX .EQ. 0 ) THEN
!
!              If not, then X < BUFFER(1) and we're done. This indicates
!              that the desired reference value is before, or in, the
!              previous block of reference values.
!
               DONE = .TRUE.

            ELSE IF ( RDRIDX .EQ. NFETCH ) THEN
!
!              If we get the last value of the buffer, then either we
!              are done, X = BUFFER(NFETCH), or X > BUFFER(NFETCH).
!
               IF ( X .EQ. BUFFER(NFETCH) ) THEN
!
!                 If X = BUFFER(NFETCH) we are done, we have a directory
!                 value, and it might be a value we want to return.
!
                  DONE   = .TRUE.
                  ISDIRV = .TRUE.

               ELSE
!
!                 Otherwise, we might have more stuff to read, so update
!                 the remainder and the current number of full buffer
!                 reads and try the loop again.
!
                  REMAIN = REMAIN - NFETCH

                  IF ( REMAIN .GT. 0 ) THEN
!
!                    We don't want to increment FULLRD for a partial
!                    buffer read. The arithmetic for the index
!                    calculations below will use RDRIDX to deal with
!                    this.
!
                     FULLRD = FULLRD + 1

                  END IF

               END IF

            ELSE
!
!              BUFFER(1) <= X < BUFFER(NFETCH), i.e., we have something
!              in the buffer. Check to see if X = BUFFER(RDRIDX). If so,
!              we are done, we have a directory value, and it might be a
!              value we want to return. Otherwise, we are just done.
!
               DONE = .TRUE.

               IF ( X .EQ. BUFFER(RDRIDX) ) THEN

                  ISDIRV = .TRUE.

               END IF

            END IF

         END DO

         RDRIDX = FULLRD * DIRSIZ + RDRIDX
!
!        There are three cases that we need to consider when X is not a
!        reference directory value:
!
!           Case 1: 0 < RDRIDX < MYNRDR (most common first)
!           Case 2: RDRIDX = 0
!           Case 3: RDRIDX = MYNRDR
!
         IF ( .NOT. ISDIRV ) THEN

            IF ( ( RDRIDX .GT. 0 ) .AND. ( RDRIDX .LT. MYNRDR ) ) THEN
!
!              If we were able to bracket X before reaching the end of
!              the reference directory, then we KNOW that we have a
!              candidate for a reference value in the reference data.
!              All we need to do is read the reference data and find it
!              in the buffer. We also read the reference directory
!              values that bracket the desired reference value into
!              BUFFER, so that they are there if we need them.
!
               NFETCH = MIN( BUFSIZ, MYNREF - RDRIDX*DIRSIZ + 1 )

               BEGIN  = MYREFB + DIRSIZ * RDRIDX
               END    = BEGIN  + NFETCH - 1

               CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF

               BFINDX = LSTLED ( X, NFETCH, BUFFER )
               MYINDX = DIRSIZ * RDRIDX + BFINDX - 1

            ELSE IF ( RDRIDX .EQ. 0 ) THEN
!
!              The reference value may be one of the reference values
!              less than the first reference directory item. So we
!              compute the beginning and ending addresses for the data,
!              read it in, and try to find a reference value.
!
               NFETCH = MIN ( BUFSIZ, MYNREF )

               BEGIN  = MYREFB + 1
               END    = BEGIN  + NFETCH - 1

               CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF

               BFINDX = LSTLED ( X, NFETCH, BUFFER )
               MYINDX = BFINDX

            ELSE IF ( RDRIDX .EQ. MYNRDR ) THEN
!
!              If we were not able to bracket X before reaching the end
!              of the reference directory, then we might have a
!              candidate for a reference value in the reference data
!              after the last reference directory value. All we need to
!              do is read the reference data and look.
!
!              NOTE: NFETCH can never be zero or negative, so we can
!              glibly use it. The reason for this is the NFETCH can only
!              be zero if the desired reference value is a reference
!              directory value, and we already know that the reference
!              value we want is not a reference directory value, because
!              we are here. For similar reasons, NFETCH can never be
!              negative.
!
               BEGIN  = MYREFB + DIRSIZ * RDRIDX
               END    = MYREFB + MYNREF
               NFETCH = END - BEGIN + 1

               CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF

               BFINDX = LSTLED ( X, NFETCH, BUFFER )
               MYINDX = DIRSIZ * RDRIDX + BFINDX - 1

            END IF

         ELSE
!
!           We have a reference directory value, whose index is easy to
!           compute.
!
            MYINDX = DIRSIZ * RDRIDX

         END IF
!
!        Now, if we have a candidate for a reference value, lets make
!        sure, based on the type of index we have.
!
         IF ( MYRDRT .EQ. EXPLT ) THEN
!
!           We have a reference value only if X > some reference
!           value.
!
            IF ( .NOT. ISDIRV ) THEN
!
!              If the value is not a reference directory value, then
!              we have two cases:
!
!                 Case 1: 0 < MYINDX <= MYNREF
!                 Case 2: MYINDX = 0
!
               IF (       ( MYINDX .GT. 0      ) &
                    .AND. ( MYINDX .LE. MYNREF ) ) THEN
!
!                 We found a reference value. The reference value we
!                 want is either the value indicated by MYINDX or
!                 the reference value immediately preceding MYINDX,
!                 if there is such a value. To deal with this we
!                 split the test up into two cases.
!
                  IF ( MYINDX .GT. 1 ) THEN
!
!                    If X > BUFFER(BFINDX) then we are done, so set the
!                    value. If not, then we want the reference value
!                    that is immediately before the current one.
!
                     IF ( X .GT. BUFFER(BFINDX) ) THEN

                        MYFND  = .TRUE.
                        MYVALU = BUFFER(BFINDX)

                     ELSE

                        MYFND = .TRUE.
                        MYVALU = BUFFER(BFINDX-1)
                        MYINDX = MYINDX - 1

                     END IF

                  ELSE
!
!                    Remember, MYINDX is 1 here. If we are greater
!                    than the first reference value in the segment,
!                    we are done. Otherwise there is no reference
!                    value to be associated with X.
!
                     IF ( X .GT. BUFFER(MYINDX) ) THEN

                        MYFND  = .TRUE.
                        MYVALU = BUFFER(MYINDX)

                     ELSE
!
!                       We did not find a reference value. X was
!                       equal to the first reference value of the
!                       generic segment.
!
                        MYFND = .FALSE.

                     END IF

                  END IF

               ELSE IF ( MYINDX .EQ. 0 ) THEN
!
!                 We did not find a reference value. X was < the
!                 first reference value for the generic segment.
!
                  MYFND = .FALSE.

               END IF

            ELSE
!
!              We have a reference directory value, and we are done.
!              Either the reference directory value is the one we
!              want or the reference value immediately preceding it
!              is the one we want.
!
               MYFND  = .TRUE.

               MYINDX = MYINDX - 1

               BEGIN  = MYREFB + MYINDX
               END    = BEGIN

               CALL DAFGDA ( HANDLE, BEGIN, END, MYVALU )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF

            END IF

         ELSE IF ( MYRDRT .EQ. EXPLE ) THEN
!
!           We have a reference value only if X >= some reference
!           value. At this point, either we have the value and index
!           we want or X is before the first reference value of the
!           generic segment. We consider two cases, the first when X
!           is not a reference directory value, and the second when
!           it is.
!
            IF ( .NOT. ISDIRV ) THEN
!
!              If X is not a directory value, then MYINDX is either
!              equal to zero, implying that X is before the first
!              reference value in the generic segment, or MYINDX > 0,
!              implying that we have found a reference value.
!
               IF (       ( MYINDX .GT. 0      ) &
                    .AND. ( MYINDX .LE. MYNREF ) ) THEN

                  MYFND  = .TRUE.
                  MYVALU = BUFFER(BFINDX)

               ELSE IF ( MYINDX .EQ. 0 ) THEN
!
!                 We did not find a reference value. X was < the
!                 first reference value for the generic segment.
!
                  MYFND = .FALSE.

               END IF

            ELSE
!
!              We have a reference directory value, and it is the one
!              we want.
!
               MYFND  = .TRUE.
               MYVALU = X

            END IF

         ELSE IF ( MYRDRT .EQ. EXPCLS ) THEN
!
!           We have a reference value for every value of X. If X <
!           the first reference value of the generic segment, the
!           closest value is the first reference value. If X > the
!           last reference value of the generic segment, the closest
!           value is the last reference value. For X between the
!           first and last reference values we simple take the
!           closest reference value to X, resolving a tie by
!           accepting the larger reference value.
!
            IF ( .NOT. ISDIRV ) THEN
!
!              If X is not a directory value, then MYINDX is either
!              equal to zero, implying that X is before the first
!              reference value in the generic segment,
!              0 < MYINDX < MYNPKT, implying X is between the first
!              and last reference values in the generic segment, or
!              MYINDX = MYNPKT implying that X is greater than or
!              equal to the last reference value.
!
               IF (       ( MYINDX .GT. 0      ) &
                    .AND. ( MYINDX .LT. MYNREF ) ) THEN

                  I = BFINDX
!
!                 Find the closest value to X, choosing the larger in
!                 the event of a tie.
!
                  IF ( (BUFFER(I+1) - X) .LE. (X - BUFFER(I)) ) THEN

                     I      = I + 1
                     MYINDX = MYINDX + 1

                  END IF

                  MYFND  = .TRUE.
                  MYVALU = BUFFER(I)

               ELSE IF ( MYINDX .EQ. 0 ) THEN
!
!                 X is before the first reference value for the
!                 generic segment, so the closest reference value is
!                 the first one.
!
                  MYFND  = .TRUE.
                  MYINDX = 1
                  MYVALU = BUFFER(1)

               ELSE IF ( MYINDX .EQ. MYNREF ) THEN
!
!                 X is at of after the last reference value for the
!                 generic segment, so the closest reference value is
!                 the last reference value, which will be in BUFFER.
!

                  MYFND  = .TRUE.
                  MYVALU = BUFFER(BFINDX)

               END IF

            ELSE
!
!              We have a reference directory value, and it is the one
!              we want.
!
               MYFND  = .TRUE.
               MYVALU = X

            END IF

         END IF

      ELSE IF ( MYRDRT .EQ. IMPLE ) THEN
!
!        Get the begin and end addresses from which to read the
!        reference values and get the reference values.
!
         BEGIN = MYREFB + 1
         END   = MYREFB + 2

         CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFRVI' )
            RETURN
         END IF

         ENDREF = BUFFER(1) + DBLE(MYNPKT-1) * BUFFER(2)
!
!        Compute the index if we can.
!
         IF ( X .LT. BUFFER(1) ) THEN
!
!           If X is less than BUFFER(1), we do not have a reference
!           value.
!
            MYFND = .FALSE.

         ELSE IF ( X .GT. ENDREF ) THEN
!
!           If X is greater than ENDREF, then we have a reference
!           value, ENDREF.
!
            MYFND  = .TRUE.
            MYINDX = MYNPKT
            MYVALU = ENDREF

         ELSE
!
!           r_1 < X < r_N, i.e., we found a value. Compute the index
!           and the reference value.
!
            IF ( MYNPKT .GT. 1 ) THEN

               MYFND = .TRUE.
!
!              Compute the index.
!
               DPTEMP = 1.0D0 + (X - BUFFER(1)) / BUFFER(2)
!
!              Test to see if we can safely convert the index to an
!              integer.
!
               IF ( DPTEMP .GT. DPIMAX ) THEN

                  CALL SETMSG ( 'The computed index is too large' &
                  //            ' to be represented as an integer.' &
                  //            ' The most likely problem is that' &
                  //            ' an incorrect value was stored' &
                  //            ' for the step size. The value' &
                  //            ' found for the step was: #'        )
                  CALL ERRDP ( '#', BUFFER(2)                       )
                  CALL SIGERR ( 'SPICE(INDEXTOOLARGE)'              )
                  CALL CHKOUT ( 'SGFRVI'                            )
                  RETURN

               END IF

               MYINDX = IDINT( DPTEMP         )
               MYINDX = MIN  ( MYINDX, MYNPKT )

            ELSE
!
!              There is only one packet.
!
               MYINDX = 1

            END IF
!
!           Compute the reference value.
!
            MYVALU = BUFFER(1) + DBLE(MYINDX-1) * BUFFER(2)

         END IF

      ELSE IF ( MYRDRT .EQ. IMPCLS ) THEN
!
!        Get the begin and end addresses from which to read the
!        reference values and get the reference values.
!
         BEGIN = MYREFB + 1
         END   = MYREFB + 2

         CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFRVI' )
            RETURN
         END IF

         ENDREF = BUFFER(1) + DBLE(MYNPKT-1) * BUFFER(2)
!
!        Compute the index if we can.
!
         IF ( X .LT. BUFFER(1) ) THEN
!
!           If X < BUFFER(1), then we found a value, the index
!           returned will be 1 and the reference value returned will
!           be BUFFER(1).
!
            MYFND  = .TRUE.
            MYINDX = 1
            MYVALU = BUFFER(1)

         ELSE IF (  X .GT. ENDREF ) THEN
!
!           If X > ENDREF, then we found a value, the index returned
!           will be MYNPKT and the reference value returned will be
!           ENDREF.
!
            MYFND  = .TRUE.
            MYINDX = MYNPKT
            MYVALU = ENDREF

         ELSE
!
!           r_1 < X < r_N, i.e., we found a value. Compute the index
!           and the reference value. If X is closer to r_I, the index
!           returned will be I with a reference value of r_I. If X is
!           closer to r_(I+1), the index returned will be I+1 with a
!           reference value of r_(I+1).
!
            IF ( MYNPKT .GT. 1 ) THEN

               MYFND = .TRUE.
!
!              Compute the index.
!
               DPTEMP = 1.5D0 + (X - BUFFER(1)) / BUFFER(2)

               IF ( DPTEMP .GT. DPIMAX + 0.5D0 ) THEN

                  CALL SETMSG ( 'The computed index is too large' &
                  //            ' to be represented as an integer.' &
                  //            ' The most likely problem is that' &
                  //            ' an incorrect value was stored' &
                  //            ' for the step size. The value' &
                  //            ' found for the step was: #'        )
                  CALL ERRDP ( '#', BUFFER(2)                       )
                  CALL SIGERR ( 'SPICE(INDEXTOOLARGE)'              )
                  CALL CHKOUT ( 'SGFRVI'                            )
                  RETURN

               END IF

               MYINDX = IDINT( DPTEMP )

            ELSE
!
!              There is only one packet.
!
               MYINDX = 1

            END IF
!
!           Compute the reference value.
!
            MYVALU = BUFFER(1) + DBLE(MYINDX-1) * BUFFER(2)

         END IF

      END IF

!
!     At this point, we have either found a value or not. If so, then we
!     need to set the index, value, and found flag for output.
!     Otherwise, we simply set the found flag.
!
      IF ( MYFND ) THEN

         INDX  = MYINDX
         VALUE = MYVALU

      END IF

      FOUND = MYFND

      CALL CHKOUT ( 'SGFRVI' )
      RETURN

      END
