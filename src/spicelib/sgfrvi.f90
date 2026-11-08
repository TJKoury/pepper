!$Procedure SGFRVI ( Generic Segments: Fetch ref. value and index )

      subroutine sgfrvi ( handle, descr, x, value, indx, found )

! $ Abstract
!
!     Find the reference value associated with the value X and its
!     index in a generic segment. The segment is identified by a DAF
!     file handle and segment descriptor.
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

      implicit none

      integer               handle
      double precision      descr ( * )
      double precision      x
      double precision      value
      integer               indx
      logical               found

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
      integer               intmax
      integer               lstled

      logical               failed
      logical               return

!
!     Local Parameters
!
!     Include the mnemonic values for the generic segment declarations.
!
      include 'sgparam.inc'

      integer               dirsiz
      parameter           ( dirsiz = 100 )

      integer               bufsiz
      parameter           ( bufsiz = dirsiz + 1 )
!
!     Local Variables
!
      double precision      buffer ( bufsiz )
      double precision      dpimax
      double precision      dptemp
      double precision      endref
      double precision      myvalu

      integer               begin
      integer               bfindx
      integer               fullrd
      integer               end
      integer               i
      integer               myindx
      integer               nfetch
      integer               mynpkt
      integer               mynrdr
      integer               mynref
      integer               myrdrb
      integer               myrdrt
      integer               myrefb
      integer               rdridx
      integer               remain

      logical               done
      logical               first
      logical               isdirv
      logical               myfnd
!
!     Saved Variables
!
      save                  first
!
!     Initial Values
!
      data                  first / .true. /

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'SGFRVI' )
!
!     Set the value for the maximum index as a double precision number,
!     but only do it the first time into the subroutine.
!
      if ( first ) then

         first  = .false.
         dpimax = dble( intmax() )

      end if

!
!     Collect the necessary meta data values common to all cases.
!
      call sgmeta ( handle, descr, npkt,   mynpkt )
      call sgmeta ( handle, descr, nref,   mynref )
      call sgmeta ( handle, descr, rdrtyp, myrdrt )
      call sgmeta ( handle, descr, refbas, myrefb )

      if ( failed() ) then
         call chkout ( 'SGFRVI' )
         return
      end if
!
!     Check to be sure that we know how to deal with the type of index
!     in the segment. The index type should be between the minimum
!     allowed index type, MNIDXT, and the maximum allowed index type,
!     MXIDXT, as specified in the file 'sgparam.inc'.
!
      if ( ( myrdrt .lt. mnidxt) .or. ( myrdrt .gt. mxidxt ) ) then

         call setmsg ( 'The generic DAF segment you attempted to' &
         //            ' read has an unsupported reference directory' &
         //            ' structure. The integer code given for this' &
         //            ' structure is #, and allowed codes are within' &
         //            ' the range # to #. The likely cause of this' &
         //            ' anomaly is your version of SPICELIB needs' &
         //            ' updating. Contact your system administrator' &
         //            ' or NAIF for a toolkit update.'               )
         call errint ( '#', myrdrt                                    )
         call errint ( '#', mnidxt                                    )
         call errint ( '#', mxidxt                                    )
         call sigerr ( 'SPICE(UNKNOWNREFDIR)'                         )
         call chkout ( 'SGFRVI'                                       )
         return

      end if
!
!     We don't have an index yet and we initialize things to zero.
!
      myfnd  = .false.
      myindx = 0
      myvalu = 0.0d0
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
      if ( ( myrdrt .ne. imple ) .and. ( myrdrt .ne. impcls ) ) then
!
!        In addition to the meta data items we already have, we also
!        need these.
!
         call sgmeta ( handle, descr, nrdr,   mynrdr )
         call sgmeta ( handle, descr, rdrbas, myrdrb )

         if ( failed() ) then
            call chkout ( 'SGFRVI' )
            return
         end if
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
         done   = .false.
         isdirv = .false.
!
!        We have not read any full buffers of reference directory values
!        yet, all of the reference directory values remain to be read,
!        and we have no index for a reference directory value.
!
         fullrd = 0
         remain = mynrdr
         rdridx = 0
!
!        Search the reference directory values to select the appropriate
!        block of reference values to read.
!
         do while ( ( .not. done ) .and. ( remain .gt. 0 ) )
!
!           Read a buffer of reference directory items.
!
            nfetch = min ( dirsiz, remain )
            begin  = myrdrb + fullrd * dirsiz + 1
            end    = begin  + nfetch - 1

            call dafgda ( handle, begin, end, buffer )

            if ( failed() ) then
               call chkout ( 'SGFRVI' )
               return
            end if
!
!           See if X is in the current buffer.
!
            rdridx = lstled ( x, nfetch, buffer )

            if ( rdridx .eq. 0 ) then
!
!              If not, then X < BUFFER(1) and we're done. This indicates
!              that the desired reference value is before, or in, the
!              previous block of reference values.
!
               done = .true.

            else if ( rdridx .eq. nfetch ) then
!
!              If we get the last value of the buffer, then either we
!              are done, X = BUFFER(NFETCH), or X > BUFFER(NFETCH).
!
               if ( x .eq. buffer(nfetch) ) then
!
!                 If X = BUFFER(NFETCH) we are done, we have a directory
!                 value, and it might be a value we want to return.
!
                  done   = .true.
                  isdirv = .true.

               else
!
!                 Otherwise, we might have more stuff to read, so update
!                 the remainder and the current number of full buffer
!                 reads and try the loop again.
!
                  remain = remain - nfetch

                  if ( remain .gt. 0 ) then
!
!                    We don't want to increment FULLRD for a partial
!                    buffer read. The arithmetic for the index
!                    calculations below will use RDRIDX to deal with
!                    this.
!
                     fullrd = fullrd + 1

                  end if

               end if

            else
!
!              BUFFER(1) <= X < BUFFER(NFETCH), i.e., we have something
!              in the buffer. Check to see if X = BUFFER(RDRIDX). If so,
!              we are done, we have a directory value, and it might be a
!              value we want to return. Otherwise, we are just done.
!
               done = .true.

               if ( x .eq. buffer(rdridx) ) then

                  isdirv = .true.

               end if

            end if

         end do

         rdridx = fullrd * dirsiz + rdridx
!
!        There are three cases that we need to consider when X is not a
!        reference directory value:
!
!           Case 1: 0 < RDRIDX < MYNRDR (most common first)
!           Case 2: RDRIDX = 0
!           Case 3: RDRIDX = MYNRDR
!
         if ( .not. isdirv ) then

            if ( ( rdridx .gt. 0 ) .and. ( rdridx .lt. mynrdr ) ) then
!
!              If we were able to bracket X before reaching the end of
!              the reference directory, then we KNOW that we have a
!              candidate for a reference value in the reference data.
!              All we need to do is read the reference data and find it
!              in the buffer. We also read the reference directory
!              values that bracket the desired reference value into
!              BUFFER, so that they are there if we need them.
!
               nfetch = min( bufsiz, mynref - rdridx*dirsiz + 1 )

               begin  = myrefb + dirsiz * rdridx
               end    = begin  + nfetch - 1

               call dafgda ( handle, begin, end, buffer )

               if ( failed() ) then
                  call chkout ( 'SGFRVI' )
                  return
               end if

               bfindx = lstled ( x, nfetch, buffer )
               myindx = dirsiz * rdridx + bfindx - 1

            else if ( rdridx .eq. 0 ) then
!
!              The reference value may be one of the reference values
!              less than the first reference directory item. So we
!              compute the beginning and ending addresses for the data,
!              read it in, and try to find a reference value.
!
               nfetch = min ( bufsiz, mynref )

               begin  = myrefb + 1
               end    = begin  + nfetch - 1

               call dafgda ( handle, begin, end, buffer )

               if ( failed() ) then
                  call chkout ( 'SGFRVI' )
                  return
               end if

               bfindx = lstled ( x, nfetch, buffer )
               myindx = bfindx

            else if ( rdridx .eq. mynrdr ) then
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
               begin  = myrefb + dirsiz * rdridx
               end    = myrefb + mynref
               nfetch = end - begin + 1

               call dafgda ( handle, begin, end, buffer )

               if ( failed() ) then
                  call chkout ( 'SGFRVI' )
                  return
               end if

               bfindx = lstled ( x, nfetch, buffer )
               myindx = dirsiz * rdridx + bfindx - 1

            end if

         else
!
!           We have a reference directory value, whose index is easy to
!           compute.
!
            myindx = dirsiz * rdridx

         end if
!
!        Now, if we have a candidate for a reference value, lets make
!        sure, based on the type of index we have.
!
         if ( myrdrt .eq. explt ) then
!
!           We have a reference value only if X > some reference
!           value.
!
            if ( .not. isdirv ) then
!
!              If the value is not a reference directory value, then
!              we have two cases:
!
!                 Case 1: 0 < MYINDX <= MYNREF
!                 Case 2: MYINDX = 0
!
               if (       ( myindx .gt. 0      ) &
                    .and. ( myindx .le. mynref ) ) then
!
!                 We found a reference value. The reference value we
!                 want is either the value indicated by MYINDX or
!                 the reference value immediately preceding MYINDX,
!                 if there is such a value. To deal with this we
!                 split the test up into two cases.
!
                  if ( myindx .gt. 1 ) then
!
!                    If X > BUFFER(BFINDX) then we are done, so set the
!                    value. If not, then we want the reference value
!                    that is immediately before the current one.
!
                     if ( x .gt. buffer(bfindx) ) then

                        myfnd  = .true.
                        myvalu = buffer(bfindx)

                     else

                        myfnd = .true.
                        myvalu = buffer(bfindx-1)
                        myindx = myindx - 1

                     end if

                  else
!
!                    Remember, MYINDX is 1 here. If we are greater
!                    than the first reference value in the segment,
!                    we are done. Otherwise there is no reference
!                    value to be associated with X.
!
                     if ( x .gt. buffer(myindx) ) then

                        myfnd  = .true.
                        myvalu = buffer(myindx)

                     else
!
!                       We did not find a reference value. X was
!                       equal to the first reference value of the
!                       generic segment.
!
                        myfnd = .false.

                     end if

                  end if

               else if ( myindx .eq. 0 ) then
!
!                 We did not find a reference value. X was < the
!                 first reference value for the generic segment.
!
                  myfnd = .false.

               end if

            else
!
!              We have a reference directory value, and we are done.
!              Either the reference directory value is the one we
!              want or the reference value immediately preceding it
!              is the one we want.
!
               myfnd  = .true.

               myindx = myindx - 1

               begin  = myrefb + myindx
               end    = begin

               call dafgda ( handle, begin, end, myvalu )

               if ( failed() ) then
                  call chkout ( 'SGFRVI' )
                  return
               end if

            end if

         else if ( myrdrt .eq. exple ) then
!
!           We have a reference value only if X >= some reference
!           value. At this point, either we have the value and index
!           we want or X is before the first reference value of the
!           generic segment. We consider two cases, the first when X
!           is not a reference directory value, and the second when
!           it is.
!
            if ( .not. isdirv ) then
!
!              If X is not a directory value, then MYINDX is either
!              equal to zero, implying that X is before the first
!              reference value in the generic segment, or MYINDX > 0,
!              implying that we have found a reference value.
!
               if (       ( myindx .gt. 0      ) &
                    .and. ( myindx .le. mynref ) ) then

                  myfnd  = .true.
                  myvalu = buffer(bfindx)

               else if ( myindx .eq. 0 ) then
!
!                 We did not find a reference value. X was < the
!                 first reference value for the generic segment.
!
                  myfnd = .false.

               end if

            else
!
!              We have a reference directory value, and it is the one
!              we want.
!
               myfnd  = .true.
               myvalu = x

            end if

         else if ( myrdrt .eq. expcls ) then
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
            if ( .not. isdirv ) then
!
!              If X is not a directory value, then MYINDX is either
!              equal to zero, implying that X is before the first
!              reference value in the generic segment,
!              0 < MYINDX < MYNPKT, implying X is between the first
!              and last reference values in the generic segment, or
!              MYINDX = MYNPKT implying that X is greater than or
!              equal to the last reference value.
!
               if (       ( myindx .gt. 0      ) &
                    .and. ( myindx .lt. mynref ) ) then

                  i = bfindx
!
!                 Find the closest value to X, choosing the larger in
!                 the event of a tie.
!
                  if ( (buffer(i+1) - x) .le. (x - buffer(i)) ) then

                     i      = i + 1
                     myindx = myindx + 1

                  end if

                  myfnd  = .true.
                  myvalu = buffer(i)

               else if ( myindx .eq. 0 ) then
!
!                 X is before the first reference value for the
!                 generic segment, so the closest reference value is
!                 the first one.
!
                  myfnd  = .true.
                  myindx = 1
                  myvalu = buffer(1)

               else if ( myindx .eq. mynref ) then
!
!                 X is at of after the last reference value for the
!                 generic segment, so the closest reference value is
!                 the last reference value, which will be in BUFFER.
!

                  myfnd  = .true.
                  myvalu = buffer(bfindx)

               end if

            else
!
!              We have a reference directory value, and it is the one
!              we want.
!
               myfnd  = .true.
               myvalu = x

            end if

         end if

      else if ( myrdrt .eq. imple ) then
!
!        Get the begin and end addresses from which to read the
!        reference values and get the reference values.
!
         begin = myrefb + 1
         end   = myrefb + 2

         call dafgda ( handle, begin, end, buffer )

         if ( failed() ) then
            call chkout ( 'SGFRVI' )
            return
         end if

         endref = buffer(1) + dble(mynpkt-1) * buffer(2)
!
!        Compute the index if we can.
!
         if ( x .lt. buffer(1) ) then
!
!           If X is less than BUFFER(1), we do not have a reference
!           value.
!
            myfnd = .false.

         else if ( x .gt. endref ) then
!
!           If X is greater than ENDREF, then we have a reference
!           value, ENDREF.
!
            myfnd  = .true.
            myindx = mynpkt
            myvalu = endref

         else
!
!           r_1 < X < r_N, i.e., we found a value. Compute the index
!           and the reference value.
!
            if ( mynpkt .gt. 1 ) then

               myfnd = .true.
!
!              Compute the index.
!
               dptemp = 1.0d0 + (x - buffer(1)) / buffer(2)
!
!              Test to see if we can safely convert the index to an
!              integer.
!
               if ( dptemp .gt. dpimax ) then

                  call setmsg ( 'The computed index is too large' &
                  //            ' to be represented as an integer.' &
                  //            ' The most likely problem is that' &
                  //            ' an incorrect value was stored' &
                  //            ' for the step size. The value' &
                  //            ' found for the step was: #'        )
                  call errdp ( '#', buffer(2)                       )
                  call sigerr ( 'SPICE(INDEXTOOLARGE)'              )
                  call chkout ( 'SGFRVI'                            )
                  return

               end if

               myindx = idint( dptemp         )
               myindx = min  ( myindx, mynpkt )

            else
!
!              There is only one packet.
!
               myindx = 1

            end if
!
!           Compute the reference value.
!
            myvalu = buffer(1) + dble(myindx-1) * buffer(2)

         end if

      else if ( myrdrt .eq. impcls ) then
!
!        Get the begin and end addresses from which to read the
!        reference values and get the reference values.
!
         begin = myrefb + 1
         end   = myrefb + 2

         call dafgda ( handle, begin, end, buffer )

         if ( failed() ) then
            call chkout ( 'SGFRVI' )
            return
         end if

         endref = buffer(1) + dble(mynpkt-1) * buffer(2)
!
!        Compute the index if we can.
!
         if ( x .lt. buffer(1) ) then
!
!           If X < BUFFER(1), then we found a value, the index
!           returned will be 1 and the reference value returned will
!           be BUFFER(1).
!
            myfnd  = .true.
            myindx = 1
            myvalu = buffer(1)

         else if (  x .gt. endref ) then
!
!           If X > ENDREF, then we found a value, the index returned
!           will be MYNPKT and the reference value returned will be
!           ENDREF.
!
            myfnd  = .true.
            myindx = mynpkt
            myvalu = endref

         else
!
!           r_1 < X < r_N, i.e., we found a value. Compute the index
!           and the reference value. If X is closer to r_I, the index
!           returned will be I with a reference value of r_I. If X is
!           closer to r_(I+1), the index returned will be I+1 with a
!           reference value of r_(I+1).
!
            if ( mynpkt .gt. 1 ) then

               myfnd = .true.
!
!              Compute the index.
!
               dptemp = 1.5d0 + (x - buffer(1)) / buffer(2)

               if ( dptemp .gt. dpimax + 0.5d0 ) then

                  call setmsg ( 'The computed index is too large' &
                  //            ' to be represented as an integer.' &
                  //            ' The most likely problem is that' &
                  //            ' an incorrect value was stored' &
                  //            ' for the step size. The value' &
                  //            ' found for the step was: #'        )
                  call errdp ( '#', buffer(2)                       )
                  call sigerr ( 'SPICE(INDEXTOOLARGE)'              )
                  call chkout ( 'SGFRVI'                            )
                  return

               end if

               myindx = idint( dptemp )

            else
!
!              There is only one packet.
!
               myindx = 1

            end if
!
!           Compute the reference value.
!
            myvalu = buffer(1) + dble(myindx-1) * buffer(2)

         end if

      end if

!
!     At this point, we have either found a value or not. If so, then we
!     need to set the index, value, and found flag for output.
!     Otherwise, we simply set the found flag.
!
      if ( myfnd ) then

         indx  = myindx
         value = myvalu

      end if

      found = myfnd

      call chkout ( 'SGFRVI' )
      return

      end
