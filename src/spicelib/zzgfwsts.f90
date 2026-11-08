!$Procedure ZZGFWSTS ( Private --- GF, sift first window thru second )

      subroutine zzgfwsts ( wndw1, wndw2, inclsn, wndw3 )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine those intervals of the first window that are
!     properly contained in an interval of the second.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INTERVALS,  WINDOWS
!
! $ Declarations

      implicit none

      integer(ip), parameter :: lbcell = -5

      real(wp)              wndw1 ( lbcell : * )
      real(wp)              wndw2 ( lbcell : * )
      character(*)          inclsn
      real(wp)              wndw3 ( lbcell : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     WNDW1      I   Input window 1.
!     WNDW2      I   Input window 2.
!     INCLSN     I   Flag indicating inclusion desired.
!     WNDW3     I/O  Result of sifting WNDW1 through WNDW2.
!
! $ Detailed_Input
!
!     WNDW1      is an initialized SPICELIB window
!
!     WNDW2      is an initialized SPICELIB window
!
!     INCLSN     is a string indicating how intervals of WNDW1 must
!                be contained in WNDW2. Allowed values are: '[]', '(]',
!                '[)', and '()', where a square bracket represents a
!                closed interval and a curved bracket an open interval.
!                Suppose that [a,b] is an interval of WNDW1 and that
!                [c,d] is an interval of WNDW2.  Then the table below
!                shows the tests used to determine the inclusion of
!                [a,b] in the interval from c to d.
!
!                []     ---  [a,b]  is contained in [c,d]
!                (]     ---  [a,b]  is contained in (c,d]
!                [)     ---  [a,b]  is contained in [c,d)
!                ()     ---  [a,b]  is contained in (c,d)
!
!                if INCLSN is not one of these four values, the
!                error SPICE(UNKNOWNINCLUSION) is signaled.
!
!
!
!     WNDW3      is an initialized SPICELIB window, used on input
!                only for the purpose of determining the amount
!                of space declared for use in WNDW3.
!
! $ Detailed_Output
!
!     WNDW3    is a window consisting those of intervals in WNDW1
!              that are wholly contained in some interval of WNDW2.
!
! $ Parameters
!
!     LBCELL     is the SPICELIB cell lower bound.
!
! $ Exceptions
!
!     1) If the window WNDW3 does not have sufficient space to
!        contain the sifting of WNDW1 through WNDW2 the error
!        'SPICE(OUTOFROOM)' is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows the user to specify two closed subsets of the
!     real line and to find the intervals of one that are contained
!     within the intervals of another. The subsets of the real line
!     are assumed to be made up of disjoint unions of closed intervals.
!
! $ Examples
!
!     Suppose that WNDW1 and WNDW2 are described by the tables below.
!
!                    WNDW1                         WNDW2
!                12.3    12.8                  11.7    13.5
!                17.8    20.4                  17.2    18.3
!                21.4    21.7                  18.5    22.6
!                38.2    39.8                  40.1    45.6
!                44.0    59.9
!
!     Then WNDW3 will be given by:
!
!                    WNDW3
!                12.3    12.8
!                21.4    21.7
!
! $ Restrictions
!
!     The set WNDW3 must not overwrite WNDW1 or WNDW2.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     L.S. Elson      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 08-DEC-2010 (EDW)
!
!        Edit to replaced term "schedule" with "window."
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT)
!
!-&

! $ Index_Entries
!
! find window intervals contained in an interval of another
!
!-&

!
!     SPICELIB functions
!

      integer(ip)           cardd
      integer(ip)           sized

      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: inclen = 2

!
!     Local Variables
!
      character(inclen)     locinc

      integer(ip)           begp1
      integer(ip)           begp2
      integer(ip)           begp3
      integer(ip)           endp1
      integer(ip)           endp2
      integer(ip)           endp3
      integer(ip)           maxpts
      integer(ip)           ovflow
      integer(ip)           size1
      integer(ip)           size2

      logical               closed
      logical               keep
      logical               left
      logical               open
      logical               right

!
!     Standard SPICE error handling.
!
      if ( return() ) return


!
!     Store the maximum number of endpoints that can be loaded into
!     WNDW3
!
      maxpts      = sized ( wndw3 )
      call ssized ( maxpts, wndw3 )

!
!     Find the number of endpoints in each of the input windows.
!
      size1     = cardd( wndw1 )
      size2     = cardd( wndw2 )

!
!     Initialize the place holders for each of the input windows.
!
      begp1     =  1
      begp2     =  1

      endp1     =  2
      endp2     =  2

      begp3     = -1
      endp3     =  0


      call cmprss ( ' ', 0, inclsn, locinc )

      open   = locinc == '()'
      left   = locinc == '[)'
      right  = locinc == '(]'
      closed = locinc == '[]'

      if ( .not. (     open &
                  .or. left &
                  .or. right &
                  .or. closed ) ) then

         call setmsg ( 'The value of the inclusion flag must be ' &
         //            'one of the following: ''[]'', ''[)'', ' &
         //            '''(]'', or ''()''.  However the value ' &
         //            'supplied was ''#''. ' )
         call errch  ( '#', inclsn )
         call sigerr ( 'SPICE(UNKNOWNINCLUSION)'  )
         return

      end if

!
!     We haven't had a chance to overflow yet.
!
      ovflow    = 0

      do while (       ( begp1 < size1 ) &
                 .and. ( begp2 < size2 ) )
!
!        Using the current interval endpoints determine the overlap of
!        the two intervals.
!
         if      ( wndw1(endp1) < wndw2(begp2)  ) then
!
!           the end of the first interval precedes the beginning of the
!           second
!
            begp1    = begp1    + 2
            endp1    = endp1    + 2

         else if ( wndw2(endp2) < wndw1(begp1) ) then
!
!           the end of the second interval precedes the beginning of the
!           first
!
            begp2    = begp2    + 2
            endp2    = endp2    + 2

         else
!
!           the intervals intersect.  Is the first contained in the
!           second?
!
            if      ( closed ) then

               keep =       ( wndw1(begp1) >= wndw2(begp2) ) &
                      .and. ( wndw1(endp1) <= wndw2(endp2) )

            else if ( open   ) then

               keep =       ( wndw1(begp1) > wndw2(begp2) ) &
                      .and. ( wndw1(endp1) < wndw2(endp2) )

            else if ( left   ) then

               keep =       ( wndw1(begp1) >= wndw2(begp2) ) &
                      .and. ( wndw1(endp1) < wndw2(endp2) )

            else if ( right ) then

               keep =       ( wndw1(begp1) > wndw2(begp2) ) &
                      .and. ( wndw1(endp1) <= wndw2(endp2) )

            end if

            if ( keep ) then

               begp3 = begp3    + 2
               endp3 = endp3    + 2

               if ( begp3 < maxpts ) then

!
!                 Adequate room is left in WNDW3 to include this
!                 interval
!
                  wndw3 ( begp3 ) = wndw1(begp1)
                  wndw3 ( endp3 ) = wndw1(endp1)

               else

                  ovflow = ovflow + 2

               end if

            end if

!
!           Determine which window pointers to increment
!
            if      (  wndw1(endp1) < wndw2(endp2)  ) then
!
!              The first interval lies before the end of the second
!
               begp1    = begp1    + 2
               endp1    = endp1    + 2

            else if ( wndw2(endp2) < wndw1(endp1) ) then
!
!              The second interval lies before the end of the first
!
               begp2    = begp2    + 2
               endp2    = endp2    + 2

            else
!
!              The first and second intervals end at the same place
!
               begp1    = begp1    + 2
               endp1    = endp1    + 2

               begp2    = begp2    + 2
               endp2    = endp2    + 2

            end if

         end if

      end do

      if ( ovflow > 0 ) then

         call setmsg ( 'The output window does not have ' &
         //            'sufficient memory to contain the result ' &
         //            'of sifting the two given windows. The ' &
         //            'output window requires space for # ' &
         //            'more values than what has been provided. ')
         call errint ( '#', ovflow        )
         call sigerr ( 'SPICE(OUTOFROOM)' )

      else

         call scardd ( endp3, wndw3 )

      end if

      return
      end
