!$Procedure      ZZMKPC ( Make a time format picture mark )

      subroutine zzmkpc ( pictur, b, e, mark, pattrn )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Given a numeric pattern, construct the appropriate time format
!     picture component.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      Time --- PRIVATE
!
! $ Declarations

      implicit none
      character*(*)         pictur
      integer               b
      integer               e
      character*(*)         mark
      character*(*)         pattrn


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PICTUR    I/O  A partially constructed time format picture
!     B          I   Beginning of substring to place a mark
!     E          I   End of substring to place a mark
!     MARK       I   Initial portion of a mark
!     PATTRN     I   Decimal pattern
!
! $ Detailed_Input
!
!     PICTUR     is a "TIMOUT" format picture that is under construction
!                The substring PICTUR(B:E) is supposed to be a sequence
!                of digits with possibly a decimal point in it.  The
!                digits before the decimal will be replaced by MARK.
!                The decimal point will be copied and digits after
!                the decimal point (up to 14 of them) will be replaced
!                by a the octothorpe character '#'.
!
!     B          are the beginning and ends of the substring mentioned
!     E          in PICTUR.
!
!     MARK       is a numeric time format component (DD, DOY, JULIAND,
!                HR, MN, SC )
!
!     PATTRN     a sequence of digits, possibly a leading minus sign
!                and possibly an embedded decimal point.
!
! $ Detailed_Output
!
!     PICTUR     is the input string with the appropriate time format
!                picture component inserted.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     This is a utility routine that assists in the construction of
!     a format picture that corresponds to a particular instance
!     of a time string.
!
! $ Examples
!
!     See ZZTIME.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 25-APR-1996 (WLT)
!
!
!-&
      integer               lastnb

      integer               maxplc
      parameter           ( maxplc = 14 )

      integer               maxsiz
      parameter           ( maxsiz = maxplc + 12 )

      character*(maxplc)    places
      character*(maxsiz)    mymark

      integer               lpat
      integer               lmrk
      integer               point
      integer               use
      integer               last


      places = '##############'
!
!     Construct the replacement marker.  First the unmodified
!     portion of the marker.  (We use LAST as the pointer to the
!     last valid character of the marker).
!
      lmrk   = lastnb(mark)
      lpat   = len   (pattrn)
      mymark = mark
      last   = lmrk
!
!     Is there a decimal point in the pattern?
!
      point  = index(pattrn, '.')

      if ( point .gt. 0 ) then
!
!        We've got a decimal point.  We have to at least put this
!        into the marker.
!
         last              = last + 1
         mymark(last:last) = '.'
!
!        If the decimal point is not at the end of the pattern, we
!        will need to add some #'s to the marker (but not more than
!        MAXPLC of them).
!
         if ( point .lt. lpat ) then

            use             = min ( maxplc, lpat - point )
            mymark(last+1:) = places(1:use)
            last            = last + use

         end if

      end if
!
!     We now let REPSUB do the work of replacing the substring
!     PICTUR(B:E) with the marker we've constructed.
!
      call zzrepsub ( pictur, b, e, mymark(1:last), pictur )


      return

      end
