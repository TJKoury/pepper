!$Procedure      ZZMKPC ( Make a time format picture mark )

      SUBROUTINE ZZMKPC ( PICTUR, B, E, MARK, PATTRN )

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

      IMPLICIT NONE
      CHARACTER*(*)         PICTUR
      INTEGER               B
      INTEGER               E
      CHARACTER*(*)         MARK
      CHARACTER*(*)         PATTRN


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
      INTEGER               LASTNB

      INTEGER               MAXPLC
      PARAMETER           ( MAXPLC = 14 )

      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = MAXPLC + 12 )

      CHARACTER*(MAXPLC)    PLACES
      CHARACTER*(MAXSIZ)    MYMARK

      INTEGER               LPAT
      INTEGER               LMRK
      INTEGER               POINT
      INTEGER               USE
      INTEGER               LAST


      PLACES = '##############'
!
!     Construct the replacement marker.  First the unmodified
!     portion of the marker.  (We use LAST as the pointer to the
!     last valid character of the marker).
!
      LMRK   = LASTNB(MARK)
      LPAT   = LEN   (PATTRN)
      MYMARK = MARK
      LAST   = LMRK
!
!     Is there a decimal point in the pattern?
!
      POINT  = INDEX(PATTRN, '.')

      IF ( POINT .GT. 0 ) THEN
!
!        We've got a decimal point.  We have to at least put this
!        into the marker.
!
         LAST              = LAST + 1
         MYMARK(LAST:LAST) = '.'
!
!        If the decimal point is not at the end of the pattern, we
!        will need to add some #'s to the marker (but not more than
!        MAXPLC of them).
!
         IF ( POINT .LT. LPAT ) THEN

            USE             = MIN ( MAXPLC, LPAT - POINT )
            MYMARK(LAST+1:) = PLACES(1:USE)
            LAST            = LAST + USE

         END IF

      END IF
!
!     We now let REPSUB do the work of replacing the substring
!     PICTUR(B:E) with the marker we've constructed.
!
      CALL ZZREPSUB ( PICTUR, B, E, MYMARK(1:LAST), PICTUR )


      RETURN

      END
