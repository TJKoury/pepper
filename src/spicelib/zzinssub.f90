!$Procedure      ZZINSSUB ( Insert a substring )

      SUBROUTINE ZZINSSUB ( IN, SUB, LOC, OUT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Insert a substring into a character string at a specified
!     location.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASSIGNMENT
!     CHARACTER
!     STRING
!
! $ Declarations

      CHARACTER*(*)    IN
      CHARACTER*(*)    SUB
      INTEGER          LOC
      CHARACTER*(*)    OUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     SUB        I   Substring to be inserted.
!     LOC        I   Position at which substring is to be inserted.
!     OUT        O   Output string.
!
! $ Detailed_Input
!
!     IN          is an input character string, into which a substring
!                 is to be inserted.
!
!     SUB         is the substring to be inserted. Leading and trailing
!                 blanks are significant.
!
!     LOC         is the position in the input string at which the
!                 substring is to be inserted. To append to the
!                 string, set LOC equal to LEN(IN) + 1.
!
! $ Detailed_Output
!
!     OUT         is the output string. This is equivalent to the
!                 string that would be created by the concatenation
!
!                    OUT = IN(1:LOC-1) // SUB // IN(LOC: )
!
!                 If the output string is too long, it is truncated
!                 on the right.
!
!                 OUT may overwrite IN.  OUT may NOT overwrite SUB.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error Free.
!
!     1) If LOC is less than 1 it is treated as having value 1.
!
!     2) If LOC is greater than LEN(IN) + 1, it is treated as if
!        it had value LEN(IN) + 1.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Shift the end of the input string, beginning with LOC, to the
!     right, leaving space for the substring. Then insert the substring
!     into the vacated space in the middle of the string. This has
!     the same effect as the concatenation
!
!        OUT = IN(1:LOC-1) // SUB // IN(LOC: )
!
!     Because this operation is not standard for strings of length (*),
!     this routine does not use concatenation.
!
!     This private routine is just a copy of the SPICE routine INSSUB
!     with "reasonable" choices made for out of bounds errors.
!
! $ Examples
!
!     The following examples illustrate the use of ZZINSSUB.
!
!        IN                SUB      LOC    OUT
!        ----------------- -------  ---    ------------------------
!        'ABCDEFGHIJ'      ' YXZ '    3    'AB XYZ CDEFGHIJ'
!        'The rabbit'      'best '    5    'The best rabbit'
!        ' other woman'    'The'      1    'The other woman'
!        'An Apple a day'  ' keeps'  15    'An Apple a day keeps'
!        'Apple a day'     'An '      0    'An Apple a day'
!        'Apple a day'     'An '     -3    'An Apple a day'
!        'An Apple a day'  ' keeps'  16    'An Apple a day keeps'
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
!     N.J. Bachman    (JPL)
!     H.A. Neilan     (JPL)
!     I.M. Underwood  (JPL)
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Reordered header sections. Corrected typos in comments.
!
!-    SPICELIB Version 1.0.0, 12-AUG-1996 (WLT)
!
!        Adapted from the SPICELIB routine INSSUB to be error free.
!
!
!-&

! $ Index_Entries
!
!     insert a substring
!
!-&



! $ Revisions
!
!-    SPICELIB Version 1.1.0, 24-OCT-1994 (NJB)
!
!        Bug fix:  case where insertion location follows end of
!        input string is now handled correctly.  Formerly, an
!        out-of-range substring bound violation was incurred in this
!        case.
!
!        Bug fix:  use of SHIFTC routine in old implementation
!        resulted in output string being truncated at length
!        LEN(IN), which is not consistent with the routine's
!        specification.
!
!        Now does discovery check-in.  Header sections re-arranged.
!        Some clean-up of header format done.
!
!-    Beta Version 2.0.0, 4-JAN-1989 (HAN)
!
!        If the location at which the substring is to be inserted is
!        not in the interval [1, LEN(IN)+1], an error is signaled.
!        Locations not within that interval refer to non-existent
!        characters positions. (To append to the string, set the
!        location equal to LEN(IN)+1.)
!
!-&


!
!     Local Variables
!
      CHARACTER*(1)         CHR

      INTEGER               FROM
      INTEGER               I
      INTEGER               INLEN
      INTEGER               MYLOC
      INTEGER               NMOVE
      INTEGER               OUTLEN
      INTEGER               SUBEND
      INTEGER               SUBLEN
      INTEGER               TO

      LOGICAL               SAME

!
!     Note to the careful reader:  in order to scrupulously avoid
!     non-standard assignments of characters from a substring of IN to
!     an overlapping substring of OUT, in the case where IN and OUT
!     refer to the same memory, we'll test whether the output and
!     input strings are the same.  If they're the same, we can avoid
!     various assignments that could cause trouble if IN and OUT
!     actually refer to the same memory.  This test has little effect on
!     performance, and allows the author to sleep more soundly at night.
!
!     Capture the lengths of the input, output, and substitution
!     strings.
!
      INLEN   =  LEN ( IN  )
      OUTLEN  =  LEN ( OUT )
      SUBLEN  =  LEN ( SUB )
      MYLOC   =  MIN ( INLEN + 1, MAX ( 1, LOC ) )


!
!     If the insertion occurs after the end of the output string,
!     just return the original string.  Don't do the assignment if
!     the output and input strings have equal values; the assignment
!     is not needed in this case and could cause a run-time error if
!     OUT and IN refer to the same memory.
!
      SAME  =  OUT .EQ. IN

      IF ( MYLOC .GT. OUTLEN ) THEN

         IF ( .NOT. SAME ) THEN
            OUT = IN
         END IF

         RETURN

      END IF


!
!     At this point, we're guaranteed that
!
!        MYLOC  <  OUTLEN
!             -
!
!        MYLOC  <  INLEN + 1
!             -
!
!        MYLOC  >  0
!
!
!     The first part of the input string is copied without change
!     to the output string, if this first part is non-empty.
!
      IF ( MYLOC .GT. 1 ) THEN
!
!        Again, do the assignment only if it's required.
!
         IF ( .NOT. SAME ) THEN
            OUT(1 : MYLOC-1) = IN
         END IF

      END IF

!
!     The part following the new substring is shifted into place, if
!     there's both something to move and a place to put it.  Move the
!     rightmost characters first.
!
      SUBEND  =  MYLOC - 1 + SUBLEN


      IF (        ( MYLOC  .LE. INLEN  ) &
            .AND. ( SUBEND .LT. OUTLEN )  ) THEN


         NMOVE  =  MIN (  OUTLEN-SUBEND, INLEN-MYLOC+1  )

         DO I   =  NMOVE,  1,  -1

            FROM         =  MYLOC  + I - 1
            TO           =  SUBEND + I
            CHR          =  IN(FROM:FROM)
            OUT (TO:TO)  =  CHR

         END DO

      END IF

!
!     And the new word is dropped into the middle.
!
      OUT( MYLOC : MIN(SUBEND,OUTLEN) )  =  SUB

!
!     Blank-pad the output string if necessary.
!
      IF ( OUTLEN .GT. INLEN+SUBLEN ) THEN
         OUT( INLEN+SUBLEN+1 : ) = ' '
      END IF

      RETURN
      END
