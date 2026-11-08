!$Procedure      CMMORE ( Command Loop---More Commands)

      LOGICAL  FUNCTION CMMORE ( COMMND )

! $ Abstract
!
!    Determine whether or not more command loop processing
!    should be performed.
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
!     None.
!
! $ Keywords
!
!     Command Loop
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         COMMND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     COMMND     I   A command to be processed by CMLOOP
!
!     The function returns .TRUE. if the command is not the "exit"
!     command. If it is the exit command it returns .FALSE.
!
! $ Detailed_Input
!
!     COMMND     A commmand that should be acted on by CMLOOP
!
!
! $ Detailed_Output
!
!     The function returns .TRUE. if this is not the exit command.
!     The meaning being "there is still more to do in CMLOOP."
!
!     If the input command is equivalent to the exit command
!     (Same words when converted to uppercase) The function
!     returns .FALSE.  The intended meaning is "there is nothing
!     left for CMLOOP to do but cleanup and return."
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
!     Error free.
!
! $ Particulars
!
!     This is utility function for use by CMLOOP.  It is the
!     function tested each pass through the loop to see if the
!     loop has finished its work
!
! $ Examples
!
!     See CMLOOP.  There is no other use for this function.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!       W.L. Taber      (JPL)
!
! $ Literature_References
!
!       None.
!
! $ Version
!
!-    Command Loop Version 1.0.0, 4-AUG-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     More command processing required
!
!-&
!     SPICELIB Functions
!
      INTEGER               RTRIM
      LOGICAL               NECHR

!
!     Local Variables.
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


      CHARACTER*(WDSIZE)    EXIT

      INTEGER               LC
      INTEGER               I
      INTEGER               R

      LOGICAL               FIRST
      SAVE


      DATA                  FIRST / .TRUE. /


!
!     On the first pass we fetch the "exit" command and
!     spruce it up a bit for use when comparing with
!     the input command.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         CALL TRNLAT ( 'EXIT',       EXIT )
         CALL CMPRSS ( ' ', 1, EXIT, EXIT )
         CALL LJUST  (  EXIT,        EXIT )
         R  = RTRIM  (  EXIT )

      END IF
!
!     If the input command is shorter than the non-blank
!     length of EXIT, then this cannot be the exit command.
!     There is more to do.
!
!     Note we assign a value to CMMORE so that the compiler
!     won't have a fit about having a function unassigned.
!     The if conditions below ensure that we assign a value
!     but most compilers aren't smart enough to figure that
!     out.
!
      CMMORE = .TRUE.
      LC     = LEN(COMMND)

      IF ( LC .LT. R ) THEN
         CMMORE = .TRUE.
         RETURN
      END IF

!
!     Check to see if the input command matches the exit command.
!     We do this a character at a time.  We search from the
!     left to right, because most commands are not EXIT and this
!     allows us to quit early in the process.
!
      DO I = 1, R

         IF ( NECHR( COMMND(I:I), EXIT(I:I) ) ) THEN
            CMMORE = .TRUE.
            RETURN
         END IF

      END DO

!
!     It's looking like this might be it.  See if the rest of
!     the input command is blank.
!
      IF ( LC.EQ. R ) THEN
!
!        We've got an exact match.  There are no more commands
!        to look at.
!
         CMMORE = .FALSE.

      ELSE IF ( LC .GT. R ) THEN
!
!        There will be more commands only if the rest of the input
!        command is non-blank.
!
         CMMORE =  COMMND(R+1:) .NE. ' '

      END IF

      RETURN
      END
