!$Procedure GETMSG ( Get Error Message )

      subroutine getmsg ( option, msg )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Retrieve the current short error message, the explanation of the
!     short error message, or the long error message.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations

      implicit none

      character*(*)          option
      character*(*)          msg

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OPTION     I   Indicates type of error message.
!     MSG        O   The error message to be retrieved.
!
! $ Detailed_Input
!
!     OPTION   is a string that indicates the type of error message to
!              be retrieved.
!
!              Possible values of OPTION are:
!
!                 'SHORT'     indicates that the short message is to be
!                             retrieved.
!
!                 'EXPLAIN'   indicates that the explanation of the
!                             short message is to be retrieved.
!
!                 'LONG'      indicates that the long message is to be
!                             retrieved.
!
!              The input strings indicating the choice of option may be
!              in mixed case. Leading and trailing blanks in OPTION are
!              not significant.
!
! $ Detailed_Output
!
!     MSG      is the error message to be retrieved. Its value depends
!              on OPTION, and on whether an error condition exists.
!
!              When there is no error condition, MSG is blank.
!
!              If an error condition does exist, and OPTION is
!
!                'SHORT'        MSG is the current short error message.
!                               This is a very condensed, 25-character
!                               description of the error.
!
!                'EXPLAIN'      MSG is the explanation of the current
!                               short error message. This is a one-line
!                               expansion of the text of the short
!                               message.
!
!                               All SPICELIB short error messages
!                               do have corresponding explanation text.
!                               For other short error messages, if
!                               there is no explanation text, MSG
!                               will be blank.
!
!                'LONG'         MSG is the current long error message.
!                               The long error message is a detailed
!                               explanation of the error, possibly
!                               containing data specific to the
!                               particular occurrence of the error.
!                               Not all errors have long error messages.
!                               If there is none, MSG will be blank.
!                               Long error messages are no longer than
!                               320 characters.
!
!              If OPTION is invalid, MSG will remain unchanged from its
!              value on input.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input OPTION is invalid, the error
!         SPICE(INVALIDMSGTYPE) is signaled. In that case no messages
!         are returned; MSG retains the value it had on input.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Please read the "required reading" first!
!
!     A good time to call this routine would be when an error
!     condition exists, as indicated by the SPICELIB function,
!     FAILED.
!
!     See the example below for a serving suggestion.
!
!     GETMSG isn't too useful if an error condition doesn't
!     exist, since it will return a blank string in that case.
!
! $ Examples
!
!     Here's an example of a real-life call to GETMSG to get the
!     explanation of the current short error message.
!
!     In this example, a SPICELIB routine, RDTEXT, is called.
!     Following the return from RDTEXT, the logical function,
!     FAILED, is tested to see whether an error occurred.
!     If it did, the message is retrieved and output via
!     a user-defined output routine:
!
!
!     C
!     C     We call RDTEXT; then test for errors...
!     C
!           CALL RDTEXT ( FILE, LINE, EOF )
!
!           IF ( FAILED ) THEN
!
!     C
!     C        Get explanation text for the current short message
!     C        and print it:
!     C
!
!              CALL GETMSG ( 'EXPLAIN', TEXT )
!
!              CALL USER_DEFINED_OUTPUT ( TEXT )
!
!                    .
!                    .   [Do more stuff here]
!                    .
!
!           END IF
!
! $ Restrictions
!
!     1)  This routine is part of the interface to the SPICELIB error
!         handling mechanism. For this reason, this routine does not
!         participate in the trace scheme, even though it has external
!         references.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved
!        disclaimer on participation of this routine in the SPICELIB
!        trace scheme from $Exceptions to $Restrictions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     get error message
!
!-&


!
!     Local Variables:
!

!
!     Length of short error message:
!

      integer(ip)           shrtln
      parameter           ( shrtln = 25 )

      character*(shrtln)    shrtms

!
!     Upper case version of the option:
!

      integer(ip)           optlen
      parameter           ( optlen = 10 )

      character*(optlen)    upopt
      character*(optlen)    locopt

!
!     Heeeeeeeeeeeeeeeeeeeeer's the code!
!

!
!     We only speak upper case in this routine,
!     so convert any lower case letters in OPTION
!     to upper case.  We save the original OPTION
!     string just in case we need to echo it in
!     an error message.
!
      call ljust ( option, upopt )
      call ucase ( upopt,  upopt )

      if ( upopt .eq. 'SHORT' ) then

!
!        Retrieve short message:
!
         call getsms ( msg )


      else if ( upopt .eq. 'EXPLAIN' ) then

!
!        Get current short message; then get explanation
!        corresponding to current short error message:
!

         call getsms  ( shrtms )
         call expln   ( shrtms, msg )


      else if ( upopt .eq. 'LONG' ) then

!
!        Grab long error message:
!
         call getlms  ( msg )

      else

!
!        Invalid value of OPTION!!  Signal error, and set long
!        error message as well:
!
         locopt  =  option

         call setmsg ( &
                      'GETMSG: An invalid value of OPTION was'        // &
                      ' input.  Valid choices are ''SHORT'','         // &
                      '       ''EXPLAIN'', or ''LONG''.  The value'   // &
                      ' that was input was:  '                        // &
                       locopt                                      )

         call sigerr ( 'SPICE(INVALIDMSGTYPE)' )



      end if


      end
