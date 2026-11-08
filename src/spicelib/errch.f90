!$Procedure ERRCH  ( Insert String into Error Message Text )

      subroutine errch ( marker, string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Substitute a character string for the first occurrence of
!     a marker in the current long error message.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     CONVERSION
!     ERROR
!
! $ Declarations

      implicit none

      include 'errhnd.inc'

      character(*)          marker
      character(*)          string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     MARKER     I   A substring of the error message to be replaced.
!     STRING     I   The character string to substitute for MARKER.
!
! $ Detailed_Input
!
!     MARKER   is a character string that marks a position in
!              the long error message where a character string
!              is to be substituted. Leading and trailing blanks
!              in MARKER are not significant.
!
!              Case IS significant:  'XX' is considered to be
!              a different marker from 'xx'.
!
!     STRING   is a character string that will be substituted for
!              the first occurrence of MARKER in the long error
!              message. This occurrence of the substring indicated
!              by MARKER will be removed and replaced by STRING.
!              Leading and trailing blanks in STRING are not
!              significant. However, if STRING is completely blank,
!              a single blank character will be substituted for
!              the marker.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     LMSGLN   is the maximum length of the long error message. See
!              the include file errhnd.inc for the value of LMSGLN.
!
! $ Exceptions
!
!     1)  If the character string resulting from the substitution
!         exceeds the maximum length of the long error message, the
!         long error message is truncated on the right. No error is
!         signaled.
!
!     2)  If MARKER is blank, no substitution is performed. No error
!         is signaled.
!
!     3)  If STRING is blank, then the first occurrence of MARKER is
!         replaced by a single blank.
!
!     4)  If MARKER does not appear in the long error message, no
!         substitution is performed. No error is signaled.
!
!     5)  If changes to the long error message are disabled, this
!         routine has no effect.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The purpose of this routine is to allow you to tailor the long
!     error message to include specific information that is available
!     only at run time. This capability is somewhat like being able to
!     put variables in your error messages.
!
! $ Examples
!
!     1)   In this example, the marker is  '#'.  We'll signal a file
!          open error, and we'll include in the error message the name
!          of the file we tried to open.  There are three steps:
!
!             -- Set the long message, using a marker for the location
!                where a value is to be substituted.
!
!             -- Substitute the file name into the error message.
!
!             -- Signal the error (causing output of error messages)
!                using the SPICELIB routine SIGERR.
!
!             C
!             C     Error on file open attempt.  Signal an error.
!             C     The character string variable FILE contains the
!             C     file name.
!             C
!             C     After the call to ERRCH, the long error message
!             C     will contain the file name held in the string
!             C     FILE.  For example, if FILE contains the name
!             C     'MYFILE.DAT', the long error message will be
!             C
!             C         'File open error.  File is MYFILE.DAT.'
!             C
!
!                   CALL SETMSG ( 'File open error.  File is #.' )
!                   CALL ERRCH  ( '#',  FILE                     )
!                   CALL SIGERR ( SPICE(FILEOPENFAILED)        )
!
!
!     2)   Same example as (1), except this time we'll use a better-
!          looking and more descriptive marker than '#'.  Instead,
!          we'll use the marker 'FILENAME'. This does not affect the
!          long error message; it just makes the code more readable.
!
!             C
!             C     Error on file open attempt. Signal an error.
!             C     The character string variable FILE contains the
!             C     file name.
!             C
!                   CALL SETMSG ( 'File open error. File is FILENAME.')
!                   CALL ERRCH  ( 'FILENAME',  FILE                   )
!                   CALL SIGERR ( SPICE(FILEOPENFAILED)             )
!
!
!     3)   Same example as (2), except this time there's a problem with
!          the variable FILE: it's blank. This time, the code fragment
!
!             C
!             C     Error on file open attempt. Signal an error.
!             C     The character string variable FILE contains the
!             C     file name.
!             C
!                   CALL SETMSG ( 'File open error. File is FILENAME.')
!                   CALL ERRCH  ( 'FILENAME',  FILE                   )
!
!          sets the long error message to
!
!             'File open error. File is  '.
!
! $ Restrictions
!
!     1)  The caller must ensure that the message length, after sub-
!         stitution is performed, doesn't exceed LMSGLN characters.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.2.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the long error message is now represented
!        by the parameter LMSGLN.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 25-MAR-1991 (JEM) (NJB)
!
!        When the input value of STRING is blank, this version
!        replaces the first occurrence of MARKER with a
!        single blank character. Header was edited to improve
!        clarity. Cosmetic changes to code were made.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     insert string into error message text
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the long error message is now represented
!        by the parameter LMSGLN.
!
!-    SPICELIB Version 2.0.0, 25-MAR-1991 (JEM) (NJB)
!
!        When the input value of STRING is blank, this version
!        replaces the first occurrence of MARKER with a
!        single blank character. The previous version made
!        no substitution, leaving the marker in the long error
!        message.
!
!        The $Exceptions, $Examples, and $Particulars sections were
!        updated to improve accuracy and clarity. Some cosmetic
!        changes were made as well.
!
!        Also, some cosmetic changes to the code were made.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           frstnb
      integer(ip)           lastnb
      integer(ip)           nblen
      logical               allowd

!
!     Local Variables:
!
      character(lmsgln)     lngmsg
      character(lmsgln)     tmpmsg

      integer(ip)           mrkpos


!
!     Changes to the long error message must be allowed, or we do
!     nothing.
!
      if ( .not. allowd() ) return

!
!     MARKER must have some non-blank characters, or we do nothing.
!
      if (  lastnb (marker) == 0  ) return

!
!     Get a copy of the current long error message.
!
      call getlms ( lngmsg )

!
!     Locate the leftmost occurrence of MARKER, if there is one
!     (ignoring leading and trailing blanks):
!
      mrkpos = index (  lngmsg, &
                        marker ( frstnb(marker):lastnb(marker) )    )

      if  ( mrkpos == 0 ) then
!
!        MARKER does not occur in the long error message, so there's
!        no substitution to perform.
!
         return

      else
!
!        We put together TMPMSG, a copy of LNGMSG with MARKER
!        replaced by STRING.
!
         if ( mrkpos > 1 ) then
!
!           MARKER is not at the beginning of the long error message.
!
            if (  mrkpos + nblen(marker)  <=  lastnb(lngmsg)  ) then
!
!              There's more of the long message after the marker.
!
               if ( string /= ' ' ) then

                  tmpmsg = lngmsg ( :mrkpos - 1 )                     // &
                           string ( frstnb(string) : lastnb(string) ) // &
                           lngmsg ( mrkpos + nblen(marker): )

               else

                  tmpmsg = lngmsg ( :mrkpos - 1 )                     // &
                           ' '                                        // &
                           lngmsg ( mrkpos + nblen(marker): )

              end if

            else
!
!              The long error message ends with MARKER.
!
               if ( string /= ' ' ) then

                  tmpmsg = lngmsg ( :mrkpos - 1 )                     // &
                           string ( frstnb(string) : lastnb(string) )

               else

                  tmpmsg = lngmsg ( :mrkpos - 1 )   //  ' '

               end if

            end if


         else
!
!           The long error message starts with MARKER (MRKPOS is 1).
!
            if ( nblen(marker) < lastnb(lngmsg) ) then
!
!              There's more of the long message after the marker...
!
               if ( string /= ' ' ) then

                  tmpmsg = string ( frstnb(string) : lastnb(string) ) // &
                           lngmsg ( 1 + nblen(marker) : )
               else

                  tmpmsg = ' ' //  lngmsg ( 1 + nblen(marker) : )

               end if

            else
!
!              The marker's the whole string:
!
               if ( string /= ' ' ) then

                  tmpmsg = string ( frstnb(string) : lastnb(string) )

               else

                  tmpmsg = ' '

               end if

            end if

         end if

!
!        Update the long error message:
!
         call putlms ( tmpmsg )

      end if

      end
