!$Procedure ERRINT ( Insert Integer into Error Message Text )

      SUBROUTINE ERRINT ( MARKER, INTNUM )

! $ Abstract
!
!     Substitute an integer for the first occurrence of a marker found
!     in the current long error message.
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
!     ERROR
!
! $ Keywords
!
!     CONVERSION
!     ERROR
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'

      CHARACTER*(*)        MARKER
      INTEGER              INTNUM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MARKER     I   A substring of the error message to be replaced.
!     INTNUM     I   The integer to substitute for MARKER.
!
! $ Detailed_Input
!
!     MARKER   is a character string which marks a position in
!              the long error message where a character string
!              representing an integer is to be substituted.
!              Leading and trailing blanks in MARKER are not
!              significant.
!
!              Case IS significant;  'XX' is considered to be
!              a different marker from 'xx'.
!
!     INTNUM   is an integer whose character representation will
!              be substituted for the first occurrence of MARKER
!              in the long error message. This occurrence of the
!              substring indicated by MARKER will be removed, and
!              replaced by a character string, with no leading or
!              trailing blanks, representing INTNUM.
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
!     1)  This routine does not detect any errors.
!
!         However, this routine is part of the SPICELIB error
!         handling mechanism.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine updates the current long error message. If no marker
!     is found, (e.g., in the case that the long error message is
!     blank), the routine has no effect. If multiple instances of the
!     marker designated by MARKER are found, only the first one is
!     replaced.
!
!     If the character string resulting from the substitution
!     exceeds the maximum length of the long error message, the
!     characters on the right are lost. No error is signaled.
!
!     This routine has no effect if changes to the long message
!     are not allowed.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a user-defined error message, including both the
!        short and long messages, providing the value of two integer
!        variables within the long message, and signal the error.
!
!
!        Example code begins here.
!
!
!              PROGRAM ERRINT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Set long error message, with two different MARKER
!        C     strings where the value of the integer variables
!        C     will go.  Our markers are '#' and 'XX'.
!        C
!              CALL SETMSG ( 'LONG MESSAGE.  Invalid operation value. '
!             .         //   '  The value was #.  Left endpoint '
!             .         //   'exceeded right endpoint.  The left '
!             .         //   'endpoint was:  XX.  The right endpoint '
!             .         //   'was:  XX.' )
!
!        C
!        C     Insert the integer number where the # is now.
!        C
!              CALL ERRINT ( '#',  5  )
!
!        C
!        C     Insert now an integer variable in the long message where
!        C     the first XX is now.
!        C
!              CALL ERRINT ( 'XX', 910 )
!
!        C
!        C     Signal the error.
!        C
!              CALL SIGERR ( 'SPICE(USERDEFINED)' )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        ============================================================***
!
!        Toolkit version: N0066
!
!        SPICE(USERDEFINED) --
!
!        LONG MESSAGE. Invalid operation value. The value was 5. Left***
!        exceeded right endpoint. The left endpoint was: 910. The right
!        endpoint was: XX.
!
!        Oh, by the way:  The SPICELIB error handling actions are USER-
!        TAILORABLE.  You can choose whether the Toolkit aborts or co***
!        when errors occur, which error messages to output, and where***
!        the output.  Please read the ERROR "Required Reading" file, ***
!        the routines ERRACT, ERRDEV, and ERRPRT.
!
!        ============================================================***
!
!
!        Warning: incomplete output. 6 lines extended past the right
!        margin of the header and have been truncated. These lines are
!        marked by "***" at the end of each line.
!
!
!        Note that the execution of this program produces the error
!        SPICE(USERDEFINED), which follows the NAIF standard as
!        described in the ERROR required reading.
!
! $ Restrictions
!
!     1)  The caller must ensure that the message length, after
!         substitution is performed, doesn't exceed LMSGLN characters.
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
!-    SPICELIB Version 2.2.0, 17-JUN-2021 (JDR)
!
!        Changed input argument name INTEGR to INTNUM consistency with
!        other routines.
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Added complete code example
!        based on existing fragments.
!
!-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the long error message is now represented
!        by the parameter LMSGLN. Miscellaneous format changes to the
!        header, code and in-line comments were made.
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
!     insert integer into error message text
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB
      INTEGER               FRSTNB
      LOGICAL               ALLOWD

!
!     Local Variables:
!
      CHARACTER*(LMSGLN)    LNGMSG
      CHARACTER*(LMSGLN)    TMPMSG
      CHARACTER*(11)        ISTRNG

      INTEGER               STRPOS


!
!     Changes to the long error message have to be allowed, or we
!     do nothing.
!
      IF ( .NOT. ALLOWD() ) THEN
         RETURN
      END IF

!
!     MARKER has to have some non-blank characters, or we do nothing.
!
      IF ( LASTNB(MARKER) .EQ. 0 ) THEN
         RETURN
      END IF

!
!     Get a copy of the current long error message.  Convert INTNUM
!     to a character string.
!
      CALL GETLMS ( LNGMSG )
      CALL INTSTR ( INTNUM, ISTRNG )

!
!     Locate the leftmost occurrence of MARKER, if there is one
!     (ignoring leading and trailing blanks):
!
      STRPOS = INDEX (  LNGMSG, &
                        MARKER ( FRSTNB(MARKER):LASTNB(MARKER) )  )

      IF  ( STRPOS .EQ. 0 ) THEN

         RETURN

      ELSE
!
!        We put together TMPMSG, a copy of LNGMSG with MARKER
!        replaced by the character representation of INTNUM:
!

         IF ( STRPOS .GT. 1 ) THEN

            IF (     ( STRPOS + LASTNB(MARKER) - FRSTNB(MARKER) ) &
                .LT.   LASTNB ( LNGMSG )                          ) THEN
!
!              There's more of the long message after the marker...
!
               TMPMSG = &
               LNGMSG ( :STRPOS - 1 )                                 // &
               ISTRNG ( :LASTNB(ISTRNG) )                             // &
               LNGMSG ( STRPOS + LASTNB(MARKER) - FRSTNB(MARKER) + 1: )

            ELSE

               TMPMSG =  LNGMSG ( :STRPOS - 1     )                   // &
                         ISTRNG ( :LASTNB(ISTRNG) )

            END IF


         ELSE
!
!           We're starting with the integer, so we know it fits...
!
            IF (      ( LASTNB(MARKER) - FRSTNB(MARKER) ) &
                 .LT.   LASTNB(LNGMSG)                    ) THEN
!
!              There's more of the long message after the marker...
!
               TMPMSG = ISTRNG ( :LASTNB(ISTRNG)                 ) // &
                        LNGMSG ( STRPOS + LASTNB(MARKER) &
                                        - FRSTNB(MARKER)  + 1 :  )
            ELSE
!
!              The marker's the whole string:
!
               TMPMSG = ISTRNG

            END IF


         END IF
!
!        Update the long message:
!
         CALL PUTLMS ( TMPMSG )

      END IF

      END
