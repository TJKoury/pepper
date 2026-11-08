!$Procedure ZZLEXMET ( Scan method string )

      SUBROUTINE ZZLEXMET ( METHOD, MAXN, N, BEGS, ENDS )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Scan a method input string passed by a caller to a SPICE
!     high-level geometry API. Return tokens found in the string.
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
!     DSK
!
! $ Keywords
!
!     SCANNING
!     PARSING
!     CONSTANTS
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         METHOD
      INTEGER               MAXN
      INTEGER               N
      INTEGER               BEGS   ( * )
      INTEGER               ENDS   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Method string.
!     MAXN       I   Maximum number of tokens that can be returned.
!     N          O   Number of tokens found.
!     BEGS       O   Beginning indices of tokens.
!     ENDS       O   Ending indices of tokens.
!
! $ Detailed_Input
!
!     METHOD     is a "method" string to be parsed. This string
!                normally is passed by a caller to a SPICE high-level
!                high-level geometry routine.
!
!                METHOD indicates whether a target body surface is
!                to be modeled as an ellipsoid or by DSK data. It
!                may contain additional, computation-dependent
!                parameters.
!
!     MAXN       is the maximum number of tokens that may be returned.
!                The output arrays BEGS and ENDS must be declared with
!                size at least MAXN.  It's an error to supply output
!                arrays that are too small to hold all of the tokens in
!                the input method string.
!
! $ Detailed_Output
!
!     N          is the number of tokens found in the method string.
!
!     BEGS,
!     ENDS       are, respectively, the indices of the first and last
!                characters of each token in the input string METHOD.
!                BEGS(I) and ENDS(I) indicate the index range of the
!                Ith token.
!
!                See the Particulars section for a list of token
!                delimiters.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error is found while parsing a quoted string,
!         the error SPICE(SYNTAXERROR) is signaled.
!
!     2)  If the number of tokens found in the method string
!         is greater than the input limit MAXN, the error
!         SPICE(ARRAYTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports parsing of the METHOD input argument
!     used by SPICE geometry APIs such as, but not limited to:
!
!        GFOCLT
!        ILLUMF
!        ILLUMG
!        ILUMIN
!        LATSRF
!        LIMBPT
!        SINCPT
!        SUBPNT
!        SUBSLR
!        TERMPT
!
!     Token delimiters are
!
!        / = , <blank>
!
!     Strings delimited by double quote characters are treated as
!     individual tokens. There is no escape syntax for indicating a
!     doubly quoted string within a doubly quoted string.
!
! $ Examples
!
!
!     Example:
!
!       Method string =
!
!          METHOD = 'INTERCEPT / '
!          //       'DSK/UNPRIORITIZED/SURFACES = "MGS '
!          //       'MOLA 128 PIXEL/DEG", MARS_LOWRES'
!
!       Token list =
!
!          INTERCEPT
!          /
!          DSK
!          /
!          UNPRIORITIZED
!          /
!          SURFACES
!          =
!          "MGS MOLA 128 PIXEL/DEG"
!          ,
!          MARS_LOWRES
!
!
! $ Restrictions
!
!     This is a specialized scanning routine. It it meant to
!     be used only for the parsing "method" strings as
!     described in Particulars.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-FEB-2016 (NJB)
!
!        Based on first version 13-JAN-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     scan method string for geometry api routines
!     lex method string for geometry api routines
!     extract tokens from method string for geometry apis
!-&


!
!     SPICELIB functions
!
      INTEGER               CPOS
      INTEGER               LTRIM
      INTEGER               RTRIM

      LOGICAL               RETURN

!
!     Local parameters
!
!
!     Note the leading blank in DELIM below; this
!     indicates that the blank character is a delimiter.
!
      CHARACTER*(*)         DELIM
      PARAMETER           ( DELIM  = ' /,=:' )

      CHARACTER*(*)         DQUOTE
      PARAMETER           ( DQUOTE  = '"' )

!
!     Local variables
!
      INTEGER               B
      INTEGER               E
      INTEGER               EQ
      INTEGER               I
      INTEGER               LOC
      INTEGER               NCHAR
      INTEGER               QPOS
      INTEGER               R
      INTEGER               ROOM
      INTEGER               TKE

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZLEXMET' )


      IF ( METHOD .EQ. ' ' ) THEN
!
!        No tokens here.
!
         N = 0

         CALL CHKOUT ( 'ZZLEXMET' )
         RETURN

      END IF


      N    = 0
      ROOM = MAXN
      R    = RTRIM( METHOD )
      I    = 1

      DO WHILE ( I .LE. R )
!
!        Look ahead in the input string for the start of a
!        quoted string.
!
         QPOS = CPOS ( METHOD(I:), DQUOTE, 1 )
         B    = I

         IF ( QPOS .EQ. 0 ) THEN
!
!           There are no quoted string tokens in the input string
!           from index I onward.
!
            E = R
         ELSE
            E = I + QPOS - 2
         END IF


         IF ( B .LE. E ) THEN
!
!           Locate any tokens between indices B and E.
!
            I = B

            DO WHILE ( I .LE. E )
!
!              Find the next delimiter in the substring
!              from indices I : E.
!
               LOC = CPOS( METHOD(I:E), DELIM, 1 )

               IF ( LOC .EQ. 1 ) THEN
!
!                 There is a delimiter character at index I in METHOD.
!
                  TKE  = I - 1 + LOC

               ELSE IF ( LOC .GT. 1 ) THEN
!
!                 There is a delimiter character at index
!
!                    I - 1 + LOC
!
!                 in METHOD.
!
                  IF ( METHOD(I:I+LOC-2) .NE. ' ' ) THEN
!
!                    There's a token preceding the delimiter.
!
                     TKE  = I - 2 + LOC

                  ELSE
!
!                    The delimiter is all we've got.
!
                     TKE = I - 1 + LOC

                  END IF

               ELSE
!
!                 The token, if any, ends at the end of
!                 substring we're considering.
!
                  TKE  = E

               END IF

!
!              There is a token, which may be blank, between
!              indices I and TKE. We don't return blank tokens
!              in the output array.
!
               IF ( METHOD( I : TKE ) .NE. ' ' ) THEN

                  IF ( ROOM .GT. 0 ) THEN

                     N       = N    + 1
                     ROOM    = ROOM - 1
                     BEGS(N) = LTRIM( METHOD(I : TKE) ) + I - 1
                     ENDS(N) = RTRIM( METHOD(I : TKE) ) + I - 1

                  ELSE

                     CALL SETMSG ( 'Need more room in output ' &
                     //            'arrays. Token count = #; ' &
                     //            'substring indices = #:#; ' &
                     //            'substring = #.'           )
                     CALL ERRINT ( '#', N                     )
                     CALL ERRINT ( '#', I                     )
                     CALL ERRINT ( '#', TKE                   )
                     CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'     )
                     CALL CHKOUT ( 'ZZLEXMET'                 )
                     RETURN

                  END IF

               END IF

               I = TKE + 1

            END DO

         END IF


         IF ( E .LT. R ) THEN
!
!           We expect to find at least one quoted string starting
!           at index E + 1.
!
            I  = E + 1

            CALL LXQSTR ( METHOD(I:), DQUOTE, 1, EQ, NCHAR )

            IF ( NCHAR .GT. 0 ) THEN

               IF ( ROOM .GT. 0 ) THEN

                  N       = N    + 1
                  ROOM    = ROOM - 1
                  BEGS(N) = I
                  ENDS(N) = I    - 1 + EQ

               ELSE

                  CALL SETMSG ( 'Need more room in output ' &
                  //            'arrays. Token count = #; ' &
                  //            'substring indices = #:#; ' &
                  //            'substring = #.'           )
                  CALL ERRINT ( '#', N                     )
                  CALL ERRINT ( '#', I                     )
                  CALL ERRINT ( '#', TKE                   )
                  CALL ERRCH  ( '#', METHOD(I:TKE)         )
                  CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'     )
                  CALL CHKOUT ( 'ZZLEXMET'                 )
                  RETURN

               END IF

            ELSE
!
!              We have a syntax error in the input string.
!
               CALL SETMSG ( 'Invalid quoted string found ' &
               //            'starting at index #. Substring ' &
               //            'is #.'                          )
               CALL ERRINT ( '#', I                           )
               CALL ERRCH  ( '#', METHOD(I:)                  )
               CALL SIGERR ( 'SPICE(SYNTAXERROR)'             )
               CALL CHKOUT ( 'ZZLEXMET'                       )
               RETURN

            END IF

            I = ENDS(N) + 1

         END IF
!
!        The index I has been moved forward by at least one position.
!
      END DO

      CALL CHKOUT ( 'ZZLEXMET' )
      RETURN
      END
