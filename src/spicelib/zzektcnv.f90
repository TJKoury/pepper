!$Procedure  ZZEKTCNV ( Private: EK, time conversion )

      SUBROUTINE ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )
      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Convert time strings from EK query to ephemeris time.
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
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ektype.inc'

      CHARACTER*(*)         TIMSTR
      DOUBLE PRECISION      ET
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TIMSTR     I   Time string.
!     ET         O   Ephemeris time in seconds past J2000, TDB.
!     ERROR      O   Error flag.
!     ERRMSG     O   Error message.
!
! $ Detailed_Input
!
!     TIMSTR         is a string representing a time value.  The value
!                    make be an SCLK string in the form
!
!                       <clock name> SCLK <clock string>
!
!                    or may be any string acceptable to ST2ET.
!
! $ Detailed_Output
!
!     ET             is the ephemeris time equivalent to the input
!                    time.
!
!     ERROR          is a logical flag indicating whether an error was
!                    detected.  Note that a time string might be
!                    syntactically valid, but incapable of being
!                    converted to ET if the appropriate time kernels
!                    (Leapseconds or SCLK) are not loaded.
!
!     ERRMSG         is an error message describing an error in the
!                    input query, if one was detected.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any sort of time conversion error occurs, the output flag
!         ERROR is set, and an error message is returned.
!
!         The routine attempts to avoid causing errors that must
!         be trapped by SPICELIB error handling.  Time string syntax
!         errors or missing kernel files, for example, should not trip
!         SPICELIB error handling.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Strings representing time values are interpreted as follows:
!
!        1)  The string is first examined to see whether it's an
!            SCLK string for a recognized clock; if it is, the
!            string is converted to the equivalent ET.
!
!        2)  If the string is not a SCLK string, it is expected
!            to be some sort of standard time representation.
!            The string is checked to see whether it's in a format
!            that TPARTV can handle.  If TPARTV can't deal with it,
!            the string is considered to be invalid.
!
! $ Examples
!
!     See ZZEKTRES.
!
! $ Restrictions
!
!     1) A leapseconds kernel must be loaded at the time this routine
!        is called.
!
!     2) In order to convert SCLK strings, an appropriate SCLK kernel
!        must be loaded at the time this routine is called.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB)
!
!        Now converts standard time strings to ET via STR2ET instead
!        of the less general routines ISO2UTC and UTC2ET.
!
!        Bug fix:  modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.
!
!        Bug fix:  construction of error messages in case where
!        FAILED() returns .TRUE. has been changed so that REPMC is
!        not called.  Instead, the error-free routine SUFFIX is
!        used.
!
!-    SPICELIB Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB)
!
!        Now converts standard time strings to ET via STR2ET instead
!        of the less general routines ISO2UTC and UTC2ET.
!
!        Bug fix:  modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.
!
!        Bug fix:  construction of error messages in case where
!        FAILED() returns .TRUE. has been changed so that REPMC is
!        not called.  Instead, the error-free routine SUFFIX is
!        used.
!
!-&

!
!     SPICELIB functions
!
      INTEGER               POSR
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 160 )

      INTEGER               NCOMP
      PARAMETER           ( NCOMP  = 10 )

      INTEGER               MAXMOD
      PARAMETER           ( MAXMOD = 10 )

      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    LOCSTR
      CHARACTER*(SHORT)     MODIFY ( MAXMOD )
      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(MSGLEN)    SCLMSG
      CHARACTER*(SHORT)     TYPE

      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TVEC   ( NCOMP )

      INTEGER               CLKID
      INTEGER               LOC
      INTEGER               NTVEC

      LOGICAL               FND
      LOGICAL               MODS
      LOGICAL               SUCCES
      LOGICAL               YABBRV


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKTCNV' )

!
!     No error to start with.
!
      ERROR  =  .FALSE.
      ERRMSG =  ' '

!
!     Get a left-justified, compressed, upper-case copy of
!     the string, so we can easily search it for substrings
!     that would identify it as SCLK.  If we do find a
!     match, remove the identifying substring (of the form
!     'MO SCLK', 'VGR1 SCLK', etc.).
!
      CALL CMPRSS ( ' ',   1,  TIMSTR,  LOCSTR )
      CALL LJUST  ( LOCSTR,             LOCSTR )
      CALL UCASE  ( LOCSTR,             LOCSTR )

      LOC  =  POSR ( LOCSTR, 'SCLK', RTRIM(LOCSTR) )

      IF ( LOC .GT. 0 ) THEN
!
!        It's a SCLK string.  Find the ID code, if we can.
!
         CALL SCN2ID ( LOCSTR(:LOC+3), CLKID, FND )


         IF ( .NOT. FND ) THEN
!
!           We don't recognize this SCLK type.
!
            ERROR  = .TRUE.

            IF ( LOC .GT. 1 ) THEN

               ERRMSG = 'Time conversion failed; SCLK type '// &
                        '<#> was not recognized.'

               CALL REPMC ( ERRMSG, '#', TIMSTR(:LOC-1),  ERRMSG )

            ELSE

               ERRMSG = 'Time conversion failed; SCLK name was not ' // &
                        'supplied.'
            END IF

            CALL CHKOUT ( 'ZZEKTCNV' )
            RETURN

         END IF

!
!        If we got this far, we recognized the SCLK type.
!        Convert the time to ET.
!
         CALL SCPARS ( CLKID, LOCSTR(LOC+4:), ERROR, SCLMSG, SCLKDP )

         IF ( FAILED() ) THEN
!
!           We'll arrive here if the required SCLK kernel hasn't
!           been loaded.
!
            ERROR  =  .TRUE.
            ERRMSG =  'Unexpected SPICELIB error encountered '  // &
                      'while attempting to parse the string <'
            CALL SUFFIX ( TIMSTR, 0, ERRMSG )
            CALL SUFFIX ( '>',    0, ERRMSG )
            CALL CHKOUT ( 'ZZEKTCNV'        )
            RETURN

         ELSE IF ( ERROR ) THEN

            ERRMSG =  'The string <#> didn''t parse '                 // &
                      'as a spacecraft clock string.'
            CALL REPMC  ( ERRMSG, '#', TIMSTR, ERRMSG )
            CALL SUFFIX ( SCLMSG,  3,          ERRMSG )
            CALL CHKOUT ( 'ZZEKTCNV'                  )
            RETURN

         ELSE

            CALL SCT2E ( CLKID, SCLKDP, ET )

            IF ( FAILED() ) THEN

               ERROR  =  .TRUE.
               ERRMSG =  'Unexpected SPICELIB error encountered '  // &
                         'while attempting to parse the string <'
               CALL SUFFIX ( TIMSTR, 0, ERRMSG )
               CALL SUFFIX ( '>',    0, ERRMSG )
               CALL CHKOUT ( 'ZZEKTCNV'        )
               RETURN

            END IF

         END IF



      ELSE
!
!        We could have a standard time string.  Make sure that the
!        time string is acceptable before actually calling STR2ET.
!
         CALL TPARTV ( LOCSTR, &
                       TVEC,   NTVEC, TYPE, &
                       MODIFY, MODS,  YABBRV, SUCCES, &
                       PICTUR, ERRMSG                 )

         IF ( SUCCES ) THEN
!
!           It's safe to pass the string to STR2ET.
!
            CALL STR2ET ( LOCSTR, ET )

            IF ( FAILED() ) THEN

               ERROR  =  .TRUE.
               ERRMSG =  'Unexpected SPICELIB error encountered '  // &
                         'while attempting to parse the string <'
               CALL SUFFIX ( TIMSTR, 0, ERRMSG )
               CALL SUFFIX ( '>',    0, ERRMSG )
               CALL CHKOUT ( 'ZZEKTCNV'        )
               RETURN

            END IF

         ELSE
!
!           The string cannot be parsed by STR2ET.  The error message
!           was set by TPARTV.
!
            ERROR  =  .TRUE.
            CALL CHKOUT ( 'ZZEKTCNV' )
            RETURN

         END IF
!
!        We're done with the standard time string case.
!
      END IF
!
!     We've parsed a time string, if it was an SCLK or standard string.
!
      CALL CHKOUT ( 'ZZEKTCNV' )
      RETURN
      END
