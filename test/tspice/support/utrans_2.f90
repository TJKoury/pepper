!$Procedure      UTRANS_2 ( Translate Units To Default Units )

      SUBROUTINE UTRANS_2 ( STRING, PLACES )
      IMPLICIT NONE

! $ Abstract
!
!     This routine replaces quantities in STRING given in terms of UNITS
!     by the equivalent quantities given in terms of default units.
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
! $ Keywords
!
!      CHARACTERS,  CONVERSION, PARSING
!
! $ Declarations

      CHARACTER*(*)    STRING
      DOUBLE PRECISION PLACES

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      STRING    I/O  The input string before and after unit conversion.
!      PLACES     I   the number of significant figures in output values
!
! $ Detailed_Input
!
!      STRING     The input string before unit conversion.
!
!
!      PLACES     is the number of significant figures that will be
!                 used for the converted quantities.  The largest number
!                 that will be output is 14.  The number of characters
!                 actually used in the output number will be PLACES + 6
!                 for negative numbers, PLACES + 5 for positive numbers.
!
! $ Detailed_Output
!
!      STRING    the input string after unit conversion.
!
! $ Detailed_Description
!
!      This routine is supposed to help translate character strings
!      containing measurements in various units such as:
!
!          32.212 253.267 7628.7827 MILES  37683219.736 FEET
!
!      to character stings giving these measurements in terms of some
!      set of processing units.  For example in the case of the above
!      string, KM might be desirable:
!
!          5.184E+01 4.075937E+02 1.22773E+04 1.148584E+04
!
!      This example is intentded to be typical,  the units are left out
!      intentionally.  After all, this representation is intended to
!      be used only for internal processing by the application using
!      this routine.  After passing through this routine there should be
!      no question as to what units are associated with each of the
!      numeric strings.
!
!      To rigourously describe the function of this routine we need to
!      define a few terms.
!
!          A word within a string is a substring consisting entirely of
!          nonblank characters delimited by the ends of the string or
!          "white space" .
!
!          A numeric word is a word that can be successfully parsed
!          by the NAIF routine NPARSD  (all standard FORTRAN string
!          representations of numbers are numeric words).
!
!          A "measurement sequence" of words is a sequence of words in
!          the string that satisfies:
!
!             1.  the first word preceeding the sequence is a
!                 non-numeric word.
!
!             2.  the last word in the sequence is non-numeric
!                 and belongs to the collection of words given by
!                 the array UNITS. (UNITS would usually contain
!                 something like 'DEGREES', 'RADIANS', 'ARCSECONDS'.)
!
!             3.  All other words in the sequence are numeric and there
!                 is at least 1 numeric word.
!
!
!          The default sequence associated with each measurement
!          sequence is the sequence of numeric words obtained by
!          replacing each of the numeric words of the measurement
!          sequence by the product of that word and the value of
!          CONVERT associated with the unit of the measurement
!          sequence. The units of the measurement sequence are not
!          part of the associated default sequence.
!
!      Now that all of the terms have been described, the action of
!      this routine can be easily explained.  Given the input string
!      each measurement sequence is replaced by its associated
!      default sequence.  The numeric words in the associated default
!      sequences will be written in scientific notation with PLACES
!      significant digits.  The total number of characters needed for
!      each of the associated default sequence words is 6+PLACES
!
! $ Examples
!
!      Suppose that the input string is:
!
!      "LATITUDE: 32.2897 DEGREES    LONGITUDE: 45.28761 DEGREES
!       ALTITUDE: 100     FEET"
!
!      and that the arrays UNITS and CONVERT are given by:
!
!              UNITS    CONVERT
!           --------  --------
!           DEGREES   0.01745329   (conversion from degrees to radians)
!           MINUTES   0.00029088   (conversion from minutes to radians)
!           SECONDS   4.8481E-06   (conversion from seconds to radians)
!           FEET      0.30480061   (conversion from feet    to meteres)
!
!      then the output string will be:
!
!      "LATITUDE: 5.6356E-01   LONGITUDE: 7.38058E-01
!       ALTITUDE: 3.048E+01"
!
!
! $ Restrictions
!
!      The user should be sure that adequate space is available in
!      STRING to contain the translated string.
!
!      Also it is possible (even likely) that non-numeric words of
!      the STRING will be shifted from their original positions.
!      However, the order of the non-unit words will remain the same.
!
! $ Input_Files
!
!      None.
!
! $ Output_Files
!
!      None.
!
! $ Common_Variables
!
!      None.
!
! $ Author_and_Institution
!
!      W. L. Taber (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version_and_Date
!
!      Version 1, 26-JUN-1987
!
!-&



!
!     NAIFLIB functions
!
      LOGICAL               UNITP
      INTEGER               FRSTNB
      INTEGER               LASTNB

!
!     Local variables
!
      CHARACTER*(32)        DPNUM
      CHARACTER*(80)        MYERR
      CHARACTER*(127)       BASICS

      INTEGER               BEG
      INTEGER               BU
      INTEGER               END
      INTEGER               EU
      INTEGER               POINTR
      INTEGER               START
      INTEGER               F
      INTEGER               L

      LOGICAL               ERASED
      LOGICAL               MEASEQ

      DOUBLE PRECISION      X
      DOUBLE PRECISION      CONVERT


!
!     First thing, we left justify the command.
!
      CALL LJUST ( STRING, STRING )

      MEASEQ  = .FALSE.
      ERASED  = .FALSE.

!
!     Find the last word of the string.
!
      START = LEN ( STRING )   + 1
      CALL FNDPTK ( STRING, ' ', START, BEG, END )

      DO WHILE ( BEG .GT. 0 )

!
!        If we are in a measurement sequence, then we need to see if
!        the current word is a number.
!
         IF ( MEASEQ ) THEN

            MYERR = ' '
            CALL NPARSD ( STRING(BEG:END), X, MYERR, POINTR )

!
!           If no error occurred in the attempt to parse this number
!           the measurement sequence continues.
!
            IF ( MYERR .EQ. ' ' ) THEN

!
!              If we haven't already erased the current unit, do so
!              now and record our action.
!
               IF ( .NOT. ERASED ) THEN

                  STRING(BU:EU) = ' '
                  ERASED        = .TRUE.

               END IF

               STRING(BEG:END) = ' '
               X               = X * CONVERT

               CALL DPSTRF ( X,     PLACES, 'E', DPNUM        )
               CALL SIGDGT ( DPNUM,              DPNUM        )
               CALL PREFIX ( DPNUM, 1,           STRING(BEG:) )

!
!           If an error DID occur while attempting to parse the
!           current word, we are ending the current measurment
!           sequence.  However, we might be beginning another ...
!
            ELSE

!
!              ... search the list of recognized units for this word
!
!
               IF ( UNITP (STRING(BEG:END)) ) THEN
!                  WRITE (*,*) STRING(BEG:END)
                  BASICS = ' '
                  CALL TRANSU   (          STRING(BEG:END), BASICS )
                  F      = MAX(1,FRSTNB(BASICS) )
                  L      = MAX(1,LASTNB(BASICS) )
!                  WRITE (*,*) BASICS(F:L)
                  CALL CONVRT_2 ( 1.0D0,   STRING(BEG:END), BASICS(F:L), &
                                  CONVERT  )
                  MEASEQ =  .TRUE.
               ELSE
                  MEASEQ = .FALSE.
               END IF

!
!              ... if this word is on the list, record its place in the
!              string.
!

               IF ( MEASEQ ) THEN

                  BU     =  BEG
                  EU     =  END

!
!                 We haven't erased this unit from the string yet.
!                 Record this observation.
!
                  ERASED = .FALSE.

               END IF

            END IF

         ELSE
!
!           We were not in a measurment sequence, but we might be
!           starting one.  Search the list of known units for the
!           current word.
!
            IF ( UNITP (STRING(BEG:END)) ) THEN
!               WRITE (*,*) STRING(BEG:END)
               BASICS = ' '
               CALL TRANSU   (          STRING(BEG:END), BASICS )
               F      = MAX(1,FRSTNB(BASICS) )
               L      = MAX(1,LASTNB(BASICS) )
!               WRITE (*,*) BASICS(F:L)
               CALL CONVRT_2 ( 1.0D0,   STRING(BEG:END), BASICS(F:L), &
                               CONVERT  )
               MEASEQ =  .TRUE.
            ELSE
               MEASEQ = .FALSE.
            END IF


            IF ( MEASEQ ) THEN
               BU = BEG
               EU = END

!
!              We certainly haven't erased this unit yet.
!
               ERASED = .FALSE.

            END IF

         END IF

!
!        Find the word previous to the current one.
!
         START = BEG
         CALL FNDPTK ( STRING, ' ', START, BEG, END )

      END DO

      RETURN

      END
