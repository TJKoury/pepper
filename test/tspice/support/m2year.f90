!$Procedure      M2YEAR ( Determine whether or not a word is a year )

      LOGICAL FUNCTION M2YEAR ( WORD )
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is a year in the
!     sense of META/2.
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
!     META/2 a language specification language.
!
! $ Keywords
!
!     ALPHANUMERIC
!     ASCII
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations

      CHARACTER*(*)         WORD

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   A character string word
!
!     The function is returned as .TRUE. if word is a META/2 year.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2YEAR     returns as .TRUE. if WORD is a META/2 year.
!               Otherwise it is returned .FALSE.
!
! $ Error_Handling
!
!     None.
!C
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine for the subroutine META2.  It
!     determines whether or not a word is a year in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2YEAR
!     -------                               ------
!     SPAM                                  .FALSE.
!     1                                     .TRUE.
!     0.289E19                              .FALSE.
!     0.2728D12                             .FALSE.
!     -12.1892e-5                           .FALSE.
!     12.E29                                .FALSE.
!     12.E291                               .FALSE.
!     1.2E10                                .TRUE.
!     .E12                                  .FALSE.
!     1.2E.12                               .FALSE.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.1.0, 29-DEC-1994 (WLT)
!
!         The computation of the length of the input string
!         was incorrect.  It has been fixed.  It used to be
!
!            LENGTH = I3 - I1 + 1
!
!         Now it is
!
!            LENGTH = I4 - I1 + 1
!
!
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Version B1.0.0, 22-MAR-1988 (WLT) (IMU)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               QRTRIM
      INTEGER               LTRIM
!
!     Local variables
!
      INTEGER               LENGTH
      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               I4
      INTEGER               VALUE
      INTEGER               VALUES ( 0:255 )

      LOGICAL               FIRST
      SAVE

      DATA                  FIRST / .TRUE. /

      IF ( FIRST ) THEN

         FIRST = .FALSE.
!
!        We will construct a value for the string by taking
!        the non-blank portion and computing the value assuming
!        that the first non-blank is a digit with the appropriate
!        power of 10 attached.  Since all non-digit characters
!        will have values of 1000, we will get a value greater
!        than 1000 if any non-digit characters are present.
!
         DO  I =  0,255
            VALUES(I) = 10000
         END DO

         VALUES( ICHAR('0') ) = 0
         VALUES( ICHAR('1') ) = 1
         VALUES( ICHAR('2') ) = 2
         VALUES( ICHAR('3') ) = 3
         VALUES( ICHAR('4') ) = 4
         VALUES( ICHAR('5') ) = 5
         VALUES( ICHAR('6') ) = 6
         VALUES( ICHAR('7') ) = 7
         VALUES( ICHAR('8') ) = 8
         VALUES( ICHAR('9') ) = 9

      END IF


!
!     Make sure the string has the right length.
!
      I1     = LTRIM  ( WORD )
      I4     = QRTRIM ( WORD )
      LENGTH = I4 - I1 + 1


!
!     Rule out the goofy cases that NPARSD will allow.
!
      IF      ( LENGTH .NE. 4 ) THEN

         VALUE = 10000

      ELSE

         I2    =  I1 + 1
         I3    =  I2 + 1
         VALUE =  1000 * VALUES( ICHAR( WORD(I1:I1) ) ) &
               +   100 * VALUES( ICHAR( WORD(I2:I2) ) ) &
               +    10 * VALUES( ICHAR( WORD(I3:I3) ) ) &
               +         VALUES( ICHAR( WORD(I4:I4) ) )


      END IF
!
!     That's all just make sure that the value is within the
!     bound required of a year.
!
      M2YEAR = (   VALUE .GE. 1000 ) .AND. ( VALUE .LE. 3000 )

      RETURN
      END
