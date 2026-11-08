!$Procedure REPMOT  ( Replace marker with ordinal text )

      SUBROUTINE REPMOT ( IN, MARKER, VALUE, RTCASE, OUT )

! $ Abstract
!
!     Replace a marker with the text representation of an ordinal
!     number.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     CONVERSION
!     STRING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         IN
      CHARACTER*(*)         MARKER
      INTEGER               VALUE
      CHARACTER*1           RTCASE
      CHARACTER*(*)         OUT

      INTEGER               MAXLON
      PARAMETER           ( MAXLON = 147 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     MARKER     I   Marker to be replaced.
!     VALUE      I   Replacement value.
!     RTCASE     I   Case of replacement text.
!     OUT        O   Output string.
!     MAXLON     P   Maximum length of an ordinal number.
!
! $ Detailed_Input
!
!     IN       is an arbitrary character string.
!
!     MARKER   is an arbitrary character string. The first occurrence of
!              MARKER in the input string is to be replaced by the text
!              representation of the ordinal number VALUE.
!
!              Leading and trailing blanks in MARKER are NOT
!              significant. In particular, no substitution is performed
!              if MARKER is blank.
!
!     VALUE    is an arbitrary integer.
!
!     RTCASE   indicates the case of the replacement text. RTCASE may be
!              any of the following:
!
!                 RTCASE   Meaning        Example
!                 ------   -----------    -----------------------
!                 U, u     Uppercase      ONE HUNDRED FIFTY-THIRD
!
!                 L, l     Lowercase      one hundred fifty-third
!
!                 C, c     Capitalized    One hundred fifty-third
!
! $ Detailed_Output
!
!     OUT      is the string obtained by substituting the text
!              representation of the ordinal number VALUE for the first
!              occurrence of MARKER in the input string.
!
!              OUT and IN must be identical or disjoint.
!
! $ Parameters
!
!     MAXLON   is the maximum expected length of any ordinal text.
!              147 characters are sufficient to hold the text
!              representing any ordinal value whose corresponding
!              ordinal value is in the range
!
!                ( -10**12, 10**12 )
!
!              An example of a number whose ordinal text representation
!              is of maximum length is
!
!                 - 777 777 777 777
!
! $ Exceptions
!
!     1)  If OUT does not have sufficient length to accommodate the
!         result of the substitution, the result will be truncated on
!         the right.
!
!     2)  If MARKER is blank, or if MARKER is not a substring of IN,
!         no substitution is performed. (OUT and IN are identical.)
!
!     3)  If the value of RTCASE is not recognized, the error
!         SPICE(INVALIDCASE) is signaled. OUT is not changed.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is one of a family of related routines for inserting values
!     into strings. They are typically to construct messages that
!     are partly fixed, and partly determined at run time. For example,
!     a message like
!
!        'The fifty-first picture was found in directory [USER.DATA].'
!
!     might be constructed from the fixed string
!
!        'The #1 picture was found in directory #2.'
!
!     by the calls
!
!        CALL REPMOT ( STRING, '#1',  51,           'L', STRING )
!        CALL REPMC  ( STRING, '#2', '[USER.DATA]',      STRING )
!
!     which substitute the ordinal text 'fifty-first' and the character
!     string '[USER.DATA]' for the markers '#1' and '#2' respectively.
!
!     The complete list of routines is shown below.
!
!        REPMC    ( Replace marker with character string value )
!        REPMD    ( Replace marker with double precision value )
!        REPMF    ( Replace marker with formatted d.p. value   )
!        REPMI    ( Replace marker with integer value          )
!        REPML    ( Replace marker with logical value          )
!        REPMCT   ( Replace marker with cardinal text          )
!        REPMOT   ( Replace marker with ordinal text           )
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following example illustrate the use of REPMOT to
!        replace a marker within a string with the ordinal text
!        corresponding to an integer.
!
!
!        Example code begins here.
!
!
!              PROGRAM REPMOT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 80 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(STRLEN)      INSTR
!              CHARACTER*(STRLEN)      MARKER
!              CHARACTER*(STRLEN)      OUTSTR
!
!
!        C
!        C     1. Uppercase
!        C
!              MARKER = '#'
!              INSTR  = 'INVALID COMMAND. # WORD NOT RECOGNIZED.'
!
!              CALL REPMOT ( INSTR, MARKER, 5, 'U', OUTSTR )
!
!              WRITE(*,*) 'Case 1: Replacement text in uppercase.'
!              WRITE(*,*) '   Input : ', INSTR
!              WRITE(*,*) '   Output: ', OUTSTR
!              WRITE(*,*) ' '
!
!        C
!        C     2. Lowercase
!        C
!              MARKER = ' XX '
!              INSTR  = 'The XX word of the XX sentence was ...'
!
!              CALL REPMOT ( INSTR, MARKER, 5, 'L', OUTSTR )
!
!              WRITE(*,*) 'Case 2: Replacement text in lowercase.'
!              WRITE(*,*) '   Input : ', INSTR
!              WRITE(*,*) '   Output: ', OUTSTR
!              WRITE(*,*) ' '
!
!        C
!        C     2. Capitalized
!        C
!              MARKER = ' XX '
!              INSTR  = 'Name:  YY.  Rank:  XX.'
!
!              CALL REPMC  ( INSTR,  'YY',  'Moriarty', OUTSTR )
!              CALL REPMOT ( OUTSTR, MARKER, 5,    'C', OUTSTR )
!
!              WRITE(*,*) 'Case 3: Replacement text capitalized.'
!              WRITE(*,*) '   Input : ', INSTR
!              WRITE(*,*) '   Output: ', OUTSTR
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Case 1: Replacement text in uppercase.
!            Input : INVALID COMMAND. # WORD NOT RECOGNIZED.
!            Output: INVALID COMMAND. FIFTH WORD NOT RECOGNIZED.
!
!         Case 2: Replacement text in lowercase.
!            Input : The XX word of the XX sentence was ...
!            Output: The fifth word of the XX sentence was ...
!
!         Case 3: Replacement text capitalized.
!            Input : Name:  YY.  Rank:  XX.
!            Output: Name:  Moriarty.  Rank:  Fifth.
!
!
! $ Restrictions
!
!     1)  VALUE must be in the range accepted by the SPICELIB routine
!         INTORD. This range is currently
!
!            ( -10**12, 10**12 )
!
!         Note that the endpoints of the interval are excluded.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
!
!        Changed input argument name CASE to RTCASE for consistency
!        with other routines.
!
!        Added IMPLICIT NONE statement.
!
!        Updated code to remove unnecessary lines of code in the
!        Standard SPICE error handling CHKIN statements.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example from existing fragments. Improved example in
!        $Particulars section.
!
!        Added REPML to the list of available replace marker routines.
!
!-    SPICELIB Version 1.2.0, 21-SEP-2013 (BVS)
!
!        Minor efficiency update: the routine now looks up the first
!        and last non-blank characters only once.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) (IMU)
!
!-&


! $ Index_Entries
!
!     replace marker with ordinal text
!
!-&


!
!     SPICELIB functions
!
      INTEGER               FRSTNB
      INTEGER               LASTNB
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(MAXLON)    ORD
      CHARACTER*(1)         TMPCAS

      INTEGER               MRKNBF
      INTEGER               MRKNBL

      INTEGER               MRKPSB
      INTEGER               MRKPSE


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'REPMOT' )

!
!     Bail out if RTCASE is not recognized.
!
      CALL LJUST ( RTCASE, TMPCAS )
      CALL UCASE ( TMPCAS, TMPCAS )

      IF (        ( TMPCAS .NE. 'U' ) &
            .AND. ( TMPCAS .NE. 'L' ) &
            .AND. ( TMPCAS .NE. 'C' )  ) THEN

         CALL SETMSG ( 'Case (#) must be U, L, or C.' )
         CALL ERRCH  ( '#', RTCASE                    )
         CALL SIGERR ( 'SPICE(INVALIDCASE)'           )
         CALL CHKOUT ( 'REPMOT'                       )
         RETURN

      END IF

!
!     If MARKER is blank, no substitution is possible.
!
      IF ( MARKER .EQ. ' ' ) THEN

         OUT = IN

         CALL CHKOUT ( 'REPMOT')
         RETURN

      END IF

!
!     Locate the leftmost occurrence of MARKER, if there is one
!     (ignoring leading and trailing blanks). If MARKER is not
!     a substring of IN, no substitution can be performed.
!
      MRKNBF = FRSTNB(MARKER)
      MRKNBL = LASTNB(MARKER)

      MRKPSB = INDEX ( IN, MARKER ( MRKNBF : MRKNBL )  )

      IF  ( MRKPSB .EQ. 0 ) THEN

         OUT = IN

         CALL CHKOUT ( 'REPMOT' )

         RETURN

      END IF

      MRKPSE = MRKPSB + MRKNBL - MRKNBF

!
!     Okay, RTCASE is recognized and MARKER has been found.
!     Generate the ordinal text corresponding to VALUE.
!
      CALL INTORD ( VALUE, ORD )

!
!     CARD is always returned in upper case; change to the specified
!     case, if required.
!
      IF ( TMPCAS .EQ. 'L' ) THEN

         CALL LCASE ( ORD, ORD )

      ELSE IF ( TMPCAS .EQ. 'C' ) THEN

         CALL LCASE ( ORD(2:), ORD(2:) )

      END IF

!
!     Replace MARKER with CARD.
!
      CALL REPSUB ( IN, &
                    MRKPSB, &
                    MRKPSE, &
                    ORD ( : LASTNB(ORD) ), &
                    OUT )

      CALL CHKOUT ( 'REPMOT' )
      RETURN
      END
