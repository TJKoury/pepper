!$Procedure NTHWD ( N'th word in a character string )

      subroutine nthwd ( string, nth, word, loc )

! $ Abstract
!
!     Return the Nth word in a character string, and its location
!     in the string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SEARCH
!     WORD
!
! $ Declarations

      implicit none

      character*(*)    string
      integer          nth
      character*(*)    word
      integer          loc

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Input character string.
!     NTH        I   Index of the word to be returned.
!     WORD       O   The NTH word in STRING.
!     LOC        O   Location of WORD in STRING.
!
! $ Detailed_Input
!
!     STRING   is the input string to be parsed. Each word of this
!              string is a maximal sequence of consecutive non-blank
!              characters.
!
!     NTH      is the index of the word to be returned. (One for the
!              first word, two for the second, and so on.)
!
! $ Detailed_Output
!
!     WORD     is the NTH word in STRING. If STRING is blank, or NTH is
!              non-positive or too large, WORD is blank.
!
!              WORD may overwrite STRING.
!
!     LOC      is the location of WORD in STRING. (That is, WORD begins
!              at STRING(LOC:LOC)). If STRING is blank, or NTH is
!              non-positive or too large, LOC is zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the declared length of WORD is not large enough to contain
!         the NTH word in STRING, the word will be truncated on the
!         right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     NTHWD, like NEXTWD, is useful primarily for parsing input commands
!     consisting of one or more words, where a word is defined to be a
!     maximal sequence of consecutive non-blank characters. Each word is
!     bounded on both sides by a blank character, or by the start or end
!     of the input string. Successive calls to NEXTWD allow the calling
!     routine to neatly parse and process one word at a time.
!
!     The chief difference between the two routines is that
!     NTHWD allows the calling routine to access the words making
!     up the input string in random order. (NEXTWD allows only
!     sequential access.)
!
!     NTHWD may be more efficient than NEXTWD, since NTHWD doesn't
!     update an output string consisting of the remaining, unparsed
!     string.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a character string, get the N'th word within, and the
!        word's location.
!
!
!        Example code begins here.
!
!
!              PROGRAM NTHWD_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         STRING
!              PARAMETER           ( STRING = ' Now is the time,   '
!             .                  //  'for all good men     to come.' )
!
!              CHARACTER*(*)         FMT
!              PARAMETER           ( FMT = '(A,I3,3A,I3)' )
!
!              INTEGER               WRDSZ
!              PARAMETER           ( WRDSZ = 5 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(WRDSZ)     WORD
!              INTEGER               LOC
!              INTEGER               NTH
!
!
!              DO NTH = -1, 11
!
!                 CALL NTHWD ( STRING, NTH, WORD, LOC )
!                 WRITE(*,FMT) 'Word #', NTH, '  is <',
!             .                WORD(:RTRIM(WORD)),
!             .                '>, starting at position', LOC
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Word # -1  is < >, starting at position  0
!        Word #  0  is < >, starting at position  0
!        Word #  1  is <Now>, starting at position  2
!        Word #  2  is <is>, starting at position  6
!        Word #  3  is <the>, starting at position  9
!        Word #  4  is <time,>, starting at position 13
!        Word #  5  is <for>, starting at position 21
!        Word #  6  is <all>, starting at position 25
!        Word #  7  is <good>, starting at position 29
!        Word #  8  is <men>, starting at position 34
!        Word #  9  is <to>, starting at position 42
!        Word # 10  is <come.>, starting at position 45
!        Word # 11  is < >, starting at position  0
!
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 26-OCT-2021 (NJB) (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on the existing fragment.
!
!        Updated header documentation. Added entry #1 in $Exceptions
!        section.
!
!-    SPICELIB Version 1.1.0, 10-MAY-2006 (EDW)
!
!        Added logic to prevent the evaluation of STRING(I:I)
!        if I exceeds the length of STRING. Functionally, the
!        evaluation had no effect on NTHWD's output, but the ifort
!        F95 compiler flagged the evaluation as an array
!        overrun error. This occurred because given:
!
!           A .AND. B
!
!        ifort evaluates A then B then performs the logical
!        comparison.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     N'th word in a character_string
!
!-&


!
!     Local variables
!
      integer          n
      integer          i
      integer          length

      logical          loop



!
!     Trivial cases first. Blank STRING? Nonpositive NTH?
!
      if ( string .eq. ' ' .or. nth .lt. 1 ) then

         word = ' '
         loc  =  0
         return

      end if

!
!     Skip leading blanks.
!
      loc = 1

      do while ( string(loc:loc) .eq. ' ' )
         loc = loc + 1
      end do

!
!     If we wanted the first word, we have the location. Otherwise,
!     keep stepping through STRING. Quit when the N'TH word is found,
!     or when the end of the string is reached. (The current word is
!     ended whenever a blank is encountered.)
!
!     N is the number of words found so far.
!     I is the current location in STRING.
!
      n      = 1
      i      = loc
      length = len ( string )

      do while ( i .lt. length .and. n .lt. nth )

         i = i + 1

!
!        Blank signals end of the current word.
!
         if ( string(i:i) .eq. ' ' ) then

!
!           Skip ahead to the next one.  The logic ensures no
!           evaluation of STRING(I:I) if I > LEN(STRING).
!
            loop = i .le. length
            if( loop ) then
               loop = loop .and. string(i:i) .eq. ' '
            end if

            do while ( loop )
               i = i + 1

               if( i .gt. length ) then
                  loop = .false.
               else if ( string(i:i) .ne. ' ' ) then
                  loop = .false.
               else
                  loop = .true.
               end if

            end do

!
!           If not at the end of the string, we have another word.
!
            if ( i .le. length ) then
               n   = n + 1
               loc = i
            end if

         end if

      end do

!
!     Couldn't find enough words? Return blank and zero.
!
      if ( n .lt. nth ) then
         word = ' '
         loc  =  0

!
!     Otherwise, find the rest of WORD (it continues until the next
!     blank), and return the current LOC.
!
      else
         i = index ( string(loc: ), ' ' )

         if ( i .eq. 0 ) then
            word = string(loc: )
         else
            word = string(loc:loc+i-1)
         end if
      end if

      return
      end
