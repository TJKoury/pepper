!$Procedure REPMD  ( Replace marker with double precision number )

      subroutine repmd ( in, marker, value, sigdig, out )

! $ Abstract
!
!     Replace a marker with a double precision number.
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

      implicit none

      character*(*)         in
      character*(*)         marker
      double precision      value
      integer               sigdig
      character*(*)         out

      integer               maxldp
      parameter           ( maxldp = 23 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     MARKER     I   Marker to be replaced.
!     VALUE      I   Replacement value.
!     SIGDIG     I   Significant digits in replacement text.
!     OUT        O   Output string.
!     MAXLDP     P   Maximum length of a DP number.
!
! $ Detailed_Input
!
!     IN       is an arbitrary character string.
!
!     MARKER   is an arbitrary character string. The first occurrence of
!              MARKER in the input string is to be replaced by VALUE.
!
!              Leading and trailing blanks in MARKER are NOT
!              significant. In particular, no substitution is performed
!              if MARKER is blank.
!
!     VALUE    is an arbitrary double precision number.
!
!     SIGDIG   is the number of significant digits with which VALUE is
!              to be represented. SIGDIG must be greater than zero and
!              less than 15.
!
! $ Detailed_Output
!
!     OUT      is the string obtained by substituting the text
!              representation of VALUE for the first occurrence of
!              MARKER in the input string.
!
!              The text representation of VALUE is in scientific
!              notation, having the number of significant digits
!              specified by SIGDIG. The representation of VALUE is
!              produced by the SPICELIB routine DPSTR; see that routine
!              for details concerning the representation of double
!              precision numbers.
!
!              OUT and IN must be identical or disjoint.
!
! $ Parameters
!
!     MAXLDP   is the maximum expected length of the text representation
!              of a double precision number. 23 characters are
!              sufficient to hold any result returned by the SPICELIB
!              routine DPSTR. (See $Restrictions)
!
!              This routine assumes that the input d.p. value is such
!              that its string representation contains no more than
!              MAXLDP characters.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If OUT does not have sufficient length to accommodate the
!         result of the substitution, the result will be truncated on
!         the right.
!
!     2)  If MARKER is blank, or if MARKER is not a substring of IN,
!         no substitution is performed. (OUT and IN are identical.)
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
!        'Fifty-one pictures were found in directory [USER.DATA].'
!
!     might be constructed from the fixed string
!
!        '#1 pictures were found in directory #2.'
!
!     by the calls
!
!        CALL REPMCT ( STRING, '#1',  51,           'C', STRING )
!        CALL REPMC  ( STRING, '#2', '[USER.DATA]',      STRING )
!
!     which substitute the cardinal text 'Fifty-one' and the character
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
!     1) The following example illustrate the use of REPMD to
!        replace a marker within a string with the text representation
!        of a double precision value.
!
!
!        Example code begins here.
!
!
!              PROGRAM REPMD_EX1
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
!        C
!        C     1. Single marker, two significant digits.
!        C
!              MARKER = '#'
!              INSTR  = 'Invalid value. The value was:  #'
!
!              CALL REPMD ( INSTR, MARKER, 5.0D1, 2, OUTSTR )
!
!              WRITE(*,*) 'Case 1: Single marker, two significant '
!             .        // 'digits.'
!              WRITE(*,*) '   Input : ', INSTR
!              WRITE(*,*) '   Output: ', OUTSTR
!              WRITE(*,*)
!
!        C
!        C     2. Multiple markers, three significant digits.
!        C
!              MARKER = ' XX '
!              INSTR  = 'Left > Right endpoint. Left: XX; Right: XX'
!
!              CALL REPMD ( INSTR, MARKER, -5.2D-9, 3, OUTSTR )
!
!              WRITE(*,*) 'Case 2: Multiple markers, three '
!             .        // 'significant digits.'
!              WRITE(*,*) '   Input : ', INSTR
!              WRITE(*,*) '   Output: ', OUTSTR
!              WRITE(*,*)
!
!        C
!        C     3. Excessive significant digits.
!        C
!              MARKER = '#'
!              INSTR  = 'Invalid value. The value was:  #'
!
!              CALL REPMD ( INSTR, MARKER, 5.0D1, 100, OUTSTR )
!
!              WRITE(*,*) 'Case 3: Excessive significant digits.'
!              WRITE(*,*) '   Input : ', INSTR
!              WRITE(*,*) '   Output: ', OUTSTR
!              WRITE(*,*)
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Case 1: Single marker, two significant digits.
!            Input : Invalid value. The value was:  #
!            Output: Invalid value. The value was:  5.0E+01
!
!         Case 2: Multiple markers, three significant digits.
!            Input : Left > Right endpoint. Left: XX; Right: XX
!            Output: Left > Right endpoint. Left: -5.20E-09; Right: XX
!
!         Case 3: Excessive significant digits.
!            Input : Invalid value. The value was:  #
!            Output: Invalid value. The value was:  5.0000000000000E+01
!
!
!        Note that, in Case #3 even though 100 digits of precision were
!        requested, only 14 were returned.
!
! $ Restrictions
!
!     1)  The maximum number of significant digits returned is 14.
!
!     2)  This routine makes explicit use of the format of the string
!         returned by the SPICELIB routine DPSTR; should that routine
!         change, substantial work may be required to bring this routine
!         back up to snuff.
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
!-    SPICELIB Version 1.3.0, 03-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing fragments.
!
!        Added REPML to the list of available replace marker routines in
!        $Particulars and extended the description of MAXLDP in
!        $Parameters.
!
!-    SPICELIB Version 1.2.0, 23-SEP-2013 (BVS)
!
!        Minor efficiency update: the routine now looks up the first
!        and last non-blank characters only once.
!
!-    SPICELIB Version 1.1.0, 15-AUG-2002 (WLT)
!
!        The routine is now error free.
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
!     replace marker with d.p. number
!
!-&


!
!     SPICELIB functions
!
      integer               frstnb
      integer               lastnb

!
!     Local variables
!
!
      character*(maxldp)    substr

      integer               mrknbf
      integer               mrknbl

      integer               mrkpsb
      integer               mrkpse

      integer               subnbf
      integer               subnbl

!
!     If MARKER is blank, no substitution is possible.
!
      if  ( marker .eq. ' ' ) then

         out = in
         return

      end if

!
!     Locate the leftmost occurrence of MARKER, if there is one
!     (ignoring leading and trailing blanks). If MARKER is not
!     a substring of IN, no substitution can be performed.
!
      mrknbf = frstnb(marker)
      mrknbl = lastnb(marker)

      mrkpsb = index ( in, marker ( mrknbf : mrknbl )  )

      if  ( mrkpsb .eq. 0 ) then

         out = in
         return

      end if

      mrkpse = mrkpsb + mrknbl - mrknbf

!
!     Okay, MARKER is non-blank and has been found. Convert the
!     number to text, and substitute the text for the marker.
!
      call dpstr  ( value, sigdig, substr )

      subnbf = frstnb(substr)
      subnbl = lastnb(substr)

      if ( subnbf .ne. 0 .and. subnbl .ne. 0 ) then

         call zzrepsub ( in, &
                         mrkpsb, &
                         mrkpse, &
                         substr( subnbf:subnbl ), &
                         out  )

      end if

      return
      end
