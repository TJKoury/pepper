!$Procedure REPML ( Replace marker with logical value text )

      subroutine repml ( in, marker, value, rtcase, out )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Replace a marker with the text representation of a logical value.
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

      character(*)          in
      character(*)          marker
      logical               value
      character(1)          rtcase
      character(*)          out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     MARKER     I   Marker to be replaced.
!     VALUE      I   Replacement logical value.
!     RTCASE     I   Case of replacement text.
!     OUT        O   Output string.
!
! $ Detailed_Input
!
!     IN       is an arbitrary character string.
!
!     MARKER   is an arbitrary character string. The first occurrence
!              of MARKER in the input string is to be replaced by
!              VALUE.
!
!              MARKER is case-sensitive.
!
!              Leading and trailing blanks in MARKER are NOT
!              significant. In particular, no substitution is
!              performed if MARKER is blank.
!
!     VALUE    is an arbitrary logical value, either .TRUE. or
!              .FALSE.
!
!     RTCASE   indicates the case of the replacement text. RTCASE may
!              be any of the following:
!
!                 RTCASE    Meaning        Output values
!                 ------    -----------    ---------------
!                 U, u      Uppercase      'TRUE', 'FALSE'
!
!                 L, l      Lowercase      'true', 'false'
!
!                 C, c      Capitalized    'True', 'False'
!
! $ Detailed_Output
!
!     OUT      is the string obtained by substituting the text
!              representation of VALUE for the first occurrence of
!              MARKER in the input string.
!
!              OUT and IN must be disjoint.
!
! $ Parameters
!
!     None.
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
!     into strings. They are typically used to construct messages that
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
!        CALL REPMCT ( STRING, '#1',  51,           'C', TMPSTR )
!        CALL REPMC  ( TMPSTR, '#2', '[USER.DATA]',      STRING )
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
!     1) The following example illustrates the use of REPML to replace
!        a marker within a string with the text representation of a
!        logical value.
!
!
!        Example code begins here.
!
!
!              PROGRAM REPML_EX1
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
!              INSTR  = 'Invalid value. The value was:  #.'
!
!              CALL REPML ( INSTR, MARKER, .FALSE. , 'U' , OUTSTR )
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
!              INSTR  = 'Invalid value. The value was:  XX.'
!
!              CALL REPML ( INSTR, MARKER, .TRUE. , 'l' , OUTSTR )
!
!              WRITE(*,*) 'Case 2: Replacement text in lowercase.'
!              WRITE(*,*) '   Input : ', INSTR
!              WRITE(*,*) '   Output: ', OUTSTR
!              WRITE(*,*) ' '
!
!        C
!        C     2. Capitalized
!        C
!              MARKER = '#'
!              INSTR  = 'Invalid value. The value was:  #.'
!
!              CALL REPML ( INSTR, MARKER, .FALSE. , 'c' , OUTSTR )
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
!            Input : Invalid value. The value was:  #.
!            Output: Invalid value. The value was:  FALSE.
!
!         Case 2: Replacement text in lowercase.
!            Input : Invalid value. The value was:  XX.
!            Output: Invalid value. The value was:  true.
!
!         Case 3: Replacement text capitalized.
!            Input : Invalid value. The value was:  #.
!            Output: Invalid value. The value was:  False.
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
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-JAN-2021 (JDR) (NJB)
!
!-&


! $ Index_Entries
!
!     replace marker with logical value
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           frstnb
      integer(ip)           isrchc
      integer(ip)           lastnb

      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: maxllt = 5

      integer(ip), parameter :: ncase = 3

!
!     Local variables
!
      character(maxllt)     lvalue
      character(maxllt)     valstr ( ncase, 2 )
      character(1)          tmpcas
      character(1)          casstr ( ncase )

      integer(ip)           casidx
      integer(ip)           mrknbf
      integer(ip)           mrknbl
      integer(ip)           mrkpsb
      integer(ip)           mrkpse
      integer(ip)           validx

!
!     Saved variables
!
      save                  casstr
      save                  valstr

!
!     Initial values
!
      data                  casstr / 'U', 'L', 'C' /

      data                  valstr / 'TRUE',  'true',  'True', &
                                     'FALSE', 'false', 'False'  /


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Identify the case string and find its index in the
!     array of uppercase case strings. Bail out if RTCASE is not
!     recognized.
!
!     RTCASE has length 1, so we need not be concerned with leading
!     blanks.
!
      call ucase ( rtcase, tmpcas )

      casidx = isrchc ( tmpcas, ncase, casstr )

      if ( casidx == 0 ) then

         call setmsg ( 'Case (#) must be U, L, or C.' )
         call errch  ( '#', rtcase                    )
         call sigerr ( 'SPICE(INVALIDCASE)'           )
         return

      end if

!
!     If MARKER is blank, no substitution is possible.
!
      if ( marker == ' ' ) then

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

!
!     MARKER is non-blank, so the index range below is valid.
!
      mrkpsb = index ( in, marker( mrknbf : mrknbl )  )

      if ( mrkpsb == 0 ) then

         out = in

         return

      end if

      mrkpse = mrkpsb + mrknbl - mrknbf

!
!     Okay, MARKER is non-blank and has been found.
!
      if ( value ) then
         validx = 1
      else
         validx = 2
      end if

!
!     Set the value string based on the case specification and
!     the input logical value.
!
      lvalue = valstr( casidx, validx )

!
!     Replace MARKER with LVALUE.
!
      call repsub ( in, &
                    mrkpsb, &
                    mrkpse, &
                    lvalue( : lastnb(lvalue) ), &
                    out )

      return
      end
