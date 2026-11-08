!$Procedure ZZTPATS (Private, Time --- Time Patterns)

      logical function zztpats ( room, nknown, known, meanng )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize the built-in time patterns.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      PRIVATE
!
! $ Declarations

      implicit none
      integer               room
      integer               nknown
      character*(*)         known  ( * )
      character*(*)         meanng ( * )

      integer               count
      parameter           ( count = 231 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ROOM       I   The declared space available for patterns
!     KNOWN      O   The patterns that are automatically recognized
!     MEANNG     O   The meaning associated with the patterns.
!     COUNT      P   The number of patterns built in to this routine.
!     The function returns .TRUE. if the initialization is successful.
!
! $ Detailed_Input
!
!     ROOM       an integer giving the room available for known patterns
!                and their meanings.
!
!                If ROOM does not equal the number of built-in patterns
!                the function returns only those patterns that will fit
!                and returns the value FALSE.
!
! $ Detailed_Output
!
!     NKNOWN     is the number of patterns/meanings returned in the
!                arrays KNOWN and MEANNG
!
!     KNOWN      is the array of automatically recognized calendar
!                date patterns.  KNOWN will be sorted according to
!                the FORTRAN collating sequence.
!
!     MEANNG     is the array of "meanings" associated with the built-in
!                patterns returned in the array KNOWN. MEANNG(I) is
!                the "meaning" associated with known pattern KNOWN(I).
!
!     The function returns TRUE if the arrays, KNOWN and MEANNG are
!     successfully initialized.  Otherwise it returns FALSE.
!
! $ Parameters
!
!     COUNT      is the number of patterns/meanings that are
!                returned by this routine.
!
! $ Exceptions
!
!     Error Free.
!
!     1) If ROOM is less than count, the function returns FALSE.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine that supports the SPICE routine
!     TPARTV that parses time strings.  This routine initializes
!     the set of built-in time patterns for use by TPARTV
!
!     The lengths of the strings assigned to an arbitrary MYKNWN
!     element must equal the string lengths of the corresponding
!     element MYMNNG. The relation, one-to-one and onto,
!     defining the meaning (or non meaning) and type of each
!     symbol in the time string abstract representation.
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.2.0, 01-OCT-2020 (EDW) (NJB) (BVS)
!
!        Implemented a FIRST block to prevent reinitialization
!        of patterns on each entry to routine.
!
!        Added patterns to accommodate ISO formats with 'Z' suffix.
!
!-    SPICELIB Version 3.1.0, 11-DEC-2013 (EDW)
!
!        Corrected typo which showed MEANNG(203) assigned
!        the value 'm*D*YH*M' rather than the correct assignment
!        to MYMNNG( 203 ). The assignment error often prevented
!        the Time subsystem from parsing time strings with
!        the format 'i-i-Yi:i'.
!
!        Corrected header section ordering to meet SPICE requirements.
!
!-    SPICELIB Version 3.0.0, 16-AUG-2002 (WLT)
!
!        The interface of the routine was changed from
!
!           ZZTPATS( ROOM, KNOWN, MEANNG )
!
!        to
!
!           ZZTPATS( ROOM, NKNOWN, KNOWN, MEANNG )
!
!        and made error free.
!
!-    SPICELIB Version 2.0.0, 16-APR-1997 (WLT)
!
!        The collection of recognized built in patterns was
!        increased from 185 to 203 patterns.  The new patterns
!        begin at KNOWN(186) below.
!
!-    SPICELIB Version 1.0.0, 02-APR-1996 (WLT)
!
!-&

      integer               wdsize
      parameter           ( wdsize = 32 )

      integer               i
      integer               ordvec ( count )

      character*(wdsize)    myknwn ( count )
      save                  myknwn

      character*(wdsize)    mymnng ( count )
      save                  mymnng

      logical               first
      save                  first

      data                  first / .true. /

      if (first) then

         first = .false.

         myknwn(   1 ) = 'Y-i-it'
         mymnng(   1 ) = 'Y*m*D*'


         myknwn(   2 ) = 'Y-i-iti:i'
         mymnng(   2 ) = 'Y*m*D*H*M'


         myknwn(   3 ) = 'Y-i-iti:i:i'
         mymnng(   3 ) = 'Y*m*D*H*M*S'


         myknwn(   4 ) = 'Y-i-iti:i:n'
         mymnng(   4 ) = 'Y*m*D*H*M*S'


         myknwn(   5 ) = 'Y-i-iti:n'
         mymnng(   5 ) = 'Y*m*D*H*M'


         myknwn(   6 ) = 'Y-i/'
         mymnng(   6 ) = 'Y*y*'


         myknwn(   7 ) = 'Y-i/i:i'
         mymnng(   7 ) = 'Y*y*H*M'


         myknwn(   8 ) = 'Y-i/i:i:i'
         mymnng(   8 ) = 'Y*y*H*M*S'


         myknwn(   9 ) = 'Y-i/i:i:n'
         mymnng(   9 ) = 'Y*y*H*M*S'


         myknwn(  10 ) = 'Y-i/i:n'
         mymnng(  10 ) = 'Y*y*H*M'


         myknwn(  11 ) = 'Y-id'
         mymnng(  11 ) = 'Y*y*'


         myknwn(  12 ) = 'Y-idi:i'
         mymnng(  12 ) = 'Y*y*H*M'


         myknwn(  13 ) = 'Y-idi:i:i'
         mymnng(  13 ) = 'Y*y*H*M*S'


         myknwn(  14 ) = 'Y-idi:i:n'
         mymnng(  14 ) = 'Y*y*H*M*S'


         myknwn(  15 ) = 'Y-idi:n'
         mymnng(  15 ) = 'Y*y*H*M'


         myknwn(  16 ) = 'Y-it'
         mymnng(  16 ) = 'Y*y*'


         myknwn(  17 ) = 'Y-iti:i'
         mymnng(  17 ) = 'Y*y*H*M'


         myknwn(  18 ) = 'Y-iti:i:i'
         mymnng(  18 ) = 'Y*y*H*M*S'


         myknwn(  19 ) = 'Y-iti:i:n'
         mymnng(  19 ) = 'Y*y*H*M*S'


         myknwn(  20 ) = 'Y-iti:n'
         mymnng(  20 ) = 'Y*y*H*M'


         myknwn(  21 ) = 'Yid'
         mymnng(  21 ) = 'Yy*'


         myknwn(  22 ) = 'Yidi:i'
         mymnng(  22 ) = 'Yy*H*M'


         myknwn(  23 ) = 'Yidi:i:i'
         mymnng(  23 ) = 'Yy*H*M*S'


         myknwn(  24 ) = 'Yidi:i:n'
         mymnng(  24 ) = 'Yy*H*M*S'


         myknwn(  25 ) = 'Yidi:n'
         mymnng(  25 ) = 'Yy*H*M'


         myknwn(  26 ) = 'Yii'
         mymnng(  26 ) = 'YmD'


         myknwn(  27 ) = 'Yiii'
         mymnng(  27 ) = 'YmDH'


         myknwn(  28 ) = 'Yiii:i'
         mymnng(  28 ) = 'YmDH*M'


         myknwn(  29 ) = 'Yiii:i:i'
         mymnng(  29 ) = 'YmDH*M*S'


         myknwn(  30 ) = 'Yiii:i:n'
         mymnng(  30 ) = 'YmDH*M*S'


         myknwn(  31 ) = 'Yiii:n'
         mymnng(  31 ) = 'YmDH*M'


         myknwn(  32 ) = 'Yiiii'
         mymnng(  32 ) = 'YmDHM'


         myknwn(  33 ) = 'Yiiiii'
         mymnng(  33 ) = 'YmDHMS'


         myknwn(  34 ) = 'Yiiiin'
         mymnng(  34 ) = 'YmDHMS'


         myknwn(  35 ) = 'Yiiin'
         mymnng(  35 ) = 'YmDHM'


         myknwn(  36 ) = 'Yiin'
         mymnng(  36 ) = 'YmDH'


         myknwn(  37 ) = 'Yim'
         mymnng(  37 ) = 'YDm'


         myknwn(  38 ) = 'Yimi'
         mymnng(  38 ) = 'YDmH'


         myknwn(  39 ) = 'Yimi:i'
         mymnng(  39 ) = 'YDmH*M'


         myknwn(  40 ) = 'Yimi:i:i'
         mymnng(  40 ) = 'YDmH*M*S'


         myknwn(  41 ) = 'Yimi:i:n'
         mymnng(  41 ) = 'YDmH*M*S'


         myknwn(  42 ) = 'Yimi:n'
         mymnng(  42 ) = 'YDmH*M'


         myknwn(  43 ) = 'Yimn'
         mymnng(  43 ) = 'YDmH'


         myknwn(  44 ) = 'Yin'
         mymnng(  44 ) = 'YmD'


         myknwn(  45 ) = 'Ymi'
         mymnng(  45 ) = 'YmD'


         myknwn(  46 ) = 'Ymii'
         mymnng(  46 ) = 'YmDH'


         myknwn(  47 ) = 'Ymii:i'
         mymnng(  47 ) = 'YmDH*M'


         myknwn(  48 ) = 'Ymii:i:i'
         mymnng(  48 ) = 'YmDH*M*S'


         myknwn(  49 ) = 'Ymii:i:n'
         mymnng(  49 ) = 'YmDH*M*S'


         myknwn(  50 ) = 'Ymii:n'
         mymnng(  50 ) = 'YmDH*M'


         myknwn(  51 ) = 'Ymin'
         mymnng(  51 ) = 'YmDH'


         myknwn(  52 ) = 'Ymn'
         mymnng(  52 ) = 'YmD'


         myknwn(  53 ) = 'Ynm'
         mymnng(  53 ) = 'YDm'


         myknwn(  54 ) = 'i-Y/'
         mymnng(  54 ) = 'y*Y*'


         myknwn(  55 ) = 'i-Y/i:i'
         mymnng(  55 ) = 'y*Y*H*M'


         myknwn(  56 ) = 'i-Y/i:i:i'
         mymnng(  56 ) = 'y*Y*H*M*S'


         myknwn(  57 ) = 'i-Y/i:i:n'
         mymnng(  57 ) = 'y*Y*H*M*S'


         myknwn(  58 ) = 'i-Y/i:n'
         mymnng(  58 ) = 'y*Y*H*M'


         myknwn(  59 ) = 'i-Yd'
         mymnng(  59 ) = 'y*Y*'


         myknwn(  60 ) = 'i-Ydi:i'
         mymnng(  60 ) = 'y*Y*H*M'


         myknwn(  61 ) = 'i-Ydi:i:i'
         mymnng(  61 ) = 'y*Y*H*M*S'


         myknwn(  62 ) = 'i-Ydi:i:n'
         mymnng(  62 ) = 'y*Y*H*M*S'


         myknwn(  63 ) = 'i-Ydi:n'
         mymnng(  63 ) = 'y*Y*H*M'


         myknwn(  64 ) = 'i-i-it'
         mymnng(  64 ) = 'Y*m*D*'


         myknwn(  65 ) = 'i-i-iti:i'
         mymnng(  65 ) = 'Y*m*D*H*M'


         myknwn(  66 ) = 'i-i-iti:i:i'
         mymnng(  66 ) = 'Y*m*D*H*M*S'


         myknwn(  67 ) = 'i-i-iti:i:n'
         mymnng(  67 ) = 'Y*m*D*H*M*S'


         myknwn(  68 ) = 'i-i-iti:n'
         mymnng(  68 ) = 'Y*m*D*H*M'


         myknwn(  69 ) = 'i-i/i:i'
         mymnng(  69 ) = 'Y*y*H*M'


         myknwn(  70 ) = 'i-i/i:i:i'
         mymnng(  70 ) = 'Y*y*H*M*S'


         myknwn(  71 ) = 'i-i/i:i:n'
         mymnng(  71 ) = 'Y*y*H*M*S'


         myknwn(  72 ) = 'i-i/i:n'
         mymnng(  72 ) = 'Y*y*H*M'


         myknwn(  73 ) = 'i-idi:i'
         mymnng(  73 ) = 'Y*y*H*M'


         myknwn(  74 ) = 'i-idi:i:i'
         mymnng(  74 ) = 'Y*y*H*M*S'


         myknwn(  75 ) = 'i-idi:i:n'
         mymnng(  75 ) = 'Y*y*H*M*S'


         myknwn(  76 ) = 'i-idi:n'
         mymnng(  76 ) = 'Y*y*H*M'


         myknwn(  77 ) = 'i-it'
         mymnng(  77 ) = 'Y*y*'


         myknwn(  78 ) = 'i-iti:i'
         mymnng(  78 ) = 'Y*y*H*M'


         myknwn(  79 ) = 'i-iti:i:i'
         mymnng(  79 ) = 'Y*y*H*M*S'


         myknwn(  80 ) = 'i-iti:i:n'
         mymnng(  80 ) = 'Y*y*H*M*S'


         myknwn(  81 ) = 'i-iti:n'
         mymnng(  81 ) = 'Y*y*H*M'


         myknwn(  82 ) = 'i:i:iimY'
         mymnng(  82 ) = 'H*M*SDmY'


         myknwn(  83 ) = 'i:i:imiY'
         mymnng(  83 ) = 'H*M*SmDY'


         myknwn(  84 ) = 'i:i:nimY'
         mymnng(  84 ) = 'H*M*SDmY'


         myknwn(  85 ) = 'i:i:nmiY'
         mymnng(  85 ) = 'H*M*SmDY'


         myknwn(  86 ) = 'i:iimY'
         mymnng(  86 ) = 'H*MDmY'


         myknwn(  87 ) = 'i:imiY'
         mymnng(  87 ) = 'H*MmDY'


         myknwn(  88 ) = 'i:nimY'
         mymnng(  88 ) = 'H*MDmY'


         myknwn(  89 ) = 'i:nmiY'
         mymnng(  89 ) = 'H*MmDY'


         myknwn(  90 ) = 'iYd'
         mymnng(  90 ) = 'yY*'


         myknwn(  91 ) = 'iYdi:i'
         mymnng(  91 ) = 'yY*H*M'


         myknwn(  92 ) = 'iYdi:i:i'
         mymnng(  92 ) = 'yY*H*M*S'


         myknwn(  93 ) = 'iYdi:i:n'
         mymnng(  93 ) = 'yY*H*M*S'


         myknwn(  94 ) = 'iYdi:n'
         mymnng(  94 ) = 'yY*H*M'


         myknwn(  95 ) = 'iiY'
         mymnng(  95 ) = 'mDY'


         myknwn(  96 ) = 'iiYi'
         mymnng(  96 ) = 'mDYH'


         myknwn(  97 ) = 'iiYi:i'
         mymnng(  97 ) = 'mDYH*M'


         myknwn(  98 ) = 'iiYi:i:i'
         mymnng(  98 ) = 'mDYH*M*S'


         myknwn(  99 ) = 'iiYi:i:n'
         mymnng(  99 ) = 'mDYH*M*S'


         myknwn( 100 ) = 'iiYi:n'
         mymnng( 100 ) = 'mDYH*M'


         myknwn( 101 ) = 'iiYn'
         mymnng( 101 ) = 'mDYH'


         myknwn( 102 ) = 'iid'
         mymnng( 102 ) = 'Yy*'


         myknwn( 103 ) = 'iidi:i'
         mymnng( 103 ) = 'Yy*H*M'


         myknwn( 104 ) = 'iidi:i:i'
         mymnng( 104 ) = 'Yy*H*M*S'


         myknwn( 105 ) = 'iidi:i:n'
         mymnng( 105 ) = 'Yy*H*M*S'


         myknwn( 106 ) = 'iidi:n'
         mymnng( 106 ) = 'Yy*H*M'


         myknwn( 107 ) = 'iim'
         mymnng( 107 ) = 'YDm'


         myknwn( 108 ) = 'iimi'
         mymnng( 108 ) = 'YDmH'


         myknwn( 109 ) = 'iimi:i'
         mymnng( 109 ) = 'YDmH*M'


         myknwn( 110 ) = 'iimi:i:i'
         mymnng( 110 ) = 'YDmH*M*S'


         myknwn( 111 ) = 'iimi:i:n'
         mymnng( 111 ) = 'YDmH*M*S'


         myknwn( 112 ) = 'iimi:n'
         mymnng( 112 ) = 'YDmH*M'


         myknwn( 113 ) = 'iimii'
         mymnng( 113 ) = 'YDmHM'


         myknwn( 114 ) = 'iimiii'
         mymnng( 114 ) = 'YDmHMS'


         myknwn( 115 ) = 'iimiin'
         mymnng( 115 ) = 'YDmHMS'


         myknwn( 116 ) = 'iimin'
         mymnng( 116 ) = 'YDmHM'


         myknwn( 117 ) = 'iimn'
         mymnng( 117 ) = 'YDmH'


         myknwn( 118 ) = 'imY'
         mymnng( 118 ) = 'DmY'


         myknwn( 119 ) = 'imYi'
         mymnng( 119 ) = 'DmYH'


         myknwn( 120 ) = 'imYi:i'
         mymnng( 120 ) = 'DmYH*M'


         myknwn( 121 ) = 'imYi:i:i'
         mymnng( 121 ) = 'DmYH*M*S'


         myknwn( 122 ) = 'imYi:i:n'
         mymnng( 122 ) = 'DmYH*M*S'


         myknwn( 123 ) = 'imYi:n'
         mymnng( 123 ) = 'DmYH*M'


         myknwn( 124 ) = 'imYn'
         mymnng( 124 ) = 'DmYH'


         myknwn( 125 ) = 'imi'
         mymnng( 125 ) = 'YmD'


         myknwn( 126 ) = 'imi:i:iY'
         mymnng( 126 ) = 'DmH*M*SY'


         myknwn( 127 ) = 'imi:i:nY'
         mymnng( 127 ) = 'DmH*M*SY'


         myknwn( 128 ) = 'imi:iY'
         mymnng( 128 ) = 'DmH*MY'


         myknwn( 129 ) = 'imi:nY'
         mymnng( 129 ) = 'DmH*MY'


         myknwn( 130 ) = 'imii'
         mymnng( 130 ) = 'YmDH'


         myknwn( 131 ) = 'imii:i'
         mymnng( 131 ) = 'YmDH*M'


         myknwn( 132 ) = 'imii:i:i'
         mymnng( 132 ) = 'YmDH*M*S'


         myknwn( 133 ) = 'imii:i:n'
         mymnng( 133 ) = 'YmDH*M*S'


         myknwn( 134 ) = 'imii:n'
         mymnng( 134 ) = 'YmDH*M'


         myknwn( 135 ) = 'imiii'
         mymnng( 135 ) = 'YmDHM'


         myknwn( 136 ) = 'imiiii'
         mymnng( 136 ) = 'YmDHMS'


         myknwn( 137 ) = 'imiiin'
         mymnng( 137 ) = 'YmDHMS'


         myknwn( 138 ) = 'imiin'
         mymnng( 138 ) = 'YmDHM'


         myknwn( 139 ) = 'imin'
         mymnng( 139 ) = 'YmDH'


         myknwn( 140 ) = 'imn'
         mymnng( 140 ) = 'YmD'


         myknwn( 141 ) = 'inY'
         mymnng( 141 ) = 'mDY'


         myknwn( 142 ) = 'inm'
         mymnng( 142 ) = 'YDm'


         myknwn( 143 ) = 'miY'
         mymnng( 143 ) = 'mDY'


         myknwn( 144 ) = 'miYi'
         mymnng( 144 ) = 'mDYH'


         myknwn( 145 ) = 'miYi:i'
         mymnng( 145 ) = 'mDYH*M'


         myknwn( 146 ) = 'miYi:i:i'
         mymnng( 146 ) = 'mDYH*M*S'


         myknwn( 147 ) = 'miYi:i:n'
         mymnng( 147 ) = 'mDYH*M*S'


         myknwn( 148 ) = 'miYi:n'
         mymnng( 148 ) = 'mDYH*M'


         myknwn( 149 ) = 'miYn'
         mymnng( 149 ) = 'mDYH'


         myknwn( 150 ) = 'mii'
         mymnng( 150 ) = 'mDY'


         myknwn( 151 ) = 'mii:i:iY'
         mymnng( 151 ) = 'mDH*M*SY'


         myknwn( 152 ) = 'mii:i:nY'
         mymnng( 152 ) = 'mDH*M*SY'


         myknwn( 153 ) = 'mii:iY'
         mymnng( 153 ) = 'mDH*MY'


         myknwn( 154 ) = 'mii:nY'
         mymnng( 154 ) = 'mDH*MY'


         myknwn( 155 ) = 'miii'
         mymnng( 155 ) = 'mDYH'


         myknwn( 156 ) = 'miii:i'
         mymnng( 156 ) = 'mDYH*M'


         myknwn( 157 ) = 'miii:i:i'
         mymnng( 157 ) = 'mDYH*M*S'


         myknwn( 158 ) = 'miii:i:n'
         mymnng( 158 ) = 'mDYH*M*S'


         myknwn( 159 ) = 'miii:n'
         mymnng( 159 ) = 'mDYH*M'


         myknwn( 160 ) = 'miiii'
         mymnng( 160 ) = 'mDYHM'


         myknwn( 161 ) = 'miiiii'
         mymnng( 161 ) = 'mDYHMS'


         myknwn( 162 ) = 'miiiin'
         mymnng( 162 ) = 'mDYHMS'


         myknwn( 163 ) = 'miiin'
         mymnng( 163 ) = 'mDYHM'


         myknwn( 164 ) = 'miin'
         mymnng( 164 ) = 'mDYH'


         myknwn( 165 ) = 'mnY'
         mymnng( 165 ) = 'mDY'


         myknwn( 166 ) = 'mni'
         mymnng( 166 ) = 'mDY'


         myknwn( 167 ) = 'nmY'
         mymnng( 167 ) = 'DmY'


         myknwn( 168 ) = 'i/i/i'
         mymnng( 168 ) = 'm*D*Y'

         myknwn( 169 ) = 'i/i/ii:i'
         mymnng( 169 ) = 'm*D*YH*M'


         myknwn( 170 ) = 'i/i/ii:n'
         mymnng( 170 ) = 'm*D*YH*M'

         myknwn( 171 ) = 'i/i/ii:i:n'
         mymnng( 171 ) = 'm*D*YH*M*S'


         myknwn( 172 ) = 'i/i/ii:i:i'
         mymnng( 172 ) = 'm*D*YH*M*S'


         myknwn( 173 ) = 'i/i/Y'
         mymnng( 173 ) = 'm*D*Y'


         myknwn( 174 ) = 'i/i/Yi:i'
         mymnng( 174 ) = 'm*D*YH*M'


         myknwn( 175 ) = 'i/i/ii:n'
         mymnng( 175 ) = 'm*D*YH*M'


         myknwn( 176 ) = 'i/i/Yi:i:n'
         mymnng( 176 ) = 'm*D*YH*M*S'

         myknwn( 177 ) = 'i/i/Yi:i:i'
         mymnng( 177 ) = 'm*D*YH*M*S'


         myknwn( 178 ) = 'Y-i-iti'
         mymnng( 178 ) = 'Y*m*D*H'


         myknwn( 179 ) = 'Y-iti'
         mymnng( 179 ) = 'Y*y*H'


         myknwn( 180 ) = 'Y-i-itn'
         mymnng( 180 ) = 'Y*m*D*H'


         myknwn( 181 ) = 'Y-itn'
         mymnng( 181 ) = 'Y*y*H'


         myknwn( 182 ) = 'i-i-iti'
         mymnng( 182 ) = 'Y*m*D*H'


         myknwn( 183 ) = 'i-i-itn'
         mymnng( 183 ) = 'Y*m*D*H'


         myknwn( 184 ) = 'i-iti'
         mymnng( 184 ) = 'Y*y*H'


         myknwn( 185 ) = 'i-itn'
         mymnng( 185 ) = 'Y*y*H'


         myknwn( 186 ) = 'i:ii/i/i'
         mymnng( 186 ) = 'H*Mm*D*Y'


         myknwn( 187 ) = 'i:ni/i/i'
         mymnng( 187 ) = 'H*Mm*D*Y'


         myknwn( 188 ) = 'i:i:ii/i/i'
         mymnng( 188 ) = 'H*M*Sm*D*Y'


         myknwn( 189 ) = 'i:i:ni/i/i'
         mymnng( 189 ) = 'H*M*Sm*D*Y'


         myknwn( 190 ) = 'i:ii/i/Y'
         mymnng( 190 ) = 'H*Mm*D*Y'


         myknwn( 191 ) = 'i:ni/i/Y'
         mymnng( 191 ) = 'H*Mm*D*Y'


         myknwn( 192 ) = 'i:i:ii/i/Y'
         mymnng( 192 ) = 'H*M*Sm*D*Y'


         myknwn( 193 ) = 'i:i:ni/i/Y'
         mymnng( 193 ) = 'H*M*Sm*D*Y'


         myknwn( 194 ) = 'i:ii-i-Y'
         mymnng( 194 ) = 'H*Mm*D*Y'


         myknwn( 195 ) = 'i:ni-i-Y'
         mymnng( 195 ) = 'H*Mm*D*Y'


         myknwn( 196 ) = 'i:i:ii-i-Y'
         mymnng( 196 ) = 'H*M*Sm*D*Y'


         myknwn( 197 ) = 'i:i:ni-i-Y'
         mymnng( 197 ) = 'H*M*Sm*D*Y'


         myknwn( 198 ) = 'i/i/Y/i:n'
         mymnng( 198 ) = 'm*D*Y*H*M'


         myknwn( 199 ) = 'i-i-Y'
         mymnng( 199 ) = 'm*D*Y'


         myknwn( 200 ) = 'i-i-Yi:n'
         mymnng( 200 ) = 'm*D*YH*M'


         myknwn( 201 ) = 'i-i-Yi:i:n'
         mymnng( 201 ) = 'm*D*YH*M*S'


         myknwn( 202 ) = 'i-i-Yi:i:i'
         mymnng( 202 ) = 'm*D*YH*M*S'


         myknwn( 203 ) = 'i-i-Yi:i'
         mymnng( 203 ) = 'm*D*YH*M'


!
!        ISO patterns allowing the 'Z' suffix, only a suffix.
!
         myknwn( 204 ) = 'Y-i-itx'
         mymnng( 204 ) = 'Y*m*D**'


         myknwn( 205 ) = 'Y-i-iti:ix'
         mymnng( 205 ) = 'Y*m*D*H*M*'


         myknwn( 206 ) = 'Y-i-iti:i:ix'
         mymnng( 206 ) = 'Y*m*D*H*M*S*'


         myknwn( 207 ) = 'Y-i-iti:i:nx'
         mymnng( 207 ) = 'Y*m*D*H*M*S*'


         myknwn( 208 ) = 'Y-i-iti:nx'
         mymnng( 208 ) = 'Y*m*D*H*M*'

         myknwn( 209 ) = 'Y-itx'
         mymnng( 209 ) = 'Y*y**'


         myknwn( 210 ) = 'Y-iti:ix'
         mymnng( 210 ) = 'Y*y*H*M*'


         myknwn( 211 ) = 'Y-iti:i:ix'
         mymnng( 211 ) = 'Y*y*H*M*S*'


         myknwn( 212 ) = 'Y-iti:i:nx'
         mymnng( 212 ) = 'Y*y*H*M*S*'


         myknwn( 213 ) = 'Y-iti:nx'
         mymnng( 213 ) = 'Y*y*H*M*'


         myknwn( 214 ) = 'i-i-itx'
         mymnng( 214 ) = 'Y*m*D**'


         myknwn( 215 ) = 'i-i-iti:ix'
         mymnng( 215 ) = 'Y*m*D*H*M*'


         myknwn( 216 ) = 'i-i-iti:i:ix'
         mymnng( 216 ) = 'Y*m*D*H*M*S*'


         myknwn( 217 ) = 'i-i-iti:i:nx'
         mymnng( 217 ) = 'Y*m*D*H*M*S*'


         myknwn( 218 ) = 'i-i-iti:nx'
         mymnng( 218 ) = 'Y*m*D*H*M*'


         myknwn( 219 ) = 'i-itx'
         mymnng( 219 ) = 'Y*y**'


         myknwn( 220 ) = 'i-iti:ix'
         mymnng( 220 ) = 'Y*y*H*M*'


         myknwn( 221 ) = 'i-iti:i:ix'
         mymnng( 221 ) = 'Y*y*H*M*S*'


         myknwn( 222 ) = 'i-iti:i:nx'
         mymnng( 222 ) = 'Y*y*H*M*S*'


         myknwn( 223 ) = 'i-iti:nx'
         mymnng( 223 ) = 'Y*y*H*M*'


         myknwn( 224 ) = 'Y-i-itix'
         mymnng( 224 ) = 'Y*m*D*H*'


         myknwn( 225 ) = 'Y-itix'
         mymnng( 225 ) = 'Y*y*H*'


         myknwn( 226 ) = 'Y-i-itnx'
         mymnng( 226 ) = 'Y*m*D*H*'


         myknwn( 227 ) = 'Y-itnx'
         mymnng( 227 ) = 'Y*y*H*'


         myknwn( 228 ) = 'i-i-itix'
         mymnng( 228 ) = 'Y*m*D*H*'


         myknwn( 229 ) = 'i-i-itnx'
         mymnng( 229 ) = 'Y*m*D*H*'


         myknwn( 230 ) = 'i-itix'
         mymnng( 230 ) = 'Y*y*H*'


         myknwn( 231 ) = 'i-itnx'
         mymnng( 231 ) = 'Y*y*H*'

      end if

!
!     Copy as many patterns and meanings as the input arrays allow.
!
      nknown = min ( count, room )
      do i = 1, nknown
         known (i) = myknwn(i)
         meanng(i) = mymnng(i)
      end do

!
!     Make sure everything is in the proper order.
!
      call orderc ( known,  nknown, ordvec )
      call reordc ( ordvec, nknown, known  )
      call reordc ( ordvec, nknown, meanng )

!
!     If there wasn't sufficient room to get all of the patterns
!     and meanings, return FALSE.
!
      if ( count .gt. room ) then
         zztpats = .false.
         return
      end if

      zztpats = .true.

      return
      end
