!     Include Section:  Private FTP Validation String Parameters
!
!        zzftprms.inc Version 1    01-MAR-1999 (FST)
!
!     This include file centralizes the definition of string sizes
!     and other parameters that are necessary to properly implement
!     the FTP error detection scheme for binary kernels.
!
!     Before making any alterations to the contents of this file,
!     refer to the header of ZZFTPSTR for a detailed discussion of
!     the FTP validation string.
!
!     Size of FTP Test String Component:
!
      INTEGER               SIZSTR
      PARAMETER           ( SIZSTR = 16 )

!
!     Size of Maximum Expanded FTP Validation String:
!
!      (This indicates the size of a buffer to hold the test
!       string sequence from a possibly corrupt file. Empirical
!       evidence strongly indicates that expansion due to FTP
!       corruption at worst doubles the number of characters.
!       So take 3*SIZSTR to be on the safe side.)
!
      INTEGER               SIZEXP
      PARAMETER           ( SIZEXP = (3 * SIZSTR) )

!
!     Size of FTP Validation String Brackets:
!
      INTEGER               SIZEND
      PARAMETER           ( SIZEND = 6 )

!
!     Size of FTP Validation String:
!
      INTEGER               SIZFTP
      PARAMETER           ( SIZFTP = ( SIZSTR + 2*SIZEND ) )

!
!     Size of DELIM.
!
      INTEGER               SIZDLM
      PARAMETER           ( SIZDLM = 1 )

!
!     Number of character clusters present in the validation string.
!
      INTEGER               NUMTST
      PARAMETER           ( NUMTST = 6 )

!
!     End Include Section:  Private FTP Validation String Parameters
!
