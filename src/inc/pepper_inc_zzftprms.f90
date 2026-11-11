!> Private FTP Validation String Parameters.
!>
!>     This include file centralizes the definition of string sizes
!>     and other parameters that are necessary to properly implement
!>     the FTP error detection scheme for binary kernels.
!>
!>     Before making any alterations to the contents of this file,
!>     refer to the header of ZZFTPSTR for a detailed discussion of
!>     the FTP validation string.
!>
!>## Version
!>
!>        zzftprms.inc Version 1    01-MAR-1999 (FST)
module pepper_inc_zzftprms

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: sizstr = 16  !! Size of FTP Test String Component:

   integer(ip), parameter :: sizexp = (3 * sizstr)  !! Size of Maximum Expanded FTP Validation String:
                                                    !!
                                                    !! (This indicates the size of a buffer to hold the test
                                                    !! string sequence from a possibly corrupt file. Empirical
                                                    !! evidence strongly indicates that expansion due to FTP
                                                    !! corruption at worst doubles the number of characters.
                                                    !! So take 3*SIZSTR to be on the safe side.)

   integer(ip), parameter :: sizend = 6  !! Size of FTP Validation String Brackets:

   integer(ip), parameter :: sizftp = ( sizstr + 2*sizend )  !! Size of FTP Validation String:

   integer(ip), parameter :: sizdlm = 1  !! Size of DELIM.

   integer(ip), parameter :: numtst = 6  !! Number of character clusters present in the validation string.

end module pepper_inc_zzftprms
