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
!
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
