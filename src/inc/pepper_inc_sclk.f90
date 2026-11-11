!> The parameters below define sizes and limits used by the SCLK system.
!>
!> SPICE private file intended solely for the support of SPICE routines.  Users should not include this file directly due to the volatile nature of this file
!>
!>## Parameters
!>
!>     See the declaration section below.
!>
!>## Author and Institution
!>
!>     N.J. Bachman    (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 2.0.1, 20-OCT-2020 (NJB)
!>
!>        Increased MXCOEF to 100000.
!>
!>        Updated comments with reminder to keep constants declared
!>        in the include file zzsc01.inc synced with constants in
!>        this file.
!>
!>-    SPICELIB Version 2.0.0, 24-MAY-2010 (NJB)
!>
!>        Increased value of maximum coefficient record count
!>        parameter MXCOEF from 10K to 50K.
!>
!>-    SPICELIB Version 1.0.0, 11-FEB-2008 (NJB)
module pepper_inc_sclk

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: ndelim = 5  !!    NOTE: many of the declarations present here are duplicated
                                         !!     in the include file zzsc01.inc. Declarations in that file
                                         !!     must be kept in sync with those in this file. The
                                         !!     duplicated declarations are:
                                         !!
                                         !!        NDELIM
                                         !!        DELIMS
                                         !!        MXPART
                                         !!        MXCOEF
                                         !!        MXNFLD
                                         !!        DPLEN
                                         !!
                                         !! Number of supported SCLK field delimiters:

   character(ndelim), parameter :: delims = '.:-, '  !! Supported SCLK string field delimiters:

   integer(ip), parameter :: mxpart = 9999  !! Maximum number of partitions:

   integer(ip), parameter :: partln = 5  !! Partition string length.
                                         !!
                                         !! Since the maximum number of partitions is given by MXPART is
                                         !! 9999, PRTSTR needs at most 4 characters for the partition number
                                         !! and one character for the slash.

   integer(ip), parameter :: mxcoef = 100000  !! Maximum number of coefficient records:

   integer(ip), parameter :: mxnfld = 10  !! Maximum number of fields in an SCLK string:

   integer(ip), parameter :: dplen = 30  !! Length of strings used to represent D.P.
                                         !! numbers:

   integer(ip), parameter :: mxtsys = 2  !! Maximum number of supported parallel time systems:

end module pepper_inc_sclk
