!> The parameters below define the structure of an aberration correction attribute block.
!>
!> SPICE private file intended solely for the support of SPICE routines.  Users should not include this file directly due to the volatile nature of this file
!>
!>## Parameters
!>
!>     An aberration correction attribute block is an array of logical
!>     flags indicating the attributes of the aberration correction
!>     specified by an aberration correction string.  The attributes
!>     are:
!>
!>        - Is the correction "geometric"?
!>
!>        - Is light time correction indicated?
!>
!>        - Is stellar aberration correction indicated?
!>
!>        - Is the light time correction of the "converged
!>          Newtonian" variety?
!>
!>        - Is the correction for the transmission case?
!>
!>        - Is the correction relativistic?
!>
!>    The parameters defining the structure of the block are as
!>    follows:
!>
!>       NABCOR    Number of aberration correction choices.
!>
!>       ABATSZ    Number of elements in the aberration correction
!>                 block.
!>
!>       GEOIDX    Index in block of geometric correction flag.
!>
!>       LTIDX     Index of light time flag.
!>
!>       STLIDX    Index of stellar aberration flag.
!>
!>       CNVIDX    Index of converged Newtonian flag.
!>
!>       XMTIDX    Index of transmission flag.
!>
!>       RELIDX    Index of relativistic flag.
!>
!>    The following parameter is not required to define the block
!>    structure, but it is convenient to include it here:
!>
!>       CORLEN    The maximum string length required by any aberration
!>                 correction string
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
!>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
module pepper_inc_zzabcorr

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: nabcor = 15  !! Number of aberration correction choices:

   integer(ip), parameter :: abatsz = 6  !! Aberration correction attribute block size
                                         !! (number of aberration correction attributes):

   integer(ip), parameter :: geoidx = 1  !! Indices of attributes within an aberration correction
                                         !! attribute block:

   integer(ip), parameter :: ltidx = geoidx + 1

   integer(ip), parameter :: stlidx = ltidx + 1

   integer(ip), parameter :: cnvidx = stlidx + 1

   integer(ip), parameter :: xmtidx = cnvidx + 1

   integer(ip), parameter :: relidx = xmtidx + 1

   integer(ip), parameter :: corlen = 5  !! Maximum length of an aberration correction string:

end module pepper_inc_zzabcorr
