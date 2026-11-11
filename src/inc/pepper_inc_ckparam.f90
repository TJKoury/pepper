!> Declarations of the CK data type specific and general CK low level routine parameters.
!>
!>## Restrictions
!>
!>     1) If new CK types are added, the size of the record passed
!>        between CKRxx and CKExx must be registered as separate
!>        parameter. If this size will be greater than current value
!>        of the CKMRSZ parameter (which specifies the maximum record
!>        size for the record buffer used inside CKPFS) then it should
!>        be assigned to CKMRSZ as a new value.
!>
!>## Author and Institution
!>
!>     N.J. Bachman      (JPL)
!>     B.V. Semenov      (JPL)
!>
!>## Literature References
!>
!>     CK Required Reading.
!>
!>## Version
!>
!>-    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB)
!>
!>        Updated to support CK type 6. Maximum degree for
!>        type 5 was updated to be consistent with the
!>        maximum degree for type 6.
!>
!>-    SPICELIB Version 2.0.0, 19-AUG-2002 (NJB)
!>
!>        Updated to support CK type 5.
!>
!>-    SPICELIB Version 1.0.0, 05-APR-1999 (BVS)
module pepper_inc_ckparam

   use pepper_kinds, only: ip, wp

   implicit none

   public

   integer(ip), parameter :: qsiz = 4  !! Number of quaternion components and number of quaternion and
                                       !! angular rate components together.

   integer(ip), parameter :: qavsiz = 7

   integer(ip), parameter :: ck1dtp = 1  !! CK Type 1 parameters: CK data type 1 ID
   integer(ip), parameter :: ck1rsz = 8  !! CK Type 1 parameters: maximum size of a record passed between CKR01
                                         !! and CKE01.

   integer(ip), parameter :: ck2dtp = 2  !! CK Type 2 parameters: CK data type 2 ID
   integer(ip), parameter :: ck2rsz = 10 !! CK Type 2 parameters: maximum size of a record passed between CKR02
                                         !! and CKE02.

   integer(ip), parameter :: ck3dtp = 3  !! CK Type 3 parameters: CK data type 3 ID
   integer(ip), parameter :: ck3rsz = 17 !! CK Type 3 parameters: maximum size of a record passed between CKR03
                                         !! and CKE03.

   ! CK Type 4 parameters:
   integer(ip), parameter :: ck4dtp = 4  !! CK data type 4 ID
   real(wp), parameter :: ck4pcd = 128.0_wp !! parameter defining integer to DP packing schema that
                                            !! is applied when seven number integer array containing
                                            !! polynomial degrees for quaternion and angular rate
                                            !! components packed into a single DP number stored in
                                            !! actual CK records in a file; the value of must not be
                                            !! changed or compatibility with existing type 4 CK files
                                            !! will be lost.
   integer(ip), parameter :: ck4mxd = 18 !! maximum Chebychev polynomial degree allowed in type 4
                                         !! records; the value of this parameter must never exceed
                                         !! value of the CK4PCD;
   integer(ip), parameter :: ck4sft = 10 !! number of additional DPs, which are not polynomial
                                         !! coefficients, located at the beginning of a type 4
                                         !! CK record that passed between routines CKR04 and CKE04;
   integer(ip), parameter :: ck4rsz = ( ck4mxd + 1 ) * qavsiz + ck4sft !! maximum size of type 4 CK record passed between CKR04
                                                                       !! and CKE04; CK4RSZ is computed as follows:
                                                                       !!
                                                                       !!     CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT

   ! CK Type 5 parameters:
   integer(ip), parameter :: ck5dtp = 5  !! CK data type 5 ID;
   integer(ip), parameter :: ck5mxd = 23 !! maximum polynomial degree allowed in type 5
                                         !! records.
   integer(ip), parameter :: ck5met = 4 !! number of additional DPs, which are not polynomial
                                        !! coefficients, located at the beginning of a type 5
                                        !! CK record that passed between routines CKR05 and CKE05;
   integer(ip), parameter :: ck5mxp = 14 !! maximum packet size for any subtype.  Subtype 2
                                         !! has the greatest packet size, since these packets
                                         !! contain a quaternion, its derivative, an angular
                                         !! velocity vector, and its derivative.  See ck05.inc
                                         !! for a description of the subtypes.
   integer(ip), parameter :: ck5rsz = (ck5mxd + 1) * ck5mxp + ck5met !! maximum size of type 5 CK record passed between CKR05
                                                                     !! and CKE05; CK5RSZ is computed as follows:
                                                                     !!
                                                                     !!     CK5RSZ = ( CK5MXD + 1 ) * CK5MXP + CK5MET

   ! CK Type 6 parameters:
   integer(ip), parameter :: ck6dtp = 6  !! CK6DTP CK data type 6 ID;
   integer(ip), parameter :: ck6mxd = 23 !! maximum polynomial degree allowed in type 6
                                         !! records.
   integer(ip), parameter :: ck6met = 4 !! number of additional DPs, which are not polynomial
                                        !! coefficients, located at the beginning of a type 6
                                        !! CK record that passed between routines CKR06 and CKE06;
   integer(ip), parameter :: ck6ps3 = 7  !! The parameter CK6PS3 must be in sync with C06PS3 defined in
                                         !! ck06.inc.
   integer(ip), parameter :: ck6rsz = ( ck6mxd + 1 ) * ( ck6ps3 + 1 ) + ck6met
         !! maximum size of type 6 CK record passed between CKR06
         !! and CKE06; CK6RSZ is computed as follows:
         !!
         !!     CK6RSZ = CK6MET + ( CK6MXD + 1 ) * ( CK6PS3 + 1 )
         !!
         !! where CK6PS3 is equal to the parameter CK06PS3 defined
         !! in ck06.inc. Note that the subtype having the largest
         !! packet size (subtype 2) does not give rise to the
         !! largest record size, because that type is Hermite and
         !! requires half the window size used by subtype 3 for a
         !! given polynomial degree.

   integer(ip), parameter :: ckmrsz = ck5rsz  !! Maximum record size that can be handled by CKPFS. This value
                                              !! must be set to the maximum of all CKxRSZ parameters (currently
                                              !! CK5RSZ.)

end module pepper_inc_ckparam
