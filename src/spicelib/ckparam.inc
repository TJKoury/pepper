! $ Abstract
!
!     Declarations of the CK data type specific and general CK low
!     level routine parameters.
!
! $ Required_Reading
!
!     CK.REQ
!
! $ Keywords
!
!     CK
!
! $ Restrictions
!
!     1) If new CK types are added, the size of the record passed
!        between CKRxx and CKExx must be registered as separate
!        parameter. If this size will be greater than current value
!        of the CKMRSZ parameter (which specifies the maximum record
!        size for the record buffer used inside CKPFS) then it should
!        be assigned to CKMRSZ as a new value.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!     B.V. Semenov      (JPL)
!
! $ Literature_References
!
!     CK Required Reading.
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB)
!
!        Updated to support CK type 6. Maximum degree for
!        type 5 was updated to be consistent with the
!        maximum degree for type 6.
!
!-    SPICELIB Version 2.0.0, 19-AUG-2002 (NJB)
!
!        Updated to support CK type 5.
!
!-    SPICELIB Version 1.0.0, 05-APR-1999 (BVS)
!
!-&

!
!     Number of quaternion components and number of quaternion and
!     angular rate components together.
!
      integer               qsiz
      parameter           ( qsiz   = 4 )

      integer               qavsiz
      parameter           ( qavsiz = 7 )

!
!     CK Type 1 parameters:
!
!     CK1DTP   CK data type 1 ID;
!
!     CK1RSZ   maximum size of a record passed between CKR01
!              and CKE01.
!
      integer               ck1dtp
      parameter           ( ck1dtp = 1 )

      integer               ck1rsz
      parameter           ( ck1rsz = 8 )

!
!     CK Type 2 parameters:
!
!     CK2DTP   CK data type 2 ID;
!
!     CK2RSZ   maximum size of a record passed between CKR02
!              and CKE02.
!
      integer               ck2dtp
      parameter           ( ck2dtp = 2 )

      integer               ck2rsz
      parameter           ( ck2rsz = 10 )


!
!     CK Type 3 parameters:
!
!     CK3DTP   CK data type 3 ID;
!
!     CK3RSZ   maximum size of a record passed between CKR03
!              and CKE03.
!
      integer               ck3dtp
      parameter           ( ck3dtp = 3 )

      integer               ck3rsz
      parameter           ( ck3rsz = 17 )

!
!     CK Type 4 parameters:
!
!     CK4DTP   CK data type 4 ID;
!
!     CK4PCD   parameter defining integer to DP packing schema that
!              is applied when seven number integer array containing
!              polynomial degrees for quaternion and angular rate
!              components packed into a single DP number stored in
!              actual CK records in a file; the value of must not be
!              changed or compatibility with existing type 4 CK files
!              will be lost.
!
!     CK4MXD   maximum Chebychev polynomial degree allowed in type 4
!              records; the value of this parameter must never exceed
!              value of the CK4PCD;
!
!     CK4SFT   number of additional DPs, which are not polynomial
!              coefficients, located at the beginning of a type 4
!              CK record that passed between routines CKR04 and CKE04;
!
!     CK4RSZ   maximum size of type 4 CK record passed between CKR04
!              and CKE04; CK4RSZ is computed as follows:
!
!                 CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT
!
      integer               ck4dtp
      parameter           ( ck4dtp = 4 )

      double precision      ck4pcd
      parameter           ( ck4pcd = 128.d0 )

      integer               ck4mxd
      parameter           ( ck4mxd = 18 )

      integer               ck4sft
      parameter           ( ck4sft = 10 )

      integer               ck4rsz
      parameter           ( ck4rsz = ( ck4mxd + 1 ) * qavsiz + ck4sft )

!
!     CK Type 5 parameters:
!
!
!     CK5DTP   CK data type 5 ID;
!
!     CK5MXD   maximum polynomial degree allowed in type 5
!              records.
!
!     CK5MET   number of additional DPs, which are not polynomial
!              coefficients, located at the beginning of a type 5
!              CK record that passed between routines CKR05 and CKE05;
!
!     CK5MXP   maximum packet size for any subtype.  Subtype 2
!              has the greatest packet size, since these packets
!              contain a quaternion, its derivative, an angular
!              velocity vector, and its derivative.  See ck05.inc
!              for a description of the subtypes.
!
!     CK5RSZ   maximum size of type 5 CK record passed between CKR05
!              and CKE05; CK5RSZ is computed as follows:
!
!                 CK5RSZ = ( CK5MXD + 1 ) * CK5MXP + CK5MET
!
      integer               ck5dtp
      parameter           ( ck5dtp = 5 )

      integer               ck5mxd
      parameter           ( ck5mxd = 23 )

      integer               ck5met
      parameter           ( ck5met =  4 )

      integer               ck5mxp
      parameter           ( ck5mxp = 14 )

      integer               ck5rsz
      parameter           ( ck5rsz = (ck5mxd + 1) * ck5mxp  +  ck5met )


!
!     CK Type 6 parameters:
!
!
!     CK6DTP   CK data type 6 ID;
!
!     CK6MXD   maximum polynomial degree allowed in type 6
!              records.
!
!     CK6MET   number of additional DPs, which are not polynomial
!              coefficients, located at the beginning of a type 6
!              CK record that passed between routines CKR06 and CKE06;
!
!     CK6MXP   maximum packet size for any subtype.  Subtype 2
!              has the greatest packet size, since these packets
!              contain a quaternion, its derivative, an angular
!              velocity vector, and its derivative.  See ck06.inc
!              for a description of the subtypes.
!
!     CK6RSZ   maximum size of type 6 CK record passed between CKR06
!              and CKE06; CK6RSZ is computed as follows:
!
!                 CK6RSZ = CK6MET + ( CK6MXD + 1 ) * ( CK6PS3 + 1 )
!
!              where CK6PS3 is equal to the parameter CK06PS3 defined
!              in ck06.inc. Note that the subtype having the largest
!              packet size (subtype 2) does not give rise to the
!              largest record size, because that type is Hermite and
!              requires half the window size used by subtype 3 for a
!              given polynomial degree.
!
      integer               ck6dtp
      parameter           ( ck6dtp =  6 )

      integer               ck6mxd
      parameter           ( ck6mxd = 23 )

      integer               ck6met
      parameter           ( ck6met =  4 )

!
!     The parameter CK6PS3 must be in sync with C06PS3 defined in
!     ck06.inc.
!
      integer               ck6ps3
      parameter           ( ck6ps3 = 7 )

      integer               ck6rsz
      parameter           ( ck6rsz =   ( ck6mxd + 1 ) * ( ck6ps3 + 1 ) &
                                     +   ck6met                        )

!
!
!     Maximum record size that can be handled by CKPFS. This value
!     must be set to the maximum of all CKxRSZ parameters (currently
!     CK5RSZ.)
!
      integer               ckmrsz
      parameter           ( ckmrsz = ck5rsz )

