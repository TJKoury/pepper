! $ Abstract
!
!     Declarations of the CK data type specific and general CK low
!     level routine parameters.
!
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
      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )

      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )

!
!     CK Type 1 parameters:
!
!     CK1DTP   CK data type 1 ID;
!
!     CK1RSZ   maximum size of a record passed between CKR01
!              and CKE01.
!
      INTEGER               CK1DTP
      PARAMETER           ( CK1DTP = 1 )

      INTEGER               CK1RSZ
      PARAMETER           ( CK1RSZ = 8 )

!
!     CK Type 2 parameters:
!
!     CK2DTP   CK data type 2 ID;
!
!     CK2RSZ   maximum size of a record passed between CKR02
!              and CKE02.
!
      INTEGER               CK2DTP
      PARAMETER           ( CK2DTP = 2 )

      INTEGER               CK2RSZ
      PARAMETER           ( CK2RSZ = 10 )


!
!     CK Type 3 parameters:
!
!     CK3DTP   CK data type 3 ID;
!
!     CK3RSZ   maximum size of a record passed between CKR03
!              and CKE03.
!
      INTEGER               CK3DTP
      PARAMETER           ( CK3DTP = 3 )

      INTEGER               CK3RSZ
      PARAMETER           ( CK3RSZ = 17 )

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
      INTEGER               CK4DTP
      PARAMETER           ( CK4DTP = 4 )

      DOUBLE PRECISION      CK4PCD
      PARAMETER           ( CK4PCD = 128.D0 )

      INTEGER               CK4MXD
      PARAMETER           ( CK4MXD = 18 )

      INTEGER               CK4SFT
      PARAMETER           ( CK4SFT = 10 )

      INTEGER               CK4RSZ
      PARAMETER           ( CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT )

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
      INTEGER               CK5DTP
      PARAMETER           ( CK5DTP = 5 )

      INTEGER               CK5MXD
      PARAMETER           ( CK5MXD = 23 )

      INTEGER               CK5MET
      PARAMETER           ( CK5MET =  4 )

      INTEGER               CK5MXP
      PARAMETER           ( CK5MXP = 14 )

      INTEGER               CK5RSZ
      PARAMETER           ( CK5RSZ = (CK5MXD + 1) * CK5MXP  +  CK5MET )


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
      INTEGER               CK6DTP
      PARAMETER           ( CK6DTP =  6 )

      INTEGER               CK6MXD
      PARAMETER           ( CK6MXD = 23 )

      INTEGER               CK6MET
      PARAMETER           ( CK6MET =  4 )

!
!     The parameter CK6PS3 must be in sync with C06PS3 defined in
!     ck06.inc.
!
      INTEGER               CK6PS3
      PARAMETER           ( CK6PS3 = 7 )

      INTEGER               CK6RSZ
      PARAMETER           ( CK6RSZ =   ( CK6MXD + 1 ) * ( CK6PS3 + 1 ) &
                                     +   CK6MET                        )

!
!
!     Maximum record size that can be handled by CKPFS. This value
!     must be set to the maximum of all CKxRSZ parameters (currently
!     CK5RSZ.)
!
      INTEGER               CKMRSZ
      PARAMETER           ( CKMRSZ = CK5RSZ )

