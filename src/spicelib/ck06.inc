! $ Abstract
!
!     Declare parameters specific to CK type 06.
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
!     CK
!
! $ Keywords
!
!     CK
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!     B.V. Semenov      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 10-MAR-2014 (NJB) (BVS)
!
!-&


!
!     Maximum polynomial degree supported by the current
!     implementation of this CK type.
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 23 )

!
!     Integer code indicating `true':
!
      INTEGER               ITRUE
      PARAMETER           ( ITRUE  =  1 )

!
!     Integer code indicating `false':
!
      INTEGER               IFALSE
      PARAMETER           ( IFALSE = -1 )


!
!     CK type 6 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion
!                 and quaternion derivatives only, no angular velocity
!                 vector provided. Quaternion elements are listed
!                 first, followed by derivatives. Angular velocity is
!                 derived from the quaternions and quaternion
!                 derivatives.
!
      INTEGER               C06TP0
      PARAMETER           ( C06TP0 = 0 )


!
!     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion
!                 only. Angular velocity is derived by differentiating
!                 the interpolating polynomials.
!
      INTEGER               C06TP1
      PARAMETER           ( C06TP1 = C06TP0 + 1 )


!
!     Subtype 2:  Hermite interpolation, 14-element packets.
!                 Quaternion and angular angular velocity vector, as
!                 well as derivatives of each, are provided. The
!                 quaternion comes first, then quaternion derivatives,
!                 then angular velocity and its derivatives.
!
      INTEGER               C06TP2
      PARAMETER           ( C06TP2 = C06TP1 + 1 )

!
!     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion
!                 and angular velocity vector provided.  The quaternion
!                 comes first.
!
      INTEGER               C06TP3
      PARAMETER           ( C06TP3 = C06TP2 + 1 )

!
!     Number of subtypes:
!
      INTEGER               C06NST
      PARAMETER           ( C06NST = 4 )

!
!     Packet sizes associated with the various subtypes:
!
      INTEGER               C06PS0
      PARAMETER           ( C06PS0 = 8 )

      INTEGER               C06PS1
      PARAMETER           ( C06PS1 = 4 )

      INTEGER               C06PS2
      PARAMETER           ( C06PS2 = 14 )

      INTEGER               C06PS3
      PARAMETER           ( C06PS3 = 7 )

!
!     Maximum packet size for type 6:
!
      INTEGER               C06MXZ
      PARAMETER           ( C06MXZ = C06PS2 )

!
!     Minimum packet size for type 6:
!
      INTEGER               C06MNZ
      PARAMETER           ( C06MNZ = C06PS1 )


!
!     The CKPFS record size declared in ckparam.inc must be at least as
!     large as the maximum possible size of a CK type 6 record.
!
!     The largest possible CK type 6 record has subtype 3 (note that
!     records of subtype 2 have half as many epochs as those of subtype
!     3, for a given polynomial degree). A subtype 3 record contains
!
!        - The evaluation epoch
!        - The subtype and packet count
!        - MAXDEG+1 packets of size C06PS3
!        - MAXDEG+1 time tags
!

      INTEGER               MAXRSZ
      PARAMETER           ( MAXRSZ =  4 + (MAXDEG+1)*(C06PS3+1) )

!
!     End of file ck06.inc.
!
