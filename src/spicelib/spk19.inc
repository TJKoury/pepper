! $ Abstract
!
!     Declare parameters specific to SPK type 19.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     SPK
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
!-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
!
!        Updated to support subtype 2.
!
!-    SPICELIB Version 1.0.0, 07-MAR-2014 (NJB) (BVS)
!
!-&


!
!     Maximum polynomial degree supported by the current
!     implementation of this SPK type.
!
!     The degree is compatible with the maximum degrees
!     supported by types 13 and 21.
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 27 )

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
!     SPK type 19 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 12-element packets.
!
      INTEGER               S19TP0
      PARAMETER           ( S19TP0 = 0 )

!
!     Subtype 1:  Lagrange interpolation, 6-element packets.
!
      INTEGER               S19TP1
      PARAMETER           ( S19TP1 = S19TP0 + 1 )

!
!     Subtype 2:  Hermite interpolation, 6-element packets.
!
      INTEGER               S19TP2
      PARAMETER           ( S19TP2 = S19TP1 + 1 )


!
!     Packet sizes associated with the various subtypes:
!
      INTEGER               S19PS0
      PARAMETER           ( S19PS0 = 12 )

      INTEGER               S19PS1
      PARAMETER           ( S19PS1 =  6 )

      INTEGER               S19PS2
      PARAMETER           ( S19PS2 =  6 )

!
!     Number of subtypes:
!
      INTEGER               S19NST
      PARAMETER           ( S19NST = 3 )

!
!     Maximum packet size for type 19:
!
      INTEGER               S19MXZ
      PARAMETER           ( S19MXZ = S19PS0 )

!
!     Minimum packet size for type 19:
!
      INTEGER               S19MNZ
      PARAMETER           ( S19MNZ = S19PS1 )

!
!     The SPKPVN record size declared in spkrec.inc must be at least as
!     large as the maximum possible size of an SPK type 19 record.
!
!     The largest possible SPK type 19 record has subtype 1 (note that
!     records of subtype 0 have half as many epochs as those of subtype
!     1, for a given polynomial degree). A type 1 record contains
!
!        - The subtype and packet count
!        - MAXDEG+1 packets of size S19PS1
!        - MAXDEG+1 time tags
!
      INTEGER               MAXRSZ
      PARAMETER           ( MAXRSZ =  2 + (MAXDEG+1)*(S19PS1+1) )

!
!     End of include file spk19.inc.
!
