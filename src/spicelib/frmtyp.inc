! $ Abstract
!
!     The parameters below form an enumerated list of the recognized
!     frame types. They are: INERTL, PCK, CK, TK, DYN, SWTCH, and ALL.
!     The meanings are outlined below.
!
! $ Parameters
!
!     INERTL      an inertial frame that is listed in the routine
!                 CHGIRF and that requires no external file to
!                 compute the transformation from or to any other
!                 inertial frame.
!
!     PCK         is a frame that is specified relative to some
!                 INERTL frame and that has an IAU model that
!                 may be retrieved from the PCK system via a call
!                 to the routine TISBOD.
!
!     CK          is a frame defined by a C-kernel.
!
!     TK          is a "text kernel" frame.  These frames are offset
!                 from their associated "relative" frames by a
!                 constant rotation.
!
!     DYN         is a "dynamic" frame.  These currently are
!                 parameterized, built-in frames where the full frame
!                 definition depends on parameters supplied via a
!                 frame kernel.
!
!     SWTCH       is a "switch" frame. These frames have orientation
!                 defined by their alignment with base frames selected
!                 from a prioritized list. The base frames optionally
!                 have associated time intervals of applicability.
!
!     ALL         indicates any of the above classes. This parameter
!                 is used in APIs that fetch information about frames
!                 of a specified class.
!
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 5.0.0, 08-OCT-2020 (NJB) (BVS)
!
!       The parameter SWTCH was added to support the switch
!       frame class.
!
!-    SPICELIB Version 4.0.0, 08-MAY-2012 (NJB)
!
!       The parameter ALL was added to support frame fetch APIs.
!
!-    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB)
!
!       The parameter DYN was added to support the dynamic frame class.
!
!-    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT)
!
!        Various unused frames types were removed and the
!        frame time TK was added.
!
!-    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT)
!
!-&



      INTEGER               INERTL
      PARAMETER           ( INERTL = 1 )

      INTEGER               PCK
      PARAMETER           ( PCK    = INERTL + 1 )

      INTEGER               CK
      PARAMETER           ( CK     = PCK    + 1 )

      INTEGER               TK
      PARAMETER           ( TK     = CK     + 1 )

      INTEGER               DYN
      PARAMETER           ( DYN    = TK     + 1 )

      INTEGER               SWTCH
      PARAMETER           ( SWTCH  = DYN    + 1 )

      INTEGER               ALL
      PARAMETER           ( ALL    = -1 )

!
!     End of INCLUDE file frmtyp.inc
!
