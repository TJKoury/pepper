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



      integer               inertl
      parameter           ( inertl = 1 )

      integer               pck
      parameter           ( pck    = inertl + 1 )

      integer               ck
      parameter           ( ck     = pck    + 1 )

      integer               tk
      parameter           ( tk     = ck     + 1 )

      integer               dyn
      parameter           ( dyn    = tk     + 1 )

      integer               swtch
      parameter           ( swtch  = dyn    + 1 )

      integer               all
      parameter           ( all    = -1 )

!
!     End of INCLUDE file frmtyp.inc
!
