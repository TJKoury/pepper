!$Procedure F_GETFOV (Family of tests for GETFOV, GETFVN )

      SUBROUTINE F_GETFOV ( OK )
      use pepper_legacy, pepper_legacy_frame => frame

! $ Abstract
!
!     This routine tests exceptions for GETFOV and GETFVN routines,
!     as well as determining whether or not all data specified
!     in an I-kernel can be retrieved.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the field-of-view routines
!
!        GETFOV
!        GETFVN
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     M. Liukis          (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 17-DEC-2021 (JDR) (BVS) (ML)
!
!        Added missing header sections, fixed typos. Added test cases
!        for GETFVN routine. Extended test cases for GETFOV to complete
!        code coverage. Added a test case for the zero boresight
!        exception.
!
!-    TSPICE Version 1.0.0, 13-APR-2000 (WLT)
!
!-&


! $ Index_Entries
!
!     Test family for the field-of-view routines
!
!-&


!
!     SPICELIB Functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      MINCOS
      PARAMETER           ( MINCOS = 1.0D-15 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = MINCOS  )

      INTEGER               BUFLEN
      PARAMETER           ( BUFLEN = 2  )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

!
!     Local Variables
!
      CHARACTER*(WDSIZE)    BOREST( 3 )
      CHARACTER*(WDSIZE)    FOVFRM
      CHARACTER*(WDSIZE)    FOVSHP
      CHARACTER*(WDSIZE)    FRAME
      CHARACTER*(WDSIZE)    INSTNM
      CHARACTER*(WDSIZE)    SHAPE
      CHARACTER*(WDSIZE)    SPEC
      CHARACTER*(WDSIZE)    UNITS

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION ANGLE_A1 ( 1 )
      EQUIVALENCE ( ANGLE, ANGLE_A1 )
      DOUBLE PRECISION      BSIGHT ( 3    )
      DOUBLE PRECISION      BOUNDS ( 3, 5 )
      DOUBLE PRECISION      CRSANG
      DOUBLE PRECISION CRSANG_A1 ( 1 )
      EQUIVALENCE ( CRSANG, CRSANG_A1 )
      DOUBLE PRECISION      FOVBS  ( 3    )
      DOUBLE PRECISION      FOVBND ( 3, 5 )
      DOUBLE PRECISION      REFVEC ( 3    )
      DOUBLE PRECISION      TMPVEC ( 3    )

      INTEGER               INSTID
      INTEGER               N
      INTEGER               ROOM

      CHARACTER*(WDSIZE)    KWBORE
      CHARACTER*(WDSIZE)    KWSHAP
      CHARACTER*(WDSIZE)    KWFRAM
      CHARACTER*(WDSIZE)    KWBOUN
      CHARACTER*(WDSIZE)    KWSPEC
      CHARACTER*(WDSIZE)    KWRVEC
      CHARACTER*(WDSIZE)    KWRANG
      CHARACTER*(WDSIZE)    KWAUNT
      CHARACTER*(WDSIZE)    KWCANG

      CHARACTER*(LINLEN)    BUFFER ( BUFLEN )

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GETFOV' )

      KWBOUN = 'INS-1000_FOV_BOUNDARY'
      KWBORE = 'INS-1000_BORESIGHT'
      KWSHAP = 'INS-1000_FOV_SHAPE'
      KWFRAM = 'INS-1000_FOV_FRAME'
      KWSPEC = 'INS-1000_FOV_CLASS_SPEC'
      KWRVEC = 'INS-1000_FOV_REF_VECTOR'
      KWRANG = 'INS-1000_FOV_REF_ANGLE'
      KWAUNT = 'INS-1000_FOV_ANGLE_UNITS'
      KWCANG = 'INS-1000_FOV_CROSS_ANGLE'

      ROOM   = 3
      INSTID = -1000

      SHAPE  = 'POLYGON'
      FRAME  = 'CKERNEL'
      INSTNM = 'CINSTR'
      UNITS  = 'DEGREES'

      BOUNDS(1,1) =  1.0D0
      BOUNDS(2,1) =  1.0D0
      BOUNDS(3,1) =  1.0D0

      BOUNDS(1,2) = -1.0D0
      BOUNDS(2,2) =  1.0D0
      BOUNDS(3,2) =  1.0D0

      BOUNDS(1,3) = -1.0D0
      BOUNDS(2,3) = -1.0D0
      BOUNDS(3,3) =  1.0D0

      BOUNDS(1,4) =  1.0D0
      BOUNDS(2,4) = -1.0D0
      BOUNDS(3,4) =  1.0D0

      BOUNDS(1,5) = 0.0D0
      BOUNDS(2,5) = 0.0D0
      BOUNDS(3,5) = 0.0D0

      BSIGHT(1)   = 0.0D0
      BSIGHT(2)   = 0.0D0
      BSIGHT(3)   = 1.0D0

      REFVEC(1)   = 0.0D0
      REFVEC(2)   = 1.0D0
      REFVEC(3)   = 0.0D0

      ANGLE       = 60.0D0
      CRSANG      = 60.0D0

      CALL CLPOOL
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )
      CALL PCPOOL ( 'INS-1001_FOV_FRAME', 1, FRAME  )

!
!     *****************************************************************
!
!     Error cases: GETFVN
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the name of the instrument can not be translated ' &
      //           'to its NAIF ID code -- check GETFVN.' )

      CALL GETFVN ( INSTNM, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the frame of the instrument has not been ' &
      //           'stored in the kernel pool -- check GETFVN.' )

      BUFFER( 1 ) = 'NAIF_BODY_CODE += -1000'
      BUFFER( 2 ) = 'NAIF_BODY_NAME += ''CINSTR'''
      CALL LMPOOL ( BUFFER, BUFLEN )

      CALL GETFVN ( INSTNM, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEMISSING)',  OK )


!
!     *****************************************************************
!
!     Error cases: GETFOV
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the frame of the instrument has not been ' &
      //           'stored in the kernel pool -- check GETFOV' )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEMISSING)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the shape of the instrument field of view is ' &
      //           'not in the kernel pool.' )


      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( 'INS-1001_FOV_SHAPE', 1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPEMISSING)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the shape specified is not one of the known ' &
      //           'shapes.' )

      SHAPE = 'SQUARE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPENOTSUPPORTED)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the boresight information has not been ' &
      //           'stored in the kernel pool.' )

      SHAPE = 'POLYGON'
      KWBORE = 'INS-1001_BORESIGHT'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BORESIGHTMISSING)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the boresight is not a 3-vector.' )

      KWBORE = 'INS-1000_BORESIGHT'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               2, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBORESIGHTSPEC)',  OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the boresight is not a numeric 3-vector.' )

      KWBORE = 'INS-1000_BORESIGHT'
      BOREST(1) = 'A'
      BOREST(2) = 'B'
      BOREST(3) = 'C'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PCPOOL ( KWBORE,               3, BOREST )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBORESIGHTSPEC)',  OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the boresight is the zero vector.' )

      KWBORE = 'INS-1000_BORESIGHT'

      TMPVEC(1) = 0.D0
      TMPVEC(2) = 0.D0
      TMPVEC(3) = 0.D0

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, TMPVEC )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, 4, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROBORESIGHT)',  OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the FOV class specification is not supported.' )

      SPEC = 'UNKNOWN_SPEC'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDSPEC)',       OK     )

!
!     The following test cases cover the 'CORNERS' specification of
!     FOV boundaries. Note that, the FOV class specification keyword
!     is not present, GETFOV defaults to 'CORNERS', therefore we will
!     not provide that keyword.
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the boundary vectors of the field of view ' &
      //           'have not been stored in the kernel pool ' )


      KWBOUN = 'INS-1001_FOV_BOUNDARY'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYMISSING)',  OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'there are too many vectors in the array of ' &
      //           'corner vectors.' )

      KWBOUN = 'INS-1000_FOV_BOUNDARY'
      ROOM   =  2
      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the size of the array of boundary numbers is ' &
      //           'not a multiple of 3.' )


      ROOM   =  4
      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              11, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'number of boundary vectors for a circular ' &
      //           'field of view is not 1.' )

      SHAPE = 'CIRCLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'number of boundary vectors for a elliptical ' &
      //           'field of view is not 2.' )

      SHAPE = 'ELLIPSE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'number of boundary vectors for a rectangular ' &
      //           'field of view is not 4.' )

      SHAPE = 'RECTANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'number of boundary vectors for a polygonal ' &
      //           'field of view is not at least 3.' )

      SHAPE = 'POLYGON'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               6, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


!
!     The following test cases cover the 'ANGLES' specification of
!     FOV boundaries.
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if ' &
      //           'the shape specified is not one of the supported ' &
      //           'shapes for the ANGLES specification.' )

      SPEC  = 'ANGLES'
      SHAPE = 'POLYGON'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPENOTSUPPORTED)',         OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference vector information has not been ' &
      //           'stored in the kernel pool.' )

      SHAPE  = 'RECTANGLE'
      KWRVEC = 'INS-1001_FOV_REF_VECTOR'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(REFVECTORMISSING)',          OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference vector is not a 3-vector.' )

      KWRVEC = 'INS-1000_FOV_REF_VECTOR'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               2, REFVEC )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)',          OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference vector is not a numeric 3-vector.' )
      BOREST(1) = 'A'
      BOREST(2) = 'B'
      BOREST(3) = 'C'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PCPOOL ( KWRVEC,               3, BOREST )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)',          OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference vector is parallel to the boresight ' &
      //           'vector.' )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, BSIGHT )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)',          OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference angle has not been stored in the kernel ' &
      //           'pool.' )

      KWRANG = 'INS-1001_FOV_REF_ANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(REFANGLEMISSING)',           OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference angle units have not been stored in the ' &
      //           'kernel pool.' )

      KWRANG = 'INS-1000_FOV_REF_ANGLE'
      KWAUNT = 'INS-1001_FOV_ANGLE_UNITS'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(UNITSMISSING)',              OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference angle units are not recognized by ' &
      //           'CONVRT.' )

      KWAUNT = 'INS-1000_FOV_ANGLE_UNITS'
      UNITS  = 'UNKNOWN'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(UNITSNOTREC)',               OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'reference angle units are not angular units.' )

      UNITS  = 'KM'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)',         OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'available room for the boundary corner vectors ' &
      //           'is not enough for ANGLES/CIRCLE specification.'  )

      SHAPE  = 'CIRCLE'
      UNITS  = 'DEGREES'
      ROOM   = 0

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',            OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'cross reference angle has not been stored in the ' &
      //           'kernel pool for ANGLES/ELLIPSE specification.' )

      SHAPE  = 'ELLIPSE'
      KWCANG = 'INS-1001_FOV_CROSS_ANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(CROSSANGLEMISSING)',         OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'available room for the boundary corner vectors ' &
      //           'is not enough for ANGLESS/ELLIPSE specification.'  )

      KWCANG = 'INS-1000_FOV_CROSS_ANGLE'
      ROOM   = 1

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',            OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the FOV ' &
      //           'cross reference angle has not been stored in the ' &
      //           'kernel pool for ANGLE/RECTANGLE specification.' )

      SHAPE  = 'RECTANGLE'
      KWCANG = 'INS-1001_FOV_CROSS_ANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(CROSSANGLEMISSING)',         OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'available room for the boundary corner vectors ' &
      //           'is not enough for ANGLES/RECTANGLE specification.' )

      KWCANG = 'INS-1000_FOV_CROSS_ANGLE'
      ROOM   = 3

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',            OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'FOV definitions for ANGLES/RECTANGLE specification ' &
      //           'result in a degenerate case due to reference ' &
      //           'angle being ~90.0 degrees.' )

      ROOM   = 4
      ANGLE  = ACOS( 0.9D-15 )
      UNITS  = 'RADIANS'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',               OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that an exception is signaled if the ' &
      //           'FOV definitions for ANGLES/RECTANGLE specification ' &
      //           'result in a degenerate case due to cross ' &
      //           'angle being ~90.0 degrees.' )

      ROOM   = 4
      ANGLE  = 1.D0
      CRSANG = ACOS( 0.9D-15 )
      UNITS  = 'RADIANS'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',               OK )


!
!     *****************************************************************
!
!     Normal cases: GETFVN
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a polygonal field ' &
      //           'of view -- check GETFVN.' )

      SHAPE = 'POLYGON'
      ROOM  = 15

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      BUFFER( 1 ) = 'NAIF_BODY_CODE += -1000'
      BUFFER( 2 ) = 'NAIF_BODY_NAME += ''CINSTR'''
      CALL LMPOOL ( BUFFER, BUFLEN )

      CALL GETFVN ( INSTNM, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 9, 0.0D0, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a elliptical field ' &
      //           'of view -- check GETFVN.' )

      SHAPE = 'ELLIPSE'
      ROOM  = 2

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               6, BOUNDS )

      BUFFER( 1 ) = 'NAIF_BODY_CODE += -1000'
      BUFFER( 2 ) = 'NAIF_BODY_NAME += ''CINSTR'''
      CALL LMPOOL ( BUFFER, BUFLEN )

      CALL GETFVN ( INSTNM, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 6, 0.0D0, OK )


!
!     *****************************************************************
!
!     Normal cases: GETFOV
!
!     *****************************************************************
!
!     The following test cases cover the 'CORNERS' specification of
!     FOV boundaries. Note that, the FOV class specification keyword
!     is not present, GETFOV defaults to 'CORNERS', therefore we will
!     not provide that keyword.
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a rectangular ' &
      //           'field of view.' )

      SHAPE = 'RECTANGLE'
      ROOM  = 4

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 12, 0.0D0, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a circular field ' &
      //           'of view.' )

      SHAPE = 'CIRCLE'
      ROOM  = 1

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               3, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 3, 0.0D0, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a elliptical field ' &
      //           'of view.' )

      SHAPE = 'ELLIPSE'
      ROOM  = 2

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               6, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 6, 0.0D0, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a polygonal field ' &
      //           'of view.' )

      SHAPE = 'POLYGON'
      ROOM  = 15

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 9, 0.0D0, OK )

!
!     The following test cases cover the 'ANGLES' specification of
!     FOV boundaries.
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a circular field ' &
      //           'of view from a CIRCLE/ANGLES specification.' )

      SHAPE = 'CIRCLE'
      ROOM  = 1
      UNITS = 'RADIANS'
      ANGLE = 1.D0

      BOUNDS(1,1) =  0.0D0
      BOUNDS(2,1) =  DSIN( ANGLE )
      BOUNDS(3,1) =  DCOS( ANGLE )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 3, 0.0D0, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a ellipsoidal field ' &
      //           'of view from a ELLIPSE/ANGLES specification.' )

      SHAPE  = 'ELLIPSE'
      ROOM   = 2
      UNITS  = 'RADIANS'
      ANGLE  = 1.D0
      CRSANG = 0.5D0

      BOUNDS(1,1) =    0.0D0
      BOUNDS(2,1) =    DSIN( ANGLE  )
      BOUNDS(3,1) =    DCOS( ANGLE  )

      BOUNDS(1,2) =  - DSIN( CRSANG )
      BOUNDS(2,2) =    0.0D0
      BOUNDS(3,2) =    DCOS( CRSANG )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 6, 0.0D0, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a rectangular field ' &
      //           'of view from a RECTANGLE/ANGLES specification.' )

      SHAPE  = 'RECTANGLE'
      ROOM   = 4
      UNITS  = 'RADIANS'
      ANGLE  = 0.1D0
      CRSANG = 0.05D0

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,1) )

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,2) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,3) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,4) )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a rectangular field ' &
      //           'of view from a RECTANGLE/ANGLES specification ' &
      //           'with reference angle with cosines close to ' &
      //           'MINCOS.' )

      SHAPE  = 'RECTANGLE'
      ROOM   = 4
      UNITS  = 'RADIANS'
      ANGLE  = HALFPI() - MINCOS
      CRSANG = 0.1D0

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,1) )

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,2) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,3) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,4) )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0,  OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a rectangular field ' &
      //           'of view from a RECTANGLE/ANGLES specification ' &
      //           'with cross angle with cosines close to ' &
      //           'MINCOS.' )

      SHAPE  = 'RECTANGLE'
      ROOM   = 4
      UNITS  = 'RADIANS'
      ANGLE  = 0.2D0
      CRSANG = HALFPI() - MINCOS

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,1) )

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,2) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,3) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,4) )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0,  OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check that we can get back a rectangular field ' &
      //           'of view from a RECTANGLE/ANGLES specification ' &
      //           'with both reference and cross angle with cosines ' &
      //           'close to  MINCOS.' )

      SHAPE  = 'RECTANGLE'
      ROOM   = 4
      UNITS  = 'RADIANS'
      ANGLE  = HALFPI() - MINCOS
      CRSANG = HALFPI() - MINCOS

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,1) )

      TMPVEC(1) = - DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,2) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) = - DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,3) )

      TMPVEC(1) =   DCOS( ANGLE ) * DSIN( CRSANG )
      TMPVEC(2) =   DSIN( ANGLE ) * DCOS( CRSANG )
      TMPVEC(3) =   DCOS( ANGLE ) * DCOS( CRSANG )

      CALL VHAT ( TMPVEC, BOUNDS(1,4) )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE_A1  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG_A1 )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0,  OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )


!
!     Leave the kernel pool clean.
!
      CALL CLPOOL

!
!     Retrieve the current test status.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
