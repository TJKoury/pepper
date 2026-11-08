C$Procedure F_GETFOV (Family of tests for GETFOV, GETFVN )

      SUBROUTINE F_GETFOV ( OK )

C$ Abstract
C
C     This routine tests exceptions for GETFOV and GETFVN routines,
C     as well as determining whether or not all data specified
C     in an I-kernel can be retrieved.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the field-of-view routines
C
C        GETFOV
C        GETFVN
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     M. Liukis          (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 17-DEC-2021 (JDR) (BVS) (ML)
C
C        Added missing header sections, fixed typos. Added test cases
C        for GETFVN routine. Extended test cases for GETFOV to complete
C        code coverage. Added a test case for the zero boresight 
C        exception.
C
C-    TSPICE Version 1.0.0, 13-APR-2000 (WLT)
C
C-&


C$ Index_Entries
C
C     Test family for the field-of-view routines
C
C-&


C
C     SPICELIB Functions
C
      DOUBLE PRECISION      HALFPI

C
C     Local parameters
C
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

C
C     Local Variables
C
      CHARACTER*(WDSIZE)    BOREST( 3 )
      CHARACTER*(WDSIZE)    FOVFRM
      CHARACTER*(WDSIZE)    FOVSHP
      CHARACTER*(WDSIZE)    FRAME
      CHARACTER*(WDSIZE)    INSTNM
      CHARACTER*(WDSIZE)    SHAPE
      CHARACTER*(WDSIZE)    SPEC
      CHARACTER*(WDSIZE)    UNITS

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      BSIGHT ( 3    )
      DOUBLE PRECISION      BOUNDS ( 3, 5 )
      DOUBLE PRECISION      CRSANG
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

C
C     Begin every test family with an open call.
C
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

C
C     *****************************************************************
C
C     Error cases: GETFVN
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the name of the instrument can not be translated '
     .//           'to its NAIF ID code -- check GETFVN.' )

      CALL GETFVN ( INSTNM, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the frame of the instrument has not been '
     .//           'stored in the kernel pool -- check GETFVN.' )

      BUFFER( 1 ) = 'NAIF_BODY_CODE += -1000'
      BUFFER( 2 ) = 'NAIF_BODY_NAME += ''CINSTR'''
      CALL LMPOOL ( BUFFER, BUFLEN )

      CALL GETFVN ( INSTNM, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEMISSING)',  OK )


C
C     *****************************************************************
C
C     Error cases: GETFOV
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the frame of the instrument has not been '
     .//           'stored in the kernel pool -- check GETFOV' )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEMISSING)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the shape of the instrument field of view is '
     .//           'not in the kernel pool.' )


      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( 'INS-1001_FOV_SHAPE', 1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPEMISSING)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the shape specified is not one of the known '
     .//           'shapes.' )

      SHAPE = 'SQUARE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPENOTSUPPORTED)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the boresight information has not been '
     .//           'stored in the kernel pool.' )

      SHAPE = 'POLYGON'
      KWBORE = 'INS-1001_BORESIGHT'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BORESIGHTMISSING)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the boresight is not a 3-vector.' )

      KWBORE = 'INS-1000_BORESIGHT'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               2, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBORESIGHTSPEC)',  OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the boresight is not a numeric 3-vector.' )

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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the boresight is the zero vector.' )

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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the FOV class specification is not supported.' )

      SPEC = 'UNKNOWN_SPEC'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDSPEC)',       OK     )

C
C     The following test cases cover the 'CORNERS' specification of
C     FOV boundaries. Note that, the FOV class specification keyword
C     is not present, GETFOV defaults to 'CORNERS', therefore we will
C     not provide that keyword.
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the boundary vectors of the field of view '
     .//           'have not been stored in the kernel pool ' )


      KWBOUN = 'INS-1001_FOV_BOUNDARY'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYMISSING)',  OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'there are too many vectors in the array of '
     .//           'corner vectors.' )

      KWBOUN = 'INS-1000_FOV_BOUNDARY'
      ROOM   =  2
      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              12, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the size of the array of boundary numbers is '
     .//           'not a multiple of 3.' )


      ROOM   =  4
      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,              11, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'number of boundary vectors for a circular '
     .//           'field of view is not 1.' )

      SHAPE = 'CIRCLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'number of boundary vectors for a elliptical '
     .//           'field of view is not 2.' )

      SHAPE = 'ELLIPSE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'number of boundary vectors for a rectangular '
     .//           'field of view is not 4.' )

      SHAPE = 'RECTANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               9, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'number of boundary vectors for a polygonal '
     .//           'field of view is not at least 3.' )

      SHAPE = 'POLYGON'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWBOUN,               6, BOUNDS )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',  OK )


C
C     The following test cases cover the 'ANGLES' specification of
C     FOV boundaries.
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if '
     .//           'the shape specified is not one of the supported '
     .//           'shapes for the ANGLES specification.' )

      SPEC  = 'ANGLES'
      SHAPE = 'POLYGON'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPENOTSUPPORTED)',         OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference vector information has not been '
     .//           'stored in the kernel pool.' )

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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference vector is not a 3-vector.' )

      KWRVEC = 'INS-1000_FOV_REF_VECTOR'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               2, REFVEC )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)',          OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference vector is not a numeric 3-vector.' )
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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference vector is parallel to the boresight '
     .//           'vector.' )

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, BSIGHT )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)',          OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference angle has not been stored in the kernel '
     .//           'pool.' )

      KWRANG = 'INS-1001_FOV_REF_ANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(REFANGLEMISSING)',           OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference angle units have not been stored in the '
     .//           'kernel pool.' )

      KWRANG = 'INS-1000_FOV_REF_ANGLE'
      KWAUNT = 'INS-1001_FOV_ANGLE_UNITS'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(UNITSMISSING)',              OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference angle units are not recognized by '
     .//           'CONVRT.' )

      KWAUNT = 'INS-1000_FOV_ANGLE_UNITS'
      UNITS  = 'UNKNOWN'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(UNITSNOTREC)',               OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'reference angle units are not angular units.' )

      UNITS  = 'KM'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)',         OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'available room for the boundary corner vectors '
     .//           'is not enough for ANGLES/CIRCLE specification.'  )

      SHAPE  = 'CIRCLE'
      UNITS  = 'DEGREES'
      ROOM   = 0

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',            OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'cross reference angle has not been stored in the '
     .//           'kernel pool for ANGLES/ELLIPSE specification.' )

      SHAPE  = 'ELLIPSE'
      KWCANG = 'INS-1001_FOV_CROSS_ANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(CROSSANGLEMISSING)',         OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'available room for the boundary corner vectors '
     .//           'is not enough for ANGLESS/ELLIPSE specification.'  )

      KWCANG = 'INS-1000_FOV_CROSS_ANGLE'
      ROOM   = 1

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',            OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the FOV '
     .//           'cross reference angle has not been stored in the '
     .//           'kernel pool for ANGLE/RECTANGLE specification.' )

      SHAPE  = 'RECTANGLE'
      KWCANG = 'INS-1001_FOV_CROSS_ANGLE'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(CROSSANGLEMISSING)',         OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'available room for the boundary corner vectors '
     .//           'is not enough for ANGLES/RECTANGLE specification.' )

      KWCANG = 'INS-1000_FOV_CROSS_ANGLE'
      ROOM   = 3

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)',            OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'FOV definitions for ANGLES/RECTANGLE specification '
     .//           'result in a degenerate case due to reference '
     .//           'angle being ~90.0 degrees.' )

      ROOM   = 4
      ANGLE  = ACOS( 0.9D-15 )
      UNITS  = 'RADIANS'

      CALL CLPOOL
      CALL PCPOOL ( KWFRAM,               1, FRAME  )
      CALL PCPOOL ( KWSPEC,               1, SPEC   )
      CALL PCPOOL ( KWSHAP,               1, SHAPE  )
      CALL PDPOOL ( KWBORE,               3, BSIGHT )
      CALL PDPOOL ( KWRVEC,               3, REFVEC )
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',               OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that an exception is signaled if the '
     .//           'FOV definitions for ANGLES/RECTANGLE specification '
     .//           'result in a degenerate case due to cross '
     .//           'angle being ~90.0 degrees.' )

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
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)',               OK )


C
C     *****************************************************************
C
C     Normal cases: GETFVN
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a polygonal field '
     .//           'of view -- check GETFVN.' )

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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a elliptical field '
     .//           'of view -- check GETFVN.' )

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


C
C     *****************************************************************
C
C     Normal cases: GETFOV
C
C     *****************************************************************
C
C     The following test cases cover the 'CORNERS' specification of
C     FOV boundaries. Note that, the FOV class specification keyword
C     is not present, GETFOV defaults to 'CORNERS', therefore we will
C     not provide that keyword.
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a rectangular '
     .//           'field of view.' )

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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a circular field '
     .//           'of view.' )

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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a elliptical field '
     .//           'of view.' )

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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a polygonal field '
     .//           'of view.' )

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

C
C     The following test cases cover the 'ANGLES' specification of
C     FOV boundaries.
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a circular field '
     .//           'of view from a CIRCLE/ANGLES specification.' )

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
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 3, 0.0D0, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a ellipsoidal field '
     .//           'of view from a ELLIPSE/ANGLES specification.' )

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
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3, 0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '=', BOUNDS, 6, 0.0D0, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a rectangular field '
     .//           'of view from a RECTANGLE/ANGLES specification.' )

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
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0, OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a rectangular field '
     .//           'of view from a RECTANGLE/ANGLES specification '
     .//           'with reference angle with cosines close to '
     .//           'MINCOS.' )

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
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0,  OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a rectangular field '
     .//           'of view from a RECTANGLE/ANGLES specification '
     .//           'with cross angle with cosines close to '
     .//           'MINCOS.' )

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
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0,  OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that we can get back a rectangular field '
     .//           'of view from a RECTANGLE/ANGLES specification '
     .//           'with both reference and cross angle with cosines '
     .//           'close to  MINCOS.' )

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
      CALL PDPOOL ( KWRANG,               1, ANGLE  )
      CALL PCPOOL ( KWAUNT,               1, UNITS  )
      CALL PDPOOL ( KWCANG,               1, CRSANG )

      CALL GETFOV ( INSTID, ROOM, FOVSHP, FOVFRM, FOVBS, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'SHAPE', FOVSHP, '=', SHAPE, OK )
      CALL CHCKSC ( 'FRAME', FOVFRM, '=', FRAME, OK )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
      CALL CHCKAD ( 'FOVBS',  FOVBS,  '=', BSIGHT, 3,  0.0D0,  OK )
      CALL CHCKAD ( 'FOVBND', FOVBND, '~', BOUNDS, 12, VTIGHT, OK )


C
C     Leave the kernel pool clean.
C
      CALL CLPOOL

C
C     Retrieve the current test status.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
