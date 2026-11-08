!$Procedure PXFORM ( Position Transformation Matrix )

      SUBROUTINE PXFORM ( FROM, TO, ET, ROTATE )

! $ Abstract
!
!     Return the matrix that transforms position vectors from one
!     specified frame to another at a specified epoch.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         FROM
      CHARACTER*(*)         TO
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROTATE  ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FROM       I   Name of the frame to transform from.
!     TO         I   Name of the frame to transform to.
!     ET         I   Epoch of the rotation matrix.
!     ROTATE     O   A rotation matrix.
!
! $ Detailed_Input
!
!     FROM     is the name of a reference frame in which a position
!              vector is known.
!
!     TO       is the name of a reference frame in which it is desired
!              to represent a position vector.
!
!     ET       is the epoch in ephemeris seconds past the epoch of J2000
!              (TDB) at which the position transformation matrix ROTATE
!              should be evaluated.
!
! $ Detailed_Output
!
!     ROTATE   is the matrix that transforms position vectors from
!              the reference frame FROM to the frame TO at epoch ET.
!              If (x, y, z) is a position relative to the frame FROM
!              then the vector ( x', y', z') is the same position
!              relative to the frame TO at epoch ET. Here the
!              vector ( x', y', z' ) is defined by the equation:
!
!                 .-   -.     .-        -.   .-  -.
!                 | x'  |     |          |   | x  |
!                 | y'  |  =  |  ROTATE  |   | y  |
!                 | z'  |     |          |   | z  |
!                 `-   -'     `-        -'   `-  -'
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If sufficient information has not been supplied via loaded
!         SPICE kernels to compute the transformation between the
!         two frames, an error is signaled by a routine
!         in the call tree of this routine.
!
!     2)  If either frame FROM or TO is not recognized, the error
!         SPICE(UNKNOWNFRAME) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides the user level interface to computing
!     position transformations from one reference frame to another.
!
!     Note that the reference frames may be inertial or non-inertial.
!     However, the user must take care that sufficient SPICE kernel
!     information is loaded to provide a complete position
!     transformation path from the FROM frame to the TO frame.
!
!     A common type of reference frame transformation is one from one
!     time-dependent frame to another, where the orientations of the
!     frames are computed at different epochs. For example, a remote
!     sensing application may compute the transformation from a target
!     body-fixed frame, with its orientation evaluated at the epoch of
!     photon emission, to a spacecraft instrument frame, with its
!     orientation evaluated at the epoch of photon reception. The
!     SPICELIB routine PXFRM2 computes this type of frame
!     transformation.
!
! $ Examples
!
!     Suppose that you have geodetic coordinates of a station on the
!     surface of the earth and that you need the inertial (J2000)
!     position of this station. The following code fragment
!     illustrates how to transform the position of the station to a
!     J2000 position.
!
!        CALL BODVRD ( 'EARTH', RADII, 3, N, ABC  )
!
!        EQUATR   =  ABC(1)
!        POLAR    =  ABC(3)
!        F        = (EQUATR - POLAR) / EQUATR
!
!        CALL GEOREC ( LONG, LAT, 0.0D0,  EQUATR, F, EPOS )
!
!        CALL PXFORM ( 'IAU_EARTH', 'J2000', ET,  ROTATE  )
!        CALL MXV    (  ROTATE,      EPOS,   JPOS         )
!
!     The position JPOS is the desired J2000 position of the station.
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
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 09-AUG-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard. Updated the
!        $Particulars header section to mention PXFRM2.
!
!-    SPICELIB Version 1.1.0, 23-SEP-2013 (BVS)
!
!        Updated to save the input frame names and POOL state counters
!        and to do frame name-ID conversions only if the counters have
!        changed.
!
!-    SPICELIB Version 1.0.3, 27-FEB-2008 (BVS)
!
!        Added FRAMES to the $Required_Reading section.
!
!-    SPICELIB Version 1.0.2, 23-OCT-2005 (NJB)
!
!        Header example had invalid flattening factor computation;
!        this was corrected. Reference to BODVAR in header was
!        replaced with reference to BODVRD.
!
!-    SPICELIB Version 1.0.1, 29-JUL-2003 (NJB) (CHA)
!
!        Various header corrections were made.
!
!-    SPICELIB Version 1.0.0, 05-APR-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Find a position transformation matrix
!
!-&


!
!     Spicelib Functions
!
      LOGICAL               RETURN

!
!     Local parameters.
!

!
!     Saved frame name length.
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local Variables.
!
      INTEGER               FCODE
      INTEGER               TCODE

!
!     Saved frame name/ID item declarations.
!
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFROM
      INTEGER               SVFCOD

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVTO
      INTEGER               SVTCDE

      LOGICAL               FIRST

!
!     Saved frame name/ID items.
!
      SAVE                  SVCTR1
      SAVE                  SVFROM
      SAVE                  SVFCOD

      SAVE                  SVCTR2
      SAVE                  SVTO
      SAVE                  SVTCDE

      SAVE                  FIRST

!
!     Initial values.
!
      DATA                  FIRST   / .TRUE. /


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PXFORM' )

!
!     Initialization.
!
      IF ( FIRST ) THEN

!
!        Initialize counters.
!
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )

         FIRST = .FALSE.

      END IF


      CALL ZZNAMFRM ( SVCTR1, SVFROM, SVFCOD, FROM, FCODE )
      CALL ZZNAMFRM ( SVCTR2, SVTO,   SVTCDE, TO,   TCODE )

!
!     Only non-zero id-codes are legitimate frame id-codes.  Zero
!     indicates that the frame wasn't recognized.
!
      IF      ( FCODE .NE. 0 .AND. TCODE .NE. 0 ) THEN

         CALL REFCHG ( FCODE, TCODE, ET, ROTATE )

      ELSE IF ( FCODE .EQ. 0 .AND. TCODE .EQ. 0 ) THEN

         CALL SETMSG ( 'Neither of the frames # or # was ' &
         //            'recognized as a known reference frame. ' )
         CALL ERRCH  ( '#', FROM )
         CALL ERRCH  ( '#', TO   )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )

      ELSE IF ( FCODE .EQ. 0 ) THEN

         CALL SETMSG ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         CALL ERRCH  ( '#', FROM )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )

      ELSE IF ( TCODE .EQ. 0 ) THEN

         CALL SETMSG ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         CALL ERRCH  ( '#', TO   )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )

      END IF

      CALL CHKOUT ( 'PXFORM' )
      RETURN
      END
