!$Procedure T_WRDSK2 ( DSK test utilities, type 2 writer umbrella )

      SUBROUTINE T_WRDSK2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, &
                            A,      B,      C,      CORSYS, CORPAR, &
                            BOUNDS, NLON,   NLAT,   NV,     NP, &
                            USRVRT, USRPLT, MAKVTL, USEPAD, NPOLYV, &
                            NCROSS, R,      RCROSS, CENTER, NORMAL, &
                            DSK                                     )
      use pepper_legacy
      use pepper_inc_dskdsc
      use pepper_inc_dsk02

! $ Abstract
!
!     This is the umbrella routine for DSK type 2 writing test
!     utilities.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     ELLIPSOID
!     PLATE
!     TILE
!     TESSELLATE
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE



      INTEGER               BODYID
      INTEGER               SURFID
      CHARACTER*(*)         FRNAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      INTEGER               CORSYS
      DOUBLE PRECISION      CORPAR ( * )
      DOUBLE PRECISION      BOUNDS ( 2, 2 )
      INTEGER               NLON
      INTEGER               NLAT
      INTEGER               NV
      INTEGER               NP
      DOUBLE PRECISION      USRVRT ( 3, * )
      INTEGER               USRPLT ( 3, * )
      LOGICAL               MAKVTL
      LOGICAL               USEPAD
      INTEGER               NPOLYV
      INTEGER               NCROSS
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RCROSS
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      CHARACTER*(*)         DSK

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     BODYID     I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK
!                    T_SECDS2, T_TORUS
!     SURFID     I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK
!                    T_SECDS2, T_TORUS
!     FRNAME     I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK
!                    T_SECDS2, T_TORUS
!     FIRST      I   T_WRTPLT, T_ELDSK2, T_SECDS2
!     LAST       I   T_WRTPLT, T_ELDSK2, T_SECDS2
!     A          I   T_ELLDSK, T_SECDS2
!     B          I   T_ELLDSK, T_SECDS2
!     C          I   T_ELLDSK, T_SECDS2
!     CORSYS     I   T_WRTPLT, T_ELDSK2, T_SECDS2
!     CORPAR     I   T_WRTPLT, T_ELDSK2, T_SECDS2
!     BOUNDS     I   T_WRTPLT, T_ELDSK2, T_SECDS2
!     NLON       I   T_ELDSK2, T_ELDS2Z, T_SECDS2
!     NLAT       I   T_ELDSK2, T_ELDS2Z, T_SECDS2
!     NV         I   T_WRTPLT, T_WRTPLZ
!     NP         I   T_WRTPLT, T_WRTPLZ
!     USRVRT     I   T_WRTPLT, T_WRTPLZ
!     USRPLT     I   T_WRTPLT, T_WRTPLZ
!     MAKVTL     I   T_WRTPLT, T_ELDSK2, T_SECDS2
!     USEPAD     I   T_SECDS2
!     NPOLYV     I   T_TORUS
!     NCROSS     I   T_TORUS
!     R          I   T_TORUS
!     RCROSS     I   T_TORUS
!     CENTER     I   T_TORUS
!     NORMAL     I   T_TORUS
!     DSK        I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK,
!                    T_CG, T_SECDS2, T_TORUS
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their inputs.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     See the entry points for descriptions of exceptions
!     specific to those routines.
!
! $ Files
!
!     See the entry points.
!
! $ Particulars
!
!     The entry points in this routine create type 2 DSKs for
!     SPICELIB regression testing.
!
!     This umbrella routine serves as a location for declaration
!     of large arrays needed for spatial index generation, and
!     in some cases, for generation of plates and vertices.
!
!     In order to take advantage of the arrays declared here, new
!     DSK generating test utilities should be implemented as new
!     entry points of this routine, if feasible.
!
!     The entry points of this routine are
!
!       T_WRTPLT     {write plates and vertices: general case}
!
!       T_WRTPLZ     {write plates and vertices: "easy"}
!
!                       This routine uses latitudinal coordinates and
!                       default values for time and spatial bounds. It
!                       does not create a vertex-plate map.
!
!       T_ELDSK2     {write tessellated ellipsoid: general case}
!
!
!       T_ELDS2Z     {write tessellated ellipsoid: "easy"}
!
!                       This routine uses latitudinal coordinates and
!                       default values for time and spatial bounds. It
!                       does not create a vertex-plate map. It obtains
!                       target body reference ellipsoid radii from the
!                       kernel pool.
!
!       T_SMLDSK     {write DSK containing a single plate}
!
!
!       T_CG         {write DSK containing extremely simplified
!                     shape representing comet C-G}
!
!       T_SECDS2     {write section of tessellated ellipsoid: general
!                     case}
!
!       T_TORUS      {write DSK containing torus plate model}
!
! $ Examples
!
!     See use of in TSPICE test families.
!
! $ Restrictions
!
!     For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.1.0, 12-OCT-2021 (JDR) (NJB)
!
!        Changed local variable C0 from scalar to an array of
!        length 3; this fixes a bug in entry point T_TORUS.
!
!        Updated umbrella and entry point headers to comply with NAIF
!        standard. Added missing arguments in T_CG entry point
!        commented declarations section.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!        Added entry point that writes simplified CG DSK.
!
!        Reduced large array sizes by a factor of 8.
!
!        Previous version was dated 21-MAR-2016.
!
!-&


! $ Index_Entries
!
!     testutil type 2 dsk writing umbrella
!
!-&


!
!     SPICELIB functions
!



!
!     EXTERNAL routines
!
      EXTERNAL T_TORCRV

!
!     Local parameters
!
      INTEGER               MAXP
      PARAMETER           ( MAXP   = MAXPLT / 8 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = MAXVRT / 8 )

      INTEGER               WORKSZ
      PARAMETER           ( WORKSZ = 2 * MAXP )

      INTEGER               VOXPSZ
      PARAMETER           ( VOXPSZ = MAXP )

      INTEGER               VOXLSZ
      PARAMETER           ( VOXLSZ = 4 * MAXP  )

      INTEGER               MXIXSZ
      PARAMETER           ( MXIXSZ =   IXIFIX + VOXPSZ &
                                     + VOXLSZ          )
      INTEGER               SMLISZ
      PARAMETER           ( SMLISZ = 100100 )

      INTEGER               DCLASS
      PARAMETER           ( DCLASS = 2 )

!
!     Local variables
!
      DOUBLE PRECISION      ADJLAT
      DOUBLE PRECISION      C0     ( 3 )
      DOUBLE PRECISION      D0     ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      FINSCL
      DOUBLE PRECISION      LOCPAR ( NSYPAR )
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MNCOR1
      DOUBLE PRECISION      MNCOR2
      DOUBLE PRECISION      MNCOR3
      DOUBLE PRECISION      MXCOR1
      DOUBLE PRECISION      MXCOR2
      DOUBLE PRECISION      MXCOR3
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SMLIXD ( IXDFIX )
      DOUBLE PRECISION      SPAIXD ( IXDFIX )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TWIST
      DOUBLE PRECISION      V0     ( 3 )
      DOUBLE PRECISION      VRTCES ( 3, MAXV )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )

      INTEGER               BASE
      INTEGER               CORSCL
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               LNLAT
      INTEGER               LNLON
      INTEGER               LNP
      INTEGER               LNV
      INTEGER               LSIZEP
      INTEGER               LSIZEV
      INTEGER               N
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               WORK   ( 2, WORKSZ )
      INTEGER               SIZE
      INTEGER               SMLIXI ( SMLISZ )
      INTEGER               SPAIXI ( MXIXSZ )
      INTEGER               SPXISZ
      INTEGER               VLSIZE
      INTEGER               VMAPSZ
      INTEGER               VPSIZE

      LOGICAL               SMALL1

!
!     Saved variables
!
!     Save all large arrays to avoid stack problems.
!     Save spatial index variables for the small DSK writer.
!
      SAVE                  PLATES
      SAVE                  SMLIXD
      SAVE                  SMLIXI
      SAVE                  SPAIXI
      SAVE                  VRTCES
      SAVE                  WORK

      SAVE                  SMALL1

!
!     Initial values
!
      DATA                  SMALL1 / .TRUE. /


      CALL CHKIN  ( 'T_WRDSK2' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_WRDSK2' )
      RETURN




!$Procedure T_WRTPLT ( DSK test, write caller-supplied plate data )

      ENTRY T_WRTPLT ( BODYID, SURFID, FRNAME, FIRST, LAST, &
                       CORSYS, CORPAR, BOUNDS, NV,    NP, &
                       USRVRT, USRPLT, MAKVTL, DSK         )

! $ Abstract
!
!     Write a type 2 DSK containing vertices and plates passed
!     in by the caller.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     PLATE
!     TEST
!     UTILITY
!     VERTEX
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     DOUBLE PRECISION      FIRST
!     DOUBLE PRECISION      LAST
!     INTEGER               CORSYS
!     DOUBLE PRECISION      CORPAR ( * )
!     DOUBLE PRECISION      BOUNDS ( 2, 2 )
!     INTEGER               NV
!     INTEGER               NP
!     DOUBLE PRECISION      USRVRT ( 3, * )
!     INTEGER               USRPLT ( 3, * )
!     LOGICAL               MAKVTL
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     FIRST      I   Start time, TDB.
!     LAST       I   Stop time, TDB.
!     CORSYS     I   Coordinate system code.
!     CORPAR     I   Coordinate parameters.
!     BOUNDS     I   Segment spatial bounds.
!     NV         I   Number of vertices in input array.
!     NP         I   Number of plates in input array.
!     USRVRT     I   Vertex array.
!     USRPLT     I   Plate array.
!     MAKVTL     I   Vertex-plate mapping flag.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!     SURFID   is surface ID code.
!
!     FRNAME   is the name of the reference frame.
!
!     FIRST,
!     LAST     are, respectively, the start and stop times of
!              the coverage interval of the segment. FIRST and
!              LAST are expressed as seconds past J2000, TDB.
!
!     CORSYS   is an integer code designating the coordinate
!              system of the segment.
!
!
!     CORPAR   is an array of parameters associated with
!              the coordinate system.
!
!              For planetodetic coordinates,
!
!                  CORPAR(1) is the equatorial radius of the
!                            reference spheroid
!
!
!                  CORPAR(2) is the flattening coefficient of the
!                            reference spheroid
!
!
!     BOUNDS   is a 2x2 array of bounds of the first two coordinates
!              of the segment's spatial coverage. BOUNDS(1,I) is
!              the lower bound of the Ith coordinate; BOUNDS(2,I) is
!              the upper bound.
!
!     NV       is the number of vertices in the input vertex array.
!
!     NP       is the number of plates in the input plate array.
!
!     USRVRT   is an array of vertices. Elements
!
!                  USRVRT(J,I), J = 1, 3
!
!              are the components of the Ith vertex. Vertex indices
!              range from 1 to NV.
!
!
!     USRPLT   is an array of plates. Elements
!
!                  USRPLT(J,I), J = 1, 3
!
!              are the components of the Ith plate. Each component
!              is a vertex index. Plate indices range from 1 to NP.
!
!     MAKVTL   is a logical flag that controls whether a vertex-plate
!              mapping is created in the output segment. A mapping
!              is created if and only if MAKVTL is .TRUE.
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK will be
!         signaled by routines in the call tree of this routine.
!
! $ Files
!
!     See the description of DSK above.
!
! $ Particulars
!
!     This is the general test API for writing plate-vertex
!     data to a DSK file.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     testutil type 2 dsk plate-vertex writer
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_WRTPLT' )

!
!     Convert the frame name to an ID code.
!
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an ' &
         //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_WRTPLT'                             )
         RETURN

      END IF

!
!     Create a spatial index for the plate set.
!
!     Use a heuristic formula for the fine scale.
!
      FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

!
!     Pick a one-size-fits-all value for the large scale.
!
      CORSCL = 10

!
!     Set the spatial index integer component size.
!
      IF ( MAKVTL ) THEN

         VPSIZE = VOXPSZ / 2
         VLSIZE = VOXLSZ / 2

         VMAPSZ = 2*NV  + 3*NP

         SPXISZ = IXIFIX + VPSIZE + VLSIZE + VMAPSZ

      ELSE

         VPSIZE = VOXPSZ
         VLSIZE = VOXLSZ
         SPXISZ = MXIXSZ

      END IF

      CALL DSKMI2 ( NV,     USRVRT, NP,     USRPLT, FINSCL, &
                    CORSCL, WORKSZ, VPSIZE, VLSIZE, MAKVTL, &
                    SPXISZ, WORK,   SPAIXD, SPAIXI         )

!
!     Generate bounds for the 3rd coordinate.
!
      CALL DSKRB2 ( NV,     USRVRT, NP,     USRPLT, &
                    CORSYS, CORPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_WRTPLT' )
         RETURN
      END IF

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID, &
                    DCLASS,      FRNAME,      CORSYS, &
                    CORPAR,      BOUNDS(1,1), BOUNDS(2,1), &
                    BOUNDS(1,2), BOUNDS(2,2), MNCOR3, &
                    MXCOR3,      FIRST,       LAST, &
                    NV,          USRVRT,      NP, &
                    USRPLT,      SPAIXD,      SPAIXI      )

!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_WRTPLT' )
      RETURN






!$Procedure T_WRTPLZ ( DSK test, write plate data, easy )

      ENTRY T_WRTPLZ ( BODYID, SURFID, FRNAME, NV, &
                       NP,     USRVRT, USRPLT, DSK )

! $ Abstract
!
!     Write a type 2 DSK containing vertices and plates passed
!     in by the caller. Use default values to simplify the call.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     PLATE
!     TEST
!     UTILITY
!     VERTEX
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     INTEGER               NV
!     INTEGER               NP
!     DOUBLE PRECISION      USRVRT ( 3, * )
!     INTEGER               USRPLT ( 3, * )
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     NV         I   Number of vertices in input array.
!     NP         I   Number of plates in input array.
!     USRVRT     I   Vertex array.
!     USRPLT     I   Plate array.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!     SURFID   is surface ID code.
!
!     FRNAME   is the name of the reference frame.
!
!     NV       is the number of vertices in the input vertex array.
!
!     NP       is the number of plates in the input plate array.
!
!     USRVRT   is an array of vertices. Elements
!
!                 USRVRT(J,I), J = 1, 3
!
!              are the components of the Ith vertex. Vertex indices
!              range from 1 to NV.
!
!
!     USRPLT   is an array of plates. Elements
!
!                 USRPLT(J,I), J = 1, 3
!
!              are the components of the Ith plate. Each component
!              is a vertex index. Plate indices range from 1 to NP.
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK will be
!         signaled by routines in the call tree of this routine.
!
! $ Files
!
!     See the description of DSK above.
!
! $ Particulars
!
!     This is a simplified API for writing plate-vertex
!     data to a DSK file. The following defaults are used
!
!        Time bounds:               -100 : 100 Julian years TDB
!
!        Coordinate system:         Latitudinal
!
!        Longitude bounds:          0     : 2*pi radians
!        Latitude bounds:           -pi/2 : pi/2 radians
!
!     This routine does not construct a vertex-plate mapping.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     testutil type 2 dsk plate-vertex easy writer
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_WRTPLZ' )

!
!     Convert the frame name to an ID code.
!
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an ' &
         //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_WRTPLZ'                             )
         RETURN

      END IF

!
!     Create a spatial index for the plate set.
!
!     Use a heuristic formula for the fine scale.
!
      FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

!
!     Pick a one-size-fits-all value for the large scale.
!
      CORSCL = 10

!
!     Set the spatial index integer component size.
!
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ

      CALL DSKMI2 ( NV,     USRVRT, NP,     USRPLT, FINSCL, &
                    CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .FALSE., &
                    SPXISZ, WORK,   SPAIXD, SPAIXI          )

!
!     Generate bounds for the 3rd coordinate.
!
      CALL DSKRB2 ( NV,     USRVRT, NP,     USRPLT, &
                    LATSYS, LOCPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_WRTPLZ' )
         RETURN
      END IF

!
!     Set default segment parameters.
!
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      CALL CLEARD ( NSYPAR, LOCPAR )

      ET0 = -100 * JYEAR()
      ET1 =  100 * JYEAR()

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID, &
                    DCLASS,      FRNAME,      LATSYS, &
                    LOCPAR,      MNCOR1,      MXCOR1, &
                    MNCOR2,      MXCOR2,      MNCOR3, &
                    MXCOR3,      ET0,         ET1, &
                    NV,          USRVRT,      NP, &
                    USRPLT,      SPAIXD,      SPAIXI )

!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_WRTPLZ' )
      RETURN






!$Procedure T_ELDSK2 ( DSK test, write tessellated ellipsoid DSK )

      ENTRY T_ELDSK2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, &
                       CORSYS, CORPAR, BOUNDS, A,      B, &
                       C,      NLON,   NLAT,   MAKVTL, DSK  )

! $ Abstract
!
!     Write a type 2 DSK containing a tessellated ellipsoid.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     ELLIPSOID
!     PLATE
!     TILE
!     TESSELLATE
!     TEST
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     DOUBLE PRECISION      FIRST
!     DOUBLE PRECISION      LAST
!     INTEGER               CORSYS
!     DOUBLE PRECISION      CORPAR ( * )
!     DOUBLE PRECISION      BOUNDS ( 2, 2 )
!     DOUBLE PRECISION      A
!     DOUBLE PRECISION      B
!     DOUBLE PRECISION      C
!     INTEGER               NLON
!     INTEGER               NLAT
!     LOGICAL               MAKVTL
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     FIRST      I   Start time, TDB.
!     LAST       I   Stop time, TDB.
!     CORSYS     I   Coordinate system code.
!     CORPAR     I   Coordinate parameters.
!     BOUNDS     I   Segment spatial bounds.
!     A          I   Length of ellipsoid semi-axis lying on the x-axis.
!     B          I   Length of ellipsoid semi-axis lying on the y-axis.
!     C          I   Length of ellipsoid semi-axis lying on the z-axis.
!     NLON       I   Number of longitude bands in plate set.
!     NLAT       I   Number of latitude bands in plate set.
!     MAKVTL     I   Vertex-plate mapping flag.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!     SURFID   is surface ID code.
!
!     FRNAME   is the name of the reference frame.
!
!     FIRST,
!     LAST     are, respectively, the start and stop times of
!              the coverage interval of the segment. FIRST and
!              LAST are expressed as seconds past J2000, TDB.
!
!     CORSYS   is an integer code designating the coordinate
!              system of the segment.
!
!
!     CORPAR   is an array of parameters associated with
!              the coordinate system.
!
!              For planetodetic coordinates,
!
!                 CORPAR(1) is the equatorial radius of the
!                           reference spheroid
!
!
!                 CORPAR(2) is the flattening coefficient of the
!                           reference spheroid
!
!
!     BOUNDS   is a 2x2 array of bounds of the first two coordinates
!              of the segment's spatial coverage. BOUNDS(1,I) is
!              the lower bound of the Ith coordinate; BOUNDS(2,I) is
!              the upper bound.
!
!     A,
!     B,
!     C        are the lengths of the semi-axes of a triaxial
!              ellipsoid. The ellipsoid is centered at the origin and
!              oriented so that its axes lie on the x, y and z axes.
!              A, B, and C are the lengths of the semi-axes that
!              point in the x, y, and z directions respectively.
!
!
!     NLON     is the number of longitude bands in the plate set to
!              be created by this routine. Each longitude band is
!              bounded by two meridians. All longitude bands have
!              equal angular extent in longitude. The vertices of any
!              plate lie on adjacent longitude band boundaries.
!
!
!     NLAT     is the number of latitude bands in the plate set. The
!              vertices of each band are bounded by two cones of
!              constant planetocentric latitude. All latitude bands
!              have equal angular extent in planetocentric latitude.
!              The vertices of any plate lie on adjacent latitude
!              band boundaries.
!
!              Each polar "cap" consists of one longitude band.
!
!
!     MAKVTL   is a logical flag that controls whether a vertex-plate
!              mapping is created in the output segment. A mapping
!              is created if and only if MAKVTL is .TRUE.
!
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK will be
!         signaled by routines in the call tree of this routine.
!
!     3)  If the length of any semi-axis of the ellipsoid is
!         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
!
! $ Files
!
!     See the description of DSK above.
!
! $ Particulars
!
!     This routine creates a DSK containing a single type 2 segment.
!     The plate and vertex data in the segment represent a tessellated
!     ellipsoid.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     write type 2 dsk containing tessellated ellipsoid
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ELDSK2' )

!
!     Convert the frame name to an ID code.
!
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an ' &
         //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_ELDSK2'                             )
         RETURN

      END IF

!
!     Create vertices and plates.
!
      CALL ZZELLPLT ( A,    B,   C,      NLON, NLAT,  MAXV, &
                      MAXP, LNV, VRTCES, LNP,  PLATES       )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDSK2' )
         RETURN
      END IF

!
!     Create a spatial index for the plate set.
!
!     Use a heuristic formula for the fine scale.
!
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

!
!     Pick a one-size-fits-all value for the coarse scale.
!
      CORSCL = 10

!
!     Set the spatial index integer component size.
!
      IF ( MAKVTL ) THEN

         VPSIZE = VOXPSZ / 2
         VLSIZE = VOXLSZ / 2

         VMAPSZ = 2*LNV  + 3*LNP

         SPXISZ = IXIFIX + VPSIZE + VLSIZE + VMAPSZ

      ELSE

         VPSIZE = VOXPSZ
         VLSIZE = VOXLSZ
         SPXISZ = MXIXSZ

      END IF

      CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL, &
                    CORSCL, WORKSZ, VPSIZE, VLSIZE, MAKVTL, &
                    SPXISZ, WORK,   SPAIXD, SPAIXI          )

!
!     Generate bounds for the 3rd coordinate.
!
      CALL DSKRB2 ( LNV,    VRTCES, LNP,    PLATES, &
                    CORSYS, CORPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDSK2' )
         RETURN
      END IF

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID, &
                    DCLASS,      FRNAME,      CORSYS, &
                    CORPAR,      BOUNDS(1,1), BOUNDS(2,1), &
                    BOUNDS(1,2), BOUNDS(2,2), MNCOR3, &
                    MXCOR3,      FIRST,       LAST, &
                    LNV,         VRTCES,      LNP, &
                    PLATES,      SPAIXD,      SPAIXI      )

!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_ELDSK2' )
      RETURN






!$Procedure T_ELDS2Z ( write tessellated ellipsoid type 2 DSK, easy )

      ENTRY T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK )

! $ Abstract
!
!     Write a type 2 DSK containing a tessellated ellipsoid.
!     Use default values to simplify the call.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     ELLIPSOID
!     PLATE
!     TILE
!     TESSELLATE
!     TEST
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     INTEGER               NLON
!     INTEGER               NLAT
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     NLON       I   Number of longitude bands in plate set.
!     NLAT       I   Number of latitude bands in plate set.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!
!     SURFID   is surface ID code.
!
!
!     FRNAME   is the name of the reference frame.
!
!
!     NLON     is the number of longitude bands in the plate set to
!              be created by this routine. Each longitude band is
!              bounded by two meridians. All longitude bands have
!              equal angular extent in longitude. The vertices of any
!              plate lie on adjacent longitude band boundaries.
!
!
!     NLAT     is the number of latitude bands in the plate set. The
!              vertices of each band are bounded by two cones of
!              constant planetocentric latitude. All latitude bands
!              have equal angular extent in planetocentric latitude.
!              The vertices of any plate lie on adjacent latitude
!              band boundaries.
!
!              Each polar "cap" consists of one longitude band.
!
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK will be
!         signaled by routines in the call tree of this routine.
!
!     3)  If the length of any semi-axis of the ellipsoid is
!         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
!
! $ Files
!
!     Input: a PCK file must be loaded in order to provide radii of
!            the reference ellipsoid for the specified body.
!
!     Output: see the description of DSK above.
!
! $ Particulars
!
!     This is a simplified API for writing a tessellated ellipsoid
!     model to a DSK file. The following defaults are used
!
!        Time bounds:               -100 : 100 Julian years TDB
!
!        Coordinate system:         Latitudinal
!
!        Longitude bounds:          0     : 2*pi radians
!        Latitude bounds:           -pi/2 : pi/2 radians
!
!     This routine does not construct a vertex-plate mapping.
!
!     Ellipsoid radii for the specified body are obtained from the
!     kernel pool.
!
!     This routine creates a DSK containing a single type 2 segment.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     write type 2 DSK containing tessellated ellipsoid, easy
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ELDS2Z' )

!
!     Convert the frame name to an ID code.
!
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an ' &
         //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_ELDS2Z'                             )
         RETURN

      END IF

!
!     Get target body radii from the kernel pool.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDS2Z' )
         RETURN
      END IF

!
!     Create vertices and plates.
!
      CALL ZZELLPLT ( RADII(1), RADII(2), RADII(3), NLON, &
                      NLAT,     MAXV,     MAXP,     LNV, &
                      VRTCES,   LNP,      PLATES         )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDS2Z' )
         RETURN
      END IF

!
!     Create a spatial index for the plate set.
!
!     Use a heuristic formula for the fine scale.
!
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

!
!     Pick a one-size-fits-all value for the coarse scale.
!
      CORSCL = 10

!
!     Set the spatial index integer component size.
!
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ

      CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL, &
                    CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .FALSE., &
                    SPXISZ, WORK,   SPAIXD, SPAIXI           )

!
!     Generate bounds for the 3rd coordinate.
!
      CALL DSKRB2 ( LNV,    VRTCES, LNP,    PLATES, &
                    LATSYS, LOCPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDS2Z' )
         RETURN
      END IF

!
!     Set default inputs.
!
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      ET0    = -JYEAR() * 100
      ET1    =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, LOCPAR )

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID, &
                    DCLASS,      FRNAME,      LATSYS, &
                    LOCPAR,      MNCOR1,      MXCOR1, &
                    MNCOR2,      MXCOR2,      MNCOR3, &
                    MXCOR3,      ET0,         ET1, &
                    LNV,         VRTCES,      LNP, &
                    PLATES,      SPAIXD,      SPAIXI )

!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_ELDS2Z' )
      RETURN



!$Procedure T_SMLDSK ( write small DSK )

      ENTRY T_SMLDSK ( BODYID, SURFID, FRNAME, DSK )

! $ Abstract
!
!     Write a trivial type 2 DSK containing one plate.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     PLATE
!     TEST
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!     SURFID   is surface ID code.
!
!     FRNAME   is the name of the reference frame.
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
!              The DSK is file is unsegregated.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK will be
!         signaled by routines in the call tree of this routine.
!
! $ Files
!
!     Output: see the description of DSK above.
!
! $ Particulars
!
!     This is a simplified API for quickly writing small DSK file. The
!     file contains one type 2 segment; the segment contains a single
!     plate.
!
!     The output file is unsegregated.
!
!     Files created by this routine are suitable for testing
!     load/unload operations.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     write small type 2 dsk
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SMLDSK' )

!
!     Convert the frame name to an ID code.
!
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an ' &
         //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_SMLDSK'                             )
         RETURN

      END IF


      LNP = 1
      LNV = 3

      CALL VPACK ( 0.D0, 0.D0, 0.D0, VRTCES(1,1) )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, VRTCES(1,2) )
      CALL VPACK ( 0.D0, 1.D0, 0.D0, VRTCES(1,3) )

      PLATES(1,1) = 1
      PLATES(2,1) = 2
      PLATES(3,1) = 3

      VPSIZE = 32
      VLSIZE = 40
      SPXISZ = SMLISZ

      FINSCL = 1.D0
      CORSCL = 1

!
!     Set default inputs.
!
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 =  0.D0
      MXCOR3 =  1.D0

      ET0    = -JYEAR() * 100
      ET1    =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, LOCPAR )


      IF ( SMALL1 ) THEN

         CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL, &
                       CORSCL, WORKSZ, VPSIZE, VLSIZE, .FALSE., &
                       SPXISZ, WORK,   SMLIXD, SMLIXI           )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_SMLDSK' )
            RETURN
         END IF

         SMALL1 = .FALSE.

      END IF

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID, &
                    DCLASS,      FRNAME,      LATSYS, &
                    LOCPAR,      MNCOR1,      MXCOR1, &
                    MNCOR2,      MXCOR2,      MNCOR3, &
                    MXCOR3,      ET0,         ET1, &
                    LNV,         VRTCES,      LNP, &
                    PLATES,      SMLIXD,      SMLIXI )
!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .FALSE. )

      CALL CHKOUT ( 'T_SMLDSK' )
      RETURN




!$Procedure T_CG ( write simplified comet C-G DSK )

      ENTRY T_CG ( BODYID, SURFID, FRNAME, DSK )

! $ Abstract
!
!     Write a DSK containing an extremely simplified shape model
!     representing the nucleus of comet Churyumov-Gerasimenko. The
!     model is a union of boxes.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     PLATE
!     TEST
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!     SURFID   is surface ID code.
!
!     FRNAME   is the name of the reference frame.
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     Output: see the description of DSK above.
!
! $ Particulars
!
!     This routine writes a DSK that contains a representation
!     of the shape similar to that of comet Churyumov-Gerasimenko.
!     The model is composed of several boxes.
!
!     The output file contains a single type 2 segment.
!
!     The ID codes in the output file are provided by the user.
!     The user may wish to center the segment on a body other
!     than comet C-G.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
!
!        Edited header to comply with NAIF standard. Added missing
!        arguments in the declarations section commented area.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     write comet cg-like dsk
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_CG' )

!
!     Treat the local vertex and plate arrays as cells,
!     since the API of ZZT_CG requires this.
!
      LSIZEV = 100
      LSIZEP = 200

      CALL SSIZED ( LSIZEV, VRTCES )
      CALL SSIZEI ( LSIZEP, PLATES )

!
!     Create vertices and plates. In each array, data
!     start at 1-dimensional index LBCELL+1. This
!     location coincides with the 2-dimensional index (1,3).
!
      CALL ZZT_CG ( VRTCES, PLATES )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CG' )
         RETURN
      END IF

      LNV = CARDD ( VRTCES )  / 3
      LNP = CARDI ( PLATES )  / 3

!
!     Create a spatial index for the plate set.
!
      FINSCL = 1.D0

!
!     We can use a small coarse scale.
!
      CORSCL = 1

!
!     Set the spatial index integer component size.
!
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ

      CALL DSKMI2 ( LNV,         VRTCES(1,3),    LNP, &
                    PLATES(1,3), FINSCL,         CORSCL, &
                    WORKSZ,      VOXPSZ, VOXLSZ, .FALSE., &
                    SPXISZ,      WORK,   SPAIXD, SPAIXI   )

!
!     Generate bounds for the 3rd coordinate.
!
      CALL DSKRB2 ( LNV,         VRTCES(1,3), LNP, &
                    PLATES(1,3), LATSYS,      LOCPAR, &
                    MNCOR3,      MXCOR3              )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CG' )
         RETURN
      END IF

!
!     Set default inputs.
!
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      ET0    = -JYEAR() * 100
      ET1    =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, LOCPAR )

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID, &
                    DCLASS,      FRNAME,      LATSYS, &
                    LOCPAR,      MNCOR1,      MXCOR1, &
                    MNCOR2,      MXCOR2,      MNCOR3, &
                    MXCOR3,      ET0,         ET1, &
                    LNV,         VRTCES(1,3), LNP, &
                    PLATES(1,3), SPAIXD,      SPAIXI )

!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .TRUE. )


      CALL CHKOUT ( 'T_CG' )
      RETURN




!$Procedure T_SECDS2 ( DSK test, write ellipsoid section DSK )

      ENTRY T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST, &
                       CORSYS, CORPAR, BOUNDS, A,      B, &
                       C,      NLON,   NLAT,   MAKVTL, USEPAD, &
                       DSK                                     )

! $ Abstract
!
!     Write a type 2 DSK containing a section of a tessellated
!     ellipsoid.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     ELLIPSOID
!     PLATE
!     TILE
!     TESSELLATE
!     TEST
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     DOUBLE PRECISION      FIRST
!     DOUBLE PRECISION      LAST
!     INTEGER               CORSYS
!     DOUBLE PRECISION      CORPAR ( * )
!     DOUBLE PRECISION      BOUNDS ( 2, 2 )
!     DOUBLE PRECISION      A
!     DOUBLE PRECISION      B
!     DOUBLE PRECISION      C
!     INTEGER               NLON
!     INTEGER               NLAT
!     LOGICAL               MAKVTL
!     LOGICAL               USEPAD
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     FIRST      I   Start time, TDB.
!     LAST       I   Stop time, TDB.
!     CORSYS     I   Coordinate system code.
!     CORPAR     I   Coordinate parameters.
!     BOUNDS     I   Segment spatial bounds.
!     A          I   Length of ellipsoid semi-axis lying on the x-axis.
!     B          I   Length of ellipsoid semi-axis lying on the y-axis.
!     C          I   Length of ellipsoid semi-axis lying on the z-axis.
!     NLON       I   Number of longitude bands in plate set.
!     NLAT       I   Number of latitude bands in plate set.
!     MAKVTL     I   Vertex-plate mapping flag.
!     USEPAD     I   Padding flag.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!     SURFID   is surface ID code.
!
!     FRNAME   is the name of the reference frame.
!
!     FIRST,
!     LAST     are, respectively, the start and stop times of
!              the coverage interval of the segment. FIRST and
!              LAST are expressed as seconds past J2000, TDB.
!
!     CORSYS   is an integer code designating the coordinate
!              system of the segment. Limited to planetocentric
!              coordinates for full functionality. Planetodetic
!              coordinates may be used if padding generation is
!              not invoked.
!
!
!     CORPAR   is an array of parameters associated with
!              the coordinate system.
!
!              For planetodetic coordinates,
!
!                 CORPAR(1) is the equatorial radius of the
!                           reference spheroid
!
!
!                 CORPAR(2) is the flattening coefficient of the
!                           reference spheroid
!
!
!     BOUNDS   is a 2x2 array of bounds of the first two coordinates
!              of the segment's spatial coverage. BOUNDS(1,I) is
!              the lower bound of the Ith coordinate; BOUNDS(2,I) is
!              the upper bound.
!
!     A,
!     B,
!     C        are the lengths of the semi-axes of a triaxial
!              ellipsoid. The ellipsoid is centered at the origin and
!              oriented so that its axes lie on the x, y and z axes.
!              A, B, and C are the lengths of the semi-axes that
!              point in the x, y, and z directions respectively.
!
!
!     NLON     is the number of longitude bands in the plate set to
!              be created by this routine. Each longitude band is
!              bounded by two meridians. All longitude bands have
!              equal angular extent in longitude. The vertices of any
!              plate lie on adjacent longitude band boundaries.
!
!              If padding is enabled, an extra longitude band will be
!              created at both longitude boundaries of the segment.
!
!
!     NLAT     is the number of latitude bands in the plate set. The
!              vertices of each band are bounded by two cones of
!              constant planetocentric latitude. All latitude bands
!              have equal angular extent in planetocentric latitude.
!              The vertices of any plate lie on adjacent latitude
!              band boundaries.
!
!              Each polar "cap" consists of one longitude band.
!
!              If padding is enabled, and if the segment does not
!              span the X-Y plane, one or more extra latitude bands
!              will be created at the segment latitude boundary
!              closest to the X-Y plane. A single extra latitude band
!              will be created at the other latitude boundary. If the
!              segment spans the X-Y plane, then a single extra
!              latitude bound will be created at each latitude
!              boundary.
!
!
!     MAKVTL   is a logical flag that controls whether a vertex-plate
!              mapping is created in the output segment. A mapping
!              is created if and only if MAKVTL is .TRUE.
!
!
!     USEPAD   is a logical flag indicating whether segment padding
!              is to be created. When USEPAD is .TRUE., sections
!              having positive lower latitude bounds are padded at
!              the bottom to ensure coverage. This padding consists
!              of an integral number of extra latitude bands. Each of
!              the other boundaries are padded with a single latitude
!              or longitude band. When USEPAD is .FALSE., no padding
!              is created.
!
!              See Restrictions below.
!
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK will be
!         signaled by routines in the call tree of this routine.
!
!     3)  If the length of any semi-axis of the ellipsoid is
!         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
!
!     4)  If either the longitude or latitude band count is
!         non-positive, the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     5)  If the input latitude bounds are out of order, the error
!         SPICE(BOUNDSOUTOFORDER) is signaled.
!
! $ Files
!
!     See the description of DSK above.
!
! $ Particulars
!
!     This routine creates a DSK containing a single type 2 segment.
!     The plate and vertex data in the segment represent a section of a
!     tessellated ellipsoid. The section covers a specified longitude/
!     latitude rectangle.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
!     2) Currently restricted to latitudinal coordinates for full
!        functionality. Planetodetic and rectangular coordinates may be
!        used if padding generation is not commanded.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     write type 2 dsk containing ellipsoid section
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SECDS2' )

!
!     Check coordinate system.
!
      IF (       ( CORSYS .NE. LATSYS ) &
           .AND. ( CORSYS .NE. PDTSYS ) &
           .AND. ( CORSYS .NE. RECSYS )  ) THEN

         CALL SETMSG ( 'Input coordinate system code was #. ' &
         //            'Only latitudinal, planetodetic, and ' &
         //            'rectangular coordinates are ' &
         //            'currently supported by this routine.' )
         CALL ERRINT ( '#', CORSYS                            )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                  )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

!
!     Convert the frame name to an ID code.
!
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an ' &
         //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

!
!     Check band counts.
!
      IF ( NLAT .LT. 1 ) THEN

         CALL SETMSG ( 'Latitude band count must be positive ' &
         //            'but was #.'                           )
         CALL ERRINT ( '#', NLAT                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'               )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

      IF ( NLON .LT. 1 ) THEN

         CALL SETMSG ( 'Longitude band count must be positive ' &
         //            'but was #.'                           )
         CALL ERRINT ( '#', NLON                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'               )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

!
!     Reject invalid latitude bounds.
!
      IF ( BOUNDS(2,2) .LT. BOUNDS(1,2) ) THEN

         CALL SETMSG ( 'Latitude bounds are out of order: bounds ' &
         //            'are #:#.'                                 )
         CALL ERRDP  ( '#',  BOUNDS(1,2)                          )
         CALL ERRDP  ( '#',  BOUNDS(2,2)                          )
         CALL SIGERR ( 'SPICE(BOUNDSOUTOFORDER)'                  )
         CALL CHKOUT ( 'T_SECDS2'                                 )
         RETURN

      END IF

!
!     Make local copies of the band counts.
!
      LNLON = NLON
      LNLAT = NLAT

!
!     Create vertices and plates.
!
      IF ( USEPAD ) THEN

         IF ( CORSYS .NE. LATSYS ) THEN

            CALL SETMSG ( 'Padding generation was requested for ' &
            //            'coordinate system #. This service is ' &
            //            'available only for planetocentric ' &
            //            'coordinates.'                         )
            CALL ERRINT ( '#', CORSYS                            )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                  )
            CALL CHKOUT ( 'T_SECDS2'                             )
            RETURN

         END IF

!
!        Determine sizes of longitude and latitude bands.
!
         MINLAT = BOUNDS(1,2)
         MAXLAT = BOUNDS(2,2)

         DLAT   = ( MAXLAT - MINLAT ) / NLAT

         MINLON = BOUNDS(1,1)
         MAXLON = BOUNDS(2,1)

         IF ( MAXLON .LE. MINLON ) THEN
            MAXLON = MAXLON + TWOPI()
         END IF

         DLON   = ( MAXLON - MINLON ) / NLON

!
!        Determine the latitude padding.
!
         IF ( MAXLAT .GE. 0.D0 ) THEN
!
!           Add just one upper band, if it will fit. If not,
!           don't create a smaller band; just omit the band.
!
            IF (  ( MAXLAT+DLAT ) .LT. HALFPI() ) THEN

               MAXLAT = MAXLAT + DLAT
               LNLAT  = LNLAT  + 1

            END IF

         ELSE
!
!           Determine the adjusted latitude needed for full
!           latitude coverage.
!
            CALL ZZCHRLAT ( MAXLAT, DLON, ADJLAT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SECDS2' )
               RETURN
            END IF

            DELTA = ADJLAT - MAXLAT

!
!           Determine the number of latitude bands that must
!           be added to give the required coverage.
!
            Q     = DELTA / DLAT

            IF ( Q .GT. AINT(Q) ) THEN
!
!              Q has a non-zero fractional part.
!
               Q = AINT(Q) + 1.D0

            END IF

            MAXLAT = MAXLAT + Q*DLAT
            LNLAT  = LNLAT  + INT(Q)

         END IF
!
!        MAXLAT and LNLAT have been updated to account for
!        upper padding, if it has been added.
!

         IF ( MINLAT .LE. 0.D0 ) THEN
!
!           Add just one lower band, if it will fit. If not,
!           don't create a smaller band; just omit the band.
!
            IF (  ( MINLAT-DLAT ) .GT. -HALFPI() ) THEN

               MINLAT = MINLAT - DLAT
               LNLAT  = LNLAT  + 1

            END IF

         ELSE
!
!           Determine the adjusted latitude needed for full
!           latitude coverage.
!
            CALL ZZCHRLAT ( MINLAT, DLON, ADJLAT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SECDS2' )
               RETURN
            END IF

            DELTA = MINLAT - ADJLAT

!
!           Determine the number of latitude bands that must
!           be added to give the required coverage.
!
            Q     = DELTA / DLAT

            IF ( Q .GT. AINT(Q) ) THEN
!
!              Q has a non-zero fractional part.
!
               Q = AINT(Q) + 1.D0

            END IF

            MINLAT = MINLAT - Q*DLAT
            LNLAT  = LNLAT  + INT(Q)

         END IF
!
!        MAXLAT and LNLAT have been updated to account for
!        lower padding, if it has been added.
!

!
!        Adjust the longitude boundaries and band count to
!        reflect longitude padding. Padded is added only if
!        the padded section would have 2*pi longitude extent or
!        less.
!
         IF ( (MAXLON - MINLON) .LT. (TWOPI()-2*DLON) ) THEN
!
!           Add longitude padding.
!
            MINLON = MINLON - DLON
            MAXLON = MAXLON + DLON
            LNLON  = LNLON  + 2

         END IF

!
!        Create a vertex and plate set that includes padding.
!
         CALL ZZELLSEC ( A,           B,           C, &
                         MINLON,      MAXLON,      MINLAT, &
                         MAXLAT,      LNLON,       LNLAT, &
                         MAXV,        MAXP,        LNV, &
                         VRTCES,      LNP,         PLATES     )

      ELSE
!
!        Create a vertex and plate set only for the specified region.
!
         CALL ZZELLSEC ( A,           B,           C, &
                         BOUNDS(1,1), BOUNDS(2,1), BOUNDS(1,2), &
                         BOUNDS(2,2), NLON,        NLAT, &
                         MAXV,        MAXP,        LNV, &
                         VRTCES,      LNP,         PLATES     )

      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_SECDS2' )
         RETURN
      END IF

!
!     Create a spatial index for the plate set.
!
!     Use a heuristic formula for the fine scale.
!
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

!
!     Pick a one-size-fits-all value for the coarse scale.
!
      CORSCL = 10

!
!     Set the spatial index integer component size.
!
      IF ( MAKVTL ) THEN

         VPSIZE = VOXPSZ / 2
         VLSIZE = VOXLSZ / 2

         VMAPSZ = 2*LNV  + 3*LNP

         SPXISZ = IXIFIX + VPSIZE + VLSIZE + VMAPSZ

      ELSE

         VPSIZE = VOXPSZ
         VLSIZE = VOXLSZ
         SPXISZ = MXIXSZ

      END IF

      CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL, &
                    CORSCL, WORKSZ, VPSIZE, VLSIZE, MAKVTL, &
                    SPXISZ, WORK,   SPAIXD, SPAIXI          )

!
!     Generate bounds for the 3rd coordinate.
!
      CALL DSKRB2 ( LNV,    VRTCES, LNP,    PLATES, &
                    CORSYS, CORPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_SECDS2' )
         RETURN
      END IF

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID, &
                    DCLASS,      FRNAME,      CORSYS, &
                    CORPAR,      BOUNDS(1,1), BOUNDS(2,1), &
                    BOUNDS(1,2), BOUNDS(2,2), MNCOR3, &
                    MXCOR3,      FIRST,       LAST, &
                    LNV,         VRTCES,      LNP, &
                    PLATES,      SPAIXD,      SPAIXI      )

!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_SECDS2' )
      RETURN





!$Procedure T_TORUS ( write DSK containing torus plate model )

      ENTRY T_TORUS ( BODYID, SURFID, FRNAME, NPOLYV, NCROSS, &
                      R,      RCROSS, CENTER, NORMAL, DSK    )

! $ Abstract
!
!     Write a DSK containing a plate model representing a torus.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     PLATE
!     TEST
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     CHARACTER*(*)         FRNAME
!     INTEGER               NPOLYV
!     INTEGER               NCROSS
!     DOUBLE PRECISION      R
!     DOUBLE PRECISION      RCROSS
!     DOUBLE PRECISION      CENTER ( 3 )
!     DOUBLE PRECISION      NORMAL ( 3 )
!     CHARACTER*(*)         DSK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID.
!     SURFID     I   Surface ID.
!     FRNAME     I   Reference frame name.
!     NPOLYV     I   Number of vertices in polygonal cross section.
!     NCROSS     I   Number of cross sections.
!     R          I   Radius of central curve.
!     RCROSS     I   Radius of cross section.
!     CENTER     I   Center of the torus.
!     NORMAL     I   Normal vector to plane of central curve.
!     DSK        I   Name of DSK file to write.
!
! $ Detailed_Input
!
!     BODYID   is the body ID code of the segment to be written
!              to the output DSK file.
!
!     SURFID   is surface ID code.
!
!     FRNAME   is the name of the reference frame.
!
!     NPOLYV   is the number of vertices per polygonal cross section
!              of the torus. Each cross section is the set of
!              vertices of a regular polygon inscribed in a circle of
!              radius RCROSS (see below). Each cross section lies in
!              a plane normal to the derivative of the central curve
!              at its point of intersection with the plane.
!
!     NCROSS   is the number of cross sections of the torus.
!
!     R        is the radius of the central curve of the torus.
!              Each cross section is centered on this curve and
!              is normal to the derivative of the curve at the point
!              of intersection.
!
!     RCROSS   is the radius of each circular cross section of the
!              torus.
!
!     CENTER   is the center of the torus. This is the center of the
!              central curve of the torus. The line normal to the
!              plane of the central curve and passing through CENTER
!              is the axis of azimuthal symmetry of the torus.
!
!     NORMAL   is a vector normal to the plane containing the
!              central curve of the torus.
!
!     DSK      is the name of the DSK file to write. If the file
!              does not exist, it will be created. If the file
!              exists, this routine will append to it.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input frame name cannot be converted to an ID code,
!         the error SPICE(FRAMEIDNOTFOUND) is signaled.
!
!     2)  Any errors that occur during the process of creating the
!         output segment's spatial index or of writing the DSK will be
!         signaled by routines in the call tree of this routine.
!
! $ Files
!
!     Output: see the description of DSK above.
!
! $ Particulars
!
!     In a Cartesian coordinate system having the Z axis parallel to
!     NORMAL, and having an origin coincident with CENTER, the torus
!     is the set of points
!
!                                2   2  1/2  2     2           2
!        { (x,y,z) : (  R  -  ( x + y  )    )  +  z   =  RCROSS  }
!
!
!     This routine can be used to create shape models suitable for
!     testing limb finding and terminator finding routines.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TESTUTIL Version 1.1.0, 12-OCT-2021 (JDR) (NJB)
!
!        Bug fix: call to T_TORCRV writes a double precision
!        array of length 3 to the second argument, but a scalar
!        second argument had been passed. Now fixed.
!
!        Edited the header to comply with NAIF standard.
!
!-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     write dsk containing torus plate model
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TORUS' )

      IF ( NPOLYV .LT. 3 ) THEN

         CALL SETMSG ( 'Polygon side count # must be at least ' &
         //            'three.'                                 )
         CALL ERRINT ( '#', NPOLYV                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'T_TORUS'                                )
         RETURN

      END IF

      IF ( NCROSS .LT. 3 ) THEN

         CALL SETMSG ( 'Cross section count # must be at least ' &
         //            'three.'                                 )
         CALL ERRINT ( '#', NCROSS                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'T_TORUS'                                )
         RETURN

      END IF

!
!     Initialize the routine that generates the central curve
!     for the torus.
!
      CALL T_TORSET ( R, CENTER, NORMAL )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TORUS' )
         RETURN
      END IF

!
!     Generate the cross-section polygon for the torus. The polygon is
!     positively oriented about the first endpoint of the central
!     curve.
!
!     Start by generating a first vertex on the polygon. V0 is a point
!     on the central curve.
!
      CALL T_TORCRV ( 0.D0, C0, D0, TWIST )

      CALL FRAME  ( D0, X, Y )

      CALL VSCL ( RCROSS, X, V0 )

!
!     Generate the rest of the vertices.
!
      CALL VEQU ( V0, VRTCES(1,1) )

      DO I = 2, NPOLYV
!
!        NPOLYV has been checked to ensure it's non-zero.
!
         THETA = (I-1) * TWOPI() / NPOLYV

         CALL VROTV ( V0, D0, THETA, VRTCES(1,I) )

      END DO

!
!     Prepare to use the rest of the VRTCES array as a cell.
!
      BASE = NPOLYV + 1

      SIZE = 3 * ( MAXV - NPOLYV - 2 )

      CALL SSIZED ( SIZE,   VRTCES(1,BASE) )
      CALL SSIZEI ( 3*MAXP, PLATES         )

!
!     Create a tube centered on the central curve. This is the
!     torus.
!
      CALL ZZPSTUBE ( NPOLYV, VRTCES,         T_TORCRV, NCROSS, &
                      .TRUE., VRTCES(1,BASE), PLATES            )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TORUS' )
         RETURN
      END IF

!
!     Get the vertex and plate counts.
!
      LNV = CARDD( VRTCES(1,BASE) ) / 3
      LNP = CARDI( PLATES         ) / 3

!
!     Create a spatial index for the plate set.
!
!     Use a heuristic formula for the fine scale.
!
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

!
!     Pick a one-size-fits-all value for the coarse scale.
!
      CORSCL = 10

!
!     Set the spatial index integer component size.
!
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ


      CALL DSKMI2 ( LNV,         VRTCES(1,BASE+2), LNP, &
                    PLATES(1,3), FINSCL,           CORSCL, &
                    WORKSZ,      VPSIZE,           VLSIZE, &
                    .FALSE.,     SPXISZ,           WORK, &
                    SPAIXD,      SPAIXI                   )

!
!     Generate bounds for the 3rd coordinate.
!
      CALL DSKRB2 ( LNV,         VRTCES(1,BASE+2), LNP, &
                    PLATES(1,3), LATSYS,           LOCPAR, &
                    MNCOR3,      MXCOR3                    )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TORUS' )
         RETURN
      END IF

!
!     Write the file.
!
      IF ( EXISTS(DSK) ) THEN
!
!        Open the existing file for writing.
!
         CALL DASOPW ( DSK, HANDLE )

      ELSE
!
!        Open a new file.
!
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


!
!     Set default inputs.
!
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      ET0    = -JYEAR() * 100
      ET1    =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, LOCPAR )

!
!     Write the file.
!
      CALL DSKW02 ( HANDLE,      BODYID,           SURFID, &
                    DCLASS,      FRNAME,           LATSYS, &
                    LOCPAR,      MNCOR1,           MXCOR1, &
                    MNCOR2,      MXCOR2,           MNCOR3, &
                    MXCOR3,      ET0,              ET1, &
                    LNV,         VRTCES(1,BASE+2), LNP, &
                    PLATES(1,3), SPAIXD,           SPAIXI       )

!
!     Close the file.
!
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_TORUS' )
      RETURN

      END
