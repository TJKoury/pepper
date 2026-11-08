C$Procedure T_WRDSK2 ( DSK test utilities, type 2 writer umbrella )

      SUBROUTINE T_WRDSK2 ( BODYID, SURFID, FRNAME, FIRST,  LAST,
     .                      A,      B,      C,      CORSYS, CORPAR,
     .                      BOUNDS, NLON,   NLAT,   NV,     NP,
     .                      USRVRT, USRPLT, MAKVTL, USEPAD, NPOLYV,
     .                      NCROSS, R,      RCROSS, CENTER, NORMAL,
     .                      DSK                                     )

C$ Abstract
C
C     This is the umbrella routine for DSK type 2 writing test
C     utilities.
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
C     DSK
C
C$ Keywords
C
C     ELLIPSOID
C     PLATE
C     TILE
C     TESSELLATE
C     TEST
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsk02.inc'


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

C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY POINTS
C     --------  ---  --------------------------------------------------
C     BODYID     I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK
C                    T_SECDS2, T_TORUS
C     SURFID     I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK
C                    T_SECDS2, T_TORUS
C     FRNAME     I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK
C                    T_SECDS2, T_TORUS
C     FIRST      I   T_WRTPLT, T_ELDSK2, T_SECDS2
C     LAST       I   T_WRTPLT, T_ELDSK2, T_SECDS2
C     A          I   T_ELLDSK, T_SECDS2
C     B          I   T_ELLDSK, T_SECDS2
C     C          I   T_ELLDSK, T_SECDS2
C     CORSYS     I   T_WRTPLT, T_ELDSK2, T_SECDS2
C     CORPAR     I   T_WRTPLT, T_ELDSK2, T_SECDS2
C     BOUNDS     I   T_WRTPLT, T_ELDSK2, T_SECDS2
C     NLON       I   T_ELDSK2, T_ELDS2Z, T_SECDS2
C     NLAT       I   T_ELDSK2, T_ELDS2Z, T_SECDS2
C     NV         I   T_WRTPLT, T_WRTPLZ
C     NP         I   T_WRTPLT, T_WRTPLZ
C     USRVRT     I   T_WRTPLT, T_WRTPLZ
C     USRPLT     I   T_WRTPLT, T_WRTPLZ
C     MAKVTL     I   T_WRTPLT, T_ELDSK2, T_SECDS2
C     USEPAD     I   T_SECDS2
C     NPOLYV     I   T_TORUS
C     NCROSS     I   T_TORUS
C     R          I   T_TORUS
C     RCROSS     I   T_TORUS
C     CENTER     I   T_TORUS
C     NORMAL     I   T_TORUS
C     DSK        I   T_WRTPLT, T_WRTPLZ, T_ELDSK2, T_ELDS2Z, T_SMLDSK,
C                    T_CG, T_SECDS2, T_TORUS
C
C$ Detailed_Input
C
C     See the entry points for descriptions of their inputs.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) is signaled.
C
C     See the entry points for descriptions of exceptions
C     specific to those routines.
C
C$ Files
C
C     See the entry points.
C
C$ Particulars
C
C     The entry points in this routine create type 2 DSKs for
C     SPICELIB regression testing.
C
C     This umbrella routine serves as a location for declaration
C     of large arrays needed for spatial index generation, and
C     in some cases, for generation of plates and vertices.
C
C     In order to take advantage of the arrays declared here, new
C     DSK generating test utilities should be implemented as new
C     entry points of this routine, if feasible.
C
C     The entry points of this routine are
C
C       T_WRTPLT     {write plates and vertices: general case}
C
C       T_WRTPLZ     {write plates and vertices: "easy"}
C
C                       This routine uses latitudinal coordinates and
C                       default values for time and spatial bounds. It
C                       does not create a vertex-plate map.
C
C       T_ELDSK2     {write tessellated ellipsoid: general case}
C
C
C       T_ELDS2Z     {write tessellated ellipsoid: "easy"}
C
C                       This routine uses latitudinal coordinates and
C                       default values for time and spatial bounds. It
C                       does not create a vertex-plate map. It obtains
C                       target body reference ellipsoid radii from the
C                       kernel pool.
C
C       T_SMLDSK     {write DSK containing a single plate}
C
C
C       T_CG         {write DSK containing extremely simplified
C                     shape representing comet C-G}
C
C       T_SECDS2     {write section of tessellated ellipsoid: general
C                     case}
C
C       T_TORUS      {write DSK containing torus plate model}
C
C$ Examples
C
C     See use of in TSPICE test families.
C
C$ Restrictions
C
C     For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.1.0, 12-OCT-2021 (JDR) (NJB)
C
C        Changed local variable C0 from scalar to an array of
C        length 3; this fixes a bug in entry point T_TORUS.
C
C        Updated umbrella and entry point headers to comply with NAIF
C        standard. Added missing arguments in T_CG entry point
C        commented declarations section.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C        Added entry point that writes simplified CG DSK.
C
C        Reduced large array sizes by a factor of 8.
C
C        Previous version was dated 21-MAR-2016.
C
C-&


C$ Index_Entries
C
C     testutil type 2 dsk writing umbrella
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      TWOPI

      INTEGER               CARDD
      INTEGER               CARDI

      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     EXTERNAL routines
C
      EXTERNAL              T_TORCRV

C
C     Local parameters
C
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
      PARAMETER           ( MXIXSZ =   IXIFIX + VOXPSZ
     .                               + VOXLSZ          )
      INTEGER               SMLISZ
      PARAMETER           ( SMLISZ = 100100 )

      INTEGER               DCLASS
      PARAMETER           ( DCLASS = 2 )

C
C     Local variables
C
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

C
C     Saved variables
C
C     Save all large arrays to avoid stack problems.
C     Save spatial index variables for the small DSK writer.
C
      SAVE                  PLATES
      SAVE                  SMLIXD
      SAVE                  SMLIXI
      SAVE                  SPAIXI
      SAVE                  VRTCES
      SAVE                  WORK

      SAVE                  SMALL1

C
C     Initial values
C
      DATA                  SMALL1 / .TRUE. /


      CALL CHKIN  ( 'T_WRDSK2' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_WRDSK2' )
      RETURN




C$Procedure T_WRTPLT ( DSK test, write caller-supplied plate data )

      ENTRY T_WRTPLT ( BODYID, SURFID, FRNAME, FIRST, LAST,
     .                 CORSYS, CORPAR, BOUNDS, NV,    NP,
     .                 USRVRT, USRPLT, MAKVTL, DSK         )

C$ Abstract
C
C     Write a type 2 DSK containing vertices and plates passed
C     in by the caller.
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
C     DSK
C
C$ Keywords
C
C     PLATE
C     TEST
C     UTILITY
C     VERTEX
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     DOUBLE PRECISION      FIRST
C     DOUBLE PRECISION      LAST
C     INTEGER               CORSYS
C     DOUBLE PRECISION      CORPAR ( * )
C     DOUBLE PRECISION      BOUNDS ( 2, 2 )
C     INTEGER               NV
C     INTEGER               NP
C     DOUBLE PRECISION      USRVRT ( 3, * )
C     INTEGER               USRPLT ( 3, * )
C     LOGICAL               MAKVTL
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     FIRST      I   Start time, TDB.
C     LAST       I   Stop time, TDB.
C     CORSYS     I   Coordinate system code.
C     CORPAR     I   Coordinate parameters.
C     BOUNDS     I   Segment spatial bounds.
C     NV         I   Number of vertices in input array.
C     NP         I   Number of plates in input array.
C     USRVRT     I   Vertex array.
C     USRPLT     I   Plate array.
C     MAKVTL     I   Vertex-plate mapping flag.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C     SURFID   is surface ID code.
C
C     FRNAME   is the name of the reference frame.
C
C     FIRST,
C     LAST     are, respectively, the start and stop times of
C              the coverage interval of the segment. FIRST and
C              LAST are expressed as seconds past J2000, TDB.
C
C     CORSYS   is an integer code designating the coordinate
C              system of the segment.
C
C
C     CORPAR   is an array of parameters associated with
C              the coordinate system.
C
C              For planetodetic coordinates,
C
C                  CORPAR(1) is the equatorial radius of the
C                            reference spheroid
C
C
C                  CORPAR(2) is the flattening coefficient of the
C                            reference spheroid
C
C
C     BOUNDS   is a 2x2 array of bounds of the first two coordinates
C              of the segment's spatial coverage. BOUNDS(1,I) is
C              the lower bound of the Ith coordinate; BOUNDS(2,I) is
C              the upper bound.
C
C     NV       is the number of vertices in the input vertex array.
C
C     NP       is the number of plates in the input plate array.
C
C     USRVRT   is an array of vertices. Elements
C
C                  USRVRT(J,I), J = 1, 3
C
C              are the components of the Ith vertex. Vertex indices
C              range from 1 to NV.
C
C
C     USRPLT   is an array of plates. Elements
C
C                  USRPLT(J,I), J = 1, 3
C
C              are the components of the Ith plate. Each component
C              is a vertex index. Plate indices range from 1 to NP.
C
C     MAKVTL   is a logical flag that controls whether a vertex-plate
C              mapping is created in the output segment. A mapping
C              is created if and only if MAKVTL is .TRUE.
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK will be
C         signaled by routines in the call tree of this routine.
C
C$ Files
C
C     See the description of DSK above.
C
C$ Particulars
C
C     This is the general test API for writing plate-vertex
C     data to a DSK file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     testutil type 2 dsk plate-vertex writer
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_WRTPLT' )

C
C     Convert the frame name to an ID code.
C
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an '
     .   //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_WRTPLT'                             )
         RETURN

      END IF

C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the large scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
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

      CALL DSKMI2 ( NV,     USRVRT, NP,     USRPLT, FINSCL,
     .              CORSCL, WORKSZ, VPSIZE, VLSIZE, MAKVTL,
     .              SPXISZ, WORK,   SPAIXD, SPAIXI         )

C
C     Generate bounds for the 3rd coordinate.
C
      CALL DSKRB2 ( NV,     USRVRT, NP,     USRPLT,
     .              CORSYS, CORPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_WRTPLT' )
         RETURN
      END IF

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID,
     .              DCLASS,      FRNAME,      CORSYS,
     .              CORPAR,      BOUNDS(1,1), BOUNDS(2,1),
     .              BOUNDS(1,2), BOUNDS(2,2), MNCOR3,
     .              MXCOR3,      FIRST,       LAST,
     .              NV,          USRVRT,      NP,
     .              USRPLT,      SPAIXD,      SPAIXI      )

C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_WRTPLT' )
      RETURN






C$Procedure T_WRTPLZ ( DSK test, write plate data, easy )

      ENTRY T_WRTPLZ ( BODYID, SURFID, FRNAME, NV,
     .                 NP,     USRVRT, USRPLT, DSK )

C$ Abstract
C
C     Write a type 2 DSK containing vertices and plates passed
C     in by the caller. Use default values to simplify the call.
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
C     DSK
C
C$ Keywords
C
C     PLATE
C     TEST
C     UTILITY
C     VERTEX
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     INTEGER               NV
C     INTEGER               NP
C     DOUBLE PRECISION      USRVRT ( 3, * )
C     INTEGER               USRPLT ( 3, * )
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     NV         I   Number of vertices in input array.
C     NP         I   Number of plates in input array.
C     USRVRT     I   Vertex array.
C     USRPLT     I   Plate array.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C     SURFID   is surface ID code.
C
C     FRNAME   is the name of the reference frame.
C
C     NV       is the number of vertices in the input vertex array.
C
C     NP       is the number of plates in the input plate array.
C
C     USRVRT   is an array of vertices. Elements
C
C                 USRVRT(J,I), J = 1, 3
C
C              are the components of the Ith vertex. Vertex indices
C              range from 1 to NV.
C
C
C     USRPLT   is an array of plates. Elements
C
C                 USRPLT(J,I), J = 1, 3
C
C              are the components of the Ith plate. Each component
C              is a vertex index. Plate indices range from 1 to NP.
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK will be
C         signaled by routines in the call tree of this routine.
C
C$ Files
C
C     See the description of DSK above.
C
C$ Particulars
C
C     This is a simplified API for writing plate-vertex
C     data to a DSK file. The following defaults are used
C
C        Time bounds:               -100 : 100 Julian years TDB
C
C        Coordinate system:         Latitudinal
C
C        Longitude bounds:          0     : 2*pi radians
C        Latitude bounds:           -pi/2 : pi/2 radians
C
C     This routine does not construct a vertex-plate mapping.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     testutil type 2 dsk plate-vertex easy writer
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_WRTPLZ' )

C
C     Convert the frame name to an ID code.
C
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an '
     .   //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_WRTPLZ'                             )
         RETURN

      END IF

C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the large scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ

      CALL DSKMI2 ( NV,     USRVRT, NP,     USRPLT, FINSCL,
     .              CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .FALSE.,
     .              SPXISZ, WORK,   SPAIXD, SPAIXI          )

C
C     Generate bounds for the 3rd coordinate.
C
      CALL DSKRB2 ( NV,     USRVRT, NP,     USRPLT,
     .              LATSYS, LOCPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_WRTPLZ' )
         RETURN
      END IF

C
C     Set default segment parameters.
C
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      CALL CLEARD ( NSYPAR, LOCPAR )

      ET0 = -100 * JYEAR()
      ET1 =  100 * JYEAR()

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID,
     .              DCLASS,      FRNAME,      LATSYS,
     .              LOCPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,
     .              MXCOR3,      ET0,         ET1,
     .              NV,          USRVRT,      NP,
     .              USRPLT,      SPAIXD,      SPAIXI )

C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_WRTPLZ' )
      RETURN






C$Procedure T_ELDSK2 ( DSK test, write tessellated ellipsoid DSK )

      ENTRY T_ELDSK2 ( BODYID, SURFID, FRNAME, FIRST,  LAST,
     .                 CORSYS, CORPAR, BOUNDS, A,      B,
     .                 C,      NLON,   NLAT,   MAKVTL, DSK  )

C$ Abstract
C
C     Write a type 2 DSK containing a tessellated ellipsoid.
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
C     DSK
C
C$ Keywords
C
C     ELLIPSOID
C     PLATE
C     TILE
C     TESSELLATE
C     TEST
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     DOUBLE PRECISION      FIRST
C     DOUBLE PRECISION      LAST
C     INTEGER               CORSYS
C     DOUBLE PRECISION      CORPAR ( * )
C     DOUBLE PRECISION      BOUNDS ( 2, 2 )
C     DOUBLE PRECISION      A
C     DOUBLE PRECISION      B
C     DOUBLE PRECISION      C
C     INTEGER               NLON
C     INTEGER               NLAT
C     LOGICAL               MAKVTL
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     FIRST      I   Start time, TDB.
C     LAST       I   Stop time, TDB.
C     CORSYS     I   Coordinate system code.
C     CORPAR     I   Coordinate parameters.
C     BOUNDS     I   Segment spatial bounds.
C     A          I   Length of ellipsoid semi-axis lying on the x-axis.
C     B          I   Length of ellipsoid semi-axis lying on the y-axis.
C     C          I   Length of ellipsoid semi-axis lying on the z-axis.
C     NLON       I   Number of longitude bands in plate set.
C     NLAT       I   Number of latitude bands in plate set.
C     MAKVTL     I   Vertex-plate mapping flag.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C     SURFID   is surface ID code.
C
C     FRNAME   is the name of the reference frame.
C
C     FIRST,
C     LAST     are, respectively, the start and stop times of
C              the coverage interval of the segment. FIRST and
C              LAST are expressed as seconds past J2000, TDB.
C
C     CORSYS   is an integer code designating the coordinate
C              system of the segment.
C
C
C     CORPAR   is an array of parameters associated with
C              the coordinate system.
C
C              For planetodetic coordinates,
C
C                 CORPAR(1) is the equatorial radius of the
C                           reference spheroid
C
C
C                 CORPAR(2) is the flattening coefficient of the
C                           reference spheroid
C
C
C     BOUNDS   is a 2x2 array of bounds of the first two coordinates
C              of the segment's spatial coverage. BOUNDS(1,I) is
C              the lower bound of the Ith coordinate; BOUNDS(2,I) is
C              the upper bound.
C
C     A,
C     B,
C     C        are the lengths of the semi-axes of a triaxial
C              ellipsoid. The ellipsoid is centered at the origin and
C              oriented so that its axes lie on the x, y and z axes.
C              A, B, and C are the lengths of the semi-axes that
C              point in the x, y, and z directions respectively.
C
C
C     NLON     is the number of longitude bands in the plate set to
C              be created by this routine. Each longitude band is
C              bounded by two meridians. All longitude bands have
C              equal angular extent in longitude. The vertices of any
C              plate lie on adjacent longitude band boundaries.
C
C
C     NLAT     is the number of latitude bands in the plate set. The
C              vertices of each band are bounded by two cones of
C              constant planetocentric latitude. All latitude bands
C              have equal angular extent in planetocentric latitude.
C              The vertices of any plate lie on adjacent latitude
C              band boundaries.
C
C              Each polar "cap" consists of one longitude band.
C
C
C     MAKVTL   is a logical flag that controls whether a vertex-plate
C              mapping is created in the output segment. A mapping
C              is created if and only if MAKVTL is .TRUE.
C
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK will be
C         signaled by routines in the call tree of this routine.
C
C     3)  If the length of any semi-axis of the ellipsoid is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
C
C$ Files
C
C     See the description of DSK above.
C
C$ Particulars
C
C     This routine creates a DSK containing a single type 2 segment.
C     The plate and vertex data in the segment represent a tessellated
C     ellipsoid.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     write type 2 dsk containing tessellated ellipsoid
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ELDSK2' )

C
C     Convert the frame name to an ID code.
C
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an '
     .   //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_ELDSK2'                             )
         RETURN

      END IF

C
C     Create vertices and plates.
C
      CALL ZZELLPLT ( A,    B,   C,      NLON, NLAT,  MAXV,
     .                MAXP, LNV, VRTCES, LNP,  PLATES       )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDSK2' )
         RETURN
      END IF

C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the coarse scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
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

      CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL,
     .              CORSCL, WORKSZ, VPSIZE, VLSIZE, MAKVTL,
     .              SPXISZ, WORK,   SPAIXD, SPAIXI          )

C
C     Generate bounds for the 3rd coordinate.
C
      CALL DSKRB2 ( LNV,    VRTCES, LNP,    PLATES,
     .              CORSYS, CORPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDSK2' )
         RETURN
      END IF

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID,
     .              DCLASS,      FRNAME,      CORSYS,
     .              CORPAR,      BOUNDS(1,1), BOUNDS(2,1),
     .              BOUNDS(1,2), BOUNDS(2,2), MNCOR3,
     .              MXCOR3,      FIRST,       LAST,
     .              LNV,         VRTCES,      LNP,
     .              PLATES,      SPAIXD,      SPAIXI      )

C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_ELDSK2' )
      RETURN






C$Procedure T_ELDS2Z ( write tessellated ellipsoid type 2 DSK, easy )

      ENTRY T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK )

C$ Abstract
C
C     Write a type 2 DSK containing a tessellated ellipsoid.
C     Use default values to simplify the call.
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
C     DSK
C
C$ Keywords
C
C     ELLIPSOID
C     PLATE
C     TILE
C     TESSELLATE
C     TEST
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     INTEGER               NLON
C     INTEGER               NLAT
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     NLON       I   Number of longitude bands in plate set.
C     NLAT       I   Number of latitude bands in plate set.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C
C     SURFID   is surface ID code.
C
C
C     FRNAME   is the name of the reference frame.
C
C
C     NLON     is the number of longitude bands in the plate set to
C              be created by this routine. Each longitude band is
C              bounded by two meridians. All longitude bands have
C              equal angular extent in longitude. The vertices of any
C              plate lie on adjacent longitude band boundaries.
C
C
C     NLAT     is the number of latitude bands in the plate set. The
C              vertices of each band are bounded by two cones of
C              constant planetocentric latitude. All latitude bands
C              have equal angular extent in planetocentric latitude.
C              The vertices of any plate lie on adjacent latitude
C              band boundaries.
C
C              Each polar "cap" consists of one longitude band.
C
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK will be
C         signaled by routines in the call tree of this routine.
C
C     3)  If the length of any semi-axis of the ellipsoid is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
C
C$ Files
C
C     Input: a PCK file must be loaded in order to provide radii of
C            the reference ellipsoid for the specified body.
C
C     Output: see the description of DSK above.
C
C$ Particulars
C
C     This is a simplified API for writing a tessellated ellipsoid
C     model to a DSK file. The following defaults are used
C
C        Time bounds:               -100 : 100 Julian years TDB
C
C        Coordinate system:         Latitudinal
C
C        Longitude bounds:          0     : 2*pi radians
C        Latitude bounds:           -pi/2 : pi/2 radians
C
C     This routine does not construct a vertex-plate mapping.
C
C     Ellipsoid radii for the specified body are obtained from the
C     kernel pool.
C
C     This routine creates a DSK containing a single type 2 segment.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     write type 2 DSK containing tessellated ellipsoid, easy
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ELDS2Z' )

C
C     Convert the frame name to an ID code.
C
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an '
     .   //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_ELDS2Z'                             )
         RETURN

      END IF

C
C     Get target body radii from the kernel pool.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDS2Z' )
         RETURN
      END IF

C
C     Create vertices and plates.
C
      CALL ZZELLPLT ( RADII(1), RADII(2), RADII(3), NLON,
     .                NLAT,     MAXV,     MAXP,     LNV,
     .                VRTCES,   LNP,      PLATES         )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDS2Z' )
         RETURN
      END IF

C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the coarse scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ

      CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL,
     .              CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .FALSE.,
     .              SPXISZ, WORK,   SPAIXD, SPAIXI           )

C
C     Generate bounds for the 3rd coordinate.
C
      CALL DSKRB2 ( LNV,    VRTCES, LNP,    PLATES,
     .              LATSYS, LOCPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_ELDS2Z' )
         RETURN
      END IF

C
C     Set default inputs.
C
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      ET0    = -JYEAR() * 100
      ET1    =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, LOCPAR )

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID,
     .              DCLASS,      FRNAME,      LATSYS,
     .              LOCPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,
     .              MXCOR3,      ET0,         ET1,
     .              LNV,         VRTCES,      LNP,
     .              PLATES,      SPAIXD,      SPAIXI )

C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_ELDS2Z' )
      RETURN



C$Procedure T_SMLDSK ( write small DSK )

      ENTRY T_SMLDSK ( BODYID, SURFID, FRNAME, DSK )

C$ Abstract
C
C     Write a trivial type 2 DSK containing one plate.
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
C     DSK
C
C$ Keywords
C
C     PLATE
C     TEST
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C     SURFID   is surface ID code.
C
C     FRNAME   is the name of the reference frame.
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C              The DSK is file is unsegregated.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK will be
C         signaled by routines in the call tree of this routine.
C
C$ Files
C
C     Output: see the description of DSK above.
C
C$ Particulars
C
C     This is a simplified API for quickly writing small DSK file. The
C     file contains one type 2 segment; the segment contains a single
C     plate.
C
C     The output file is unsegregated.
C
C     Files created by this routine are suitable for testing
C     load/unload operations.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     write small type 2 dsk
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SMLDSK' )

C
C     Convert the frame name to an ID code.
C
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an '
     .   //            'ID code.'                             )
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

C
C     Set default inputs.
C
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

         CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL,
     .                 CORSCL, WORKSZ, VPSIZE, VLSIZE, .FALSE.,
     .                 SPXISZ, WORK,   SMLIXD, SMLIXI           )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_SMLDSK' )
            RETURN
         END IF

         SMALL1 = .FALSE.

      END IF

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID,
     .              DCLASS,      FRNAME,      LATSYS,
     .              LOCPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,
     .              MXCOR3,      ET0,         ET1,
     .              LNV,         VRTCES,      LNP,
     .              PLATES,      SMLIXD,      SMLIXI )
C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .FALSE. )

      CALL CHKOUT ( 'T_SMLDSK' )
      RETURN




C$Procedure T_CG ( write simplified comet C-G DSK )

      ENTRY T_CG ( BODYID, SURFID, FRNAME, DSK )

C$ Abstract
C
C     Write a DSK containing an extremely simplified shape model
C     representing the nucleus of comet Churyumov-Gerasimenko. The
C     model is a union of boxes.
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
C     DSK
C
C$ Keywords
C
C     PLATE
C     TEST
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C     SURFID   is surface ID code.
C
C     FRNAME   is the name of the reference frame.
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK is
C         signaled by a routine in the call tree of this routine.
C
C$ Files
C
C     Output: see the description of DSK above.
C
C$ Particulars
C
C     This routine writes a DSK that contains a representation
C     of the shape similar to that of comet Churyumov-Gerasimenko.
C     The model is composed of several boxes.
C
C     The output file contains a single type 2 segment.
C
C     The ID codes in the output file are provided by the user.
C     The user may wish to center the segment on a body other
C     than comet C-G.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
C
C        Edited header to comply with NAIF standard. Added missing
C        arguments in the declarations section commented area.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     write comet cg-like dsk
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_CG' )

C
C     Treat the local vertex and plate arrays as cells,
C     since the API of ZZT_CG requires this.
C
      LSIZEV = 100
      LSIZEP = 200

      CALL SSIZED ( LSIZEV, VRTCES )
      CALL SSIZEI ( LSIZEP, PLATES )

C
C     Create vertices and plates. In each array, data
C     start at 1-dimensional index LBCELL+1. This
C     location coincides with the 2-dimensional index (1,3).
C
      CALL ZZT_CG ( VRTCES, PLATES )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CG' )
         RETURN
      END IF

      LNV = CARDD ( VRTCES )  / 3
      LNP = CARDI ( PLATES )  / 3

C
C     Create a spatial index for the plate set.
C
      FINSCL = 1.D0

C
C     We can use a small coarse scale.
C
      CORSCL = 1

C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ

      CALL DSKMI2 ( LNV,         VRTCES(1,3),    LNP,
     .              PLATES(1,3), FINSCL,         CORSCL,
     .              WORKSZ,      VOXPSZ, VOXLSZ, .FALSE.,
     .              SPXISZ,      WORK,   SPAIXD, SPAIXI   )

C
C     Generate bounds for the 3rd coordinate.
C
      CALL DSKRB2 ( LNV,         VRTCES(1,3), LNP,
     .              PLATES(1,3), LATSYS,      LOCPAR,
     .              MNCOR3,      MXCOR3              )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CG' )
         RETURN
      END IF

C
C     Set default inputs.
C
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      ET0    = -JYEAR() * 100
      ET1    =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, LOCPAR )

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID,
     .              DCLASS,      FRNAME,      LATSYS,
     .              LOCPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,
     .              MXCOR3,      ET0,         ET1,
     .              LNV,         VRTCES(1,3), LNP,
     .              PLATES(1,3), SPAIXD,      SPAIXI )

C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .TRUE. )


      CALL CHKOUT ( 'T_CG' )
      RETURN




C$Procedure T_SECDS2 ( DSK test, write ellipsoid section DSK )

      ENTRY T_SECDS2 ( BODYID, SURFID, FRNAME, FIRST,  LAST,
     .                 CORSYS, CORPAR, BOUNDS, A,      B,
     .                 C,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                 DSK                                     )

C$ Abstract
C
C     Write a type 2 DSK containing a section of a tessellated
C     ellipsoid.
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
C     DSK
C
C$ Keywords
C
C     ELLIPSOID
C     PLATE
C     TILE
C     TESSELLATE
C     TEST
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     DOUBLE PRECISION      FIRST
C     DOUBLE PRECISION      LAST
C     INTEGER               CORSYS
C     DOUBLE PRECISION      CORPAR ( * )
C     DOUBLE PRECISION      BOUNDS ( 2, 2 )
C     DOUBLE PRECISION      A
C     DOUBLE PRECISION      B
C     DOUBLE PRECISION      C
C     INTEGER               NLON
C     INTEGER               NLAT
C     LOGICAL               MAKVTL
C     LOGICAL               USEPAD
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     FIRST      I   Start time, TDB.
C     LAST       I   Stop time, TDB.
C     CORSYS     I   Coordinate system code.
C     CORPAR     I   Coordinate parameters.
C     BOUNDS     I   Segment spatial bounds.
C     A          I   Length of ellipsoid semi-axis lying on the x-axis.
C     B          I   Length of ellipsoid semi-axis lying on the y-axis.
C     C          I   Length of ellipsoid semi-axis lying on the z-axis.
C     NLON       I   Number of longitude bands in plate set.
C     NLAT       I   Number of latitude bands in plate set.
C     MAKVTL     I   Vertex-plate mapping flag.
C     USEPAD     I   Padding flag.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C     SURFID   is surface ID code.
C
C     FRNAME   is the name of the reference frame.
C
C     FIRST,
C     LAST     are, respectively, the start and stop times of
C              the coverage interval of the segment. FIRST and
C              LAST are expressed as seconds past J2000, TDB.
C
C     CORSYS   is an integer code designating the coordinate
C              system of the segment. Limited to planetocentric
C              coordinates for full functionality. Planetodetic
C              coordinates may be used if padding generation is
C              not invoked.
C
C
C     CORPAR   is an array of parameters associated with
C              the coordinate system.
C
C              For planetodetic coordinates,
C
C                 CORPAR(1) is the equatorial radius of the
C                           reference spheroid
C
C
C                 CORPAR(2) is the flattening coefficient of the
C                           reference spheroid
C
C
C     BOUNDS   is a 2x2 array of bounds of the first two coordinates
C              of the segment's spatial coverage. BOUNDS(1,I) is
C              the lower bound of the Ith coordinate; BOUNDS(2,I) is
C              the upper bound.
C
C     A,
C     B,
C     C        are the lengths of the semi-axes of a triaxial
C              ellipsoid. The ellipsoid is centered at the origin and
C              oriented so that its axes lie on the x, y and z axes.
C              A, B, and C are the lengths of the semi-axes that
C              point in the x, y, and z directions respectively.
C
C
C     NLON     is the number of longitude bands in the plate set to
C              be created by this routine. Each longitude band is
C              bounded by two meridians. All longitude bands have
C              equal angular extent in longitude. The vertices of any
C              plate lie on adjacent longitude band boundaries.
C
C              If padding is enabled, an extra longitude band will be
C              created at both longitude boundaries of the segment.
C
C
C     NLAT     is the number of latitude bands in the plate set. The
C              vertices of each band are bounded by two cones of
C              constant planetocentric latitude. All latitude bands
C              have equal angular extent in planetocentric latitude.
C              The vertices of any plate lie on adjacent latitude
C              band boundaries.
C
C              Each polar "cap" consists of one longitude band.
C
C              If padding is enabled, and if the segment does not
C              span the X-Y plane, one or more extra latitude bands
C              will be created at the segment latitude boundary
C              closest to the X-Y plane. A single extra latitude band
C              will be created at the other latitude boundary. If the
C              segment spans the X-Y plane, then a single extra
C              latitude bound will be created at each latitude
C              boundary.
C
C
C     MAKVTL   is a logical flag that controls whether a vertex-plate
C              mapping is created in the output segment. A mapping
C              is created if and only if MAKVTL is .TRUE.
C
C
C     USEPAD   is a logical flag indicating whether segment padding
C              is to be created. When USEPAD is .TRUE., sections
C              having positive lower latitude bounds are padded at
C              the bottom to ensure coverage. This padding consists
C              of an integral number of extra latitude bands. Each of
C              the other boundaries are padded with a single latitude
C              or longitude band. When USEPAD is .FALSE., no padding
C              is created.
C
C              See Restrictions below.
C
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK will be
C         signaled by routines in the call tree of this routine.
C
C     3)  If the length of any semi-axis of the ellipsoid is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
C
C     4)  If either the longitude or latitude band count is
C         non-positive, the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     5)  If the input latitude bounds are out of order, the error
C         SPICE(BOUNDSOUTOFORDER) is signaled.
C
C$ Files
C
C     See the description of DSK above.
C
C$ Particulars
C
C     This routine creates a DSK containing a single type 2 segment.
C     The plate and vertex data in the segment represent a section of a
C     tessellated ellipsoid. The section covers a specified longitude/
C     latitude rectangle.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C     2) Currently restricted to latitudinal coordinates for full
C        functionality. Planetodetic and rectangular coordinates may be
C        used if padding generation is not commanded.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.0.1, 07-APR-2020 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     write type 2 dsk containing ellipsoid section
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SECDS2' )

C
C     Check coordinate system.
C
      IF (       ( CORSYS .NE. LATSYS )
     .     .AND. ( CORSYS .NE. PDTSYS )
     .     .AND. ( CORSYS .NE. RECSYS )  ) THEN

         CALL SETMSG ( 'Input coordinate system code was #. '
     .   //            'Only latitudinal, planetodetic, and '
     .   //            'rectangular coordinates are '
     .   //            'currently supported by this routine.' )
         CALL ERRINT ( '#', CORSYS                            )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                  )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

C
C     Convert the frame name to an ID code.
C
      CALL NAMFRM ( FRNAME, FRAMID )

      IF ( FRAMID .EQ. 0 ) THEN

         CALL SETMSG ( 'Could not convert frame name # to an '
     .   //            'ID code.'                             )
         CALL ERRCH  ( '#', FRNAME                            )
         CALL SIGERR ( 'SPICE(FRAMEIDNOTFOUND)'               )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

C
C     Check band counts.
C
      IF ( NLAT .LT. 1 ) THEN

         CALL SETMSG ( 'Latitude band count must be positive '
     .   //            'but was #.'                           )
         CALL ERRINT ( '#', NLAT                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'               )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

      IF ( NLON .LT. 1 ) THEN

         CALL SETMSG ( 'Longitude band count must be positive '
     .   //            'but was #.'                           )
         CALL ERRINT ( '#', NLON                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'               )
         CALL CHKOUT ( 'T_SECDS2'                             )
         RETURN

      END IF

C
C     Reject invalid latitude bounds.
C
      IF ( BOUNDS(2,2) .LT. BOUNDS(1,2) ) THEN

         CALL SETMSG ( 'Latitude bounds are out of order: bounds '
     .   //            'are #:#.'                                 )
         CALL ERRDP  ( '#',  BOUNDS(1,2)                          )
         CALL ERRDP  ( '#',  BOUNDS(2,2)                          )
         CALL SIGERR ( 'SPICE(BOUNDSOUTOFORDER)'                  )
         CALL CHKOUT ( 'T_SECDS2'                                 )
         RETURN

      END IF

C
C     Make local copies of the band counts.
C
      LNLON = NLON
      LNLAT = NLAT

C
C     Create vertices and plates.
C
      IF ( USEPAD ) THEN

         IF ( CORSYS .NE. LATSYS ) THEN

            CALL SETMSG ( 'Padding generation was requested for '
     .      //            'coordinate system #. This service is '
     .      //            'available only for planetocentric '
     .      //            'coordinates.'                         )
            CALL ERRINT ( '#', CORSYS                            )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                  )
            CALL CHKOUT ( 'T_SECDS2'                             )
            RETURN

         END IF

C
C        Determine sizes of longitude and latitude bands.
C
         MINLAT = BOUNDS(1,2)
         MAXLAT = BOUNDS(2,2)

         DLAT   = ( MAXLAT - MINLAT ) / NLAT

         MINLON = BOUNDS(1,1)
         MAXLON = BOUNDS(2,1)

         IF ( MAXLON .LE. MINLON ) THEN
            MAXLON = MAXLON + TWOPI()
         END IF

         DLON   = ( MAXLON - MINLON ) / NLON

C
C        Determine the latitude padding.
C
         IF ( MAXLAT .GE. 0.D0 ) THEN
C
C           Add just one upper band, if it will fit. If not,
C           don't create a smaller band; just omit the band.
C
            IF (  ( MAXLAT+DLAT ) .LT. HALFPI() ) THEN

               MAXLAT = MAXLAT + DLAT
               LNLAT  = LNLAT  + 1

            END IF

         ELSE
C
C           Determine the adjusted latitude needed for full
C           latitude coverage.
C
            CALL ZZCHRLAT ( MAXLAT, DLON, ADJLAT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SECDS2' )
               RETURN
            END IF

            DELTA = ADJLAT - MAXLAT

C
C           Determine the number of latitude bands that must
C           be added to give the required coverage.
C
            Q     = DELTA / DLAT

            IF ( Q .GT. AINT(Q) ) THEN
C
C              Q has a non-zero fractional part.
C
               Q = AINT(Q) + 1.D0

            END IF

            MAXLAT = MAXLAT + Q*DLAT
            LNLAT  = LNLAT  + INT(Q)

         END IF
C
C        MAXLAT and LNLAT have been updated to account for
C        upper padding, if it has been added.
C

         IF ( MINLAT .LE. 0.D0 ) THEN
C
C           Add just one lower band, if it will fit. If not,
C           don't create a smaller band; just omit the band.
C
            IF (  ( MINLAT-DLAT ) .GT. -HALFPI() ) THEN

               MINLAT = MINLAT - DLAT
               LNLAT  = LNLAT  + 1

            END IF

         ELSE
C
C           Determine the adjusted latitude needed for full
C           latitude coverage.
C
            CALL ZZCHRLAT ( MINLAT, DLON, ADJLAT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SECDS2' )
               RETURN
            END IF

            DELTA = MINLAT - ADJLAT

C
C           Determine the number of latitude bands that must
C           be added to give the required coverage.
C
            Q     = DELTA / DLAT

            IF ( Q .GT. AINT(Q) ) THEN
C
C              Q has a non-zero fractional part.
C
               Q = AINT(Q) + 1.D0

            END IF

            MINLAT = MINLAT - Q*DLAT
            LNLAT  = LNLAT  + INT(Q)

         END IF
C
C        MAXLAT and LNLAT have been updated to account for
C        lower padding, if it has been added.
C

C
C        Adjust the longitude boundaries and band count to
C        reflect longitude padding. Padded is added only if
C        the padded section would have 2*pi longitude extent or
C        less.
C
         IF ( (MAXLON - MINLON) .LT. (TWOPI()-2*DLON) ) THEN
C
C           Add longitude padding.
C
            MINLON = MINLON - DLON
            MAXLON = MAXLON + DLON
            LNLON  = LNLON  + 2

         END IF

C
C        Create a vertex and plate set that includes padding.
C
         CALL ZZELLSEC ( A,           B,           C,
     .                   MINLON,      MAXLON,      MINLAT,
     .                   MAXLAT,      LNLON,       LNLAT,
     .                   MAXV,        MAXP,        LNV,
     .                   VRTCES,      LNP,         PLATES     )

      ELSE
C
C        Create a vertex and plate set only for the specified region.
C
         CALL ZZELLSEC ( A,           B,           C,
     .                   BOUNDS(1,1), BOUNDS(2,1), BOUNDS(1,2),
     .                   BOUNDS(2,2), NLON,        NLAT,
     .                   MAXV,        MAXP,        LNV,
     .                   VRTCES,      LNP,         PLATES     )

      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_SECDS2' )
         RETURN
      END IF

C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the coarse scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
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

      CALL DSKMI2 ( LNV,    VRTCES, LNP,    PLATES, FINSCL,
     .              CORSCL, WORKSZ, VPSIZE, VLSIZE, MAKVTL,
     .              SPXISZ, WORK,   SPAIXD, SPAIXI          )

C
C     Generate bounds for the 3rd coordinate.
C
      CALL DSKRB2 ( LNV,    VRTCES, LNP,    PLATES,
     .              CORSYS, CORPAR, MNCOR3, MXCOR3  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_SECDS2' )
         RETURN
      END IF

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


      CALL DSKW02 ( HANDLE,      BODYID,      SURFID,
     .              DCLASS,      FRNAME,      CORSYS,
     .              CORPAR,      BOUNDS(1,1), BOUNDS(2,1),
     .              BOUNDS(1,2), BOUNDS(2,2), MNCOR3,
     .              MXCOR3,      FIRST,       LAST,
     .              LNV,         VRTCES,      LNP,
     .              PLATES,      SPAIXD,      SPAIXI      )

C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_SECDS2' )
      RETURN





C$Procedure T_TORUS ( write DSK containing torus plate model )

      ENTRY T_TORUS ( BODYID, SURFID, FRNAME, NPOLYV, NCROSS,
     .                R,      RCROSS, CENTER, NORMAL, DSK    )

C$ Abstract
C
C     Write a DSK containing a plate model representing a torus.
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
C     DSK
C
C$ Keywords
C
C     PLATE
C     TEST
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BODYID
C     INTEGER               SURFID
C     CHARACTER*(*)         FRNAME
C     INTEGER               NPOLYV
C     INTEGER               NCROSS
C     DOUBLE PRECISION      R
C     DOUBLE PRECISION      RCROSS
C     DOUBLE PRECISION      CENTER ( 3 )
C     DOUBLE PRECISION      NORMAL ( 3 )
C     CHARACTER*(*)         DSK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID.
C     SURFID     I   Surface ID.
C     FRNAME     I   Reference frame name.
C     NPOLYV     I   Number of vertices in polygonal cross section.
C     NCROSS     I   Number of cross sections.
C     R          I   Radius of central curve.
C     RCROSS     I   Radius of cross section.
C     CENTER     I   Center of the torus.
C     NORMAL     I   Normal vector to plane of central curve.
C     DSK        I   Name of DSK file to write.
C
C$ Detailed_Input
C
C     BODYID   is the body ID code of the segment to be written
C              to the output DSK file.
C
C     SURFID   is surface ID code.
C
C     FRNAME   is the name of the reference frame.
C
C     NPOLYV   is the number of vertices per polygonal cross section
C              of the torus. Each cross section is the set of
C              vertices of a regular polygon inscribed in a circle of
C              radius RCROSS (see below). Each cross section lies in
C              a plane normal to the derivative of the central curve
C              at its point of intersection with the plane.
C
C     NCROSS   is the number of cross sections of the torus.
C
C     R        is the radius of the central curve of the torus.
C              Each cross section is centered on this curve and
C              is normal to the derivative of the curve at the point
C              of intersection.
C
C     RCROSS   is the radius of each circular cross section of the
C              torus.
C
C     CENTER   is the center of the torus. This is the center of the
C              central curve of the torus. The line normal to the
C              plane of the central curve and passing through CENTER
C              is the axis of azimuthal symmetry of the torus.
C
C     NORMAL   is a vector normal to the plane containing the
C              central curve of the torus.
C
C     DSK      is the name of the DSK file to write. If the file
C              does not exist, it will be created. If the file
C              exists, this routine will append to it.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name cannot be converted to an ID code,
C         the error SPICE(FRAMEIDNOTFOUND) is signaled.
C
C     2)  Any errors that occur during the process of creating the
C         output segment's spatial index or of writing the DSK will be
C         signaled by routines in the call tree of this routine.
C
C$ Files
C
C     Output: see the description of DSK above.
C
C$ Particulars
C
C     In a Cartesian coordinate system having the Z axis parallel to
C     NORMAL, and having an origin coincident with CENTER, the torus
C     is the set of points
C
C                                2   2  1/2  2     2           2
C        { (x,y,z) : (  R  -  ( x + y  )    )  +  z   =  RCROSS  }
C
C
C     This routine can be used to create shape models suitable for
C     testing limb finding and terminator finding routines.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TESTUTIL Version 1.1.0, 12-OCT-2021 (JDR) (NJB)
C
C        Bug fix: call to T_TORCRV writes a double precision
C        array of length 3 to the second argument, but a scalar
C        second argument had been passed. Now fixed.
C
C        Edited the header to comply with NAIF standard.
C
C-    TESTUTIL Version 1.0.0, 28-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     write dsk containing torus plate model
C
C-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TORUS' )

      IF ( NPOLYV .LT. 3 ) THEN

         CALL SETMSG ( 'Polygon side count # must be at least '
     .   //            'three.'                                 )
         CALL ERRINT ( '#', NPOLYV                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'T_TORUS'                                )
         RETURN

      END IF

      IF ( NCROSS .LT. 3 ) THEN

         CALL SETMSG ( 'Cross section count # must be at least '
     .   //            'three.'                                 )
         CALL ERRINT ( '#', NCROSS                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'T_TORUS'                                )
         RETURN

      END IF

C
C     Initialize the routine that generates the central curve
C     for the torus.
C
      CALL T_TORSET ( R, CENTER, NORMAL )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TORUS' )
         RETURN
      END IF

C
C     Generate the cross-section polygon for the torus. The polygon is
C     positively oriented about the first endpoint of the central
C     curve.
C
C     Start by generating a first vertex on the polygon. V0 is a point
C     on the central curve.
C
      CALL T_TORCRV ( 0.D0, C0, D0, TWIST )

      CALL FRAME  ( D0, X, Y )

      CALL VSCL ( RCROSS, X, V0 )

C
C     Generate the rest of the vertices.
C
      CALL VEQU ( V0, VRTCES(1,1) )

      DO I = 2, NPOLYV
C
C        NPOLYV has been checked to ensure it's non-zero.
C
         THETA = (I-1) * TWOPI() / NPOLYV

         CALL VROTV ( V0, D0, THETA, VRTCES(1,I) )

      END DO

C
C     Prepare to use the rest of the VRTCES array as a cell.
C
      BASE = NPOLYV + 1

      SIZE = 3 * ( MAXV - NPOLYV - 2 )

      CALL SSIZED ( SIZE,   VRTCES(1,BASE) )
      CALL SSIZEI ( 3*MAXP, PLATES         )

C
C     Create a tube centered on the central curve. This is the
C     torus.
C
      CALL ZZPSTUBE ( NPOLYV, VRTCES,         T_TORCRV, NCROSS,
     .                .TRUE., VRTCES(1,BASE), PLATES            )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TORUS' )
         RETURN
      END IF

C
C     Get the vertex and plate counts.
C
      LNV = CARDD( VRTCES(1,BASE) ) / 3
      LNP = CARDI( PLATES         ) / 3

C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( LNP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the coarse scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ
      VLSIZE = VOXLSZ
      SPXISZ = MXIXSZ


      CALL DSKMI2 ( LNV,         VRTCES(1,BASE+2), LNP,
     .              PLATES(1,3), FINSCL,           CORSCL,
     .              WORKSZ,      VPSIZE,           VLSIZE,
     .              .FALSE.,     SPXISZ,           WORK,
     .              SPAIXD,      SPAIXI                   )

C
C     Generate bounds for the 3rd coordinate.
C
      CALL DSKRB2 ( LNV,         VRTCES(1,BASE+2), LNP,
     .              PLATES(1,3), LATSYS,           LOCPAR,
     .              MNCOR3,      MXCOR3                    )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_TORUS' )
         RETURN
      END IF

C
C     Write the file.
C
      IF ( EXISTS(DSK) ) THEN
C
C        Open the existing file for writing.
C
         CALL DASOPW ( DSK, HANDLE )

      ELSE
C
C        Open a new file.
C
         CALL DSKOPN ( DSK, DSK, 0, HANDLE )

      END IF


C
C     Set default inputs.
C
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      ET0    = -JYEAR() * 100
      ET1    =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, LOCPAR )

C
C     Write the file.
C
      CALL DSKW02 ( HANDLE,      BODYID,           SURFID,
     .              DCLASS,      FRNAME,           LATSYS,
     .              LOCPAR,      MNCOR1,           MXCOR1,
     .              MNCOR2,      MXCOR2,           MNCOR3,
     .              MXCOR3,      ET0,              ET1,
     .              LNV,         VRTCES(1,BASE+2), LNP,
     .              PLATES(1,3), SPAIXD,           SPAIXI       )

C
C     Close the file.
C
      CALL DSKCLS ( HANDLE, .TRUE. )

      CALL CHKOUT ( 'T_TORUS' )
      RETURN

      END
