!$Procedure ZZT_BODDSK (Make type 2 DSK for body's reference ellipsoid)

      SUBROUTINE ZZT_BODDSK ( DSK, BODY, FIXREF, LOAD, HANDLE )

! $ Abstract
!
!     Create a DSK containing a tessellation of the reference
!     ellipsoid of a specified body.
!
! $ Required_Reading
!
!     DSK
!     PCK
!
! $ Keywords
!
!     BODY
!     ELLIPSOID
!     PLATE
!     TILE
!     TESSELLATE
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         DSK
      CHARACTER*(*)         BODY
      CHARACTER*(*)         FIXREF
      LOGICAL               LOAD
      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DSK        I   Name of DSK file to create.
!     BODY       I   Name of central body and of surface.
!     FIXREF     I   Name of body-fixed, body-centered reference frame.
!     LOAD       I   Flag indicating whether to load the DSK.
!     HANDLE     O   DAS handle of DSK if loaded.
!
! $ Detailed_Input
!
!     DSK        is the name of a new DSK file to create.
!
!
!     BODY       is the name of a body for which a DSK file is to be
!                created. The ID code associated with BODY is used as
!                both the surface and body ID code in the DSK file.
!
!                Case and leading and trailing blanks in the name are
!                not significant.
!
!
!     FIXREF     is the name of a body-fixed, body-centered reference
!                frame associated with BODY.
!
!                Case and leading and trailing blanks in the name are
!                not significant.
!
!
!     LOAD       is a logical flag that indicates whether to load the
!                DSK file for read access after the file has been
!                created. If LOAD is set to .TRUE., the file is loaded
!                for read access by DASOPR.
!
! $ Detailed_Output
!
!     HANDLE     is a DAS handle associated with the specified
!                DSK file, if the input flag LOAD is .TRUE.
!
!                HANDLE is undefined if the input LOAD is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input body name cannot be mapped to an integer
!         ID code, the error SPICE(BODYIDNOTFOUND) will be signaled.
!
!     2)  If the input frame name cannot be mapped to an integer
!         ID code, the error SPICE(UNKNOWNFRAME) will be signaled.
!
!     3)  If the radii of the specified body are not available
!         in the kernel pool, the error will be diagnosed by
!         a routine in the call tree of this routine.
!
!     4)  If the reference frame designated by FIXREF is not
!         centered at BODY, the error SPICE(BADFRAMECENTER)
!         will be signaled.
!
!     5)  If the specified DSK file already exists, it will not be
!         overwritten. An error will be signaled by a routine in the
!         call tree of this routine.
!
!     6)  If an error is encountered while writing the DSK file, the
!         error will be signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     1)  This routine requires radii for BODY to be available
!         in the kernel pool. Normally these radii are made
!         available by loading a PCK containing them.
!
!     2)  This routine creates a type 2 segment containing a
!         tessellation of the reference ellipsoid of a specified
!         body. The segment contains
!
!            1000   longitude bands
!             500   latitude bands
!
!         which yields
!
!            499002 vertices
!            998000 plates
!
!         The body and surface IDs of the segment are both the NAIF ID
!         corresponding to BODY.
!
!         The coordinate system of the segment is planetocentric.
!         The longitude range is
!
!           -180 : 180 degrees
!
!         The latitude range is
!
!            -90 :  90 degrees
!
!         Time coverage of the segment is J2000 TDB +/- 100 Julian
!         years.
!
!         The data class of the segment is 1 (spherically
!         parameterized surface).
!
!         The size of the file is roughly 50 Mb. The size will
!         depend on voxel sizes and voxel grid dimensions, which
!         vary with the body's shape and size.
!
! $ Particulars
!
!     See Files above.
!
! $ Examples
!
!     See use of C language version in tspice_c test families.
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
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 27-JUL-2016 (NJB)
!
!        Re-implemented. Now uses T_WRDSK2 entry poin T_ELDS2Z.
!
!     30-APR-2014 (NJB)
!
!        Original version.
!
!-&

! $ Index_Entries
!
!     tessellate reference ellipsoid of body
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NLAT
      PARAMETER           ( NLAT = 500 )

      INTEGER               NLON
      PARAMETER           ( NLON = 1000 )

!
!     Local variables
!
      INTEGER               BODCDE
      INTEGER               FRCENT
      INTEGER               FRCODE
      INTEGER               FRCLSS
      INTEGER               FRCLID
      INTEGER               SURFID

      LOGICAL               FOUND


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZT_BODDSK' )

!
!     Get body ID code.
!
      CALL BODS2C ( BODY, BODCDE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The body name # could not be mapped ' &
         //            'to an ID code.'                       )
         CALL ERRCH  ( '#',  BODY                             )
         CALL SIGERR ( 'SPICE(BODYIDNOTFOUND)'                )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF

!
!     Map frame name to code.
!
      CALL NAMFRM ( FIXREF, FRCODE )

      IF ( FRCODE .EQ. 0 ) THEN

         CALL SETMSG ( 'The frame name # could not be mapped ' &
         //            'to an ID code.'                       )
         CALL ERRCH  ( '#',  FIXREF                           )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                  )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF

!
!     Get frame info and check frame center.
!
      CALL FRINFO ( FRCODE, FRCENT, FRCLSS, FRCLID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZT_BODDSK' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN
!
!        We really don't expect to get here, but....
!
         CALL SETMSG ( 'The frame name # could not be mapped ' &
         //            'to an ID code.'                       )
         CALL ERRCH  ( '#',  FIXREF                           )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                  )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF

      IF ( FRCENT .NE. BODCDE ) THEN

         CALL SETMSG ( 'The frame # is centered at #; the ' &
         //            'center must coincide with the body # ' &
         //            'which has ID #.'                      )
         CALL ERRCH  ( '#',  FIXREF                           )
         CALL ERRINT ( '#',  FRCENT                           )
         CALL ERRCH  ( '#',  BODY                             )
         CALL ERRINT ( '#',  BODCDE                           )
         CALL SIGERR ( 'SPICE(BADFRAMECENTER)'                )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF


!
!     Create DSK file.
!
!     Some of the *PL02 routines require the surface ID to
!     match the body ID.
!
      SURFID = BODCDE

      CALL T_ELDS2Z ( BODCDE, SURFID, FIXREF, NLON, NLAT, DSK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZT_BODDSK' )
         RETURN
      END IF

!
!     Open the file for read access if so directed.
!
      IF ( LOAD ) THEN

         CALL DASOPR ( DSK, HANDLE )

      ELSE
!
!        Ensure that the stale handle can't be used.
!
         HANDLE = 0

      END IF

      CALL CHKOUT ( 'ZZT_BODDSK' )
      RETURN
      END
