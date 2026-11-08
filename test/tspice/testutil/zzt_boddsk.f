C$Procedure ZZT_BODDSK (Make type 2 DSK for body's reference ellipsoid)

      SUBROUTINE ZZT_BODDSK ( DSK, BODY, FIXREF, LOAD, HANDLE )
  
C$ Abstract
C
C     Create a DSK containing a tessellation of the reference 
C     ellipsoid of a specified body.
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
C     PCK
C
C$ Keywords
C
C     BODY
C     ELLIPSOID
C     PLATE
C     TILE
C     TESSELLATE
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         DSK
      CHARACTER*(*)         BODY
      CHARACTER*(*)         FIXREF
      LOGICAL               LOAD
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     DSK        I   Name of DSK file to create.
C     BODY       I   Name of central body and of surface.
C     FIXREF     I   Name of body-fixed, body-centered reference frame.
C     LOAD       I   Flag indicating whether to load the DSK.
C     HANDLE     O   DAS handle of DSK if loaded.
C
C$ Detailed_Input
C
C     DSK        is the name of a new DSK file to create.
C
C
C     BODY       is the name of a body for which a DSK file is to be
C                created. The ID code associated with BODY is used as
C                both the surface and body ID code in the DSK file.
C
C                Case and leading and trailing blanks in the name are
C                not significant.  
C
C
C     FIXREF     is the name of a body-fixed, body-centered reference
C                frame associated with BODY.
C
C                Case and leading and trailing blanks in the name are
C                not significant.  
C
C
C     LOAD       is a logical flag that indicates whether to load the
C                DSK file for read access after the file has been
C                created. If LOAD is set to .TRUE., the file is loaded
C                for read access by DASOPR.
C
C$ Detailed_Output
C
C     HANDLE     is a DAS handle associated with the specified 
C                DSK file, if the input flag LOAD is .TRUE.
C                
C                HANDLE is undefined if the input LOAD is .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input body name cannot be mapped to an integer
C         ID code, the error SPICE(BODYIDNOTFOUND) will be signaled.
C
C     2)  If the input frame name cannot be mapped to an integer
C         ID code, the error SPICE(UNKNOWNFRAME) will be signaled.
C
C     3)  If the radii of the specified body are not available
C         in the kernel pool, the error will be diagnosed by
C         a routine in the call tree of this routine.
C
C     4)  If the reference frame designated by FIXREF is not
C         centered at BODY, the error SPICE(BADFRAMECENTER) 
C         will be signaled. 
C
C     5)  If the specified DSK file already exists, it will not be
C         overwritten. An error will be signaled by a routine in the
C         call tree of this routine.
C
C     6)  If an error is encountered while writing the DSK file, the
C         error will be signaled by a routine in the call tree of this
C         routine.
C
C$ Files
C
C     1)  This routine requires radii for BODY to be available
C         in the kernel pool. Normally these radii are made 
C         available by loading a PCK containing them.
C
C     2)  This routine creates a type 2 segment containing a 
C         tessellation of the reference ellipsoid of a specified
C         body. The segment contains
C
C            1000   longitude bands
C             500   latitude bands
C
C         which yields
C
C            499002 vertices
C            998000 plates
C
C         The body and surface IDs of the segment are both the NAIF ID
C         corresponding to BODY.
C
C         The coordinate system of the segment is planetocentric.
C         The longitude range is
C
C           -180 : 180 degrees
C
C         The latitude range is
C
C            -90 :  90 degrees
C
C         Time coverage of the segment is J2000 TDB +/- 100 Julian
C         years.
C
C         The data class of the segment is 1 (spherically
C         parameterized surface).
C
C         The size of the file is roughly 50 Mb. The size will
C         depend on voxel sizes and voxel grid dimensions, which
C         vary with the body's shape and size.
C
C$ Particulars
C 
C     See Files above.
C
C$ Examples
C
C     See use of C language version in tspice_c test families.
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 27-JUL-2016 (NJB)
C
C        Re-implemented. Now uses T_WRDSK2 entry poin T_ELDS2Z.
C
C     30-APR-2014 (NJB)
C
C        Original version.
C
C-&

C$ Index_Entries
C
C     tessellate reference ellipsoid of body
C
C-&

C     
C     SPICELIB functions
C
      LOGICAL               FAILED      
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               NLAT
      PARAMETER           ( NLAT = 500 )

      INTEGER               NLON
      PARAMETER           ( NLON = 1000 )

C
C     Local variables
C
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

C
C     Get body ID code.
C
      CALL BODS2C ( BODY, BODCDE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The body name # could not be mapped '
     .   //            'to an ID code.'                       )
         CALL ERRCH  ( '#',  BODY                             )
         CALL SIGERR ( 'SPICE(BODYIDNOTFOUND)'                )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF

C
C     Map frame name to code.
C
      CALL NAMFRM ( FIXREF, FRCODE )

      IF ( FRCODE .EQ. 0 ) THEN

         CALL SETMSG ( 'The frame name # could not be mapped '
     .   //            'to an ID code.'                       )
         CALL ERRCH  ( '#',  FIXREF                           )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                  )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF

C
C     Get frame info and check frame center.
C
      CALL FRINFO ( FRCODE, FRCENT, FRCLSS, FRCLID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZT_BODDSK' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN
C
C        We really don't expect to get here, but....
C
         CALL SETMSG ( 'The frame name # could not be mapped '
     .   //            'to an ID code.'                       )
         CALL ERRCH  ( '#',  FIXREF                           )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                  )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF

      IF ( FRCENT .NE. BODCDE ) THEN

         CALL SETMSG ( 'The frame # is centered at #; the '
     .   //            'center must coincide with the body # '
     .   //            'which has ID #.'                      )
         CALL ERRCH  ( '#',  FIXREF                           )
         CALL ERRINT ( '#',  FRCENT                           )
         CALL ERRCH  ( '#',  BODY                             )
         CALL ERRINT ( '#',  BODCDE                           )    
         CALL SIGERR ( 'SPICE(BADFRAMECENTER)'                )
         CALL CHKOUT ( 'ZZT_BODDSK'                             )
         RETURN

      END IF


C
C     Create DSK file.
C
C     Some of the *PL02 routines require the surface ID to 
C     match the body ID. 
C     
      SURFID = BODCDE

      CALL T_ELDS2Z ( BODCDE, SURFID, FIXREF, NLON, NLAT, DSK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZT_BODDSK' )
         RETURN
      END IF

C
C     Open the file for read access if so directed.
C
      IF ( LOAD ) THEN

         CALL DASOPR ( DSK, HANDLE )

      ELSE
C
C        Ensure that the stale handle can't be used.
C
         HANDLE = 0

      END IF

      CALL CHKOUT ( 'ZZT_BODDSK' )
      RETURN
      END
