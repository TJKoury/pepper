!$Procedure STCF01 (STAR catalog type 1, find stars in RA-DEC box)

      subroutine stcf01 ( catnam, westra, eastra, sthdec, nthdec, &
                          nstars)

! $ Abstract
!
!     Search through a type 1 star catalog and return the number of
!     stars within a specified RA - DEC rectangle.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     None.
!
! $ Declarations

      implicit none

      character*(*)         catnam
      double precision      westra
      double precision      eastra
      double precision      sthdec
      double precision      nthdec
      integer               nstars

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CATNAM     I   Catalog table name.
!     WESTRA     I   Western most right ascension in radians.
!     EASTRA     I   Eastern most right ascension in radians.
!     STHDEC     I   Southern most declination in radians.
!     NTHDEC     I   Northern most declination in radians.
!     NSTARS     O   Number of stars found.
!
! $ Detailed_Input
!
!     CATNAM   is name of the catalog data table. This name is
!              returned by the catalog loader routine STCL01.
!
!     WESTRA   are right ascension and declination constraints
!     EASTRA   giving the western, eastern, southern and northern
!     STHDEC   boundaries of a search rectangle as follows:
!     NTHDEC
!                    RA  BETWEEN WESTRA  AND EASTRA  and
!                    DEC BETWEEN STHDEC AND NTHDEC
!
!              where RA and DEC are the right ascension and
!              declination of a star. WESTRA always represents
!              "west" side of this rectangle and EASTRA -- the
!              "east" side. STHDEC represents the "south" side
!              of the rectangle, NTHDEC represents the "north"
!              side of the rectangle.
!
!              For an observer standing on the surface
!              of the earth at the equator, the west side of the
!              rectangle ( the side associated with WESTRA) rises
!              first. The east side (the side associated with
!              EASTRA) rises last. All meridians that rise between
!              the rising of the west and east edges of the
!              rectangle  cross through the RA-DEC rectangle.
!
!              To specify the 6 degrees wide RA-DEC
!              square centered on the celestial equator that
!              has western most right ascension of 357 degrees,
!              use the following values for WESTRA, EASTRA, STHDEC,
!              and NTHDEC (we multiply the angles by the SPICELIB
!              function RPD to convert degrees to radians).
!
!                   WESTRA  = 357.0D0 * RPD()
!                   EASTRA  =   3.0D0 * RPD()
!                   STHDEC  =  -3.0D0 * RPD()
!                   DEXMAX  =   3.0D0 * RPD()
!
!              To specify a 5 degree wide RA-DEC square that has
!              western most right ascension 10 degrees and
!              eastern most right ascension 15 degrees and southern
!              most declination of 45 degrees, assign the following
!              values to WESTRA, EASTRA, STHDEC and NTHDEC.
!
!                   WESTRA  =  10.0D0 * RPD()
!                   EASTRA  =  15.0D0 * RPD()
!                   STHDEC  =  45.0D0 * RPD()
!                   DEXMAX  =  50.0D0 * RPD()
!
!              All RA and DECS should be in radians and relative
!              to the J2000 inertial frame.
!
!              All Right Ascension values should be in the
!              interval [0, 2*pi ).  This routine does
!              not "fold" Right Ascension values into the this
!              interval. For example if you request stars in
!              whose right ascensions lie between 3*pi and 4*pi
!              no stars will be found.
!
!              All Declination values should be in the interval
!              [-pi,pi].
!
! $ Detailed_Output
!
!     NSTARS   is number of catalog stars found within the
!              specified RA - DEC rectangle.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If no star catalog has been loaded, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If the catalog query fails for any reason, the error
!         SPICE(QUERYFAILURE) is signaled.
!
! $ Files
!
!     This routine searches for stars within SPICE type 1 star catalog
!     files that have been loaded by calls to the STCL01 routine and
!     that contain that catalog data table named CATNAM.
!
!     SPICE type 1 star catalog files MUST contain a single data table.
!     It can occupy a single segment or it can spread across multiple
!     segments. This table MUST include the following columns:
!
!        column name                data type          units
!     -------------------------------------------------------
!        RA                   DOUBLE PRECISION        DEGREES
!        DEC                  DOUBLE PRECISION        DEGREES
!        RA_SIGMA             DOUBLE PRECISION        DEGREES
!        DEC_SIGMA            DOUBLE PRECISION        DEGREES
!        CATALOG_NUMBER       INTEGER
!        SPECTRAL_TYPE        CHARACTER*(4)
!        VISUAL_MAGNITUDE     DOUBLE PRECISION
!
!     Nulls are not allowed in any of the columns.
!     Other columns can also be present in the table but their data
!     will NOT be accessible through STCF01 and STCG01 --
!     the interface used to access data in the catalog. Note
!     that the names and attributes of these additional columns
!     must be identical for all segments containing this table.
!
! $ Particulars
!
!     This routine is intended to be a part of the user interface to
!     the SPICE type 1 star catalog. It allows the caller to find all
!     stars within a specified RA - DEC rectangle in the SPICE
!     EK type 1 star catalog files loaded by STCL01. This
!     subroutine MUST NOT be called before a catalog file has
!     been loaded.
!
!     Other routines in the SPICE type 1 star catalog access
!     family are:
!
!        STCL01  load the catalog file and make its data
!                available for search and retrieval.
!
!        STCG01  retrieve position and characteristics for
!                a specified star in the set found by this
!                routine.
!
! $ Examples
!
!     In the following code fragment, STCF01 is used to find
!     all stars within a specified RA - DEC rectangle in a SPICE
!     EK type 1 star catalog.
!
!     C
!     C     Load catalog file.
!     C
!           CALL STCL01 ( CATFN, TABNAM, HANDLE )
!     C
!     C     Search through the loaded catalog.
!     C
!           CALL STCF01 ( TABNAM, WESTRA,  EASTRA,
!          .              STHDEC, NTHDEC, NSTARS )
!     C
!     C     Retrieve data for every star found.
!     C
!           DO I = 1, NSTARS
!
!              CALL STCG01 ( I, RA, DEC, RASIG, DECSIG,
!          .                 CATNUM, SPTYPE, VMAG )
!
!           END DO
!
! $ Restrictions
!
!     1)  The catalog file STCF01 searches through MUST be loaded
!         by STCL01 before STCF01 is called.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 15-MAY-1996 (BVS)
!
!-&


! $ Index_Entries
!
!     find stars in RA-DEC rectangle in type 1 star catalog
!
!-&


!
!
!     SPICELIB functions
!
      logical               return
      double precision      dpr

!
!     Local parameters.
!
      integer               errlen
      parameter           ( errlen = 512 )

      integer               qrylen
      parameter           ( qrylen = 512 )

!
!     Local variables
!
      character*(errlen)    errmsg
      character*(qrylen)    query
      character*(qrylen)    qrytm1
      character*(qrylen)    qrytm2

      double precision      ramin
      double precision      ramax
      double precision      decmin
      double precision      decmax

      logical               error

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'STCF01' )
      end if

!
!     Query templates.
!
      qrytm1 = &
       'SELECT '                                            // &
       'RA, DEC, RA_SIGMA, DEC_SIGMA,'                      // &
       'CATALOG_NUMBER, SPECTRAL_TYPE, VISUAL_MAGNITUDE '   // &
       'FROM # '                                            // &
       'WHERE '                                             // &
       '( RA  BETWEEN # AND # ) AND '                       // &
       '( DEC BETWEEN # AND # ) '

      qrytm2 = &
       'SELECT '                                            // &
       'RA, DEC, RA_SIGMA, DEC_SIGMA,'                      // &
       'CATALOG_NUMBER, SPECTRAL_TYPE, VISUAL_MAGNITUDE '   // &
       'FROM # '                                            // &
       'WHERE '                                             // &
       '( ( RA BETWEEN # AND 360 ) OR '                     // &
       '  ( RA BETWEEN 0 AND #   )      ) AND '             // &
       '  ( DEC BETWEEN # AND # ) '

!
!     Choose query template to be used.
!
      if ( westra .le. eastra ) then
         query = qrytm1
      else
         query = qrytm2
      end if

!
!     Convert angles in radians to angles in degrees.
!
      ramin   = westra * dpr()
      ramax   = eastra * dpr()
      decmin  = sthdec * dpr()
      decmax  = nthdec * dpr()

!
!     Construct query using inputs and chosen template.
!
      call repmc( query, '#', catnam,     query )
      call repmd( query, '#', ramin,  15, query )
      call repmd( query, '#', ramax,  15, query )
      call repmd( query, '#', decmin, 15, query )
      call repmd( query, '#', decmax, 15, query )

!
!     Submit query and get number of stars. Check for
!     errors in QUERY.
!
      call ekfind( query, nstars, error, errmsg )

      if ( error ) then
         call setmsg ( 'Error querying type 1 star catalog. '// &
                       'Error message: # '                  )
         call errch  ( '#', errmsg                          )
         call sigerr ( 'SPICE(QUERYFAILURE)'                )
         call chkout ( 'STCF01' )
         return
      end if

      call chkout ( 'STCF01' )
      return
      end
