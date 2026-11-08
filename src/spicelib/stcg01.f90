!$Procedure STCG01 ( STAR catalog type 1, get star data )

      subroutine stcg01 ( index,  ra,     dec,    rasig, &
                          decsig, catnum, sptype, vmag )

! $ Abstract
!
!     Get data for a single star from a SPICE type 1 star catalog.
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

      integer               index
      double precision      ra
      double precision      dec
      double precision      rasig
      double precision      decsig
      integer               catnum
      character*(*)         sptype
      double precision      vmag

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INDEX      I   Star index.
!     RA         O   Right ascension in radians.
!     DEC        O   Declination in radians.
!     RASIG      O   Right ascension uncertainty in radians.
!     DECSIG     O   Declination uncertainty in radians.
!     CATNUM     O   Catalog number.
!     SPTYPE     O   Spectral type.
!     VMAG       O   Visual magnitude.
!
! $ Detailed_Input
!
!     INDEX    is the index of the star in the list of stars
!              that satisfy the selection criteria specified in
!              the last call to STCF01.
!
! $ Detailed_Output
!
!     RA       is right ascension of the star at the catalog epoch
!              in radians relative to the J2000 inertial frame.
!
!     DEC      is declination of the star at the catalog epoch in
!              radians relative to the J2000 inertial frame.
!
!     RASIG    is the uncertainty in right ascension of the star at
!              the catalog epoch in radians.
!
!     DECSIG   is the uncertainty in declination of the star at
!              the catalog epoch in radians.
!
!     CATNUM   is the star number in the catalog.
!
!     SPTYPE   is the star's spectral type. See catalog description
!              for more information regarding encoding of spectral
!              type values.
!
!     VMAG     is the visual magnitude of the star.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If fetching of any of output values fails, the error
!         SPICE(BADSTARINDEX) is signaled.
!
!     2)  If no star catalog has been loaded, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If a star catalog type 1 was not queried by calling STCF01
!         before calling this routine, an error is signaled by a routine
!         in the call tree of this routine.
!
! $ Files
!
!     This routine reads the data from SPICE type 1 star catalog file
!     loaded into the program by a call to STCL01.
!
!     SPICE type 1 star catalog files MUST contain a single data table.
!     It can occupy a single segment or it can spread across multiple
!     segments. This table MUST include the following columns:
!
!        column name                data type          units
!        ----------------------------------------------------
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
!     the SPICE type 1 star catalog. It allows the caller to retrieve
!     data for a single star found by STCF01 using the star's
!     index within the search result array. This subroutine MUST
!     NOT be called before a search by STCF01 was done.
!
!     Other routines in the SPICE type 1 star catalog access
!     family are:
!
!        STCL01  load the catalog file and make its data
!                available for search and retrieval.
!
!        STCF01  search through the catalog for all stars within
!                a specified RA-DEC rectangle.
!
! $ Examples
!
!     In the following code fragment, STCG01 is used to retrieve
!     position and characteristics for every star within an RA - DEC
!     rectangle from a particular SPICE type 1 star catalog.
!
!     C
!     C     Load catalog file.
!     C
!           CALL STCL01 ( CATFN, TABNAM, HANDLE )
!     C
!     C     Search through the loaded catalog.
!     C
!           CALL STCF01 ( TABNAM, RAMIN,  RAMAX,
!          .              DECMIN, DECMAX, NSTARS )
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
!     1)  The catalog file STCG01 reads data from MUST be loaded
!         by STCL01 and a search through the catalog MUST be done by
!         STCF01 before STCG01 is called.
!
!     2)  No other EK queries can be made between the call to STCF01
!         and the call to STCG01.
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
!-    SPICELIB Version 1.1.0, 16-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Updated header to comply with NAIF standard. Corrected argument
!        names "RASIG" and "DECSIG" in $Brief_I/O.
!
!        Updated entry #3 in $Exceptions section.
!
!-    SPICELIB Version 1.0.0, 15-MAY-1996 (BVS)
!
!-&


! $ Index_Entries
!
!     get data for single star from a type 1 star catalog
!
!-&


!
!
!     SPICELIB functions
!
      logical               return
      double precision      rpd

!
!     Local variables.
!
      logical               found
      logical               null

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'STCG01' )
      end if

!
!     Fetch data from the catalog in the following order
!     as defined QUERY string template in STCF01 routine
!
!           RA, DEC, RASIG, DECSIG, CATNUM, SPTYPE, VMAG
!
!     Check FOUNDs and report error if any of the parameters
!     is not found.
!
!     Since NULLs are not allowed in any of the star catalog
!     columns, no check for NULLs is performed.
!
      call ekgd (  1, index, 1, ra, null, found )
      if ( .not. found ) then
         call setmsg ( 'RA value for star # not found. '    )
         call errint ( '#', index                           )
         call sigerr ( 'SPICE(BADSTARINDEX)'                )
         call chkout ( 'STCG01' )
         return
      end if

      call ekgd (  2, index, 1, dec, null, found )
      if ( .not. found ) then
         call setmsg ( 'DEC value for star # not found. '   )
         call errint ( '#', index                           )
         call sigerr ( 'SPICE(BADSTARINDEX)'                )
         call chkout ( 'STCG01' )
         return
      end if

      call ekgd (  3, index, 1, rasig, null, found )
      if ( .not. found ) then
         call setmsg ( 'RASIG value for star # not found. ' )
         call errint ( '#', index                           )
         call sigerr ( 'SPICE(BADSTARINDEX)'                )
         call chkout ( 'STCG01' )
         return
      end if

      call ekgd (  4, index, 1, decsig, null, found )
      if ( .not. found ) then
         call setmsg ( 'DECSIG value for star # not found.' )
         call errint ( '#', index                           )
         call sigerr ( 'SPICE(BADSTARINDEX)'                )
         call chkout ( 'STCG01' )
         return
      end if

      call ekgi (  5, index, 1, catnum, null, found )
      if ( .not. found ) then
         call setmsg ( 'CATNUM value for star # not found.' )
         call errint ( '#', index                           )
         call sigerr ( 'SPICE(BADSTARINDEX)'                )
         call chkout ( 'STCG01' )
         return
      end if

      call ekgc (  6, index, 1, sptype, null, found )
      if ( .not. found ) then
         call setmsg ( 'SPTYPE value for star # not found.' )
         call errint ( '#', index                           )
         call sigerr ( 'SPICE(BADSTARINDEX)'                )
         call chkout ( 'STCG01' )
         return
      end if

      call ekgd (  7, index, 1, vmag, null, found )
      if ( .not. found ) then
         call setmsg ( 'VMAG value for star # not found. '  )
         call errint ( '#', index                           )
         call sigerr ( 'SPICE(BADSTARINDEX)'                )
         call chkout ( 'STCG01' )
         return
      end if

!
!     Convert angles to radians before return.
!
      ra     = ra     * rpd()
      dec    = dec    * rpd()
      rasig  = rasig  * rpd()
      decsig = decsig * rpd()

      call chkout ( 'STCG01' )
      return
      end
