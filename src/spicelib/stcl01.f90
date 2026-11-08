!$Procedure STCL01 ( STAR catalog type 1, load catalog file )

      SUBROUTINE STCL01 ( CATFNM, TABNAM, HANDLE )

! $ Abstract
!
!     Load SPICE type 1 star catalog and return the catalog's
!     table name.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      CHARACTER*(*)         CATFNM
      CHARACTER*(*)         TABNAM
      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CATFNM     I   Catalog file name.
!     TABNAM     O   Catalog table name.
!     HANDLE     O   Catalog file handle.
!
! $ Detailed_Input
!
!     CATFNM   is the name of the catalog file.
!
! $ Detailed_Output
!
!     TABNAM   is the name of the table loaded from the catalog
!              file. This name must be provided as an input argument
!              to STCF01 catalog search routine. Multiple catalogs
!              containing the table TABNAM may be loaded. Sets of
!              columns, column names and attributes must be
!              identical through all these files.
!
!     HANDLE   is the integer handle of the catalog file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the indicated file cannot be opened, an error is signaled
!         by a routine in the call tree of this routine.
!
!     2)  If the indicated file has the wrong architecture version, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     3)  If an I/O error occurs while reading the indicated file, the
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     4)  If the catalog file is not a type 1 star catalog file, the
!         error SPICE(BADCATALOGFILE) is signaled.
!
! $ Files
!
!     This routine loads a SPICE type 1 star catalog file.
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
!     This STCL01 routine is intended to be part of the user
!     interface to the SPICE type 1 star catalog. It loads a
!     SPICE type 1 star catalog file and makes its data available
!     for searches and retrieval.
!
!     Other routines in SPICE type 1 star catalog access family are:
!
!        STCF01  search through the catalog for all stars within
!                a specified RA-DEC rectangle.
!
!        STCG01  retrieve position and characteristics for
!                every single star found.
!
! $ Examples
!
!     In the following code fragment, STCL01 is used to load
!     a SPICE type 1 star catalog.
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
!     C     Retrieve data for every star that matched the
!     C     search criteria.
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
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Balanced calls to CHKIN/CHKOUT.
!
!-    SPICELIB Version 1.0.0, 15-MAY-1996 (BVS)
!
!-&


! $ Index_Entries
!
!     load a type 1 star catalog file
!
!-&


!
!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      LOGICAL               ISTYP1
      CHARACTER*(256)       ERRMSG

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STCL01' )
      END IF

!
!     Check whether the file is really a type 1 star catalog file.
!     If not then signal an error.
!
      CALL STCC01 ( CATFNM, TABNAM, ISTYP1, ERRMSG )

      IF ( .NOT. ISTYP1 ) THEN
         CALL SETMSG ('File # is not type 1 star catalog ' // &
                         'file.' // ERRMSG                  )
         CALL ERRCH  ( '#', CATFNM                          )
         CALL SIGERR ( 'SPICE(BADCATALOGFILE)'              )
         CALL CHKOUT ( 'STCL01' )
         RETURN
      END IF

!
!     Load the catalog file with the high level EK loader.
!
      CALL EKLEF  ( CATFNM, HANDLE )

      CALL CHKOUT ( 'STCL01' )
      RETURN
      END
