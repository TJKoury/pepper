!$Procedure STCC01 ( STAR catalog type 1, check whether type 1 )

      SUBROUTINE STCC01 ( CATFNM, TABNAM, ISTYP1, ERRMSG )

! $ Abstract
!
!     Check whether a file is a type 1 star catalog and return the
!     catalog's table name if it is.
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

      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ekglimit.inc'

      CHARACTER*(*)         CATFNM
      CHARACTER*(*)         TABNAM
      LOGICAL               ISTYP1
      CHARACTER*(*)         ERRMSG

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CATFNM     I   Catalog file name.
!     TABNAM     O   Catalog table name.
!     ISTYP1     O   .TRUE. when file is type 1 star catalog.
!     ERRMSG     O   Error message.
!
! $ Detailed_Input
!
!     CATFNM   is the name of the catalog file.
!
! $ Detailed_Output
!
!     TABNAM   is the name of the data table contained in the
!              catalog. Set to blank if file is not a type 1 star
!              catalog.
!
!     ISTYP1   is .TRUE. when the file is a type 1 star catalog. .FALSE.
!              otherwise.
!
!     ERRMSG   is a diagnostic message indicating why the file is
!              not a type 1 star catalog. Set to blank if the file
!              is a type 1 star catalog.
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
! $ Files
!
!     This routine checks whether file is really SPICE type 1 star
!     catalog file.
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
!     will NOT be accessible through type 1 star catalog access
!     routines. Note that the names and attributes of these additional
!     columns must be identical for all segments containing this table.
!
! $ Particulars
!
!     This routine does not need to be called by the user's program.
!     It is used by star catalog loader routines to check
!     whether a particular file is a type 1 star catalog before loading
!     the file.
!
! $ Examples
!
!     In the following code fragment, STCC01 is used to determine
!     whether a file is a SPICE type 1 star catalog.
!
!     C
!     C     Call STCC01 to determine whether the file is type 1 star
!     C     catalog file.
!     C
!           CALL STCC01 ( CATFNM, TABNAM, ISTYP1, ERRMSG )
!
!     C
!     C     Check ISTYP1 flag and stop execution and report an
!     C     error if file is not type 1 star catalog file.
!     C
!           IF ( .NOT. ISTYP1 ) THEN
!          .   WRITE (*,*) 'The file:'
!          .   WRITE (*,*) '  ',CATFNM(1:RTRIM(CATFNM))
!          .   WRITE (*,*) 'is not a type 1 star catalog.'
!          .   WRITE (*,*) ERRMSG
!              STOP
!           END IF
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
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
!     check whether a file is a type 1 star catalog
!
!-&


!
!
!     SPICELIB functions
!
      LOGICAL               RETURN
      INTEGER               EKNSEG
      INTEGER               NBLEN
      INTEGER               ISRCHC


!
!     Local parameters.
!
      INTEGER               CAT1NC
      PARAMETER           ( CAT1NC = 7 )

!
!     Local variables
!

      CHARACTER*(4)         CAT1DT ( CAT1NC )
      CHARACTER*(4)         DTYPES ( MXCLSG )
      CHARACTER*(CNAMSZ)    CAT1NM ( CAT1NC )
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(TNAMSZ)    TMPTNM
      CHARACTER*(TNAMSZ)    TNMPRV

      INTEGER               I
      INTEGER               J
      INTEGER               NCOLS
      INTEGER               NROWS
      INTEGER               NUMSEG
      INTEGER               SIZES  ( MXCLSG )
      INTEGER               STRLNS ( MXCLSG )
      INTEGER               TMPHND

      LOGICAL               FOUND
      LOGICAL               INDEXD ( MXCLSG )
      LOGICAL               NULLOK ( MXCLSG )

      SAVE

!
!     Initial values.
!
      DATA                  CAT1NM  /  'CATALOG_NUMBER', &
                                       'RA', &
                                       'DEC', &
                                       'RA_SIGMA', &
                                       'DEC_SIGMA', &
                                       'VISUAL_MAGNITUDE', &
                                       'SPECTRAL_TYPE' /

      DATA                  CAT1DT  /  'INT', &
                                       'DP', &
                                       'DP', &
                                       'DP', &
                                       'DP', &
                                       'DP', &
                                       'CHR' /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STCC01' )
      END IF

!
!     More initial values.
!
      TABNAM = ' '
      ERRMSG = ' '
      ISTYP1 = .TRUE.

!
!     Open star catalog file with low level "open for read access"
!     EK routine.
!
      CALL EKOPR ( CATFNM, TMPHND )

!
!     Get the number of segments in the file and check whether it is
!     greater than 0 (i.e. some data are is present in the file). If
!     not then set an error message and return to the calling routine.
!
      NUMSEG = EKNSEG ( TMPHND )

      IF ( NUMSEG .LE. 0 ) THEN
         ERRMSG = 'File contains no data.'
         ISTYP1 = .FALSE.
         CALL CHKOUT ( 'STCC01' )
         RETURN
      END IF

!
!     Loop through the segments to find out whether all of them
!     contain pieces of the same table. If not then set
!     an error message and return to the calling routine.
!
      DO I = 1, NUMSEG

         CALL EKSSUM (  TMPHND,  I,       TMPTNM,  NROWS, &
                        NCOLS,   CNAMES,  DTYPES,  SIZES, &
                        STRLNS,  INDEXD,  NULLOK          )

         IF ( I .GT. 1 ) THEN

            IF ( TMPTNM .NE. TNMPRV ) THEN
               ERRMSG = 'File contains more than one data table.'
               ISTYP1 = .FALSE.
               CALL CHKOUT ( 'STCC01' )
               RETURN
            END IF

         END IF

         TNMPRV = TMPTNM

      END DO

!
!     Check whether the  number of columns is less than it
!     is supposed to be in type 1 star catalogs. If so then set
!     an error message and return to a calling routine.
!

      IF ( NCOLS .LT. CAT1NC ) THEN
         ERRMSG = 'File contains too few data columns.'
         ISTYP1 = .FALSE.
         CALL CHKOUT ( 'STCC01' )
         RETURN
      END IF

!
!     Check whether all columns that will be used in catalog search and
!     star data fetching are present in the data table. If not
!     then set an error message and return to a calling routine.
!
      DO I = 1, CAT1NC

         FOUND = .FALSE.

         J = ISRCHC ( CAT1NM(I), NCOLS, CNAMES )

         IF ( J .GT. 0  ) THEN
            FOUND = CAT1DT(I) .EQ. DTYPES(J) .AND. .NOT. NULLOK(J)
         END IF

         IF ( .NOT. FOUND ) THEN
            ERRMSG = ' Column '                             // &
                     CAT1NM(I)(1:NBLEN(CAT1NM(I)))          // &
                     ' is not found or'                     // &
                     ' improperly declared in the file.'
            ISTYP1 = .FALSE.
            CALL CHKOUT ( 'STCC01' )
            RETURN
         END IF

      END DO

!
!     If we got to this point then all checks were passed successfully
!     and the file can be processed as a type 1 star catalog. We
!     "return" the table name and close the file with the EK close
!     routine.
!
      TABNAM = TMPTNM

      CALL EKCLS ( TMPHND )

      CALL CHKOUT ( 'STCC01' )
      RETURN
      END
