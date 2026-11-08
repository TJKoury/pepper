!$Procedure STCC01 ( STAR catalog type 1, check whether type 1 )

      subroutine stcc01 ( catfnm, tabnam, istyp1, errmsg )
      use pepper_kinds, only: ip

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

      implicit none

      include 'ekcnamsz.inc'
      include 'ektnamsz.inc'
      include 'ekglimit.inc'

      character(*)          catfnm
      character(*)          tabnam
      logical               istyp1
      character(*)          errmsg

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
      logical               return
      integer(ip)           eknseg
      integer(ip)           nblen
      integer(ip)           isrchc


!
!     Local parameters.
!
      integer(ip), parameter :: cat1nc = 7

!
!     Local variables
!

      character(4)          cat1dt ( cat1nc )
      character(4)          dtypes ( mxclsg )
      character(cnamsz)     cat1nm ( cat1nc )
      character(cnamsz)     cnames ( mxclsg )
      character(tnamsz)     tmptnm
      character(tnamsz)     tnmprv

      integer(ip)           i
      integer(ip)           j
      integer(ip)           ncols
      integer(ip)           nrows
      integer(ip)           numseg
      integer(ip)           sizes  ( mxclsg )
      integer(ip)           strlns ( mxclsg )
      integer(ip)           tmphnd

      logical               found
      logical               indexd ( mxclsg )
      logical               nullok ( mxclsg )

      save

!
!     Initial values.
!
      data                  cat1nm  /  'CATALOG_NUMBER', &
                                       'RA', &
                                       'DEC', &
                                       'RA_SIGMA', &
                                       'DEC_SIGMA', &
                                       'VISUAL_MAGNITUDE', &
                                       'SPECTRAL_TYPE' /

      data                  cat1dt  /  'INT', &
                                       'DP', &
                                       'DP', &
                                       'DP', &
                                       'DP', &
                                       'DP', &
                                       'CHR' /

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     More initial values.
!
      tabnam = ' '
      errmsg = ' '
      istyp1 = .true.

!
!     Open star catalog file with low level "open for read access"
!     EK routine.
!
      call ekopr ( catfnm, tmphnd )

!
!     Get the number of segments in the file and check whether it is
!     greater than 0 (i.e. some data are is present in the file). If
!     not then set an error message and return to the calling routine.
!
      numseg = eknseg ( tmphnd )

      if ( numseg <= 0 ) then
         errmsg = 'File contains no data.'
         istyp1 = .false.
         return
      end if

!
!     Loop through the segments to find out whether all of them
!     contain pieces of the same table. If not then set
!     an error message and return to the calling routine.
!
      do i = 1, numseg

         call ekssum (  tmphnd,  i,       tmptnm,  nrows, &
                        ncols,   cnames,  dtypes,  sizes, &
                        strlns,  indexd,  nullok          )

         if ( i > 1 ) then

            if ( tmptnm /= tnmprv ) then
               errmsg = 'File contains more than one data table.'
               istyp1 = .false.
               return
            end if

         end if

         tnmprv = tmptnm

      end do

!
!     Check whether the  number of columns is less than it
!     is supposed to be in type 1 star catalogs. If so then set
!     an error message and return to a calling routine.
!

      if ( ncols < cat1nc ) then
         errmsg = 'File contains too few data columns.'
         istyp1 = .false.
         return
      end if

!
!     Check whether all columns that will be used in catalog search and
!     star data fetching are present in the data table. If not
!     then set an error message and return to a calling routine.
!
      do i = 1, cat1nc

         found = .false.

         j = isrchc ( cat1nm(i), ncols, cnames )

         if ( j > 0  ) then
            found = cat1dt(i) == dtypes(j) .and. .not. nullok(j)
         end if

         if ( .not. found ) then
            errmsg = ' Column '                             // &
                     cat1nm(i)(1:nblen(cat1nm(i)))          // &
                     ' is not found or'                     // &
                     ' improperly declared in the file.'
            istyp1 = .false.
            return
         end if

      end do

!
!     If we got to this point then all checks were passed successfully
!     and the file can be processed as a type 1 star catalog. We
!     "return" the table name and close the file with the EK close
!     routine.
!
      tabnam = tmptnm

      call ekcls ( tmphnd )

      return
      end
