!$Procedure      ZZEKCNAM ( EK, get column name )

      SUBROUTINE ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

! $ Abstract
!
!     Look up the name of a column, given the column's descriptor.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'

      INTEGER               HANDLE
      INTEGER               COLDSC ( CDSCSZ )
      CHARACTER*(*)         COLUMN

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     COLDSC     I   Column descriptor.
!     COLUMN     O   Column name.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     COLDSC         is the column descriptor of a column whose name is
!                    desired.
!
! $ Detailed_Output
!
!     COLNAM         is the name of the specified column.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine centralizes the coded needed to look up the
!     name of a specified column.  This is a frequently
!     performed function.
!
! $ Examples
!
!     See EKDELR.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&



!
!     Local variables
!
      INTEGER               NAMBAS

!
!     Use discovery check-in.
!
!
!     Look up the name.
!
      NAMBAS = COLDSC( NAMIDX )

      CALL DASRDC ( HANDLE,  NAMBAS+1,  NAMBAS+CNAMSZ, &
                    1,       CNAMSZ,    COLUMN         )


      END
