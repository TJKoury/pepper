!$Procedure ZZNSPPOK (Private Routine -- NSPIO Port)

      INTEGER FUNCTION ZZNSPPOK ( PORT, NPORTS, PORTS )

! $ Abstract
!
!     Find the integer associated with an NSPIO PORT string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEXT
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         PORT
      INTEGER               NPORTS
      CHARACTER*(*)         PORTS ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   is a string indicating the port to find.
!     NPORTS     I   is the number of ports in the PORTS array.
!     PORTS      I   an array of strings containing the possible ports.
!
!     The function returns an integer that represents the position
!     of PORT in the PORTS array.
!
! $ Detailed_Input
!
!     PORT       is the name of a port supported by the NSPIO
!                umbrella, and is an entry in the PORTS array.
!
!     NPORTS     is the number of entries in the PORTS arrray.
!
!     PORTS      is a list of acceptable PORTs supported by the
!                NSPIO umbrella.
!
! $ Detailed_Output
!
!     The function returns an integer that represents the position
!     of PORT in the PORTS array.  This integer is used in NSPIO
!     to access information in parallel arrays.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If PORT is not found in the PORTS array, then the error
!        NSPIO(UNKNOWNPORT) is signaled.
!
! $ Particulars
!
!     This private routine is simply a place to consolidate the
!     PORT to integer code conversion.
!
! $ Examples
!
!     This routine is a simple private routine. See NSPIO and its
!     entry points for samples of its usage.
!
! $ Restrictions
!
!     1) NPORTS must not be greater than the number of available
!        members of the PORTS array, else memory violation will
!        occur.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               ISRCHC

!
!     Local Variables
!
      INTEGER               ID

!
!     Find PORT in the PORTS array.
!
      ID = ISRCHC ( PORT, NPORTS, PORTS )

!
!     Set ZZNSPPOK to the return value.
!
      ZZNSPPOK = ID

!
!     Check to see if we were able to find the integer ID of PORT.
!     If not, use discovery check in/out and signal an error.
!
      IF ( ID .EQ. 0 ) THEN

         CALL CHKIN  ( 'ZZNSPPOK'                   )
         CALL SETMSG ( '$ is an unrecognized port.' )
         CALL ERRCH  ( '$', PORT                    )
         CALL SIGERR ( 'NSPIO(UNKNOWNPORT)'         )
         CALL CHKOUT ( 'ZZNSPPOK'                   )

      END IF

      RETURN

      END
