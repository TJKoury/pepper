!$Procedure ZZDDHGSD ( Private --- DDH Get String Definitions )

      SUBROUTINE ZZDDHGSD ( CLASS, ID, LABEL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Retrieve string definitions for various CLASS and ID pairs.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      CHARACTER*(*)         CLASS
      INTEGER               ID
      CHARACTER*(*)         LABEL

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CLASS      I   String indicating the class of label requested.
!     ID         I   Integer ID desired for the decoding.
!     LABEL      O   Corresponding label for the CLASS/ID pair.
!
! $ Detailed_Input
!
!     CLASS      is the class of the input ID code.  Acceptable
!                values are:
!
!                   'METHOD' - File access method
!                   'ARCH'   - File architecture
!                   'BFF'    - Binary file format
!
!     ID         is a integer value that is defined in the include
!                file 'zzddhman.inc'.
!
! $ Detailed_Output
!
!     LABEL      is the 'decoded' ID value that describes a particular
!                CLASS/ID pair. In the event that CLASS and ID do not
!                correspond to parameters in the include file, the
!                routine simply returns a blank string.
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
!     1) If the CLASS/ID pair is not known, then this routine
!        simply returns a blank label.
!
! $ Particulars
!
!     This routine is designed to consolidate label definitions for
!     parameters in the include file 'zzddhman.inc'.  These labels
!     are to be used in error messages and user level subroutines.
!
! $ Examples
!
!     See ZZDDHMAN for sample usage.
!
! $ Restrictions
!
!     This routine must be kept compatible with the contents of
!     'zzddhman.inc' as its functionality is heavily dependent
!     on its contents.
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
!-    SPICELIB Version 1.1.0, 11-OCT-2005 (NJB)
!
!        Removed embedded blanks from reference to parameter ARCH.
!        Previous spelling was 'ARC  H'.  This was not a problem
!        for Fortran compilers but was irritating for human readers.
!
!-    SPICELIB Version 1.0.0, 07-JUN-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               ISRCHC

!
!     Local Parameters
!
      INTEGER               LOCSIZ
      PARAMETER           ( LOCSIZ = 8 )

!
!     Number of classes of label/ID pairs of which this routine is
!     aware.
!
      INTEGER               NUMCLS
      PARAMETER           ( NUMCLS = 3 )

!
!     Integer codes for the currently supported classes.
!
      INTEGER               METHOD
      PARAMETER           ( METHOD = 1 )

      INTEGER               ARCH
      PARAMETER           ( ARCH   = METHOD + 1 )

      INTEGER               BFF
      PARAMETER           ( BFF    = ARCH   + 1 )

!
!     Local Variables
!
      CHARACTER*(LOCSIZ)    CLSLST ( NUMCLS )
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(LOCSIZ)    TMPSTR

      INTEGER               CLSID

!
!     Saved Variables
!
      SAVE                  CLSLST
      SAVE                  STRAMH
      SAVE                  STRARC
      SAVE                  STRBFF

!
!     Data Statements
!
      DATA                  CLSLST / 'METHOD', 'ARCH', 'BFF' /

      DATA                  STRAMH / 'READ',     'WRITE', &
                                     'SCRATCH',  'NEW'       /

      DATA                  STRARC / 'DAF',      'DAS'       /

      DATA                  STRBFF / 'BIG-IEEE', 'LTL-IEEE', &
                                     'VAX-GFLT', 'VAX-DFLT'  /

!
!     Left justify and convert the input to upper case.
!
      CALL LJUST ( CLASS,  TMPSTR )
      CALL UCASE ( TMPSTR, TMPSTR )

      CLSID = ISRCHC ( TMPSTR, NUMCLS, CLSLST )

!
!     Initialize LABEL to the default response.
!
      LABEL = ' '

!
!     Branch on CLSID and return the appropriate label as requested
!     by ID.
!
      IF      (       ( CLSID .EQ. METHOD ) &
                .AND. (    ID .GE. 1      ) &
                .AND. (    ID .LE. NUMAMH ) ) THEN

         LABEL = STRAMH ( ID )

      ELSE IF (       ( CLSID .EQ. ARCH   ) &
                .AND. (    ID .GE. 1      ) &
                .AND. (    ID .LE. NUMARC ) ) THEN

         LABEL = STRARC ( ID )

      ELSE IF (       ( CLSID .EQ. BFF    ) &
                .AND. (    ID .GE. 1      ) &
                .AND. (    ID .LE. NUMBFF ) ) THEN

         LABEL = STRBFF ( ID )

      END IF

      RETURN
      END
