!$Procedure      HEADER (HEADER for a report)

      SUBROUTINE HEADER ( N, COMP, VALUE, WDTH )
      use pepper_legacy

! $ Abstract
!
!    This is an umbrella routine for setting up headers
!    on tabular reports.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     REPORTS
!
! $ Declarations

      IMPLICIT NONE
      INTEGER               N
      INTEGER               COMP
      CHARACTER*(*)         VALUE
      INTEGER               WDTH

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N         I/O  Column number
!     COMP      I/O  Component number
!     VALUE     I/O  String Value.
!     WDTH       O   Non-blank width of VALUE
!
! $ Detailed_Input
!
!     N         specifies which column is being defined.
!               Legitimate values are 1 to 40.
!
!     COMP      specifies which column component is being
!               specified.  Legitimate values are 1 to 10.
!
!     VALUE     specifies the column component value. It should
!               be 800 or fewer characters in length.
!
! $ Detailed_Output
!
!     N         specifies which column to fetch information from.
!               Legitimate values are 1 to 40.
!
!     COMP      specifies which column component is to obtain
!               information for. Legitimate values are 1 to 10.
!
!     VALUE     Value of requested column component.
!
!     WDTH      is the non-blank width of VALUE.
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
!     If N or NCOMP is out of range:
!
!        SCOLMN simply returns.  No data is buffered. No warning
!        or error is issued.
!
!        GCOLMN returns a blank.  WDTH will be set to 1.
!
!
! $ Particulars
!
!     This is a routine designed to work with the routine
!     TABRPT when creating tabular outputs.  It is primarily
!     intended for creating the header portion of a report.
!
!     For example, the area marked with the arrow below.
!
!        Name       Phone       Address    <<=============
!        =================================
!        Bill       555-1212    Pasadena, CA
!        Bob        555-2121    Flint, Michigan
!        Ian        555-1234    San Jose, CA
!
!     You could use this to fill out the contents of the report
!     if you don't have something already  that fetches
!     string values.
!
! $ Examples
!
!     Suppose you wanted to create the header above and have
!     it appear on your reports.  Here's all you need to do.
!
!        CALL SCOLMN ( 1, 1, 'Name'    )
!        CALL SCOLMN ( 2, 1, 'Phone'   )
!        CALL SCOLMN ( 3, 1, 'Address' )
!
!     Then simply pass the entry point GCOLMN to TABRPT to construct
!     the header portion of the report.
!
!        CALL TABRPT ( 3, item,   size,
!    .                    width,  justr,
!    .                    presrv, spcial,
!    .                    lmarge, space,
!    .                    GCOLMN   )
!
!     filling out the various items as is appropriate for the
!     table you plan to create.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!       W.L. Taber      (JPL)
!
! $ Literature_References
!
!       None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 3-AUG-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Setting and getting values for reports
!
!-&
!
!     SPICELIB Functions
!

!
!     Buffer declarations
!

      INTEGER               NCOL
      PARAMETER           ( NCOL  = 40 )

      INTEGER               NCOMP
      PARAMETER           ( NCOMP = 10 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 1600 )

      CHARACTER*(LNSIZE)    BUFFER ( NCOL, NCOMP )

      INTEGER               I
      INTEGER               J

      LOGICAL               FIRST

      SAVE

      DATA                  FIRST / .TRUE. /



      RETURN

!
!     Set a column component value.
!
      ENTRY SCOLMN ( N, COMP, VALUE )

         IF ( FIRST ) THEN

            DO I = 1, NCOL
               DO J = 1, NCOMP
                  BUFFER(I,J) = ' '
               END DO
            END DO

            FIRST = .FALSE.

         END IF

         IF (      N    .GE. 1 &
             .AND. N    .LE. NCOL &
             .AND. COMP .GE. 1 &
             .AND. COMP .LE. NCOMP) THEN

            BUFFER( N, COMP ) = VALUE

         END IF

         RETURN

!
!     Get a column component value.
!
      ENTRY GCOLMN ( N, COMP, VALUE, WDTH )

         IF ( FIRST ) THEN

            DO I = 1, NCOL
               DO J = 1, NCOMP
                  BUFFER(I,J) = ' '
               END DO
            END DO

            FIRST = .FALSE.

         END IF

         IF (      N    .GE. 1 &
             .AND. N    .LE. NCOL &
             .AND. COMP .GE. 1 &
             .AND. COMP .LE. NCOMP) THEN

            VALUE = BUFFER( N, COMP )
            WDTH  = RTRIM ( VALUE   )

         ELSE

            VALUE = ' '
            WDTH  = 1

         END IF

         RETURN

      ENTRY CCOLMN

         DO I = 1, NCOL
            DO J = 1, NCOMP
               BUFFER(I,J) = ' '
            END DO
         END DO
         RETURN


      END
