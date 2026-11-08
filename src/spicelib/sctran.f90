!$Procedure SCTRAN  ( SCLK name/ID code translation )

      SUBROUTINE SCTRAN ( CLKNAM, CLKID, FOUND )

! $ Abstract
!
!     Convert between SCLK name strings and ID codes.
!
! $ Required_Reading
!
!     SCLK
!
! $ Keywords
!
!     CONVERSION
!     PARSING
!     SCLK
!     TIME
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         CLKNAM
      INTEGER               CLKID
      LOGICAL               FOUND

      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     CLKNAM    I-O  SCID2N, SCN2ID
!     CLKID     I-O  SCID2N, SCN2ID
!     FOUND      O   SCID2N, SCN2ID
!     MAXLEN     P   All
!
! $ Detailed_Input
!
!     See the entry points for a discussion of their arguments.
!
! $ Detailed_Output
!
!     See the entry points for a discussion of their arguments.
!
! $ Parameters
!
!     MAXLEN   is the maximum allowed length, in characters, of a
!              string containing the name of a spacecraft clock.
!
! $ Exceptions
!
!     See the entry points for a discussion of exceptions specific to
!     those routines.
!
!     1)  This is an umbrella subroutine that contains declarations
!         for its entry points. This routine should never be called
!         directly. If it is, the error SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This set of subroutines centralizes the mapping between
!     spacecraft clock names and their corresponding NAIF integer
!     codes. Translation between these names and codes is frequently
!     required by user interface functions.
!
!     The set of supported clocks is identical to the set of spacecraft
!     supported by BODTRN. The mapping may be extended by calling
!     BODDEF.
!
! $ Examples
!
!     See the entry points for examples of their usage.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 29-OCT-2001 (NJB)
!
!        Bug fix: modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.
!
!-    SPICELIB Version 1.1.0, 25-FEB-2000 (NJB)
!
!        Updated to use BODTRN for SCLK name/code mapping.
!
!-    SPICELIB Version 1.0.0, 17-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     convert between SCLK ID codes and names
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 29-OCT-2001 (NJB)
!
!        Bug fix: modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.  SCN2ID now uses POSR to locate
!        the substring "SCLK" in the input string.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               POSR
      INTEGER               RTRIM

      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(MAXLEN)    TMPNAM

      INTEGER               LOC

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCTRAN' )
      END IF

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'SCTRAN' )
      RETURN





!$Procedure SCN2ID  ( SCLK name to ID code )

      ENTRY SCN2ID ( CLKNAM, CLKID, FOUND )

! $ Abstract
!
!     Convert an SCLK name string to a NAIF integer code.
!
! $ Required_Reading
!
!     SCLK
!
! $ Keywords
!
!     CONVERSION
!     PARSING
!     SCLK
!     TIME
!     UTILITY
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         CLKNAM
!     INTEGER               CLKID
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CLKNAM     I   String giving spacecraft clock name.
!     CLKID      O   NAIF integer code of spacecraft clock.
!     FOUND      O   Flag indicating whether item was found.
!
! $ Detailed_Input
!
!     CLKNAM   is a short string identifying the spacecraft
!              clock of interest. The form of the string
!              is:
!
!                 <spacecraft name or acronym> SCLK
!
!              for example
!
!                 VGR1 SCLK
!                 VOYAGER 1 SCLK
!                 GLL SCLK
!                 GALILEO ORBITER SCLK
!
!              Case and white space (including embedded white
!              space) are not significant.
!
! $ Detailed_Output
!
!     CLKID    is the NAIF integer code associated with the
!              input clock. CLKID is defined only if the
!              output flag FOUND is returned .TRUE.
!
!     FOUND    is a logical flag indicating whether the input
!              string specified a clock known to this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the input name is not recognized, FOUND is set to .FALSE.
!         CLKID is not modified.
!
!     2)  If the input name is recognized but does not refer to a
!         spacecraft, no error is signaled. For example, the string
!         'JUPITER BARYCENTER SCLK' maps to the code 5.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     SCN2ID provides a means of mapping human-readable clock names
!     to integer codes used by the SPICELIB SCLK routines to
!     identify spacecraft clocks.
!
! $ Examples
!
!     1)  Look up the spacecraft clock code for the Galileo orbiter.
!
!            CALL SCN2ID ( 'GLL SCLK', CLKID, FOUND )
!
!         The outputs will be
!
!            CLKID  =  -77
!            FOUND  =  .TRUE.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 12-AUG-2001 (NJB)
!
!        Bug fix: modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.
!
!-    SPICELIB Version 1.1.0, 25-FEB-2000 (NJB)
!
!        Updated to use BODTRN for SCLK name/code mapping.
!
!-    SPICELIB Version 1.0.0, 17-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     convert an SCLK name to an SCLK ID code
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 12-AUG-2001 (NJB)
!
!        Bug fix: modified algorithm to handle case where string
!        "SCLK" appears in SCLK name.  SCN2ID now uses POSR to locate
!        the substring "SCLK" in the input string.
!
!-&


!
!     Convert name to upper case.
!
      CALL UCASE ( CLKNAM, TMPNAM )

!
!     Remove the final occurrence of the  string 'SCLK' from
!     the input name.
!
      LOC = POSR ( TMPNAM, 'SCLK', RTRIM(TMPNAM) )


      IF ( LOC .GT. 0 ) THEN
         TMPNAM(LOC:LOC+3) = ' '
      END IF

      CALL BODN2C ( TMPNAM, CLKID, FOUND )

      RETURN







!$Procedure SCID2N  ( SCLK ID code to name )

      ENTRY SCID2N ( CLKID, CLKNAM, FOUND )

! $ Abstract
!
!     Convert a NAIF integer code for a spacecraft clock to an SCLK name
!     string.
!
! $ Required_Reading
!
!     SCLK
!
! $ Keywords
!
!     CONVERSION
!     PARSING
!     SCLK
!     TIME
!     UTILITY
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               CLKID
!     CHARACTER*(*)         CLKNAM
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CLKID      I   NAIF integer code of spacecraft clock.
!     CLKNAM     O   String giving spacecraft clock name.
!     FOUND      O   Flag indicating whether item was found.
!
! $ Detailed_Input
!
!     CLKID    is the NAIF integer code of a spacecraft clock of
!              interest.
!
! $ Detailed_Output
!
!     CLKNAM   is a short, human-readable string identifying
!              the specified spacecraft clock. The returned
!              string has the form
!
!                 <spacecraft name or acronym> SCLK
!
!              where the spacecraft name is the same string
!              returned by BODC2N when CLKID is supplied as the
!              input code.
!
!              CLKNAM is defined only if the output flag FOUND is
!              returned .TRUE.
!
!     FOUND    is a logical flag indicating whether the input
!              code specified a clock known to this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the input code is not recognized, FOUND is set to .FALSE.
!         CLKNAM is not modified.
!
!     2)  If the input code is recognized but does not refer to a
!         spacecraft, no error is signaled. For example, the code
!         5 maps to the string 'JUPITER BARYCENTER SCLK'.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine converts a NAIF spacecraft clock code to a human-
!     readable string. This function is useful for constructing
!     messages.
!
! $ Examples
!
!     1)  Look up the spacecraft clock name for code -77.
!
!            CALL SCID2N ( -77, CLKNAM, FOUND )
!
!         The outputs will be
!
!            CLKNAM  =  'GALILEO ORBITER SCLK'
!            FOUND   =  .TRUE.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
!
!        Updated to use BODTRN for SCLK name/code mapping.
!
!-    SPICELIB Version 1.0.0, 17-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     convert an SCLK name to an SCLK ID code
!
!-&


      CALL BODC2N ( CLKID, CLKNAM, FOUND )

      IF ( .NOT. FOUND ) THEN
         RETURN
      END IF

      CALL SUFFIX ( 'SCLK', 1, CLKNAM )

      RETURN
      END
