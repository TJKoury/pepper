!$Procedure      M2HAVE ( META/2 --- How many matches do we have )

      INTEGER FUNCTION M2HAVE ( NAME )
      IMPLICIT NONE

! $ Abstract
!
!     Find the number of matches there were for a particular named
!     META/2 template word.
!
! $ Required_Reading
!
!     META/2 --- a language specification language.
!
! $ Keywords
!
!     META/2
!     PARSING
!
! $ Declarations

      CHARACTER*(*)         NAME

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   The name of a META/2 template word.
!
!     M2HAVE is returned with the number of words that matched the
!     template word specified by NAME.
!
! $ Detailed_Input
!
!     NAME       is the name of a META/2 template word that may have
!                been matched by a call to M2GMCH.  The case of NAME
!                is significant.  'BOB', 'Bob', and 'bob' will be
!                regarded as different names.
!
! $ Detailed_Output
!
!     M2HAVE     is the number of matches that were made agains
!                the named META/2 template word specified by NAME.
!                If there were no matches, M2HAVE is returned as zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Once a string has matched some META/2 template, one normally
!     needs to retrieve the information in the string.  In some cases
!     the META/2 template will allow for a variable number of
!     matches with a particular template word.  To extract the
!     information in the string, it is necessary to determine how many
!     words matched the template word in question.
!
!     This routine exists so that you can easily find out how many
!     matches there were for a particular named template word.
!
! $ Examples
!
!     Suppose that a string is known to have matched the following
!     META/2 template.
!
!        FIND UNION OF (2:)@name[sets]
!
!     To accurately carry out the task specified by this string,
!     you will need to find the "names" of the sets specified.
!
!        NSETS = M2HAVE('sets')
!
!        CALL M2GETC ( 'sets', STRING, 1, FOUND, NAME )
!
!           copy the named set into the set UNION.
!
!        DO I = 2, NSETS
!
!           CALL M2GETC ( 'sets', STRING, I, FOUND, NAME )
!
!           form the union of UNION with the set specified by NAME
!
!        END DO
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
!     W.L. Taber     (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!-    Beta Version 1.0.0, 27-NOV-1991 (WLT)
!
!-&
!
! $ Index_Entry
!
!     Check for the presence of a named match in the META/2 tables.
!
!-&

!
!     Local variables
!
      INTEGER               SIZE

      SAVE


!
!     Find out how many endpoints were matched, and put the answer into
!     M2HAVE.
!
      CALL M2VSIZ ( NAME, SIZE )

      M2HAVE = SIZE

      RETURN
      END
