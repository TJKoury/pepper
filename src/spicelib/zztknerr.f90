!$Procedure  ZZTKNERR ( Create ZZTOKNS overflow error message )

      SUBROUTINE ZZTKNERR ( TEMPLT, STRING, TOKEN, ERROR, STATUS )

      IMPLICIT NONE

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Create an error message for ZZTOKNS token buffer and picture
!     overflow exceptions.
!
! $ Required_Reading
!
!     TIME
!
! $ Keywords
!
!     TIME
!     PRIVATE
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         TEMPLT
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TOKEN
      CHARACTER*(*)         ERROR
      LOGICAL               STATUS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TEMPLT     I   Error message template
!     STRING     I   First string to be substituted
!     TOKEN      I   Second string to be substituted
!     ERROR      O   Complete error message
!     STATUS     O   Status flag
!
! $ Detailed_Input
!
!     TEMPLT         is the input error message template. It should
!                    have two '#' characters in it.
!
!     STRING         is the string that should replace the first '#' in
!                    the template.
!
!     TOKEN          is the string that should replace the second '#'
!                    in the template.
!
! $ Detailed_Output
!
!     ERROR          is the complete output error message.
!
!     STATUS         is the output status flag always set .FALSE.
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
!     The sole purpose of this routine is to encapsulate in a single
!     routine the two REPMC calls and one ZZTOKNS = .FALSE. assignments
!     needed to make the error messages and returned status used in
!     numerous overflow check IF blocks in ZZTOKNS.
!
! $ Examples
!
!     For these inputs
!
!        TEMPLT = 'This time string ''#'' has a bad token ''#''.'
!
!        STRING = '2001-01-01 boo 00:00:00'
!
!        TOKEN  = 'boo'
!
!     this routine returns
!
!        ERROR  =  'This time string ''2001-01-01 boo 00:00:00'' has '
!       .          'a bad token ''boo''.'
!
!        STATUS = .FALSE.
!
! $ Restrictions
!
!     1) This is a private routine. SPICE user applications should not
!        call this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2014 (BVS)
!
!-&

! $ Index_Entries
!
!     create error message for ZZTOKNS overflow failure
!
!-&

      CALL REPMC ( TEMPLT, '#', STRING, ERROR )
      CALL REPMC ( ERROR,  '#', TOKEN,  ERROR )
      STATUS = .FALSE.

      RETURN

      END
