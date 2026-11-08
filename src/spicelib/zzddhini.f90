!$Procedure ZZDDHINI ( Private --- DDH Initialize Structures )

      SUBROUTINE ZZDDHINI ( NATBFF, &
                            SUPBFF, &
                            NUMSUP, &
                            STRAMH, &
                            STRARC, &
                            STRBFF  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize ZZDDHMAN data structures.
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

      INTEGER               NATBFF
      INTEGER               SUPBFF ( * )
      INTEGER               NUMSUP
      CHARACTER*(*)         STRAMH ( * )
      CHARACTER*(*)         STRARC ( * )
      CHARACTER*(*)         STRBFF ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NATBFF     O   Native binary file format.
!     SUPBFF     O   List of supported binary file formats for reading.
!     NUMSUP     O   Number of entries returned in SUPBFF.
!     STRAMH     O   List of labels for METHOD ID codes
!     STRARC     O   List of labels for ARCH ID codes
!     STRBFF     O   List of labels for BFF ID codes
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     NATBFF     is an integer code for the binary file format native
!                to this system as described in the include file
!                'zzddhman.inc'.  Possible values are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!     SUPBFF     is an array of integer codes for the binary file
!                formats supports for reading.  At the very minimum
!                it includes NATBFF, but potentially NUMBFF entries.
!                See the include file 'zzddhman.inc'.
!
!     NUMSUP     is the number of entries in the SUPBFF list.
!
!     STRAMH     is a list of strings containing the labels for the
!                access method ID codes defined in the include file
!                'zzddhman.inc'.  These values are retrieved from
!                ZZDDHGSD.  See it for details.
!
!     STRARC     is a list of strings containing the labels for the
!                file architecture ID codes defined in the include file
!                'zzddhman.inc'.  These values are retrieved from
!                ZZDDHGSD.  See it for details.
!
!     STRBFF     is a list of strings containing the labels for the
!                binary file format ID codes defined in the include
!                file 'zzddhman.inc'.  These values are retrieved from
!                ZZDDHGSD.  See it for details.
!
! $ Parameters
!
!     See the include file 'zzddhman.inc'.
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
!     This routine populates a variety data structures that ZZDDHMAN
!     requires to perform its functions.
!
! $ Examples
!
!     See ZZDDHMAN for sample usage.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 03-OCT-2021 (NJB)
!
!        Corrected typo in long error message. Reordered header
!        sections.
!
!-    SPICELIB Version 1.0.0, 13-AUGUST-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               ISRCHC
      LOGICAL               RETURN

!
!     Local Variables.
!
      CHARACTER*(STRLEN)    LINSTR
      CHARACTER*(STRSIZ)    TMPSTR

      INTEGER               I

      LOGICAL               DONE

!
!     Standard SPICE error handling with discovery check in/out.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Populate the STR### arrays.
!
      DO I = 1, NUMAMH
         CALL ZZDDHGSD ( 'METHOD', I, STRAMH(I) )
      END DO

      DO I = 1, NUMARC
         CALL ZZDDHGSD ( 'ARCH', I, STRARC(I) )
      END DO

      DO I = 1, NUMBFF
         CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
      END DO

!
!     Get the native binary file format.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
      CALL UCASE    ( TMPSTR,        TMPSTR )

      NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )

      IF ( NATBFF .EQ. 0 ) THEN

         CALL CHKIN  ( 'ZZDDHINI'                                   )
         CALL SETMSG ( 'The binary file format, ''#'', is not ' &
         //            'supported by this version of the toolkit. ' &
         //            'This is a serious problem, contact NAIF.'   )
         CALL ERRCH  ( '#', TMPSTR                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'ZZDDHINI'                                   )
         RETURN

      END IF

!
!     Now fetch the list of supported binary file formats.
!
      CALL ZZPLATFM ( 'READS_BFF', LINSTR )

!
!     Parse the wordlist that is sitting in LINSTR.
!
      I = 0
      DONE = .FALSE.

      DO WHILE ( .NOT. DONE )

!
!        Increment the counter and pop the next word
!        off.
!
         I = I + 1
         CALL NEXTWD ( LINSTR, TMPSTR, LINSTR )

!
!        See if we're done.
!
         DONE = ( I .GT. NUMBFF ) .OR. ( TMPSTR .EQ. ' ' )

!
!        If we're not done, then convert this string to the
!        appropriate integer code.
!
         IF ( .NOT. DONE ) THEN

            SUPBFF(I) = ISRCHC ( TMPSTR, NUMBFF, STRBFF )

!
!           Check to see if the binary file format listed
!           is properly supported.
!
            IF ( SUPBFF(I) .EQ. 0 ) THEN

               CALL CHKIN  ( 'ZZDDHINI'                             )
               CALL SETMSG ( 'The binary file format, ''#'', is ' &
               //            'not supported by this version of ' &
               //            'the toolkit. This is a serious ' &
               //            'problem, contact NAIF.            '   )
               CALL ERRCH  ( '#', TMPSTR                            )
               CALL SIGERR ( 'SPICE(BUG)'                           )
               CALL CHKOUT ( 'ZZDDHINI'                             )
               RETURN

            END IF

         END IF

      END DO

!
!     Now setup NUMSUP.  Given the way the WHILE loop above executes,
!     we need to subtract one from I to get the number of entries added
!     to SUPBFF.  This smacks of kludge... but it works.
!
      NUMSUP = I - 1

      RETURN
      END
