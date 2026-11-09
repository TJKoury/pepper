!$Procedure T_CTRBEQF ( Set Equal ZZBODS2C and ZZNAMFRM Counters )

      SUBROUTINE T_CTRBEQF (  )
      use pepper_inc_zzctr


! $ Abstract
!
!     This routine makes the ZZBODTRN and ZZNAMFRM state counters equal
!     to each other by ``running'' up the counter that's behind to the
!     value of the counter that's ahead.
!
!     Setting these counters to the same value is instrumental in
!     testing the routines that uses these counters to verify that the
!     saved body and frame IDs can be stale and have to be re-looked
!     up.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST UTILITY
!
! $ Declarations

      IMPLICIT NONE


! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     WARNING: this routine relies on the counter implementation as of
!     N0065. If that implementation changes, this routine should change
!     to match the new implementation.
!
!     This routine checks which of the two counters (ZZBODTRN and
!     ZZBODS2C) is behind and runs that counter up to make the counters
!     equal by either repeatedly calling BODDEF (if ZZBODTRN counter is
!     behind) or by calling PIPOOL (if ZZNAMFRM counter is behind).
!
! $ Examples
!
!     See F_ZZBODS2C and F_ZZNAMFRM.
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
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 31-MAR-2014 (BVS)
!
!
!-&

!
!     SPICELIB functions.
!
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

!
!     Local variables
!
      INTEGER               FRMCTR ( CTRSIZ )

      CHARACTER*(WDSIZE)    FRNAME
      INTEGER               FRCODE
      LOGICAL               FOUND

      CHARACTER*(WDSIZE)    SAVNAM
      INTEGER               SAVCDE

      INTEGER               BODCTR ( CTRSIZ )

      CHARACTER*(WDSIZE)    BODY
      INTEGER               BODYID

      CHARACTER*(WDSIZE)    SVBODY
      INTEGER               SVBDID
      LOGICAL               SVFND

      INTEGER               SAVCTR ( CTRSIZ )

      LOGICAL               UPDATE

      INTEGER               IVALS ( 1 )

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_CTRBEQF' )
      END IF

!
!     Get the current body name/ID translation state counter.
!
      BODY   = 'SUN'
      BODYID = -10
      FOUND  = .FALSE.
      SVBODY = 'EARTH'
      SVBDID = 399
      SVFND  = .TRUE.
      CALL ZZCTRUIN ( BODCTR )

      CALL ZZBODS2C ( BODCTR, SVBODY, SVBDID, SVFND, &
                      BODY, BODYID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CTRBEQF' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'ZZBODS2C error: expected FOUND = ' // &
                       '.TRUE., got FOUND = .FALSE.'       )
         CALL SIGERR ( 'SPICE(ZZBODS2CFAILED1)'            )
         CALL CHKOUT ( 'T_CTRBEQF'                         )
         RETURN
      END IF

      IF ( BODYID .NE. 10 ) THEN
         CALL SETMSG ( 'ZZBODS2C error: expected BODYID = '// &
                       '10, got BODYID = #.'               )
         CALL ERRINT ( '#', BODYID                         )
         CALL SIGERR ( 'SPICE(ZZBODS2CFAILED2)'            )
         CALL CHKOUT ( 'T_CTRBEQF'                         )
         RETURN
      END IF

!
!     Get the current frame name/ID translation state counter.
!
      FRNAME = 'J2000'
      FRCODE = -1
      SAVNAM = 'ECLIPJ2000'
      SAVCDE = 17
      CALL ZZCTRUIN ( FRMCTR )

      CALL ZZNAMFRM ( FRMCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CTRBEQF' )
         RETURN
      END IF

      IF ( FRCODE .NE. 1 ) THEN
         CALL SETMSG ( 'ZZNAMFRM error: expected FRCODE = '// &
                       '1, got FRCODE = #.'                )
         CALL ERRINT ( '#', FRCODE                         )
         CALL SIGERR ( 'SPICE(ZZNAMFRMFAILED1)'            )
         CALL CHKOUT ( 'T_CTRBEQF'                         )
         RETURN
      END IF

!
!     Check counters against each other to see if one of them
!     needs to be run up to match the other.
!
      CALL MOVEI ( BODCTR, CTRSIZ, SAVCTR )

      CALL ZZCTRCHK ( FRMCTR, SAVCTR, UPDATE )

      IF ( UPDATE ) THEN

!
!        They are different. Which one is behind? NOTE: this check
!        relies on the two-integer counter implementation as of N0065.
!
         IF ( ( FRMCTR(2) .GT. BODCTR(2)       ) .OR. &
              ( FRMCTR(2) .EQ. BODCTR(2) .AND. &
                FRMCTR(1) .GT. BODCTR(1)       )      ) THEN

!
!           The body counter is behind. Run it up to the frame
!           counter by repeatedly calling BODDEF.
!
            DO WHILE ( UPDATE )

               CALL BODDEF( 'SUN', 10 )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_CTRBEQF' )
                  RETURN
               END IF

               BODY   = 'SUN'
               BODYID = -10
               FOUND  = .FALSE.
               SVBODY = 'EARTH'
               SVBDID = 399
               SVFND  = .TRUE.
               CALL ZZCTRUIN ( BODCTR )

               CALL ZZBODS2C ( BODCTR, SVBODY, SVBDID, SVFND, &
                               BODY, BODYID, FOUND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_CTRBEQF' )
                  RETURN
               END IF

               IF ( .NOT. FOUND ) THEN
                  CALL SETMSG ( 'ZZBODS2C error: expected FOUND = ' // &
                                '.TRUE., got FOUND = .FALSE.'       )
                  CALL SIGERR ( 'SPICE(ZZBODS2CFAILED3)'            )
                  CALL CHKOUT ( 'T_CTRBEQF'                         )
                  RETURN
               END IF

               IF ( BODYID .NE. 10 ) THEN
                  CALL SETMSG ( 'ZZBODS2C error: expected BODYID = '// &
                                '10, got BODYID = #.'               )
                  CALL ERRINT ( '#', BODYID                         )
                  CALL SIGERR ( 'SPICE(ZZBODS2CFAILED4)'            )
                  CALL CHKOUT ( 'T_CTRBEQF'                         )
                  RETURN
               END IF

               CALL MOVEI ( BODCTR, CTRSIZ, SAVCTR )

               CALL ZZCTRCHK ( FRMCTR, SAVCTR, UPDATE )

            END DO

         ELSE

!
!           The frame counter is behind. Run it up to the body
!           counter by repeatedly calling PIPOOL.
!
            DO WHILE ( UPDATE )

               IVALS(1) = 0
               CALL PIPOOL ( 'F_CTRBEQF_DUMMY', 1, IVALS )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_CTRBEQF' )
                  RETURN
               END IF

               FRNAME = 'J2000'
               FRCODE = -1
               SAVNAM = 'ECLIPJ2000'
               SAVCDE = 17
               CALL ZZCTRUIN ( FRMCTR )

               CALL ZZNAMFRM ( FRMCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_CTRBEQF' )
                  RETURN
               END IF

               IF ( FRCODE .NE. 1 ) THEN
                  CALL SETMSG ( 'ZZNAMFRM error: expected FRCODE = '// &
                                '1, got FRCODE = #.'                )
                  CALL ERRINT ( '#', FRCODE                         )
                  CALL SIGERR ( 'SPICE(ZZNAMFRMFAILED2)'            )
                  CALL CHKOUT ( 'T_CTRBEQF'                         )
                  RETURN
               END IF

               CALL MOVEI ( BODCTR, CTRSIZ, SAVCTR )

               CALL ZZCTRCHK ( FRMCTR, SAVCTR, UPDATE )

            END DO

         END IF

      END IF

      CALL CHKOUT ( 'T_CTRBEQF' )
      RETURN

      END
