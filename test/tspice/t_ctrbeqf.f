C$Procedure T_CTRBEQF ( Set Equal ZZBODS2C and ZZNAMFRM Counters )
 
      SUBROUTINE T_CTRBEQF (  )

 
C$ Abstract
C
C     This routine makes the ZZBODTRN and ZZNAMFRM state counters equal
C     to each other by ``running'' up the counter that's behind to the
C     value of the counter that's ahead.
C
C     Setting these counters to the same value is instrumental in
C     testing the routines that uses these counters to verify that the
C     saved body and frame IDs can be stale and have to be re-looked
C     up.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     WARNING: this routine relies on the counter implementation as of
C     N0065. If that implementation changes, this routine should change
C     to match the new implementation.
C
C     This routine checks which of the two counters (ZZBODTRN and
C     ZZBODS2C) is behind and runs that counter up to make the counters
C     equal by either repeatedly calling BODDEF (if ZZBODTRN counter is
C     behind) or by calling PIPOOL (if ZZNAMFRM counter is behind).
C
C$ Examples
C
C     See F_ZZBODS2C and F_ZZNAMFRM.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 31-MAR-2014 (BVS)
C
C
C-&

C
C     SPICELIB functions.
C
      LOGICAL               RETURN
      LOGICAL               FAILED

C
C     Local parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

C
C     Local variables
C
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

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_CTRBEQF' )
      END IF

C
C     Get the current body name/ID translation state counter.
C
      BODY   = 'SUN'
      BODYID = -10
      FOUND  = .FALSE.
      SVBODY = 'EARTH'
      SVBDID = 399
      SVFND  = .TRUE.
      CALL ZZCTRUIN ( BODCTR )

      CALL ZZBODS2C ( BODCTR, SVBODY, SVBDID, SVFND,
     .                BODY, BODYID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CTRBEQF' )
         RETURN 
      END IF

      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'ZZBODS2C error: expected FOUND = ' //
     .                 '.TRUE., got FOUND = .FALSE.'       )
         CALL SIGERR ( 'SPICE(ZZBODS2CFAILED1)'            )
         CALL CHKOUT ( 'T_CTRBEQF'                         )
         RETURN          
      END IF
               
      IF ( BODYID .NE. 10 ) THEN
         CALL SETMSG ( 'ZZBODS2C error: expected BODYID = '//
     .                 '10, got BODYID = #.'               )
         CALL ERRINT ( '#', BODYID                         )
         CALL SIGERR ( 'SPICE(ZZBODS2CFAILED2)'            )
         CALL CHKOUT ( 'T_CTRBEQF'                         )
         RETURN          
      END IF

C
C     Get the current frame name/ID translation state counter.
C
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
         CALL SETMSG ( 'ZZNAMFRM error: expected FRCODE = '//
     .                 '1, got FRCODE = #.'                )
         CALL ERRINT ( '#', FRCODE                         )
         CALL SIGERR ( 'SPICE(ZZNAMFRMFAILED1)'            )
         CALL CHKOUT ( 'T_CTRBEQF'                         )
         RETURN          
      END IF

C
C     Check counters against each other to see if one of them
C     needs to be run up to match the other.
C      
      CALL MOVEI ( BODCTR, CTRSIZ, SAVCTR )
      
      CALL ZZCTRCHK ( FRMCTR, SAVCTR, UPDATE )
      
      IF ( UPDATE ) THEN

C
C        They are different. Which one is behind? NOTE: this check
C        relies on the two-integer counter implementation as of N0065.
C
         IF ( ( FRMCTR(2) .GT. BODCTR(2)       ) .OR.
     .        ( FRMCTR(2) .EQ. BODCTR(2) .AND. 
     .          FRMCTR(1) .GT. BODCTR(1)       )      ) THEN

C
C           The body counter is behind. Run it up to the frame
C           counter by repeatedly calling BODDEF.
C           
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

               CALL ZZBODS2C ( BODCTR, SVBODY, SVBDID, SVFND,
     .                         BODY, BODYID, FOUND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_CTRBEQF' )
                  RETURN
               END IF

               IF ( .NOT. FOUND ) THEN
                  CALL SETMSG ( 'ZZBODS2C error: expected FOUND = ' //
     .                          '.TRUE., got FOUND = .FALSE.'       )
                  CALL SIGERR ( 'SPICE(ZZBODS2CFAILED3)'            )
                  CALL CHKOUT ( 'T_CTRBEQF'                         )
                  RETURN          
               END IF
               
               IF ( BODYID .NE. 10 ) THEN
                  CALL SETMSG ( 'ZZBODS2C error: expected BODYID = '//
     .                          '10, got BODYID = #.'               )
                  CALL ERRINT ( '#', BODYID                         )
                  CALL SIGERR ( 'SPICE(ZZBODS2CFAILED4)'            )
                  CALL CHKOUT ( 'T_CTRBEQF'                         )
                  RETURN          
               END IF
               
               CALL MOVEI ( BODCTR, CTRSIZ, SAVCTR )
               
               CALL ZZCTRCHK ( FRMCTR, SAVCTR, UPDATE )
                              
            END DO
            
         ELSE
            
C
C           The frame counter is behind. Run it up to the body
C           counter by repeatedly calling PIPOOL.
C           
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
                  CALL SETMSG ( 'ZZNAMFRM error: expected FRCODE = '//
     .                          '1, got FRCODE = #.'                )
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
