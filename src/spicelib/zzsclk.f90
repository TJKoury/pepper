!$Procedure    ZZSCLK ( Is there and SCLK for a CKID )

      LOGICAL FUNCTION ZZSCLK ( CKID, SCLKID )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine whether or not an SCLK kernel is available for mapping
!     ET to Ticks and back again for a particular C-kernel ID.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTITILITY
!
! $ Declarations

      IMPLICIT NONE
      INTEGER               CKID
      INTEGER               SCLKID

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CKID       I   CK ID-code for the object of interest.
!     SCLKID     I   Idcode to pass to the SCLOCK routines for CKID
!
!     The function returns TRUE is SCLK information is available.
!
! $ Detailed_Input
!
!     CKID       is the C-kernel ID-code for some object for which
!                and SCLK is required.
!
!     SCLKID     is the ID-code to pass to SCE2C to convert ET times
!                to ticks.
!
! $ Detailed_Output
!
!     The function returns TRUE if an SCLK specification is present
!     in the kernel pool that is suitable for mapping ticks to ET and
!     back for the C-kernel object specified by CKID.  If such
!     information is not available, or is deemed to be corrupt or
!     incomplete, the function returns FALSE.
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
!     None.
!
! $ Particulars
!
!     This is a utility for checking that sufficient SCLK information
!     is available for mapping between ET and SCLK for the object
!     specified by CKID
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 15-AUG-2000 (WLT)
!
!        Removed the check fo the SCLK Time system as it is not
!        formally required for an SCLK specification to be complete.
!
!-    SPICELIB Version 1.0.0, 17-FEB-2000 (WLT)
!
!-&

! $ Index_Entries
!
!     Determine whether a file specifies and SCLK
!
!-&
!     SPICELIB Functions
!
      INTEGER               CARDI
      INTEGER               SIZEI

      LOGICAL               RETURN
      LOGICAL               ELEMI

!
!     Local Variables
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXIDS
      PARAMETER           ( MAXIDS = 10 )


      CHARACTER*(WDSIZE)    SCLK
      CHARACTER*(WDSIZE)    SCLKVR ( 7 )
      CHARACTER*(WDSIZE)    TYPE
      CHARACTER*(WDSIZE)    AGENT

      INTEGER               DTSIZE ( 7 )
      INTEGER               I
      INTEGER               N

      INTEGER               KNOWN  ( LBCELL : MAXIDS )
      INTEGER               PASSED ( LBCELL : MAXIDS )

      LOGICAL               KEEPID
      LOGICAL               UPDATE
      LOGICAL               WATCH
      LOGICAL               FIRST

      LOGICAL               FOUND

      SAVE                  KNOWN
      SAVE                  PASSED
      SAVE                  FIRST
      SAVE                  DTSIZE

      DATA                  FIRST / .TRUE. /


      ZZSCLK = .FALSE.
!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSCLK')

      IF ( FIRST ) THEN
         FIRST = .FALSE.

         DTSIZE(1) = 1
         DTSIZE(2) = 1
         DTSIZE(3) = 1
         DTSIZE(4) = 1
         DTSIZE(5) = 3
         DTSIZE(6) = 1
         DTSIZE(7) = 1

         CALL SSIZEI ( MAXIDS, KNOWN )
         CALL SSIZEI ( MAXIDS, PASSED )
      END IF
!
!     We've got a text kernel (or meta kernel). See if there is an
!     SCLK kernel loaded for the CKID provided in the calling inputs.
!     If not, we'll use the default -CKID/1000 for the SCLK ID.
!
      CALL INTSTR ( -SCLKID, SCLK )
      AGENT = 'ZZSCLK' // SCLK

!
!     See if this is an ID-code we've encountered before.  If it
!     is we can make use of stored knowledge about this ID-code.
!
      IF ( ELEMI( SCLKID, KNOWN ) ) THEN

         WATCH  = .FALSE.
         KEEPID = .TRUE.
         CALL CVPOOL ( AGENT, UPDATE )

      ELSE IF ( CARDI(KNOWN) .LT. SIZEI(KNOWN) ) THEN
!
!        The SCLKID specified is not in the list of SCLKIDs for
!        this routine and there is room left in the pool of
!        SCLKIDs to keep track of one more.  Put this ID into
!        the list of known IDS
!
         CALL INSRTI( SCLKID, KNOWN )
         UPDATE = .TRUE.
         WATCH  = .TRUE.
         KEEPID = .TRUE.

      ELSE

         UPDATE = .TRUE.
         KEEPID = .FALSE.
         WATCH  = .FALSE.

      END IF

      IF ( .NOT. UPDATE ) THEN
!
!        Nothing has changed in the kernel pool w.r.t this agent.
!        The test for an SCLK will not have changed either.
!
         ZZSCLK = ELEMI( SCLKID, PASSED )
         CALL CHKOUT ( 'ZZSCLK' )
         RETURN

      END IF
!
!     If we are still here, we need to look in the kernel pool
!     to see if we have an SCLK for this object.
!
!     Construct all of the expected SCLK variables are
!     available for this SCLK.
!
      SCLKVR(1) = 'SCLK_DATA_TYPE_'       // SCLK
      SCLKVR(2) = 'SCLK01_N_FIELDS_'      // SCLK
      SCLKVR(3) = 'SCLK01_MODULI_'        // SCLK
      SCLKVR(4) = 'SCLK01_OFFSETS_'       // SCLK
      SCLKVR(5) = 'SCLK01_COEFFICIENTS_'  // SCLK
      SCLKVR(6) = 'SCLK_PARTITION_START_' // SCLK
      SCLKVR(7) = 'SCLK_PARTITION_END_'   // SCLK
!
!     If we are supposed to watch for this agent, we add him to
!     the list of kernel pool agents.
!
      IF ( WATCH ) THEN
         CALL SWPOOL ( AGENT, 7, SCLKVR )
         CALL CVPOOL ( AGENT,    UPDATE )
      END IF
!
!     Check for all of the required variables and structure in
!     the kernel pool.
!
      DO I = 1, 7

         CALL DTPOOL ( SCLKVR(I), FOUND, N, TYPE )

         IF (       .NOT. FOUND &
              .OR.  TYPE .NE. 'N' &
              .OR. (N/DTSIZE(I))*DTSIZE(I) .NE. N   )THEN
!
!           We don't have adequate SCLK data for the specified
!           object.  Remove this AGENT from the list of agents
!           that have passed the test.
!
            CALL REMOVI ( SCLKID, PASSED )
            CALL CHKOUT ( 'ZZSCLK' )
            RETURN
         END IF

      END DO
!
!     Once we get to this point, we know we have SCLK data.  If
!     there is room to WATCH for this agent,
!
      IF ( KEEPID ) THEN
         CALL INSRTI ( SCLKID, PASSED )
      END IF

!
!     As far as we can tell, everything looks ok.
!
      ZZSCLK = .TRUE.
      CALL CHKOUT ( 'ZZSCLK' )
      RETURN
      END
