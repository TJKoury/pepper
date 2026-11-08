!$Procedure ZZNAMFRM ( Frame name to ID translation, with bypass )

      subroutine zznamfrm ( usrctr, savnam, savcde, frname, frcode )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Translate a string containing a frame name to its ID code, but
!     bypass calling NAMFRM and return saved value provided by the
!     caller if the name is the same as the saved name and the POOL
!     state did not change.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     PRIVATE
!     FRAME
!
! $ Declarations

      implicit none

      include              'zzctr.inc'

      integer(ip)           usrctr    ( ctrsiz )
      character(*)          savnam
      integer(ip)           savcde
      character(*)          frname
      integer(ip)           frcode

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     USRCTR    I/O  POOL state counter saved by the caller.
!     SAVNAM    I/O  Frame name saved by the caller.
!     SAVCDE    I/O  Frame ID code saved by the caller.
!     FRNAME     I   Frame name.
!     FRCODE     O   Frame ID code.
!     CTRSIZ     P   Counter array size.
!
! $ Detailed_Input
!
!     USRCTR      is the value of the POOL state counter tracked by
!                 (saved in) the caller (user) routine specifically for
!                 this frame name/ID pair of variables.
!
!     SAVNAM      is the frame name saved in the caller routine
!                 specifically for this frame name/ID pair of
!                 variables. For detailed description of allowed values
!                 see description of the FRNAME argument in NAMFRM.
!
!     SAVCDE      is the frame ID code saved in the caller routine
!                 specifically for this frame name/ID pair of
!                 variables. For detailed description of allowed values
!                 see description of the FRCODE argument in NAMFRM.
!
!     FRNAME      is the input frame name. For detailed description of
!                 allowed values see description of the FRNAME argument
!                 in NAMFRM.
!
! $ Detailed_Output
!
!     USRCTR      is the current POOL state counter.
!
!     SAVNAM      is the frame name saved in the caller routine
!                 specifically for this frame name/ID pair of
!                 variables. On the output SAVNAM always equals FRNAME.
!
!     SAVCDE      is the frame ID code saved in the caller routine
!                 specifically for this frame name/ID pair of
!                 variables. If the frame name cannot be mapped to an
!                 ID, FRCODE is returned as 0. On the output SAVCDE
!                 always equals FRCODE.
!
!     FRCODE      is the output frame ID code. For detailed description
!                 of possible values see description of the FRCODE
!                 argument in NAMFRM. If the frame name cannot be
!                 mapped to an ID, FRCODE is returned as 0. On the
!                 output FRCODE always equals SAVCDE.
!
! $ Parameters
!
!     CTRSIZ      is the dimension of the counter array used by
!                 various SPICE subsystems to uniquely identify
!                 changes in their states. This parameter is
!                 defined in the private include file 'zzctr.inc'.
!
! $ Exceptions
!
!     1) Errors may be signaled by routines in the call tree of this
!        routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine translates a string containing a frame name to an
!     integer code but bypasses calling NAMFRM to do the translation
!     and simply returns the saved ID value provided by the caller if
!     the input name is the same as the saved name provided by the
!     caller, the saved ID provided by the caller is not 0, and the
!     POOL state counter tracked by the caller is the same as the
!     current POOL state counter.
!
!     The POOL state counter and name/ID pair of saved variables
!     tracked by the caller must be specific for each frame of
!     interest. I.e. if the caller routine needs to call this routine
!     to do translations for two distinct frames, the caller routine
!     mush use must have its own set of saved POOL state counter and
!     name/ID variables for each of the two frames.
!
! $ Examples
!
!     This example shows how a routine that needs to do frame name-ID
!     conversion for two distinct frames (FRA and FRB) can do it using
!     ZZNAMFRM.
!
!           SUBROUTINE <name> ( FRA, FRB, ... )
!           ...
!           INCLUDE               'zzctr.inc'
!           ...
!     C
!     C     Saved frame name length.
!     C
!           INTEGER               FRNMLN
!           PARAMETER           ( FRNMLN = 32 )
!           ....
!     C
!     C
!     C     Saved name/ID item declarations.
!     C
!           INTEGER               SVCTR1 ( CTRSIZ )
!           CHARACTER*(FRNMLN)    SVFRA
!           INTEGER               SVFRAI
!           INTEGER               SVCTR2 ( CTRSIZ )
!           CHARACTER*(FRNMLN)    SVFRB
!           INTEGER               SVFRBI
!           LOGICAL               FIRST
!     C
!     C     Saved name/ID items.
!     C
!           SAVE                  SVCTR1
!           SAVE                  SVFRA
!           SAVE                  SVFRAI
!           SAVE                  SVCTR2
!           SAVE                  SVFRB
!           SAVE                  SVFRBI
!           SAVE                  FIRST
!     C
!     C     Initial values.
!     C
!           DATA                  FIRST   / .TRUE. /
!           ...
!     C
!     C     Initialization.
!     C
!           IF ( FIRST ) THEN
!     C
!     C        Initialize POOL counters.
!     C
!              CALL ZZCTRUIN( SVCTR1 )
!              CALL ZZCTRUIN( SVCTR2 )
!              FIRST = .FALSE.
!           END IF
!     C
!     C     Starting from translation of FRA name to ID.
!     C
!           CALL ZZNAMFRM ( SVCTR1, SVFRA, SVFRAI, FRA, FRAID )
!
!           IF ( FRAID .EQ. 0 ) THEN
!              CALL SETMSG ( '...' )
!              CALL SIGERR ( 'SPICE(FRAMEAIDCODENOTFOUND)' )
!              CALL CHKOUT ( '<name>' )
!              RETURN
!           END IF
!     C
!     C     Now do the same for FRB.
!     C
!           CALL ZZNAMFRM ( SVCTR2, SVFRB, SVFRBI, FRB, FRBID )
!
!           IF ( FRBID .EQ. 0 ) THEN
!              CALL SETMSG ( '...' )
!              CALL SIGERR ( 'SPICE(FRAMEBIDCODENOTFOUND)' )
!              CALL CHKOUT ( '<name>' )
!              RETURN
!           END IF
!           ...
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
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 23-SEP-2013 (BVS)
!
!-&

!
!     SPICE functions.
!
      logical               return

!
!     Local variables.
!
      logical               update

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Check/update POOL state counter.
!
      call zzpctrck ( usrctr, update )

!
!     Check update flag, saved ID, and saved name against the input.
!
      if ( .not. update       .and. &
           savcde /= 0      .and. &
           savnam == frname       ) then

!
!        No change in the POOL state, the saved name was successfully
!        resolved earlier, and input and saved names are the same.
!        Return saved ID.
!
         frcode = savcde

      else

!
!        Check in because NAMFRM may fail.
!

!
!        POOL state changed, or the saved name was never successfully
!        resolved earlier, or input and saved names are different. Call
!        NAMFRM to look up ID and reset saved values.
!
         call namfrm( frname, frcode )

         savnam = frname
         savcde = frcode

!
!        Check out.
!

      end if

      return

      end
