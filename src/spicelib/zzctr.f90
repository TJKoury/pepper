!$Procedure ZZCTR ( Manipulate Counter Array )

      subroutine zzctr ( newctr, oldctr, update )

! $ Abstract
!
!     Manipulate counter array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      include                'zzctr.inc'

      integer               newctr ( ctrsiz )
      integer               oldctr ( ctrsiz )
      logical               update

! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     NEWCTR     I   ZZCTRCHK
!     OLDCTR    I/O  ZZCTRUIN, ZZCTRSIN, ZZCTRINC, ZZCTRCHK
!     UPDATE     O   ZZCTRCHK
!
!     CTRSIZ     P   (All)
!
! $ Detailed_Input
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Detailed_Output
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Parameters
!
!     CTRSIZ      is the dimension of the counter array used by
!                 various SPICE subsystems to identify
!                 changes in their states. This parameter is
!                 defined in the include file.
!
! $ Exceptions
!
!     1) If ZZCTR is called directly, the error SPICE(BOGUSENTRY) is
!        signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     ZZCTR should never be called directly, but should instead be
!     accessed only through its entry points.
!
!     The purpose of this routine is to manipulate counter arrays used
!     by various SPICE subsystems to identify changes in their states.
!
!           ZZCTRUIN       Initialize counter array kept by a user
!                          routine.
!
!           ZZCTRSIN       Initialize counter array kept by a
!                          subsystem routine.
!
!           ZZCTRINC       Increment counter array.
!
!           ZZCTRCHK       Check/update counter array.
!
!     A counter array consists of CTRSIZ elements representing
!     cascading counters. The fastest counter is at index 1, the
!     slowest counter is at index CTRSIZ. At the start of counting all
!     counter array elements are set to INTMIN. In the process of
!     counting the fastest element is incremented by one. As with any
!     cascading counters when the fastest counter reaches INTMAX it
!     rolls back to INTMIN and the next counter is incremented by 1.
!     When all counters reach INTMAX, ZZCTRINC signals an error.
!
! $ Examples
!
!     The counter array gives subsystems like POOL a way to provide to
!     the user routines of that subsystem's services a simple indicator
!     of whether the subsystem's state has changed, meaning that some
!     saved values may have become stale and need to be updated or some
!     other action needs to be taken in the user routines.
!
!     For that the subsystem initializes a counter array ("subsystem
!     counter") using ZZCTRSIN, saves it, increments it using ZZCTRINC
!     each time the subsystem's state changes, and resets a user
!     counter to subsystem counter and indicates the need for update
!     when counters are not the same, like this:
!
!        C
!        C     Include zzctr.inc to access CTRSIZ.
!        C
!              INCLUDE              'zzctr.inc'
!              ...
!        C
!        C     In input/output variable declarations declare user
!        C     user counter (USRCTR) as I/O and the update flag
!        C     (UPDATE) as O.
!        C
!              INTEGER               USRCTR ( CTRSIZ )
!              LOGICAL               UPDATE
!              ...
!        C
!        C     In local variable declarations declare and save the
!        C     subsystem counter (SUBCTR) that will incremented
!        C     with each change of state.
!        C
!              INTEGER               SUBCTR ( CTRSIZ )
!              ...
!              SAVE                  SUBCTR
!              ...
!        C
!        C     In all places where initialization is done set
!        C     subsystem counter using ZZCTRSIN.
!        C
!              IF ( FIRST ) THEN
!                 ...
!                 CALL ZZCTRSIN( SUBCTR )
!                 FIRST = .FALSE.
!              END IF
!              ...
!        C
!        C     In all places where the subsystem state changes
!        C     increment the subsystem counter using ZZCTRINC.
!        C
!              CALL ZZCTRINC( SUBCTR )
!              ...
!        C
!        C     Make a special entry that checks and resets input
!        C     user counter to the current subsystem counter and
!        C     indicates whether it was updated using ZZCTRCHK.
!        C
!              ENTRY <entry_name> ( USRCTR, UPDATE )
!
!              CALL ZZCTRCHK( SUBCTR, USRCTR, UPDATE )
!
!              RETURN
!              ...
!
!     The users of the subsystem services initialize a counter array
!     ("user counter") using ZZCTRUIN, save it, and check against the
!     subsystem's counter and update, if needed, using the subsystem's
!     entry point, like this:
!
!        C
!        C     Include zzctr.inc to access CTRSIZ.
!        C
!              INCLUDE              'zzctr.inc'
!              ...
!        C
!        C     In local variable declarations declare and save
!        C     user counter. Also declare an update flag.
!        C
!              INTEGER               USRCTR ( CTRSIZ )
!              LOGICAL               UPDATE
!              ...
!              SAVE                  USRCTR
!              ...
!        C
!        C     In all places where initialization is done initialize
!        C     the user counter using ZZCTRUIN to ensure update on
!        C     the first check.
!        C
!              IF ( FIRST ) THEN
!                 ...
!                 CALL ZZCTRUIN( USRCTR )
!                 FIRST = .FALSE.
!              END IF
!              ...
!        C
!        C     In all places where there is a need to check for
!        C     the subsystem state change call subsystem's entry
!        C     that checks and updates the user counter based on
!        C     value of the subsystem counter.
!        C
!              CALL <entry_name> ( USRCTR, UPDATE )
!
!              IF ( UPDATE ) THEN
!
!        C
!        C        Do what needs to be done when the subsystem
!        C        state has changed.
!        C
!                 ...
!
!              END IF
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
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     manipulate counter array
!
!-&

!
!     SPICELIB functions
!
      integer               intmax
      integer               intmin

      logical               return

!
!     Local variables.
!
      integer               ctrhgh
      integer               ctrlow

      logical               first

!
!     Save EVERYTHING.
!
      save

!
!     Initial values.
!
      data                  first  / .true. /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

!
!     This routine should never be called. If this routine is called,
!     an error is signaled.
!
      call chkin  ( 'ZZCTR' )

      call setmsg ( 'ZZCTR: You have called an entry which performs ' // &
                    'performs no run-time function. This may '        // &
                    'indicate a bug. Please check the documentation ' // &
                    'for the subroutine ZZCTR.' )

      call sigerr ( 'SPICE(BOGUSENTRY)' )

      call chkout ( 'ZZCTR' )

      return


!$Procedure ZZCTRUIN ( CounTeR array, User counter INitialization )

      entry zzctruin ( oldctr )

! $ Abstract
!
!     Set counter array to the initial values for the user routines.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               OLDCTR ( CTRSIZ )
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     OLDCTR     O   Counter array to be set to initial user values.
!
!     CTRSIZ     P   Counter array size.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OLDCTR      is the counter array set to the initial values for
!                 the user routines.
!
! $ Parameters
!
!     CTRSIZ      is the dimension of the counter array used by
!                 various SPICE subsystems to identify
!                 changes in their states.
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
!     This type of initialization must be done by all user routines
!     to ensure update on the first check.
!
! $ Examples
!
!     See umbrella Examples.
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
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     initialize counter array to user values
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

!
!     Initialize the high and low values.
!
      if ( first ) then

         ctrhgh = intmax()
         ctrlow = intmin()

         first = .false.

      end if

!
!     Set counter.
!
      oldctr(1) = ctrhgh
      oldctr(2) = ctrhgh

      return


!$Procedure ZZCTRSIN ( CounTeR array, Subsystem counter INitialization )

      entry zzctrsin ( oldctr )

! $ Abstract
!
!     Set counter array to the initial values for the subsystem
!     routines.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               OLDCTR ( CTRSIZ )
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     OLDCTR     O   Counter array to be set to initial subsystem values
!
!     CTRSIZ     P   Counter array size.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OLDCTR      is the counter array set to the initial values for
!                 the subsystem routines.
!
! $ Parameters
!
!     CTRSIZ      is the dimension of the counter array used by
!                 various SPICE subsystems to identify
!                 changes in their states.
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
!     This type of initialization must be done by all subsystem
!     routines to set the counters to the lowest initial values.
!
! $ Examples
!
!     See umbrella Examples.
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
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     initialize counter array to subsystem values
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

!
!     Initialize the high and low values.
!
      if ( first ) then

         ctrhgh = intmax()
         ctrlow = intmin()

         first = .false.

      end if

!
!     Set counter.
!
      oldctr(1) = ctrlow
      oldctr(2) = ctrlow

      return




!$Procedure ZZCTRINC ( CounTeR Array, INCrement counter )

      entry zzctrinc ( oldctr )

! $ Abstract
!
!     Increment counter array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               OLDCTR ( CTRSIZ )
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     OLDCTR    I/O  Counter array to be incremented.
!
!     CTRSIZ     P   Counter array size.
!
! $ Detailed_Input
!
!     OLDCTR      is the counter array to be incremented.
!
! $ Detailed_Output
!
!     OLDCTR      is the counter array that was incremented.
!
! $ Parameters
!
!     CTRSIZ      is the dimension of the counter array used by
!                 various SPICE subsystems to identify
!                 changes in their states.
!
! $ Exceptions
!
!     1) If all elements of the counter array on the input are equal
!        to INTMAX, the error '(SPICEISTIRED)' is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See umbrella Examples.
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
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     increment counter array
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

!
!     Initialize the high and low values.
!
      if ( first ) then

         ctrhgh = intmax()
         ctrlow = intmin()

         first = .false.

      end if

!
!     Signal an error if both input counter array elements have high
!     values.
!
      if ( oldctr(1) .eq. ctrhgh .and. oldctr(2) .eq. ctrhgh ) then

         call chkin  ( 'ZZCTRINC' )

         call setmsg ( 'A subsystem state counter overflowed. '    // &
                       'For this to happen there must be '         // &
                       'a SPICE bug or you must have been '        // &
                       'running your SPICE-based application for ' // &
                       'a very long time. Please contact NAIF.'    // &
                       'and report the circumstances under '       // &
                       'which this happened.'                      )

         call sigerr ( 'SPICE(SPICEISTIRED)' )

         call chkout ( 'ZZCTRINC' )

         return

      end if

!
!     Increment counters.
!
      if ( oldctr(1) .eq. ctrhgh ) then

         oldctr(1) = ctrlow
         oldctr(2) = oldctr(2) + 1

      else

         oldctr(1) = oldctr(1) + 1

      end if

      return



!$Procedure ZZCTRCHK ( CounTeR array, CHecK and update )

      entry zzctrchk ( newctr, oldctr, update )

! $ Abstract
!
!     Check and update, if needed, counter array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               NEWCTR ( CTRSIZ )
!     INTEGER               OLDCTR ( CTRSIZ )
!     LOGICAL               UPDATE
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     NEWCTR     I   New counter value.
!     OLDCTR    I/O  Old counter value.
!     UPDATE     O   Update flag.
!
!     CTRSIZ     P   Counter array size.
!
! $ Detailed_Input
!
!     NEWCTR      is the new counter value against which the old
!                 value should be checked.
!
!     OLDCTR      is the old counter value.
!
! $ Detailed_Output
!
!     OLDCTR      is the updated the old counter value, set equal to
!                 the new counter value if counters were different or
!                 kept the same if counters were the same.
!
!     UPDATE      is the logical flag indicating whether the old
!                 counter was updated.
!
! $ Parameters
!
!     CTRSIZ      is the dimension of the counter array used by
!                 various SPICE subsystems to identify
!                 changes in their states.
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
!     None.
!
! $ Examples
!
!     See umbrella Examples.
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
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     check and update counter array
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

!
!     Do counters differ?
!
      update = newctr(1) .ne. oldctr(1) .or. newctr(2) .ne. oldctr(2)

!
!     If they do, set old counter to new value.
!
      if ( update ) then

         oldctr(1) = newctr(1)
         oldctr(2) = newctr(2)

      end if

      return

      end
