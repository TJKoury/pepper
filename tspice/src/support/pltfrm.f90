!$Procedure      PLTFRM ( Get platform attributes )

      SUBROUTINE PLTFRM ( ROOM, N, ATTR )
      use pepper_legacy
!~  NEXT
!*    IMPLICIT NONE
!~~

! $ Abstract
!
!     Return platform id and various attributes of the platform
!     environment
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

      INTEGER               ROOM
      INTEGER               N
      CHARACTER*(*)         ATTR ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ROOM       I   amount of room available for returning attributes
!     N          O   number of attributes returned
!     ATTR       O   string values of various attributes
!
! $ Detailed_Input
!
!     ROOM        is the amount of space available in the character
!                 string array ATTR for returning platform attributes.
!
! $ Detailed_Output
!
!     N           is the actual number of attributes returned.  N will
!                 always be less than or equal to ROOM.
!
!     ATTR        is an array of attributes about the platform
!                 and environment on which this routine is running.
!
!                 ATTR will contain in the following order
!
!                 1) machine name    :   HP, NEXT, PC, SGI, etc.
!                 2) fortran compiler:   HP , ABSOFT, etc.
!                 3) Operating System
!
!                 Other items may be added later.  Check your local
!                 listing for details.
!
!                 If a value is not available it will be returned
!                 with the value '<unavailable>'
!
!                 The routine that calls this should declare
!                 ATTR to be at least CHARACTER*(32).
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
!     Error free.
!
!     1)  If ROOM is less than or equal to zero, the N will be
!         returned with a value of zero and ATTR will not be
!         changed from it's input state.
!
! $ Particulars
!
!     This routine serves to identify the platforma and compiler
!     used in creating SPICELIB.  It is provided so that routines
!     and programs can make run-time decisions based upon the
!     ambient fortran environment.
!
! $ Examples
!
!     This routine could be used so that a single routine
!     can be written that translates the meaning of IOSTAT values
!     that depend upon the machine and compiler.  At run time
!     the routine can look up the appropriate message to associate
!     with an IOSTAT value.
!C
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
!-    Support Version 1.4.0, 28-JUL-1999 (WLT)
!
!        Changed routine to call new SPICE private routine ZZPLATFM
!        The routine is no longer environment specific.
!
!-    Inspekt Version 1.3.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitely given.  Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    Inspekt Version 1.3.0, 05-APR-1998 (NJB)
!
!        Added the PC-LINUX environment.
!
!-    Inspekt Version 1.2.0, 12-AUG-1996 (WLT)
!
!        Added the DEC-OSF1 environment.
!
!-    Inspekt Version 1.1.0, 16-JUN-1995 (WLT)
!
!        Master version of original machine dependent collection.
!        Copyright notice added.
!
!-    Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!        This is the configured version of the Command Loop
!        software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 21-APR-1994 (WLT)
!
!
!-&


! $ Index_Entries
!
!     Determine the machine, OS and fortran version.
!
!-&

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               NATTR
      PARAMETER           ( NATTR  =  3 )

      CHARACTER*(WDSIZE)    ITEM ( NATTR )

      INTEGER               LIMIT
      INTEGER               I

      ITEM(1) = 'SYSTEM'
      ITEM(2) = 'COMPILER'
      ITEM(3) = 'O/S'

      LIMIT = MAX( 0, MIN ( NATTR, ROOM ) )

      DO I = 1, LIMIT
         CALL ZZPLATFM ( ITEM(I), ATTR(I) )
      END DO

      N = LIMIT

      RETURN
      END
