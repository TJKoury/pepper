!$Procedure      DSPVRS ( Display Version )

      SUBROUTINE DSPVRS ( PNAME, VRSN )

! $ Abstract
!
!    This routine displays the name of a program as well as its
!    version and the version of SPICELIB that the calling
!    program has been linked with.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTITITY
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         PNAME
      CHARACTER*(*)         VRSN

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PNAME      I   The name of the calling program
!     VRSN       I   The version number of the calling program
!
! $ Detailed_Input
!
!     PNAME      is the name of the calling program
!
!     VRSN       is the version number of the calling program
!
! $ Detailed_Output
!
!     None.
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
! $ Particulars
!
!     This is a utility routine for printing the name and
!     version number of a program as well as the identifier
!     of the SPICELIB library that was used in linking
!     the program.
!
!     The following template is filled out and then displayed
!     at standard output.
!
!       <pname> --- Version <vrsn>,  SPICE Toolkit <tkvrsn>
!
! $ Examples
!
!     Suppose you are creating an program called "DoIt"
!     and you would like to have the name and current version
!     of the program displayed along with the linked version
!     of SPICELIB at some point in the execution of the program,
!     Here is how you can use this routine to perform the
!     version display function.
!
!        CALL DSPVRS ( 'DoIt', '1.0.0' )
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
!-    SPICELIB Version 1.0.0, 26-SEP-1997 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Print a version line
!
!-&


      INTEGER               TKVSIZ
      PARAMETER           ( TKVSIZ = 8 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(TKVSIZ)    TKV


      CALL TKVRSN ( 'toolkit', TKV )

      LINE = PNAME

      CALL SUFFIX ( 'Version',         1, LINE )
      CALL SUFFIX (  VRSN,             1, LINE )
      CALL SUFFIX ( ', SPICE Toolkit', 0, LINE )
      CALL SUFFIX (  TKV,              1, LINE )

      CALL TOSTDO ( LINE )

      RETURN
      END
