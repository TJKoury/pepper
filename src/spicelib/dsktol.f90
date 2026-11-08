!$Procedure DSKTOL ( DSK, tolerance umbrella )

      subroutine dsktol ( keywrd, dpval )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Umbrella routine for DSK tolerance and margin parameter access
!     routines.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     MARGIN
!     NUMERIC
!     TOLERANCE
!
! $ Declarations

      implicit none

      include 'dsktol.inc'

      integer(ip)           keywrd
      real(wp)              dpval

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     KEYWRD     I   DSKGTL, DSKSTL
!     DPVAL     I-O  DSKGTL, DSKSTL
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their input arguments.
!
! $ Detailed_Output
!
!     See the entry points for descriptions of their output arguments.
!
! $ Parameters
!
!     See the include file
!
!        dsktol.inc
!
!     for descriptions and values of the tolerance or margin parameters
!     accessed by the entry points of this routine, and of the keyword
!     parameters used to refer to them.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     2)  See the entry points for descriptions of exceptions specific
!         to those entry points.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The routines in this package serve to centralize numeric
!     tolerance and margin values used by the DSK subsystem. The
!     subsystem retrieves values from this package to use at run time.
!
!     The entry points of this routine are
!
!        DSKGTL {DSK, get tolerance value}
!        DSKSTL {DSK, set tolerance value}
!
!     To minimize run time overhead, the "keywords" used by these
!     routines to identify parameters are actually integer codes.
!
!     SPICE users may override certain values maintained by this
!     package; others values are fixed.
!
!     One use of this system would be to disable the "greedy"
!     algorithms used to prevent "false miss" ray-surface intercepts
!     results. Setting to zero the margins used for this purpose would
!     accomplish this.
!
!     It is recommended that any change to the tolerance values made at
!     run time be programmed only by SPICE experts.
!
! $ Examples
!
!     See the entry points.
!
! $ Restrictions
!
!     1)  The default settings used by the DSK subsystem should
!         be overridden only by expert SPICE users.
!
!     2)  The entry points of this routine do not check the
!         validity of new parameter values supplied by the
!         calling application.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
!
!        Edited the entry points and umbrella headers to comply with
!        NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     DSK tolerance and margin umbrella
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: ndspar = 6

      integer(ip), parameter :: namlen = 6

!
!     Local variables
!
      character(namlen)     names  ( ndspar )

      real(wp)              dppars ( ndspar )

      logical               isfixd ( ndspar )

!
!     Saved values
!
      save                  dppars
      save                  isfixd
      save                  names

!
!     Initial values
!
!     Caution: this array must be declared in
!     the order defined by the keywords declared
!     in dsktol.inc.
!
      data                  dppars / &
                            xfract, sgreed, sgpadm, &
                            ptmemm, angmrg, lonali &
                                   /

      data                  isfixd / &
                            .false., .false., .false., &
                            .false., .true.,  .true. &
                                   /

      data                  names  / &
                            'XFRACT', 'SGREED', 'SGPADM', &
                            'PTMEMM', 'ANGMRG', 'LONALI' &
                                   /

      call sigerr ( 'SPICE(BOGUSENTRY)' )
      return

!$Procedure DSKGTL ( DSK, get tolerance )

      entry dskgtl ( keywrd, dpval )

! $ Abstract
!
!     Retrieve the value of a specified DSK tolerance or margin
!     parameter.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     MARGIN
!     NUMERIC
!     TOLERANCE
!
! $ Declarations
!
!     INTEGER               KEYWRD
!     DOUBLE PRECISION      DPVAL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     KEYWRD     I   Code specifying parameter to retrieve.
!     DPVAL      O   Value of parameter.
!
! $ Detailed_Input
!
!     KEYWRD   is an integer code specifying the parameter to
!              retrieve. See the include file dsktol.inc for
!              a description of the possible keywords.
!
! $ Detailed_Output
!
!     DPVAL    is the value of the parameter specified by KEYWRD.
!
! $ Parameters
!
!     See the include file
!
!        dsktol.inc
!
!     for descriptions and values of the tolerance or margin parameters
!     accessed by this routine, and of the keyword parameters used to
!     refer to them.
!
! $ Exceptions
!
!     1)  If the input keyword is not recognized, the error
!         SPICE(INDEXOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The routines in this package serve to centralize numeric
!     tolerance and margin values used by the DSK subsystem.
!     The subsystem retrieves values from this package to use
!     at run time.
!
!     The entry points of this routine are
!
!        DSKGTL {DSK, get tolerance value}
!        DSKSTL {DSK, set tolerance value}
!
!     To minimize run time overhead, the "keywords" used by
!     these routines to identify parameters are actually
!     integer codes.
!
!     SPICE users may override certain values maintained by
!     this package; others values are fixed. It is recommended
!     that any change to the tolerance values made at run
!     time be performed only by expert SPICE users.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Obtain and display the DSK type 2 plate expansion fraction.
!
!        Example code begins here.
!
!              PROGRAM DSKGTL_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include files
!        C
!              INCLUDE 'dsktol.inc'
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      DPVAL
!
!              CALL DSKGTL ( KEYXFR, DPVAL )
!
!              WRITE(*,*) 'Plate expansion fraction = ', DPVAL
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Plate expansion fraction =    1.0000000000000000E-010
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
!
!        Updated the header to comply with NAIF standard. Added
!        complete example code.
!
!-    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     retrieve DSK tolerance or margin parameters
!
!-&

!
!     Use discovery check-in.
!
      if (  ( keywrd < 1 ) .or. ( keywrd > ndspar )  ) then

         call setmsg ( 'Valid keyword range is 1:#; keyword was #.' )
         call errint ( '#', ndspar                                  )
         call errint ( '#', keywrd                                  )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'                     )
         return

      end if

      dpval = dppars( keywrd )

      return

!$Procedure DSKSTL ( DSK, set tolerance )

      entry dskstl ( keywrd, dpval )

! $ Abstract
!
!     Set the value of a specified DSK tolerance or margin parameter.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     MARGIN
!     NUMERIC
!     TOLERANCE
!
! $ Declarations
!
!     INTEGER               KEYWRD
!     DOUBLE PRECISION      DPVAL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     KEYWRD     I   Code specifying parameter to modify.
!     DPVAL      I   New value of parameter.
!
! $ Detailed_Input
!
!     KEYWRD   is an integer code specifying the parameter to
!              set. See the include file dsktol.inc for
!              a description of the possible keywords.
!
!     DPVAL    is the new value of the parameter specified by KEYWRD.
!              This value will be retrieved by future calls to
!              to DSKGTL made to retrieve the specified parameter.
!
!              <<< Use extreme caution. This routine performs no
!              checks on DPVAL. >>>
!
! $ Detailed_Output
!
!     None.
!
!     This routine operates by side effects.
!
! $ Parameters
!
!     See the include file
!
!        dsktol.inc
!
!     for descriptions and values of the tolerance or margin parameters
!     accessed by this routine, and of the keyword parameters used to
!     refer to them.
!
! $ Exceptions
!
!     1)  If the input keyword is not recognized, the error
!         SPICE(INDEXOUTOFRANGE) is signaled.
!
!     2)  If an attempt is made to modify a fixed parameter,
!         the error SPICE(IMMUTABLEVALUE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The routines in this package serve to centralize numeric
!     tolerance and margin values used by the DSK subsystem.
!     The subsystem retrieves values from this package to use
!     at run time.
!
!     The entry points of this routine are
!
!        DSKGTL {DSK, get tolerance value}
!        DSKSTL {DSK, set tolerance value}
!
!     To minimize run time overhead, the "keywords" used by
!     these routines to identify parameters are actually
!     integer codes.
!
!     SPICE users may override certain values maintained by
!     this package; others values are fixed. It is recommended
!     that any change to the tolerance values made at run
!     time be performed only by expert SPICE users.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Obtain, display, and update the DSK type 2 plate expansion
!        fraction.
!
!        Example code begins here.
!
!              PROGRAM DSKSTL_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include files
!        C
!              INCLUDE 'dsktol.inc'
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      DPVAL
!
!              CALL DSKGTL ( KEYXFR, DPVAL )
!
!              WRITE(*,*) 'Default plate expansion fraction = ', DPVAL
!
!        C
!        C     Update the parameter.
!        C
!              CALL DSKSTL ( KEYXFR, 1.D-8 )
!
!        C
!        C     Verify the update.
!        C
!              CALL DSKGTL ( KEYXFR, DPVAL )
!
!              WRITE(*,*) 'New plate expansion fraction     = ', DPVAL
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Default plate expansion fraction =    1.0000000000000000E-010
!         New plate expansion fraction     =    1.0000000000000000E-008
!
! $ Restrictions
!
!     1)  The default settings used by the DSK subsystem should
!         be overridden only by expert SPICE users.
!
!     2)  This routine does not check the  validity of new parameter
!         values supplied by the calling application.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
!
!        Updated the header to comply with NAIF standard. Added
!        complete example code.
!
!-    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     set DSK tolerance or margin parameters
!
!-&

      if ( return() ) return

      if (  ( keywrd < 1 ) .or. ( keywrd > ndspar )  ) then

         call setmsg ( 'Valid keyword range is 1:#; keyword was #.' )
         call errint ( '#', ndspar                                  )
         call errint ( '#', keywrd                                  )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'                     )
         return

      end if

      if ( isfixd(keywrd) ) then

         call setmsg ( 'The parameter # cannot be modified.' )
         call errch  ( '#', names(keywrd)                    )
         call sigerr ( 'SPICE(IMMUTABLEVALUE)'               )
         return

      end if

!
!     We have a valid parameter index. We don't check
!     the new parameter value; the user presumably knows
!     the reason for change.
!
      dppars(keywrd) = dpval

      return
      end
