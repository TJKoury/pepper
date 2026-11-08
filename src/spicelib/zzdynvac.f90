!$Procedure ZZDYNVAC ( Fetch array, character frame kernel variable )

      SUBROUTINE ZZDYNVAC ( FRNAME, FRCODE, ITEM, MAXN, N, VALUES )

      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Look up array-valued character frame kernel variable.  The frame
!     name or frame ID may be used as part of the variable's name.
!
!     If the kernel variable is not present, or if the variable
!     has the wrong data type, signal an error.
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
!     FRAMES
!     KERNEL
!     PRIVATE
!     UTILITY
!
! $ Declarations

      INCLUDE 'zzdyn.inc'

      CHARACTER*(*)         FRNAME
      INTEGER               FRCODE
      CHARACTER*(*)         ITEM
      INTEGER               MAXN
      INTEGER               N
      CHARACTER*(*)         VALUES  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     FRNAME     I   Frame name.
!     FRCODE     I   Frame ID code.
!     ITEM       I   Item associated with frame definition.
!     MAXN       I   Maximum number of values to return.
!     N          O   Number of returned values.
!     VALUES     O   Output kernel variable.
!
! $ Detailed_Input
!
!     FRNAME         is the name of the reference frame with which
!                    the requested variable is associated.
!
!     FRCODE         is the frame ID code of the reference frame with
!                    which the requested variable is associated.
!
!     ITEM           is a string identifying the specific datum
!                    to be fetched.  The kernel variable name
!                    has the form
!
!                       FRAME_<frame ID code>_<ITEM>
!
!                    or
!
!                       FRAME_<frame name>_<ITEM>
!
!                    The former of the two names takes precedence:
!                    this routine will look for a character variable
!                    of that name first.
!
! $ Detailed_Output
!
!     N              is the number of values returned in the array
!                    VALUES.
!
!     VALUES         are the values associated with the requested
!                    array-valued, character kernel variable. The
!                    kernel variable name of the form
!
!                       FRAME_<frame ID code>_<ITEM>
!
!                    will be looked up first; if this variable
!                    is found and has character type, the associated
!                    values will be returned.  If this variable is
!                    not found, the variable
!
!                       FRAME_<frame name>_<ITEM>
!
!                    will be looked up.  If a character variable
!                    having that name is found, the associated
!                    values will be returned.
!
! $ Parameters
!
!     See zzdyn.inc.
!
! $ Exceptions
!
!     1) If neither the frame-ID-based or frame-name-based form of the
!        requested kernel variable name matches a kernel variable
!        present in the kernel pool, the error SPICE(KERNELVARNOTFOUND)
!        will be signaled.
!
!     2) If either the frame-ID-based or frame-name-based form of the
!        requested kernel variable name has length greater than KVNMLN,
!        the excessively long name will not be searched for. A search
!        will still be done using the alternative form of the name if
!        that form has length less than or equal to KVNMLN.
!
!     3) If both the frame-ID-based and frame-name-based forms of the
!        requested kernel variable name have length greater than KVNMLN,
!        the error SPICE(VARNAMETOOLONG) will be signaled.
!
!     4) If kernel variable matching one form of the requested kernel
!        variable names is found, but that variable has numeric data
!        type, the error SPICE(BADVARIABLETYPE) will be signaled.
!
!     5) If kernel variable matching one form of the requested kernel
!        variable names is found, but that variable has more than MAXN
!        associated values, the error SPICE(BADVARIABLESIZE) will be
!        signaled.
!
! $ Files
!
!     1) Kernel variables fetched by this routine are normally
!        introduced into the kernel pool by loading one or more
!        frame kernels.  See the Frames Required Reading for
!        details.
!
! $ Particulars
!
!     This routine centralizes logic for kernel variable lookups that
!     must be performed by the SPICELIB frame subsystem. Part of the
!     functionality of this routine consists of handling error
!     conditions such as the unavailability of required kernel
!     variables; hence no "found" flag is returned to the caller.
!
!     As indicated above, the requested kernel variable may have a name
!     of the form
!
!        FRAME_<frame ID code>_<ITEM>
!
!     or
!
!        FRAME_<frame name>_<ITEM>
!
!     Because most frame definition keywords have the first form, this
!     routine looks for a name of that form first.
!
!     Note that although this routine considers the two forms of the
!     names to be synonymous, from the point of view of the kernel pool
!     data structure, these names are distinct.  Hence kernel variables
!     having names of both forms, but having possibly different
!     attributes, can be simultaneously present in the kernel pool.
!     Intentional use of this kernel pool feature is discouraged.
!
! $ Examples
!
!     See ZZDYNFRM.
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject
!        to change without notice.  User applications should not
!        call this routine.
!
!     2) A scalar-valued kernel variable matching the "ID code form"
!        of the requested kernel variable name could potentially
!        mask an array-valued kernel variable matching the "name
!        form" of the requested name.  This problem can be prevented
!        by sensible frame kernel design.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
      CHARACTER*(*)         TEMPLT
      PARAMETER           ( TEMPLT = 'FRAME_#_#' )

!
!     TEMPLN is the length of the keyword template, minus
!     the sum of the lengths of the two substitution markers ('#').
!
      INTEGER               TEMPLN
      PARAMETER           ( TEMPLN = 7 )

!
!     Local Variables
!
      CHARACTER*(KVNMLN)    CDESTR
      CHARACTER*(1)         DTYPE
      CHARACTER*(KVNMLN)    KVNAME

      INTEGER               CODELN
      INTEGER               ITEMLN
      INTEGER               NAMELN
      INTEGER               REQNAM
      INTEGER               REQNUM

      LOGICAL               FOUND

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDYNVAC' )

!
!     Prepare to check the name of the kernel variable we're about
!     to look up.
!
!     Convert the frame code to a string.
!
      CALL INTSTR ( FRCODE, CDESTR )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDYNVAC' )
         RETURN
      END IF

!
!     Get the lengths of the input frame code, name and item.
!     Compute the length of the ID-based kernel variable name;
!     check this length against the maximum allowed value.  If
!     the name is too long, proceed to look up the form of the
!     kernel variable name based on the frame name.
!
      CODELN = RTRIM ( CDESTR )
      NAMELN = RTRIM ( FRNAME )
      ITEMLN = RTRIM ( ITEM   )

      REQNUM = CODELN + ITEMLN + TEMPLN

      IF ( REQNUM .LE. KVNMLN ) THEN
!
!        First try looking for a kernel variable including the frame ID
!        code.
!
!        Note the template is
!
!            'FRAME_#_#'
!
         CALL REPMI  ( TEMPLT, '#',   FRCODE, KVNAME )
         CALL REPMC  ( KVNAME, '#',   ITEM,   KVNAME )
         CALL DTPOOL ( KVNAME, FOUND, N,      DTYPE  )

      ELSE
!
!        The ID-based name is too long. We can't find the variable if
!        we can't look it up.
!
         FOUND = .FALSE.

      END IF


      IF ( .NOT. FOUND ) THEN
!
!        We need to look up the frame name-based kernel variable.
!        Determine the length of the name of this variable; make
!        sure it's not too long.
!
         REQNAM = NAMELN + ITEMLN + TEMPLN

         IF (       ( REQNAM .GT. KVNMLN ) &
              .AND. ( REQNUM .GT. KVNMLN ) ) THEN
!
!           Both forms of the name are too long.
!
            CALL SETMSG ( 'Kernel variable FRAME_#_# has length #; '// &
                          'kernel variable FRAME_#_# has length #; '// &
                          'maximum allowed length is #.  Neither '  // &
                          'variable could be searched for in the '  // &
                          'kernel pool due to these name length '   // &
                          'errors.'                                 )
            CALL ERRINT ( '#',  FRCODE                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNUM                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNAM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL SIGERR ( 'SPICE(VARNAMETOOLONG)'                   )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN


         ELSE IF  ( REQNAM .GT. KVNMLN ) THEN
!
!           We couldn't find the variable having the ID-based name,
!           and the frame name-based variable name is too long to
!           look up.
!
!           Note that at this point KVNAME contains the ID-based
!           kernel variable name.
!
            CALL SETMSG ( 'Kernel variable # was expected to '      // &
                          'be present in the kernel pool but '      // &
                          'was not found.  The alternative '        // &
                          'form of kernel variable name '           // &
                          'FRAME_#_# was not searched for '         // &
                          'because this name has excessive '        // &
                          'length (# characters vs allowed '        // &
                          'maximum of #).  One of these variables ' // &
                          'is needed to define the parameterized '  // &
                          'dynamic frame #.  Usually this type '    // &
                          'of problem is due to an error in a '     // &
                          'frame definition provided in a frame '   // &
                          'kernel.'                                )
            CALL ERRCH  ( '#',  KVNAME                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNAM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN

         END IF

!
!        Now try looking for a kernel variable including the frame
!        name.
!
         CALL REPMC  ( TEMPLT, '#',   FRNAME, KVNAME )
         CALL REPMC  ( KVNAME, '#',   ITEM,   KVNAME )
         CALL DTPOOL ( KVNAME, FOUND, N,      DTYPE  )


         IF (  ( .NOT. FOUND ) .AND. ( REQNUM .GT. KVNMLN )  ) THEN
!
!           The kernel variable's presence (in one form or the other)
!           is mandatory:  signal an error.  The error message
!           depends on which variables we were able to try to
!           look up.  In this case, we never tried to look up the
!           frame ID-based name.
!
!           Note that at this point KVNAME contains the name-based
!           kernel variable name.
!
            CALL SETMSG ( 'Kernel variable # was expected to '      // &
                          'be present in the kernel pool but '      // &
                          'was not found.  The alternative '        // &
                          'form of kernel variable name '           // &
                          'FRAME_#_# was not searched for '         // &
                          'because this name has excessive '        // &
                          'length (# characters vs allowed '        // &
                          'maximum of #).  One of these variables ' // &
                          'is needed to define the parameterized '  // &
                          'dynamic frame #.  Usually this type '    // &
                          'of problem is due to an error in a '     // &
                          'frame definition provided in a frame '   // &
                          'kernel.'                                 )
            CALL ERRCH  ( '#',  KVNAME                              )
            CALL ERRINT ( '#',  FRCODE                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNUM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN


         ELSE IF ( .NOT. FOUND ) THEN
!
!           We tried to look up both names and failed.
!
            CALL SETMSG ( 'At least one of the kernel variables '   // &
                          'FRAME_#_# or FRAME_#_# was expected to ' // &
                          'be present in the kernel pool but '      // &
                          'neither was found. One of these '        // &
                          'variables is needed to define the '      // &
                          'parameterized dynamic frame #.  Usually '// &
                          'this type of problem is due to a '       // &
                          'missing keyword assignment in a frame '  // &
                          'kernel.  Another, less likely, '         // &
                          'possibility is that other errors in a '  // &
                          'frame kernel have confused the '         // &
                          'frame subsystem into wrongly '           // &
                          'deciding these variables are needed.'    )
            CALL ERRINT ( '#',  FRCODE                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN

         END IF

      END IF

!
!     Getting to this point means we found a kernel variable. The name
!     of the variable is KVNAME.  The data type is DTYPE and the
!     cardinality is N.
!
!     Rather than using BADKPV, we check the data type and cardinality
!     of the kernel variable in-line so we can create a more detailed
!     error message if need be.
!
      IF ( DTYPE .EQ. 'N' ) THEN

         CALL SETMSG ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have character '    // &
                       'data type but in fact has numeric '         // &
                       'data type.  Usually this type of '          // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         CALL ERRCH  ( '#',  KVNAME                                 )
         CALL ERRCH  ( '#',  FRNAME                                 )
         CALL SIGERR ( 'SPICE(BADVARIABLETYPE)'                     )
         CALL CHKOUT ( 'ZZDYNVAC'                                   )
         RETURN

      END IF


      IF ( N .GT. MAXN ) THEN

         CALL SETMSG ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have size not '     // &
                       'exceeding # but in fact has size #. '       // &
                       'Usually this type of '                      // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         CALL ERRCH  ( '#',  KVNAME                                 )
         CALL ERRCH  ( '#',  FRNAME                                 )
         CALL ERRINT ( '#',  MAXN                                   )
         CALL ERRINT ( '#',  N                                      )
         CALL SIGERR ( 'SPICE(BADVARIABLESIZE)'                     )
         CALL CHKOUT ( 'ZZDYNVAC'                                   )
         RETURN

      END IF

!
!     Look up the kernel variable.
!
      CALL GCPOOL ( KVNAME, 1, MAXN, N, VALUES, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Variable # not found after DTPOOL ' // &
                       'indicated it was present in pool.'  )
         CALL ERRCH  ( '#',  KVNAME                         )
         CALL SIGERR ( 'SPICE(BUG)'                         )
         CALL CHKOUT ( 'ZZDYNVAC'                           )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZDYNVAC' )
      RETURN
      END
