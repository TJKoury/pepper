!$Procedure ZZDYNBID ( Fetch body ID kernel variable )

      subroutine zzdynbid ( frname, frcode, item, idcode )
      use pepper_kinds, only: ip
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Look up a frame definition kernel variable whose associated value
!     is a body name or body ID code.  The returned value is always an
!     ID code.  The frame name or frame ID may be used as part of the
!     variable's name.
!
!     If the kernel variable is not present, or if the variable
!     is not a body name or a numeric value, signal an error.
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

      include 'zzbodtrn.inc'
      include 'zzdyn.inc'

      character(*)          frname
      integer(ip)           frcode
      character(*)          item
      integer(ip)           idcode

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     FRNAME     I   Frame name.
!     FRCODE     I   Frame ID code.
!     ITEM       I   Item associated with frame definition.
!     IDCODE     O   Body ID code.
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
!                    this routine will look for a numeric variable
!                    of that name first.
!
!                    The value associated with the kernel variable
!                    must be one of
!
!                       - a nbody ID code
!
!                       - a string representation of an integer,
!                         for example '5'
!
!                       - a body frame name
!
! $ Detailed_Output
!
!     IDCODE         is the requested body ID code.
!
!                    The kernel variable name of the form
!
!                       FRAME_<frame ID code>_<ITEM>
!
!                    will be looked up first; if this variable
!                    is found and has numeric type, the associated
!                    value will be returned.  If this variable is
!                    found and has character type, the value will
!                    be converted to a body ID code, and that
!                    code will be returned.
!
!                    If this variable is not found, the variable
!
!                       FRAME_<frame name>_<ITEM>
!
!                    will be looked up.  If this variable is found and
!                    has numeric type, the associated value will be
!                    returned.  If this variable is found and has
!                    character type, the value will be converted to a
!                    body ID code, and that code will be returned.
!
!                    If a numeric value associated with the selected
!                    kernel variable is not integral, it will be
!                    rounded to the closest integer.
!
! $ Parameters
!
!     See zzdyn.inc for definition of KVNMLN.
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
!        that variable will not be searched for.
!
!     3) If both the frame-ID-based and frame-name-based forms of the
!        requested kernel variable name have length greater than KVNMLN,
!        the error SPICE(VARNAMETOOLONG) will be signaled.
!
!     4) If kernel variable matching one form of the requested kernel
!        variable names is found, but that variable has more than 1
!        associated value, the error SPICE(BADVARIABLESIZE) will be
!        signaled.
!
!     5) If a name match is found for a character kernel variable, but
!        the value associated with the variable cannot be mapped to a
!        body ID code, the error SPICE(NOTRANSLATION) will be
!        signaled.
!
!     6) If a name match is found for a numeric kernel variable,
!        but that variable has a value that cannot be rounded to an
!        integer representable on the host platform, an error will
!        be signaled by a routine in the call tree of this routine.
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
!     access routines, these names are distinct.  Hence kernel
!     variables having names of both forms, but having possibly
!     different attributes, can be simultaneously present in the kernel
!     pool. Intentional use of this kernel pool feature is discouraged.
!
! $ Examples
!
!     1) See ZZDYNFRM.
!
!     2) Applications of this routine include finding ID codes of
!        observer or target bodies serving to define two-vector dynamic
!        frames.
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject
!        to change without notice.  User applications should not
!        call this routine.
!
!     2) An array-valued kernel variable matching the "ID code form"
!        of the requested kernel variable name could potentially
!        mask a scalar-valued kernel variable matching the "name
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
!-    SPICELIB Version 2.0.0, 05-AUG-2005 (NJB)
!
!        References to parameterized dynamic frames in long error
!        messages were changed to references to "reference frames."
!        This change was made to enable this utility to support
!        kernel variable look-ups for non-dynamic frames.
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-AUG-2005 (NJB)
!
!        References to parameterized dynamic frames in long error
!        messages were changed to references to "reference frames."
!        This change was made to enable this utility to support
!        kernel variable look-ups for non-dynamic frames.
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           rtrim

      logical               failed
      logical               return

!
!     Local parameters
!
      character(*)          templt
      parameter           ( templt = 'FRAME_#_#' )

!
!     TEMPLN is the length of the keyword template, minus
!     the sum of the lengths of the two substitution markers ('#').
!
      integer(ip)           templn
      parameter           ( templn = 7 )

!
!     Local variables
!
      character(bdnmln)     bodnam
      character(kvnmln)     cdestr
      character(1)          dtype
      character(kvnmln)     kvname

      integer(ip)           codeln
      integer(ip)           itemln
      integer(ip)           n
      integer(ip)           nameln
      integer(ip)           reqnam
      integer(ip)           reqnum

      logical               found


      if ( return() ) return


!
!     Prepare to check the name of the kernel variable we're about
!     to look up.
!
!     Convert the frame code to a string.
!
      call intstr ( frcode, cdestr )

      if ( failed() ) return

!
!     Get the lengths of the input frame code, name and item.
!     Compute the length of the ID-based kernel variable name;
!     check this length against the maximum allowed value.  If
!     the name is too long, proceed to look up the form of the
!     kernel variable name based on the frame name.
!
      codeln = rtrim ( cdestr )
      nameln = rtrim ( frname )
      itemln = rtrim ( item   )

      reqnum = codeln + itemln + templn

      if ( reqnum <= kvnmln ) then
!
!        First try looking for a kernel variable including the frame ID
!        code.
!
!        Note the template is
!
!            'FRAME_#_#'
!
         call repmi  ( templt, '#',   frcode, kvname )
         call repmc  ( kvname, '#',   item,   kvname )
         call dtpool ( kvname, found, n,      dtype  )

      else
!
!        The ID-based name is too long. We can't find the variable if
!        we can't look it up.
!
         found = .false.

      end if


      if ( .not. found ) then
!
!        We need to look up the frame name-based kernel variable.
!        Determine the length of the name of this variable; make
!        sure it's not too long.
!
         reqnam = nameln + itemln + templn

         if (       ( reqnam > kvnmln ) &
              .and. ( reqnum > kvnmln ) ) then
!
!           Both forms of the name are too long.
!
            call setmsg ( 'Kernel variable FRAME_#_# has length #; '// &
                          'kernel variable FRAME_#_# has length #; '// &
                          'maximum allowed length is #.  Neither '  // &
                          'variable could be searched for in the '  // &
                          'kernel pool due to these name length '   // &
                          'errors.'                                 )
            call errint ( '#',  frcode                              )
            call errch  ( '#',  item                                )
            call errint ( '#',  reqnum                              )
            call errch  ( '#',  frname                              )
            call errch  ( '#',  item                                )
            call errint ( '#',  reqnam                              )
            call errint ( '#',  kvnmln                              )
            call sigerr ( 'SPICE(VARNAMETOOLONG)'                   )
            return


         else if  ( reqnam > kvnmln ) then
!
!           We couldn't find the variable having the ID-based name,
!           and the frame name-based variable name is too long to
!           look up.
!
!           Note that at this point KVNAME contains the ID-based
!           kernel variable name.
!
            call setmsg ( 'Kernel variable # was expected to '      // &
                          'be present in the kernel pool but '      // &
                          'was not found.  The alternative '        // &
                          'form of kernel variable name '           // &
                          'FRAME_#_# was not searched for '         // &
                          'because this name has excessive '        // &
                          'length (# characters vs allowed '        // &
                          'maximum of #).  One of these variables ' // &
                          'is needed to define the reference '      // &
                          'frame #.  Usually '                      // &
                          'this type of problem is due to a '       // &
                          'missing keyword assignment in a frame '  // &
                          'kernel.  Another, less likely, '         // &
                          'possibility is that other errors in a '  // &
                          'frame kernel have confused the '         // &
                          'frame subsystem into wrongly '           // &
                          'deciding these variables are needed.'    )
            call errch  ( '#',  kvname                              )
            call errch  ( '#',  frname                              )
            call errch  ( '#',  item                                )
            call errint ( '#',  reqnam                              )
            call errint ( '#',  kvnmln                              )
            call errch  ( '#',  frname                              )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)'                )
            return

         end if

!
!        Now try looking for a kernel variable including the frame
!        name.
!
         call repmc  ( templt, '#',   frname, kvname )
         call repmc  ( kvname, '#',   item,   kvname )
         call dtpool ( kvname, found, n,      dtype  )


         if (  ( .not. found ) .and. ( reqnum > kvnmln )  ) then
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
            call setmsg ( 'Kernel variable # was expected to '      // &
                          'be present in the kernel pool but '      // &
                          'was not found.  The alternative '        // &
                          'form of kernel variable name '           // &
                          'FRAME_#_# was not searched for '         // &
                          'because this name has excessive '        // &
                          'length (# characters vs allowed '        // &
                          'maximum of #).  One of these variables ' // &
                          'is needed to define the reference '      // &
                          'frame #.  Usually '                      // &
                          'this type of problem is due to a '       // &
                          'missing keyword assignment in a frame '  // &
                          'kernel.  Another, less likely, '         // &
                          'possibility is that other errors in a '  // &
                          'frame kernel have confused the '         // &
                          'frame subsystem into wrongly '           // &
                          'deciding these variables are needed.'    )
            call errch  ( '#',  kvname                              )
            call errint ( '#',  frcode                              )
            call errch  ( '#',  item                                )
            call errint ( '#',  reqnum                              )
            call errint ( '#',  kvnmln                              )
            call errch  ( '#',  frname                              )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)'                )
            return


         else if ( .not. found ) then
!
!           We tried to look up both names and failed.
!
            call setmsg ( 'At least one of the kernel variables '   // &
                          'FRAME_#_# or FRAME_#_# was expected to ' // &
                          'be present in the kernel pool but '      // &
                          'neither was found. One of these '        // &
                          'variables is needed to define the '      // &
                          'reference frame #.  Usually '            // &
                          'this type of problem is due to a '       // &
                          'missing keyword assignment in a frame '  // &
                          'kernel.  Another, less likely, '         // &
                          'possibility is that other errors in a '  // &
                          'frame kernel have confused the '         // &
                          'frame subsystem into wrongly '           // &
                          'deciding these variables are needed.'    )
            call errint ( '#',  frcode                              )
            call errch  ( '#',  item                                )
            call errch  ( '#',  frname                              )
            call errch  ( '#',  item                                )
            call errch  ( '#',  frname                              )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)'                )
            return

         end if

      end if

!
!     Getting to this point means we found a kernel variable. The name
!     of the variable is KVNAME.  The data type is DTYPE and the
!     cardinality is N.
!
      if ( dtype == 'C' ) then
!
!        Rather than using BADKPV, we check the cardinality of the
!        kernel variable in-line so we can create a more detailed error
!        message if need be.
!
         if ( n > 1 ) then

            call setmsg ( 'The kernel variable # has used to define ' // &
                          'frame # was expected to have size not '    // &
                          'exceeding 1 but in fact has size #. '      // &
                          'Usually this type of '                     // &
                          'problem is due to an error in a frame '    // &
                          'definition provided in a frame kernel.'    )
            call errch  ( '#',  kvname                                )
            call errch  ( '#',  frname                                )
            call errint ( '#',  n                                     )
            call sigerr ( 'SPICE(BADVARIABLESIZE)'                    )
            return

         end if

!
!        Look up the kernel variable.
!
         call gcpool ( kvname, 1, 1, n, bodnam, found )

         if ( .not. found ) then

            call setmsg ( 'Variable # not found after DTPOOL ' // &
                          'indicated it was present in pool.'  )
            call errch  ( '#',  kvname                         )
            call sigerr ( 'SPICE(BUG)'                         )
            return

         end if

!
!        Convert the body name to a body code.
!
         call bods2c ( bodnam, idcode, found )

         if ( .not. found ) then

            call setmsg ( 'Body name # could not be translated ' // &
                          'to an ID code.'                       )
            call errch  ( '#',  bodnam                           )
            call sigerr ( 'SPICE(NOTRANSLATION)'                 )
            return

         end if


      else
!
!        The variable has numeric type.
!
         if ( n > 1 ) then

            call setmsg ( 'The kernel variable # has used to define ' // &
                          'frame # was expected to have size not '    // &
                          'exceeding 1 but in fact has size #. '      // &
                          'Usually this type of '                     // &
                          'problem is due to an error in a frame '    // &
                          'definition provided in a frame kernel.'    )
            call errch  ( '#',  kvname                                )
            call errch  ( '#',  frname                                )
            call errint ( '#',  n                                     )
            call sigerr ( 'SPICE(BADVARIABLESIZE)'                    )
            return

         end if

!
!        Look up the kernel variable.
!
         call gipool ( kvname, 1, 1, n, idcode, found )

         if ( .not. found ) then

            call setmsg ( 'Variable # not found after DTPOOL ' // &
                          'indicated it was present in pool.'  )
            call errch  ( '#',  kvname                         )
            call sigerr ( 'SPICE(BUG)'                         )
            return

         end if

      end if

      return
      end


