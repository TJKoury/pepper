!$Procedure ZZDYNVAI ( Fetch array, integer frame kernel variable )

      subroutine zzdynvai ( frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Look up array-valued integer frame kernel variable. The frame
!     name or frame ID may be used as part of the variable's name.
!
!     If the kernel variable is not present, or if the variable
!     has the wrong data type, signal an error.
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

      include 'zzdyn.inc'

      character*(*)         frname
      integer(ip)           frcode
      character*(*)         item
      integer(ip)           maxn
      integer(ip)           n
      integer(ip)           values  ( * )

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
!                    this routine will look for a numeric variable
!                    of that name first.
!
! $ Detailed_Output
!
!     N              is the number of values returned in the array
!                    VALUES.
!
!     VALUES         are the values associated with the requested
!                    array-valued, integer kernel variable. The kernel
!                    variable name of the form
!
!                       FRAME_<frame ID code>_<ITEM>
!
!                    will be looked up first; if this variable
!                    is found and has numeric type, the associated
!                    values will be returned.  If this variable is
!                    not found, the variable
!
!                       FRAME_<frame name>_<ITEM>
!
!                    will be looked up.  If a numeric variable
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
!        variable names is found, but that variable has character data
!        type, the error SPICE(BADVARIABLETYPE) will be signaled.
!
!     5) If kernel variable matching one form of the requested kernel
!        variable names is found, but that variable has more than MAXN
!        associated values, the error SPICE(BADVARIABLESIZE) will be
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
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           rtrim

      logical               failed
      logical               return

!
!     Local Parameters
!
      character*(*)         templt
      parameter           ( templt = 'FRAME_#_#' )

!
!     TEMPLN is the length of the keyword template, minus
!     the sum of the lengths of the two substitution markers ('#').
!
      integer(ip)           templn
      parameter           ( templn = 7 )

!
!     Local Variables
!
      character*(kvnmln)    cdestr
      character*(1)         dtype
      character*(kvnmln)    kvname

      integer(ip)           codeln
      integer(ip)           itemln
      integer(ip)           nameln
      integer(ip)           reqnam
      integer(ip)           reqnum

      logical               found

!
!     Standard SPICE error handling
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZDYNVAI' )

!
!     Prepare to check the name of the kernel variable we're about
!     to look up.
!
!     Convert the frame code to a string.
!
      call intstr ( frcode, cdestr )

      if ( failed() ) then
         call chkout ( 'ZZDYNVAI' )
         return
      end if

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

      if ( reqnum .le. kvnmln ) then
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

         if (       ( reqnam .gt. kvnmln ) &
              .and. ( reqnum .gt. kvnmln ) ) then
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
            call chkout ( 'ZZDYNVAI'                                )
            return


         else if  ( reqnam .gt. kvnmln ) then
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
                          'is needed to define the parameterized '  // &
                          'dynamic frame #.  Usually this type '    // &
                          'of problem is due to an error in a '     // &
                          'frame definition provided in a frame '   // &
                          'kernel.'                                )
            call errch  ( '#',  kvname                              )
            call errch  ( '#',  frname                              )
            call errch  ( '#',  item                                )
            call errint ( '#',  reqnam                              )
            call errint ( '#',  kvnmln                              )
            call errch  ( '#',  frname                              )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)'                )
            call chkout ( 'ZZDYNVAI'                                )
            return

         end if

!
!        Now try looking for a kernel variable including the frame
!        name.
!
         call repmc  ( templt, '#',   frname, kvname )
         call repmc  ( kvname, '#',   item,   kvname )
         call dtpool ( kvname, found, n,      dtype  )


         if (  ( .not. found ) .and. ( reqnum .gt. kvnmln )  ) then
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
                          'is needed to define the parameterized '  // &
                          'dynamic frame #.  Usually this type '    // &
                          'of problem is due to an error in a '     // &
                          'frame definition provided in a frame '   // &
                          'kernel.'                                 )
            call errch  ( '#',  kvname                              )
            call errint ( '#',  frcode                              )
            call errch  ( '#',  item                                )
            call errint ( '#',  reqnum                              )
            call errint ( '#',  kvnmln                              )
            call errch  ( '#',  frname                              )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)'                )
            call chkout ( 'ZZDYNVAI'                                )
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
                          'parameterized dynamic frame #.  Usually '// &
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
            call chkout ( 'ZZDYNVAI'                                )
            return

         end if

      end if

!
!     Getting to this point means we found a kernel variable. The name
!     of the variable is KVNAME.  The data type is DTYPE and the
!     cardinality is N.
!
!     Rather than using BADKPV, we check the data type and cardinality
!     of the kernel variable in-line so we can create a more detailed
!     error message if need be.
!
      if ( dtype .eq. 'C' ) then

         call setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have integer '      // &
                       'data type but in fact has '                 // &
                       'character type.  Usually this type of '     // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call errch  ( '#',  kvname                                 )
         call errch  ( '#',  frname                                 )
         call sigerr ( 'SPICE(BADVARIABLETYPE)'                     )
         call chkout ( 'ZZDYNVAI'                                   )
         return

      end if


      if ( n .gt. maxn ) then

         call setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have size not '     // &
                       'exceeding # but in fact has size #. '       // &
                       'Usually this type of '                      // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call errch  ( '#',  kvname                                 )
         call errch  ( '#',  frname                                 )
         call errint ( '#',  maxn                                   )
         call errint ( '#',  n                                      )
         call sigerr ( 'SPICE(BADVARIABLESIZE)'                     )
         call chkout ( 'ZZDYNVAI'                                   )
         return

      end if

!
!     Look up the kernel variable.
!
      call gipool ( kvname, 1, maxn, n, values, found )

      if ( .not. found ) then

         call setmsg ( 'Variable # not found after DTPOOL ' // &
                       'indicated it was present in pool.'  )
         call errch  ( '#',  kvname                         )
         call sigerr ( 'SPICE(BUG)'                         )
         call chkout ( 'ZZDYNVAI'                           )
         return

      end if

      call chkout ( 'ZZDYNVAI' )
      return
      end


