!$Procedure ZZPRSMET ( Private: parse method string )

      subroutine zzprsmet ( bodyid, method, mxnsrf, shape,  subtyp, &
                            pri,    nsurf,  srflst, pntdef, trmtyp )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Parse a method input string passed by a caller to a SPICE
!     high-level geometry API. Return parameter values specified by the
!     string.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     SCANNING
!     PARSING
!     CONSTANTS
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           bodyid
      character(*)          method
      integer(ip)           mxnsrf
      character(*)          shape
      character(*)          subtyp
      logical               pri
      integer(ip)           nsurf
      integer(ip)           srflst ( * )
      character(*)          pntdef
      character(*)          trmtyp

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID code.
!     METHOD     I   Method string.
!     MXNSRF     I   Maximum number of surface IDs that can be returned.
!     SHAPE      O   Target shape.
!     SUBTYP     O   Sub-point type.
!     PRI        O   Prioritization flag.
!     NSURF      O   Number of surface IDs in list.
!     SRFLST     O   Surface ID list.
!     PNTDEF     O   Limb or terminator point definition.
!     TRMTYP     O   Terminator type.
!     MAXSRF     P   DSK subsystem maximum surface list size.
!
! $ Detailed_Input
!
!     BODYID     is the body ID code of an ephemeris object. This
!                object is normally the "target" body of a geometric
!                computation. Any surfaces specified in the method
!                string are associated with this body. BODYID is
!                needed to map surface names to surface ID codes.
!
!     METHOD     is a "method" string to be parsed. This string
!                normally is passed by a caller to a SPICE high-level
!                high-level geometry routine.
!
!                METHOD indicates whether a target body surface is
!                to be modeled as an ellipsoid or by DSK data. It
!                may contain additional, computation-dependent
!                parameters.
!
!     MXNSRF     is the maximum number of surface IDs that can be
!                returned in the output array SRFLST. Normally
!                the caller should use the parameter MAXSRF to
!                set this value.
!
!
! $ Detailed_Output
!
!     SHAPE      is a string describing the shape model. Values are
!
!                    'ELLIPSOID' or 'DSK'
!
!
!     SUBTYP     is a string describing a sub-observer or sub-solar
!                point definition. Values are
!
!                    'INTERCEPT' or 'NEAR POINT'
!
!
!     PRI        is a logical flag indicating whether a DSK-based
!                computation uses DSK segments in a prioritized
!                fashion. PRI is .TRUE. if and only if the data
!                usage is prioritized. Note that for the N0066
!                version of SPICE, all DSK-based computations
!                use unprioritized data.
!
!
!     NSURF      is the number of elements of the surface ID list.
!
!
!     SRFLST     is a list of surface ID codes. All surfaces are
!                associated with the body designated by BODYID.
!
!
!     PNTDEF     is a string describing the point definition used
!                to compute limb or terminator points. Values are
!
!                   'TANGENT' or 'GUIDED'
!
!
!     TRMTYP     is a string that indicates the definition of the
!                terminator on the target body. Values are:
!
!                   'UMBRAL' or 'PENUMBRAL'
!
! $ Parameters
!
!     MAXSRF is the maximum surface list size used by the DSK
!     subsystem. See dsk.inc for the parameter's value.
!
! $ Exceptions
!
!     1)  If more than one shape keyword is found in METHOD,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!     2)  If more than one surface keyword is found in METHOD,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!     3)  If the surface keyword is found in METHOD, but no
!         surfaces are listed, the error SPICE(BADMETHODSYNTAX) is
!         signaled.
!
!     4)  If the equals sign ('=') is missing from the surface list,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!     5)  If the method string contains a double quote character (")
!         that doesn't delimit a quoted-string, the error
!          SPICE(BADMETHODSYNTAX) is signaled.
!
!     6)  If a surface name in the METHOD string can't be converted
!         to a surface ID code, the error SPICE(IDCODENOTFOUND) is
!         signaled.
!
!     7)  If all ID codes in the surface list can't be stored in the
!         SRFLST array, the error SPICE(TOOMANYSURFACES) is signaled.
!
!     8)  If the surface list in METHOD contains a comma not followed
!         by a surface name, the error SPICE(BADMETHODSYNTAX) is
!         signaled.
!
!     9)  If the surface list in METHOD contains an unrecognized token,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!    10)  If the string METHOD contains an extra prioritization token,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!    12)  If the string METHOD contains an extra sub-point type token,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!    13)  If the string METHOD contains an extra point definition
!         token, the error SPICE(BADMETHODSYNTAX) is signaled.
!
!    14)  If the string METHOD contains an extra terminator type token,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!    15)  If the string METHOD contains an extra slash character,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
!    16)  If the string METHOD lacks a prioritization keyword,
!         the error SPICE(BADPRIORITYSPEC) is signaled.
!
!    17)  If a "legacy" method string is not one of the expected
!         values, the error SPICE(BADMETHODSYNTAX) is signaled.
!
!    18)  If the string METHOD contains any other unexpected tokens,
!         the error SPICE(BADMETHODSYNTAX) is signaled.
!
! $ Files
!
!     Surface name-to-ID mappings may be defined at run time by loading
!     text kernels containing kernel variable assignments of the form
!
!        NAIF_SURFACE_NAME += ( <surface name 1>, ... )
!        NAIF_SURFACE_CODE += ( <surface code 1>, ... )
!        NAIF_SURFACE_BODY += ( <body code 1>,    ... )
!
!     Above, the Ith elements of the lists on the assignments' right
!     hand sides together define the Ith surface name/ID mapping.
!
!     The same effect can be achieved using assignments formatted as
!     follows:
!
!        NAIF_SURFACE_NAME += <surface name 1>
!        NAIF_SURFACE_CODE += <surface code 1>
!        NAIF_SURFACE_BODY += <body code 1>
!
!        NAIF_SURFACE_NAME += <surface name 2>
!        NAIF_SURFACE_CODE += <surface code 2>
!        NAIF_SURFACE_BODY += <body code 2>
!
!           ...
!
!     Note the use of the
!
!        +=
!
!     operator; this operator appends to rather than overwrites the
!     kernel variable named on the left hand side of the assignment.
!
!
! $ Particulars
!
!     This routine supports parsing of the METHOD input argument
!     used by the SPICE geometry APIs
!
!        GFOCLT
!        ILLUMF
!        ILLUMG
!        ILUMIN
!        LATSRF
!        LIMBPT
!        SINCPT
!        SUBPNT
!        SUBSLR
!        TERMPT
!
!     Each API has its own syntax for its METHOD input argument.
!     All routines use a common syntax for surface lists. See
!     the headers of the routines for the exact syntax supported
!     by each routine.
!
!     In the METHOD string, the token delimiters are
!
!        / = , <blank>
!
!     Strings delimited by double quote characters are treated as
!     individual tokens. There is no escape syntax for indicating a
!     doubly quoted string within a doubly quoted string.
!
! $ Examples
!
!
!     1) For the routine SUBPNT,
!
!           Method string =
!
!              METHOD = 'INTERCEPT / '
!              //       'DSK/UNPRIORITIZED/SURFACES = "MGS '
!              //       'MOLA 128 PIXEL/DEG", MARS_LOWRES'
!
!          Output parameters:
!
!             SHAPE  = 'DSK'
!             SUBTYP = 'INTERCEPT'
!             PRI    = .FALSE.
!             NSURF  = 2
!             SRFLST = <ID code 1>, <ID code 2>
!             PNTDEF = ' '
!             TRMTYP = ' '
!
!
!     2) For the routine TERMPT,
!
!           Method string =
!
!              METHOD = 'UMBRAL/TANGENT/ '
!              //       'DSK/UNPRIORITIZED/SURFACES = "MGS '
!              //       'MOLA 128 PIXEL/DEG", MARS_LOWRES'
!
!          Output parameters:
!
!             SHAPE  = 'DSK'
!             SUBTYP = ' '
!             PRI    = .FALSE.
!             NSURF  = 2
!             SRFLST = <ID code 1>, <ID code 2>
!             PNTDEF = 'TANGENT'
!             TRMTYP = 'UMBRAL'
!
!
! $ Restrictions
!
!     This is a specialized parsing routine. It it meant to be used
!     only for the parsing "method" strings as described in
!     Particulars.
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
!-    SPICELIB Version 1.0.0, 24-FEB-2016 (NJB)
!
!        Based on version  4.0.0 12-NOV-2015 (NJB)
!        First version was 1.0.0 14-JAN-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     parse method string for geometry api routines
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr
      logical               failed
      logical               return

!
!     Local parameters
!
      character(*), parameter :: comma = ','

      character(*), parameter :: dquote = '"'

      character(*), parameter :: equals = '='

      character(*), parameter :: slash = '/'

      integer(ip), parameter :: nmax = 100

      integer(ip), parameter :: metlen = 1000

!
!     Local variables
!
      character(1)          chr
      character(metlen)     locmth

      integer(ip)           begs ( nmax + 2 )
      integer(ip)           code
      integer(ip)           ends ( nmax + 2 )
      integer(ip)           i
      integer(ip)           n
      integer(ip)           npri
      integer(ip)           nshape
      integer(ip)           nlmbtp
      integer(ip)           nsubtp
      integer(ip)           nsrfls
      integer(ip)           ntrmtp

      logical               done
      logical               fnd


      if ( return() ) return



!
!     No shape or surfaces have been specified yet.
!
      shape  = ' '
      subtyp = ' '
      pntdef = ' '
      trmtyp = ' '
      pri    = .true.
      nsurf  = 0

!
!     Initialize clause counts.
!
      npri   = 0
      nshape = 0
      nsubtp = 0
      nlmbtp = 0
      ntrmtp = 0
      nsrfls = 0

      locmth(1:1) = slash
      begs(1)     = 1
      ends(1)     = 1

      call ljucrs ( 1, method, locmth(2:) )

!
!     Tokenize the input method string.
!
      call zzlexmet ( locmth(2:), nmax, n, begs(2), ends(2) )

      if ( failed() ) return

      n = n + 1

      do i = 2, n
         begs(i) = begs(i) + 1
         ends(i) = ends(i) + 1
      end do

!
!     Identify the target shape.
!
      done = .false.
      i    = 1

      do while ( .not. done )

         if (  locmth( begs(i):ends(i) ) ==  'ELLIPSOID' ) then

            nshape = nshape + 1
            shape  = 'ELLIPSOID'

         else if (  locmth( begs(i):ends(i) ) ==  'DSK' ) then

            nshape = nshape + 1
            shape  = 'DSK'

         end if

         if ( i == n ) then

            done = .true.
         else
            i    = i + 1
         end if

      end do


      if ( nshape /= 1 ) then

         call setmsg ( 'The "method" or "shape" string must contain ' &
         //            'exactly one instance of a shape keyword: ' &
         //            'ELLIPSOID or DSK, or additionally, in the ' &
         //            'case of occultation computations, POINT. ' &
         //            'The number of shape keywords was #. ' &
         //            'The input string was <#>.'                  )
         call errint ( '#', nshape                                  )
         call errch  ( '#', method                                  )
         call sigerr ( 'SPICE(BADMETHODSYNTAX)'                     )
         return

      end if


      if ( shape == 'DSK' ) then
!
!        Traverse the tokens; identify clauses in the method string.
!
         i = 1

         do while ( i < n )

            chr = locmth( begs(i):ends(i) )

            if ( chr == slash ) then
!
!              If the method string is correct, the next token should
!              be a keyword.
!
               i = i + 1

               if ( locmth( begs(i):ends(i) )  ==  'SURFACES' ) then
!
!                 Normally, we're looking at the start of a surface
!                 list at this point. We need at least an assignment
!                 operator ('=') and a surface name or ID code following
!                 the SURFACES keyword.
!
                  nsrfls = nsrfls + 1

                  if ( nsrfls > 1 ) then
!
!                    We have too many surface specifications.
!
                     call setmsg ( 'Extra SURFACES keyword was found ' &
                     //            'in the method string <#>.'       )
                     call errch  ( '#', method                       )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'          )
                     return

                  end if

                  if ( i > n-2 ) then

                     call setmsg ( 'The surface list in the method ' &
                     //            'string appears to be truncated. ' &
                     //            'The method string is <#>.'       )
                     call errch  ( '#', method                       )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'          )
                     return

                  end if

                  i   = i + 1

                  chr = locmth( begs(i):ends(i) )

                  if ( chr /= equals ) then

                     call setmsg ( 'The surface list in the method ' &
                     //            'string lacks an "=" sign after ' &
                     //            'the SURFACES keyword. ' &
                     //            'The method string is <#>.'      )
                     call errch  ( '#', method                      )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'         )
                     return

                  end if

!
!                 Prepare to read a list of names, quoted
!                 strings, or ID codes. The list must be non-empty. If
!                 there are multiple list entries, they're delimited by
!                 commas.
!
                  i    = i + 1
                  done = .false.

                  do while ( .not. done )

                     chr = locmth( begs(i):begs(i) )

                     if ( chr == dquote ) then
!
!                       We expect the current token to be a quoted
!                       string.
!
                        if ( ends(i) > begs(i)+1 ) then

                           begs(i) = begs(i) + 1
                           ends(i) = ends(i) - 1

                        else
!
!                          We have an invalid quoted string.
!
                           call setmsg ( 'The surface list in the ' &
                           //            'method string ' &
                           //            'contains a double quote ' &
                           //            'character that is not a ' &
                           //            'delimiter of a valid quoted ' &
                           //            'string. ' &
                           //            'The method string is <#>.'   )
                           call errch  ( '#', method                   )
                           call sigerr ( 'SPICE(BADMETHODSYNTAX)'      )
                           return

                        end if

                     end if

!                    We have either a name or an integer in string form.
!                    Convert the token to a surface ID code.
!
                     call srfscc ( locmth(begs(i):ends(i)), &
                                   bodyid,   code,   fnd   )

                     if ( failed() ) return

                     if ( .not. fnd ) then

                        call setmsg ( 'The surface name <#> could not ' &
                        //            'be translated to an ID code. ' &
                        //            'The method string is <#>.'     )
                        call errch  ( '#', locmth(begs(i):ends(i))    )
                        call errch  ( '#', method                     )
                        call sigerr ( 'SPICE(IDCODENOTFOUND)'         )
                        return

                     end if


                     nsurf = nsurf + 1
!
!                    Make sure there's room in the surface ID array.
!
                     if ( nsurf > mxnsrf ) then

                        call setmsg ( 'The surface name <#> could not ' &
                        //            'be stored in the surface list ' &
                        //            'due to lack of room. The max' &
                        //            'imum number of surfaces that ' &
                        //            'can be specified is #. ' &
                        //            'The method string is <#>.'     )
                        call errch  ( '#', locmth(begs(i):ends(i))    )
                        call errint ( '#', mxnsrf                     )
                        call errch  ( '#', method                     )
                        call sigerr ( 'SPICE(TOOMANYSURFACES)'        )
                        return

                     end if

!
!                    Append the surface ID to the surface ID array.
!
                     srflst( nsurf ) = code


                     if ( i == n ) then
!
!                       We're at the end of the method string.
!
                        done = .true.

                     else
!
!                       There are more tokens; the surface list may
!                       contain more surface names or ID codes.
!
                        i   = i + 1
                        chr = locmth( begs(i):ends(i) )

                        if ( chr == slash ) then
!
!                          We're at the end of the surface list.
!
                           done = .true.
!
!                          Decrement the token pointer so that the
!                          slash will be seen as the next token.
!
                           i    = i - 1


                        else if ( chr == comma ) then
!
!                          We expect to find another surface name or
!                          ID in the list.
!
                           if ( i < n ) then

                              i = i + 1

                           else
!
!                             We have a syntax error in the surface
!                             list.
!
                              call setmsg ( 'The surface list in the ' &
                              //            'method string contains ' &
                              //            'a comma that is not ' &
                              //            'followed by a surface ' &
                              //            'name or ID code. The ' &
                              //            'method string is <#>.'   )
                              call errch  ( '#', method               )
                              call sigerr ( 'SPICE(BADMETHODSYNTAX)'  )
                              return

                           end if


                        else
!
!                          We have a syntax error in the surface list.
!
                           call setmsg ( 'The surface list in the ' &
                           //            'method string is followed ' &
                           //            'by the unexpected token <#>.' &
                           //            ' The method string is <#>.'  )
                           call errch  ( '#', locmth(begs(i):ends(i))  )
                           call errch  ( '#', method                   )
                           call sigerr ( 'SPICE(BADMETHODSYNTAX)'      )
                           return

                        end if

                     end if

                  end do


               else if (      locmth( begs(i):ends(i) ) &
                         == 'UNPRIORITIZED'            ) then


                  npri = npri + 1

                  if ( npri > 1 ) then
!
!                    We have too many prioritization specifications.
!
                     call setmsg ( 'Extra prioritization keyword was ' &
                     //            'found in the method string <#>.' )
                     call errch  ( '#', method                       )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'          )
                     return

                  end if


                  pri  = .false.


               else if (     (       locmth( begs(i):ends(i) ) &
                               == 'INTERCEPT'                ) &
                        .or. (       locmth( begs(i):ends(i) ) &
                               == 'NADIR'                    )  ) then
!
!                 This is a sub-point type specification.
!
                  nsubtp = nsubtp + 1

                  if ( nsubtp > 1 ) then
!
!                    We have too many sub-point specifications.
!
                     call setmsg ( 'Extra sub-point type <#> was found ' &
                     //            'in the method string <#>.'         )
                     call errch  ( '#', locmth( begs(i):ends(i) )      )
                     call errch  ( '#', method                         )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'            )
                     return

                  end if

                  subtyp = locmth( begs(i):ends(i) )


               else if (     (       locmth( begs(i):ends(i) ) &
                               == 'TANGENT'                ) &
                        .or. (       locmth( begs(i):ends(i) ) &
                               == 'GUIDED'                   )  ) then
!
!                 This is a point definition specification.
!
                  nlmbtp = nlmbtp + 1

                  if ( nlmbtp > 1 ) then
!
!                    We have too many point definition specifications.
!
                     call setmsg ( 'Extra point definition <#> was ' &
                     //            'found in the method string <#>.'   )
                     call errch  ( '#', locmth( begs(i):ends(i) )      )
                     call errch  ( '#', method                         )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'            )
                     return

                  end if

                  pntdef = locmth( begs(i):ends(i) )



               else if (     (       locmth( begs(i):ends(i) ) &
                               == 'UMBRAL'                   ) &
                        .or. (       locmth( begs(i):ends(i) ) &
                               == 'PENUMBRAL'                )  ) then
!
!                 This is a terminator type specification.
!
                  ntrmtp = ntrmtp + 1

                  if ( ntrmtp > 1 ) then
!
!                    We have too many terminator type specifications.
!
                     call setmsg ( 'Extra terminator type <#> was ' &
                     //            'found in the method string <#>.'   )
                     call errch  ( '#', locmth( begs(i):ends(i) )      )
                     call errch  ( '#', method                         )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'            )
                     return

                  end if

                  trmtyp = locmth( begs(i):ends(i) )



               else if (  locmth( begs(i):ends(i) ) == slash  )  then
!
!                 Adjust the token index in the message to account
!                 for the insertion of a slash at the beginning.
!
                  call setmsg ( 'An unexpected slash character was ' &
                  //            'found at index # in the method ' &
                  //            'string <#>.'                          )
                  call errint ( '#', begs(i)-1                         )
                  call errch  ( '#', method                            )
                  call sigerr ( 'SPICE(BADMETHODSYNTAX)'               )
                  return


               else if ( locmth( begs(i):ends(i) )  /=  'DSK'  ) then
!
!                 'DSK' was the only valid token that could appear
!                 at this point.
!
                  call setmsg ( 'Unexpected token <#> was found ' &
                  //            'in the method string <#>.'       )
                  call errch  ( '#', locmth( begs(i):ends(i) )    )
                  call errch  ( '#', method                       )
                  call sigerr ( 'SPICE(BADMETHODSYNTAX)'          )
                  return

               end if


            end if

            i = i + 1

         end do

!
!        Currently, the UNPRIORITIZED keyword must be provided
!        for DSK shapes.
!
         if ( pri ) then

            call setmsg ( 'The keyword UNPRIORITIZED must be ' &
            //            'present in the method string <#> ' &
            //            'when the target shape is DSK. ' &
            //            'Prioritized DSK segment searches ' &
            //            'are not currently supported.'      )
            call errch  ( '#', method                         )
            call sigerr ( 'SPICE(BADPRIORITYSPEC)'            )
            return

         end if


      else if ( shape == 'ELLIPSOID' ) then
!
!        Check for legacy string inputs.
!
         if ( eqstr(method, 'ELLIPSOID') ) then
!
!           There are no other outputs to set.
!
            return

         else if (      eqstr(method, 'NEARPOINT:ELLIPSOID') &
                   .or. eqstr(method, 'ELLIPSOID:NEARPOINT') &
                   .or. eqstr(method, 'NEARPOINT/ELLIPSOID') &
                   .or. eqstr(method, 'ELLIPSOID/NEARPOINT')  ) then

            subtyp = 'NEAR POINT'

            return

         else if (      eqstr(method, 'INTERCEPT:ELLIPSOID') &
                   .or. eqstr(method, 'ELLIPSOID:INTERCEPT') &
                   .or. eqstr(method, 'INTERCEPT/ELLIPSOID') &
                   .or. eqstr(method, 'ELLIPSOID/INTERCEPT')  ) then

            subtyp = 'INTERCEPT'

            return

         end if

!
!        At this point, we should have a "modern" style of method
!        specification for an ellipsoidal shape model.
!        Parse the method string.
!
!        Traverse the tokens; identify clauses in the method string.
!
         i = 1

         do while ( i <= n )

            chr = locmth( begs(i):ends(i) )

            if ( chr == slash ) then
!
!              If the method string is correct, the next token should
!              be a keyword. There had better be a next token.
!
               if ( i == n ) then

                  call setmsg ( 'Expected more tokens after final <#> ' &
                     //         'in the method string <#>.'            )
                  call errch  ( '#', locmth( begs(i):ends(i) )         )
                  call errch  ( '#', method                            )
                  call sigerr ( 'SPICE(BADMETHODSYNTAX)'               )
                  return

               end if

               i = i + 1


               if (  locmth( begs(i):ends(i) ) ==  'ELLIPSOID' ) then
!
!                 This case is a no-op.
!

               else if (     (       locmth( begs(i):ends(i) ) &
                               == 'INTERCEPT'                ) &
                        .or. (       locmth( begs(i):ends(i) ) &
                               == 'NADIR'                    ) &
                        .or. (       locmth( begs(i):ends(i) ) &
                               == 'NEAR'                     )  ) then
!
!                 This is a sub-point type specification.
!
                  nsubtp = nsubtp + 1

                  if ( nsubtp > 1 ) then
!
!                    We have too many sub-point specifications.
!
                     call setmsg ( 'Extra sub-point type <#> was found ' &
                     //            'in the method string <#>.'         )
                     call errch  ( '#', locmth( begs(i):ends(i) )      )
                     call errch  ( '#', method                         )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'            )
                     return

                  end if

                  if ( locmth( begs(i):ends(i) ) == 'NEAR' ) then
!
!                    This may be a combination of old style and
!                    new syntax, e.g. 'ELLIPSOID/NEAR POINT'.
!
                     if (       ( shape == 'ELLIPSOID' ) &
                          .and. ( i     <  n          )  ) then
!
!                       We allow the "near point" sub-point type
!                       for ellipsoids but not for DSK surfaces.
!
                        if (     locmth( begs(i+1) : ends(i+1) ) &
                            == 'POINT'                          ) then

                           subtyp = 'NADIR'

                        end if

                     end if

                  else
                     subtyp = locmth( begs(i):ends(i) )
                  end if





               else if (     (       locmth( begs(i):ends(i) ) &
                               == 'TANGENT'                ) &
                        .or. (       locmth( begs(i):ends(i) ) &
                               == 'GUIDED'                   )  ) then
!
!                 This is a point definition specification.
!

                  nlmbtp = nlmbtp + 1

                  if ( nlmbtp > 1 ) then
!
!                    We have too many point definition specifications.
!
                     call setmsg ( 'Extra point definition <#> was ' &
                     //            'found in the method string <#>.'   )
                     call errch  ( '#', locmth( begs(i):ends(i) )      )
                     call errch  ( '#', method                         )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'            )
                     return

                  end if

                  pntdef = locmth( begs(i):ends(i) )

               else if (     (       locmth( begs(i):ends(i) ) &
                               == 'UMBRAL'                   ) &
                        .or. (       locmth( begs(i):ends(i) ) &
                               == 'PENUMBRAL'                )  ) then
!
!                 This is a terminator type specification.
!
                  ntrmtp = ntrmtp + 1

                  if ( ntrmtp > 1 ) then
!
!                    We have too many terminator type specifications.
!
                     call setmsg ( 'Extra terminator type <#> was ' &
                     //            'found in the method string <#>.'   )
                     call errch  ( '#', locmth( begs(i):ends(i) )      )
                     call errch  ( '#', method                         )
                     call sigerr ( 'SPICE(BADMETHODSYNTAX)'            )
                     return

                  end if

                  trmtyp = locmth( begs(i):ends(i) )

               else
!
!                 Sorry, no other strings are allowed.
!
                  call setmsg( 'Unexpected method string <#> was found ' &
                  //           'specifying an ellipsoid shape.'        )
                  call errch ( '#', method                             )
                  call sigerr( 'SPICE(BADMETHODSYNTAX)'                )
                  return

               end if

            else

               call setmsg( 'Unexpected method string <#> was found ' &
               //           'specifying an ellipsoid shape.'        )
               call errch ( '#', method                             )
               call sigerr( 'SPICE(BADMETHODSYNTAX)'                )
               return

            end if



            i = i + 1

         end do

      else
!
!        This is a backstop error check.
!
         call setmsg ( 'Unexpected shape value # was found. ' &
         //            'This is due to a coding error, not ' &
         //            'to user input.'                      )
         call errch  ( '#', shape                            )
         call sigerr ( 'SPICE(BUG)'                          )
         return

      end if

      return
      end
