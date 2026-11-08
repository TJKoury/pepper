!$Procedure ZZPRSMET ( Private: parse method string )

      SUBROUTINE ZZPRSMET ( BODYID, METHOD, MXNSRF, SHAPE,  SUBTYP, &
                            PRI,    NSURF,  SRFLST, PNTDEF, TRMTYP )

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

      IMPLICIT NONE

      INTEGER               BODYID
      CHARACTER*(*)         METHOD
      INTEGER               MXNSRF
      CHARACTER*(*)         SHAPE
      CHARACTER*(*)         SUBTYP
      LOGICAL               PRI
      INTEGER               NSURF
      INTEGER               SRFLST ( * )
      CHARACTER*(*)         PNTDEF
      CHARACTER*(*)         TRMTYP

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
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(*)         COMMA
      PARAMETER           ( COMMA  = ',' )

      CHARACTER*(*)         DQUOTE
      PARAMETER           ( DQUOTE = '"' )

      CHARACTER*(*)         EQUALS
      PARAMETER           ( EQUALS = '=' )

      CHARACTER*(*)         SLASH
      PARAMETER           ( SLASH  = '/' )

      INTEGER               NMAX
      PARAMETER           ( NMAX   = 100 )

      INTEGER               METLEN
      PARAMETER           ( METLEN = 1000 )

!
!     Local variables
!
      CHARACTER*(1)         CHR
      CHARACTER*(METLEN)    LOCMTH

      INTEGER               BEGS ( NMAX + 2 )
      INTEGER               CODE
      INTEGER               ENDS ( NMAX + 2 )
      INTEGER               I
      INTEGER               N
      INTEGER               NPRI
      INTEGER               NSHAPE
      INTEGER               NLMBTP
      INTEGER               NSUBTP
      INTEGER               NSRFLS
      INTEGER               NTRMTP

      LOGICAL               DONE
      LOGICAL               FND


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZPRSMET' )


!
!     No shape or surfaces have been specified yet.
!
      SHAPE  = ' '
      SUBTYP = ' '
      PNTDEF = ' '
      TRMTYP = ' '
      PRI    = .TRUE.
      NSURF  = 0

!
!     Initialize clause counts.
!
      NPRI   = 0
      NSHAPE = 0
      NSUBTP = 0
      NLMBTP = 0
      NTRMTP = 0
      NSRFLS = 0

      LOCMTH(1:1) = SLASH
      BEGS(1)     = 1
      ENDS(1)     = 1

      CALL LJUCRS ( 1, METHOD, LOCMTH(2:) )

!
!     Tokenize the input method string.
!
      CALL ZZLEXMET ( LOCMTH(2:), NMAX, N, BEGS(2), ENDS(2) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZPRSMET' )
         RETURN
      END IF

      N = N + 1

      DO I = 2, N
         BEGS(I) = BEGS(I) + 1
         ENDS(I) = ENDS(I) + 1
      END DO

!
!     Identify the target shape.
!
      DONE = .FALSE.
      I    = 1

      DO WHILE ( .NOT. DONE )

         IF (  LOCMTH( BEGS(I):ENDS(I) ) .EQ.  'ELLIPSOID' ) THEN

            NSHAPE = NSHAPE + 1
            SHAPE  = 'ELLIPSOID'

         ELSE IF (  LOCMTH( BEGS(I):ENDS(I) ) .EQ.  'DSK' ) THEN

            NSHAPE = NSHAPE + 1
            SHAPE  = 'DSK'

         END IF

         IF ( I .EQ. N ) THEN

            DONE = .TRUE.
         ELSE
            I    = I + 1
         END IF

      END DO


      IF ( NSHAPE .NE. 1 ) THEN

         CALL SETMSG ( 'The "method" or "shape" string must contain ' &
         //            'exactly one instance of a shape keyword: ' &
         //            'ELLIPSOID or DSK, or additionally, in the ' &
         //            'case of occultation computations, POINT. ' &
         //            'The number of shape keywords was #. ' &
         //            'The input string was <#>.'                  )
         CALL ERRINT ( '#', NSHAPE                                  )
         CALL ERRCH  ( '#', METHOD                                  )
         CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'                     )
         CALL CHKOUT ( 'ZZPRSMET'                                   )
         RETURN

      END IF


      IF ( SHAPE .EQ. 'DSK' ) THEN
!
!        Traverse the tokens; identify clauses in the method string.
!
         I = 1

         DO WHILE ( I .LT. N )

            CHR = LOCMTH( BEGS(I):ENDS(I) )

            IF ( CHR .EQ. SLASH ) THEN
!
!              If the method string is correct, the next token should
!              be a keyword.
!
               I = I + 1

               IF ( LOCMTH( BEGS(I):ENDS(I) )  .EQ.  'SURFACES' ) THEN
!
!                 Normally, we're looking at the start of a surface
!                 list at this point. We need at least an assignment
!                 operator ('=') and a surface name or ID code following
!                 the SURFACES keyword.
!
                  NSRFLS = NSRFLS + 1

                  IF ( NSRFLS .GT. 1 ) THEN
!
!                    We have too many surface specifications.
!
                     CALL SETMSG ( 'Extra SURFACES keyword was found ' &
                     //            'in the method string <#>.'       )
                     CALL ERRCH  ( '#', METHOD                       )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'          )
                     CALL CHKOUT ( 'ZZPRSMET'                        )
                     RETURN

                  END IF

                  IF ( I .GT. N-2 ) THEN

                     CALL SETMSG ( 'The surface list in the method ' &
                     //            'string appears to be truncated. ' &
                     //            'The method string is <#>.'       )
                     CALL ERRCH  ( '#', METHOD                       )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'          )
                     CALL CHKOUT ( 'ZZPRSMET'                        )
                     RETURN

                  END IF

                  I   = I + 1

                  CHR = LOCMTH( BEGS(I):ENDS(I) )

                  IF ( CHR .NE. EQUALS ) THEN

                     CALL SETMSG ( 'The surface list in the method ' &
                     //            'string lacks an "=" sign after ' &
                     //            'the SURFACES keyword. ' &
                     //            'The method string is <#>.'      )
                     CALL ERRCH  ( '#', METHOD                      )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'         )
                     CALL CHKOUT ( 'ZZPRSMET'                       )
                     RETURN

                  END IF

!
!                 Prepare to read a list of names, quoted
!                 strings, or ID codes. The list must be non-empty. If
!                 there are multiple list entries, they're delimited by
!                 commas.
!
                  I    = I + 1
                  DONE = .FALSE.

                  DO WHILE ( .NOT. DONE )

                     CHR = LOCMTH( BEGS(I):BEGS(I) )

                     IF ( CHR .EQ. DQUOTE ) THEN
!
!                       We expect the current token to be a quoted
!                       string.
!
                        IF ( ENDS(I) .GT. BEGS(I)+1 ) THEN

                           BEGS(I) = BEGS(I) + 1
                           ENDS(I) = ENDS(I) - 1

                        ELSE
!
!                          We have an invalid quoted string.
!
                           CALL SETMSG ( 'The surface list in the ' &
                           //            'method string ' &
                           //            'contains a double quote ' &
                           //            'character that is not a ' &
                           //            'delimiter of a valid quoted ' &
                           //            'string. ' &
                           //            'The method string is <#>.'   )
                           CALL ERRCH  ( '#', METHOD                   )
                           CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'      )
                           CALL CHKOUT ( 'ZZPRSMET'                    )
                           RETURN

                        END IF

                     END IF

!                    We have either a name or an integer in string form.
!                    Convert the token to a surface ID code.
!
                     CALL SRFSCC ( LOCMTH(BEGS(I):ENDS(I)), &
                                   BODYID,   CODE,   FND   )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'ZZPRSMET' )
                        RETURN
                     END IF

                     IF ( .NOT. FND ) THEN

                        CALL SETMSG ( 'The surface name <#> could not ' &
                        //            'be translated to an ID code. ' &
                        //            'The method string is <#>.'     )
                        CALL ERRCH  ( '#', LOCMTH(BEGS(I):ENDS(I))    )
                        CALL ERRCH  ( '#', METHOD                     )
                        CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'         )
                        CALL CHKOUT ( 'ZZPRSMET'                      )
                        RETURN

                     END IF


                     NSURF = NSURF + 1
!
!                    Make sure there's room in the surface ID array.
!
                     IF ( NSURF .GT. MXNSRF ) THEN

                        CALL SETMSG ( 'The surface name <#> could not ' &
                        //            'be stored in the surface list ' &
                        //            'due to lack of room. The max' &
                        //            'imum number of surfaces that ' &
                        //            'can be specified is #. ' &
                        //            'The method string is <#>.'     )
                        CALL ERRCH  ( '#', LOCMTH(BEGS(I):ENDS(I))    )
                        CALL ERRINT ( '#', MXNSRF                     )
                        CALL ERRCH  ( '#', METHOD                     )
                        CALL SIGERR ( 'SPICE(TOOMANYSURFACES)'        )
                        CALL CHKOUT ( 'ZZPRSMET'                      )
                        RETURN

                     END IF

!
!                    Append the surface ID to the surface ID array.
!
                     SRFLST( NSURF ) = CODE


                     IF ( I .EQ. N ) THEN
!
!                       We're at the end of the method string.
!
                        DONE = .TRUE.

                     ELSE
!
!                       There are more tokens; the surface list may
!                       contain more surface names or ID codes.
!
                        I   = I + 1
                        CHR = LOCMTH( BEGS(I):ENDS(I) )

                        IF ( CHR .EQ. SLASH ) THEN
!
!                          We're at the end of the surface list.
!
                           DONE = .TRUE.
!
!                          Decrement the token pointer so that the
!                          slash will be seen as the next token.
!
                           I    = I - 1


                        ELSE IF ( CHR .EQ. COMMA ) THEN
!
!                          We expect to find another surface name or
!                          ID in the list.
!
                           IF ( I .LT. N ) THEN

                              I = I + 1

                           ELSE
!
!                             We have a syntax error in the surface
!                             list.
!
                              CALL SETMSG ( 'The surface list in the ' &
                              //            'method string contains ' &
                              //            'a comma that is not ' &
                              //            'followed by a surface ' &
                              //            'name or ID code. The ' &
                              //            'method string is <#>.'   )
                              CALL ERRCH  ( '#', METHOD               )
                              CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'  )
                              CALL CHKOUT ( 'ZZPRSMET'                )
                              RETURN

                           END IF


                        ELSE
!
!                          We have a syntax error in the surface list.
!
                           CALL SETMSG ( 'The surface list in the ' &
                           //            'method string is followed ' &
                           //            'by the unexpected token <#>.' &
                           //            ' The method string is <#>.'  )
                           CALL ERRCH  ( '#', LOCMTH(BEGS(I):ENDS(I))  )
                           CALL ERRCH  ( '#', METHOD                   )
                           CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'      )
                           CALL CHKOUT ( 'ZZPRSMET'                    )
                           RETURN

                        END IF

                     END IF

                  END DO


               ELSE IF (      LOCMTH( BEGS(I):ENDS(I) ) &
                         .EQ. 'UNPRIORITIZED'            ) THEN


                  NPRI = NPRI + 1

                  IF ( NPRI .GT. 1 ) THEN
!
!                    We have too many prioritization specifications.
!
                     CALL SETMSG ( 'Extra prioritization keyword was ' &
                     //            'found in the method string <#>.' )
                     CALL ERRCH  ( '#', METHOD                       )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'          )
                     CALL CHKOUT ( 'ZZPRSMET'                        )
                     RETURN

                  END IF


                  PRI  = .FALSE.


               ELSE IF (     (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'INTERCEPT'                ) &
                        .OR. (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'NADIR'                    )  ) THEN
!
!                 This is a sub-point type specification.
!
                  NSUBTP = NSUBTP + 1

                  IF ( NSUBTP .GT. 1 ) THEN
!
!                    We have too many sub-point specifications.
!
                     CALL SETMSG ( 'Extra sub-point type <#> was found ' &
                     //            'in the method string <#>.'         )
                     CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )      )
                     CALL ERRCH  ( '#', METHOD                         )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'            )
                     CALL CHKOUT ( 'ZZPRSMET'                          )
                     RETURN

                  END IF

                  SUBTYP = LOCMTH( BEGS(I):ENDS(I) )


               ELSE IF (     (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'TANGENT'                ) &
                        .OR. (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'GUIDED'                   )  ) THEN
!
!                 This is a point definition specification.
!
                  NLMBTP = NLMBTP + 1

                  IF ( NLMBTP .GT. 1 ) THEN
!
!                    We have too many point definition specifications.
!
                     CALL SETMSG ( 'Extra point definition <#> was ' &
                     //            'found in the method string <#>.'   )
                     CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )      )
                     CALL ERRCH  ( '#', METHOD                         )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'            )
                     CALL CHKOUT ( 'ZZPRSMET'                          )
                     RETURN

                  END IF

                  PNTDEF = LOCMTH( BEGS(I):ENDS(I) )



               ELSE IF (     (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'UMBRAL'                   ) &
                        .OR. (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'PENUMBRAL'                )  ) THEN
!
!                 This is a terminator type specification.
!
                  NTRMTP = NTRMTP + 1

                  IF ( NTRMTP .GT. 1 ) THEN
!
!                    We have too many terminator type specifications.
!
                     CALL SETMSG ( 'Extra terminator type <#> was ' &
                     //            'found in the method string <#>.'   )
                     CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )      )
                     CALL ERRCH  ( '#', METHOD                         )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'            )
                     CALL CHKOUT ( 'ZZPRSMET'                          )
                     RETURN

                  END IF

                  TRMTYP = LOCMTH( BEGS(I):ENDS(I) )



               ELSE IF (  LOCMTH( BEGS(I):ENDS(I) ) .EQ. SLASH  )  THEN
!
!                 Adjust the token index in the message to account
!                 for the insertion of a slash at the beginning.
!
                  CALL SETMSG ( 'An unexpected slash character was ' &
                  //            'found at index # in the method ' &
                  //            'string <#>.'                          )
                  CALL ERRINT ( '#', BEGS(I)-1                         )
                  CALL ERRCH  ( '#', METHOD                            )
                  CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'               )
                  CALL CHKOUT ( 'ZZPRSMET'                             )
                  RETURN


               ELSE IF ( LOCMTH( BEGS(I):ENDS(I) )  .NE.  'DSK'  ) THEN
!
!                 'DSK' was the only valid token that could appear
!                 at this point.
!
                  CALL SETMSG ( 'Unexpected token <#> was found ' &
                  //            'in the method string <#>.'       )
                  CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )    )
                  CALL ERRCH  ( '#', METHOD                       )
                  CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'          )
                  CALL CHKOUT ( 'ZZPRSMET'                        )
                  RETURN

               END IF


            END IF

            I = I + 1

         END DO

!
!        Currently, the UNPRIORITIZED keyword must be provided
!        for DSK shapes.
!
         IF ( PRI ) THEN

            CALL SETMSG ( 'The keyword UNPRIORITIZED must be ' &
            //            'present in the method string <#> ' &
            //            'when the target shape is DSK. ' &
            //            'Prioritized DSK segment searches ' &
            //            'are not currently supported.'      )
            CALL ERRCH  ( '#', METHOD                         )
            CALL SIGERR ( 'SPICE(BADPRIORITYSPEC)'            )
            CALL CHKOUT ( 'ZZPRSMET'                          )
            RETURN

         END IF


      ELSE IF ( SHAPE .EQ. 'ELLIPSOID' ) THEN
!
!        Check for legacy string inputs.
!
         IF ( EQSTR(METHOD, 'ELLIPSOID') ) THEN
!
!           There are no other outputs to set.
!
            CALL CHKOUT ( 'ZZPRSMET' )
            RETURN

         ELSE IF (      EQSTR(METHOD, 'NEARPOINT:ELLIPSOID') &
                   .OR. EQSTR(METHOD, 'ELLIPSOID:NEARPOINT') &
                   .OR. EQSTR(METHOD, 'NEARPOINT/ELLIPSOID') &
                   .OR. EQSTR(METHOD, 'ELLIPSOID/NEARPOINT')  ) THEN

            SUBTYP = 'NEAR POINT'

            CALL CHKOUT ( 'ZZPRSMET' )
            RETURN

         ELSE IF (      EQSTR(METHOD, 'INTERCEPT:ELLIPSOID') &
                   .OR. EQSTR(METHOD, 'ELLIPSOID:INTERCEPT') &
                   .OR. EQSTR(METHOD, 'INTERCEPT/ELLIPSOID') &
                   .OR. EQSTR(METHOD, 'ELLIPSOID/INTERCEPT')  ) THEN

            SUBTYP = 'INTERCEPT'

            CALL CHKOUT ( 'ZZPRSMET' )
            RETURN

         END IF

!
!        At this point, we should have a "modern" style of method
!        specification for an ellipsoidal shape model.
!        Parse the method string.
!
!        Traverse the tokens; identify clauses in the method string.
!
         I = 1

         DO WHILE ( I .LE. N )

            CHR = LOCMTH( BEGS(I):ENDS(I) )

            IF ( CHR .EQ. SLASH ) THEN
!
!              If the method string is correct, the next token should
!              be a keyword. There had better be a next token.
!
               IF ( I .EQ. N ) THEN

                  CALL SETMSG ( 'Expected more tokens after final <#> ' &
                     //         'in the method string <#>.'            )
                  CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )         )
                  CALL ERRCH  ( '#', METHOD                            )
                  CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'               )
                  CALL CHKOUT ( 'ZZPRSMET'                             )
                  RETURN

               END IF

               I = I + 1


               IF (  LOCMTH( BEGS(I):ENDS(I) ) .EQ.  'ELLIPSOID' ) THEN
!
!                 This case is a no-op.
!

               ELSE IF (     (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'INTERCEPT'                ) &
                        .OR. (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'NADIR'                    ) &
                        .OR. (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'NEAR'                     )  ) THEN
!
!                 This is a sub-point type specification.
!
                  NSUBTP = NSUBTP + 1

                  IF ( NSUBTP .GT. 1 ) THEN
!
!                    We have too many sub-point specifications.
!
                     CALL SETMSG ( 'Extra sub-point type <#> was found ' &
                     //            'in the method string <#>.'         )
                     CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )      )
                     CALL ERRCH  ( '#', METHOD                         )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'            )
                     CALL CHKOUT ( 'ZZPRSMET'                          )
                     RETURN

                  END IF

                  IF ( LOCMTH( BEGS(I):ENDS(I) ) .EQ. 'NEAR' ) THEN
!
!                    This may be a combination of old style and
!                    new syntax, e.g. 'ELLIPSOID/NEAR POINT'.
!
                     IF (       ( SHAPE .EQ. 'ELLIPSOID' ) &
                          .AND. ( I     .LT.  N          )  ) THEN
!
!                       We allow the "near point" sub-point type
!                       for ellipsoids but not for DSK surfaces.
!
                        IF (     LOCMTH( BEGS(I+1) : ENDS(I+1) ) &
                            .EQ. 'POINT'                          ) THEN

                           SUBTYP = 'NADIR'

                        END IF

                     END IF

                  ELSE
                     SUBTYP = LOCMTH( BEGS(I):ENDS(I) )
                  END IF





               ELSE IF (     (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'TANGENT'                ) &
                        .OR. (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'GUIDED'                   )  ) THEN
!
!                 This is a point definition specification.
!

                  NLMBTP = NLMBTP + 1

                  IF ( NLMBTP .GT. 1 ) THEN
!
!                    We have too many point definition specifications.
!
                     CALL SETMSG ( 'Extra point definition <#> was ' &
                     //            'found in the method string <#>.'   )
                     CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )      )
                     CALL ERRCH  ( '#', METHOD                         )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'            )
                     CALL CHKOUT ( 'ZZPRSMET'                          )
                     RETURN

                  END IF

                  PNTDEF = LOCMTH( BEGS(I):ENDS(I) )

               ELSE IF (     (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'UMBRAL'                   ) &
                        .OR. (       LOCMTH( BEGS(I):ENDS(I) ) &
                               .EQ. 'PENUMBRAL'                )  ) THEN
!
!                 This is a terminator type specification.
!
                  NTRMTP = NTRMTP + 1

                  IF ( NTRMTP .GT. 1 ) THEN
!
!                    We have too many terminator type specifications.
!
                     CALL SETMSG ( 'Extra terminator type <#> was ' &
                     //            'found in the method string <#>.'   )
                     CALL ERRCH  ( '#', LOCMTH( BEGS(I):ENDS(I) )      )
                     CALL ERRCH  ( '#', METHOD                         )
                     CALL SIGERR ( 'SPICE(BADMETHODSYNTAX)'            )
                     CALL CHKOUT ( 'ZZPRSMET'                          )
                     RETURN

                  END IF

                  TRMTYP = LOCMTH( BEGS(I):ENDS(I) )

               ELSE
!
!                 Sorry, no other strings are allowed.
!
                  CALL SETMSG( 'Unexpected method string <#> was found ' &
                  //           'specifying an ellipsoid shape.'        )
                  CALL ERRCH ( '#', METHOD                             )
                  CALL SIGERR( 'SPICE(BADMETHODSYNTAX)'                )
                  CALL CHKOUT( 'ZZPRSMET'                              )
                  RETURN

               END IF

            ELSE

               CALL SETMSG( 'Unexpected method string <#> was found ' &
               //           'specifying an ellipsoid shape.'        )
               CALL ERRCH ( '#', METHOD                             )
               CALL SIGERR( 'SPICE(BADMETHODSYNTAX)'                )
               CALL CHKOUT( 'ZZPRSMET'                              )
               RETURN

            END IF



            I = I + 1

         END DO

      ELSE
!
!        This is a backstop error check.
!
         CALL SETMSG ( 'Unexpected shape value # was found. ' &
         //            'This is due to a coding error, not ' &
         //            'to user input.'                      )
         CALL ERRCH  ( '#', SHAPE                            )
         CALL SIGERR ( 'SPICE(BUG)'                          )
         CALL CHKOUT ( 'ZZPRSMET'                            )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZPRSMET' )
      RETURN
      END
