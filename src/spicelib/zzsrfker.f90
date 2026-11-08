!$Procedure ZZSRFKER ( Surface translation, process kernel update )

      subroutine zzsrfker ( kernam, nornam, kersid, kerbid, &
                            extker, nkvar,  snmhls, snmpol, &
                            snmidx, sidhls, sidpol, sididx )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Update ZZSRFTRN's name-based and ID-based data structure arrays
!     using the contents of kernel variables that define the surface
!     name/ID mapping.
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

      include              'srftrn.inc'
      include              'zzsrftrn.inc'

      character(sfnmln)     kernam (          * )
      character(sfnmln)     nornam (          * )
      integer(ip)           kersid (          * )
      integer(ip)           kerbid (          * )
      logical               extker
      integer(ip)           nkvar
      integer(ip)           snmhls (          * )
      integer(ip)           snmpol ( lbsngl : * )
      integer(ip)           snmidx (          * )
      integer(ip)           sidhls (          * )
      integer(ip)           sidpol ( lbsngl : * )
      integer(ip)           sididx (          * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     KERNAM     O   Array of surface names from kernel pool.
!     NORNAM     O   Array of normalized surface names.
!     KERSID     O   Array of surface ID codes from kernel pool.
!     KERBID     O   Array of body ID codes from kernel pool.
!     EXTKER     O   Logical flag indicating kernel data are present.
!     NKVAR      O   Number of surface name, code, body tuples.
!     SNMHLS     O   Surface name-based hash head node pointer list
!     SNMPOL     O   Surface name-based hash node collision list
!     SNMIDX     O   Surface name-based hash index storage array
!     SIDHLS     O   Surface ID-based hash head node pointer list
!     SIDPOL     O   Surface ID-based hash node collision list
!     SIDIDX     O   Surface ID-based hash index storage array
!     LBSNGL     P   Lower bound of hash pool arrays
!     SFNMLN     P   Maximum length of surface name strings
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     KERNAM    is an array containing surface names from kernel pool
!               assignments to the kernel variable NAIF_SURFACE_NAME.
!
!               Array elements from masked assignments are not included
!               in KERNAM.
!
!     NORNAM    is an array parallel to KERNAM containing normalized
!               names. The Ith element of NORNAM is obtained from the
!               Ith element of KERNAM by conversion to uppercase,
!               left-justification, and compression of consecutive
!               embedded blanks to a single blank.
!
!     KERSID    is an array containing surface names from kernel pool
!               assignments to the kernel variable NAIF_SURFACE_CODE.
!               The Ith element of KERSID is the code associated with
!               the Ith element of KERNAM.
!
!     KERBID    is an array containing surface names from kernel pool
!               assignments to the kernel variable NAIF_SURFACE_BODY.
!               The Ith element of KERBID is the code associated with
!               the Ith element of KERNAM.
!
!     EXTKER    is a logical flag indicating whether kernel data
!               defining a surface name/ID mapping are available.
!               EXTKER is set to .TRUE. if the data are present and is
!               .FALSE. otherwise.
!
!     NKVAR     is the count of names in the array KERNAM; the
!               arrays NORNAM, KERSID, and KERBID also contain NKVAR
!               entries.
!
!     SNMHLS
!     SNMPOL    are the surface name-based hash head node pointer and
!               collision lists. Together with the arrays SNMIDX,
!               NORNAM, KERBID, and KERSID, they enable mapping pairs
!               of normalized surface names and body ID codes to
!               surface ID codes.
!
!     SNMIDX    is the surface name-based hash index storage array. It
!               maps nodes in the name collision list to entries in the
!               parallel NORNAM, KERBID, and KERSID arrays.
!
!     SIDHLS
!     SIDPOL    are the surface ID-based hash head node pointer and
!               collision lists. Together with the arrays SIDIDX,
!               KERSID, KERBID, and KERNAM, they enable mapping pairs of
!               surface ID codes and body ID codes to surface names.
!
!     SIDIDX    is the surface ID-based hash index storage array. It
!               maps nodes in the ID collision list to entries in the
!               parallel KERSID, KERBID, and KERNAM arrays.
!
! $ Parameters
!
!     LBSNGL    is the lower bound of the hashes' collision list array.
!
!     SFNMLN    is the maximum length of a surface name. Defined in the
!               include file 'srftrn.inc'.
!
! $ Exceptions
!
!     1)  If an error occurs while fetching kernel variables, the error
!         will be signaled by a routine in the call tree of this
!         routine.
!
!     2)  All three of the kernel variables defining the surface name/ID
!         map must be present in the kernel pool, or all must be absent.
!         If this condition is not met, the error SPICE(BADSURFACEMAP)
!         will be signaled.
!
!     3)  All three of the kernel variables defining the surface name/ID
!         map must have the correct data type; if not, the error
!         SPICE(BADVARIABLETYPE) will be signaled.
!
!     4)  If any of the kernel variables defining the surface name/ID
!         map have size exceeding MXNSRF, the error
!         SPICE(TOOMANYSURFACES) will be signaled.
!
!     5)  All three of the kernel variables defining the surface name/ID
!         map must have the same number of elements; if not, the error
!         SPICE(ARRAYSIZEMISMATCH) will be signaled.
!
!     6)  If any surface name is blank, the error
!         SPICE(BLANKNAMEASSIGNED) will be signaled.
!
!     7)  Any error occurring during initialization of the hash data
!         structures will be signaled by a routine in the call tree
!         of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine manages updates of the surface name/ID mapping
!     subsystem data structures. The outputs of this routine contain
!     mapping information provided by kernel variables, as well as
!     associated bookkeeping information.
!
!     On the first pass through this routine, the routine sets a watch
!     on the kernel variables that define the surface name/ID mapping.
!     This routine must be called after any change to these variables.
!
! $ Examples
!
!     See the routine ZZSRFTRN.
!
! $ Restrictions
!
!     1)  This routine is intended only for use by ZZSRFTRN and
!         ZZSRFKER.
!
!     2)  All output hash arrays must be declared with the same
!         dimension which is greater than or equal to MAXVAL.
!
!     3)  The order of mappings in the input arrays determines the
!         priority, with the mapping with the lowest priority being the
!         first and the mapping with the highest priority being the
!         last.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 04-DEC-2015 (NJB) (BVS) (EDW)
!
!-&



!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local parameters
!
      character(*)          kvsfbd
      parameter           ( kvsfbd = 'NAIF_SURFACE_BODY' )

      character(*)          kvsfcd
      parameter           ( kvsfcd = 'NAIF_SURFACE_CODE' )

      character(*)          kvsfnm
      parameter           ( kvsfnm = 'NAIF_SURFACE_NAME' )

      integer(ip)           kvnmln
      parameter           ( kvnmln = 32 )

      integer(ip)           nnames
      parameter           ( nnames = 3 )

!
!     Local variables
!
      character(1)          bdtype
      character(1)          cdtype
      character(1)          ndtype
      character(kvnmln)     names  ( nnames )

      integer(ip)           i
      integer(ip)           nbody
      integer(ip)           ncode
      integer(ip)           nname

      logical               pass1
      logical               fndbod
      logical               fndcde
      logical               fndnam


!
!     Saved variables
!
      save                  pass1

!
!     Initial values
!
      data                  pass1 / .true. /
      data                  names / kvsfbd, kvsfcd, kvsfnm /


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZSRFKER' )

!
!     The primary functions performed inline in this routine are
!
!        - Setting a watch on the mapping kernel variables
!
!        - Fetching the mapping kernel variables' values
!
!        - Performing error checks on the kernel variables that define
!          the surface name/ID mapping. Initialization of data
!          structures is delegated to ZZSRFINI.
!
!
      if ( pass1 ) then
!
!        Set watch on kernel variables used for the surface mapping.
!
         call swpool ( 'ZZSRFTRN', nnames, names )

         if ( failed() ) then
            call chkout ( 'ZZSRFKER' )
            return
         end if

         pass1 = .false.

      end if

!
!     Indicate that no data are available until we find out
!     otherwise.
!
      extker = .false.
      nkvar  =  0

!
!     Fetch attributes of the surface mapping kernel variables.
!
      call dtpool ( kvsfnm, fndnam, nname, ndtype )
      call dtpool ( kvsfcd, fndcde, ncode, cdtype )
      call dtpool ( kvsfbd, fndbod, nbody, bdtype )

      if ( failed() ) then
         call chkout ( 'ZZSRFKER' )
         return
      end if

!
!     The variables must all be present or all be absent.
!
      if ( ( fndcde .neqv. fndnam ) .or. ( fndbod .neqv. fndnam ) ) then

         call setmsg ( 'Surface mapping kernel variables are in an ' &
         //            'inconsistent state. # was #; # was #; # ' &
         //            'was #.'                                      )

         call errch  ( '#', kvsfnm )

         if ( fndnam ) then
            call errch ( '#', 'found' )
         else
            call errch ( '#', 'not found' )
         end if


         call errch  ( '#', kvsfcd )

         if ( fndcde ) then
            call errch ( '#', 'found' )
         else
            call errch ( '#', 'not found' )
         end if


         call errch  ( '#', kvsfbd )

         if ( fndbod ) then
            call errch ( '#', 'found' )
         else
            call errch ( '#', 'not found' )
         end if

         call sigerr ( 'SPICE(BADSURFACEMAP)' )
         call chkout ( 'ZZSRFKER'             )
         return

      end if

!
!     If the variables are not present, leave now.
!
      extker = fndnam .and. fndcde .and. fndbod

      if ( .not. extker ) then
         call chkout ( 'ZZSRFKER' )
         return
      end if

!
!     Make sure the kernel variables aren't larger than our arrays.
!     Also make sure the variable have matching dimensions.
!
!     Check variable types.
!
      if (     ( ndtype /= 'C' ) &
          .or. ( cdtype /= 'N' ) &
          .or. ( bdtype /= 'N' ) ) then

         call setmsg ( 'Surface mapping kernel variable types ' &
         //            'are: # = #; # = #; # = #. These types ' &
         //            'must be, respectively, ''C'', ''N'', ' &
         //            '''N''.'                                )
         call errch  ( '#', kvsfnm                             )
         call errch  ( '#', ndtype                             )
         call errch  ( '#', kvsfcd                             )
         call errch  ( '#', cdtype                             )
         call errch  ( '#', kvsfbd                             )
         call errch  ( '#', bdtype                             )
         call sigerr ( 'SPICE(BADVARIABLETYPE)'                )
         call chkout ( 'ZZSRFKER'                              )
         return

      end if

!
!     Check variable dimensions.
!
      if (     ( nname > mxnsrf ) &
          .or. ( ncode > mxnsrf ) &
          .or. ( nbody > mxnsrf ) ) then

         call setmsg ( 'Surface mapping kernel variable sizes ' &
         //            'are: # = #; # = #; # = #. Maximum ' &
         //            'allowed size is #.'                    )
         call errch  ( '#', kvsfnm                             )
         call errint ( '#', nname                              )
         call errch  ( '#', kvsfcd                             )
         call errint ( '#', ncode                              )
         call errch  ( '#', kvsfbd                             )
         call errint ( '#', nbody                              )
         call errint ( '#', mxnsrf                             )
         call sigerr ( 'SPICE(TOOMANYSURFACES)'                )
         call chkout ( 'ZZSRFKER'                              )
         return

      end if

      if (  ( ncode /= nname ) .or. ( nbody /= nname )  ) then

         call setmsg ( 'Surface variable sizes do not match. Size ' &
         //            'of # is #; size of # is #; size of # is #.'  )
         call errch  ( '#', kvsfnm                                   )
         call errint ( '#', nname                                    )
         call errch  ( '#', kvsfcd                                   )
         call errint ( '#', ncode                                    )
         call errch  ( '#', kvsfbd                                   )
         call errint ( '#', nbody                                    )
         call sigerr ( 'SPICE(ARRAYSIZEMISMATCH)'                    )
         call chkout ( 'ZZSRFKER'                                    )
         return

      end if

!
!     Fetch mapping variables.
!
!     Note that we'll check the variable sizes below.
!
      call gcpool ( kvsfnm, 1, mxnsrf, nname, kernam, fndnam )
      call gipool ( kvsfcd, 1, mxnsrf, ncode, kersid, fndcde )
      call gipool ( kvsfbd, 1, mxnsrf, nbody, kerbid, fndbod )

      if ( failed() ) then
         call chkout ( 'ZZSRFKER' )
         return
      end if


      nkvar = nname

!
!     Produce normalized name array. Check for blank names
!     as we go.
!
      do i = 1, nkvar

         if ( kernam(i) == ' ' ) then

            call setmsg ( 'An attempt to assign the code, #, to ' &
            //            'a blank string was made.  Check loaded ' &
            //            'text kernels for a blank string in ' &
            //            'the NAIF_SURFACE_NAME array.'            )
            call errint ( '#', i                                    )
            call sigerr ( 'SPICE(BLANKNAMEASSIGNED)'                )
            call chkout ( 'ZZSRFKER'                                )
            return

         end if

         call ljucrs ( 1, kernam(i), nornam(i) )

      end do

!
!     Initialize hash data structures.
!
      call zzsrfini ( nornam, kersid, kerbid, nkvar, &
                      nroom,  snmhls, snmpol, snmidx, &
                      sidhls, sidpol, sididx         )

      call chkout ( 'ZZSRFKER' )
      return
      end
