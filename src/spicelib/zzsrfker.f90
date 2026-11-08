!$Procedure ZZSRFKER ( Surface translation, process kernel update )

      SUBROUTINE ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, &
                            EXTKER, NKVAR,  SNMHLS, SNMPOL, &
                            SNMIDX, SIDHLS, SIDPOL, SIDIDX )

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

      IMPLICIT NONE

      INCLUDE              'srftrn.inc'
      INCLUDE              'zzsrftrn.inc'

      CHARACTER*(SFNMLN)    KERNAM (          * )
      CHARACTER*(SFNMLN)    NORNAM (          * )
      INTEGER               KERSID (          * )
      INTEGER               KERBID (          * )
      LOGICAL               EXTKER
      INTEGER               NKVAR
      INTEGER               SNMHLS (          * )
      INTEGER               SNMPOL ( LBSNGL : * )
      INTEGER               SNMIDX (          * )
      INTEGER               SIDHLS (          * )
      INTEGER               SIDPOL ( LBSNGL : * )
      INTEGER               SIDIDX (          * )

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
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(*)         KVSFBD
      PARAMETER           ( KVSFBD = 'NAIF_SURFACE_BODY' )

      CHARACTER*(*)         KVSFCD
      PARAMETER           ( KVSFCD = 'NAIF_SURFACE_CODE' )

      CHARACTER*(*)         KVSFNM
      PARAMETER           ( KVSFNM = 'NAIF_SURFACE_NAME' )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               NNAMES
      PARAMETER           ( NNAMES = 3 )

!
!     Local variables
!
      CHARACTER*(1)         BDTYPE
      CHARACTER*(1)         CDTYPE
      CHARACTER*(1)         NDTYPE
      CHARACTER*(KVNMLN)    NAMES  ( NNAMES )

      INTEGER               I
      INTEGER               NBODY
      INTEGER               NCODE
      INTEGER               NNAME

      LOGICAL               PASS1
      LOGICAL               FNDBOD
      LOGICAL               FNDCDE
      LOGICAL               FNDNAM


!
!     Saved variables
!
      SAVE                  PASS1

!
!     Initial values
!
      DATA                  PASS1 / .TRUE. /
      DATA                  NAMES / KVSFBD, KVSFCD, KVSFNM /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSRFKER' )

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
      IF ( PASS1 ) THEN
!
!        Set watch on kernel variables used for the surface mapping.
!
         CALL SWPOOL ( 'ZZSRFTRN', NNAMES, NAMES )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSRFKER' )
            RETURN
         END IF

         PASS1 = .FALSE.

      END IF

!
!     Indicate that no data are available until we find out
!     otherwise.
!
      EXTKER = .FALSE.
      NKVAR  =  0

!
!     Fetch attributes of the surface mapping kernel variables.
!
      CALL DTPOOL ( KVSFNM, FNDNAM, NNAME, NDTYPE )
      CALL DTPOOL ( KVSFCD, FNDCDE, NCODE, CDTYPE )
      CALL DTPOOL ( KVSFBD, FNDBOD, NBODY, BDTYPE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSRFKER' )
         RETURN
      END IF

!
!     The variables must all be present or all be absent.
!
      IF ( ( FNDCDE .NEQV. FNDNAM ) .OR. ( FNDBOD .NEQV. FNDNAM ) ) THEN

         CALL SETMSG ( 'Surface mapping kernel variables are in an ' &
         //            'inconsistent state. # was #; # was #; # ' &
         //            'was #.'                                      )

         CALL ERRCH  ( '#', KVSFNM )

         IF ( FNDNAM ) THEN
            CALL ERRCH ( '#', 'found' )
         ELSE
            CALL ERRCH ( '#', 'not found' )
         END IF


         CALL ERRCH  ( '#', KVSFCD )

         IF ( FNDCDE ) THEN
            CALL ERRCH ( '#', 'found' )
         ELSE
            CALL ERRCH ( '#', 'not found' )
         END IF


         CALL ERRCH  ( '#', KVSFBD )

         IF ( FNDBOD ) THEN
            CALL ERRCH ( '#', 'found' )
         ELSE
            CALL ERRCH ( '#', 'not found' )
         END IF

         CALL SIGERR ( 'SPICE(BADSURFACEMAP)' )
         CALL CHKOUT ( 'ZZSRFKER'             )
         RETURN

      END IF

!
!     If the variables are not present, leave now.
!
      EXTKER = FNDNAM .AND. FNDCDE .AND. FNDBOD

      IF ( .NOT. EXTKER ) THEN
         CALL CHKOUT ( 'ZZSRFKER' )
         RETURN
      END IF

!
!     Make sure the kernel variables aren't larger than our arrays.
!     Also make sure the variable have matching dimensions.
!
!     Check variable types.
!
      IF (     ( NDTYPE .NE. 'C' ) &
          .OR. ( CDTYPE .NE. 'N' ) &
          .OR. ( BDTYPE .NE. 'N' ) ) THEN

         CALL SETMSG ( 'Surface mapping kernel variable types ' &
         //            'are: # = #; # = #; # = #. These types ' &
         //            'must be, respectively, ''C'', ''N'', ' &
         //            '''N''.'                                )
         CALL ERRCH  ( '#', KVSFNM                             )
         CALL ERRCH  ( '#', NDTYPE                             )
         CALL ERRCH  ( '#', KVSFCD                             )
         CALL ERRCH  ( '#', CDTYPE                             )
         CALL ERRCH  ( '#', KVSFBD                             )
         CALL ERRCH  ( '#', BDTYPE                             )
         CALL SIGERR ( 'SPICE(BADVARIABLETYPE)'                )
         CALL CHKOUT ( 'ZZSRFKER'                              )
         RETURN

      END IF

!
!     Check variable dimensions.
!
      IF (     ( NNAME .GT. MXNSRF ) &
          .OR. ( NCODE .GT. MXNSRF ) &
          .OR. ( NBODY .GT. MXNSRF ) ) THEN

         CALL SETMSG ( 'Surface mapping kernel variable sizes ' &
         //            'are: # = #; # = #; # = #. Maximum ' &
         //            'allowed size is #.'                    )
         CALL ERRCH  ( '#', KVSFNM                             )
         CALL ERRINT ( '#', NNAME                              )
         CALL ERRCH  ( '#', KVSFCD                             )
         CALL ERRINT ( '#', NCODE                              )
         CALL ERRCH  ( '#', KVSFBD                             )
         CALL ERRINT ( '#', NBODY                              )
         CALL ERRINT ( '#', MXNSRF                             )
         CALL SIGERR ( 'SPICE(TOOMANYSURFACES)'                )
         CALL CHKOUT ( 'ZZSRFKER'                              )
         RETURN

      END IF

      IF (  ( NCODE .NE. NNAME ) .OR. ( NBODY .NE. NNAME )  ) THEN

         CALL SETMSG ( 'Surface variable sizes do not match. Size ' &
         //            'of # is #; size of # is #; size of # is #.'  )
         CALL ERRCH  ( '#', KVSFNM                                   )
         CALL ERRINT ( '#', NNAME                                    )
         CALL ERRCH  ( '#', KVSFCD                                   )
         CALL ERRINT ( '#', NCODE                                    )
         CALL ERRCH  ( '#', KVSFBD                                   )
         CALL ERRINT ( '#', NBODY                                    )
         CALL SIGERR ( 'SPICE(ARRAYSIZEMISMATCH)'                    )
         CALL CHKOUT ( 'ZZSRFKER'                                    )
         RETURN

      END IF

!
!     Fetch mapping variables.
!
!     Note that we'll check the variable sizes below.
!
      CALL GCPOOL ( KVSFNM, 1, MXNSRF, NNAME, KERNAM, FNDNAM )
      CALL GIPOOL ( KVSFCD, 1, MXNSRF, NCODE, KERSID, FNDCDE )
      CALL GIPOOL ( KVSFBD, 1, MXNSRF, NBODY, KERBID, FNDBOD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSRFKER' )
         RETURN
      END IF


      NKVAR = NNAME

!
!     Produce normalized name array. Check for blank names
!     as we go.
!
      DO I = 1, NKVAR

         IF ( KERNAM(I) .EQ. ' ' ) THEN

            CALL SETMSG ( 'An attempt to assign the code, #, to ' &
            //            'a blank string was made.  Check loaded ' &
            //            'text kernels for a blank string in ' &
            //            'the NAIF_SURFACE_NAME array.'            )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(BLANKNAMEASSIGNED)'                )
            CALL CHKOUT ( 'ZZSRFKER'                                )
            RETURN

         END IF

         CALL LJUCRS ( 1, KERNAM(I), NORNAM(I) )

      END DO

!
!     Initialize hash data structures.
!
      CALL ZZSRFINI ( NORNAM, KERSID, KERBID, NKVAR, &
                      NROOM,  SNMHLS, SNMPOL, SNMIDX, &
                      SIDHLS, SIDPOL, SIDIDX         )

      CALL CHKOUT ( 'ZZSRFKER' )
      RETURN
      END
