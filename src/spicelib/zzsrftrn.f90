!$Procedure ZZSRFTRN ( Surface name/ID mapping umbrella )

      SUBROUTINE ZZSRFTRN ( BODYID, SRFNAM, SURFID, &
                            USRCTR, FOUND,  UPDATE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Umbrella routine for surface name/ID mapping entry points
!     and the data structures they use.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     CONVERSION
!     DSK
!     ID
!     NAME
!     STRING
!     SURFACE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'srftrn.inc'
      INCLUDE 'zzctr.inc'
      INCLUDE 'zzsrftrn.inc'

      INTEGER               BODYID
      CHARACTER*(*)         SRFNAM
      INTEGER               SURFID
      INTEGER               USRCTR ( 2 )
      LOGICAL               FOUND
      LOGICAL               UPDATE

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     BODYID     I   ZZSRFC2N, ZZSRFN2C
!     SRFNAM    I-O  ZZSRFC2N, ZZSRFN2C
!     SURFID    I-O  ZZSRFC2N, ZZSRFN2C
!     USRCTR    I-O  ZZSRFTRK
!     FOUND      O   ZZSRFC2N, ZZSRFN2C
!     UPDATE     O   ZZSRFTRK
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for descriptions of their outputs.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If this routine is called directly, the error
!        SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     Surface-to-name mappings may be defined at run time by loading
!     text kernels containing kernel variable assignments of the form
!
!        NAIF_SURFACE_NAME += ( <surface name 1>, ... )
!        NAIF_SURFACE_CODE += ( <surface code 1>, ... )
!        NAIF_SURFACE_BODY += ( <body code 1>,    ... )
!
!     Here the set of the three Ith list items on the right hand side
!     of the three assignments define the Ith surface name/ID mapping.
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
! $ Particulars
!
!     This umbrella routine contains declarations of data structures
!     that support surface name/ID mapping.
!
!     This umbrella routine contains the following entry points:
!
!        ZZSRFC2N   {Surface code to name}
!        ZZSRFN2C   {Surface name to code}
!        ZZSRFTRK   {Track surface map updates}
!
!
! $ Examples
!
!     See the routines
!
!        SRFC2S
!        SRFCSS
!        SRFS2C
!        SRFSCC
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
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 05-NOV-2021 (NJB)
!
!        Corrected I/O descriptions in header of entry point ZZSRFC2N.
!
!-    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS)
!
!-&

! $ Index_Entries
!
!     surface name id mapping umbrella
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ZZHASH2
      INTEGER               ZZHASHI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!
!     Hash control area items.
!
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX =   0 )

      INTEGER               FREIDX
      PARAMETER           ( FREIDX =  -1 )

!
!     Local variables
!

!
!     Data structures in this package
!     ===============================
!
!     The kernel variable table
!     -------------------------
!
!     This table contains file scope arrays populated with the values
!     of the kernel variables that define the surface name/ID mapping.
!     These arrays contain
!
!        - surface names
!        - surface ID codes
!        - bodies associated with surface name/ID pairs
!        - an array of normalized names. These names are
!          upper case, left-justified, and compressed so that
!          the names contain no consecutive, embedded blanks
!
!
!     The surface ID table
!     --------------------
!
!     This table enables pairs of surface IDs and body IDs to be mapped
!     to surface names. The table consists of
!
!        - a singly linked list pool
!        - a list head array
!        - a pointer array that maps pool nodes to
!          indices in the kernel variable table
!
!     The pointer array maps each node belonging to a collision list in
!     the pool to the index in the kernel table of the associated
!     values. The kernel table values used by this mapping are
!
!        - the  surface ID code
!        - the body ID code
!        - the original surface name
!
!     An integer hash function is used to map each surface ID to the
!     index in the list head array where the index of the head node for
!     that surface ID is located.
!
!     The layout of the structure is:
!
!                                                 Kernel variable table
!                                                 (only the portions
!                                                 used here are shown)
!
!                                                         body IDs
!                                                            |
!   +-- integer_hash( surface ID )                           |  original
!   |                                            surface IDs |  surface
!   |                                                   |    |   names
!   |                                                   |    |    |
!   |  list heads     list pool   pointer array         |    |    |
!   |  +---------+    +--------+  +-----------+        +--+ +--+ +--+
!   |  |         |    |        |  |           |   +--->|  | |  | |  |
!   |  +---------+    +--------+  +-----------+   |    +--+ +--+ +--+
!   +->|         |-+  | ^   *  |->|           |---+    |  | |  | |  |
!      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+
!                  |    |   |
!          ...     |    |...|          ...                   ...
!                  |    |   |
!      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+
!      |         | +->| *   |  |->|           |---+ +->|  | |  | |  |
!      +---------+    +-----|--+  +-----------+   | |  +--+ +--+ +--+
!          ...           ...|          ...      +-|-+        ...
!      +---------+    +-----|--+  +-----------+ | |    +--+ +--+ +--+
!      |         |    |     v  |->|           |-+ +--->|  | |  | |  |
!      +---------+    +--------+  +-----------+        +--+ +--+ +--+
!
!      ----------------------------------------        --------------
!                       NROOM                              MXNSRF
!
!
!      The diagram above is not to scale: the arrays on the left have
!      available length NROOM, while the arrays on the right have
!      length MXNSRF.
!
!      Note that the pool array is dimensioned (LBSNGL:NROOM). Elements
!      at indices -1 and 0 contain the size and location of the
!      first free element, respectively.
!
!
!     The surface name table
!     ----------------------
!
!     This table enables pairs of surface names and body IDs to be
!     mapped to surface IDs. The structure is parallel to that of
!     the surface ID table; it contains
!
!        - a singly linked list pool
!        - a list head array
!        - a pointer array that maps pool nodes to
!          indices in the kernel variable table
!
!     The pointer array maps each node belonging to a collision list in
!     the pool to the index in the kernel table of the associated
!     values. The kernel table values used by this mapping are
!
!        - the normalized surface name
!        - the surface ID code
!        - the body ID code
!
!     An string hash function is used to map each surface name to the
!     index in the list head array where the index of the head node for
!     that surface ID is located.
!
!     The hash function is applied to the input string after it has
!     been normalized and then had all embedded blanks compressed out.
!     This allows the hash function terminate when it encounters the
!     first blank in the input string, while taking into account all
!     non-blank characters in the string. This makes it efficient while
!     enabling it to discriminate well between strings that may have
!     initial words in common. These compressed strings are not used
!     for any other purpose than hashing. For detection of the correct
!     matching elements in the kernel table, the normalized version
!     of the input string (which may contain blanks) is used.
!
!     The layout of the structure is:
!
!
!                                                 Kernel variable table
!                                                 (only the portions
!                                                 used here are shown)
!
!                                                         body IDs
!                                                            |
!   +-- string_hash(surface name)                            |
!   |                                         normalized     |  surface
!   |                                         surface names  |   IDs
!   |                                                   |    |    |
!   |                                                   |    |    |
!   |  list heads     list pool   pointer array         |    |    |
!   |  +---------+    +--------+  +-----------+        +--+ +--+ +--+
!   |  |         |    |        |  |           |   +--->|  | |  | |  |
!   |  +---------+    +--------+  +-----------+   |    +--+ +--+ +--+
!   +->|         |-+  | ^   *  |->|           |---+    |  | |  | |  |
!      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+
!                  |    |   |
!          ...     |    |...|          ...                   ...
!                  |    |   |
!      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+
!      |         | +->| *   |  |->|           |---+ +->|  | |  | |  |
!      +---------+    +-----|--+  +-----------+   | |  +--+ +--+ +--+
!          ...           ...|          ...      +-|-+        ...
!      +---------+    +-----|--+  +-----------+ | |    +--+ +--+ +--+
!      |         |    |     v  |->|           |-+ +--->|  | |  | |  |
!      +---------+    +--------+  +-----------+        +--+ +--+ +--+
!
!
!      ----------------------------------------        --------------
!                       NROOM                              MXNSRF
!
!
!      The diagram above is not to scale: the arrays on the left have
!      available length NROOM, while the arrays on the right have
!      length MXNSRF.
!
!      Note that the pool array is dimensioned (LBSNGL:NROOM). Elements
!      at indices -1 and 0 contain the size and location of the
!      first free element, respectively.
!
!
!
!
!     Declarations of data structures
!     ===============================
!
!        Kernel variable table
!        =====================
!
!        Input names:                 KERNAM
!        Input surface IDs:           KERSID
!        Input body IDs:              KERBID
!
      INTEGER               KERSID ( MXNSRF )
      INTEGER               KERBID ( MXNSRF )
      CHARACTER*(SFNMLN)    KERNAM ( MXNSRF )
      INTEGER               NKVAR
!
!        Normalized names:            NRMNAM
!
!     Each of these surface names is prefixed with an 11-character
!     string containing the associated body ID.
!

      CHARACTER*(SFNMLN)    NORNAM ( MXNSRF )

!
!
!        Surface ID table
!        ================
!
!        Surface ID list heads:       SIDHLS
!        Surface ID pool:             SIDPOL
!        Surface ID name pointers:    SIDIDX
!
!
      INTEGER               SIDHLS ( NROOM )
      INTEGER               SIDPOL ( LBSNGL : NROOM )
      INTEGER               SIDIDX ( NROOM )


!        Surface Name table
!        ==================
!
!        Surface name list heads:     SNMHLS
!        Surface name pool:           SNMPOL
!        Surface name ID pointers:    SNMIDP
!
!
      INTEGER               SNMHLS ( NROOM )
      INTEGER               SNMPOL ( LBSNGL : NROOM )
      INTEGER               SNMIDX ( NROOM )


!
!     Other local declarations:
!
      CHARACTER*(SFNMLN)    NSRFNM
      CHARACTER*(SFNMLN)    SQSHNM

      INTEGER               ITEMAT
      INTEGER               LOOKAT
      INTEGER               NODE

!
!     POLCTR tracks the state of the kernel pool.
!     SRFCTR tracks the state of the surface mapping
!     kernel variables.
!
      INTEGER               POLCTR ( CTRSIZ )
      INTEGER               SRFCTR ( CTRSIZ )

      LOGICAL               EXTKER
      LOGICAL               PASS1
      LOGICAL               LUPDTE

!
!
!     Saved variables
!
      SAVE


!
!     Initial values
!
      DATA                  EXTKER / .FALSE.    /
      DATA                  POLCTR / CTRSIZ * 0 /
      DATA                  SRFCTR / CTRSIZ * 0 /
      DATA                  PASS1  / .TRUE.     /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZSRFTRN'                                    )
      CALL SETMSG ( 'ZZSRFTRN is an umbrella routine. It should ' &
      //            'never be called directly.'                   )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)'                           )
      CALL CHKOUT ( 'ZZSRFTRN'                                    )
      RETURN




!$Procedure ZZSRFN2C ( Surface name to ID code mapping )

      ENTRY ZZSRFN2C ( SRFNAM, BODYID, SURFID, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Map a surface name and body ID code to a surface ID code.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     CONVERSION
!     DSK
!     ID
!     NAME
!     STRING
!     SURFACE
!
! $ Declarations
!
!     CHARACTER*(*)         SRFNAM
!     INTEGER               BODYID
!     INTEGER               SURFID
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     SRFNAM     I   Surface name.
!     BODYID     I   Body ID code.
!     SURFID     O   Surface ID code.
!     FOUND      O   Found flag.
!
! $ Detailed_Input
!
!     SRFNAM     is the name of surface to be translated.
!
!     BODYID     is the ID code of a body with which the surface
!                designated by SRFNAM is associated.
!
! $ Detailed_Output
!
!     SURFID     is the surface ID code associated with the input
!                surface name and body ID code.
!
!                If multiple assignments for the input surface
!                name and body ID are present in the kernel pool,
!                the latest one is used to determine SURFID.
!
!     FOUND      is a logical flag that is .TRUE. if the inputs
!                were mapped to a surface ID code and .FALSE.
!                otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs when the data structures of this package
!         are initialized, it will be signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     Surface-to-name mappings may be defined at run time by loading
!     text kernels containing kernel variable assignments of the form
!
!        NAIF_SURFACE_NAME += ( <surface name 1>, ... )
!        NAIF_SURFACE_CODE += ( <surface code 1>, ... )
!        NAIF_SURFACE_BODY += ( <body code 1>,    ... )
!
!     Here the set of the three Ith list items on the right hand side
!     of the three assignments define the Ith surface name/ID mapping.
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
! $ Particulars
!
!     This routine maps pairs of surface names and body ID codes to
!     surface ID codes. It relies on the mapping variables in the
!     kernel pool.
!
!     On the first pass through this routine, this routine
!     initializes the shared data structures of this package,
!     if the initialization has not already been done.
!
! $ Examples
!
!     See the routines
!
!        SRFS2C
!        SRFSCC
!
! $ Restrictions
!
!     This routine must not be called by user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS)
!
!-&

! $ Index_Entries
!
!     map surface name and body id to surface id
!
!-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSRFN2C' )

!
!     No result has been found.
!
      FOUND = .FALSE.


      IF ( PASS1 ) THEN
!
!        Initialize the surface kernel variable update counter
!        and the local pool counter. Note that this routine
!        is a "subsystem" as seen by its callers and a "user"
!        with respect to the kernel pool. Hence the different
!        initializations.
!
         CALL ZZCTRSIN ( SRFCTR )
         CALL ZZCTRUIN ( POLCTR )

!
!        Initialize local data structures. The first instance of this
!        call also sets a watch on the surface mapping kernel
!        variables.
!
         CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, &
                         EXTKER, NKVAR,  SNMHLS, SNMPOL, &
                         SNMIDX, SIDHLS, SIDPOL, SIDIDX )
!
!        Sync POLCTR with the kernel pool counter.
!
         CALL ZZCVPOOL ( 'ZZSRFTRN', POLCTR, LUPDTE )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSRFN2C' )
            RETURN
         END IF

         PASS1  = .FALSE.

      END IF

!
!     Determine whether the data structures need to be updated
!     due to a change in the kernel pool contents.
!
      CALL ZZCVPOOL ( 'ZZSRFTRN', POLCTR, LUPDTE )

      IF ( LUPDTE ) THEN
!
!        Conservatively increment the ZZSRFTRN state counter in
!        expectation of successful update.
!
         CALL ZZCTRINC ( SRFCTR )
!
!        Initialize local data structures.
!
         CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, &
                         EXTKER, NKVAR,  SNMHLS, SNMPOL, &
                         SNMIDX, SIDHLS, SIDPOL, SIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSRFN2C' )
            RETURN
         END IF

      END IF

!
!     No translation can be done if the surface mapping variables
!     are not in the pool.
!
      IF ( .NOT. EXTKER ) THEN
         CALL CHKOUT ( 'ZZSRFN2C' )
         RETURN
      END IF

!
!     Get a "normalized" copy of the input name: left-justified,
!     compressed, upper case.
!
      CALL LJUCRS ( 1, SRFNAM, NSRFNM )

!
!     Get a "squished" version of the above name: a version
!     containing no blanks.
!
      CALL CMPRSS ( ' ', 0, NSRFNM, SQSHNM )

!
!     Find the hash value of the squished input name.
!
      LOOKAT = ZZHASH2( SQSHNM, SNMPOL(SIZIDX) )
      NODE   = SNMHLS ( LOOKAT )

      FOUND = .FALSE.

      IF ( NODE .GT. 0 ) THEN
!
!        Start at the head node and check each normalized name saved
!           for this hash value until we find a name and body ID that
!           match or run out of items in the collision list.
!
         DO WHILE (  ( NODE .GT. 0 ) .AND. ( .NOT. FOUND )  )

            FOUND  =       ( NSRFNM  .EQ. NORNAM( SNMIDX(NODE) ) ) &
                     .AND. ( BODYID  .EQ. KERBID( SNMIDX(NODE) ) )

            ITEMAT = NODE
            NODE   = SNMPOL ( NODE )

         END DO
!
!        ITEMAT is the value of the last node checked, or
!        0 if the list is empty.
!
      END IF

      IF ( FOUND ) THEN

         SURFID = KERSID( SNMIDX(ITEMAT) )

      END IF

      CALL CHKOUT ( 'ZZSRFN2C' )
      RETURN





!$Procedure ZZSRFC2N ( Surface ID code to name mapping )

      ENTRY ZZSRFC2N ( SURFID, BODYID, SRFNAM, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Map a surface ID code and body ID code to a surface name.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     CONVERSION
!     DSK
!     ID
!     NAME
!     STRING
!     SURFACE
!
! $ Declarations
!
!     INTEGER               SURFID
!     INTEGER               BODYID
!     CHARACTER*(*)         SRFNAM
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     SURFID     I   Surface ID code.
!     BODYID     I   Body ID code.
!     SRFNAM     O   Surface name.
!     FOUND      O   Found flag.
!
! $ Detailed_Input
!
!     SURFID     is a surface ID code.
!
!     BODYID     is the ID code of a body with which the surface
!                designated by SURFID is associated.
!
! $ Detailed_Output
!
!     SRFNAM     is the name of the surface corresponding to the input
!                surface ID code and body ID code.
!
!                If multiple assignments for the input surface ID and
!                body ID are present in the kernel pool, the latest one
!                is used to determine SRFNAM.
!
!     FOUND      is a logical flag that is .TRUE. if the inputs
!                were mapped to a surface name and .FALSE.
!                otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs when the data structures of this package
!         are initialized, it will be signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     Surface-to-name mappings may be defined at run time by loading
!     text kernels containing kernel variable assignments of the form
!
!        NAIF_SURFACE_NAME += ( <surface name 1>, ... )
!        NAIF_SURFACE_CODE += ( <surface code 1>, ... )
!        NAIF_SURFACE_BODY += ( <body code 1>,    ... )
!
!     Here the set of the three Ith list items on the right hand side
!     of the three assignments define the Ith surface name/ID mapping.
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
! $ Particulars
!
!     This routine maps pairs of surface ID codes and body ID codes to
!     surface names. It relies on the mapping variables in the kernel
!     pool.
!
!     On the first pass through this routine, this routine
!     initializes the shared data structures of this package,
!     if the initialization has not already been done.
!
! $ Examples
!
!     See the routines
!
!        SRFC2S
!        SRFCSS
!
! $ Restrictions
!
!     This routine must not be called by user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 05-NOV-2021 (NJB)
!
!        Corrected errors in the $Brief_I/O and $Detailed_Output
!        sections of the header.
!
!-    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS)
!
!-&

! $ Index_Entries
!
!     map surface id and body id to surface name
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSRFC2N' )

!
!     No result has been found.
!
      FOUND = .FALSE.


      IF ( PASS1 ) THEN
!
!        Initialize the surface kernel variable update counter
!        and the local pool counter. Note that this routine
!        is a "subsystem" as seen by its callers and a "user"
!        with respect to the kernel pool. Hence the different
!        initializations.
!
         CALL ZZCTRSIN ( SRFCTR )
         CALL ZZCTRUIN ( POLCTR )

!
!        Initialize local data structures. The first instance of this
!        call also sets a watch on the surface mapping kernel
!        variables.
!
         CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, &
                         EXTKER, NKVAR,  SNMHLS, SNMPOL, &
                         SNMIDX, SIDHLS, SIDPOL, SIDIDX )
!
!        Sync SRFCTR with the kernel pool counter.
!
         CALL ZZCVPOOL ( 'ZZSRFTRN', POLCTR, LUPDTE )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSRFC2N' )
            RETURN
         END IF


         PASS1  = .FALSE.

      END IF

!
!     Determine whether the data structures need to be updated
!     due to a change in the kernel pool contents.
!
      CALL ZZCVPOOL ( 'ZZSRFTRN', POLCTR, LUPDTE )

      IF ( LUPDTE ) THEN
!
!        Conservatively increment the ZZSRFTRN state counter in
!        expectation of successful update.
!
         CALL ZZCTRINC ( SRFCTR )

!
!        Initialize local data structures.
!
         CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, &
                         EXTKER, NKVAR,  SNMHLS, SNMPOL, &
                         SNMIDX, SIDHLS, SIDPOL, SIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSRFC2N' )
            RETURN
         END IF

      END IF

!
!     No translation can be done if the surface mapping variables
!     are not in the pool.
!
      IF ( .NOT. EXTKER ) THEN
         CALL CHKOUT ( 'ZZSRFC2N' )
         RETURN
      END IF

!
!     Find the hash value of the squished input name.
!
      LOOKAT = ZZHASHI( SURFID, SIDPOL(SIZIDX) )
      NODE   = SIDHLS ( LOOKAT )

      FOUND  = .FALSE.

      IF ( NODE .GT. 0 ) THEN
!
!        Start at the head node and check each normalized name saved
!           for this hash value until we find a name and body ID that
!           match or run out of items in the collision list.
!
         DO WHILE (  ( NODE .GT. 0 ) .AND. ( .NOT. FOUND )  )

            FOUND  =       ( SURFID  .EQ. KERSID( SIDIDX(NODE) ) ) &
                     .AND. ( BODYID  .EQ. KERBID( SIDIDX(NODE) ) )

            ITEMAT = NODE
            NODE   = SIDPOL ( NODE )

         END DO
!
!        ITEMAT is the value of the last node checked, or
!        0 if the list is empty.
!
      END IF

      IF ( FOUND ) THEN

         SRFNAM = KERNAM( SIDIDX(ITEMAT) )

      END IF

      CALL CHKOUT ( 'ZZSRFC2N' )
      RETURN






!$Procedure ZZSRFTRK ( Surface mapping tracker )

      ENTRY ZZSRFTRK ( USRCTR, UPDATE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Check a user counter against the surface mapping counter
!     maintained by this package. Indicate whether the caller
!     needs to update variables associated with the user counter.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     CONVERSION
!     DSK
!     ID
!     NAME
!     STRING
!     SURFACE
!
! $ Declarations
!
!     INTEGER               USRCTR ( * )
!     LOGICAL               UPDATE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     USRCTR    I-O  User counter.
!     UPDATE     O   Update flag.
!
! $ Detailed_Input
!
!     USRCTR     is a counter passed in by a calling routine. Normally
!                USRCTR would be used to enable the application calling
!                this routine to determine whether local associations
!                of surface names and IDs are up to date.
!
!
! $ Detailed_Output
!
!     USRCTR     is the user counter, updated if necessary to match the
!                counter maintained by this package.
!
!
!     UPDATE     is a logical flag indicating whether USRCTR was
!                updated.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs when the data structures of this package
!         are initialized, it will be signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     Surface-to-name mappings may be defined at run time by loading
!     text kernels containing kernel variable assignments of the form
!
!        NAIF_SURFACE_NAME += ( <surface name 1>, ... )
!        NAIF_SURFACE_CODE += ( <surface code 1>, ... )
!        NAIF_SURFACE_BODY += ( <body code 1>,    ... )
!
!     Here the set of the three Ith list items on the right hand side
!     of the three assignments define the Ith surface name/ID mapping.
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
! $ Particulars
!
!     This routine allows SPICELIB routines to determine whether the
!     the surface mapping has been updated.
!
!     On every pass through this routine, this routine tests whether
!     the local mapping data structures need to be updated. If they do,
!     the data structures are re-initialized using the surface mapping
!     variables, if these are present in the kernel pool.
!
! $ Examples
!
!     See use of this routine in SPICELIB high-level geometry
!     routines such as
!
!        ILLUMF
!        SINCPT
!        SUBPNT
!
! $ Restrictions
!
!     This routine must not be called by user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS)
!
!-&

! $ Index_Entries
!
!     track surface mapping variable updates
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF


      IF ( PASS1 ) THEN
!
!        Check in because ZZSRFKER can fail.
!
         CALL CHKIN ( 'ZZSRFTRK' )
!
!        Initialize the surface kernel variable update counter
!        and the local pool counter. Note that this routine
!        is a "subsystem" as seen by its callers and a "user"
!        with respect to the kernel pool. Hence the different
!        initializations.
!
         CALL ZZCTRSIN ( SRFCTR )
         CALL ZZCTRUIN ( POLCTR )

!
!        Initialize local data structures. The first instance of this
!        call also sets a watch on the surface mapping kernel
!        variables.
!
         CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, &
                         EXTKER, NKVAR,  SNMHLS, SNMPOL, &
                         SNMIDX, SIDHLS, SIDPOL, SIDIDX )
!
!        Sync SRFCTR with the kernel pool counter.
!
         CALL ZZCVPOOL ( 'ZZSRFTRN', POLCTR, LUPDTE )
!
!        Check out here since this routine doesn't check out
!        before its normal exit.
!
         CALL CHKOUT ( 'ZZSRFTRK' )

         IF ( FAILED() ) THEN
            RETURN
         END IF

         PASS1  = .FALSE.

      END IF

!
!     Check for updates to the kernel pool variables.
!
      CALL ZZCVPOOL ( 'ZZSRFTRN', POLCTR, LUPDTE )

      IF ( LUPDTE ) THEN
!
!        Check in because ZZSRFKER can fail.
!
         CALL CHKIN ( 'ZZSRFTRK' )

!
!        Conservatively increment the ZZSRFTRN state counter in
!        expectation of successful update.
!
         CALL ZZCTRINC ( SRFCTR )

!
!        Update kernel pool mapping lists and hashes.
!
         CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, &
                         EXTKER, NKVAR,  SNMHLS, SNMPOL, &
                         SNMIDX, SIDHLS, SIDPOL, SIDIDX )

         CALL CHKOUT ( 'ZZSRFTRK' )

         IF ( FAILED() ) THEN
            RETURN
         END IF

      END IF

!
!     Check the input counter against the ZZSRFTRN counter;
!     sync the user counter.
!
      CALL ZZCTRCHK ( SRFCTR, USRCTR, UPDATE )

      RETURN
      END
